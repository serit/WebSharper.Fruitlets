namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Html

[<JavaScript>]
module DataSource =

    // REST 
    type ApiCrud<'T> =
        {
            mutable CreateFunc: string option
            mutable ReadFunc: string option
            mutable UpdateFunc: ('T -> string) option
            mutable DeleteFunc: ('T -> string) option
            //mutable GetFunc: ('U -> string) option
            mutable Authenticate: (JQuery.AjaxSettings -> JQuery.AjaxSettings) 
            mutable RemoteDataType: JQuery.DataType
            mutable DeserializeMany: obj -> 'T []
            mutable DeserializeSingle: obj -> 'T
            mutable SerializeSingle: 'T -> obj
            ErrorStatus: Var<string>
        }
        member private this.Get url success =
            this.ErrorStatus.Value <- ""
            AjaxSettings(
                Url = url,
                DataType = this.RemoteDataType,
                Success = success,
                Error = System.Action<_,_,_>(fun errorTuple _ _ -> this.ErrorStatus.Value <- sprintf "%A" errorTuple)
            )
            |> this.Authenticate
            |> JQuery.Ajax
            |> ignore
        member private this.Post url data success =
            this.ErrorStatus.Value <- ""
            AjaxSettings(
                Url = url,
                Type = RequestType.POST,
                Data = data,
                DataType = this.RemoteDataType,
                Success = success,
                Error =  System.Action<_,_,_>(fun errorTuple _ _ -> this.ErrorStatus.Value <- sprintf "%A" errorTuple)

            )
            |> this.Authenticate
            |> JQuery.Ajax
            |> ignore
            
        member this.Create (currentItem: Var<'T option>) (model : ListModel<'U,'T>) =
            match this.CreateFunc with
            | Some cf ->
                this.Post cf null (System.Action<_,_,_>(fun result _ _ -> currentItem.Value <- Some (this.DeserializeSingle result)))
            | None -> ()
        member this.Read (model : ListModel<'U,'T>) =
            match this.ReadFunc with
            | Some rf -> 
                this.Get rf (System.Action<_,_,_>(fun result _ _ -> model.Set (this.DeserializeMany result)))
            | None -> ()
        member this.Update (item: 'T) (model : ListModel<'U,'T>) idFunc =
            match this.UpdateFunc with
            | Some uf ->   
                this.Post (uf item) (this.SerializeSingle item) (System.Action<_,_,_>(fun result _ _ -> 
                    let key = idFunc <| this.DeserializeSingle result
                    if model.ContainsKey key then 
                        model.UpdateBy (fun t -> Some item) key
                    else model.Add <| this.DeserializeSingle result)
                )
            | None -> ()
        member this.Delete (item: 'T) (model : ListModel<'U,'T>) idFunc=
            match this.DeleteFunc with
            | Some df ->
                this.Post (df item) (this.SerializeSingle item) (System.Action<_,_,_>(fun result _ _ -> model.RemoveByKey (idFunc item)))
            | None -> ()
        static member empty =
            {
                CreateFunc = None
                ReadFunc = None
                UpdateFunc = None
                DeleteFunc = None
                Authenticate = id
                RemoteDataType = DataType.Json
                DeserializeMany = As<'T []>
                DeserializeSingle = As<'T>
                SerializeSingle = As<obj>
                ErrorStatus = Var.Create ""
            }
        static member Init (createFunction,readFunction,updateFunction,deleteFunction) =
            {
                ApiCrud<'T>.empty with
                    CreateFunc = createFunction
                    ReadFunc = readFunction
                    UpdateFunc = updateFunction
                    DeleteFunc = deleteFunction
            }

    // WS RPC
    type RpcCrud<'U,'T> when 'U : equality =
        {
            mutable CreateFunc: (unit -> Async<Result.Result<'T,string>>) option
            mutable ReadFunc: (unit -> Async<'T []>) option
            mutable UpdateFunc: ('T -> Async<Result.Result<'T,string>>) option
            mutable DeleteFunc: ('T -> Async<Result.Result<'T,string>>) option //('T -> Async<bool>) option
            ErrorStatus: Var<string>
        }
        member this.Create (currentItem: Var<'T option>) (model : ListModel<'U,'T>) =
            match this.CreateFunc with
            | Some cf ->
                this.ErrorStatus.Value <- ""
                async {
                    let! result = cf ()
                    match result with
                    | Result.Success r -> currentItem.Value <- Some r
                    | Result.Failure msg -> 
                        Console.Warn msg
                        this.ErrorStatus.Value <- msg
                } |> Async.Start
            | None -> ()
        member this.Read (model : ListModel<'U,'T>) =
            match this.ReadFunc with
            | Some rf ->
                this.ErrorStatus.Value <- ""
                async {
                    let! data = rf()
                    model.Set data
                } |> Async.Start
            | None -> ()
        member this.Update (item: 'T) (model : ListModel<'U,'T>) idFunc =
            match this.UpdateFunc with
            | Some uf ->                
                this.ErrorStatus.Value <- ""
                async {
                    let! result = uf item
                    match result with
                    | Result.Success data ->
                        if model.ContainsKey (idFunc data)
                        then model.UpdateBy (fun t -> Some item) (idFunc data)
                        else model.Add data
                    | Result.Failure msg -> 
                        Console.Warn msg
                        this.ErrorStatus.Value <- msg

                    // todo: if false, there should be an error somewhere
                } |> Async.Start
            | None -> ()
        member this.Delete (item: 'T) (model : ListModel<'U,'T>) idFunc=
            match this.DeleteFunc with
            | Some df ->
                this.ErrorStatus.Value <- ""
                async {
                    let! result = df item
                    match result with
                    | Result.Success _ -> model.RemoveByKey (idFunc item)
                    | Result.Failure msg ->
                        Console.Warn msg
                        this.ErrorStatus.Value <- msg
                } |> Async.Start
            | None -> ()
        static member empty =
            {
                CreateFunc = None
                ReadFunc = None
                UpdateFunc = None
                DeleteFunc = None
                ErrorStatus = Var.Create ""
            }
        static member Init (createFunction,readFunction,updateFunction,deleteFunction) =
            {
                CreateFunc = createFunction
                ReadFunc = readFunction
                UpdateFunc = updateFunction
                DeleteFunc = deleteFunction
                ErrorStatus = Var.Create ""
            }

    // non-async
    type SynchCrud<'U,'T> when 'U : equality =
        {
            mutable CreateFunc: (unit -> 'T) option
            mutable ReadFunc: (unit -> array<'T>) option
            mutable UpdateFunc: ('T -> Result.Result<'T,string>) option
            mutable DeleteFunc: ('T -> bool) option
            ErrorStatus: Var<string>
        }
        member this.Create (currentItem: Var<'T option>) (model : ListModel<'U,'T>) =
            match this.CreateFunc with
            | Some cf ->
                let result = cf ()
                currentItem.Value <- Some result
            | None -> ()
        member this.Read (model : ListModel<'U,'T>) =
            match this.ReadFunc with
            | Some rf -> model.Set <| rf()
            | None -> ()
        member this.Update (item: 'T) (model : ListModel<'U,'T>) idFunc =
            match this.UpdateFunc with
            | Some uf ->
                let result = uf item
                match result with
                | Result.Success data ->
                        if model.ContainsKey (idFunc data)
                        then model.UpdateBy (fun t -> Some item) (idFunc data)
                        else model.Add data
                | Result.Failure msg -> Console.Log msg
            | None -> ()
        member this.Delete (item: 'T) (model : ListModel<'U,'T>) idFunc=
            match this.DeleteFunc with
            | Some df ->
                let result = df item
                if result then model.RemoveByKey (idFunc item)
            | None -> ()
        static member empty =
            {
                CreateFunc = None
                ReadFunc = None
                UpdateFunc = None
                DeleteFunc = None
                //GetFunc = None
                ErrorStatus = Var.Create ""
            }

        static member Init (createFunction,readFunction,updateFunction,deleteFunction) =
            {
                CreateFunc = createFunction
                ReadFunc = readFunction
                UpdateFunc = updateFunction
                DeleteFunc = deleteFunction
                //GetFunc = getFunction
                ErrorStatus = Var.Create ""
            }



    type CRUD<'U,'T> when 'U : equality =
        | Api of ApiCrud<'T>
        | Rpc of RpcCrud<'U,'T>
        | Synchronous of SynchCrud<'U,'T>

    // WS ListModel
    type DS<'U,'T> when 'U : equality =
        {
            IdFunc: ('T -> 'U)
            Model: ListModel<'U,'T>
            mutable CrudFunctions: CRUD<'U,'T>
            SortFunction: Var<Sort.SortFunction<'T> option>
        }
        member this.Create (currentItem: Var<'T option>) =
            match this.CrudFunctions with
            | Api functions ->
                functions.Create currentItem this.Model
            | Rpc functions ->
                functions.Create currentItem this.Model
            | _ -> ()
        member this.Read () =
            match this.CrudFunctions with
            | Api functions ->
                functions.Read this.Model
            | Rpc functions ->
                functions.Read this.Model
            | Synchronous functions ->
                functions.Read this.Model
            //| _ -> ()
        member this.Update (item: 'T) =
            match this.CrudFunctions with
            | Rpc functions ->
                functions.Update item this.Model this.IdFunc
            | Api functions ->
                functions.Update item this.Model this.IdFunc
            | _ -> ()
        member this.Delete (item: 'T) =
            match this.CrudFunctions with
            | Rpc functions ->
                functions.Delete item this.Model this.IdFunc
            | Api functions ->
                functions.Delete item this.Model this.IdFunc
            | _ -> ()
        member this.CreateFunc =
            match this.CrudFunctions with
            | Api functions -> functions.CreateFunc.IsSome
            | Rpc functions -> functions.CreateFunc.IsSome
            | _ -> false
        member this.ReadFunc =
            match this.CrudFunctions with
            | Api functions -> functions.ReadFunc.IsSome
            | Rpc functions -> functions.ReadFunc.IsSome
            | _ -> false
        member this.UpdateFunc =
            match this.CrudFunctions with
            | Api functions -> functions.UpdateFunc.IsSome
            | Rpc functions -> functions.UpdateFunc.IsSome
            | _ -> false
        member this.DeleteFunc =
            match this.CrudFunctions with
            | Api functions -> functions.DeleteFunc.IsSome
            | Rpc functions -> functions.DeleteFunc.IsSome
            | _ -> false
        static member Create (idf,readFunction,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Rpc <| RpcCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                SortFunction = Var.Create None
            }
        static member Create (idf,readFunction,sortDirection,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Rpc <| RpcCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                SortFunction = Var.Create <| Some sortDirection
            }
        static member Create (idf,readFunction,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Synchronous <| SynchCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                SortFunction = Var.Create None
            }
        static member Create (idf,readFunction,sortDirection,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Synchronous <| SynchCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                SortFunction = Var.Create <| Some sortDirection
            }
        static member Create (idf,readFunction,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Api <| ApiCrud<'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                SortFunction = Var.Create None
            }
        static member Create (idf,readFunction,sortDirection,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Api <| ApiCrud<'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                SortFunction = Var.Create <| Some sortDirection
            }


