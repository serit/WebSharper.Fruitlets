namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

[<JavaScript>]
module DataSource =


    type ApiCrud<'U,'T> when 'U : equality =
        {
            mutable CreateFunc: string option
            mutable ReadFunc: string option
            mutable UpdateFunc: ('T -> string) option
            mutable DeleteFunc: ('T -> string) option
            mutable GetFunc: ('U -> string) option
            ErrorStatus: Var<string>
        }

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
//        member this.Delete (item: 'T) (model : ListModel<'U,'T>) idFunc=
//            match this.DeleteFunc with
//            | Some df ->
//                async {
//                    let! result = df item
//                    if result
//                    then model.RemoveByKey (idFunc item)
//                }|> Async.Start
//            | None -> ()
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
        //| Api of ApiCrud<'U,'T>
        | Rpc of RpcCrud<'U,'T>
        | Synchronous of SynchCrud<'U,'T>

    type DS<'U,'T> when 'U : equality =
        {

            IdFunc: ('T -> 'U)
            Model: ListModel<'U,'T>
            mutable CrudFunctions: CRUD<'U,'T>
            mutable ItemSelectFunc: ('T -> Dom.Element -> Dom.Event -> unit) option
            SortFunction: Var<Sort.SortFunction<'T> option>

        }
        member this.Create (currentItem: Var<'T option>) =
            match this.CrudFunctions with
            | Rpc functions ->
                functions.Create currentItem this.Model
            | _ -> ()
        member this.Read () =
            match this.CrudFunctions with
            | Rpc functions ->
                functions.Read this.Model
            | Synchronous functions ->
                functions.Read this.Model
            //| _ -> ()
        member this.Update (item: 'T) =
            match this.CrudFunctions with
            | Rpc functions ->
                functions.Update item this.Model this.IdFunc
            | _ -> ()
        member this.Delete (item: 'T) =
            match this.CrudFunctions with
            | Rpc functions ->
                functions.Delete item this.Model this.IdFunc
            | _ -> ()
        member this.CreateFunc =
            match this.CrudFunctions with
            | Rpc functions -> functions.CreateFunc.IsSome
            | _ -> false
        member this.ReadFunc =
            match this.CrudFunctions with
            | Rpc functions -> functions.ReadFunc.IsSome
            | _ -> false
        member this.UpdateFunc =
            match this.CrudFunctions with
            | Rpc functions -> functions.UpdateFunc.IsSome
            | _ -> false
        member this.DeleteFunc =
            match this.CrudFunctions with
            | Rpc functions -> functions.DeleteFunc.IsSome
            | _ -> false
        static member Create (idf,readFunction,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Rpc <| RpcCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                ItemSelectFunc = None
                SortFunction = Var.Create None
            }
        static member Create (idf,readFunction,itemSelectFunction,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Rpc <| RpcCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                ItemSelectFunc = Some itemSelectFunction
                SortFunction = Var.Create None
            }
        static member Create (idf,readFunction,sortDirection,?ItemSelectFunction,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Rpc <| RpcCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                ItemSelectFunc = ItemSelectFunction
                SortFunction = Var.Create <| Some sortDirection
            }
        static member Create (idf,readFunction,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Synchronous <| SynchCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                ItemSelectFunc = None
                SortFunction = Var.Create None
            }
        static member Create (idf,readFunction,itemSelectFunction,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Synchronous <| SynchCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                ItemSelectFunc = Some itemSelectFunction
                SortFunction = Var.Create None
            }
        static member Create (idf,readFunction,sortDirection,?ItemSelectFunction,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Synchronous <| SynchCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                ItemSelectFunc = ItemSelectFunction
                SortFunction = Var.Create <| Some sortDirection
            }


