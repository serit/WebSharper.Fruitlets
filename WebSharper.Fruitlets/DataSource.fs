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

        }

    type RpcCrud<'U,'T> when 'U : equality =
        {
            mutable CreateFunc: (unit -> Async<'T>) option
            mutable ReadFunc: (unit -> Async<array<'T>>) option
            mutable UpdateFunc: ('T -> Async<Result.Result<'T,string>>) option
            mutable DeleteFunc: ('T -> Async<bool>) option

        }
        member this.Create (currentItem: Var<'T option>) (model : ListModel<'U,'T>) =
            match this.CreateFunc with
            | Some cf ->
                async {
                    let! result = cf ()
                    currentItem.Value <- Some result
                }|> Async.Start
            | None -> ()
        member this.Read (model : ListModel<'U,'T>) =
            match this.ReadFunc with
            | Some rf ->
                async {
                    let! data = rf()
                    model.Set data
                } |> Async.Start
            | None -> ()
        member this.Update (item: 'T) (model : ListModel<'U,'T>) idFunc =
            match this.UpdateFunc with
            | Some uf ->
                async {
                    let! result = uf item
                    match result with
                    | Result.Success data ->
                            if model.ContainsKey (idFunc data)
                            then model.UpdateBy (fun t -> Some item) (idFunc data)
                            else model.Add data
                    | Result.Failure msg -> Console.Log msg
                    // todo: if false, there should be an error somewhere
                }|> Async.Start
            | None -> ()
        member this.Delete (item: 'T) (model : ListModel<'U,'T>) idFunc=
            match this.DeleteFunc with
            | Some df ->
                async {
                    let! result = df item
                    if result
                    then model.RemoveByKey (idFunc item)
                }|> Async.Start
            | None -> ()
        static member empty =
            {
                CreateFunc = None
                ReadFunc = None
                UpdateFunc = None
                DeleteFunc = None
            }
        static member Init (createFunction,readFunction,updateFunction,deleteFunction) =
            {
                CreateFunc = createFunction
                ReadFunc = readFunction
                UpdateFunc = updateFunction
                DeleteFunc = deleteFunction
            }

    type SynchCrud<'U,'T> when 'U : equality =
        {
            mutable CreateFunc: (unit -> 'T) option
            mutable ReadFunc: (unit -> array<'T>) option
            mutable UpdateFunc: ('T -> Result.Result<'T,string>) option
            mutable DeleteFunc: ('T -> bool) option
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
                // todo: if false, there should be an error somewhere
            | None -> ()
        member this.Delete (item: 'T) (model : ListModel<'U,'T>) idFunc=
            match this.DeleteFunc with
            | Some df ->
                let result = df item
                if result
                then model.RemoveByKey (idFunc item)
            | None -> ()
        static member empty =
            {
                CreateFunc = None
                ReadFunc = None
                UpdateFunc = None
                DeleteFunc = None
                //GetFunc = None
            }

        static member Init (createFunction,readFunction,updateFunction,deleteFunction) =
            {
                CreateFunc = createFunction
                ReadFunc = readFunction
                UpdateFunc = updateFunction
                DeleteFunc = deleteFunction
                //GetFunc = getFunction
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
            mutable SortDirection: Sort.SortDirection

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
            | _ -> ()
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
                SortDirection = Sort.Asc -1
            }
        static member Create (idf,readFunction,itemSelectFunction,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Rpc <| RpcCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                ItemSelectFunc = Some itemSelectFunction
                SortDirection = Sort.Asc -1
            }
        static member Create (idf,readFunction,sortDirection,?ItemSelectFunction,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Rpc <| RpcCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                ItemSelectFunc = ItemSelectFunction
                SortDirection = sortDirection
            }
        static member Create (idf,readFunction,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Synchronous <| SynchCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                ItemSelectFunc = None
                SortDirection = Sort.Asc -1
            }
        static member Create (idf,readFunction,itemSelectFunction,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Synchronous <| SynchCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                ItemSelectFunc = Some itemSelectFunction
                SortDirection = Sort.Asc -1
            }
        static member Create (idf,readFunction,sortDirection,?ItemSelectFunction,?CreateFunction,?UpdateFunction,?DeleteFunction) =
            {
                IdFunc = idf
                Model = ListModel.Create idf Seq.empty
                CrudFunctions = Synchronous <| SynchCrud<'U,'T>.Init(CreateFunction, Some readFunction,UpdateFunction,DeleteFunction)
                ItemSelectFunc = ItemSelectFunction
                SortDirection = sortDirection
            }


