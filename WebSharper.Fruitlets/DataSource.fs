namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Html

[<JavaScript>]
module DataSource =

    /// ApiCrud is used as a REST datasource for the Table type
    /// Urls or functions that build urls are needed for basic CRUD operations
    /// <example>
    /// { ApiCrud<Book>.empty with
    ///     CreateFunc = Some "https://www.example.com/create.a.new.book"
    ///     ReadFunc = Some "https://www.googleapis.com/books/v1/volumes?q=harry+potter"
    ///     UpdateFunc = Some (fun book -> sprintf "https://www.example.com/update/%i" book.Id)
    ///     DeleteFunc = Some (fun book -> sprintf "https://www.example.com/delete/%i" book.Id)
    ///     Authenticate = fun settings -> 
    ///         settings.Headers <- Object<string>([|("Authorization", sprintf "Base %s" secretAuthorizationToken)|])
    ///         settings
    ///     DeserializeMany = fun data -> 
    ///         Console.Log data
    ///         As<BookCollection> data |> fun d -> d.items
    /// }
    /// </example>
    type ApiCrud<'T> =
        {
            /// Url to create a new item
            mutable CreateFunc: string option
            /// Url to read 
            mutable ReadFunc: string option
            /// Url to update an item of type 'T 
            mutable UpdateFunc: ('T -> string) option
            /// Url to delete an item of type 'T
            mutable DeleteFunc: ('T -> string) option
            /// How to edit the settings to allow authentication
            mutable Authenticate: (JQuery.AjaxSettings -> JQuery.AjaxSettings) 
            /// The type of data. Default = json
            mutable RemoteDataType: JQuery.DataType 
            /// Allow for a different way to deserialize an array of type 'T [] from WebSharpers standard method As<'T []>
            mutable DeserializeMany: obj -> 'T []
            /// Allow for a different way to deserialize an item of type 'T from WebSharpers standard method As<'T>
            mutable DeserializeSingle: obj -> 'T
            /// Allow for a different way to serialize an item of type 'T from WebSharpers standard method As<obj>
            /// this method is used to include data in Update and Delete requests
            mutable SerializeSingle: 'T -> obj
            /// ErrorStatus is lifted up to the Table class. In case of an error it can be displayed as an alert to the user
            ErrorStatus: Var<string>
        }
        // generic GET request
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
        // generic POST request
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
            
        /// create a new item if a create Url is defined and set it as the current item
        member this.Create (currentItem: Var<'T option>) (model : ListModel<'U,'T>) =
            match this.CreateFunc with
            | Some cf ->
                this.Post cf null (System.Action<_,_,_>(fun result _ _ -> currentItem.Value <- Some (this.DeserializeSingle result)))
            | None -> ()
        /// read data if a read Url is defined and set the response as the value of the list model
        member this.Read (model : ListModel<'U,'T>) =
            match this.ReadFunc with
            | Some rf -> 
                this.Get rf (System.Action<_,_,_>(fun result _ _ -> model.Set (this.DeserializeMany result)))
            | None -> ()
        /// update an item if an update Url is defined and update the listmodel with the new item
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
        /// delete an item if a delete Url is defined and delete the item from the listmodel
        member this.Delete (item: 'T) (model : ListModel<'U,'T>) idFunc =
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
        // Create a standard instance of the ApiCrud with the given CRUD functions         
        static member Init (createFunction,readFunction,updateFunction,deleteFunction) =
            {
                ApiCrud<'T>.empty with
                    CreateFunc = createFunction
                    ReadFunc = readFunction
                    UpdateFunc = updateFunction
                    DeleteFunc = deleteFunction
            }

    /// RpcCrud is used as a datasource for the Table type
    /// Typically, the provided functions are WebSharper Rpc functions, but any asynchronous functions will suffice
    /// Many of these functions use the result type, to allow for proper handling of exceptions
    /// <example>
    /// [<Rpc>]
    /// let read() : Async<Book []> = async{return GetBooksFromDatabase()}
    /// 
    /// [<Rpc>]
    /// let update book =
    ///     async{
    ///         return rlog{
    ///             return! UpdateBookInDatabase book
    ///         }
    ///     }
    /// 
    /// { RpcCrud<int, Book>.empty with
    ///     ReadFunc = Some read
    ///     UpdateFunc = Some update
    /// }
    /// </example>
    type RpcCrud<'U,'T> when 'U : equality =
        {
            // create a new item            
            mutable CreateFunc: (unit -> Async<Result.Result<'T,string>>) option
            // read all items
            mutable ReadFunc: (unit -> Async<'T []>) option
            // update an existing item
            mutable UpdateFunc: ('T -> Async<Result.Result<'T,string>>) option
            // delete an existing item
            mutable DeleteFunc: ('T -> Async<Result.Result<'T,string>>) option
            /// ErrorStatus is lifted up to the Table class. In case of an error it can be displayed as an alert to the user
            ErrorStatus: Var<string>
        }
        /// create a new item if a create function is defined and set it as the current item
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
        /// read data if a read function is defined and set the response as the value of the list model
        member this.Read (model : ListModel<'U,'T>) =
            match this.ReadFunc with
            | Some rf ->
                this.ErrorStatus.Value <- ""
                async {
                    let! data = rf()
                    model.Set data
                } |> Async.Start
            | None -> ()
        /// update an item if an update function is defined and update the listmodel with the new item
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
        /// delete an item if a delete function is defined and delete the item from the listmodel
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
        // Create a standard instance of the RpcCrud with the given CRUD functions
        static member Init (createFunction,readFunction,updateFunction,deleteFunction) =
            {
                CreateFunc = createFunction
                ReadFunc = readFunction
                UpdateFunc = updateFunction
                DeleteFunc = deleteFunction
                ErrorStatus = Var.Create ""
            }

    
    /// SynchCrud is used as a datasource for the Table type with synchronous functions as basis
    /// Currently it can only be used for readonly tables
    /// <example>
    /// let getSomeNumbers() = Array.init 10 (fun i -> i, i * i)
    /// 
    /// { SynchCrud<int, int * int>.empty with
    ///     ReadFunc = Some getSomeNumbers
    /// }
    /// </example>
    type SynchCrud<'U,'T> when 'U : equality =
        {
            /// create a new item    
            mutable CreateFunc: (unit -> 'T) option
            /// read all items
            mutable ReadFunc: (unit -> array<'T>) option
            /// update a new item   
            mutable UpdateFunc: ('T -> Result.Result<'T,string>) option
            /// delete a new item   
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


    /// A wrapper for the different types of datasource classes
    type CRUD<'U,'T> when 'U : equality =
        | Api of ApiCrud<'T>
        | Rpc of RpcCrud<'U,'T>
        | Synchronous of SynchCrud<'U,'T>

    /// The main datasource type used in the Table class
    /// The different CRUD functions are managed through this type
    type DS<'U,'T> when 'U : equality =
        {
            /// How to obtain a unique Id from type 'T. 'U is used as a key in the ListModel
            IdFunc: ('T -> 'U)
            /// The listmodel where all items are stored
            Model: ListModel<'U,'T>
            /// The underlying CRUD model
            mutable CrudFunctions: CRUD<'U,'T>
            /// How the items are sorted on display
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