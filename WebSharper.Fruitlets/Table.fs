namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Html

// Bootstrap table
[<JavaScript>]
module Table =
    open Modal

    let private flip f a b = f b a 

    type Column<'DataType> = Column.Column<'DataType>
    type Permission = Column.Permission
    type ColumnPermission = Column.ColumnPermission

    type TableElementAttributes<'DataType>() =
        member val Table = [attr.``class`` "table fruit-table table-bordered table-striped"] with get,set
        member val THead = [attr.``class`` "fruit-table-head"] with get,set
        member val THeadRow = [attr.``class`` "fruit-table-head-row"] with get,set
        member val TH = [attr.``class`` "fruit-table-head-cell"] with get,set
        member val TBody = [attr.``class`` "fruit-table-body"] with get,set
        member val TR = fun (_: 'DataType) -> [] with get,set
            

    type TableRowDetail<'DataType> =
        | NoDetail
        | AllRows of ('DataType -> Elt)
        | SelectedRow of ('DataType -> Elt)        
        
    type ButtonStatic =
        {
            Attributes:  Attr list
            Docs:  Doc list
        }

    type ButtonFromItem<'DataType> =
        {
            Attributes: 'DataType -> Attr list
            Docs: 'DataType -> Doc list
        }

    type Validation<'DataType> = 'DataType -> Result.Result<bool, string>

    type TableSettings<'DataType> =
        {            
            /// <summary>
            /// Whether a detail wil be shown for no rows, all rows or the selected row
            /// The row detail will be show in a separate row underneath the main one
            /// </summary>
            ShowDetail: TableRowDetail<'DataType>
               
            /// <summary>
            /// Optional custom buttons   
            /// </summary>
            AddButton: ButtonStatic option
            EditButton: ButtonFromItem<'DataType> option
            DeleteButton: ButtonFromItem<'DataType> option
               
            /// <summary>
            /// Optional custom modal buttons   
            /// </summary>
            ModalSaveButton: (ButtonFromItem<'DataType> option)
            ModalSaveValidation: Validation<'DataType> option
            ModalCancelButton: ButtonFromItem<'DataType> option
            ModalDeleteButton: ButtonFromItem<'DataType> option
            ModalDeleteValidation: Validation<'DataType> option
               
            /// <summary>
            /// ShowErrors. On by default   
            /// </summary>
            ShowErrors: bool
               
            /// <summary>
            /// Set attributes for different elements of the table   
            /// </summary>
            TableElementAttributes: TableElementAttributes<'DataType>
               
            /// <summary>
            /// Shown as description in modal header   
            /// </summary>
            ModalUpdateHeader: 'DataType -> string
            ModalDeleteHeader: 'DataType -> string
               
            /// <summary>
            /// Show Table header   
            /// </summary>
            ShowTableHeader: bool

            mutable ItemSelectFunc: ('DataType -> Dom.Element -> Dom.Event -> unit) option
        }
        static member Default =
            {                
                ShowDetail = NoDetail
                ModalUpdateHeader = (fun _ -> "Update item")
                ModalDeleteHeader = (fun _ -> "Delete item")
                AddButton = None
                EditButton = None
                DeleteButton = None
                ModalSaveButton = None
                ModalSaveValidation = None
                ModalCancelButton = None
                ModalDeleteButton = None
                ModalDeleteValidation = None
                ShowErrors = true
                TableElementAttributes = TableElementAttributes<'DataType>()
                ShowTableHeader = true
                ItemSelectFunc = None
            }

    type Table<'Key, 'DataType> when 'Key : equality  =
        {   
            /// <summary>
            /// the id of the table       
            /// </summary>
            Id': string   
            /// <summary>
            /// The datasource for the table rows   
            /// </summary>
            DataSource: DataSource.DS<'Key,'DataType>   
            /// <summary>
            /// The Column type generates a column from type 'DataType   
            /// </summary>
            Columns: Column<'DataType> []   
            /// <summary>
            /// CustomSettings   
            /// </summary>
            Settings: TableSettings<'DataType>
        }
        member private this.SortFunctionView =    
            this.DataSource.SortFunction.View
            |> View.Map ( function
                | Some (Sort.AscByFunction f) -> Seq.sortBy f
                | Some (Sort.DescByFunction f) -> Seq.sortByDescending f
                | Some (Sort.AscByColumn i) -> 
                    match Array.tryItem i this.Columns |> Option.bind ( fun column -> column.SortFunction) with
                    | Some f -> Seq.sortBy f
                    | _ -> id
                | Some (Sort.DescByColumn i) -> 
                    match Array.tryItem i this.Columns |> Option.bind ( fun column -> column.SortFunction) with
                    | Some f -> Seq.sortByDescending f
                    | _ -> id
                | None -> id
            )
        member this.ErrorStatus =
            match this.DataSource.CrudFunctions with
            | DataSource.CRUD.Api api -> api.ErrorStatus
            | DataSource.CRUD.Rpc rpc -> rpc.ErrorStatus
            | DataSource.CRUD.Synchronous syn -> syn.ErrorStatus
        member private this.ErrorBoxView (errorVar: Var<string>) =
            Doc.BindView( function
                | "" -> Doc.Empty
                | msg ->
                    div[
                        attr.``class`` "alert alert-danger alert-dismissable"
                    ][
                        a[
                            attr.href "#"
                            attr.``class`` "close"
                            attr.``data-`` "dismiss" "alert"
                            on.click (fun _ _ -> errorVar.Value <- "")
                        ][i[attr.``class`` "fa fa-close"][]]
                        text msg
                    ] :> Doc
            ) errorVar.View
        member this.ErrorBox () =
            this.ErrorBoxView this.ErrorStatus
        member this.isEditable =
            this.Columns
            |> Array.exists (fun c -> c.EditField.IsSome) 
            && this.DataSource.UpdateFunc
        member this.isDeletable = this.DataSource.DeleteFunc
        member private this.SaveButton windowId (errorVar : Var<string>) (t : 'DataType) =
            let saveButtonAttrs, saveButtonContent =
                match this.Settings.ModalSaveButton with
                | Some button -> button.Attributes t, button.Docs t
                | None -> [attr.``class`` "btn btn-primary fruit-btn fruit-btn-save"], [ text " Save" ]
            let extraAttributes =
                match this.Settings.ModalSaveValidation with
                | None -> 
                    [   attr.``data-`` "dismiss" "modal"
                        on.click (fun el ev -> this.DataSource.Update t )]
                | Some vf -> 
                    [   on.click (fun el ev -> 
                            match vf t with
                            | Result.Success true -> 
                                this.DataSource.Update t
                                errorVar.Value <- ""
                                Modal.CloseModal <| "#" + windowId
                            | Result.Success false ->
                                errorVar.Value <- "Invalid data"
                            | Result.Failure msg->
                                errorVar.Value <- msg )]
            button
                (saveButtonAttrs @ extraAttributes ) 
                saveButtonContent :> Doc
        member private this.DeleteButtonShow (t : 'DataType) =
            let modalId = sprintf "confirm-delete-%s-%s" this.Id' <| (this.DataSource.IdFunc t).ToString()
            let confirmDialog () =

                let modalwindow =
                    {
                        Header =  
                            h2
                                [attr.``class`` "fruit-modal-header"] 
                                [text <| this.Settings.ModalDeleteHeader t]
                        Body = text "Are you sure you want to delete this item?"
                        Footer =
                            div [] [
                                this.ModalDeleteButtonShow t
                                this.ModalCancelButtonShow t
                            ]
                        Size = Modal.Small
                        Id = modalId
                    }
                modalwindow.Show()
                
            let attrs, content =
                match this.Settings.DeleteButton with
                | Some button -> button.Attributes t, button.Docs t
                | None -> [attr.``class``  "btn btn-danger fruit-btn fruit-btn-delete fruit-btn-icon-only"], [ i[ attr.``class`` "fa fa-trash"][] ]

            div[attr.style "display: inline-block"][
                confirmDialog()
                Modal.Button modalId attrs content
            ] :> Doc
        member private this.EditFooter windowId errorVar (item : 'DataType option) =
            match item with
            | Some v ->
                div [] [
                    (if this.DataSource.UpdateFunc then this.SaveButton windowId errorVar v else Doc.Empty)
                    this.ModalCancelButtonShow v
                ] :> Doc
            | None -> Doc.Empty
        member private this.ModalCancelButtonShow (item) =
            let cancelButtonAttrs, cancelButtonContent =
                match this.Settings.ModalCancelButton with
                | Some button -> button.Attributes item, button.Docs item
                | None -> [attr.``class`` "btn btn-secondary fruit-btn fruit-btn-cancel"], [ i[ attr.``class`` "fa fa-close"][]; text " Close" ]
            button(attr.``data-`` "dismiss" "modal" :: cancelButtonAttrs) cancelButtonContent 
        member private this.ModalDeleteButtonShow (t) =
            let deleteButtonAttrs, deleteButtonContent =
                match this.Settings.ModalDeleteButton with
                | Some button -> button.Attributes t, button.Docs t
                | None -> [attr.``class`` "btn btn-danger fruit-btn fruit-btn-delete"], [ i[ attr.``class`` "fa fa-trash"][]; text " Delete" ]
            button
                ([
                    attr.``data-`` "dismiss" "modal"
                    on.click (fun el ev ->
                        this.DataSource.Delete t
                        )] @ deleteButtonAttrs) 
                deleteButtonContent 
            
        member this.EditWindow (item : Var<'DataType option>) windowId =
            
            let error = Var.Create ""

            let editForm () =
                this.Columns
                |> Array.filter ( fun column ->
                    column.Permission.Form <> Permission.Invisible
                )
                |> Array.map (fun column ->
                    match (column.EditField, column.Permission.Form, column.EditFieldAttrList) with
                    | None, _, _
                    | _, Permission.Read, _ -> (Input.Disabled column.DocList).show column.Name item
                    | Some (Column.InputField editField), _, None -> editField.show column.Name item
                    | Some (Column.InputField editField), _, Some attrList -> 
                        let attrListView = fun (t:Var<'DataType option>) ->
                            [
                                attr.``dataDyn-`` "represents" (View.Map (function Some t' -> sprintf "%A" <| this.DataSource.IdFunc t' | None -> "empty") t.View)
                            ] @ attrList t
                        editField.show(column.Name, attrListView, editField.formWrapper column.Name) item
                    | Some (Column.DocField docField), _, _ -> docField item
                )
                |> Array.toList
                |> form [attr.``class`` "fruit-form"]

            {
                Id = windowId
                Header = 
                    h2
                        [attr.``class`` "fruit-modal-header"] 
                        [textView <| View.Map (function 
                            | Some t' ->  this.Settings.ModalUpdateHeader t'
                            | None -> ""
                            ) item.View ]
                Body = 
                    [   this.ErrorBoxView error
                        editForm () :> Doc ] 
                    |> Doc.Concat
                Footer = Doc.BindView (fun t -> this.EditFooter windowId error t) item.View
                Size = Modal.WindowSize.Normal
            }
        member private this.EditButtonShow idCode (currentItem: Var<'DataType option>) (t) =
            let attrs, content =
                match this.Settings.EditButton with
                | Some button -> button.Attributes t, button.Docs t
                | None -> [attr.``class``  "btn btn-primary fruit-btn fruit-btn-edit fruit-btn-icon-only"], [ i[ attr.``class`` "fa fa-edit"][] ]

            Modal.Button
                idCode (attrs @ [ on.click( fun el ev -> currentItem.Value <- Some t)]) content :> Doc
            
        member private this.CreateButtonShow() =
            if this.DataSource.CreateFunc
            then
                // a button and an edit field: new. on new, open window with empty form
                let currentItem = Var.Create None
                let newModalId = (sprintf "new-%s" this.Id')
                
                let attrs, content =
                    match this.Settings.AddButton with
                    | Some button -> button.Attributes, button.Docs
                    | None -> [attr.``class`` "btn btn-success fruit-btn fruit-btn-create fruit-btn-icon-only"], [ i[ attr.``class`` "fa fa-plus"][] ]

                div [] [
                    (this.EditWindow currentItem newModalId).Show()
                    button 
                        ( attrs @ 
                            [
                                attr.``data-`` "toggle" "modal"
                                attr.``data-`` "target" <| "#" + newModalId
                                on.click (fun el ev -> this.DataSource.Create currentItem)
                            ] ) 
                        content
                ]:> Doc
            else Doc.Empty
        member this.ShowHeader () =
            // add an extra column with an edit button
            let extracol = 
                if this.isEditable || this.isDeletable then 
                    [| th[ attr.``class`` "fruit-edit-btn-column" ][] :> Doc |] 
                else [||]

            this.Columns
            |> Array.indexed
            |> Array.filter ( fun (_, column) ->
                column.Permission.Table <> Permission.Invisible )
            |> Array.map (fun (index, column) ->
                column.showHeader index this.DataSource )
            |> flip Array.append extracol
            |> fun headerRow -> 
                thead
                    this.Settings.TableElementAttributes.THead
                    [tr this.Settings.TableElementAttributes.THeadRow headerRow] :> Doc
        member private this.ShowTableFilter (filter: 'DataType -> bool ) (currentItem : Var<'DataType option>) sortFunction =
            let idCode = sprintf "%s-edit-%i" this.Id' ( int <| (Math.Random() * 100000.) + 1.)
            let editdeleteColumn t =
                if this.isEditable || this.isDeletable
                then
                    [
                        td[attr.``class`` "fruit-edit-btn-column"][
                            (
                                if this.isEditable
                                then this.EditButtonShow idCode currentItem t
                                else Doc.Empty)
                            (
                                if this.isDeletable
                                then this.DeleteButtonShow t
                                else Doc.Empty)
                        ] :> Doc
                    ]
                else List.empty
                |> List.toArray
            let rows (view: ListModel<'Key, 'DataType>) =
                let rowFunction (t: Var<'DataType>) =
                    let row =
                        this.Columns
                        |> Array.filter ( fun column ->
                            column.Permission.Table <> Permission.Invisible )
                        |> Array.map ( fun column ->
                            column.showRow t )
                        |> flip Array.append (editdeleteColumn t.Value)
                    let trAttributes =
                        this.Settings.TableElementAttributes.TR t.Value @ [
                            on.click (fun el ev ->
                                JQuery.JQuery("#" + this.Id' + " tr").RemoveClass("fruit-active-row") |> ignore
                                JQuery.JQuery.Of(el).AddClass ("fruit-active-row") |> ignore
                                currentItem.Value <- Some t.Value
                                match this.Settings.ItemSelectFunc with
                                | Some selectF -> selectF t.Value el ev
                                | _ -> ()
                            )
                        ]
                    tr trAttributes row :> Doc
                let detailRowFunction (t: Var<'DataType>) =
                    match this.Settings.ShowDetail with
                    | NoDetail -> Doc.Empty  
                    | AllRows f -> Doc.BindView f t.View
                    | SelectedRow f -> 
                        currentItem.View 
                        |> Doc.BindView(function 
                            | Some t' when this.DataSource.IdFunc t' = this.DataSource.IdFunc t.Value -> Doc.BindView f t.View
                            | _ -> Doc.Empty)
                
                // get a new index based on the sortfunction, then map that function to the listmodel, to avoid regeneration of a table on each change
//
//                let mapping itemInOriginalList =
//                    let sortedMap =
//                        view.Value
//                        |> sortFunction
//                        |> Seq.indexed
//                    Seq.tryPick (fun (indexInSortedList, item) -> 
//                        if this.DataSource.IdFunc itemInOriginalList = this.DataSource.IdFunc item && filter itemInOriginalList then 
//                            Some (indexInSortedList, this.DataSource.IdFunc itemInOriginalList)
//                        else None) sortedMap
//
//                [Doc.BindSeqCachedViewBy mapping (function
//                    | Some (_, key) ->
//                        fun _ ->
//                            let tLens = view.Lens key
//                            [rowFunction tLens; detailRowFunction tLens] |> Doc.Concat
//                    | None -> fun _ -> Doc.Empty) view.View]


                view.Value
                |> sortFunction
                |> Seq.filter filter
                |> Seq.collect (fun t -> 
                        let key = this.DataSource.IdFunc t
                        let tLens = view.Lens key
                        List.toSeq [rowFunction tLens; detailRowFunction tLens])
                |> Seq.toList

            let tableAttributes = attr.id this.Id' :: this.Settings.TableElementAttributes.Table
            let tableContents =
                if this.Settings.ShowTableHeader then
                    this.ShowHeader() :: [tbody this.Settings.TableElementAttributes.TBody <| rows this.DataSource.Model]
                else [tbody this.Settings.TableElementAttributes.TBody <| rows this.DataSource.Model]
            div [] [
                (if this.isEditable
                then (this.EditWindow currentItem idCode).Show()
                else Doc.Empty)
                table tableAttributes tableContents
            ]   
        /// <summary>
        /// Show all data in a table   
        /// </summary>
        member this.ShowTable () =
            let currentItem = Var.Create None
            this.DataSource.Read()
            div[
                attr.``class`` "fruit-table-wrapper"
            ][
                (if this.Settings.ShowErrors then this.ErrorBox() else Doc.Empty)
                this.CreateButtonShow()
                (this.DataSource.Model.View, this.SortFunctionView)
                ||> View.Map2 ( fun a b -> a, b)
                |> Doc.BindView ( fun (_, sortFunction) ->
                    this.ShowTableFilter (fun _ -> true) currentItem sortFunction
                ) 
            ]  
        /// <summary>
        /// Show all data in pages with each table with #pageSize rows
        /// </summary>
        member this.ShowTableWithPages pageSize =
            let currentItem = Var.Create None
            this.DataSource.Read()
            div[
                attr.``class`` "fruit-table-wrapper"
            ][
                (if this.Settings.ShowErrors then this.ErrorBox() else Doc.Empty)
                this.CreateButtonShow()
                (this.DataSource.Model.View, this.SortFunctionView)
                ||> View.Map2 ( fun a b -> a, b)
                |> Doc.BindView ( fun (rowdata, sortFunction) ->
                    let pages =
                        {0 ..  ((Seq.length rowdata - 1)/ pageSize)}
                        |> Seq.map (fun p ->
                            let dataList = 
                                rowdata 
                                |> sortFunction
                                |> Seq.indexed 
                                |> Seq.filter (fun (i,t) -> i / pageSize = p) 
                                |> Seq.map (fun (_,t) -> this.DataSource.IdFunc t)
                            p, this.ShowTableFilter (fun t -> Seq.exists (fun d -> d = this.DataSource.IdFunc t) dataList) currentItem sortFunction :> Doc
                        )
                    Pagination.show pages Pagination.PagerPosition.Down
                ) 
            ]
        static member Create(keyFunction : 'DataType -> 'Key) =
            {
                Id' = ""
                DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, (fun () -> async{return Array.empty}))
                Columns = [||]
                Settings = TableSettings.Default
            }  
        /// <summary>
        /// Create a read only table based on an asynchronous source  
        /// </summary>
        static member Create (Id, (keyFunction: ('DataType -> 'Key)), columns, (readFunc: unit -> Async<array<'DataType>>)) =
           {
               Table<'Key, 'DataType>.Create(keyFunction) with 
                    Id' = Id
                    Columns = columns
                    DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc)
            }
              
        /// <summary>
        /// Create a table based on an asynchronous, editable source  
        /// </summary>
        static member Create (Id, (keyFunction: 'DataType -> 'Key), columns, (readFunc: unit -> Async<array<'DataType>>), createFunc, updateFunc, deleteFunc) =
            {
                Table<'Key, 'DataType>.Create(Id,keyFunction,columns,readFunc) with 
                    DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc, createFunc, updateFunc, deleteFunc)
                }  
        /// <summary>
        /// Create a table based on a synchronous, editable source  
        /// </summary>
        static member Create (Id, (keyFunction: 'DataType -> 'Key), columns, (readFunc: unit -> array<'DataType>), createFunc, updateFunc, deleteFunc) =
           {
               Table<'Key, 'DataType>.Create(keyFunction) with 
                    Id' = Id
                    Columns = columns
                    DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc, createFunc, updateFunc, deleteFunc)
            }  
        /// <summary>
        /// Create a table based on an api, editable source  
        /// </summary>
        static member Create (Id, (keyFunction: 'DataType -> 'Key), columns, (readFunc: string)) =
           {
                Id' = Id
                Columns = columns
                DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc)
                Settings = TableSettings.Default
            }
        static member Create (Id, (keyFunction: 'DataType -> 'Key), columns, (readFunc: string), createFunc, updateFunc, deleteFunc) =
           {
                Id' = Id
                Columns = columns
                DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc, createFunc, updateFunc, deleteFunc)
                Settings = TableSettings.Default
            }




