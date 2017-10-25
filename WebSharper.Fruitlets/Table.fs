namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

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
            /// Whether a detail wil be shown for no rows, all rows or the selected row
            /// The row detail will be show in a separate row underneath the main one
            ShowDetail: TableRowDetail<'DataType>

            /// Optional custom buttons
            AddButton: ButtonStatic option
            EditButton: ButtonFromItem<'DataType> option
            DeleteButton: ButtonFromItem<'DataType> option

            /// Optional custom modal buttons
            ModalSaveButton: (ButtonFromItem<'DataType> option)
            ModalSaveValidation: Validation<'DataType> option
            ModalCancelButton: ButtonStatic option
            ModalDeleteButton: ButtonFromItem<'DataType> option
            ModalDeleteValidation: Validation<'DataType> option
            
            /// ShowErrors. On by default
            ShowErrors: bool

            /// Set attributes for different elements of the table
            TableElementAttributes: TableElementAttributes<'DataType>

            /// Shown as description in modal header
            ModalUpdateHeader: 'DataType -> string
            ModalDeleteHeader: 'DataType -> string

            /// Show Table header
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
            /// the id of the table    
            Id': string
            /// The datasource for the table rows
            DataSource: DataSource.DS<'Key,'DataType>
            /// The Column type generates a column from type 'DataType
            Columns: Column<'DataType> []
            /// CustomSettings
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
                    divAttr[
                        attr.``class`` "alert alert-danger alert-dismissable"
                    ][
                        aAttr[
                            attr.href "#"
                            attr.``class`` "close"
                            attr.``data-`` "dismiss" "alert"
                            on.click (fun _ _ -> errorVar.Value <- "")
                        ][iAttr[attr.``class`` "fa fa-close"][]]
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
            buttonAttr
                (saveButtonAttrs @ extraAttributes ) 
                saveButtonContent :> Doc
        member private this.DeleteButtonShow (t : 'DataType) =
            let modalId = sprintf "confirm-delete-%s-%s" this.Id' <| (this.DataSource.IdFunc t).ToString()
            let confirmDialog () =

                let modalwindow =
                    {
                        Header =  
                            h2Attr 
                                [attr.``class`` "fruit-modal-header"] 
                                [text <| this.Settings.ModalDeleteHeader t]
                        Body = text "Are you sure you want to delete this item?"
                        Footer =
                            div[
                                this.ModalDeleteButtonShow t
                                this.ModalCancelButtonShow()
                            ]
                        Size = Modal.Small
                        Id = modalId
                    }
                modalwindow.Show()
                
            let attrs, content =
                match this.Settings.DeleteButton with
                | Some button -> button.Attributes t, button.Docs t
                | None -> [attr.``class``  "btn btn-danger fruit-btn fruit-btn-delete fruit-btn-icon-only"], [ iAttr[ attr.``class`` "fa fa-trash"][] ]

            divAttr[attr.style "display: inline-block"][
                confirmDialog()
                Modal.Button modalId attrs content
            ] :> Doc
        member private this.EditFooter windowId errorVar (item : 'DataType option) =
            match item with
            | Some v ->
                div[
                    (if this.DataSource.UpdateFunc then this.SaveButton windowId errorVar v else Doc.Empty)
                    this.ModalCancelButtonShow()
                ] :> Doc
            | None -> Doc.Empty
        member private this.ModalCancelButtonShow () =
            let cancelButtonAttrs, cancelButtonContent =
                match this.Settings.ModalCancelButton with
                | Some button -> button.Attributes, button.Docs
                | None -> [attr.``class`` "btn btn-secondary fruit-btn fruit-btn-cancel"], [ iAttr[ attr.``class`` "fa fa-close"][]; text " Close" ]
            buttonAttr(attr.``data-`` "dismiss" "modal" :: cancelButtonAttrs) cancelButtonContent 
        member private this.ModalDeleteButtonShow (t) =
            let deleteButtonAttrs, deleteButtonContent =
                match this.Settings.ModalDeleteButton with
                | Some button -> button.Attributes t, button.Docs t
                | None -> [attr.``class`` "btn btn-danger fruit-btn fruit-btn-delete"], [ iAttr[ attr.``class`` "fa fa-trash"][]; text " Delete" ]
            buttonAttr
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
                    | Some editField, _, None -> editField.show column.Name item
                    | Some editField, _, Some attrList -> 
                        // assuming attributes are pretty stable
                        let stableAttrList = fun t ->
                            [
                                attr.id column.Name
                                attr.name column.Name
                                attr.``class`` "form-control fruit-form-control"
                            ] @ attrList t
                        editField.show(column.Name, stableAttrList, editField.formWrapper column.Name) item
                )
                |> Array.toList
                |> formAttr [attr.``class`` "fruit-form"]

            {
                Id = windowId
                Header = 
                    h2Attr 
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
                | None -> [attr.``class``  "btn btn-primary fruit-btn fruit-btn-edit fruit-btn-icon-only"], [ iAttr[ attr.``class`` "fa fa-edit"][] ]

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
                    | None -> [attr.``class`` "btn btn-success fruit-btn fruit-btn-create fruit-btn-icon-only"], [ iAttr[ attr.``class`` "fa fa-plus"][] ]

                div[
                    (this.EditWindow currentItem newModalId).Show()
                    buttonAttr 
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
                    [| tdAttr[ attr.``class`` "fruit-edit-btn-column" ][] :> Doc |] 
                else [||]

            this.Columns
            |> Array.indexed
            |> Array.filter ( fun (_, column) ->
                column.Permission.Table <> Permission.Invisible )
            |> Array.map (fun (index, column) ->
                column.showHeader index this.DataSource )
            |> flip Array.append extracol
            |> fun headerRow -> 
                theadAttr
                    this.Settings.TableElementAttributes.THead
                    [trAttr this.Settings.TableElementAttributes.THeadRow headerRow] :> Doc
        member private this.ShowTableFilter (filter: 'DataType -> bool ) (currentItem : Var<'DataType option>) sortFunction =
            let idCode = sprintf "%s-edit-%i" this.Id' ( int <| (Math.Random() * 100000.) + 1.)
            let editdeleteColumn t =
                if this.isEditable || this.isDeletable
                then
                    [
                        tdAttr[attr.``class`` "fruit-edit-btn-column"][
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
            let rows view =
                let rowFunction t =
                    let row =
                        this.Columns
                        |> Array.filter ( fun column ->
                            column.Permission.Table <> Permission.Invisible )
                        |> Array.map ( fun column ->
                            column.showRow t :> Doc )
                        |> flip Array.append ( editdeleteColumn t )
                    let trAttributes =
                        this.Settings.TableElementAttributes.TR t @ [
                            on.click (fun el ev ->
                                JQuery.JQuery("#" + this.Id' + " tr").RemoveClass("fruit-active-row").RemoveAttr("style") |> ignore
                                el.SetAttribute ("class", "fruit-active-row")
                                currentItem.Value <- Some t
                                match this.Settings.ItemSelectFunc with
                                | Some selectF -> selectF t el ev
                                | None -> ()
                            )
                        ]
                    trAttr trAttributes row :> Doc
                let detailRowFunction t =
                    match this.Settings.ShowDetail with
                    | NoDetail -> Doc.Empty  
                    | AllRows f -> f t :> Doc 
                    | SelectedRow f -> 
                        currentItem.View 
                        |> Doc.BindView(function 
                            | Some t' when this.DataSource.IdFunc t' = this.DataSource.IdFunc t -> f t :> Doc
                            | _ -> Doc.Empty)
                view
                |> sortFunction
                |> Seq.filter filter
                |> Seq.collect (fun t -> List.toSeq [rowFunction t; detailRowFunction t])
                |> Seq.toList
            let tableAttributes = attr.id this.Id' :: this.Settings.TableElementAttributes.Table
            let tableContents =
                if this.Settings.ShowTableHeader then
                    this.ShowHeader() :: [tbodyAttr this.Settings.TableElementAttributes.TBody <| rows this.DataSource.Model.Value]
                else [tbodyAttr this.Settings.TableElementAttributes.TBody <| rows this.DataSource.Model.Value]
            div [
                (if this.isEditable
                then (this.EditWindow currentItem idCode).Show()
                else Doc.Empty)
                tableAttr tableAttributes tableContents
            ]
        /// Show all data in a table
        member this.ShowTable () =
            let currentItem = Var.Create None
            this.DataSource.Read()
            divAttr[
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
        /// Show all data in pages with 1 table each of length pageSize
        member this.ShowTableWithPages pageSize =
            // let currentPage = Var.Create 0
            let currentItem = Var.Create None
            this.DataSource.Read()
            divAttr[
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
        /// Create a read only table based on an asynchronous source
        static member Create (Id, (keyFunction: ('DataType -> 'Key)), columns, (readFunc: unit -> Async<array<'DataType>>)) =
           {
               Table<'Key, 'DataType>.Create(keyFunction) with 
                    Id' = Id
                    Columns = columns
                    DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc)
            }
            
        /// Create a table based on an asynchronous, editable source
        static member Create (Id, (keyFunction: 'DataType -> 'Key), columns, (readFunc: unit -> Async<array<'DataType>>), createFunc, updateFunc, deleteFunc) =
            {
                Table<'Key, 'DataType>.Create(Id,keyFunction,columns,readFunc) with 
                    DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc, createFunc, updateFunc, deleteFunc)
                }
        /// Create a table based on a synchronous, editable source
        static member Create (Id, (keyFunction: 'DataType -> 'Key), columns, (readFunc: unit -> array<'DataType>), createFunc, updateFunc, deleteFunc) =
           {
               Table<'Key, 'DataType>.Create(keyFunction) with 
                    Id' = Id
                    Columns = columns
                    DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc, createFunc, updateFunc, deleteFunc)
            }
        /// Create a table based on an api, editable source
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




