namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

// Bootstrap table
[<JavaScript>]
module Table =
    open Input

    [<Inline "$obj[$field]">]
    let private JSGetIntField obj field = X<int>
    [<Inline "$obj[$field]">]
    let private JSGetFloatField obj field = X<float>
    [<Inline "$obj[$field]">]
    let private JSGetStringField obj field = X<string>
    [<Inline "$obj[$field]">]
    let private JSGetBoolField obj field = X<bool>
    [<Inline "$obj[$field]">]
    let private JSGetObjField obj field = X<obj>
    [<Inline "$obj[$field]">]
    let private JSGetTimeSpanField obj field = X<System.TimeSpan>
    [<Inline "$obj[$field]">]
    let private JSGetDateTimeField obj field = X<System.DateTime>

    [<Inline "$obj[$field]">]
    let private JSGetIntFieldOption obj field = X<int option>
    [<Inline "$obj[$field]">]
    let private JSGetFloatFieldOption obj field = X<float option>
    [<Inline "$obj[$field]">]
    let private JSGetStringFieldOption obj field = X<string option>
    [<Inline "$obj[$field]">]
    let private JSGetBoolFieldOption obj field = X<bool option>
    [<Inline "$obj[$field]">]
    let private JSGetObjFieldOption obj field = X<obj option>
    [<Inline "$obj[$field]">]
    let private JSGetTimeSpanFieldOption obj field = X<System.TimeSpan option>
    [<Inline "$obj[$field]">]
    let private JSGetDateTimeFieldOption obj field = X<System.DateTime option>

    [<Inline "$obj[$field] = $value">]
    let private JSSetField obj field value = X<Unit>

    let inline private getter name jSGet = (fun t -> jSGet t name)
    let private defaultSetter name t v = JSSetField t name v; t

    let private flip f a b = f b a

    type TableClass =
        | Striped
        | Bordered
        | Custom of string
        member this.show =
            match this with
            | Striped -> "table-striped"
            | Bordered -> "table-bordered"
            | Custom cl -> cl

    type FieldClass =
        | Int
        | Float
        | String
        | Text
        | Bool
        | Time
        | Date
        | Select of Map<int,string>
        | SelectDyn of Var<Map<int,string>>
        | Optional of FieldClass

    type ColumnPermission =
        {
            Table : Permission
            Form: Permission
        }
    and Permission =
        | Invisible
        | Read
        | ReadWrite

    type Column<'DataType> =
        {
            Name: string
            Header: (unit -> Doc) option
            AttrList: ('DataType -> list<Attr>) option
            DocList: ('DataType -> list<Doc>)
            SortFunction: ('DataType -> Sort.SortableType) option
            EditFieldAttrList: (Var<'DataType option> -> list<Attr>) option
            EditField: InputType<'DataType> option
            Permission: ColumnPermission
        }
        member this.showHeader index (ds : DataSource.DS<_,'DataType>) =
            let headerField () =
                match this.Header with
                | Some header' -> header' ()
                | None -> text this.Name
            match this.SortFunction with
            | Some sortFunc ->
                tdAttr([
                        on.click ( fun _ _ ->
                            ds.SortDirection <- ds.SortDirection.Flip index
                            Console.Log ds.SortDirection
                            (sortFunc, ds.Model.Value)
                            ||> ds.SortDirection.SortFunc index
                            |> ds.Model.Set
                        )
                ] )[
                    headerField ()
                    iAttr[
                        attr.style "color: #aaa;"
                        attr.``class`` <| ds.SortDirection.FAClass index
                    ][] :> Doc
                ] :> Doc
            | None ->
                td [
                    headerField ()
                ] :> Doc

        member this.showRow item =
//            match this.Permission.Table with
//            | Invisible -> []
//            | Read -> (this.DocList item)
//            | ReadWrite -> [
//                match this.EditField with
//                | Some editfield -> editfield.show "" item]
            this.DocList item
            |>
            match this.AttrList with
            | Some attrList -> tdAttr ( attrList item )
            | None -> td

        static member empty =
            {Name = ""; Header = None; AttrList = None; DocList = (fun t -> List.empty); SortFunction = None; EditField = None; EditFieldAttrList = None; Permission = {Table = Read; Form = ReadWrite }}

        static member SimpleColumn (name, get : ('DataType -> obj)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('DataType -> int)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('DataType -> string)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text <| get t ])}
        static member SimpleColumn (name, get : ('DataType -> Date)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('DataType -> float)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('DataType -> decimal)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('DataType -> bool)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleSortColumn (name, get : ('DataType -> int)) =
            { Column<'DataType>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.Int <| get t )}
        static member SimpleSortColumn (name, get : ('DataType -> string)) =
            { Column<'DataType>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.String <| get t )}
        static member SimpleSortColumn (name, get : ('DataType -> float)) =
            { Column<'DataType>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.Float <| get t )}
        static member SimpleSortColumn (name, get : ('DataType -> decimal)) =
            { Column<'DataType>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.Float << float <| get t )}

        /// A string field
        static member EditColumn (name, get : ('DataType -> string), set : ('DataType -> string -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text <| get t ])
                SortFunction = Some (fun t -> Sort.String <| get t )
                EditField = Some (Input.String (get, set))
            }
        /// A string option field
        static member EditColumn (name, get : ('DataType -> string option), set : ('DataType -> string option -> 'DataType)) =
            let SomeOrDefault = function
                | Some t' -> t'
                | None -> "-"
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [ text << SomeOrDefault <| get t])
                SortFunction = Some (fun t -> Sort.String << SomeOrDefault <| get t )
                EditField = Some (Input.StringOption ( get, set)) }
        /// An int field
        static member EditColumn (name, get : ('DataType -> int), set : ('DataType -> int -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << string <| get t ])
                SortFunction = Some (fun t -> Sort.Int <| get t )
                EditField = Some (Input.Int (get, set)) }
        /// An int option field
        static member EditColumn (name, get : ('DataType -> int option), set : ('DataType -> int option -> 'DataType)) =
            let SomeOrDefaultString = function
                | Some t' -> string t'
                | None -> "-"
            let SomeOrDefaultSort = function
                | Some t' -> t'
                | None -> 0
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << SomeOrDefaultString <| get t ])
                SortFunction = Some (fun t -> Sort.Int << SomeOrDefaultSort <| get t )
                EditField = Some (Input.IntOption (get, set)) }

        static member EditColumn (name, get : ('DataType -> float), set : ('DataType -> float -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << string <| get t ])
                SortFunction = Some (fun t -> Sort.Float <| get t )
                EditField = Some (Input.Float (get, set)) }
        static member EditColumn (name, get : ('DataType -> float option), set : ('DataType -> float option -> 'DataType)) =
            let SomeOrDefaultString = function
                | Some t' -> string t'
                | None -> "-"
            let SomeOrDefaultSort = function
                | Some t' -> t'
                | None -> 0.
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << SomeOrDefaultString <| get t ])
                SortFunction = Some (fun t -> Sort.Float << SomeOrDefaultSort <| get t )
                EditField = Some (Input.FloatOption (get, set)) }

        static member EditColumn (name, get : ('DataType -> bool), set : ('DataType -> bool -> 'DataType)) =
            { Column<'DataType>.empty with 
                Name = name
                DocList = (fun t -> [text <| if get t then "\u00D7" else "" ])
                SortFunction = Some (fun t -> Sort.Bool <| get t )
                EditField = Some (Input.Bool (get, set)) }

        /// TimeSpan Field
        static member EditTimeSpanColumn (name, get : ('DataType -> System.TimeSpan), set : ('DataType -> System.TimeSpan -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << Time.showTime <| (get t).Ticks ])
                SortFunction = Some (fun t -> Sort.Int << int <| (get t).Ticks )
                EditField = Some (Input.Time (get, set)) }

        /// TimeSpan option Field
        static member EditTimeSpanColumn (name, get : ('DataType -> System.TimeSpan option), set : ('DataType -> System.TimeSpan option -> 'DataType)) =
            let SomeOrDefaultString (t: System.TimeSpan option) =
                match t with
                | Some t' -> Time.showTime (t'.Ticks)
                | None -> "-"
            let SomeOrDefaultSort (t: System.TimeSpan option) =
                match t with
                | Some t' -> int (t'.Ticks)
                | None -> 0
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << SomeOrDefaultString <| get t ])
                SortFunction = Some (fun t -> Sort.Int << SomeOrDefaultSort <| get t )
                EditField = Some (Input.TimeOption (get, set))
            }
        /// Under construction
        static member EditDateColumn (name, get : ('DataType -> System.DateTime), set : ('DataType -> System.DateTime -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t ->
                    let dt = get t |> fun dt' -> new Date(dt'.Year,dt'.Month - 1, dt'.Day)
                    [text <| dt.ToDateString() ])
                SortFunction = Some (fun t -> Sort.DateTime <| (get t ))
                EditField = Some (Input.Date (get, set)) }
        /// Under construction
        static member EditDateColumn (name, get : ('DataType -> System.DateTime option), set : ('DataType -> System.DateTime option -> 'DataType)) =
            let SomeOrDefaultString (dt: System.DateTime option) =
                match dt with
                | Some dt' -> (new Date(dt'.Year,dt'.Month - 1, dt'.Day)).ToDateString()
                | None -> "-"
            let SomeOrDefaultSort = function
                | Some t' -> t'
                | None -> System.DateTime.Parse("01-01-1970")
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << SomeOrDefaultString <| get t ])
                SortFunction = Some (fun t -> Sort.DateTime << SomeOrDefaultSort <| get t )
                EditField = Some (Input.DateOption (get, set)) }
                
        /// This column can be used to represent and change foreign keys
        static member EditSelectColumn (name, get : ('DataType -> int), set : ('DataType -> int -> 'DataType), optionMap : Var<Map<int,string>>) =
            let findInMap (map : Map<int,string>) key =
                    match map.TryFind key with
                    | Some v -> v
                    | None -> ""
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [textView <| View.Map (fun map -> findInMap map (get t)) optionMap.View])
                SortFunction = Some (fun t -> Sort.String <| findInMap optionMap.Value (get t) )
                EditField = Some (Input.Select (get, set, optionMap)) }
        /// This column can be used to represent and change foreign keys
        static member EditSelectColumn (name, get : ('DataType -> string), set : ('DataType -> string -> 'DataType), optionMap : Var<Map<string,string>>) =
            let findInMap (map : Map<string,string>) key =
                    match map.TryFind key with
                    | Some v -> v
                    | None -> ""
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [textView <| View.Map (fun map -> findInMap map (get t)) optionMap.View])
                SortFunction = Some (fun t -> Sort.String <| findInMap optionMap.Value (get t) )
                EditField = Some (Input.SelectWithString (get, set, optionMap)) }

        /// This column can be used to represent and change Nullable foreign keys
        static member EditSelectColumn (name, get : ('DataType -> int option), set : ('DataType -> int option -> 'DataType), optionMap : Var<Map<int,string>>) =
            let findInMap (map : Map<int,string>) (key: int option) =
                match key with
                | None -> "-"
                | Some k ->
                    match  map.TryFind k with
                    | Some v -> v
                    | None -> "-"
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [textView <| View.Map (fun map -> findInMap map (get t)) optionMap.View])
                SortFunction = Some (fun t -> Sort.String <| findInMap optionMap.Value (get t) )
                EditField = Some (Input.SelectOption (get, set, optionMap)) }

        /// Use the parse functions with caution. They are not typesafe and meant to be used on the client side in combination with Reflection on the server side.
        static member Parse(name, _type) =
            let IntGetter = (fun (t: 'DataType) -> JSGetIntField t name)
            let IntOptionGetter = (fun (t: 'DataType) -> JSGetIntFieldOption t name)
            match _type with
            | Int -> Column<'DataType>.EditColumn(name, IntGetter, defaultSetter name)
            | String -> Column<'DataType>.EditColumn(name, getter name JSGetStringField, defaultSetter name)
            | Text ->
                let column = Column<'DataType>.EditColumn(name, getter name JSGetStringField, defaultSetter name)
                match column.EditField.Value with
                | InputType.String obj -> {column with EditField = Some <| InputType.Text obj}
                | _ -> column
            | Bool -> Column.EditColumn(name, getter name JSGetBoolField, defaultSetter name)
            | Float -> Column.EditColumn(name, getter name JSGetFloatField, defaultSetter name)
            | Time -> Column.EditTimeSpanColumn(name, getter name JSGetTimeSpanField, defaultSetter name)
            | Date -> Column.EditDateColumn(name, getter name JSGetDateTimeField, defaultSetter name)
            | Select m ->
                let varmap = Var.Create m
                Column.EditSelectColumn(name, IntGetter, defaultSetter name, varmap)
            | SelectDyn m -> Column.EditSelectColumn(name, IntGetter, defaultSetter name, m)
            | Optional field ->
                match field with
                | Int -> Column.EditColumn(name, IntOptionGetter, defaultSetter name)
                | String -> Column.EditColumn(name, getter name JSGetStringFieldOption, defaultSetter  name)
                | Text ->
                    let column = Column.EditColumn(name, getter name JSGetStringFieldOption, defaultSetter name)
                    // ???
                    match column.EditField.Value with
                    | InputType.String obj -> {column with EditField = Some <| InputType.Text obj}
                    | _ -> column
                | Bool -> Column.EditColumn(name, getter name JSGetBoolField, defaultSetter name)
                | Float -> Column.EditColumn(name, getter name JSGetFloatFieldOption, defaultSetter name)
                | Time -> Column.EditTimeSpanColumn(name, (fun t -> (JSGetTimeSpanFieldOption t name)),(fun t v -> JSSetField t name v; t))
                | Date -> Column.EditDateColumn(name, getter name JSGetDateTimeFieldOption, defaultSetter name)
                | Select m ->
                    let varmap = Var.Create m
                    Column.EditSelectColumn(name, IntOptionGetter, defaultSetter name, varmap)
                | SelectDyn m -> Column.EditSelectColumn(name, IntOptionGetter, defaultSetter name, m)
                | Optional field' -> Column<'DataType>.Parse (name, Optional field')
        /// Use the parse functions with caution. They are not typesafe and meant to be used on the client side in combination with Reflection on the server side.
        static member Parse(name, _type, header) =
            { Column<'DataType>.Parse(name, _type) with Header = header }

    type TableRowDetail<'DataType> =
        | NoDetail
        | AllRows of ('DataType -> Elt)
        | SelectedRow of ('DataType -> Elt)        
        
    type RowButtonStatic =
        {
            Attributes:  Attr list
            Docs:  Doc list
        }
    type RowButtonFromItem<'DataType> =
        {
            Attributes: 'DataType -> Attr list
            Docs: 'DataType -> Doc list
        }

    type Table<'Key, 'DataType> when 'Key : equality  =
        {
            /// the id of the table    
            Id': string
            /// Set default table classes Striped | Bordered | Custom of string
            Class: TableClass []
            /// The datasource for the table rows
            DataSource: DataSource.DS<'Key,'DataType>
            /// The Column type generates a column from type 'DataType
            Columns: Column<'DataType> []
            /// Optional additional attributes added to the table object
            Attributes: Attr []
            /// Whether a detail wil be shown for no rows, all rows or the selected row
            /// The row detail will be show in a separate row uderneath the main one
            ShowDetail: TableRowDetail<'DataType>
            /// Shown as description in form header
            ItemDescription: 'DataType -> string
            /// Optional custom buttons
            AddButton: RowButtonStatic option
            EditButton: RowButtonFromItem<'DataType> option
            DeleteButton: RowButtonFromItem<'DataType> option

            // To implement:
            // ModalSaveButton: RowButtonFromItem<'DataType> option
            // ModalCancelButton: RowButtonStatic option
            //
            /// ShowErrors. On by default
            ShowErrors: bool

        }
        member this.ErrorStatus =
            match this.DataSource.CrudFunctions with
            | DataSource.CRUD.Rpc rpc -> rpc.ErrorStatus
            | DataSource.CRUD.Synchronous syn -> syn.ErrorStatus
        member this.ErrorBox () =
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
                            on.click (fun _ _ -> this.ErrorStatus.Value <- "")
                        ][iAttr[attr.``class`` "fa fa-close"][]]
                        text msg
                    ] :> Doc
            ) this.ErrorStatus.View
        member this.isEditable =
            this.Columns
            |> Array.exists (fun c -> c.EditField.IsSome)
        member this.isDeletable = this.DataSource.DeleteFunc
        member private this.SaveButton (t : 'DataType) =
            buttonAttr[
                attr.``class`` "btn btn-primary fruit-btn fruit-btn-save"
                attr.``data-`` "dismiss" "modal"
                on.click (fun el ev ->
                    this.DataSource.Update t
                    )][text "Save"] :> Doc
        member private this.DeleteButtonShow (t : 'DataType) =
            let modalId = sprintf "confirm-delete-%s-%s" this.Id' <| (this.DataSource.IdFunc t).ToString()
            let confirmDialog () =
                let modalwindow =
                    Modal.Window.Create
                        modalId
                        Doc.Empty
                        (text "Are you sure you want to delete this item?")
                        (div[
                            buttonAttr[
                                attr.``class`` "btn btn-danger fruit-btn fruit-btn-delete"
                                attr.``data-`` "dismiss" "modal"
                                on.click (fun el ev ->
                                    // TODO: confirmation popup
                                    this.DataSource.Delete t
                                    )][text "Delete"] :> Doc
                            buttonAttr[
                                attr.``class`` "btn btn-default fruit-btn fruit-btn-cancel"
                                attr.``data-`` "dismiss" "modal"][text "Cancel"] :> Doc

                        ])
                        Modal.Small
                modalwindow.Show()
                
            let attrs, content =
                match this.DeleteButton with
                | Some button -> button.Attributes t, button.Docs t
                | None -> [attr.``class``  "btn btn-danger fruit-btn fruit-btn-delete fruit-btn-icon-only"], [ iAttr[ attr.``class`` "fa fa-trash"][] ]

            divAttr[attr.style "display: inline-block"][
                confirmDialog()
                Modal.Button modalId attrs content
            ] :> Doc
        member private this.EditFooter (t : 'DataType option) =
            match t with
            | Some v ->
                div[
                    buttonAttr[attr.``class`` "btn btn-secondary fruit-btn fruit-btn-cancel"; attr.``data-`` "dismiss" "modal"][text "Close"] :> Doc
                    (if this.DataSource.UpdateFunc
                    then this.SaveButton v
                    else Doc.Empty)
                ] :> Doc
            | None -> Doc.Empty
        member this.EditWindow (item : Var<'DataType option>) windowId =
            let editForm () =
                this.Columns
                |> Array.filter ( fun column ->
                    column.Permission.Form <> Invisible
                )
                |> Array.map (fun column ->
                    match (column.EditField, column.Permission.Form, column.EditFieldAttrList) with
                    | None, _, _  
                    | _, Read, _ -> (Input.Disabled column.DocList).show column.Name item
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

            Modal.Window.Create
                windowId
                (h2Attr [attr.``class`` "fruit-modal-header"] [textView <| View.Map (fun t ->  (match t with | Some t' ->  sprintf "Item %s" (this.ItemDescription t') | None -> "")) item.View ] )
                (editForm ())
                (Doc.BindView (fun t -> this.EditFooter t) item.View)
                Modal.WindowSize.Normal
        member private this.EditButtonShow idCode (currentItem: Var<'DataType option>) (t) =
            let attrs, content =
                match this.EditButton with
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
                    match this.AddButton with
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
                column.Permission.Table <> Invisible )
            |> Array.map (fun (index, column) ->
                column.showHeader index this.DataSource )
            |> flip Array.append extracol
            |> fun headerRow -> theadAttr[attr.``class`` "fruit-table-head"][trAttr [attr.``class`` "fruit-table-head-row"] headerRow] :> Doc
        member private this.ShowTableFilter (filter: 'DataType -> bool ) (currentItem : Var<'DataType option>) =
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
                            column.Permission.Table <> Invisible )
                        |> Array.map ( fun column ->
                            column.showRow t :> Doc )
                        |> flip Array.append ( editdeleteColumn t )
                    trAttr [
                        on.click (fun el ev ->
                            JQuery.JQuery("#" + this.Id' + " tr").RemoveClass("fruit-active-row").RemoveAttr("style") |> ignore
                            el.SetAttribute ("class", "fruit-active-row")
                            match this.DataSource.ItemSelectFunc with
                            | Some selectF -> selectF t el ev
                            | None -> ()
                        )
                    ] row :> Doc
                let detailRowFunction t =
                    match this.ShowDetail with
                    | NoDetail -> Doc.Empty  
                    | AllRows f -> f t :> Doc 
                    | SelectedRow f -> f t :> Doc      
                view
                |> Seq.filter filter
                |> Seq.collect (fun t -> List.toSeq [rowFunction t; detailRowFunction t])
                |> Seq.toList
            let classes =
                if Array.isEmpty this.Class then "table fruit-table"
                else
                    this.Class
                    |> Array.map (fun cl -> cl.show)
                    |> Array.append [|"table"; "fruit-table"|]
                    |> String.concat " "
            div [
                (if this.isEditable
                then (this.EditWindow currentItem idCode).Show()
                else Doc.Empty)
                tableAttr [
                    attr.``class`` classes
                    attr.id this.Id'
                    ]
                    (this.ShowHeader() :: [tbodyAttr [attr.``class`` "fruit-table-body"] <| rows this.DataSource.Model.Value])
            ]
        /// Show all data in a table
        member this.ShowTable () =
            let currentItem = Var.Create None
            this.DataSource.Read()
            divAttr[
                attr.``class`` "fruit-table-wrapper"
            ][
                (if this.ShowErrors then this.ErrorBox() else Doc.Empty)
                this.CreateButtonShow()
                Doc.BindView ( fun _ ->
                    this.ShowTableFilter (fun _ -> true) currentItem
                ) this.DataSource.Model.View
            ]
        /// Show all data in pages with 1 table each of length pageSize
        member this.ShowTableWithPages pageSize =
            let currentPage = Var.Create 0
            let currentItem = Var.Create None
            this.DataSource.Read()
            divAttr[
                attr.``class`` "fruit-table-wrapper"
            ][
                (if this.ShowErrors then this.ErrorBox() else Doc.Empty)
                this.CreateButtonShow()
                Doc.BindView ( fun rowdata ->
                    let pages =
                        {0 ..  ((Seq.length rowdata - 1)/ pageSize)}
                        |> Seq.map (fun p ->
                            let dataList = rowdata |> Seq.indexed |> Seq.filter (fun (i,t) -> i / pageSize = p) |> Seq.map (fun (_,t) -> this.DataSource.IdFunc t)
                            p, this.ShowTableFilter (fun t -> Seq.exists (fun d -> d = this.DataSource.IdFunc t) dataList) currentItem :> Doc
                        )
                    Pagination.show pages Pagination.PagerPosition.Down
                ) this.DataSource.Model.View
            ]
        static member empty =
            {
                Id' = ""
                Class = Array.empty
                DataSource = DataSource.DS<'Key,'DataType>.Create (id, (fun () -> async{return Array.empty}))
                Columns = [||]
                Attributes = [||]
                ShowDetail = NoDetail
                ItemDescription = (fun _ -> "")
                AddButton = None
                EditButton = None
                DeleteButton = None
                ShowErrors = true
            }
        /// Create a read only table based on an asynchronous source
        static member Create (Id, (keyFunction: ('DataType -> 'Key)), columns, (readFunc: unit -> Async<array<'DataType>>)) =
           {
                Id' = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc)
                Attributes = [||]
                ShowDetail = NoDetail
                ItemDescription = (fun _ -> "")
                AddButton = None
                EditButton = None
                DeleteButton = None
                ShowErrors = true
            }
        /// Create a table based on an asynchronous, editable source
        static member Create (Id, (keyFunction: 'DataType -> 'Key), columns, (readFunc: unit -> Async<array<'DataType>>), createFunc, updateFunc, deleteFunc) =
           {
                Id' = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc, createFunc, updateFunc, deleteFunc)
                Attributes = [||]
                ShowDetail = NoDetail
                ItemDescription = (fun _ -> "")
                AddButton = None
                EditButton = None
                DeleteButton = None
                ShowErrors = true
            }
        /// Create a table based on an asynchronous, editable source + a function for when an item is selected
        static member Create (Id, (keyFunction: 'DataType -> 'Key), columns, (itemSelectFunc), (readFunc: unit -> Async<array<'DataType>>), createFunc, updateFunc, deleteFunc) =
           {
                Id' = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc, itemSelectFunc, createFunc, updateFunc, deleteFunc)
                Attributes = [||]
                ShowDetail = NoDetail
                ItemDescription = (fun _ -> "")
                AddButton = None
                EditButton = None
                DeleteButton = None
                ShowErrors = true
            }
        /// Create a table based on a synchronous, editable source
        static member Create (Id, (keyFunction: 'DataType -> 'Key), columns, (readFunc: unit -> array<'DataType>), createFunc, updateFunc, deleteFunc) =
           {
                Id' = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc, createFunc, updateFunc, deleteFunc)
                Attributes = [||]
                ShowDetail = NoDetail
                ItemDescription = (fun _ -> "")
                AddButton = None
                EditButton = None
                DeleteButton = None
                ShowErrors = true
            }





