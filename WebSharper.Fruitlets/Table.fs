namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

// Bootstrap table
[<JavaScript>]
module Table =
    open Form

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
            {Name = ""; Header = None; AttrList = None; DocList = (fun t -> List.empty); SortFunction = None; EditField = None; Permission = {Table = Read; Form = ReadWrite }}

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
                EditField = Some (Form.String (get, set))
            }
        /// A string option field
        static member EditColumn (name, get : ('DataType -> string option), set : ('DataType -> string option -> 'DataType)) =
            let SomeOrDefault = function
                |Some t' -> t'
                |None -> "-"
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [ text << SomeOrDefault <| get t])
                SortFunction = Some (fun t -> Sort.String << SomeOrDefault <| get t )
                EditField = Some (Form.StringOption ( get, set))
            }
        /// An int field
        static member EditColumn (name, get : ('DataType -> int), set : ('DataType -> int -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << string <| get t ])
                SortFunction = Some (fun t -> Sort.Int <| get t )
                EditField = Some (Form.Int (get, set))
            }
        /// An int option field
        static member EditColumn (name, get : ('DataType -> int option), set : ('DataType -> int option -> 'DataType)) =
            let SomeOrDefaultString = function
                |Some t' -> string t'
                |None -> "-"
            let SomeOrDefaultSort = function
                |Some t' -> t'
                |None -> 0
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << SomeOrDefaultString <| get t ])
                SortFunction = Some (fun t -> Sort.Int << SomeOrDefaultSort <| get t )
                EditField = Some (Form.IntOption (get, set))
            }

        static member EditColumn (name, get : ('DataType -> float), set : ('DataType -> float -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << string <| get t ])
                SortFunction = Some (fun t -> Sort.Float <| get t )
                EditField = Some (Form.Float (get, set))
            }
        static member EditColumn (name, get : ('DataType -> float option), set : ('DataType -> float option -> 'DataType)) =
            let SomeOrDefaultString = function
                |Some t' -> string t'
                |None -> "-"
            let SomeOrDefaultSort = function
                |Some t' -> t'
                |None -> 0.
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << SomeOrDefaultString <| get t ])
                SortFunction = Some (fun t -> Sort.Float << SomeOrDefaultSort <| get t )
                EditField = Some (Form.FloatOption (get, set))
            }

        static member EditColumn (name, get : ('DataType -> bool), set : ('DataType -> bool -> 'DataType)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text <| if get t then "\u00D7" else "" ]); SortFunction = Some (fun t -> Sort.Bool <| get t ); EditField = Some (Form.Bool (get, set))}

        /// TimeSpan Field
        static member EditTimeSpanColumn (name, get : ('DataType -> System.TimeSpan), set : ('DataType -> System.TimeSpan -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << Time.showTime <| (get t).Ticks ])
                SortFunction = Some (fun t -> Sort.Int << int <| (get t).Ticks )
                EditField = Some (Form.Time (get, set))
            }
        /// TimeSpan option Field
        static member EditTimeSpanColumn (name, get : ('DataType -> System.TimeSpan option), set : ('DataType -> System.TimeSpan option -> 'DataType)) =
            let SomeOrDefaultString (t: System.TimeSpan option) =
                match t with
                |Some t' -> Time.showTime (t'.Ticks)
                |None -> "-"
            let SomeOrDefaultSort (t: System.TimeSpan option) =
                match t with
                |Some t' -> int (t'.Ticks)
                |None -> 0
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << SomeOrDefaultString <| get t ])
                SortFunction = Some (fun t -> Sort.Int << SomeOrDefaultSort <| get t )
                EditField = Some (Form.TimeOption (get, set))
            }
        /// Under construction
        static member EditDateColumn (name, get : ('DataType -> System.DateTime), set : ('DataType -> System.DateTime -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t ->
                    let dt = get t |> fun dt' -> new Date(dt'.Year,dt'.Month - 1, dt'.Day)
                    [text <| dt.ToDateString() ])
                SortFunction = Some (fun t -> Sort.DateTime <| (get t ))
                EditField = Some (Form.Date (get, set))
            }
        /// Under construction
        static member EditDateColumn (name, get : ('DataType -> System.DateTime option), set : ('DataType -> System.DateTime option -> 'DataType)) =
            let SomeOrDefaultString (dt: System.DateTime option) =
                match dt with
                |Some dt' -> (new Date(dt'.Year,dt'.Month - 1, dt'.Day)).ToDateString()
                |None -> "-"
            let SomeOrDefaultSort = function
                |Some t' -> t'
                |None -> System.DateTime.Parse("01-01-1970")
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << SomeOrDefaultString <| get t ])
                SortFunction = Some (fun t -> Sort.DateTime << SomeOrDefaultSort <| get t )
                EditField = Some (Form.DateOption (get, set))
            }

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
                EditField = Some (Form.Select (get, set, optionMap))}

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
                EditField = Some (Form.SelectOption (get, set, optionMap))}

        /// Use the parse functions with caution. They are not typesafe and meant to be used in combination with Reflection.
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
        /// Use the parse functions with caution. They are not typesafe and meant to be used in combination with Reflection.
        static member Parse(name, _type, header) =
            { Column<'DataType>.Parse(name, _type) with Header = header }

    type Table<'Key, 'DataType> when 'Key : equality  =
        {
            Id': string
            Class: TableClass []
            DataSource: DataSource.DS<'Key,'DataType>
            Columns: Column<'DataType> []
        }
        member this.isEditable =
            this.Columns
            |> Array.exists (fun c -> c.EditField.IsSome)
        member this.isDeletable =
            this.DataSource.DeleteFunc
        member private this.SaveButton (t : 'DataType) =
            buttonAttr[
                attr.``class`` "btn btn-primary"
                attr.``data-`` "dismiss" "modal"
                on.click (fun el ev ->
                    this.DataSource.Update t
                    )][text "Save"] :> Doc
        member private this.DeleteButton (t : 'DataType) =
            let modalId = sprintf "confirm-delete-%A" (this.DataSource.IdFunc t)
            let confirmDialog () =
                let modalwindow =
                    Modal.Window.Create
                        modalId
                        Doc.Empty
                        (text "Are you sure you want to delete this item?")
                        (div[
                            buttonAttr[
                                attr.``class`` "btn btn-danger"
                                attr.``data-`` "dismiss" "modal"
                                on.click (fun el ev ->
                                    // TODO: confirmation popup
                                    this.DataSource.Delete t
                                    )][text "Delete"] :> Doc
                            buttonAttr[
                                attr.``class`` "btn btn-default"
                                attr.``data-`` "dismiss" "modal"][text "Cancel"] :> Doc

                        ])
                        Modal.Small
                modalwindow.Show()
            divAttr[attr.style "display: inline-block"][
                confirmDialog()
                Modal.Button modalId
                    [
                        attr.``class`` "btn btn-danger"
                    ] [iAttr[attr.``class`` "fa fa-trash"][]]
            ] :> Doc


        member private this.EditFooter (t : 'DataType option) =
            match t with
            | Some v ->
                div[
                    buttonAttr[attr.``class`` "btn btn-secondary"; attr.``data-`` "dismiss" "modal"][text "Close"] :> Doc
                    (if this.DataSource.UpdateFunc
                    then this.SaveButton v
                    else Doc.Empty)
//                    (if this.DataSource.DeleteFunc
//                    then this.DeleteButton v
//                    else Doc.Empty)
                ] :> Doc
            | None -> Doc.Empty
        member this.EditWindow (item : Var<'DataType option>) windowId =
            let editForm () =
                this.Columns
                |> Array.filter ( fun column ->
                    column.Permission.Form <> Invisible
                )
                |> Array.map (fun column ->
                    match (column.EditField, column.Permission.Form) with
                    | None, _  | _, Read -> (Form.Disabled column.DocList).show column.Name item
                    | Some editField, _ -> editField.show column.Name item
                )
                |> Array.toList
                |> form

            Modal.Window.Create
                windowId
                (h2 [textView <| View.Map (fun t ->  (match t with | Some t' ->  sprintf "Item %A" (this.DataSource.IdFunc t') | None -> "")) item.View ] )
                (editForm ())
                (Doc.BindView (fun t -> this.EditFooter t) item.View)
                Modal.WindowSize.Normal
        member private this.CreateButton() =
            if this.DataSource.CreateFunc
            then
                // a button and an edit field: new. on new, open window with empty form
                let currentItem = Var.Create None
                let newModalId = (sprintf "new-%s" this.Id')
                div[
                    (this.EditWindow currentItem newModalId).Show()
                    buttonAttr[
                        attr.``class`` "btn btn-success"
                        attr.``data-`` "toggle" "modal"
                        attr.``data-`` "target" <| "#" + newModalId
                        on.click (fun el ev -> this.DataSource.Create currentItem)
                    ][
                        iAttr[attr.``class`` "fa fa-plus"][]
                        //text " Add"
                    ]
                ]:> Doc
            else Doc.Empty
        member this.ShowHeader () =
            // add an extra column with an edit button
            let extracol = if this.isEditable || this.isDeletable then [|td[] :> Doc|] else [||]

            this.Columns
            |> Array.indexed
            |> Array.filter ( fun (_, column) ->
                column.Permission.Table <> Invisible
            )
            |> Array.map (fun (index, column) ->
                column.showHeader index this.DataSource
            )
            |> flip Array.append extracol
            |> fun headerRow -> thead[tr headerRow] :> Doc
        member private this.ShowTableFilter (filter: 'DataType -> bool ) (currentItem : Var<'DataType option>) =
            let idCode = sprintf "%s-edit-%i" this.Id' ( int <| (Math.Random() * 100000.) + 1.)
            let editdeleteColumn t =
                if this.isEditable || this.isDeletable
                then
                    [
                        td [
                            (
                                if this.isEditable
                                then
                                    Modal.Button
                                        idCode
                                        [on.click( fun el ev -> currentItem.Value <- Some t)]
                                        [iAttr[ attr.``class`` "fa fa-edit"][]] :> Doc
                                else Doc.Empty)
                            (
                                if this.isDeletable
                                then this.DeleteButton t
                                else Doc.Empty)
                        ] :> Doc
                    ]
                else List.empty
                |> List.toArray
            let rows view =
                view
                |> Seq.filter filter
                |> Seq.map ( fun t ->
                    let row =
                        this.Columns
                        |> Array.filter ( fun column ->
                            column.Permission.Table <> Invisible )
                        |> Array.map ( fun column ->
                            column.showRow t :> Doc )
                        |> flip Array.append ( editdeleteColumn t )
                    trAttr [
                        on.click
                            (fun el ev ->
                            JQuery.JQuery("#" + this.Id' + " tr").RemoveClass("active-row").RemoveAttr("style") |> ignore
                            el.SetAttribute ("class", "active-row")
                            el.SetAttribute ("style", "background: #d9edf7")
                            match this.DataSource.ItemSelectFunc with
                            | Some selectF -> selectF t el ev
                            | None -> ()

                        )
                    ] row :> Doc
                )
                |> Seq.toList
            let classes =
                if Array.isEmpty this.Class
                then
                    "table"
                else
                    this.Class
                    |> Array.map (fun cl -> cl.show)
                    |> Array.append [|"table"|]
                    |> String.concat " "
            div [
                (if this.isEditable
                then (this.EditWindow currentItem idCode).Show()
                else Doc.Empty)
                tableAttr [
                    attr.``class`` classes
                    attr.id this.Id'
                    ]
                    (this.ShowHeader() :: [tbody <| rows this.DataSource.Model.Value])
            ]
        /// Show all data in a table
        member this.ShowTable () =
            let currentItem = Var.Create None
            this.DataSource.Read()
            div[
                this.CreateButton()
                Doc.BindView ( fun _ ->
                    this.ShowTableFilter (fun _ -> true) currentItem
                ) this.DataSource.Model.View
            ]
        /// Show all data in pages with 1 table each of length pageSize
        member this.ShowTableWithPages pageSize =
            let currentPage = Var.Create 0
            let currentItem = Var.Create None
            this.DataSource.Read()
            div[
                this.CreateButton()
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
            }
        static member Create (Id, (keyFunction: ('DataType -> 'Key)), columns, (readFunc: unit -> Async<array<'DataType>>)) =
           {
                Id' = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc)
            }
        static member Create (Id, (keyFunction: 'DataType -> 'Key), columns, (readFunc: unit -> Async<array<'DataType>>), createFunc, updateFunc, deleteFunc) =
           {
                Id' = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc, createFunc, updateFunc, deleteFunc)
            }
        static member Create (Id, (keyFunction: 'DataType -> 'Key), columns, (itemSelectFunc), (readFunc: unit -> Async<array<'DataType>>), createFunc, updateFunc, deleteFunc) =
           {
                Id' = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc, itemSelectFunc, createFunc, updateFunc, deleteFunc)
            }
        static member Create (Id, (keyFunction: 'DataType -> 'Key), columns, (readFunc: unit -> array<'DataType>), createFunc, updateFunc, deleteFunc) =
           {
                Id' = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                DataSource = DataSource.DS<'Key,'DataType>.Create (keyFunction, readFunc, createFunc, updateFunc, deleteFunc)
            }





