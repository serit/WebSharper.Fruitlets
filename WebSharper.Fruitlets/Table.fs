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
    [<Inline "$obj[$field] = $value">]
    let private JSSetField obj field value = X<Unit>

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

    type Column<'DataType> =
        {
            Name: string
            Header: (unit -> Doc) option
            AttrList: ('DataType -> list<Attr>) option
            DocList: ('DataType -> list<Doc>)
            SortFunction: ('DataType -> Sort.SortableType) option
            EditField: InputType<'DataType> option
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
            match this.AttrList with
            | Some attrList ->
                tdAttr ( attrList item ) (this.DocList item)
            | None ->
                td (this.DocList item)
        static member empty =
            {Name = ""; Header = None; AttrList = None; DocList = (fun t -> List.empty); SortFunction = None; EditField = None}
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
            { Column<'DataType>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.I <| get t )}
        static member SimpleSortColumn (name, get : ('DataType -> string)) =
            { Column<'DataType>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.S <| get t )}
        static member SimpleSortColumn (name, get : ('DataType -> float)) =
            { Column<'DataType>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.F <| get t )}
        static member SimpleSortColumn (name, get : ('DataType -> decimal)) =
            { Column<'DataType>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.F << float <| get t )}
        static member EditColumn (name, get : ('DataType -> string), set : ('DataType -> string -> 'DataType)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text <| get t ]); SortFunction = Some (fun t -> Sort.S <| get t ); EditField = Some (Form.String (get, set))}
        static member EditColumn (name, get : ('DataType -> int), set : ('DataType -> int -> 'DataType)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << string <| get t ]); SortFunction = Some (fun t -> Sort.I <| get t ); EditField = Some (Form.Int (get, set))}
        static member EditColumn (name, get : ('DataType -> float), set : ('DataType -> float -> 'DataType)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << string <| get t ]); SortFunction = Some (fun t -> Sort.F <| get t ); EditField = Some (Form.Float (get, set))}
        static member EditColumn (name, get : ('DataType -> bool), set : ('DataType -> bool -> 'DataType)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text <| if get t then "\u00D7" else "" ]); SortFunction = Some (fun t -> Sort.B <| get t ); EditField = Some (Form.Bool (get, set))}
        /// Be aware that this column has to use get, set functions that operate on TimeSpan.Ticks so e.g. (get: (fun t -> t.Time.Ticks) (set: (fun t s -> {t with Time = System.TimeSpan.FromTicks s})))
        static member EditTimeSpanColumn (name, get : ('DataType -> int64), set : ('DataType -> int64 -> 'DataType)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << Time.showTime <| get t ]); SortFunction = Some (fun t -> Sort.I << int <| get t ); EditField = Some (Form.Time (get, set))}
        /// Under construction
        static member EditDateColumn (name, get : ('DataType -> System.DateTime), set : ('DataType -> System.DateTime -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t ->
                    let dt = get t |> fun dt' -> new Date(dt'.Year,dt'.Month - 1, dt'.Day)
                    [text <| dt.ToDateString() ])
                SortFunction = Some (fun t -> Sort.D <| (get t ))
                EditField = Some (Form.Date (get, set))
            }
        /// This column can be used to represent and change foreign keys
        static member EditSelectColumn (name, get : ('DataType -> int), set : ('DataType -> int -> 'DataType), optionMap : Var<Map<int,string>>) =
            let findInMap (map : Map<int,string>) value =
                if map.IsEmpty
                then ""
                else
                    match map.TryFind value with
                    | Some v -> v
                    | None -> ""
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [textView <| View.Map (fun map -> findInMap map (get t)) optionMap.View])
                SortFunction = Some (fun t -> Sort.S <| findInMap optionMap.Value (get t) )
                EditField = Some (Form.Select (get, set, optionMap))}
        /// Use the parse functions with caution. They are not typesafe and meant to be used in combination with Reflection.
        static member Parse(name, _type) =
            match _type with
            | Int -> Column.EditColumn(name, (fun t -> JSGetIntField t name),(fun t v -> JSSetField t name v; t))
            | String -> Column.EditColumn(name, (fun t -> JSGetStringField t name),(fun t v -> JSSetField t name v; t))
            | Text ->
                let column = Column.EditColumn(name, (fun t -> JSGetStringField t name),(fun t v -> JSSetField t name v; t))
                match column.EditField.Value with
                | InputType.String obj -> {column with EditField = Some <| InputType.Text obj}
                | _ -> column
            | Bool -> Column.EditColumn(name, (fun t -> JSGetBoolField t name),(fun t v -> JSSetField t name v; t))
            | Float -> Column.EditColumn(name, (fun t -> JSGetFloatField t name),(fun t v -> JSSetField t name v; t))
            | Time -> Column.EditTimeSpanColumn(name, (fun t -> (JSGetTimeSpanField t name).Ticks),(fun t v -> JSSetField t name (System.TimeSpan.FromTicks v); t))
            | Date -> Column.EditDateColumn(name, (fun t -> JSGetDateTimeField t name),(fun t v -> JSSetField t name v; t))
            | Select m ->
                let varmap = Var.Create m
                Column.EditSelectColumn(name, (fun t -> JSGetIntField t name),(fun t v -> JSSetField t name v; t), varmap)
            | SelectDyn m -> Column.EditSelectColumn(name, (fun t -> JSGetIntField t name),(fun t v -> JSSetField t name v; t), m)
        /// Use the parse functions with caution. They are not typesafe and meant to be used in combination with Reflection.
        static member Parse(name, _type, header) =
            { Column<'DataType>.Parse(name, _type) with Header = header }

    type Table<'Key, 'DataType> when 'Key : equality  =
        {
            Id': string
            Class: TableClass []
            DataSource: DataSource.DS<'Key,'DataType>
            Columns: Column<'DataType> []
            //mutable Direction: DataSource.SortDirection
        }
        member this.isEditable =
            this.Columns
            |> Array.exists (fun c -> c.EditField.IsSome)
        member private this.SaveButton (t : 'DataType) =
            buttonAttr[
                attr.``class`` "btn btn-primary"
                attr.``data-`` "dismiss" "modal"
                on.click (fun el ev ->
                    this.DataSource.Update t
                    )][text "Save"] :> Doc
        member private this.DeleteButton (t : 'DataType) =
            buttonAttr[
                attr.``class`` "btn btn-danger"
                attr.``data-`` "dismiss" "modal"
                on.click (fun el ev ->
                    this.DataSource.Delete t
                    )][text "Delete"] :> Doc
        member private this.EditFooter (t : 'DataType option) =
            match t with
            | Some v ->
                div[
                    buttonAttr[attr.``class`` "btn btn-secondary"; attr.``data-`` "dismiss" "modal"][text "Close"] :> Doc
                    (if this.DataSource.UpdateFunc
                    then this.SaveButton v
                    else Doc.Empty)
                    (if this.DataSource.DeleteFunc
                    then this.DeleteButton v
                    else Doc.Empty)
                ] :> Doc
            | None -> Doc.Empty
        member this.EditWindow (item : Var<'DataType option>) windowId =
            let editForm () =
                this.Columns
                |> Array.map (fun column ->
                    match column.EditField with
                    | Some editField -> editField.show column.Name item
                    | None -> (Form.Disabled column.DocList).show column.Name item
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
            let extracol = if this.isEditable then [|td[] :> Doc|] else [||]
            this.Columns
            |> Array.indexed
            |> Array.map (fun (index, column) ->
                column.showHeader index this.DataSource
            )
            |> flip Array.append extracol
            |> fun headerRow -> thead[tr headerRow] :> Doc
        member private this.ShowTableFilter (filter: 'DataType -> bool ) (currentItem : Var<'DataType option>) =
            let idCode = sprintf "%s-edit-%i" this.Id' ( int <| (Math.Random() * 100000.) + 1.)
            let editColumn t =
                if this.isEditable
                then
                    [
                        td [
                            Modal.Button
                                idCode
                                [on.click( fun el ev -> currentItem.Value <- Some t)]
                                [iAttr[ attr.``class`` "fa fa-edit"][]]
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
                        |> Array.map (fun column ->
                            column.showRow t :> Doc
                        )
                        |> flip Array.append (editColumn t )
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





