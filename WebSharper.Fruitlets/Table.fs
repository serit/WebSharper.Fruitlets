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
        | IntField
        | FloatField
        | StringField
        | TextField
        | BoolField
        | TimeField
        | DateField
        | SelectField of Map<int,string>
        | SelectFieldVar of Var<Map<int,string>>

    type Column<'T> =
        {
            Name: string
            Header: (unit -> Doc) option
            AttrList: ('T -> list<Attr>) option
            DocList: ('T -> list<Doc>)
            SortFunction: ('T -> Sort.SortableType) option
            EditField: InputType<'T> option
        }
        member this.showHeader index (ds : DataSource.DS<_,'T>) =
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
        static member SimpleColumn (name, get : ('T -> obj)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('T -> int)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('T -> string)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text <| get t ])}
        static member SimpleColumn (name, get : ('T -> Date)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('T -> float)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('T -> decimal)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('T -> bool)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleSortColumn (name, get : ('T -> int)) =
            { Column<'T>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.I <| get t )}
        static member SimpleSortColumn (name, get : ('T -> string)) =
            { Column<'T>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.S <| get t )}
        static member SimpleSortColumn (name, get : ('T -> float)) =
            { Column<'T>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.F <| get t )}
        static member SimpleSortColumn (name, get : ('T -> decimal)) =
            { Column<'T>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.F << float <| get t )}
        static member EditColumn (name, get : ('T -> string), set : ('T -> string -> 'T)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text <| get t ]); SortFunction = Some (fun t -> Sort.S <| get t ); EditField = Some (Form.StringInput (get, set))}
        static member EditColumn (name, get : ('T -> int), set : ('T -> int -> 'T)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << string <| get t ]); SortFunction = Some (fun t -> Sort.I <| get t ); EditField = Some (Form.IntInput (get, set))}
        static member EditColumn (name, get : ('T -> float), set : ('T -> float -> 'T)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << string <| get t ]); SortFunction = Some (fun t -> Sort.F <| get t ); EditField = Some (Form.FloatInput (get, set))}
        static member EditColumn (name, get : ('T -> bool), set : ('T -> bool -> 'T)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text <| if get t then "\u00D7" else "" ]); SortFunction = Some (fun t -> Sort.B <| get t ); EditField = Some (Form.BoolInput (get, set))}
        /// Be aware that this column has to use get, set functions that operate on TimeSpan.Ticks so e.g. (get: (fun t -> t.Time.Ticks) (set: (fun t s -> {t with Time = System.TimeSpan.FromTicks s})))
        static member EditTimeSpanColumn (name, get : ('T -> int64), set : ('T -> int64 -> 'T)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << Time.showTime <| get t ]); SortFunction = Some (fun t -> Sort.I << int <| get t ); EditField = Some (Form.TimeInput (get, set))}
        /// Under construction
        static member EditDateColumn (name, get : ('T -> System.DateTime), set : ('T -> System.DateTime -> 'T)) =
            { Column<'T>.empty with
                Name = name
                DocList = (fun t ->
                    let dt = get t |> fun dt' -> new Date(dt'.Year,dt'.Month - 1, dt'.Day)
                    [text <| dt.ToDateString() ])
                SortFunction = Some (fun t -> Sort.D <| (get t ))
                EditField = Some (Form.DateInput (get, set))
            }
        /// This column can be used to represent and change foreign keys
        static member EditSelectColumn (name, get : ('T -> int), set : ('T -> int -> 'T), optionMap : Var<Map<int,string>>) =
            let findInMap (map : Var<Map<int,string>>) value =
                if map.Value.IsEmpty
                then ""
                else
                    match map.Value.TryFind value with
                    | Some v -> v
                    | None -> ""
            { Column<'T>.empty with
                Name = name
                DocList = (fun t -> [text <| findInMap optionMap (get t)])
                SortFunction = Some (fun t -> Sort.S <| findInMap optionMap (get t) )
                EditField = Some (Form.SelectInput (get, set, optionMap))}
        /// Use the parse functions with caution. They are not typesafe and meant to be used in combination with Reflection.
        static member Parse(name, _type) =
            match _type with
            | IntField -> Column.EditColumn(name, (fun t -> JSGetIntField t name),(fun t v -> JSSetField t name v; t))
            | StringField -> Column.EditColumn(name, (fun t -> JSGetStringField t name),(fun t v -> JSSetField t name v; t))
            | TextField ->
                let column = Column.EditColumn(name, (fun t -> JSGetStringField t name),(fun t v -> JSSetField t name v; t))
                match column.EditField.Value with
                | StringInput obj -> {column with EditField = Some <| TextInput obj}
                | _ -> column
            | BoolField -> Column.EditColumn(name, (fun t -> JSGetBoolField t name),(fun t v -> JSSetField t name v; t))
            | FloatField -> Column.EditColumn(name, (fun t -> JSGetFloatField t name),(fun t v -> JSSetField t name v; t))
            | TimeField -> Column.EditTimeSpanColumn(name, (fun t -> (JSGetTimeSpanField t name).Ticks),(fun t v -> JSSetField t name (System.TimeSpan.FromTicks v); t))
            | DateField -> Column.EditDateColumn(name, (fun t -> JSGetDateTimeField t name),(fun t v -> JSSetField t name v; t))
            | SelectField m -> 
                let varmap = Var.Create m
                Column.EditSelectColumn(name, (fun t -> JSGetIntField t name),(fun t v -> JSSetField t name v; t), varmap)
            | SelectFieldVar m -> Column.EditSelectColumn(name, (fun t -> JSGetIntField t name),(fun t v -> JSSetField t name v; t), m)
        static member Parse(name, _type, header) =
            { Column<'T>.Parse(name, _type) with Header = header }

    type Table<'U, 'T> when 'U : equality  =
        {
            Id': string
            Class: TableClass []
            DataSource: DataSource.DS<'U,'T>
            Columns: Column<'T> []
            //mutable Direction: DataSource.SortDirection
        }
        member this.isEditable =
            this.Columns
            |> Array.exists (fun c -> c.EditField.IsSome)
        member private this.EditFooter (t : 'T option) =
            match t with
            | Some v ->
                div[
                    buttonAttr[attr.``class`` "btn btn-secondary"; attr.``data-`` "dismiss" "modal"][text "Close"] :> Doc
                    (if this.DataSource.UpdateFunc
                    then
                        buttonAttr[
                            attr.``class`` "btn btn-primary"
                            attr.``data-`` "dismiss" "modal"
                            on.click (fun el ev ->
                                Console.Log <| sprintf "Save: %A" v
                                this.DataSource.Update v
                                )][text "Save"] :> Doc
                    else Doc.Empty)
                    (if this.DataSource.DeleteFunc
                    then
                        buttonAttr[
                            attr.``class`` "btn btn-danger"
                            attr.``data-`` "dismiss" "modal"
                            on.click (fun el ev ->
                                Console.Log <| sprintf "Remove: %A" v
                                this.DataSource.Delete v
                                )][text "Delete"] :> Doc
                    else Doc.Empty)
                ] :> Doc
            | None -> Doc.Empty
        member this.EditWindow (item : Var<'T option>) windowId =
            let editForm () =
                let editcolumns =
                    this.Columns
                    //|> Array.filter(fun c -> c.EditField.IsSome)
                editcolumns
                |> Array.map (fun column ->
                    match column.EditField with
                    | Some editField -> editField.show column.Name item
                    | None -> (Form.DisabledInput column.DocList).show column.Name item
                )
                |> Array.toList
                |> form

            Modal.Window.Create
                windowId
                (h2 [textView <| View.Map (fun t ->  (match t with | Some t' ->  sprintf "Item %A" (this.DataSource.IdFunc t') | None -> "")) item.View ] )
                (editForm ())//(p [textView <| View.Map (fun t -> sprintf "%A" t) item.View])
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
        member private this.ShowTableFilter (filter: 'T -> bool ) (currentItem : Var<'T option>) =
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
        member this.ShowTable () =
            let currentItem = Var.Create None
            this.DataSource.Read()
            div[
                this.CreateButton()
                Doc.BindView ( fun rowdata ->
                    this.ShowTableFilter (fun _ -> true) currentItem
                ) this.DataSource.Model.View
            ]
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
                DataSource = DataSource.DS<'U,'T>.Create (id, (fun () -> async{return Array.empty}))
                Columns = [||]
            }
        static member Create (Id, (keyFunction: ('T -> 'U)), columns, (readFunc: unit -> Async<array<'T>>)) =
           {
                Id' = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                DataSource = DataSource.DS<'U,'T>.Create (keyFunction, readFunc)
            }
        static member Create (Id, (keyFunction: 'T -> 'U), columns, (readFunc: unit -> Async<array<'T>>), createFunc, updateFunc, deleteFunc, getFunc) =
           {
                Id' = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                DataSource = DataSource.DS<'U,'T>.Create (keyFunction, readFunc, createFunc, updateFunc, deleteFunc, getFunc)
            }
        static member Create (Id, (keyFunction: 'T -> 'U), columns, (itemSelectFunc), (readFunc: unit -> Async<array<'T>>), createFunc, updateFunc, deleteFunc, getFunc) =
           {
                Id' = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                DataSource = DataSource.DS<'U,'T>.Create (keyFunction, readFunc, itemSelectFunc, createFunc, updateFunc, deleteFunc, getFunc)
            }
        static member Create (Id, (keyFunction: 'T -> 'U), columns, (readFunc: unit -> array<'T>), createFunc, updateFunc, deleteFunc, getFunc) =
           {
                Id' = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                DataSource = DataSource.DS<'U,'T>.Create (keyFunction, readFunc, createFunc, updateFunc, deleteFunc, getFunc)
            }





