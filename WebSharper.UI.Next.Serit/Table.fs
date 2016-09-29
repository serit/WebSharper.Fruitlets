namespace WebSharper.UI.Next.Serit

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

// Bootstrap table
[<JavaScript>]
module Table =
    open Form

    type SortDirection =
        | Asc of int
        | Desc of int
        member this.SortFunc index =
            match this with
            | Asc i when i = index -> Seq.sortByDescending
            | _ -> Seq.sortBy
        member this.Flip index =
            match this with
            | Asc i when i = index -> Desc index
            | _ -> Asc index
        member this.FAClass index =
            "fa fa-fw fa-sort" +
            match this with
            | Asc i when i = index -> "-asc"
            | Desc i when i = index -> "-desc"
            | _ -> ""


    [<StructuralComparison;StructuralEquality>]
    type SortableType =
        | I of int
        | S of string
        | F of float
        | D of System.DateTime
        | B of bool
        static member DefaultShow =
            fun (st : SortableType) ->
                match st with
                | I i -> sprintf "%i" i
                | S s -> s
                | F f -> sprintf "%f" f
                | D d -> d.ToShortDateString()
                | B b -> if b then "1" else "0"
        //| T of obj //* ('T -> ColumnType<'T>)

    type SortShow =
        (SortableType -> string)

    type TableClass =
        | Striped
        | Bordered
        | Custom of string
        member this.show =
            match this with
            | Striped -> "table-striped"
            | Bordered -> "table-bordered"
            | Custom cl -> cl


    let private showTime (time: int64) =
        let hour = 60L * 60L * 1000L * 10000L
        let hourFunc t = int <| (t  % (24L * hour) ) / hour

        let minute = 60L * 1000L * 10000L
        let minFunc t = int <| (t % hour) / minute

        let t = new Date(0,0,0, hourFunc time, minFunc time)
        sprintf "%02i:%02i" (t.GetHours()) (t.GetMinutes())

    type Column<'T> =
        {
            Name: string
            AttrList: ('T -> list<Attr>) option
            DocList: ('T -> list<Doc>)
            SortFunction: ('T -> SortableType) option
            EditField: InputType<'T> option
        }
        member this.showHeader index table =
            match this.SortFunction with
            | Some sortFunc ->
                tdAttr([
                        on.click ( fun _ _ ->
                            table.Direction <- table.Direction.Flip index

                            (sortFunc, table.RowData.Model.Value)
                            ||> table.Direction.SortFunc index
                            |> table.RowData.Model.Set
                        )
                ] )[
                    text this.Name
                    iAttr[
                        attr.style "color: #aaa;"
                        attr.``class`` <| table.Direction.FAClass index
                    ][] :> Doc
                ] :> Doc
            | None ->
                td [
                    text this.Name
                ] :> Doc

        member this.showRow item =
            match this.AttrList with
            | Some attrList ->
                tdAttr ( attrList item ) (this.DocList item)
            | None ->
                td (this.DocList item)
        static member empty =
            {Name = ""; AttrList = None; DocList = (fun t -> List.empty); SortFunction = None; EditField = None}
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
            { Column<'T>.SimpleColumn(name, get) with SortFunction = Some (fun t -> I <| get t )}
        static member SimpleSortColumn (name, get : ('T -> string)) =
            { Column<'T>.SimpleColumn(name, get) with SortFunction = Some (fun t -> S <| get t )}
        static member SimpleSortColumn (name, get : ('T -> float)) =
            { Column<'T>.SimpleColumn(name, get) with SortFunction = Some (fun t -> F <| get t )}
        static member SimpleSortColumn (name, get : ('T -> decimal)) =
            { Column<'T>.SimpleColumn(name, get) with SortFunction = Some (fun t -> F << float <| get t )}
        static member EditColumn (name, get : ('T -> string), set : ('T -> string -> 'T)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text <| get t ]); SortFunction = Some (fun t -> S <| get t ); EditField = Some (Form.StringInput (get, set))}
        static member EditColumn (name, get : ('T -> int), set : ('T -> int -> 'T)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << string <| get t ]); SortFunction = Some (fun t -> I <| get t ); EditField = Some (Form.IntInput (get, set))}
        static member EditColumn (name, get : ('T -> float), set : ('T -> float -> 'T)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << string <| get t ]); SortFunction = Some (fun t -> F <| get t ); EditField = Some (Form.FloatInput (get, set))}
        static member EditColumn (name, get : ('T -> bool), set : ('T -> bool -> 'T)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text <| if get t then "\u00D7" else "" ]); SortFunction = Some (fun t -> B <| get t ); EditField = Some (Form.BoolInput (get, set))}
        static member EditTimeSpanColumn (name, get : ('T -> int64), set : ('T -> int64 -> 'T)) =
            /// Be aware that this column has to use get, set functions that operate on TimeSpan.Ticks so e.g. (get: (fun t -> t.Time.Ticks) (set: (fun t s -> {t with Time = System.TimeSpan.FromTicks s})))
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << showTime <| get t ]); SortFunction = Some (fun t -> I << int <| get t ); EditField = Some (Form.TimeInput (get, set))}
        static member EditSelectColumn (name, get : ('T -> int), set : ('T -> int -> 'T), optionMap : Var<Map<int,string>>) =
            /// This column can be used to represent and change foreign keys
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
                SortFunction = Some (fun t -> S <| findInMap optionMap (get t) )
                EditField = Some (Form.SelectInput (get, set, optionMap))}

    and Table<'U, 'T> when 'U : equality  =
        {
            Id: string
            Class: TableClass []
            //RowIdFunc: ('T -> 'U)
            RowData: DataSource.DataSource<'U,'T> //ListModel<'U,'T>
            Columns: Column<'T> []
            mutable Direction: SortDirection
        }
        member this.isEditable =
            this.Columns
            |> Array.exists (fun c -> c.EditField.IsSome)
        member this.EditWindow (item : Var<'T option>) windowId =
            let editForm () =
//                //View.
//                match item.Value with
//                | Some v ->
                let editcolumns =
                    this.Columns
                    |> Array.filter(fun c -> c.EditField.IsSome)
                // should be a Var of t which updates the entire form and is updated by the form
                editcolumns
                |> Array.map (fun c ->
                    (c.EditField.Value).show c.Name item
                )
                |> Array.toList
                |> form
            let footer t =
                match t with
                | Some v ->
                    div[
                        buttonAttr[attr.``class`` "btn btn-secondary"; attr.``data-`` "dismiss" "modal"][text "Close"] :> Doc
                        (match this.RowData.UpdateFunc with
                        | Some f ->
                            buttonAttr[
                                attr.``class`` "btn btn-primary"
                                attr.``data-`` "dismiss" "modal"
                                on.click (fun el ev ->
                                    Console.Log <| sprintf "Save: %A" v
                                    this.RowData.Model.UpdateBy (fun _ -> Some v ) <| this.RowData.IdFunc v
                                    f v)][text "Save"] :> Doc
                        | None -> Doc.Empty)
                        (match this.RowData.DeleteFunc with
                        | Some f ->
                            buttonAttr[
                                attr.``class`` "btn btn-danger"
                                attr.``data-`` "dismiss" "modal"
                                on.click (fun el ev ->
                                    Console.Log <| sprintf "Remove: %A" v
                                    this.RowData.Model.Remove v
                                    f v)][text "Delete"] :> Doc
                        | None -> Doc.Empty)
                    ] :> Doc
                | None -> Doc.Empty

            Modal.Window.Create
                windowId
                (h2 [textView <| View.Map (fun t ->  (match t with | Some t' ->  sprintf "Item %A" (this.RowData.IdFunc t') | None -> "")) item.View ] )
                (editForm ())//(p [textView <| View.Map (fun t -> sprintf "%A" t) item.View])
                (Doc.BindView (fun t -> footer t) item.View)
                Modal.WindowSize.Normal

        member this.ShowHeader () =
            let extracol = if this.isEditable then [|td[] :> Doc|] else [||]
            this.Columns
            |> Array.indexed
            |> Array.map (fun (index, column) ->
                column.showHeader index this
            )
            |> Array.append extracol
            |> fun headerRow -> thead[tr headerRow] :> Doc
        member private this.ShowTableFilter (filter: 'T -> bool ) =
            let currentItem = Var.Create None
            let idCode = sprintf "%s-edit-%i" this.Id ( int <| (Math.Random() * 100000.) + 1.)
            let editColumn t =
                if this.isEditable
                then [ td[
                        Modal.Button idCode [on.click( fun el ev -> currentItem.Value <- Some t)] [
                                iAttr[ attr.``class`` "fa fa-edit"][]]] :> Doc]
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
                        |> Array.append (editColumn t )
                    tr row :> Doc
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
            div[
                (if this.isEditable
                then (this.EditWindow currentItem idCode).Show()
                else Doc.Empty)
                tableAttr [attr.``class`` classes ]
                    (this.ShowHeader() :: [tbody <| rows this.RowData.Model.Value])
            ]
        member this.ShowTable () =
            Doc.BindView ( fun rowdata ->
                this.ShowTableFilter (fun _ -> true)
            ) this.RowData.Model.View

        member this.ShowTableWithPages pageSize =
            let currentPage = Var.Create 0
            Doc.BindView ( fun rowdata ->
                let pages =
                    {0 ..  ((Seq.length rowdata - 1)/ pageSize)}
                    |> Seq.map (fun p ->
                        let dataList = rowdata |> Seq.indexed |> Seq.filter (fun (i,t) -> i / pageSize = p) |> Seq.map (fun (_,t) -> this.RowData.IdFunc t)
                        p, this.ShowTableFilter (fun t -> Seq.exists (fun d -> d = this.RowData.IdFunc t) dataList) :> Doc

                    )
                Pagination.show pages Pagination.PagerPosition.Up
            ) this.RowData.Model.View
        static member empty =
            {
                Id = ""
                Class = Array.empty
                Direction = Asc -1
                RowData = DataSource.DataSource<'U,'T>.Create (id, ListModel.FromSeq [])
                Columns = [||]
            }
        static member Create (Id, idFunc, columns, data) =
           {
                Id = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                Direction = Asc -1
                RowData = DataSource.DataSource<'U,'T>.Create (id, ListModel.Create idFunc data)
            }
        static member Create (Id, idFunc, columns, data, createFunc, updateFunc, deleteFunc) =
           {
                Id = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                Direction = Asc -1
                RowData = DataSource.DataSource<'U,'T>.Create (id, ListModel.Create idFunc data, createFunc, updateFunc, deleteFunc)
            }





