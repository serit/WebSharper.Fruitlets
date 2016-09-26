namespace WebSharper.UI.Next.Serit

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

// Bootstrap table
[<JavaScript>]
module Table =

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
        static member DefaultShow =
            fun (st : SortableType) ->
                match st with
                | I i -> sprintf "%i" i
                | S s -> s
                | F f -> sprintf "%f" f
                | D d -> d.ToShortDateString()
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


    type InputType<'T> =
        | StringInput of (('T -> string) * ('T -> string -> 'T))
        member this.show (tbl : Table<_,'T>) =
            match this with
            | StringInput (f, f') ->
                (fun t ->
                    let s = Var.Create (f t)
                    Doc.Input [
                        on.change (fun el ev ->
                            tbl.RowData.UpdateBy (fun t' -> Some <| f' t' s.Value) (tbl.RowIdFunc t)
                        )] s
                    )
    and Column<'T> =
        | Editable of string * ('T -> list<Attr>) * ('T -> list<Doc>) * option<('T -> SortableType)> * ('T -> InputType<'T>)
        | Sortable of string * ('T -> list<Attr>) * ('T -> list<Doc>) * ('T -> SortableType)
        | Plain of string * ('T -> list<Attr>) * ('T -> list<Doc>)
        member this.showHeader index table =
            match this with
            | Sortable (name, _, _, sortFunc) | Editable (name, _, _, Some sortFunc, _) ->

                tdAttr([
                        on.click ( fun _ _ ->
                            table.Direction <- table.Direction.Flip index

                            (sortFunc, table.RowData.Value)
                            ||> table.Direction.SortFunc index
                            |> table.RowData.Set
                        )
                ] )[
                    text name
                    iAttr[
                        attr.style "color: #aaa;"
                        attr.``class`` <| table.Direction.FAClass index
                    ][] :> Doc
                ] :> Doc
            | Plain (name, _,_ ) | Editable (name, _, _, None, _)->
                td [
                    text name
                ] :> Doc

        member this.showRow item =
            match this with
            | Editable (_, attrList, docList, _, _)
            | Sortable (_, attrList, docList, _)
            | Plain (_, attrList, docList) ->
                tdAttr ( attrList item ) (docList item)
        static member SimpleColumn (name, fieldValueFunction : ('T -> obj)) =
            Plain (name, (fun t -> []), (fun t -> [text << string <| fieldValueFunction t ]))
        static member SimpleColumn (name, fieldValueFunction : ('T -> int)) =
            Plain (name, (fun t -> []), (fun t -> [text << string <| fieldValueFunction t ]))
        static member SimpleColumn (name, fieldValueFunction : ('T -> string)) =
            Plain (name, (fun t -> []), (fun t -> [text <| fieldValueFunction t ]))
        static member SimpleColumn (name, fieldValueFunction : ('T -> float)) =
            Plain (name, (fun t -> []), (fun t -> [text << string <| fieldValueFunction t ]))
        static member SimpleColumn (name, fieldValueFunction : ('T -> decimal)) =
            Plain (name, (fun t -> []), (fun t -> [text << string <| fieldValueFunction t ]))
        static member SimpleSortColumn (name, fieldValueFunction : ('T -> int)) =
            Sortable (name, (fun t -> []), (fun t -> [text << string <| fieldValueFunction t ]), (fun t -> I <| fieldValueFunction t ))
        static member SimpleSortColumn (name, fieldValueFunction : ('T -> string)) =
            Sortable (name, (fun t -> []), (fun t -> [text <| fieldValueFunction t ]), (fun t -> S <| fieldValueFunction t ))
        static member SimpleSortColumn (name, fieldValueFunction : ('T -> float)) =
            Sortable (name, (fun t -> []), (fun t -> [text << string <| fieldValueFunction t ]), (fun t -> F <| fieldValueFunction t ))
        static member SimpleSortColumn (name, fieldValueFunction : ('T -> decimal)) =
            Sortable (name, (fun t -> []), (fun t -> [text << string <| fieldValueFunction t ]), (fun t -> F << float <| fieldValueFunction t ))

    and Table<'U, 'T> when 'U : equality  =
        {
            Id: string
            Class: TableClass []
            RowIdFunc: ('T -> 'U)
            RowData: ListModel<'U,'T>
            Columns: Column<'T> []
            mutable Direction: SortDirection
        }
        member this.isEditable =
            this.Columns
            |> Array.exists (fun c ->
                match c with
                | Editable _ -> true
                | _ -> false)
        member this.EditWindow (item : Var<'T option>) (idCode) =
            Modal.Window.Create
                (sprintf "edit-%i" idCode)
                (h2 [textView <| View.Map (fun t ->  (match t with | Some t' ->  sprintf "%A" (this.RowIdFunc t') | None -> "")) item.View ] )
                (p [textView <| View.Map (fun t -> sprintf "%A" t) item.View])
                Doc.Empty
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
        member this.ShowTableFilter (filter: 'T -> bool ) =
            let currentItem = Var.Create None
            let idCode = int <| (Math.Random() * 100000.) + 1.
            let editColumn t =
                if this.isEditable
                then [ td[
                        Modal.Button (sprintf "edit-%i" idCode) [on.click( fun el ev -> currentItem.Value <- Some t)] [
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
                (this.EditWindow currentItem idCode).Show()
                tableAttr [attr.``class`` classes ]
                    (this.ShowHeader() :: [tbody <| rows this.RowData.Value])
            ]
        member this.ShowTable () =
            this.ShowTableFilter (fun _ -> true)

        member this.ShowTableWithPages pageSize =
            let currentPage = Var.Create 0
            Doc.BindView ( fun rowdata ->
                let pages =
                    {0 ..  ((Seq.length rowdata - 1)/ pageSize)}
                    |> Seq.map (fun p ->
                        let dataList = rowdata |> Seq.indexed |> Seq.filter (fun (i,t) -> i / pageSize = p) |> Seq.map (fun (_,t) -> this.RowIdFunc t)
                        p, this.ShowTableFilter (fun t -> Seq.exists (fun d -> d = this.RowIdFunc t) dataList) :> Doc

                    )
                Pagination.show pages
            ) this.RowData.View
        static member empty =
            {
                Id = ""
                Class = Array.empty
                Direction = Asc -1
                RowData = ListModel.FromSeq []
                Columns = [||]
                RowIdFunc = id
            }
        static member Create (Id, idFunc, columns, data) =
           {
                Id = Id
                Class = [| Striped |]
                Columns = columns
                Direction = Asc -1
                RowIdFunc = idFunc
                RowData = ListModel.Create idFunc data

            }





