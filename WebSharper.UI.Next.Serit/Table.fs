namespace WebSharper.UI.Next.Serit

open WebSharper
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

    type Column<'T> =
        | Sortable of string * ('T -> list<Attr>) * ('T -> list<Doc>) * ('T -> SortableType)
        | Plain of string * ('T -> list<Attr>) * ('T -> list<Doc>)
        member this.showHeader index table =
            match this with
            | Sortable (name, _, _, sortFunc) ->

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
            | Plain (name, _,_ ) ->
                td [
                    text name
                ] :> Doc

        member this.showRow item =
            match this with
            | Sortable (_, attrList, docList, _) ->
                tdAttr ( attrList item ) (docList item)
            | Plain (_, attrList, docList) ->
                tdAttr ( attrList item ) (docList item)
        static member SimpleColumn (name, fieldValueFunction : ('T -> int)) =
            Plain (name, (fun t -> []), (fun t -> [text << string <| fieldValueFunction t ]))
        static member SimpleColumn (name, fieldValueFunction : ('T -> string)) =
            Plain (name, (fun t -> []), (fun t -> [text <| fieldValueFunction t ]))
        static member SimpleColumn (name, fieldValueFunction : ('T -> float)) =
            Plain (name, (fun t -> []), (fun t -> [text << string <| fieldValueFunction t ]))
        static member SimpleSortColumn (name, fieldValueFunction : ('T -> int)) =
            Sortable (name, (fun t -> []), (fun t -> [text << string <| fieldValueFunction t ]), (fun t -> I <| fieldValueFunction t ))
        static member SimpleSortColumn (name, fieldValueFunction : ('T -> string)) =
            Sortable (name, (fun t -> []), (fun t -> [text <| fieldValueFunction t ]), (fun t -> S <| fieldValueFunction t ))
        static member SimpleSortColumn (name, fieldValueFunction : ('T -> float)) =
            Sortable (name, (fun t -> []), (fun t -> [text << string <| fieldValueFunction t ]), (fun t -> F <| fieldValueFunction t ))

    and Table<'U, 'T> when 'U : equality  =
        {
            Id: string
            Class: TableClass []
            RowIdFunc: ('T -> 'U)
            RowData: ListModel<'U,'T>
            Columns: Column<'T> []
            mutable Direction: SortDirection
        }
        member this.ShowHeader () =
            this.Columns
            |> Array.indexed
            |> Array.map (fun (index, column) ->
                column.showHeader index this
            )
            |> fun headerRow -> thead[tr headerRow] :> Doc
        member this.ShowTableFilter (filter: 'T -> bool ) =
            let rows view =
                view
                |> Seq.filter filter
                |> Seq.map ( fun t ->
                    let row =
                        this.Columns
                        |> Array.map (fun column ->
                            column.showRow t :> Doc
                        )
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

            tableAttr [attr.``class`` classes ]
                (this.ShowHeader() :: [tbody <| rows this.RowData.Value])
        member this.ShowTable () =
            this.ShowTableFilter (fun _ -> true)

        member this.ShowTableWithPages pageSize =
            let currentPage = Var.Create 0
            Doc.BindView ( fun rowdata ->
                let pages =
                    {0 ..  (Seq.length rowdata / pageSize)}
                    |> Seq.map (fun p ->
                        let dataList = rowdata |> Seq.indexed |> Seq.filter (fun (i,t) -> i / pageSize = p) |> Seq.map (fun (_,t) -> this.RowIdFunc t)
                        p, this.ShowTableFilter (fun t -> Seq.exists (fun d -> d = this.RowIdFunc t) dataList) :> Doc

                    )
                Pagination.show pages
            ) this.RowData.View





