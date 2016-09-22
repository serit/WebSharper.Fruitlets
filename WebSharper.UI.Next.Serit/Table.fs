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
    type ColumnType =
        | I of int
        | S of string
        | F of float
        | D of System.DateTime
        static member DefaultShow =
            fun (ct : ColumnType) ->
                match ct with
                | I i -> sprintf "%i" i
                | S s -> s
                | F f -> sprintf "%f" f
                | D d -> d.ToShortDateString()
        //| T of obj //* ('T -> ColumnType<'T>)

    type ColumnShow =
        (ColumnType -> string)

    type TablePages =
        {
            ChunkSize: int
        }

    type TableClass =
        | Striped
        | Bordered
        member this.show =
            match this with
            | Striped -> "table-striped"
            | Bordered -> "table-bordered"

    type Table<'U,'T> when 'U : equality =
        {
            Id: string
            Class: TableClass []
            Paging: TablePages option
            Sortable: bool
            RowData: ListModel<'U, 'T>
            Columns: (string * ('T -> ColumnType) * ColumnShow) []
            mutable Direction: SortDirection
        }
        member this.ShowHeader () =
            this.Columns
            |> Array.indexed
            |> Array.map (fun (index, (name, value, show)) ->
                tdAttr[
                    (if this.Sortable
                    then
                        on.click ( fun _ _ ->
                            let convertFunction (t : 'T) =
                                t |> value |> show
                            this.Direction <- this.Direction.Flip index

                            (convertFunction, this.RowData.Value)
                            ||> this.Direction.SortFunc index
                            |> this.RowData.Set
                        )
                    else
                        Attr.Empty
                    )
                ][
                    text name
                    (
                        if this.Sortable
                        then
                            iAttr[
                                attr.style "color: #aaa;"
                                attr.``class`` <| this.Direction.FAClass index
                            ][] :> Doc
                        else
                            Doc.Empty
                    )
                ] :> Doc
            )
            |> fun headerRow -> thead[tr headerRow] :> Doc
        member this.ShowTable () =
            let rows view =
                view
                |> Seq.map ( fun t ->
                    let row =
                        this.Columns
                        |> Array.map (fun (name, value, show) ->
                            td [t |> value |> show |> text] :> Doc
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

            Doc.BindView ( fun view ->
                tableAttr [attr.``class`` classes ]
                    (this.ShowHeader() :: rows view)
            ) this.RowData.View

