namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

[<JavaScript>]
module Sort =

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


