namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Html

[<JavaScript>]
module Sort =
//
//    type SortDirection =
//        | Asc //of int
//        | Desc //of int
//        | AscByColumn of int
//        | DescByColumn of int
////        member this.SortFunc index =
////            match this with
////            | Asc i when i = index -> Seq.sortByDescending
////            | _ -> Seq.sortBy
//        member this.Flip =
//            match this with
//            | Asc -> Desc
//            | _ -> Asc
////        member this.FAClass index =
////            "fa fa-fw fa-sort" +
////            match this with
////            | Asc i when i = index -> "-asc"
////            | Desc i when i = index -> "-desc"
////            | _ -> ""//


    [<StructuralComparison;StructuralEquality>]
    type SortableType =
        | Int of int
        | String of string
        | Float of float
        | DateTime of System.DateTime
        | Bool of bool
        | IntOption of int option
        | StringOption of string option
        | FloatOption of float option
        | DateTimeOption of System.DateTime option
        | BoolOption of bool option
        static member DefaultShow =
            fun (st : SortableType) ->
                match st with
                | Int i -> sprintf "%i" i
                | String s -> s
                | Float f -> sprintf "%f" f
                | DateTime d -> d.ToShortDateString()
                | Bool b -> if b then "1" else "0"
                | IntOption i ->
                    match i with
                    | Some i' -> sprintf "%i" i'
                    | None -> "-"
                | StringOption s ->
                    match s with
                    | Some s' -> s'
                    | None -> "-"
                | FloatOption f ->
                    match f with
                    | Some f' -> sprintf "%f" f'
                    | None -> "-"
                | DateTimeOption d ->
                    match d with
                    | Some d' ->  d'.ToShortDateString()
                    | None -> "-"
                | BoolOption b ->
                    match b with
                    | Some b' -> if b' then "1" else "0"
                    | None -> "-"
        //| T of obj //* ('T -> ColumnType<'T>)

        
    type SortFunction<'T> =
        | AscByFunction of ('T -> SortableType)
        | DescByFunction of ('T -> SortableType)
        | AscByColumn of int
        | DescByColumn of int
        member this.SortFunc index =
            match this with
            | AscByColumn i when i = index -> Seq.sortByDescending
            | _ -> Seq.sortBy
        member this.Flip =
            match this with
            | AscByFunction f -> DescByFunction f
            | DescByFunction f -> AscByFunction f
            | AscByColumn i -> DescByColumn i
            | DescByColumn i -> AscByColumn i
        member this.FlipColumn index =
            match this with
            | AscByColumn i when i = index -> DescByColumn index
            | _ -> AscByColumn index
        member this.FAClass index =
            "fa fa-fw fa-sort" +
            match this with
            | AscByColumn i when i = index -> "-asc"
            | DescByColumn i when i = index -> "-desc"
            | _ -> ""
