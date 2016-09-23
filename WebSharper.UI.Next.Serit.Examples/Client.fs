namespace WebSharper.UI.Next.Serit.Examples

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Serit

//open FSharp.Data


[<JavaScript>]
module Client =

    open WebSharper.UI.Next.Serit.Table

    type IndexTemplate = Templating.Template<"index.html">


    let dataList = ['a'..'z'] |> List.indexed

    let tbl =
        {
            Id = "table"
            RowData = ListModel.Create (fun (i,c) -> i ) dataList
            RowIdFunc = (fun (i,c) -> i)
            Class = [| Striped ; Bordered |]
            Columns =
                [|
                        Column.SimpleColumn ("Id", (fun ((i:int),_) -> i))
                        Column.SimpleColumn ("Square", (fun ((i:int),_) -> i * i))
                        Column.SimpleColumn ("To Char", (fun (_,c) -> string c |> Seq.replicate 5 |> Seq.toArray |> String.concat ""))
                        Column.SimpleSortColumn ("Id", (fun ((i:int),_) -> i))
                        Column.SimpleSortColumn ("Square", (fun ((i:int),_) -> i * i))
                        Column.SimpleSortColumn ("To Char", (fun (_,c) -> string c |> Seq.replicate 5 |> Seq.toArray |> String.concat ""))

                |]
            Direction = Asc 1
        }

    let Main =
        let newName = Var.Create ""

        IndexTemplate.Main.Doc(
            body = [tbl.ShowTableWithPages 5]
        )
        |> Doc.RunById "main"
