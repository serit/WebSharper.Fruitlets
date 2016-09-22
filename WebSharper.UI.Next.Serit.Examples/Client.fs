namespace WebSharper.UI.Next.Serit.Examples

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Serit

[<JavaScript>]
module Client =

    open WebSharper.UI.Next.Serit.Table

    type IndexTemplate = Templating.Template<"index.html">

    let data = [1..10]



    let tbl =
        {
            Id = "table"
            Sortable = true
            RowData = ListModel.Create id data
            Class = [| Striped |]
            Columns =
                [|
                    "Id", I << id, ColumnType.DefaultShow
                    "Square", (fun x -> I <| x*x), ColumnType.DefaultShow
                |]
            Direction = Asc 1
            Paging = None
        }

    let Main =
        let newName = Var.Create ""

        IndexTemplate.Main.Doc(
            body = [tbl.ShowTable()]
        )
        |> Doc.RunById "main"
