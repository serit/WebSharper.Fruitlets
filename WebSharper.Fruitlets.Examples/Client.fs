namespace WebSharper.Fruitlets.Examples

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Server
open WebSharper.Fruitlets
open WebSharper.Sitelets

open FSharp.Data
open Server

[<JavaScript>]
module Client =

    open WebSharper.Fruitlets.Table
    let now = Server.now()

    let Body () =
        let gameList = ListModel.Create (fun (r: Server.GameObject) -> r.Rank) Array.empty

        let inputTitleLens (g: Server.GameObject) =
            let v = Var.Create g.Title
            [Doc.Input [on.change (fun el ev -> gameList.UpdateBy ( fun g -> Some {g with Title = v.Value}) g.Rank)] v :> Doc]

        let testList = Var.Create <|Map([(1,"a");(102,"b")])

        let columns =
            [|
                ("Title", String, None)
                ("Rating", Float, None)
                ("Voters", Int, None)
                ("Rank", Int, None)
                ("Rank", SelectDyn testList, Some <| (fun () -> text "Rank 2"))
            |]
            |> Array.map Column<GameObject>.Parse

        let createFunc () =
            let newItem : Server.GameObject = {Title = ""; Rating = 0.; Rank = 0; Voters = 0; Year = 0}
            async {return newItem}

        let gameTable : Table<int,Server.GameObject> =
            {
                Id' = "gameTable"
                DataSource =
                    {DataSource.DS<int,Server.GameObject>.Create(
                        (fun r -> r.Rank),
                        Server.GetGames,
                        CreateFunction = createFunc,
                        UpdateFunction = (fun t -> async{return true})
                        )
                        with SortDirection = Sort.Asc 3}
                Class = [| Striped ; Bordered |]
                Columns = columns
            }


        let columns =
            [|
                ("Time", Time)
                ("Date", Date)
            |] |> Array.map Column<RandomType>.Parse
        let testTable =
            Table.Create(
                "time",
                (fun (t:RandomType) -> t.Id),
                columns, 
                Server.GetTestData,
                (fun () -> async {return {Id = 0; Time = System.TimeSpan.FromHours 3.; Date = System.DateTime.Parse("05-10-2016")}}),
                (fun t -> async {return true}),
                (fun t -> async {return true})
                )

        let newName = Var.Create ""
        div [
              h2 [text "Test"]
              testTable.ShowTable()
              h2 [text "Games"]
              gameTable.ShowTableWithPages 5
        ]

type Endpoint =
    |[<EndPoint "/">] Home

module Site =

    type IndexTemplate = Templating.Template<"index.html">

    [<Website>]
    let Main =

        Sitelet.Infer <| fun ctx action ->
            match action with
            | Home ->
                Content.Page(
                    IndexTemplate.Doc(
                        body = [div [ client <@ Client.Body() @>]]
                     ))