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
        let testList = Var.Create <| Map([(1,"a");(102,"b")])

        let columns =
            [|
                ("Title", String, None)
                ("Rating", Float, None)
                ("Voters", Int, None)
                ("Rank", Int |> Optional, None)
                ("Rank", SelectDyn testList |> Optional, Some <| (fun () -> text "Rank 2"))
            |]
            |> Array.map Column<GameObject>.Parse

        let createFunc () =
            let newItem : Server.GameObject = {Title = ""; Rating = 0.; Rank = None; Voters = 0; Year = 0}
            async {return newItem}

        let gameTable : Table<string,Server.GameObject> = Table.Create("gameTable", (fun (r: Server.GameObject) -> r.Title),columns, Server.GetGames)


        let createFunc () =
            async {return {Id = 0; Time = System.TimeSpan.FromHours 3.; Date = Some <| System.DateTime.Parse("05-10-2016"); OptionalString = None}}

        let columns =
            [|
                ("Time", Time), {Table = Read; Form = ReadWrite}
                ("Date", Date |> Optional), {Table = Invisible; Form = Read}
                ("Date", Date |> Optional), {Table = Invisible; Form = ReadWrite}
                ("OptionalString", (String |> Optional)), {Table = Read; Form = Read}
                ("OptionalString", (String |> Optional)), {Table = Read; Form = ReadWrite}
            |] |> Array.map ( fun ((name, _type), permission) ->
                {Column<RandomType>.Parse (name, _type) with Permission = permission})

        let testTable =
            Table.Create(
                "time",
                (fun (t:RandomType) -> t.Id),
                columns,
                Server.GetTestData,
                createFunc,
                Server.UpdateTestData,
                Server.DeleteTestData
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