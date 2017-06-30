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
    
        let testList = Var.Create <| Map([ ( 1, "a" ); ( 102, "b" ) ])

        let columns =
            [|
                ("Title", FieldClass.String, None)
                ("Rating", FieldClass.Float, None)
                ("Voters", FieldClass.Int, None)
                ("Rank", FieldClass.Int |> Optional, None)
                ("Rank", FieldClass.SelectDyn testList |> Optional, Some <| (fun () -> text "Rank 2"))
            |]
            |> Array.map Column<GameObject>.Parse

        let gameTable : Table<string, Server.GameObject> = Table.Create("gameTable", (fun (r: Server.GameObject) -> r.Title), columns, Server.GetGames)

        div [
              h2 [text "Games"]
              gameTable.ShowTableWithPages 5

        ]

type Endpoint =
    |[<EndPoint "/">] Home
    |[<EndPoint "/form">] Form
    |[<EndPoint "/ansatt">] Buss

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
            | Form ->
                Content.Page(
                    IndexTemplate.Doc(
                        body = [div [ client <@ FormClient.FormPage() @>]]
                     ))