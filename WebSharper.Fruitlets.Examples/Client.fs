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
    open WebSharper.Fruitlets.Column
    let now = Server.now()
    
    let Body () =
    
        let testList = Var.Create <| Map([ ( 1, fun () -> divAttr[attr.style "background: red"][text "a"] :> Doc ); ( 102, fun () -> divAttr[attr.style "background: green"][text "b"] :> Doc ) ])
        let testList2 = Var.Create <| Map([ ( 1, "a" ); ( 2, "b" ); ( 102, "c" ) ])
        
        let columns =
            [|
                Column.EditColumn("Title", (fun g -> g.Title), (fun g v -> {g with Title = v}))
                Column.EditColumn("Rating", (fun g -> g.Rating), (fun g v -> {g with Rating = v}))
                Column.EditColumn("Voters", (fun g -> g.Voters), (fun g v -> {g with Voters = v}))
                Column.EditColumn("Rank", (fun g -> g.Rank), (fun g v -> {g with Rank = v}))
                Column.EditSelectColumn("Rank", (fun g -> g.Rank), (fun g v -> {g with Rank = v}), testList )
                Column.EditSelectColumn("Rank", (fun g -> g.Rank), (fun g v -> {g with Rank = v}), testList2 )
            |]

        let gameTable : Table<string, Server.GameObject> = Table.Create("gameTable", (fun (r: Server.GameObject) -> r.Title), columns, Server.GetGames)

        div [
              h2 [text "Games"]
              gameTable.ShowTableWithPages 5
        ]

type Endpoint =
    |[<EndPoint "/">] Home
    |[<EndPoint "/form">] Form
    |[<EndPoint "/books">] Books

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
            | Books ->
                Content.Page(
                    IndexTemplate.Doc(
                        body = [div [ client <@ BooksAPI.BookPage() @>]]
                     ))