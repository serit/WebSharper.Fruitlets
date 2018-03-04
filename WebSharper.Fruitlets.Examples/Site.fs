namespace WebSharper.Fruitlets.Examples

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI
open WebSharper.UI.Server
open WebSharper.UI.Html

type Endpoint =
    |[<EndPoint "/">] Home
    |[<EndPoint "/form">] Form
    |[<EndPoint "/books">] Books

module Site =

    type IndexTemplate = Templating.Template<"index.html">

    [<Website>]
    let Main =
        // Sitelet.Infer <| fun ctx action ->
        Application.MultiPage (fun ctx endpoint ->
            match endpoint with
            | Home ->
                Content.Page(
                    IndexTemplate()
                        .Body([(div [] [ client <@ Client.Body() @>]) :> Doc])
                        .Doc()
                )
            | Form ->
                Content.Page(
                    IndexTemplate()
                        .Body([(div [] [ client <@ FormClient.FormPage() @>]) :> Doc])
                        .Doc()
                )
            | Books ->
                Content.Page(
                    IndexTemplate()
                        .Body([(div [][ client <@ BooksAPI.BookPage() @>]) :> Doc])
                        .Doc()
                )
        )
