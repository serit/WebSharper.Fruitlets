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

    let BusBody () =
        aAttr[attr.href "http://ansatt.norgesbuss.no"][text "Proceed to login"]

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

//        let createFunc () =
//            let newItem : Server.GameObject = {Title = ""; Rating = 0.; Rank = None; Voters = 0; Year = 0}
//            async {return newItem}

        let gameTable : Table<string, Server.GameObject> = Table.Create("gameTable", (fun (r: Server.GameObject) -> r.Title), columns, Server.GetGames)

        let createFunc () =
            async {return {Id = 0; Time = System.TimeSpan.FromHours 3.; Date = Some <| System.DateTime.Parse("05-10-2016"); OptionalString = None}}

        let columns =
            [|
                ("Time", FieldClass.Time), {Table = Read; Form = ReadWrite}
                ("Date", FieldClass.Date |> Optional), {Table = Invisible; Form = Read}
                ("Date", FieldClass.Date |> Optional), {Table = Invisible; Form = ReadWrite}
                ("OptionalString", (FieldClass.String |> Optional)), {Table = Read; Form = Read}
                ("OptionalString", (FieldClass.String |> Optional)), {Table = Read; Form = ReadWrite}
            |] |> Array.map ( fun ((name, _type), permission) ->
                {Column<RandomType>.Parse (name, _type) with Permission = permission})
//
//        let testTable =
//            Table.Create(
//                "time",
//                (fun (t:RandomType) -> t.Id),
//                columns,
//                Server.GetTestData,
//                createFunc,
//                Server.UpdateTestData,
//                Server.DeleteTestData
//                )

        let newName = Var.Create ""
        let loaded = ref false
        div [
              h2 [text "Test"]
              //testTable.ShowTable()
              h2 [text "Games"]
              gameTable.ShowTableWithPages 5

//              iframeAttr[
//                // attr.src "/ansatt" //"about:blank" 
//                attr.src "http://ansatt.norgesbuss.no"
//                attr.id "ansatt-iframe"
//                attr.style "width:500px;height:800px;"
//                ][
//                ]
//                on.load(fun el ev ->
//                    if not !loaded then
//                        el.AppendChild(
//                            (divAttr[attr.id "ansatt-iframe-div"][]).Dom
//                        )|> ignore
//                        JQuery.JQuery("#ansatt-iframe div#ansatt-iframe-div").Html("<object data=\"http://ansatt.norgesbuss.no\">") |> ignore
//                    loaded := true
//                    )
//                on.load(fun el ev ->
//                    JS.Window.Location.Assign "http://ansatt.norgesbuss.no"
//                )
//                attr.id "ansatt-iframe"
//                on.load(fun el ev ->
//                    //JS.Window.Location.Assign "http://ansatt.norgesbuss.no"
//                    if not !loaded then
//                        JS.Document.GetElementById("ansatt-iframe").SetAttribute("src", "http://ansatt.norgesbuss.no")
//                    loaded := true
//                )
//              divAttr[
//                attr.id "ansatt-iframe-div"
//                
//                on.afterRender(fun el ->
//                    if not !loaded then
//                        JQuery.JQuery("div#ansatt-iframe-div").Load("//ansatt.norgesbuss.no") |> ignore
//                    loaded := true
//                    )
//              
//              ][]
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
            | Buss ->
                Content.Page(
                    IndexTemplate.Doc(
                        body = [div [ client <@ Client.BusBody() @>]]
                     ))