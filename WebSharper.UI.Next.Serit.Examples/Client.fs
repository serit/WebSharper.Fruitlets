namespace WebSharper.UI.Next.Serit.Examples

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Serit
open WebSharper.UI.Next.Server
open WebSharper.Sitelets

open FSharp.Data


[<JavaScript>]
module Client =

    [<Inline "$obj[$field]">]
    let GetField field obj = X<obj>

    open WebSharper.UI.Next.Serit.Table
    let now = Server.now()
//    let showTime (time: System.TimeSpan) =
//        let t = new Date(0,0,0, time.Hours, time.Minutes)
//        sprintf "%02i:%02i" (t.GetHours()) (t.GetMinutes())

    let Body () =
        let gameList = ListModel.Create (fun (r: Server.GameObject) -> r.Rank) Array.empty

        async {
            let! gl = Server.GetGames()
            gameList.Set gl
        } |> Async.Start

        let inputTitleLens (g: Server.GameObject) =
            let v = Var.Create g.Title
            [Doc.Input [on.change (fun el ev -> gameList.UpdateBy ( fun g -> Some {g with Title = v.Value}) g.Rank)] v :> Doc]

        let testList = Var.Create <| Map([(1,"a");(102,"b")])

        let columns =
            [|
                {Column.SimpleSortColumn ("Title" , (fun (r: Server.GameObject) -> r.Title)) with EditField = Some(Form.StringInput ((fun r -> r.Title), (fun r t -> {r with Title = t})))}
                Column.SimpleSortColumn ("Rating" , (fun (r: Server.GameObject) -> r.Rating))
                Column.SimpleSortColumn ("Voters" , (fun (r: Server.GameObject) -> r.Voters))
                Column.SimpleSortColumn ("Rank" , (fun (r: Server.GameObject) -> r.Rank))
                {Column.SimpleColumn (" > 2010" , (fun (r: Server.GameObject) -> r.Year > 2010)) with
                    EditField = Some(Form.BoolInput ((fun r -> r.Year > 2010), (fun r t -> {r with Title = r.Title + "!!!"})))}
                {Column.SimpleColumn (" Now" , (fun (r: Server.GameObject) -> sprintf "%02i:%02i" now.Hours now.Minutes)) with
                    EditField = Some(Form.TimeInput ((fun r -> now.Ticks), (fun r t -> r)))}
                Column.EditSelectColumn ("Rank 2" , (fun (r: Server.GameObject) -> r.Rank), (fun r t -> { r with Rank = t}), testList )
            |]

        let gameTable : Table<int,Server.GameObject> =
            {
                Id = "gameTable"
                RowData = DataSource.DataSource<int,Server.GameObject>.Create((fun r -> r.Rank), gameList, (fun g -> g.Rank), (fun g -> gameList.UpdateBy (fun g' -> Some g') g.Rank))
                Class = [| Striped ; Bordered |]
                Columns = columns
                Direction = Asc 3
            }

//        let reflectedgames = Var.Create None
//        async {
//             let! rg = Server.ReflectedGames()
//             reflectedgames.Value <-
//                let tbl = Table<string,Server.Game.Row>.Create rg
//                Some tbl
//        } |> Async.Start

        let newName = Var.Create ""
        div [
              h2 [text "normal"]
              gameTable.ShowTableWithPages 10
              h2 [text "reflected"]
//              Doc.BindView( fun (v : option<Table<_,_>>) ->
//                    match v with
//                    | Some tbl -> tbl.ShowTableWithPages 10
//                    | None -> Doc.Empty
//                ) reflectedgames.View
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