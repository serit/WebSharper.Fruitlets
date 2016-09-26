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

    let Body () =
        let gameList = ListModel.Create (fun (r: Server.GameObject) -> r.Title) Array.empty

        async {
            let! gl = Server.GetGames()
            gameList.Set gl
        } |> Async.Start

        let inputTitleLens (g: Server.GameObject) =
            let v = Var.Create g.Title
            [Doc.Input [on.change (fun el ev -> gameList.UpdateBy ( fun g -> Some {g with Title = v.Value}) g.Title)] v :> Doc]


        let columns =
//            Server.GetGameNames()
//            |> Array.map (fun name ->
//                let mapfunc = (fun (g : Server.GameObject) ->
//                    let result = GetField name g
//                    As result
//                    match result with
//                    | :? int as i -> Column.SimpleColumn (name, (fun g -> i))
//                    | :? float as f -> Column.SimpleColumn (name, (fun g -> f))
//                    | :? string as s -> Column.SimpleColumn (name, (fun g -> s))
//                    | _ -> Column.SimpleColumn (name, (fun g -> string result))
//                    )
//                mapfunc
//            )
//        Console.Log columns
            [|
                Column.SimpleSortColumn ("Title" , (fun (r: Server.GameObject) -> r.Title))
                Column.Plain ("Editable title", (fun g -> []), (fun g -> inputTitleLens g ))
                Column.Editable ("Editable title special", (fun g -> []), (fun g -> inputTitleLens g ), None, (fun g -> StringInput ((fun g -> g.Title),(fun g t -> {g with Title = t}))))
                Column.SimpleSortColumn ("Rating" , (fun (r: Server.GameObject) -> r.Rating))
                Column.SimpleSortColumn ("Voters" , (fun (r: Server.GameObject) -> r.Voters))
                Column.SimpleSortColumn ("Rank" , (fun (r: Server.GameObject) -> r.Rank))
                //Column.Plain ("", (fun g -> []), (fun g -> [buttonAttr[on.click (fun el ev -> Console.Log g)] [text "Save"]] ))
            |]

        let gameTable =
            {
                Id = "gameTable"
                RowData = gameList
                RowIdFunc = (fun r -> r.Title)
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