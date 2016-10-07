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

        let testList = Map([(1,"a");(102,"b")])

//                Column.EditSelectColumn ("Rank 2" , (fun r-> r.Rank), (fun r t -> { r with Rank = t}), testList )
//            |]

        let columns =
            [|
                ("Title", StringField, None)
                ("Rating", FloatField, None)
                ("Voters", IntField, None)
                ("Rank", IntField, None)
                ("Rank", SelectField testList, Some <| (fun () -> text "Rank 2"))
            |]
            |> Array.map Column<GameObject>.Parse


        let gameTable : Table<int,Server.GameObject> =
            {
                Id' = "gameTable"
                DataSource =
                    {DataSource.DS<int,Server.GameObject>.Create(
                        (fun r -> r.Rank),
                        Server.GetGames,
                        (fun () -> async {return 1}),
                        (fun g -> async { gameList.UpdateBy (fun g' -> Some g') g.Rank; return true}),
                        getFunction = (fun i -> async {return gameList.FindByKey 1})
                        )
                        with SortDirection = Sort.Asc 3}
                Class = [| Striped ; Bordered |]
                Columns = columns
            }


        let columns =
            [|
                ("Time", TimeField)
                ("Date", DateField)
            |] |> Array.map Column<RandomType>.Parse
        let testTable = Table.Create("time", (fun (t:RandomType) -> t.Id), columns, Server.GetTestData)

        match testTable.DataSource.CrudFunctions with
        | DataSource.Rpc fs ->
            fs.GetFunc <- Some (fun i -> async {return testTable.DataSource.Model.FindByKey i})
            fs.UpdateFunc <- Some (fun t -> async {return testTable.DataSource.Model.UpdateBy (fun t' -> Some t) t.Id; return true})
            fs.CreateFunc <-
                Some(fun () -> async {
                            let newId = testTable.DataSource.Model.Length
                            testTable.DataSource.Model.Add({Id = newId; Time = System.TimeSpan.FromHours 3.; Date = System.DateTime.Parse("05-10-2016")})
                            return newId})
        | _ -> ()

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