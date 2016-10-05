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

    [<Inline "$obj[$field]">]
    let private JSGetField field obj = X<_>
    [<Inline "$obj[$field] = $value">]
    let private JSSetField field obj value = X<Unit>

    let GetField obj (field :string) (objtype: Sort.SortableType) =
        match objtype with
        | Sort.I _ -> Sort.I <| JSGetField obj field
        | Sort.S _ -> Sort.S <| JSGetField obj field
        | Sort.F _ -> Sort.F <| JSGetField obj field
        | Sort.D _ -> Sort.D <| JSGetField obj field
        | Sort.B _ -> Sort.B <| JSGetField obj field

    let SetField obj (field :string) value =
        JSSetField obj field value
        field

    open WebSharper.Fruitlets.Table
    let now = Server.now()
//    let showTime (time: System.TimeSpan) =
//        let t = new Date(0,0,0, time.Hours, time.Minutes)
//        sprintf "%02i:%02i" (t.GetHours()) (t.GetMinutes())

    let Body () =
        let gameList = ListModel.Create (fun (r: Server.GameObject) -> r.Rank) Array.empty

        let inputTitleLens (g: Server.GameObject) =
            let v = Var.Create g.Title
            [Doc.Input [on.change (fun el ev -> gameList.UpdateBy ( fun g -> Some {g with Title = v.Value}) g.Rank)] v :> Doc]

        let testList = Var.Create <| Map([(1,"a");(102,"b")])

        let columns =
            [|
                {Column.SimpleSortColumn ("Title" , (fun r -> r.Title)) with EditField = Some(Form.StringInput ((fun r -> r.Title), (fun r t -> {r with Title = t})))}
                {Column.SimpleSortColumn ("Rating" , (fun r -> r.Rating)) with Header = Some(fun () -> div[text "Rating"]:> Doc)}
                Column.SimpleSortColumn ("Voters" , (fun r -> r.Voters))
                Column.SimpleSortColumn ("Rank" , (fun r -> r.Rank))
                {Column.SimpleColumn (" > 2010" , (fun r -> r.Year > 2010)) with
                    EditField = Some(Form.BoolInput ((fun r -> r.Year > 2010), (fun r t -> {r with Title = r.Title + "!!!"})))}
                {Column.SimpleColumn (" Now" , (fun _ -> sprintf "%02i:%02i" now.Hours now.Minutes)) with
                    EditField = Some(Form.TimeInput ((fun r -> now.Ticks), (fun r t -> r)))}
                Column.EditSelectColumn ("Rank 2" , (fun r-> r.Rank), (fun r t -> { r with Rank = t}), testList )
            |]

        let gameTable : Table<int,Server.GameObject> =
            {
                Id = "gameTable"
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
                Column.EditTimeSpanColumn("Time",(fun t -> t.Time.Ticks),(fun t v -> {t with Time = System.TimeSpan.FromTicks v}))
                Column.EditDateColumn("Date",(fun t -> t.Date),(fun t v -> {t with Date = v}))
            |]
        let testTable : Table<int,RandomType> =
            {
                Id = "time"
                DataSource = DataSource.DS<int,RandomType>.Create((fun t -> t.Id), Server.GetTestData)
                Class = [|Striped|]
                Columns = columns
            }
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