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

    type InputForm =
        {
            First: string
            Last: string
            Age: int
        }

    let emptyForm = Var.Create <| Some {First= ""; Last = ""; Age = 0}

    open WebSharper.Fruitlets.Table
    open WebSharper.Fruitlets.Form
    let now = Server.now()

    let Body () =

        let hasCapital (s:string) =
            s.Length > 0
            &&
            List.exists ((=) s.[0]) ['A'..'Z']

        let firstHasCapital =
            {
                ValidationFunction = (fun i -> hasCapital i.First)
                OnError = "First name should start with a capital"
            }
        let firstNotEmpty =
            {
                ValidationFunction = (fun i -> i.First <> "")
                OnError = "First name can not be empty"
            }
        let lastMoreThan2 =
            {
                ValidationFunction = (fun i -> i.Last.Length > 2)
                OnError = "Last name should contain more than 2 characters"
            }

        let testForm =
            {
                Fields =
                    [
                        FormField<InputForm>.Create ("First", [firstHasCapital; firstNotEmpty], InputType.String ((fun i -> i.First), fun i f -> {i with First = f}))
                        FormField<InputForm>.Create ("Last", [lastMoreThan2], InputType.String ((fun i -> i.Last), fun i l -> {i with Last = l}))
                    ]
                SubmitButtonText = "Submit"
                SubmitSuccess = "Success"
                SubmitFailure = "Failure"
                OnSubmit = fun (t: InputForm option) el ev -> Console.Log t; true
            }

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
              h2 [text "Testform"]
              div <| testForm.show emptyForm
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