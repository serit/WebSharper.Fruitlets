namespace WebSharper.Fruitlets.Examples

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client
open WebSharper.UI.Server
open WebSharper.Fruitlets
open WebSharper.Sitelets

open FSharp.Data
open Server

[<JavaScript>]
module Client =

    open WebSharper.Fruitlets.Table
    open WebSharper.Fruitlets.Column
    let now = Server.now()
    let today = System.DateTime.Now

    let Body () =

        let testList = Var.Create <| Map([ ( 1, fun () -> div[attr.style "background: red"][text "a"] :> Doc ); ( 102, fun () -> div[attr.style "background: green"][text "b"] :> Doc ) ])
        let testList2 = Var.Create <| Map([ ( 1, "a" ); ( 2, "b" ); ( 102, "c" ) ])

        let columns =
            [|
                {Column.EditColumn("In top 10", (fun g -> g.Rank <= 10), (fun g v -> g)) with
                    EditField = Some << InputField <| Input.Bool((fun g -> g.Rank <= 10), (fun g v -> g))
                    Permission = {Table = ReadWrite; Form = Invisible}}
                {Column.EditColumn("Title", (fun g -> g.Title), (fun g v -> {g with Title = v})) with
                    EditFieldAttrList =
                        Some <| fun g' ->
                            [ on.clickView g'.View (fun _ _ g'' -> JS.Alert <| sprintf "%A" g'')]}
                {Column.EditDateColumn("Date", (fun g -> Some today), (fun g _ -> g)) with
                    EditFieldAttrList =
                        Some <| fun g' ->
                            [   attr.style "background: yellow"
                                on.clickView g'.View (fun _ _ g'' -> JS.Alert <| sprintf "%A" g'')]}
                {Column.EditDateColumn("Date", (fun g -> today), (fun g _ -> g)) with
                    EditFieldAttrList =
                        Some <| fun g' ->
                            [   attr.style "background: yellow"
                                on.clickView g'.View (fun _ _ g'' -> JS.Alert <| sprintf "%A" g'')]}
                Column.EditColumn("Rating", (fun g -> g.Rating), (fun g v -> {g with Rating = v}))
                Column.EditColumn("Voters", (fun g -> g.Voters), (fun g v -> {g with Voters = v}))
                Column.EditColumn("Rank", (fun g -> g.Rank), (fun g v -> {g with Rank = v}))
                {Column.EditSelectColumn("Rank", (fun g -> g.Rank), (fun g v -> {g with Rank = v}), testList ) with
                    AttrList = Some <| fun _ -> [attr.``class`` "bg-danger"]
                    DocList = fun item ->
                        [
                            Input.SelectDoc((fun g -> g.Rank), (fun g v -> {g with Rank = v}), testList).show "Rank:" <| Var.Create (Some item)
                        ]


                }
                Column.EditSelectColumn("Rank", (fun g -> Some g.Rank), (fun g -> function Some v -> {g with Rank = v} | None -> {g with Rank = 0}), testList2 )
            |]

        let create () : Async<Result.Result<GameObject, string>> = async{return Result.Success <| GameObject.Create()}
        let update item : Async<Result.Result<GameObject, string>> = async{return Result.Success <| item}
        let delete item : Async<Result.Result<GameObject, string>> = async{return Result.Success <| item}
        let gameTable : Table<string, Server.GameObject> = Table.Create("gameTable", (fun (r: Server.GameObject) -> r.Title), columns, Server.GetGames, create, update, delete)

        let validation g =
            if g.Title = "Test" then
                Result.Failure "That is not a real title"
            else Result.Success true

        let gameTable' = { gameTable with Settings = {gameTable.Settings with ModalSaveValidation = Some validation}}

        div [][
              h2 [] [text "Games"]
              gameTable'.ShowTableWithPages 50
        ]
