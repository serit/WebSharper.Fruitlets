namespace WebSharper.Fruitlets.Examples

open WebSharper
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html
open WebSharper.Web

open FSharp.Data
open FSharp.Quotations

open System.Reflection

module Server =

    open WebSharper.Fruitlets.Table

    type Fields =
        | TypeFields of System.Type []
        | PropertyFields of PropertyInfo []

    let retrieve ( T : System.Type) =
            T.GetProperties()

    let TableConstructor (data: seq<'T>) (T: System.Type) (idFunc: 'T -> 'U) =
//        try
            let Id = (typeof<'T>).Name
            let fields =
                retrieve T
            let columns =
                fields
                |> Array.map ( fun pi ->
                    let meth : ('T * array<_> -> obj) = pi.GetMethod.Invoke
                    Column.SimpleColumn (pi.Name , (fun t -> meth(t, Array.empty)))
                )
            (Id, idFunc, columns, data)
//        with
//        | ex -> failwith ex.Message


    [<Literal>]
    let gamePath = "..\\data\\games.csv"

    type Game =
        CsvProvider<gamePath>

    [<JavaScript>]
    type GameObject =
        { Title : string; Rating : float; Rank: int; Voters: int; Year: int}
        static member Create () =
            { Title = ""; Rating = 0.0; Rank = 0; Voters = 0; Year = 0}
        static member Create (title:string) =
            { GameObject.Create() with Title = title}
        static member Create (title:string, rating:float, rank:int,voters:int, year) =
            { Title = title; Rating = rating; Rank = rank; Voters = voters; Year = year}


    [<Rpc>]
    let ReflectedGames() =

        let year (title:string) =
            try
                    ( title.Trim ')') |> fun s -> s.Split '(' |> Array.item 1 |> int
            with
                | _ -> 0

        async {
            let Games = Game.Load(gamePath)
            return
                Games.Rows
                //|> Seq.map (fun row -> GameObject.Create(row.Title, (row.``Avg Rating`` |> float), row.``Board Game Rank``,row.``Num Voters``, year row.Title))
                |> Seq.toArray
            //return TableConstructor Games.Rows (Games.Rows.GetType()) (fun g -> g.Title)
        }

    [<Rpc>]
    let now() = System.DateTime.Now.TimeOfDay

    [<Rpc>]
    let GetGames () =
        async {
            let Games = Game.Load(gamePath)
            return
                Games.Rows
                |> Seq.map (fun t ->

                        let year =
                            try
                                 ( t.Title.Trim ')') |> fun s -> s.Split '(' |> Array.item 1 |> int
                            with
                                | _ -> 0
                        GameObject.Create(t.Title,t.``Avg Rating`` |> float, t.``Board Game Rank``, t.``Num Voters``, year))
                |> Seq.toArray
        }

    type RandomType =
        {
            Id : int
            Time : System.TimeSpan
            Date : System.DateTime
        }

    [<Rpc>]
    let GetTestData () =
        async {
            return
                Seq.init 5 (fun i -> i, System.DateTime.Now)
                |> Seq.map (fun (i,dt) -> {Id = i;Time = dt.TimeOfDay; Date = dt})
                |> Seq.toArray

        }