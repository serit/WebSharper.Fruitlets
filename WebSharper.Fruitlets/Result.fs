namespace WebSharper.Fruitlets

open WebSharper
[<JavaScript>]
module Result =

    type Result<'Success, 'Failure> =
    | Success of 'Success
    | Failure of 'Failure
    let map f =
        function
        | Success x -> Success (f x)
        | Failure x -> Failure x
    let join (x: Result<Result<'T, 'X>, 'X>) =
        match x with
        | Success y -> y
        | Failure y -> Failure y
    let bind f =
        function
        | Success x -> f x |> join
        | Failure x -> f x |> join


