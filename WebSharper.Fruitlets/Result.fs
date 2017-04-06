namespace WebSharper.Fruitlets

open WebSharper

[<JavaScript>]
module Result =

    type Result<'Success, 'Failure> =
    | Success of 'Success
    | Failure of 'Failure
        member this.toOption =
            match this with
            | Success success -> Some success
            | Failure _ -> None
        static member ofOption (option: Option<'Success>) =
            match option with
            | Some success -> Success success
            | None -> Failure "None"
        static member ofOptionWithFail (option: Option<'Success>) (fail:string) =
            match option with
            | Some success -> Success success
            | None -> Failure fail

    let foldResult combine resultSeq  =
        match Seq.length resultSeq with
        | 0 -> Failure "The sequence is empty"
        | _ ->
            Seq.fold(fun acc r ->
                match acc with
                | Success state ->
                    match r with
                    | Success newstate -> Success (combine state newstate)
                    | Failure msg -> Failure msg
                | Failure msg -> Failure msg
            ) (Seq.head resultSeq) (Seq.tail resultSeq)


    let map f =
        function
        | Success x -> Success (f x)
        | Failure x -> Failure x

    let (<!>) f a = map f a

    let join (x: Result<Result<'T, 'X>, 'X>) =
        match x with
        | Success y -> y
        | Failure y -> Failure y

    let bind (f: 'Success -> Result<'T,'U>) =
        function
        | Success x -> f x
        | Failure x -> Failure x

    let (>>=) f a = bind f a
    let (=<<) b a = a >>= b

    let apply (f: Result<'A->'B,'X>) (a:Result<'A,'X>) = // : Result<'A->'B,'X> -> Result<'A,'X> -> Result<'B,'X> =
        match (f, a) with
        | Success f', Success a' -> f' a' |> Success
        | Success _, Failure x -> Failure x
        | Failure x, _ -> Failure x

    let (<*>) f a = apply f a

    let lift2 f x y =
        f <!> x <*> y

    let lift3 f x y z =
        //apply (apply (map f x) y) z
        f <!> x <*> y <*> z


