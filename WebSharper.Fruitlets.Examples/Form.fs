namespace WebSharper.Fruitlets.Examples

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Server
open WebSharper.Fruitlets


[<JavaScript>]
module FormClient = 

    open WebSharper.Fruitlets.Form
    type InputForm =
        {
            First: string
            Last: string
            Age: int
            Hobbies: seq<string>
        }

    let emptyForm = Var.Create <| Some {First= ""; Last = ""; Age = 0; Hobbies = ["One"; "Two"]}

    let FormPage() =
        let hasCapital (s:string) =
            s.Length > 0
            &&
            List.exists ((=) s.[0]) ['A'..'Z']

        let firstHasCapital =
            {
                ValidationFunction = ValidationFunction<InputForm>.Sync (fun i -> hasCapital i.First)
                OnError = ValidationError<InputForm>.Simple "First name should start with a capital"
            }
        let firstNotEmpty =
            {
                ValidationFunction = ValidationFunction<InputForm>.Sync (fun i -> i.First <> "")
                OnError = ValidationError<InputForm>.Simple "First name can not be empty"
            }
        let lastMoreThan2 =
            {
                ValidationFunction = ValidationFunction<InputForm>.Sync (fun i -> i.Last.Length > 1)
                OnError = ValidationError<InputForm>.Simple "Last name should contain more than 1 character"
            }
            
        let askTheServer speed =
            {
                ValidationFunction = ValidationFunction<InputForm>.Async (fun i -> Server.AskTheServer (List.ofSeq i.Hobbies) speed)
                OnError = ValidationError<InputForm>.Sync (fun i -> sprintf "Hi %s you forgot to enter hobbies %i" i.First speed)
            }

        let testForm =
            {
                Id = 1
                Fields =
                    [
                        FormField<InputForm>.Create ("First", [firstHasCapital; firstNotEmpty], Input.String ((fun i -> i.First), fun i f -> {i with First = f}))
                        FormField<InputForm>.Create ("Last", [lastMoreThan2], Input.String ((fun i -> i.Last), fun i l -> {i with Last = l}))
                        FormField<InputForm>.Create ("Hobbies", [], Input.String ((fun i -> String.concat ";" i.Hobbies), fun i l -> {i with Hobbies = l.Split ';'}))
                        FormField<InputForm>.Create ("Hobbies", [askTheServer 2000; askTheServer 0; askTheServer 5000; askTheServer 1000], Input.StringSeq ((fun i -> i.Hobbies), fun i l -> {i with Hobbies = l}))
                    ]
                SubmitButtonText = "Submit"
                SubmitSuccess = "Success"
                SubmitFailure = "Failure"
                OnSubmit = Sync <| fun (t: InputForm option) el ev -> Console.Log t; true
            }

        div[
              h2 [text "Testform"]
              div <| testForm.show emptyForm
        ]