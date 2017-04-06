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
                Id = 1
                Fields =
                    [
                        FormField<InputForm>.Create ("First", [firstHasCapital; firstNotEmpty], InputType.String ((fun i -> i.First), fun i f -> {i with First = f}))
                        FormField<InputForm>.Create ("Last", [lastMoreThan2], InputType.String ((fun i -> i.Last), fun i l -> {i with Last = l}))
                        FormField<InputForm>.Create ("Hobbies", [], InputType.StringSeq ((fun i -> i.Hobbies), fun i l -> {i with Hobbies = l}))
                    ]
                SubmitButtonText = "Submit"
                SubmitSuccess = "Success"
                SubmitFailure = "Failure"
                OnSubmit = fun (t: InputForm option) el ev -> Console.Log t; true
            }

        div[
              h2 [text "Testform"]
              div <| testForm.show emptyForm
        ]