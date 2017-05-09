namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

// Bootstrap formfields
[<JavaScript>]
module Form =

    open Input

    (* Field, FormField and Validation are used to generate forms with validation *)
    type Validation<'DataType> =
        {
            ValidationFunction: 'DataType -> bool
            OnError: string
        }

    type FormField<'DataType> =
        {
            Label: string
            Validations: Validation<'DataType> list
            Input: InputType<'DataType>
            mutable HasError: bool
        }
        member this.Prevalidate t =
            not <| List.isEmpty this.Validations
            &&
            this.Validations
            |> List.filter (fun v -> not <| v.ValidationFunction t)
            |> List.isEmpty
        static member empty =
            {
                Label = ""
                Validations = []
                Input = Disabled (fun _ -> [])
                HasError = false
            }
        static member Create (label', validations, input') =
            {
                Label = label'
                Validations = validations
                Input = input'
                HasError = false
            }
            
    type FormSubmitFunction<'DataType> =
        | Sync of ('DataType option -> Dom.Element -> Dom.MouseEvent -> bool)
        | Async of ('DataType option -> Dom.Element -> Dom.MouseEvent -> Async<Result.Result<bool,string>>)

    type Form<'DataType> =
        {
            // incase multiple forms are defined on the same page
            Id: int
            Fields: FormField<'DataType> list
            SubmitButtonText: string
            SubmitSuccess: string
            SubmitFailure: string
            // todo: OnSubmit should have a variant where the result is asynchronous
            OnSubmit: FormSubmitFunction<'DataType>
        }
        member this.show =
            fun (t : Var<'DataType option>) ->

                let localFields = Var.Create (this.Fields)
                let mutable formVersion = -1
                View.Sink ( fun t' ->
                    // form verion updates to 0 on initialization
                    formVersion <- formVersion + 1
                    match t' with
                    | Some t'' ->
                        localFields.Value |> List.iter ( fun field -> field.HasError <- not <| field.Prevalidate t'')
                    | None -> ()
                ) t.View
                let errorMsg = Var.Create []
                let successMsg = Var.Create ""
                let errorAlert =
                    Doc.BindView (fun errs ->
                        if List.isEmpty errs then 
                            Doc.Empty
                        else
                            errs
                            |> List.map (fun err -> li [text err] :> Doc)
                            |> fun errList -> [ul errList :> Doc]
                            |> divAttr[attr.``class`` "alert alert-danger fruit-alert fruit-alert-danger"] :> Doc
                    ) errorMsg.View
                let successAlert =
                    Doc.BindView (fun succ ->
                        if succ = "" then Doc.Empty
                        else
                            divAttr[attr.``class`` "alert alert-success fruit-alert fruit-alert-success"][text succ] :> Doc
                    ) successMsg.View
                let fields =
                    Doc.BindView( fun fields ->
                        fields
                        |> List.map (fun field ->
                            if formVersion < 1 then
                                field.Input.show field.Label t
                            elif field.HasError then
                                let attrs =
                                    [
                                        attr.id field.Label
                                        attr.name field.Label
                                        attr.``class`` "form-control has-error fruit-form-control fruit-has-error"
                                    ]
                                field.Input.show (field.Label, attrs, field.Input.errorFormWrapper field.Label) t
                            else
                                let attrs =
                                    [
                                        attr.name field.Label
                                        attr.``class`` "form-control has-success fruit-form-control fruit-has-success"
                                    ]
                                field.Input.show (field.Label, attrs, field.Input.successFormWrapper field.Label) t
                            )
                            |> Doc.Concat
                    ) localFields.View
                let buttons =
                    t.View
                    |> Doc.BindView( fun t' ->
                        buttonAttr[
                            attr.id <| sprintf "fruit-form-submit-%i" this.Id
                            attr.``class`` "btn btn-info fruit-btn fruit-btn-save"
                            on.click(fun el ev ->
                                successMsg.Value <- ""
                                formVersion <- formVersion + 1
                                match t' with
                                | Some t'' ->
                                    let errorFields =
                                        this.Fields
                                        |> List.map (fun field ->
                                            { field with
                                                Validations =
                                                    field.Validations
                                                    |> List.filter (fun validation -> not <| validation.ValidationFunction t'')
                                            }
                                        )
                                        //|> List.concat
                                    errorMsg.Value <-
                                        errorFields
                                        |> List.map ( fun field -> field.Validations )
                                        |> List.concat
                                        |> List.map ( fun validation -> validation.OnError )
                                    localFields.Value <-
                                        List.map2 ( fun a b ->
                                            { a with HasError = not <| List.isEmpty b.Validations }
                                        ) this.Fields errorFields

                                    if
                                        errorFields
                                        |> List.map (fun field -> field.Validations)
                                        |> List.concat
                                        |> List.isEmpty
                                    then
                                        (
                                            match this.OnSubmit with
                                            | Sync onSubmit ->
                                            
                                                if onSubmit t' el ev then
                                                    errorMsg.Value <- []
                                                    successMsg.Value <- this.SubmitSuccess
                                                else
                                                    errorMsg.Value <- [this.SubmitFailure]
                                            | Async onSubmit ->
                                                async{
                                                    let! result = onSubmit t' el ev
                                                    match result with
                                                    | Result.Success true ->
                                                        errorMsg.Value <- []
                                                        successMsg.Value <- this.SubmitSuccess
                                                    | Result.Success false ->
                                                        errorMsg.Value <- [this.SubmitFailure]
                                                    | Result.Failure msg ->
                                                        errorMsg.Value <- [msg]
                                                } |> Async.Start
                                            )
                                    else ()

                                | None -> ()
                                )
                            ][text this.SubmitButtonText] :> Doc
                         )
                errorAlert :: [successAlert] @ [fields] @ [buttons]