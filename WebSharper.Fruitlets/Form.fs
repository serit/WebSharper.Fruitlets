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
    
    type ValidationFunction<'DataType> =
        | Sync of ('DataType -> bool)
        | Async of ('DataType -> Async<Result.Result<bool,string>>)
        member this.validates (t: 'DataType) =
            match this with
            | Sync f -> Var.Create <| Some (f t)
            | Async f -> 
                let varBool = Var.Create <| None
                async{
                    let! b = f t
                    match b with
                    | Result.Success true -> varBool.Value <- Some true
                    | _ -> varBool.Value <- Some false
                } |> Async.Start
                varBool
            
            
    type ValidationError<'DataType> =
        | Simple of string
        | Sync of ('DataType -> string)
        //| Async of ('DataType -> Async<Result.Result<string,string>>)
        member this.Value t =
            match this with
            | Sync f -> f t 
            | Simple str -> str
    
    (* Field, FormField and Validation are used to generate forms with validation *)
    type Validation<'DataType> =
        {
            ValidationFunction: ValidationFunction<'DataType> //'DataType -> bool
            OnError: ValidationError<'DataType>
        }

    type FormField<'DataType> =
        {
            Label: string
            Validations: Validation<'DataType> list
            Input: InputType<'DataType>
            mutable HasError: bool
        }
        member this.PrevalidateView t =            
            let initialValidate = View.Const (Some true)
            this.Validations 
            |> List.map (fun v -> (v.ValidationFunction.validates t).View)
            |> List.fold FormField<'DataType>.accumulatorViewMapBool initialValidate
        member this.PrevalidateAndErrorView t =            
            let initialValidate = View.Const (Some true)
            this.Validations 
            |> List.map (fun v -> (v.ValidationFunction.validates t).View, v.OnError)

        member this.PrevalidateNecessary =  
            not <| List.isEmpty this.Validations
        static member accumulatorViewMapBool (acc: View<bool option>) (t: View<bool option>) =
            View.Map2 (fun acc' t' -> 
                match (acc', t') with
                | Some bAcc, Some b -> Some (b && bAcc)
                | Some _, None -> None
                | None, _ -> None
                ) acc t
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
                        localFields.Value 
                        |> List.iter ( fun field -> 
                            if field.PrevalidateNecessary then
                                let prevalidateView = field.PrevalidateView t''
                                View.Sink ( function
                                    | Some noErrors -> field.HasError <- not noErrors
                                    | _ -> ()
                                ) prevalidateView
                                //field.HasError <- not <| field.Prevalidate t''
                        )
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

                                    // use the prevalidate view, to generate error. 
                                    // on some false, show error
                                    // on some true, start submit    

                                    let validationViews =
                                        this.Fields
                                        |> List.map (fun field -> field.PrevalidateAndErrorView t'')

                                    errorMsg.Value <- []
                                    let accumulatorViewMapString (acc: View<string list>) (t: (View<bool option> * ValidationError<'DataType>)) =
                                        let boolView = fst t
                                        let errorF = snd t
                                        View.Map2 (fun acc' bo -> 
                                            match bo with
                                            | Some false -> acc' @ [errorF.Value t'']
                                            | Some true
                                            | None -> acc'
                                            ) acc boolView
                                            
                                    let errorListView = 
                                        validationViews
                                        |> List.concat
                                        |> List.fold accumulatorViewMapString (View.Const [])
                                    let validatedView = 
                                        validationViews
                                        |> List.concat
                                        |> List.map fst
                                        |> List.fold FormField<'DataType>.accumulatorViewMapBool (View.Const (Some true))
                                    
                                    // update error messages
                                    View.Sink (fun errorList -> errorMsg.Value <- errorList ) errorListView

                                    // update local fields
                                    
                                    let fields =
                                        List.map2 (fun a b ->
                                            let fieldValidation =
                                                b
                                                |> List.map fst
                                                |> List.fold FormField<'DataType>.accumulatorViewMapBool (View.Const (Some true))
                                            View.Map (function
                                                | Some false -> {a with HasError = true}
                                                | _ -> a
                                            ) fieldValidation
                                    
                                        ) this.Fields validationViews
                                        |> List.fold (View.Map2 (fun acc' fv -> acc' @ [fv])) (View.Const [])

                                    View.Sink (fun newFields -> localFields.Value <- newFields) fields 

                                    // do submit if everything has validated
                                    validatedView
                                    |> View.Sink (function
                                        | Some true ->
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
                                        | _ -> ()
                                    )   
                                | None -> ()           
                                
                                )
                            ][text this.SubmitButtonText] :> Doc
                         )
                errorAlert :: [successAlert] @ [fields] @ [buttons]