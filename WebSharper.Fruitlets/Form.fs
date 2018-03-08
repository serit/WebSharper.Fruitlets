// iTODO: Split into two or three, move out validation and form status
namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Html

// Bootstrap formfields
[<JavaScript>]
module Form =

    open Input
    
    /// Function to validate form input
    type ValidationFunction<'DataType> =
        /// Typically a client side validation function
        | Sync of ('DataType -> bool)
        /// Typically a server side validation function called through Rpc
        /// <returns>
        /// A Var<bool option> which will be updated once the asynchronous function completes
        /// </returns>
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
            
            
    /// Types of validation errors
    type ValidationError<'DataType> =
        /// Simple string validation error
        | Simple of string
        /// Validation error dependent on the item that it validates for
        | Sync of ('DataType -> string)
        //| Async of ('DataType -> Async<Result.Result<string,string>>)
        member this.Value t =
            match this with
            | Sync f -> f t 
            | Simple str -> str
    
    /// Validation contains a function to validate and an error to return on failed validation
    type Validation<'DataType> =
        {
            ValidationFunction: ValidationFunction<'DataType>
            OnError: ValidationError<'DataType>
        }

    /// An inputfield in a Form
    type FormField<'DataType> =
        {
            Label: string
            /// Each inputfield runs through a list of validations on form submit
            Validations: Validation<'DataType> list
            Input: InputType<'DataType>
            /// The inputfield will get an updated look based on validation result
            mutable HasError: bool option
        }
        /// Before submitting the contents of the form, each validation function is run.
        /// Only after all validation functions return succefully, will submit happen
        member this.PrevalidateView t =            
            let initialValidate = View.Const (Some true)
            this.Validations 
            |> List.map (fun v -> (v.ValidationFunction.validates t).View)
            |> List.fold FormField<'DataType>.accumulatorViewMapBool initialValidate
        member this.PrevalidateAndErrorView t =            
            let initialValidate = View.Const (Some true)
            this.Validations 
            |> List.map (fun v -> (v.ValidationFunction.validates t).View, v.OnError)

        /// If no validation function is defined for this field, no validation is necessary
        member this.PrevalidateNecessary =  
            not <| List.isEmpty this.Validations
        /// Used in prevalidate fold. This fold will return None as long as one of the Validation functions is not complete
        /// Once all validation functions have terminated, the fold returns Boolean AND over all of their results
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
                HasError = None
            }
        static member Create (label', validations, input') =
            {
                Label = label'
                Validations = validations
                Input = input'
                HasError = None
            }

    /// Types of form submit functions        
    type FormSubmitFunction<'DataType> =
        /// Client side submit function (triggered by a button click)
        | Sync of ('DataType option -> Dom.Element -> Dom.MouseEvent -> bool)
        /// Server side submit function (triggered by a button click)
        | Async of ('DataType option -> Dom.Element -> Dom.MouseEvent -> Async<Result.Result<bool,string>>)
    
    type ButtonText = string
    type ReCaptchaKey = string
    
    /// Types of Form submit, used to generate the submit button
    type SubmitType =
        /// the simple type will generate a simple submit button, 
        | Simple of ButtonText
        /// The recapctha type requires a recaptcha key
        /// which can be generated here: https://www.google.com/recaptcha/admin
        /// the recaptcha script is loaded in the code
        /// It is assumed that there is at most one recaptcha button per page  
        | ReCaptcha of ReCaptchaKey * ButtonText

        
    [<Direct "grecaptcha.render($item, {'sitekey': $siteKey})">]
    let reCaptchaRender item siteKey = X<Unit>
    [<Direct "grecaptcha.getResponse()">]
    let reCaptchaResponse () = X<bool>
    [<Inline "typeof grecaptcha.render == 'undefined'">]
    let reCaptchaNotLoaded () = X<bool>

    /// Different statuses for the form. Will influence appearance of the form, input fields, errorboxes and buttons
    type FormStatus =
        | NoStatus
        | Loaded
        | Validating
        | Submitting
        | LoadingCaptcha
        | Submitted
        | Error of string list

    /// The form
    /// <example>
    /// let isPrime =
    ///    {
    ///         ValidationFunction = ValidationFunction<InputForm>.Sync (fun i -> IsPrime i)
    ///         OnError = ValidationError<InputForm>.Sync (fun i -> sprintf "%i is not a prime number" i)
    ///    }
    /// let primeForm : Form<int> =
    ///     {
    ///         Id = 1
    ///         Fields =
    ///             [
    ///                 FormField<InputForm>.Create ("Enter a prime number", [isPrime], Input.Int (id, fun _ i -> i))
    ///             ]
    ///         SubmitButton = ReCaptcha ("6LeT2CMUAAAAAMihux1kajdTxA8kw041f5pB7gCH", "Submit")
    ///         SubmitSuccess = "Successfully entered a prime number"
    ///         SubmitFailure = "Failure"
    ///         OnSubmit = Sync <| fun (t: InputForm option) el ev -> Console.Log t; true
    ///         Status = Var.Create NoStatus
    ///     }
    /// primeForm.show 2
    /// </example>
    type Form<'DataType> =
        {
            // in case multiple forms are defined on the same page, an Id will prevent any conflicts during render and submit
            Id: int
            /// input fields on the form
            Fields: FormField<'DataType> list
            /// submit button (plain or recaptcha)
            SubmitButton: SubmitType
            /// Message on submit success
            SubmitSuccess: string
            /// Message on submit failure
            SubmitFailure: string
            /// OnSubmit defines the submit function
            OnSubmit: FormSubmitFunction<'DataType>
            /// Current status of the form
            Status: Var<FormStatus>
        }
        member this.show =
            fun (t : Var<'DataType option>) ->

                let localFields = Var.Create (this.Fields)
                let mutable formVersion = -1 // for recaptcha
                this.Status.Value <- Loaded
                View.Sink ( fun t' ->
                    // form verion updates to 0 on initialization
                    formVersion <- formVersion + 1
//                    match t' with
//                    | Some t'' ->
//                        localFields.Value 
//                        |> List.iter ( fun field -> ()
//                            //field
////                            if field.PrevalidateNecessary then
////                                let prevalidateView = field.PrevalidateView t''
////                                View.Sink ( function
////                                    | Some noErrors -> field.HasError <- not noErrors
////                                    | _ -> ()
////                                ) prevalidateView
//                                //field.HasError <- not <| field.Prevalidate t''
//                        )
//                    | None -> ()
                ) t.View

                // will display any problems with the form
                let alertBox =
                    Doc.BindView (function
                        | Error errs ->
                            errs
                            |> List.map (fun err -> li [] [text err] :> Doc)
                            |> fun errList -> [ul [] errList :> Doc]
                            |> div[attr.``class`` "alert alert-danger fruit-alert fruit-alert-danger"] :> Doc
                        | Submitted ->
                            div[attr.``class`` "alert alert-success fruit-alert fruit-alert-success"][text this.SubmitSuccess] :> Doc
                        | Validating -> 
                            div[attr.``class`` "alert alert-info fruit-alert fruit-alert-info"][
                                i[attr.``class`` "fa fa-refresh fa-spin"][]
                                text " Validating"] :> Doc
                        | Submitting  -> 
                            div[attr.``class`` "alert alert-info fruit-alert fruit-alert-info"][
                                i[attr.``class`` "fa fa-refresh fa-spin"][]
                                text " Submitting"] :> Doc
                        | _ -> Doc.Empty
                    ) this.Status.View

                // display all fields (with error or success marker once the form has been submitted)
                let fields =
                    Doc.BindView( fun fields ->
                        fields
                        |> List.map (fun field ->
                            if formVersion < 1 then
                                field.Input.show field.Label t
                            else
                               match field.HasError with
                               | Some true ->
                                    let attrs = fun (_: Var<'DataType option>) ->
                                        [
                                            attr.id field.Label
                                            attr.name field.Label
                                            attr.``class`` "form-control has-error fruit-form-control fruit-has-error"
                                        ]
                                    field.Input.show (field.Label, attrs, field.Input.errorFormWrapper field.Label) t
                               | Some false ->
                                    let attrs = fun (_: Var<'DataType option>) ->
                                        [
                                            attr.name field.Label
                                            attr.``class`` "form-control has-success fruit-form-control fruit-has-success"
                                        ]
                                    field.Input.show (field.Label, attrs, field.Input.successFormWrapper field.Label) t
                               | _ ->
                                    field.Input.show field.Label t
                            )
                            |> Doc.Concat
                    ) localFields.View

                // run on submit    
                let submit t' t'' el ev =
                    
                    // use the prevalidate view, to generate error. 
                    // on some false, show error
                    // on some true, start submit    

                    let validationViews =
                        this.Fields
                        |> List.map (fun field -> field.PrevalidateAndErrorView t'')

                    this.Status.Value <- Validating
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
                    View.Sink (fun errorList -> 
                        if List.length errorList > 0 then
                            this.Status.Value <- Error errorList 
                        ) errorListView

                    // update local fields   
                    let fields =
                        List.map2 (fun a b ->
                            let fieldValidation =
                                b
                                |> List.map fst
                                |> List.fold FormField<'DataType>.accumulatorViewMapBool (View.Const (Some true))
                            View.Map (function
                                | Some false -> {a with HasError = Some true}
                                | Some true -> {a with HasError = Some false}
                                | _ -> a
                            ) fieldValidation
                                    
                        ) this.Fields validationViews
                        |> List.fold (View.Map2 (fun acc' fv -> acc' @ [fv])) (View.Const [])
                    
                    View.Sink (fun newFields -> localFields.Value <- newFields) fields 

                    // do submit if everything has validated
                    validatedView
                    |> View.Sink (function
                        | Some true ->
                            this.Status.Value <- Submitting
                            match this.OnSubmit with
                            | Sync onSubmit ->                                            
                                if onSubmit t' el ev then
                                    this.Status.Value <- Submitted
                                else
                                    this.Status.Value <- Error [this.SubmitFailure]
                            | Async onSubmit ->
                                async{
                                    let! result = onSubmit t' el ev
                                    match result with
                                    | Result.Success true ->
                                        this.Status.Value <- Submitted
                                    | Result.Success false ->
                                        this.Status.Value <- Error [this.SubmitFailure]
                                    | Result.Failure msg ->
                                        this.Status.Value <- Error [msg]
                                } |> Async.Start                                             
                        | _ -> ()
                    )   

                let buttons =
                    match this.SubmitButton with
                    | Simple buttonText ->
                        // the appearance of the button depends on the form status
                        View.Map2 (fun a b -> a,b) t.View this.Status.View
                        |> Doc.BindView( fun (t', status) ->
                            let clickFunction el ev = 
                                // form version updates on each submit. Error fields will not show until after the first submit 
                                formVersion <- formVersion + 1
                                match t' with
                                | Some t'' -> submit t' t'' el ev
                                | None -> ()           
                            match status with
                            | Loaded 
                            | Error _ 
                            | Submitted ->
                                button [
                                    attr.id <| sprintf "fruit-form-submit-%i" this.Id
                                    attr.``class`` "btn btn-info fruit-btn fruit-btn-save"
                                    on.click clickFunction
                                    ] [text buttonText] :> Doc
                            | _ -> Doc.Empty
                        )
                    | ReCaptcha (reCaptchaKey, buttonText) ->
                        this.Status.Value <- LoadingCaptcha
                        // everything that's not in the recaptcha-div
                        let partOutsideRecaptcha =
                            t.View
                            |> Doc.BindView( fun t' ->
                                let clickFunction el ev =
                                    // form version updates on each submit. Error fields will not show until after the first submit
                                    formVersion <- formVersion + 1
                                    match t' with
                                    | Some t'' -> 
                                        if reCaptchaResponse () then submit t' t'' el ev
                                        else this.Status.Value <- Error ["Either you are a robot or reCaptcha was not properly loaded"]
                                    | None -> ()
                                // give the recaptcha scripts a second to load before attempting to check recaptcha keys                                
                                let afterRender (el: Dom.Element)  =                                       
                                    let rec tryLoadText () =
                                        try 
                                            if reCaptchaResponse() then this.Status.Value <- Loaded
                                            else JS.SetTimeout tryLoadText 1000 |> ignore
                                        with | exn -> Console.Log exn
                                    JS.SetTimeout tryLoadText 1000 |> ignore
                                div[
                                    on.afterRender afterRender
                                ][
                                    Doc.BindView( function
                                        | Loaded 
                                        | Error _ 
                                        | Submitted ->
                                            button[
                                                attr.id <| sprintf "fruit-form-submit-%i" this.Id
                                                attr.``class`` "btn btn-info fruit-btn fruit-btn-save"
                                                on.click clickFunction
                                            ][
                                                text buttonText
                                            ] :> Doc       
                                        | _ -> Doc.Empty                       
                                    ) this.Status.View
                                ]
                            )

                        //let widget = reCaptchaRender (sprintf "fruit-form-recaptcha-%i" this.Id) reCaptchaKey 
                        div [] [
                            div [
                                attr.id <| sprintf "fruit-form-recaptcha-placeholder-%i " this.Id
                                attr.``class`` "g-recaptcha"
                                attr.``data-`` "sitekey" reCaptchaKey
                                ] []
                            partOutsideRecaptcha
                        ]:> Doc
                alertBox :: [fields] @ [buttons]