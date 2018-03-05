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
            mutable HasError: bool option
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
                HasError = None
            }
        static member Create (label', validations, input') =
            {
                Label = label'
                Validations = validations
                Input = input'
                HasError = None
            }
            
    type FormSubmitFunction<'DataType> =
        | Sync of ('DataType option -> Dom.Element -> Dom.MouseEvent -> bool)
        | Async of ('DataType option -> Dom.Element -> Dom.MouseEvent -> Async<Result.Result<bool,string>>)
    
    type ButtonText = string
    type ReCaptchaKey = string
    /// <summary>  
    /// the simple type will generate a simple submit button, 
    /// while the recapctha requires a recaptcha key
    /// which can be generated here: https://www.google.com/recaptcha/admin
    /// the recaptcha script is loaded in the code
    /// It is assumed that there is at most one recaptcha button per page
    /// </summary>  
    type SubmitType =
        | Simple of ButtonText
        | ReCaptcha of ReCaptchaKey * ButtonText

        
    [<Direct "grecaptcha.render($item, {'sitekey': $siteKey})">]
    let reCaptchaRender item siteKey = X<Unit>
    [<Direct "grecaptcha.getResponse()">]
    let reCaptchaResponse () = X<bool>
    [<Inline "typeof grecaptcha.render == 'undefined'">]
    let reCaptchaNotLoaded () = X<bool>

    type FormStatus =
        | NoStatus
        | Loaded
        | Validating
        | Submitting
        | LoadingCaptcha
        | Submitted
        | Error of string list

    type Form<'DataType> =
        {
            // incase multiple forms are defined on the same page
            Id: int
            Fields: FormField<'DataType> list
            SubmitButton: SubmitType
            SubmitSuccess: string
            SubmitFailure: string
            // todo: OnSubmit should have a variant where the result is asynchronous
            OnSubmit: FormSubmitFunction<'DataType>
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

                let submit t' t'' el ev =
                    
                    // use the prevalidate view, to generate error. 
                    // on some false, show error
                    // on some true, start submit    

                    let validationViews =
                        this.Fields
                        |> List.map (fun field -> field.PrevalidateAndErrorView t'')

                    //errorMsg.Value <- []
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
                        View.Map2 (fun a b -> a,b) t.View this.Status.View
                        |> Doc.BindView( fun (t', status) ->
                            let clickFunction el ev = 
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
                        //let reCaptchaLoaded = Var.Create false
                        this.Status.Value <- LoadingCaptcha
                        let partOutsideRecaptcha =
                            t.View
                            |> Doc.BindView( fun t' ->
                                let clickFunction el ev =
                                    formVersion <- formVersion + 1
                                    match t' with
                                    | Some t'' -> 
                                        if reCaptchaResponse () then submit t' t'' el ev
                                        else this.Status.Value <- Error ["Either you are a robot or reCaptcha was not properly loaded"]
                                    | None -> ()
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