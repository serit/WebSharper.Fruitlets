namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

// Bootstrap formfields
[<JavaScript>]
module Form =
    [<Core.Attributes.Inline "$this.options[$this.selectedIndex].value">]
    let private getSelected this = X<string>

    let private Select' attrs (options: Var<Map<int,string>>) (current: int) (targetLens:IRef<int option>) =
        Doc.BindView( fun (map : Map<int,string>) ->
            selectAttr
                ([
                    on.change (fun el _ ->
                        targetLens.Set << Some << int <| getSelected el)
                    ] @ attrs )
                (

                    [for item in map do
                        yield
                            Doc.Element
                                "option"
                                [
                                    attr.value <| string item.Key
                                    (if current = item.Key
                                    then
                                        attr.selected "selected"
                                    else
                                        Attr.Empty)
                                ] [text item.Value] :> Doc

                    ]) :> Doc
        ) options.View

//    let private SelectOption' attrs (options: Var<Map<int,string>>) (targetLens:IRef<int option>) =
//        let mutable current = 0
//        View.Sink( fun t ->
//            match t with
//            | Some v -> current <- v
//            | None -> current <- 0
//
//        ) targetLens.View
//
//        Select' attrs options current targetLens

//        Doc.BindView( fun (map : Map<int,string>) ->
//            selectAttr
//                ([
//                    on.change (fun el _ ->
//                        targetLens.Set << Some << int <| getSelected el)
//                    ] @ attrs )
//                ([
//                    for item in map do
//                        yield
//                            Doc.Element
//                                "option"
//                                [
//                                    attr.value <| string item.Key
//                                    (if current = item.Key
//                                    then
//                                        attr.selected "selected"
//                                    else
//                                        Attr.Empty)
//                                ] [text item.Value] :> Doc
//
//                    ]) :> Doc
//        ) options.View

    let private SomeOrDefault getter def = function
        | Some t' -> getter t'
        | None -> def
    let private SomeSetter setter (t: 'DataType option) s =
        Some <| setter t.Value s
    let private OptionToBool = function
        | Some _ -> true
        | None -> false

    type OptionalInputType<'DataType, 'ValueType> =
        {
            Label: string
            Getter: 'DataType -> 'ValueType option
            Setter: 'DataType -> 'ValueType option -> 'DataType
        }
        member this.formWrapper content =
            divAttr[ attr.``class`` "form-group"][
                labelAttr[attr.``for`` this.Label][text this.Label]
                content
            ] :> Doc
        member this.defaultAttrs =
            [
                attr.id this.Label
                attr.``class`` "form-control"
            ]
        member this.OptionToBool =
            fun (t': Var<'DataType option>) optionToValue ->
                let s = Var.Lens t' (SomeOrDefault this.Getter None) (fun t s -> Some <| this.Setter t.Value s)
                Var.Lens t' (SomeOrDefault (OptionToBool << this.Getter) false) (fun t s' -> Some <| this.Setter t.Value (if s' then Some <| optionToValue s.Value else None))
        member this.Show inputField =

            let hideField (sBool : IRef<bool>) =
                View.Map (function
                    | true -> "display:inline-block"
                    | false -> "display:none"
                ) sBool.View
            fun (t': Var<'DataType option>) optionToValue ->
                let sBool = this.OptionToBool t' optionToValue

                divAttr[attr.``class`` "form-inline"][
                    Doc.CheckBox [] sBool
                    divAttr[attr.styleDyn <| hideField sBool][inputField]
                ] |> this.formWrapper
        // default: optionToValue defaultValue inputType
        member this.OptionToDefault defaultValue = function
            |Some s -> s
            |None -> defaultValue
        member this.GenericField defaultValue inputType =
            let optionToValueGetter = (fun t -> (this.OptionToDefault defaultValue << this.Getter <| t))
            fun t' ->
                let sGeneric = Var.Lens t' (SomeOrDefault optionToValueGetter defaultValue) (fun t s' -> Some <| this.Setter t.Value (Some s'))
                let inputField = inputType this.defaultAttrs sGeneric
                this.Show inputField t' (this.OptionToDefault defaultValue)
        static member StringField label' getter setter =
            {Label = label'; Getter = getter; Setter = setter}.GenericField "" Doc.Input
        static member TextField label' getter setter =
            {Label = label'; Getter = getter; Setter = setter}.GenericField "" Doc.InputArea
        static member IntField label' getter setter =
            {Label = label'; Getter = getter; Setter = setter}.GenericField 0 Doc.IntInputUnchecked
        static member FloatField label' getter setter =
            {Label = label'; Getter = getter; Setter = setter}.GenericField 0. Doc.FloatInputUnchecked
        static member TimeField label' getter setter =
            let TimeInput attrs timeLens = Time.Timepicker timeLens attrs label'
            {Label = label'; Getter = getter; Setter = setter}.GenericField 0L TimeInput
        static member DateField label' getter setter =
            let DateToDateTime (t : Date option) =
                match t with
                | Some t' -> Some <| System.DateTime.Parse(t'.ToDateString())
                | None -> None
            let setter' = (fun (t: 'DataType) s -> setter t <| DateToDateTime s)
            let DatePicker attrs dateLens = Time.Datepicker'' dateLens attrs label'
            {Label = label'; Getter = getter; Setter = setter'}.GenericField (new Date()) DatePicker
        /// Under construction: Logic of current value should be fixed: Reset when t' is updated
        static member SelectField label' (getter: 'DataType -> int option) setter options =

            let field = {Label = label'; Getter = getter; Setter = setter}

            let optionToValueGetter = (fun t -> (field.Getter t))
            fun (t': Var<'Datatype option>) ->
                let Select'' attrs (selectLens: IRef<int option>) =
                    Select' attrs options (match t'.Value with |Some v -> field.OptionToDefault 0 (getter v) | None -> 0 ) selectLens
                let sGeneric : IRef<int option> = Var.Lens t' (SomeOrDefault optionToValueGetter None) (fun t s' -> Some <| field.Setter t.Value (s'))
                let inputField = Select'' field.defaultAttrs sGeneric // inputType this.defaultAttrs sGeneric
                field.Show inputField t' (field.OptionToDefault 0)



    /// InputType is a lens and should be used in a form that represents a Var<'DataType>
    type InputType<'DataType> =
        | Disabled of ('DataType -> Doc list)
        | String of (('DataType -> string) * ('DataType -> string -> 'DataType))
        | Text of (('DataType -> string) * ('DataType -> string -> 'DataType))
        | Int of (('DataType -> int) * ('DataType -> int -> 'DataType))
        | Float of (('DataType -> float) * ('DataType -> float -> 'DataType))
        | Bool of (('DataType -> bool) * ('DataType -> bool -> 'DataType))
        | Date of (('DataType -> System.DateTime) * ('DataType -> System.DateTime -> 'DataType))
        //| Time of (('DataType -> int64) * ('DataType -> int64 -> 'DataType))
        | Time of (('DataType -> System.TimeSpan) * ('DataType -> System.TimeSpan -> 'DataType))
        | Select of (('DataType -> int) * ('DataType -> int -> 'DataType) * Var<Map<int,string>>)
        | StringOption of (('DataType -> string option) * ('DataType -> string option -> 'DataType))
        | TextOption of (('DataType -> string option) * ('DataType -> string option -> 'DataType))
        | IntOption of (('DataType -> int option) * ('DataType -> int option -> 'DataType))
        | FloatOption of (('DataType -> float option) * ('DataType -> float option -> 'DataType))
//        | BoolOption of (('DataType -> bool option) * ('DataType -> bool option -> 'DataType))
        | DateOption of (('DataType -> System.DateTime option) * ('DataType -> System.DateTime option -> 'DataType))
        | TimeOption of (('DataType -> System.TimeSpan option) * ('DataType -> System.TimeSpan option -> 'DataType))
        | SelectOption of (('DataType -> int option) * ('DataType -> int option -> 'DataType) * Var<Map<int,string>>)
        member this.formWrapper label' content =
            divAttr[ attr.``class`` "form-group"][
                labelAttr[attr.``for`` label'][text label']
                content
            ] :> Doc
        member this.errorFormWrapper label' content =
            divAttr[ attr.``class`` "form-group has-error has-feedback"][
                labelAttr[attr.``for`` label'][text label']
                content
                spanAttr[attr.``class`` "glyphicon glyphicon-remove form-control-feedback"][]
            ] :> Doc
        member this.successFormWrapper label' content =
            divAttr[ attr.``class`` "form-group has-success has-feedback"][
                labelAttr[attr.``for`` label'][text label']
                content
                spanAttr[attr.``class`` "glyphicon glyphicon-ok form-control-feedback"][]
            ] :> Doc
        member this.show (label' : string, attrs : Attr list, formWrapper) =
            //let formWrapper = this.formWrapper label'
            fun (t' : Var<'DataType option>) ->
                match this with
                | Disabled (getter) ->
                    Doc.BindView( fun t'' ->
                        let s = SomeOrDefault getter List.Empty t''
                        divAttr (attr.disabled "disabled"::attrs) s |> formWrapper
                    ) t'.View

                | String (getter, setter) ->
                    let s = Var.Lens t' (SomeOrDefault getter "") (SomeSetter setter)
                    Doc.Input attrs s |> formWrapper
                | StringOption (getter, setter) ->
                    OptionalInputType<'DataType,string>.StringField label' getter setter t'
                | Text (getter, setter) ->
                    let s = Var.Lens t' (SomeOrDefault getter "") (SomeSetter setter)
                    Doc.InputArea attrs s |> formWrapper
                | TextOption (getter, setter) ->
                    OptionalInputType<'DataType,string>.TextField label' getter setter t'
                | Int (getter, setter) ->
                    let s = Var.Lens t' (SomeOrDefault getter 0) (SomeSetter setter)
                    Doc.IntInputUnchecked attrs s |> formWrapper
                | IntOption (getter, setter) ->
                    OptionalInputType<'DataType,int>.IntField label' getter setter t'
                | Float (getter, setter) ->
                    let s = Var.Lens t' (SomeOrDefault getter 0.) (SomeSetter setter)
                    Doc.FloatInputUnchecked attrs s |> formWrapper
                | FloatOption (getter, setter) ->
                    OptionalInputType<'DataType,float>.FloatField label' getter setter t'
                | Bool (getter, setter) ->
                    let s = Var.Lens t' (SomeOrDefault getter false) (SomeSetter setter)
                    divAttr[ attr.``class`` "checkbox"][

                        label[
                            Doc.CheckBox [] s
                            text label'
                        ]
                    ] :> Doc
                | Time (getter, setter) ->
                    let TimeToTicks (t: System.TimeSpan) = t.Ticks
                    let SetTicksToTime t s = SomeSetter setter t (System.TimeSpan.FromTicks s)
                    let timeLens = Var.Lens t' (SomeOrDefault (TimeToTicks << getter) 0L) SetTicksToTime //(SomeSetter setter)
                    Time.Timepicker timeLens attrs label'
                | TimeOption (getter, setter) ->
                    let OptionTimeToTicks (t: System.TimeSpan option) =
                        match t with
                        |Some s -> Some s.Ticks
                        |None -> None
                    let SetOptionTicksToTime t s =
                        match s with
                        | Some s' -> Some (System.TimeSpan.FromTicks s')
                        | None -> None
                        |> setter t
                    OptionalInputType<'DataType,System.TimeSpan>.TimeField label' (OptionTimeToTicks << getter) SetOptionTicksToTime t'

                | Date (getter, setter) ->
                    let DateTimeToDate (t : System.DateTime) = new Date(t.Year,t.Month - 1, t.Day)
                    let DateToDateTime (t : Date) = System.DateTime.Parse(t.ToDateString())
                    let s = Var.Lens t' (SomeOrDefault (DateTimeToDate << getter) (new Date())) (fun t s -> Some <| (setter t.Value <| DateToDateTime s))
                    Time.Datepicker'' s attrs label'
                | DateOption (getter, setter) ->
                    let OptionDateTimeToDate (t : System.DateTime option) =
                        match t with
                        | Some t' -> Some <| new Date(t'.Year, t'.Month - 1, t'.Day)
                        | None -> None
                    OptionalInputType<'DataType,System.DateTime>.DateField label' (OptionDateTimeToDate << getter) setter t'
                | Select (getter, setter, options) ->
                    let s = Var.Lens t' (SomeOrDefault (Some << getter) None) (fun t s -> match s with |Some v -> Some <| setter t.Value v | None -> t)
                    Doc.BindView( fun t ->
                        Select' attrs options (match t with | Some t' -> getter t' | None -> 0) s |> this.formWrapper label'
                    ) t'.View
                | SelectOption (getter, setter, options) ->
                    OptionalInputType<'DataType,float>.SelectField label' getter setter options t'
        member this.show (label') =
            let attrs =
                [
                    attr.id label'
                    attr.``class`` "form-control"
                ]
            this.show(label', attrs, this.formWrapper label')

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
    type Form<'DataType> =
        {
            Fields: FormField<'DataType> list
            SubmitButtonText: string
            SubmitSuccess: string
            SubmitFailure: string
            OnSubmit: ('DataType option -> Dom.Element -> Dom.MouseEvent -> bool)
        }
        member this.show =
            fun (t : Var<'DataType option>) ->
                let localFields = Var.Create (this.Fields)
                View.Sink ( fun t' ->
                    match t' with
                    | Some t'' ->
                        localFields.Value |> List.iter ( fun field -> field.HasError <- not <| field.Prevalidate t'')
                    | None -> ()
                ) t.View
                let errorMsg = Var.Create []
                let successMsg = Var.Create ""
                let errorAlert =
                    Doc.BindView (fun errs ->
                        if List.isEmpty errs
                        then Doc.Empty
                        else
                            errs
                            |> List.map (fun err -> li [text err] :> Doc)
                            |> fun errList -> [ul errList :> Doc]
                            |> divAttr[attr.``class`` "alert alert-danger"] :> Doc
                    ) errorMsg.View
                let successAlert =
                    Doc.BindView (fun succ ->
                        if succ = ""
                        then Doc.Empty
                        else
                            divAttr[attr.``class`` "alert alert-success"][text succ] :> Doc
                    ) successMsg.View
                let fields =
                    Doc.BindView( fun fields ->
                        fields
                        |> List.map (fun field ->
                            if field.HasError
                            then
                                let attrs =
                                    [
                                        attr.id field.Label
                                        attr.``class`` "form-control has-error"
                                    ]
                                field.Input.show (field.Label, attrs, field.Input.errorFormWrapper field.Label) t
                            else
                                let attrs =
                                    [
                                        attr.id field.Label
                                        attr.``class`` "form-control has-success"
                                    ]
                                field.Input.show (field.Label, attrs, field.Input.successFormWrapper field.Label) t
                            )
                            |> Doc.Concat
                    ) localFields.View
                let buttons =
                    t.View
                    |> Doc.BindView( fun t' ->
                        buttonAttr[
                            attr.``class`` "btn btn-info"
                            on.click(fun el ev ->
                                successMsg.Value <- ""
                                match t' with
                                | Some t'' ->
                                    let errorFields =
                                        this.Fields
                                        |> List.map (fun field ->
                                            { field with
                                                Validations =
                                                    field.Validations
                                                    |> List.filter (fun validation ->
                                                        validation.ValidationFunction t''
                                                        |> not
                                                    )
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
                                            if this.OnSubmit t' el ev
                                            then
                                                errorMsg.Value <- []
                                                successMsg.Value <- this.SubmitSuccess
                                            else
                                                errorMsg.Value <- [this.SubmitFailure])
                                    else ()


                                | None -> ()
                                )
                            ][text this.SubmitButtonText] :> Doc
                         )
                errorAlert :: [successAlert] @ [fields] @ [buttons]