namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html


[<JavaScript>]
module OptionalInput =
    
    let private SomeOrDefault getter def = function
        | Some t' -> getter t'
        | None -> def
        
    let private OptionToBool (t : 'T option) = t.IsSome

    /// Input where value is 'T option
    type OptionalInputType<'DataType, 'ValueType> =
        {
            Label: string
            Getter: 'DataType -> 'ValueType option
            Setter: 'DataType -> 'ValueType option -> 'DataType
        }
        member this.formWrapper content =
            divAttr[ attr.``class`` "form-group fruit-form-group"][
                labelAttr[attr.``for`` this.Label][text this.Label]
                content
            ] :> Doc
        member this.defaultAttrs =
            [
                attr.id this.Label
                attr.``class`` "form-control fruit-form-control"
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

                divAttr[attr.``class`` "form-inline fruit-form-inline"][
                    Doc.CheckBox [attr.``class`` "fruit-form-checkbox"] sBool
                    divAttr[attr.styleDyn <| hideField sBool][inputField]
                ] |> this.formWrapper
        // default: optionToValue defaultValue inputType
        member __.OptionToDefault defaultValue = function
            | Some s -> s
            | None -> defaultValue
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
            fun (t': Var<'DataType option>) ->
                let current = (match t'.Value with |Some v -> field.OptionToDefault 0 (getter v) | None -> 0 )
                let Select'' attrs (selectLens: IRef<int option>) =
                    //Select' attrs options selectLens t'
                    Select.SelectInt attrs options selectLens t'
                let sGeneric : IRef<int option> = Var.Lens t' (SomeOrDefault optionToValueGetter None) (fun t s' -> Some <| field.Setter t.Value (s'))
                let inputField = Select'' field.defaultAttrs sGeneric // inputType this.defaultAttrs sGeneric
                field.Show inputField t' (field.OptionToDefault 0)
