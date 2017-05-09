namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

// Bootstrap formfields
[<JavaScript>]
module Input =

    open OptionalInput
    
    let private SomeOrDefault getter def = function
        | Some t' -> getter t'
        | None -> def
    let private SomeSetter setter (t: 'DataType option) s =
        Some <| setter t.Value s
  
    type Getter<'DataType,'T> = 'DataType -> 'T
    type Setter<'DataType,'T> = 'DataType -> 'T -> 'DataType
    type GetSetter<'DataType,'T> = Getter<'DataType,'T> * Setter<'DataType,'T>

    /// InputType is a lens and should be used in a form that represents a Var<'DataType>
    type InputType<'DataType> =
        | Disabled of ('DataType -> Doc list)
        | String of GetSetter<'DataType, string>
        | GenericString of (string * ('DataType -> string) * ('DataType -> string -> 'DataType))
        | Text of GetSetter<'DataType, string>
        | Int of GetSetter<'DataType, int>
        | Float of GetSetter<'DataType, float>
        | Bool of GetSetter<'DataType, bool>
        | Date of GetSetter<'DataType, System.DateTime>
        | Time of GetSetter<'DataType, System.TimeSpan>
        | Select of (('DataType -> int) * ('DataType -> int -> 'DataType) * Var<Map<int,string>>)
        | SelectWithString of (('DataType -> string) * ('DataType -> string -> 'DataType) * Var<Map<string,string>>)
        | StringOption of GetSetter<'DataType, string option>
        | TextOption of GetSetter<'DataType, string option>
        | IntOption of GetSetter<'DataType, int option>
        | FloatOption of GetSetter<'DataType, float option>
        | DateOption of GetSetter<'DataType, System.DateTime option>
        | TimeOption of GetSetter<'DataType, System.TimeSpan option>
        | SelectOption of (('DataType -> int option) * ('DataType -> int option -> 'DataType) * Var<Map<int,string>>)
        | StringSeq of GetSetter<'DataType, seq<string>>
        member this.formWrapper label' content =
            divAttr[ attr.``class`` "form-group fruit-form-group"][
                labelAttr[attr.``for`` label'][text label']
                content
            ] :> Doc
        member this.errorFormWrapper label' content =
            divAttr[ attr.``class`` "form-group has-error has-feedback fruit-form-group fruit-has-error fruit-has-feedback"][
                labelAttr[attr.``for`` label'][text label']
                content
                spanAttr[attr.``class`` "glyphicon glyphicon-remove form-control-feedback"][]
            ] :> Doc
        member this.successFormWrapper label' content =
            divAttr[ attr.``class`` "form-group has-success has-feedback fruit-form-group fruit-has-success fruit-has-feedback"][
                labelAttr[attr.``for`` label'][text label']
                content
                spanAttr[attr.``class`` "glyphicon glyphicon-ok form-control-feedback"][]
            ] :> Doc
        member this.show (label' : string, attrs : Attr list, formWrapper) =
            fun (t' : Var<'DataType option>) ->
                match this with
                | Disabled (getter) ->

                    Doc.BindView ( fun t'' ->
                        let s = SomeOrDefault getter List.Empty t''
                        divAttr (attr.disabled "disabled" :: attrs) s |> formWrapper
                    ) t'.View
                    
                | String (getter, setter) ->
                    let s = Var.Lens t' (SomeOrDefault getter "") (SomeSetter setter)
                    Doc.Input attrs s |> formWrapper
                | GenericString (typeAttr,getter, setter) ->
                    let s = Var.Lens t' (SomeOrDefault getter "") (SomeSetter setter)
                    Doc.Input (attrs @ [attr.``type`` typeAttr] ) s |> formWrapper
                | StringOption (getter, setter) ->
                    OptionalInputType<'DataType,string>.StringField label' getter setter t'
                | StringSeq (getter, setter) ->
                    MultipleInput.MultipleInputType<'DataType,string>.StringSeq label' getter setter t'
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
                    let timeLens = Var.Lens t' (SomeOrDefault (TimeToTicks << getter) 0L) SetTicksToTime
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
                        Select.SelectInt attrs options s t' |> this.formWrapper label' 
                    ) t'.View
                | SelectWithString (getter, setter, options) ->
                    let s = Var.Lens t' (SomeOrDefault (Some << getter) None) (fun t s -> match s with |Some v -> Some <| setter t.Value v | None -> t)
                    Doc.BindView( fun t ->
                        Select.SelectString attrs options s t' |> this.formWrapper label'
                    ) t'.View
                | SelectOption (getter, setter, options) ->
                    OptionalInputType<'DataType,float>.SelectField label' getter setter options t'
        member this.show (label') =
            let attrs =
                [
                    attr.id label'
                    attr.name label'
                    attr.``class`` "form-control fruit-form-control"
                ]
            this.show(label', attrs, this.formWrapper label')

