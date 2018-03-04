namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Html

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

    /// <summary>    
    /// InputType is a lens and should be used in a form that represents a Var<'DataType>
    /// </summary>  
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
        | SelectDoc of (('DataType -> int) * ('DataType -> int -> 'DataType) * Var<Map<int, unit -> Doc>>)
        | SelectWithString of (('DataType -> string) * ('DataType -> string -> 'DataType) * Var<Map<string,string>>)
        | StringOption of GetSetter<'DataType, string option>
        | TextOption of GetSetter<'DataType, string option>
        | IntOption of GetSetter<'DataType, int option>
        | FloatOption of GetSetter<'DataType, float option>
        | DateOption of GetSetter<'DataType, System.DateTime option>
        | TimeOption of GetSetter<'DataType, System.TimeSpan option>
        | SelectOption of (('DataType -> int option) * ('DataType -> int option -> 'DataType) * Var<Map<int,string>>)
        | StringSeq of GetSetter<'DataType, seq<string>>
        member this.formWrapper label' extraAttrs content =
            div([ attr.``class`` "form-group fruit-form-group"] @ extraAttrs)[
                label[attr.``for`` label'][text label']
                content
            ] :> Doc
        member this.errorFormWrapper label' attrs content  =
            div[ attr.``class`` "form-group has-error has-feedback fruit-form-group fruit-has-error fruit-has-feedback"][
                label[attr.``for`` label'][text label']
                content
                span[attr.``class`` "glyphicon glyphicon-remove form-control-feedback"][]
            ] :> Doc
        member this.successFormWrapper label' attrs content  =
            div[ attr.``class`` "form-group has-success has-feedback fruit-form-group fruit-has-success fruit-has-feedback"][
                label[attr.``for`` label'][text label']
                content
                span[attr.``class`` "glyphicon glyphicon-ok form-control-feedback"][]
            ] :> Doc
        member this.show (label' : string, attrs : Var<'DataType option> -> Attr list, formWrapper) =
            let baseAttrs = 
                [
                    attr.id label'
                    attr.name label'
                    attr.``class`` "form-control fruit-form-control"
                ]
            fun (t' : Var<'DataType option>) ->
                match this with
                | Disabled (getter) ->

                    Doc.BindView ( fun t'' ->
                        let s = SomeOrDefault getter List.Empty t''
                        div (attr.disabled "disabled" :: baseAttrs) s |> formWrapper (attrs t')
                    ) t'.View
                    
                | String (getter, setter) ->
                    let s = Var.Lens t' (SomeOrDefault getter "") (SomeSetter setter)
                    Doc.Input baseAttrs s |> formWrapper (attrs t')
                | GenericString (typeAttr,getter, setter) ->
                    let s = Var.Lens t' (SomeOrDefault getter "") (SomeSetter setter)
                    Doc.Input (baseAttrs @ [attr.``type`` typeAttr] ) s |> formWrapper (attrs t')
                | StringOption (getter, setter) ->
                    OptionalInput.StringField label' getter setter t'
                | StringSeq (getter, setter) ->
                    MultipleInput.MultipleInputType<'DataType,string>.StringSeq label' getter setter t'
                | Text (getter, setter) ->
                    let s = Var.Lens t' (SomeOrDefault getter "") (SomeSetter setter)
                    Doc.InputArea baseAttrs s |> formWrapper (attrs t')
                | TextOption (getter, setter) ->
                    OptionalInput.TextField label' getter setter t'
                | Int (getter, setter) ->
                    let s = Var.Lens t' (SomeOrDefault getter 0) (SomeSetter setter)
                    Doc.IntInputUnchecked baseAttrs s |> formWrapper (attrs t')
                | IntOption (getter, setter) ->
                    OptionalInput.IntField label' getter setter t'
                | Float (getter, setter) ->
                    let s = Var.Lens t' (SomeOrDefault getter 0.) (SomeSetter setter)
                    Doc.FloatInputUnchecked baseAttrs s |> formWrapper (attrs t')
                | FloatOption (getter, setter) ->
                    OptionalInput.FloatField label' getter setter t'
                | Bool (getter, setter) ->
                    let s = Var.Lens t' (SomeOrDefault getter false) (SomeSetter setter)
                    div(attrs t' @ [ attr.``class`` "checkbox"])[
                        label [] [
                            Doc.CheckBox [] s
                            text label'
                        ]
                    ] :> Doc
                | Time (getter, setter) ->
                    let TimeToTicks (t: System.TimeSpan) = t.Ticks
                    let SetTicksToTime t s = SomeSetter setter t (System.TimeSpan.FromTicks s)
                    let timeLens = Var.Lens t' (SomeOrDefault (TimeToTicks << getter) 0L) SetTicksToTime
                    Time.Timepicker timeLens baseAttrs (attrs t') label'
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
                    OptionalInput.TimeField label' (attrs t') (OptionTimeToTicks << getter) SetOptionTicksToTime t'

                | Date (getter, setter) ->
                    let DateTimeToDate (t : System.DateTime) = new Date(t.Year,t.Month - 1, t.Day)
                    let DateToDateTime (t : Date) = System.DateTime.Parse(t.ToDateString()).AddMinutes(- t.GetTimezoneOffset())
                    let s = Var.Lens t' (SomeOrDefault (DateTimeToDate << getter) (new Date())) (fun t s -> Some <| (setter t.Value <| DateToDateTime s))
                    Time.Datepicker'' s baseAttrs (attrs t') label'
                | DateOption (getter, setter) ->
                    let OptionDateTimeToDate (t : System.DateTime option) =
                        match t with
                        | Some t' -> Some <| new Date(t'.Year, t'.Month - 1, t'.Day)
                        | None -> None
                    OptionalInput.DateField label' (attrs t') (OptionDateTimeToDate << getter) setter t'
                | Select (getter, setter, options) ->
                    let s = Var.Lens t' (SomeOrDefault (Some << getter) None) (fun t s -> match s with |Some v -> Some <| setter t.Value v | None -> t)
                    let optionsAsDocs = Var.Lens options ( Map.map (fun _ v () -> text v )) (fun m _ -> m)
                    Doc.BindView( fun t ->
                        Select.SelectInt [] optionsAsDocs s t' |> this.formWrapper label' (attrs t' @ [ attr.``class`` "form-group fruit-form-group"])
                    ) t'.View
                | SelectDoc (getter, setter, options) ->
                    let s = Var.Lens t' (SomeOrDefault (Some << getter) None) (fun t s -> match s with |Some v -> Some <| setter t.Value v | None -> t)
                    Doc.BindView( fun t ->
                        Select.SelectDoc [] options s t' |> this.formWrapper label' (attrs t' @ [ attr.``class`` "form-group fruit-form-group"])
                    ) t'.View
                | SelectWithString (getter, setter, options) ->
                    let s = Var.Lens t' (SomeOrDefault (Some << getter) None) (fun t s -> match s with |Some v -> Some <| setter t.Value v | None -> t)
                    let optionsAsDocs = Var.Lens options ( Map.map (fun _ v () -> text v )) (fun m _ -> m)
                    Doc.BindView( fun t ->
                        Select.SelectString [] optionsAsDocs s t' |> this.formWrapper label' (attrs t' @ [ attr.``class`` "form-group fruit-form-group"])
                    ) t'.View
                | SelectOption (getter, setter, options) ->
                    let s = Var.Lens t' (SomeOrDefault (fun t -> match getter t with None -> Some 0 | v -> v) (Some 0)) (fun t -> function Some 0 -> Some <| setter t.Value None | v -> Some <| setter t.Value v )
                    let optionsAsDocs = 
                        Var.Lens 
                            options 
                            (fun optionMap ->  
                                if optionMap.ContainsKey 0 then
                                    Map.map (fun _ v () -> text v ) optionMap
                                else
                                    optionMap.Add (0, "-")
                                    |> Map.map (fun _ v () -> text v )
                                ) (fun m _ -> m)
                    Doc.BindView( fun t ->
                        Select.SelectInt [] optionsAsDocs s t' |> this.formWrapper label' (attrs t' @ [ attr.``class`` "form-group fruit-form-group"])
                    ) t'.View
        member this.show (label') =
            let attrs (_: Var<'DataType option>) : Attr list = []
//                [
//                    attr.id label'
//                    attr.name label'
//                    attr.``class`` "form-control fruit-form-control"
                //]
            this.show(label', attrs, this.formWrapper label')

