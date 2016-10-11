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


    let private Select' attrs (l: Var<Map<int,string>>) (current: int) (targetLens:IRef<int option>) =
        let tryItem (map : Map<int,string>) value =
            match map.TryFind value with
            | Some v -> v
            | None -> ""
        Doc.BindView( fun (map : Map<int,string>) ->
            let values = [for (item) in map do yield item.Key]

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
        ) l.View


    /// InputType is a lens and should be used in a form that represents a Var<'DataType>
    type InputType<'DataType> =
        | Disabled of ('DataType -> Doc list)
        | String of (('DataType -> string) * ('DataType -> string -> 'DataType))
        | Text of (('DataType -> string) * ('DataType -> string -> 'DataType))
        | Int of (('DataType -> int) * ('DataType -> int -> 'DataType))
        | Float of (('DataType -> float) * ('DataType -> float -> 'DataType))
        | Bool of (('DataType -> bool) * ('DataType -> bool -> 'DataType))
        | Date of (('DataType -> System.DateTime) * ('DataType -> System.DateTime -> 'DataType))
        | Time of (('DataType -> int64) * ('DataType -> int64 -> 'DataType))
        | Select of (('DataType -> int) * ('DataType -> int -> 'DataType) * Var<Map<int,string>>)
        member this.formWrapper label' content =
            divAttr[ attr.``class`` "form-group"][
                labelAttr[attr.``for`` label'][text label']
                content
            ] :> Doc
        member this.show (label' : string) =
            let attrs =
                [
                    attr.id label'
                    attr.``class`` "form-control"

                ]
            fun (t' : Var<'DataType option>) ->
                match this with
                | Disabled (getter) ->
                    Doc.BindView( fun t'' ->
                        let s = match t'' with | Some t -> getter t | None -> List.empty
                        divAttr (attr.disabled "disabled"::attrs) s |> this.formWrapper label'
                    ) t'.View
                | String (getter, setter) ->
                    let s = Var.Lens t' (fun t -> match t with |Some t' -> getter t' | None -> "") (fun t s -> Some <| setter t.Value s)
                    Doc.Input attrs s |> this.formWrapper label'
                | Text (getter, setter) ->
                    let s = Var.Lens t' (fun t -> match t with |Some t' -> getter t' | None -> "") (fun t s -> Some <| setter t.Value s)
                    Doc.InputArea attrs s |> this.formWrapper label'
                | Int (getter, setter)->
                    let s = Var.Lens t' (fun t -> match t with |Some t' -> getter t' | None -> 0) (fun t s -> Some <| setter t.Value s)
                    Doc.IntInputUnchecked attrs s |> this.formWrapper label'
                | Float (getter, setter)->
                    let s = Var.Lens t' (fun t -> match t with |Some t' -> getter t' | None -> 0.) (fun t s -> Some <| setter t.Value s)
                    Doc.FloatInputUnchecked attrs s |> this.formWrapper label'
                | Bool (getter, setter)->
                    let s = Var.Lens t' (fun t -> match t with |Some t' -> getter t' | None -> false) (fun t s -> Some <| setter t.Value s)
                    divAttr[ attr.``class`` "checkbox"][

                        label[
                            Doc.CheckBox [] s
                            text label'
                        ]
                    ] :> Doc
                | Time (getter, setter) ->
                    let timeLens = Var.Lens t' (fun t -> match t with | Some t' -> getter t' | None -> 0L) (fun t s -> Some <| setter t.Value s)
                    Time.Timepicker timeLens attrs label'

                | Date (getter, setter) ->
                    let DateTimeToDate (t : System.DateTime) = new Date(t.Year,t.Month - 1, t.Day)
                    let DateToDateTime (t : Date) = System.DateTime.Parse(t.ToDateString())
                    let s = Var.Lens t' (fun t -> match t with |Some t' -> DateTimeToDate(getter t') | None -> new Date()) (fun t s -> Some <| (setter t.Value <| DateToDateTime s))
                    Time.Datepicker s attrs label'
                | Select (getter, setter, options) ->
                    let s = Var.Lens t' (fun t -> match t with | Some t' -> Some <| getter t' | None -> None) (fun t s -> match s with |Some v -> Some <| setter t.Value v | None -> t)
                    Doc.BindView( fun t ->
                        Select' attrs options (match t with | Some t' -> getter t' | None -> 0) s|> this.formWrapper label'
                    ) t'.View

    type Validation<'DataType> =
        {
            ValidationFunction: 'DataType -> bool
            OnError: string
        }

    type Form<'DataType> =
        {
            Fields: (string * Validation<'DataType> list * InputType<'DataType>) list
            SubmitButtonText: string
            SubmitSuccess: string
            SubmitFailure: string
            OnSubmit: ('DataType option -> Dom.Element -> Dom.Event -> bool)
        }
        member this.show =
            fun t ->
                let errorMsg = Var.Create ""
                let fields =
                    this.Fields |> List.map (fun (label', _, input') ->
                        input'.show label' t)
                let buttons =
                    t.View
                    |> Doc.BindView( fun t' ->
                        buttonAttr[
                            attr.``class`` "btn btn-info"
                            on.submit(fun el ev ->

                                match t' with
                                | Some t'' ->
                                    let errors =
                                        this.Fields
                                        |> List.map (fun (_, validations, _) ->
                                            validations
                                            |> List.filter (fun validation -> validation.ValidationFunction t'' |> not )
                                        )
                                        |> List.concat
                                    errorMsg.Value <-
                                        errors
                                        |> List.map (fun validation -> validation.OnError)
                                        |> String.concat "\n"
                                    if List.isEmpty errors
                                    then
                                        if this.OnSubmit t' el ev
                                        then
                                            errorMsg.Value <- ""
                                        else
                                            errorMsg.Value <- this.SubmitFailure
                                    else ()
                                | None -> ()
                                )
                            ][text this.SubmitButtonText] :> Doc
                         )
                List.append fields [buttons]