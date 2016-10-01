namespace WebSharper.UI.Next.Serit

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html
//open WebSharper.JQueryUI

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



    /// InputType is a lens and should be used in a form that represents a Var<'T>
    type InputType<'T> =
        | StringInput of (('T -> string) * ('T -> string -> 'T))
        | TextInput of (('T -> string) * ('T -> string -> 'T))
        | IntInput of (('T -> int) * ('T -> int -> 'T))
        | FloatInput of (('T -> float) * ('T -> float -> 'T))
        | BoolInput of (('T -> bool) * ('T -> bool -> 'T))
        | DateInput of (('T -> Date) * ('T -> Date -> 'T))
        | TimeInput of (('T -> int64) * ('T -> int64 -> 'T))
        | SelectInput of (('T -> int) * ('T -> int -> 'T) * Var<Map<int,string>>)
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
            (fun (t' : Var<'T option>) ->
                match this with
                | StringInput (f, f') ->
                    let s = Var.Lens t' (fun t -> match t with |Some t' -> f t' | None -> "") (fun t s -> Some <| f' t.Value s)
                    Doc.Input attrs s |> this.formWrapper label'
                | TextInput (f, f') ->
                    let s = Var.Lens t' (fun t -> match t with |Some t' -> f t' | None -> "") (fun t s -> Some <| f' t.Value s)
                    Doc.InputArea attrs s |> this.formWrapper label'
                | IntInput (f, f')->
                    let s = Var.Lens t' (fun t -> match t with |Some t' -> f t' | None -> 0) (fun t s -> Some <| f' t.Value s)
                    Doc.IntInputUnchecked attrs s |> this.formWrapper label'
                | FloatInput (f, f')->
                    let s = Var.Lens t' (fun t -> match t with |Some t' -> f t' | None -> 0.) (fun t s -> Some <| f' t.Value s)
                    Doc.FloatInputUnchecked attrs s |> this.formWrapper label'
                | BoolInput (f, f')->
                    let s = Var.Lens t' (fun t -> match t with |Some t' -> f t' | None -> false) (fun t s -> Some <| f' t.Value s)
                    divAttr[ attr.``class`` "checkbox"][

                        label[
                            Doc.CheckBox [] s
                            text label'
                        ]
                    ] :> Doc
                | TimeInput (f, f') ->
                    // in future this should take into account 12 or 24-hour clock (extra select field :AM/PM)
                    let hour = 60L * 60L * 1000L * 10000L
                    let hourFunc t = (t  % (24L * hour) ) / hour
                    let hourToTime t h = (t - (hourFunc t) * hour ) + h * hour

                    let minute = 60L * 1000L * 10000L
                    let minFunc t = (t % hour) / minute
                    let minToTime t m = (t - (minFunc t) * minute ) + m * minute

                    let showT t = sprintf "%02i" t
                    let hourLens = Var.Lens t' (fun t -> match t with |Some t' -> hourFunc (f t') |> int  | None -> 0) (fun t s -> Some <| f' t.Value (hourToTime (f t.Value) <| int64 s))
                    let minLens = Var.Lens t' (fun t -> match t with |Some t' -> minFunc (f t') |> int | None -> 0) (fun t s -> Some <| f' t.Value (minToTime (f t.Value) <| int64 s))

                    let hourList = [0..23]
                    let minuteList = [0..59]

                    divAttr[attr.``class`` "form-inline"; attr.style "margin-top:10px;"][
                        Doc.Select attrs showT hourList hourLens |> this.formWrapper (label' + " ")
                        Doc.Select attrs showT minuteList minLens |> this.formWrapper (":")
                    ]  :> Doc
                | DateInput (f, f')->
                    let s = Var.Lens t' (fun t -> match t with |Some t' -> f t' | None -> new Date()) (fun t s -> Some <| f' t.Value s)
                    div[text "datepicker..."]  :> Doc
                | SelectInput (f, f', l) ->
                    let s = Var.Lens t' (fun t -> match t with | Some t' -> Some <| f t' | None -> None) (fun t s -> match s with |Some v -> Some <| f' t.Value v | None -> t)
                    Doc.BindView( fun t ->
                        Select' attrs l (match t with | Some t' -> f t' | None -> 0) s|> this.formWrapper label'
                    ) t'.View
//                | SelectInput (f, f', l) ->
//                    let tryItem (map : Map<int,string>) value =
//                        match map.TryFind value with
//                        | Some v -> v
//                        | None -> ""
//                    Doc.BindView( fun (map : Map<int,string>) ->
//                        let values = [for (item) in map do yield item.Key]
//                        let s = Var.Lens t' (fun t -> match t with | Some t' -> Some <| f t' | None -> None) (fun t s -> match s with |Some v -> Some <| f' t.Value v | None -> t)
//                        (Doc.SelectOptional attrs "-" (fun i -> tryItem map i) values s |> this.formWrapper label')
//                    ) l.View
            )

