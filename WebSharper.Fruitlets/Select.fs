// Helpers for Input "select"
namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Html

// Bootstrap select fields
[<JavaScript>]
module Select =
    [<Inline "$this.options[$this.selectedIndex].value">]
    let private getSelected this = X<string>
    [<Inline "$this.selectedIndex">]
    let private selectedIndex this = X<int>
    
    type GenericSelect<'K, 'T> when 'K: comparison =
        {
            Options: Var<Map<'K, unit -> Doc>>
            SelectedLens: Var<'K option>
            SelectedOrigin: Var<'T option>
            TransformToKeyFromString: string -> 'K
            TransformToStringFromKey: 'K -> string
            Default: 'K
        }
        member this.Show attrs =
            let detectChangeInOptionsAndItem = View.Map2 (fun opt target -> opt, target) this.Options.View this.SelectedLens.View 
            Doc.BindView( fun ((map : Map<'K, unit -> Doc>), (target: 'K option)) ->
                let mapList = Map.toList map
                let current =
                    match target with
                    | Some value -> value
                    | _ -> this.Default
                select
                    ([
                        attr.``class`` "fruit-form-select form-control"
                        on.change (fun el _ -> this.SelectedLens.Set << Some << this.TransformToKeyFromString <| getSelected el)
                        on.afterRender (fun el ->
                            try
                                // if an item is loaded and there exists a list, but the value selected is not the same as the target, then select something in the list and update the item accordingly  
                                if selectedIndex el > -1 then
                                    match (this.SelectedOrigin.Value, getSelected el, List.tryHead mapList) with
                                    | Some _, selectedValue, Some (key, _) when this.TransformToKeyFromString selectedValue <> current -> this.SelectedLens.Set <| Some key
                                    | _ -> ()
                            with
                                | exn -> Console.Log exn
                            )
                        ] @ attrs )
                    (
                        mapList
                        |> List.map ( fun (key, value) ->
                            Doc.Element
                                "option"
                                [
                                    attr.``class`` "fruit-form-select-option"
                                    attr.value <| this.TransformToStringFromKey key
                                    (if current = key
                                    then attr.selected "selected"
                                    else
                                        Attr.Empty)
                                ] [value () ] :> Doc
                            )
                    ) :> Doc
            ) detectChangeInOptionsAndItem
            
        member this.ShowDropDown attrs =
            let detectChangeInOptionsAndItem = 
                View.Map2 
                    (fun (opt: Map<'K, unit -> Doc>) target -> 
                        opt, 
                            target 
                            |> Option.bind (fun key -> 
                                match opt.TryFind key with
                                | Some v -> Some (key, v)
                                | None -> None))
                    this.Options.View 
                    this.SelectedLens.View 
            Doc.BindView( fun ((map : Map<'K, unit -> Doc>), (target: ('K * (unit -> Doc)) option)) ->
                let mapList = Map.toList map
                let currentKey, currentDoc =
                    match target with
                    | Some value -> value
                    | _ -> this.Default, (fun () -> text " ")
                div
                    [attr.``class`` "dropdown fruit-form-dropdown"]
                    [
                        button
                            [
                                attr.``class`` "btn dropdown-toggle"
                                attr.``type`` "button"
                                attr.``data-`` "toggle" "dropdown"
                            ]
                            [
                                currentDoc ()                  
                                span[attr.``class`` "caret"][]
                            ]
                        ul
                            (attrs @ [ attr.``class`` "dropdown-menu" ] )
                            (
                                mapList
                                |> List.map ( fun (key, value) ->
                                    li
                                        [ on.click (fun _ _ -> this.SelectedLens.Set <| Some key) ] 
                                        [ a[attr.href "javascript:false;"][value ()] ] :> Doc
                                    )
                            ) :> Doc
                    ]
                ) detectChangeInOptionsAndItem
            
    let internal SelectInt attrs (options: Var<Map<int, unit -> Doc>>) (targetLens:Var<int option>) (datatype: Var<'T option>) =
        let selectObject =
            {
                Options = options
                SelectedLens = targetLens
                SelectedOrigin = datatype
                TransformToKeyFromString = int
                TransformToStringFromKey = string
                Default = -1
                
            }
        selectObject.Show attrs
    let SelectDoc attrs  (options: Var<Map<int, unit -> Doc>>) (targetLens:Var<int option>) (datatype: Var<'T option>) =
        let selectObject =
            {
                Options = options
                SelectedLens = targetLens
                SelectedOrigin = datatype
                TransformToKeyFromString = int
                TransformToStringFromKey = string
                Default = -1
                
            }
        selectObject.ShowDropDown attrs
    let internal SelectString attrs (options: Var<Map<string, unit -> Doc>>) (targetLens:Var<string option>) (datatype: Var<'T option>) =
        let selectObject =
            {
                Options = options
                SelectedLens = targetLens
                SelectedOrigin = datatype
                TransformToKeyFromString = id
                TransformToStringFromKey = id
                Default = "-"
                
            }
        selectObject.Show attrs
