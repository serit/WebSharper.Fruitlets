namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

// Bootstrap select fields
[<JavaScript>]
module Select =
    [<Inline "$this.options[$this.selectedIndex].value">]
    let private getSelected this = X<string>
    [<Inline "$this.selectedIndex">]
    let private selectedIndex this = X<int>
    
    type GenericSelect<'K, 'T> when 'K: comparison =
        {
            Options: IRef<Map<'K, unit -> Doc>>
            SelectedLens: IRef<'K option>
            SelectedOrigin: IRef<'T option>
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
                selectAttr
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
                    | _ -> this.Default, (fun () -> text "-")
                divAttr
                    [attr.``class`` "dropdown fruit-form-dropdown"]
                    [
                        buttonAttr
                            [
                                attr.``class`` "btn dropdown-toggle"
                                attr.``type`` "button"
                                attr.``data-`` "toggle" "dropdown"
                            ]
                            [
                                currentDoc ()                  
                                spanAttr[attr.``class`` "caret"][]
                            ]
                        ulAttr
                            (attrs @ [
                                attr.``class`` "dropdown-menu"
                                //on.change (fun el _ -> this.SelectedLens.Set << Some << this.TransformToKeyFromString <| getSelected el)
//                                on.afterRender (fun el ->
//                                    try
//                                        // if an item is loaded and there exists a list, but the value selected is not the same as the target, then select something in the list and update the item accordingly  
//                                        if selectedIndex el > -1 then
//                                            match (this.SelectedOrigin.Value, getSelected el, List.tryHead mapList) with
//                                            | Some _, selectedValue, Some (key, _) when this.TransformToKeyFromString selectedValue <> currentKey -> this.SelectedLens.Set <| Some key
//                                            | _ -> ()
//                                    with
//                                        | exn -> Console.Log exn
//                                    )
                                ] )
                            (
                                mapList
                                |> List.map ( fun (key, value) ->
                                    liAttr
                                        [
                                            on.click (fun _ _ -> this.SelectedLens.Set <| Some key)
                                            //attr.value <| this.TransformToStringFromKey key
//                                            (if currentKey = key
//                                            then attr.``class`` "selected"
//                                            else
//                                                Attr.Empty)
                                        ] [
                                            aAttr[attr.href "javascript:return false;"][value ()]
                                            ] :> Doc
                                    )
                            ) :> Doc
                    ]
                ) detectChangeInOptionsAndItem
            
    let internal SelectInt attrs (options: IRef<Map<int, unit -> Doc>>) (targetLens:IRef<int option>) (datatype: IRef<'T option>) =
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
    let SelectDoc attrs  (options: IRef<Map<int, unit -> Doc>>) (targetLens:IRef<int option>) (datatype: IRef<'T option>) =
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
    let internal SelectString attrs (options: IRef<Map<string, unit -> Doc>>) (targetLens:IRef<string option>) (datatype: IRef<'T option>) =
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
//
//
//    let private Select' attrs (options: Var<Map<int,string>>) (targetLens:IRef<int option>) (datatype: IRef<'T option>) =
//        let detectChangeInOptionsAndItem = View.Map2 (fun opt target -> opt, target) options.View targetLens.View 
//        Doc.BindView( fun ((map : Map<int,string>), (target: int option)) ->
//            let mapList = Map.toList map
//            let current =
//                match target with
//                | Some value -> value
//                | _ -> -1
//            selectAttr
//                ([
//                    attr.``class`` "fruit-form-select"
//                    on.change (fun el _ -> targetLens.Set << Some << int <| getSelected el)
//                    on.afterRender (fun el ->
//                        try
//                            // if an item is loaded and there exists a list, but the value selected is not the same as the target, then select something in the list and update the item accordingly  
//                            if selectedIndex el > -1 then
//                                match (datatype.Value, getSelected el, List.tryHead mapList) with
//                                | Some _, selectedValue, Some (key, _) when int selectedValue <> current -> targetLens.Set <| Some key
//                                | _ -> ()
//                        with
//                            | exn -> Console.Log exn
//                        )
//                    ] @ attrs )
//                (
//                    mapList
//                    |> List.map ( fun (key, value) ->
//                        Doc.Element
//                            "option"
//                            [
//                                attr.``class`` "fruit-form-select-option"
//                                attr.value <| string key
//                                (if current = key
//                                then attr.selected "selected"
//                                else
//                                    Attr.Empty)
//                            ] [text value] :> Doc
//                        )
//                ) :> Doc
//        ) detectChangeInOptionsAndItem
//    let private SelectWithString' attrs (options: Var<Map<string,string>>) (targetLens:IRef<string option>) (datatype: IRef<'T option>)  =
//        let detectChangeInOptionsAndItem = View.Map2 (fun opt target -> opt, target) options.View targetLens.View 
//        Doc.BindView( fun ((map : Map<string,string>), (target: string option)) ->
//            let mapList = Map.toList map
//            let current =
//                match target with
//                | Some value -> value
//                | None -> "-"
//            selectAttr
//                ([
//                    attr.``class`` "fruit-form-select"
//                    on.change (fun el _ -> targetLens.Set << Some <| getSelected el)
//                    on.afterRender (fun el ->
//                        try     
//                            // if an item is loaded and there exists a list, but the value selected is not the same as the target, then select something in the list and update the item accordingly   
//                            if selectedIndex el > -1 then
//                                match (datatype.Value, getSelected el, List.tryHead mapList) with
//                                | Some _, selectedValue, Some (key, _) when selectedValue <> current -> targetLens.Set <| Some key
//                                | _ -> ()
//                        with
//                            | exn -> Console.Log exn
//                        )
//                    ] @ attrs )
//                (
//                    mapList
//                    |> List.map ( fun (key, value) ->
//                        Doc.Element
//                            "option"
//                            [
//                                attr.``class`` "fruit-form-select-option"
//                                attr.value key
//                                (if current = key
//                                then
//                                    attr.selected "selected"
//                                else
//                                    Attr.Empty)
//                            ] [text value] :> Doc
//
//                    )) :> Doc
//        ) detectChangeInOptionsAndItem

