namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

// Bootstrap formfields
[<JavaScript>]
module MultipleInput =
    

    let internal SomeOrDefault getter def = function
        | Some t' -> getter t'
        | None -> def
    let internal SomeSetter setter (t: 'DataType option) s =
        Some <| setter t.Value s

        
    let private wrapSeq (vls: IRef<seq<'T>>) : IRef<seq<Var<'T>>> =

        let getter (ls : seq<'T>) : seq<Var<'T>> =
            Seq.map (fun l ->
                Var.Create l
            ) ls

        let setter (ls : seq<'T>) (newls : seq<Var<'T>>) : seq<'T> =
            newls |> Seq.map (fun v -> v.Value)

        let varSeq' = Var.Lens vls getter setter

        //varSeq'.View |> View.Sequence |> View.Sink ()
        varSeq'
        
    /// <summary>    
    /// Input that takes a seq<'Type> as input
    /// Incomplete: should have +/- buttons
    /// </summary>    
    type MultipleInputType<'DataType, 'ValueType> =
        {
            Label: string
            Getter: 'DataType -> 'ValueType seq
            Setter: 'DataType -> 'ValueType seq -> 'DataType

        }
        member this.formWrapper content =
            divAttr[ attr.``class`` "form-group fruit-form-group"][
                labelAttr[attr.``for`` this.Label][text this.Label]
                content
            ] :> Doc
        member this.FieldWrapperWithAdd content =
            divAttr[ attr.``class`` "input-group fruit-form-input-group"][
                content
                spanAttr[attr.``class`` "input-group-btn fruit-form-input-group-btn"][
                    buttonAttr[attr.``class`` "btn btn-success btn-add fruit-btn fruit-btn-form-add"][
                        iAttr[attr.``class`` "fa fa-plus"][]
                    ]
                ]
            ] :> Doc
        member this.FieldWrapperWithRemove content =
            divAttr[ attr.``class`` "input-group fruit-form-input-group"][
                content
                spanAttr[attr.``class`` "input-group-btn fruit-form-input-group-btn"][
                    buttonAttr[attr.``class`` "btn btn-danger btn-remove fruit-btn-form-remove"][
                        iAttr[attr.``class`` "fa fa-minus"][]
                    ]
                ]
            ] :> Doc
        static member StringSeq label' (getter : 'DataType -> string seq) setter =
            let field = {Label = label'; Getter = getter; Setter = setter}
            // many fields with (+ -) buttons
            // also up/down
            let attrs =
                [
                    attr.id label'
                    attr.``class`` "form-control fruit-form-control"
                ]
            fun (t' : Var<'DataType option>) ->
                let sList = Var.Lens t' (SomeOrDefault field.Getter Seq.empty) (SomeSetter field.Setter)
                let wrapped = wrapSeq sList
                Doc.BindView( fun vars ->
                    vars
                    |> Seq.map (fun s -> Doc.Input attrs s :> Doc)
                    |> Doc.Concat
                ) wrapped.View
                |> field.formWrapper


