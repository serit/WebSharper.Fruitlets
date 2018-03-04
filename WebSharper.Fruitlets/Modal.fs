namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Html

[<JavaScript>]
module Modal =


    [<Inline "$($id).modal('hide')">]
    let CloseModal id = X<unit>


    let private divClass ``class`` content =
        div [attr.``class`` ``class``] content

    type WindowSize =
        | Small
        | Normal
        | Large
        | Custom of int * int


    and Window =
        {
            Id: string
            Header: Doc
            Body: Doc
            Footer: Doc
            Size: WindowSize
        }
        member this.Show () =

            let dialogSizeAttrs =
                match this.Size with
                | Large -> [attr.``class`` "modal-dialog modal-lg fruit-modal-dialog fruit-modal-lg"]
                | _ -> [attr.``class`` "modal-dialog fruit-modal-dialog"]

            div[
                attr.``class`` "modal fade fruit-modal"
                attr.id this.Id
                attr.tabindex "-1"
                Attr.Create "role" "dialog"
                attr.style "overflow:scroll"
                ][
                div (
                    [
                     Attr.Create "role" "document"
                    ] @ dialogSizeAttrs )[
                        divClass "modal-content fruit-modal-content"
                            [
                                divClass "modal-header fruit-modal-header" [
                                    button[
                                        attr.``class`` "close fruit-modal-close"
                                        attr.``data-`` "dismiss" "modal"
                                        ][text "\u00D7"]
                                    this.Header
                                ]
                                divClass "modal-body fruit-modal-body" [this.Body]
                                divClass "modal-footer fruit-modal-footer" [this.Footer]
                            ]
                    ]
            ] :> Doc
        static member Create Id header body footer size =
            {
                Id = Id
                Header = header
                Body = body
                Footer = footer
                Size = size
            }
        static member empty =
            {
                Id = "modal"
                Header = Doc.Empty
                Body = Doc.Empty
                Footer = Doc.Empty
                Size = WindowSize.Normal
            }


    let ButtonSimple Id attrList title =
        button (
            [
                attr.``type`` "button"
                attr.``class`` "btn btn-primary fruit-btn"
                attr.``data-`` "toggle" "modal"
                attr.``data-`` "target" <| "#" + Id
            ] @ attrList )[
                text title
            ]

    let Button Id attrList docList =
        button (
            [
                attr.``type`` "button"
                attr.``class`` "btn btn-primary fruit-btn"
                attr.``data-`` "toggle" "modal"
                attr.``data-`` "target" <| "#" + Id
            ] @ attrList ) docList

    let Header title =
        divClass "modal-header" [
            button[
                attr.``type`` "button"
                attr.``class`` "close fruit-modal-close"
                attr.``data-`` "dismiss" "modal"
            ][text "x"]
            h4 [] [text title]
        ]

