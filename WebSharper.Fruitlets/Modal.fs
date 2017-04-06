namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

[<JavaScript>]
module Modal =


    let private divClass ``class`` content =
        divAttr [attr.``class`` ``class``] content

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

            divAttr[
                attr.``class`` "modal fade fruit-modal"
                attr.id this.Id
                attr.tabindex "-1"
                Attr.Create "role" "dialog"
                attr.style "overflow:scroll"
                ][
                divAttr (
                    [
                     Attr.Create "role" "document"
                    ] @ dialogSizeAttrs )[
                        divClass "modal-content fruit-modal-content"
                            [
                                divClass "modal-header fruit-modal-header" [
                                    buttonAttr[
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
        buttonAttr (
            [
                attr.``type`` "button"
                attr.``class`` "btn btn-primary fruit-btn"
                attr.``data-`` "toggle" "modal"
                attr.``data-`` "target" <| "#" + Id
            ] @ attrList )[
                text title
            ]

    let Button Id attrList docList =
        buttonAttr (
            [
                attr.``type`` "button"
                attr.``class`` "btn btn-primary fruit-btn"
                attr.``data-`` "toggle" "modal"
                attr.``data-`` "target" <| "#" + Id
            ] @ attrList ) docList

    let Header title =
        divClass "modal-header" [
            buttonAttr[
                attr.``type`` "button"
                attr.``class`` "close fruit-modal-close"
                attr.``data-`` "dismiss" "modal"
            ][text "x"]
            h4[text title]
        ]

