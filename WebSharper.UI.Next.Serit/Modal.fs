namespace WebSharper.UI.Next.Serit

open WebSharper
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

[<JavaScript>]
module Modal =

    let private divClass ``class`` content =
        divAttr [attr.``class`` ``class``] content

    let Button Id attrList title =
        buttonAttr (
            [
                attr.``type`` "button"
                attr.``class`` "btn btn-primary"
                attr.``data-`` "toggle" "modal"
                attr.``data-`` "target" <| "#" + Id
            ] @ attrList )[
                text title
            ]

    let Header title =
        divClass "modal-header" [
            buttonAttr[
                attr.``type`` "button"
                attr.``class`` "close"
                attr.``data-`` "dismiss" "modal"
            ][text "x"]
            h4[text title]
        ]

    let Window modalId size header body footer =
        divAttr[
            attr.``class`` "modal fade"
            attr.id modalId
            attr.tabindex "-1"
            Attr.Create "role" "dialog"
            attr.style "overflow:scroll"
            ][
            divAttr[
                attr.``class`` <| "modal-dialog" + size
                Attr.Create "role" "document"
                ][
                    divClass "modal-content fs-modal-content"
                        [
                            header
                            divClass "modal-body" [body]
                            footer
                        ]
                ]
        ]

