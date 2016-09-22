namespace WebSharper.UI.Next.Serit

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
                | Large -> [attr.``class`` "modal-dialog modal-lg"]
                | _ -> [attr.``class`` "modal-dialog"]

            divAttr[
                attr.``class`` "modal fade"
                attr.id this.Id
                attr.tabindex "-1"
                Attr.Create "role" "dialog"
                attr.style "overflow:scroll"
                ][
                divAttr (
                    [
                     Attr.Create "role" "document"
                    ] @ dialogSizeAttrs )[
                        divClass "modal-content fs-modal-content"
                            [
                                this.Header
                                divClass "modal-body" [this.Body]
                                this.Footer
                            ]
                    ]
            ]
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

