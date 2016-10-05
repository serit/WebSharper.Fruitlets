namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

[<JavaScript>]
module Div =

    let divClass ``class`` content =
        divAttr [
            attr.``class`` ``class``
        ] content

[<JavaScript>]
module Button =

    let buttonClass ``class`` content =
        buttonAttr [
            attr.``type`` "button"
            attr.``class`` ``class``
        ] content

    let buttonClassAttr ``class`` attrList content =
        buttonAttr (
            [
                attr.``type`` "button"
                attr.``class`` ``class``
            ] @ attrList)
            content
