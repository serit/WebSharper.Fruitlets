namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Html

[<JavaScript>]
module Div =

    let divClass ``class`` content =
        div [
            attr.``class`` ``class``
        ] content

[<JavaScript>]
module Button =

    let buttonClass ``class`` content =
        button [
            attr.``type`` "button"
            attr.``class`` ``class``
        ] content

    let buttonClassAttr ``class`` attrList content =
        button (
            [
                attr.``type`` "button"
                attr.``class`` ``class``
            ] @ attrList)
            content
