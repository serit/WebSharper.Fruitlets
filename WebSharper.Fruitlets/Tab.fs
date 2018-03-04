namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Html

[<JavaScript>]
module Tab =
    type Tab =
        {
            Id: string
            Title: string
            AttrList: Attr list
            Content: Doc
            Active: bool
        }
        member this.ShowLink () =
            let attrs =
                if this.Active
                then (attr.``class`` "active") :: this.AttrList
                else this.AttrList

            li attrs [
                a[
                    attr.``data-`` "toggle" "tab"
                    attr.href <| sprintf "#%s" this.Id
                    ][text this.Title]
            ] :> Doc
        member this.ShowDiv () =
            let cls =
                if this.Active
                then "tab-pane fade in active fruit-tab-pane"
                else "tab-pane fade fruit-tab-pane"
            div[
                attr.id this.Id
                attr.``class`` cls
                ] [
                    this.Content
                ] :> Doc

    let ShowHeader (tabList : Tab list) =
        tabList
        |> List.map (fun t -> t.ShowLink())
        |> ul [attr.``class`` "nav nav-tabs fruit-nav fruit-nav-tabs"]

    let ShowContent (tabList : Tab list) =
        tabList
        |> List.map (fun t -> t.ShowDiv())
        |> div [attr.``class`` "tab-content fruit-tab-content"]


