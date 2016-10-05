namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

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

            liAttr attrs [
                aAttr[
                    attr.``data-`` "toggle" "tab"
                    attr.href <| sprintf "#%s" this.Id
                    ][text this.Title]
            ] :> Doc
        member this.ShowDiv () =
            let cls =
                if this.Active
                then "tab-pane fade in active"
                else "tab-pane fade"
            divAttr[
                attr.id this.Id
                attr.``class`` cls
                ] [
                    //h3 [text this.Title]
                    this.Content
                ] :> Doc

    let ShowHeader (tabList : Tab list) =
        tabList
        |> List.map (fun t -> t.ShowLink())
        |> ulAttr [attr.``class`` "nav nav-tabs"]

    let ShowContent (tabList : Tab list) =
        tabList
        |> List.map (fun t -> t.ShowDiv())
        |> divAttr [attr.``class`` "tab-content"]


