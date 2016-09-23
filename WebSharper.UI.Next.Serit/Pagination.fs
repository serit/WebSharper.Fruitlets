namespace WebSharper.UI.Next.Serit

open WebSharper
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

[<JavaScript>]
module Pagination =

    let show (pageContent : seq<int*Doc>) =
        let currentPage = Var.Create 0
        let PreviousPage () =
            liAttr[attr.``class`` "page-item"][
                aAttr[
                    attr.``class`` "page-link"
                    attr.href "#"
                    on.click (fun _ _ ->
                        if currentPage.Value > 0
                        then currentPage.Value <- currentPage.Value - 1
                    )
                ][ text "\u00ab"
                ]
            ] :> Doc

        let NextPage () =
            liAttr[attr.``class`` "page-item"][
                aAttr[
                    on.click (fun _ _ ->
                        if currentPage.Value < (Seq.length pageContent) - 1
                        then currentPage.Value <- currentPage.Value + 1
                    )
                    attr.``class`` "page-link"
                    attr.href "#"
                    ][ text "\u00bb"
                    ]
            ] :> Doc

        let pageLink index =
            liAttr[
                attr.classDyn <|
                    View.Map ( fun page ->
                        if page = index
                        then "page-item active"
                        else "page-item"
                    ) currentPage.View
                ][
                aAttr[
                    attr.``class`` "page-link"
                    attr.href "#"
                    on.click (fun _ _ ->
                        currentPage.Value <- index
                    )
                ][text <| string (index + 1)]

            ] :> Doc

        let pagination () =
            ulAttr[attr.``class`` "pagination"]
                ( [PreviousPage ()]
                    @ (
                        pageContent
                        |> Seq.map(fun (index, _) -> pageLink index)
                        |> Seq.toList
                    )
                    @ [NextPage()]
                ) :> Doc

        div (
                [pagination ()]
                @ [
                        for (index, page) in pageContent do
                            yield
                                divAttr[
                                    attr.classDyn <|
                                        View.Map ( fun page ->
                                            if page = index
                                            then ""
                                            else "hidden"
                                        ) currentPage.View
                                ][
                                    page
                                ] :> Doc
                    ]
            )
