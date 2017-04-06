namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

[<JavaScript>]
module Pagination =

    type PagerPosition =
        | Up
        | Down

    let show (pageContent : seq<int*Doc>) position =
        let currentPage = Var.Create 0
        let PreviousPage () =
            liAttr[attr.``class`` "fruit-page-item"][
                aAttr[
                    attr.``class`` "fruit-page-link fruit-page-previous"
                    attr.href "#"
                    on.click (fun _ _ ->
                        if currentPage.Value > 0
                        then currentPage.Value <- currentPage.Value - 1
                    )
                ][ text "\u00ab"
                ]
            ] :> Doc

        let NextPage () =
            liAttr[attr.``class`` "fruit-page-item"][
                aAttr[
                    on.click (fun _ _ ->
                        if currentPage.Value < (Seq.length pageContent) - 1
                        then currentPage.Value <- currentPage.Value + 1
                    )
                    attr.``class`` "fruit-page-link fruit-page-next"
                    attr.href "#"
                    ][ text "\u00bb"
                    ]
            ] :> Doc

        let pageLink index =
            liAttr[
                attr.classDyn <|
                    View.Map ( fun page ->
                        if page = index
                        then "fruit-page-item active"
                        else "fruit-page-item"
                    ) currentPage.View
                ][
                aAttr[
                    attr.``class`` "fruit-page-link fruit-bg-default"
                    attr.href "#"
                    on.click (fun _ _ ->
                        currentPage.Value <- index
                    )
                ][text <| string (index + 1)]

            ] :> Doc

        let pagination attrs =
            let positionClass = 
                "pagination fruit-pagination fruit-pagination-position-" +
                match position with
                | Up -> "up"
                | Down -> "down"
            ulAttr([attr.``class`` positionClass] @ attrs)
                ( [PreviousPage ()]
                    @ (
                        pageContent
                        |> Seq.map(fun (index, _) -> pageLink index)
                        |> Seq.toList
                    )
                    @ [NextPage()]
                ) :> Doc

        let content =
            [
                for (index, page) in pageContent do
                    yield
                        divAttr[
                            attr.classDyn <|
                                View.Map ( fun page ->
                                    if page = index
                                    then "fruit-page"
                                    else "fruit-page hidden"
                                ) currentPage.View
                        ][
                            page
                        ] :> Doc
            ]

        div (
                match position with
                | Up -> [pagination []] @ content
                | Down -> content @ [pagination []]
            )
