namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Html

[<JavaScript>]
module Pagination =

    type PagerPosition =
        | Up
        | Down

    let show (pageContent : seq<int*Doc>) position =
        let currentPage = Var.Create 0
        let PreviousPage () =
            li[attr.``class`` "fruit-page-item"][
                a[
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
            li[attr.``class`` "fruit-page-item"][
                a[
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
            li[
                attr.classDyn <|
                    View.Map ( fun page ->
                        if page = index
                        then "fruit-page-item active"
                        else "fruit-page-item"
                    ) currentPage.View
                ][
                a[
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
            ul([attr.``class`` positionClass] @ attrs)
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
                        div[
                            attr.classDyn <|
                                View.Map ( fun pageIndex ->
                                    if pageIndex = index
                                    then "fruit-page"
                                    else "fruit-page hidden"
                                ) currentPage.View
                        ][
                            Doc.BindView ( fun pageIndex ->
                                if pageIndex = index
                                then page
                                else Doc.Empty
                            ) currentPage.View
                        ] :> Doc
            ]

        div [] (
                match position with
                | Up -> [pagination []] @ content
                | Down -> content @ [pagination []]
            )
