namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

[<JavaScript>]
module Time =
    let private formWrapper label' attrs content =
        divAttr([ attr.``class`` "form-group fruit-form-group"] @ attrs)[
            labelAttr[attr.``for`` label'][text label']
            content
        ] :> Doc


    let showTime (time: int64) =
        let hour = 60L * 60L * 1000L * 10000L
        let hourFunc t = int <| (t  % (24L * hour) ) / hour

        let minute = 60L * 1000L * 10000L
        let minFunc t = int <| (t % hour) / minute

        let t = new Date(0,0,0, hourFunc time, minFunc time)
        sprintf "%02i:%02i" (t.GetHours()) (t.GetMinutes())

    let Timepicker (time: IRef<int64>) attrs wrapperAttrs label' =

        // in future this should take into account 12 or 24-hour clock (extra select field :AM/PM)
        let hour = 60L * 60L * 1000L * 10000L
        let hourFunc t = (t  % (24L * hour) ) / hour
        let hourToTime t h = (t - (hourFunc t) * hour ) + h * hour

        let minute = 60L * 1000L * 10000L
        let minFunc t = (t % hour) / minute
        let minToTime t m = (t - (minFunc t) * minute ) + m * minute

        let showT t = sprintf "%02i" t
        let hourLens = Var.Lens time (fun t -> hourFunc t |> int ) (fun t s -> hourToTime t <| int64 s)
        let minLens = Var.Lens time (fun t -> minFunc t |> int ) (fun t s -> minToTime t <| int64 s)

        let hourList = [0..23]
        let minuteList = [0..59]

        divAttr[attr.``class`` "form-inline fruit-form-inline fruit-form-timepicker"][
            Doc.Select attrs showT hourList hourLens |> formWrapper (label' + " ") wrapperAttrs 
            Doc.Select attrs showT minuteList minLens |> formWrapper ":" wrapperAttrs 
        ]  :> Doc
    let Datepicker (date: IRef<Date>) attrs wrapperAttrs label' =
        let yearLens = Var.Lens date (fun d -> d.GetFullYear()) (fun d v -> d.SetFullYear(v); d)
        let yearList = [2000..2050]
        let monthLens = Var.Lens date (fun d -> d.GetMonth()) (fun d v -> d.SetMonth(v); d)
        let monthList = [0..11]
        let monthShow = function
            | 0 -> "Jan"
            | 1 -> "Feb"
            | 2 -> "Mar"
            | 3 -> "Apr"
            | 4 -> "May"
            | 5 -> "Jun"
            | 6 -> "Jul"
            | 7 -> "Aug"
            | 8 -> "Sep"
            | 9 -> "Oct"
            | 10 -> "Nov"
            | _ -> "Dec"
        let dayLens = Var.Lens date (fun d -> d.GetDate()) (fun d v -> d.SetDate(v); d)
        let dayList = [0..31]
        divAttr[attr.``class`` "form-inline fruit-form-inline fruit-form-datepicker"][
            Doc.Select attrs string yearList yearLens |> formWrapper label' wrapperAttrs 
            Doc.Select attrs monthShow monthList monthLens |> formWrapper " - " wrapperAttrs 
            Doc.Select attrs string dayList dayLens |> formWrapper " - " wrapperAttrs 
        ]  :> Doc
    let Datepicker' (date: IRef<Date>) attrs wrapperAttrs label' =
        let yearLens = Var.Lens date (fun d -> d.GetFullYear()) (fun d v -> d.SetFullYear(v); d)
        let yearList = [2000..2050]
        let monthLens = Var.Lens date (fun d -> d.GetMonth()) (fun d v -> d.SetMonth(v); d)
        let monthList = [0..11]
        let monthShow = function
            | 0 -> "Jan"
            | 1 -> "Feb"
            | 2 -> "Mar"
            | 3 -> "Apr"
            | 4 -> "May"
            | 5 -> "Jun"
            | 6 -> "Jul"
            | 7 -> "Aug"
            | 8 -> "Sep"
            | 9 -> "Oct"
            | 10 -> "Nov"
            | _ -> "Dec"
        let dayLens = Var.Lens date (fun d -> d.GetDate()) (fun d v -> d.SetDate(v); d)
        let dayList = [0..31]

        // dropdown containing current month. on next or previous, set current month to new and generate
        let mutable selectedDate = date.Value

        // prev Month Year next
        // Su Mo ..
        // first day is x days before 1st day of month
        // last days is 7 - x days after last day of month

        Doc.BindView(fun (d : Date) ->
            divAttr
                [attr.``class`` "dropdown"]
                [
                    buttonAttr
                        [
                            attr.``class`` "btn btn-default dropdown-toggle fruit-btn fruit-dropdown-toggle"
                            attr.``data-`` "toggle" "dropdown"
                        ]
                        [
                            text <| d.ToDateString()
                            spanAttr[attr.``class`` "caret"][]
                        ]
                    divAttr
                        [
                            attr.``class`` "dropdown-menu fruit-dropdown-menu"
                        ][
                            text "test"

                            divAttr[attr.``class`` "form-inline fruit-form-inline fruit-form-datepicker"][
                                Doc.Select [on.click(fun el ev -> ev.StopPropagation())] string yearList yearLens |> formWrapper label' wrapperAttrs 
                                Doc.Select attrs monthShow monthList monthLens |> formWrapper " - " wrapperAttrs 
                                Doc.Select attrs string dayList dayLens |> formWrapper " - " wrapperAttrs 
                            ]

                        ]
                ]
        ) date.View
    let Datepicker'' (date: IRef<Date>) attrs wrapperAttrs label' =
        let toDateString (d: Date) = sprintf "%i-%02i-%02i" <| d.GetFullYear() <| (d.GetMonth() + 1) <| d.GetDate()

        // the default input value for the HTML5 datepciker is rfc3339: (https://tools.ietf.org/html/rfc3339#section-5.6)
        let fromDateString (s: string) =
            try
                let datefields = s.Split '-' |> Array.map int
                let year, month, day = datefields.[0], datefields.[1], datefields.[2]
                new Date(year, month - 1, day)
            with
                | _ ->
                    Console.Log "Invalid date format: using today's date"
                    new Date()
            //sprintf "%i-%i-%i" <| d.GetFullYear() <| d.GetMonth() <| d.GetDate()
        let dateLens =  Var.Lens date (toDateString) (fun d v -> fromDateString v)// d.SetDate(v); d)
        Doc.Input (attrs @ [attr.``type`` "date"; attr.valueDyn (dateLens.View)] ) dateLens |> formWrapper label' wrapperAttrs


