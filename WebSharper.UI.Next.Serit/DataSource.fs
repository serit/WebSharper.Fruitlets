namespace WebSharper.UI.Next.Serit

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

// Bootstrap table
[<JavaScript>]
module DataSource =
    type DataSource<'U,'T> when 'U : equality =
        {

            IdFunc: ('T -> 'U)
            Model: ListModel<'U,'T>
            CreateFunc: ('T -> 'U) option
            UpdateFunc: ('T -> unit) option
            DeleteFunc: ('T -> unit) option

        }
        static member Create (idf, model,?cf,?uf,?df) =
            {
                IdFunc = idf
                Model = model
                CreateFunc = cf
                UpdateFunc = uf
                DeleteFunc = df
            }


