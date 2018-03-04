namespace WebSharper.Fruitlets.Examples

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client
open WebSharper.UI.Server
open WebSharper.Fruitlets
open WebSharper.Sitelets

open FSharp.Data
open Server

module Books =

    [<Literal>]
    let bookUrl = "https://www.googleapis.com/books/v1/volumes?q=harry+potter"

//    type Book = JsonProvider<bookUrl>
//
//    Book.GetSample().Items
//    |> Seq.map(fun book -> book.)


    type VolumeInfo = 
        {
            title: string
            subTitle: string
            authors: string [] 
            description: string
            averageRating: decimal option
            //PublishedDate: string
            categories: string []
        }
    type Book =
        {
            id: string
            volumeInfo: VolumeInfo
        }

    type Collection =
        {
            items: Book []
        }


    //let Books = Book.P

[<JavaScript>]
module BooksAPI =
    open WebSharper.Fruitlets.Table
    open WebSharper.Fruitlets.Column
    open WebSharper.Fruitlets.DataSource


    let BookPage () =
            
            
        let getter = (fun (book: Books.Book) -> book)
                
        let columns =
            [|  
                Column.SimpleColumn("Id", (fun (book: Books.Book) -> book.id))
                Column.SimpleColumn("Title", (fun (book: Books.Book) -> 
                    Console.Log book.volumeInfo
                    book.volumeInfo.title))
                Column.SimpleColumn("Subtitle", (fun (book: Books.Book) -> book.volumeInfo.subTitle))
                Column.SimpleColumn("Authors", (fun (book: Books.Book) -> book.volumeInfo.authors |> String.concat ", "))
                Column.SimpleColumn("Description", (fun (book: Books.Book) -> book.volumeInfo.description))
                Column.SimpleColumn("Categories", (fun (book: Books.Book) -> book.volumeInfo.categories |> String.concat ", "))
            |]

        

        let dataSource = 
            {ApiCrud<Books.Book>.empty with
                ReadFunc = Some Books.bookUrl
                DeserializeMany = fun data -> 
                    Console.Log data
                    As<Books.Collection> data |> fun d -> d.items
            }
        let bookTable' = Table.Create("bookTable", (fun (book: Books.Book) -> book.id), columns, Books.bookUrl)
        let bookTable = { bookTable' with DataSource = {bookTable'.DataSource with CrudFunctions = Api dataSource}}


        div [] [
              h2 [] [text "Books"]
              bookTable.ShowTableWithPages 5
        ]

