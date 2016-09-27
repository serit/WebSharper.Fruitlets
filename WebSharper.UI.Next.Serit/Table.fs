namespace WebSharper.UI.Next.Serit

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

// Bootstrap table
[<JavaScript>]
module Table =

    type SortDirection =
        | Asc of int
        | Desc of int
        member this.SortFunc index =
            match this with
            | Asc i when i = index -> Seq.sortByDescending
            | _ -> Seq.sortBy
        member this.Flip index =
            match this with
            | Asc i when i = index -> Desc index
            | _ -> Asc index
        member this.FAClass index =
            "fa fa-fw fa-sort" +
            match this with
            | Asc i when i = index -> "-asc"
            | Desc i when i = index -> "-desc"
            | _ -> ""


    [<StructuralComparison;StructuralEquality>]
    type SortableType =
        | I of int
        | S of string
        | F of float
        | D of System.DateTime
        static member DefaultShow =
            fun (st : SortableType) ->
                match st with
                | I i -> sprintf "%i" i
                | S s -> s
                | F f -> sprintf "%f" f
                | D d -> d.ToShortDateString()
        //| T of obj //* ('T -> ColumnType<'T>)

    type SortShow =
        (SortableType -> string)

    type TableClass =
        | Striped
        | Bordered
        | Custom of string
        member this.show =
            match this with
            | Striped -> "table-striped"
            | Bordered -> "table-bordered"
            | Custom cl -> cl

    type InputType<'T> =
        | StringInput of (('T -> string) * ('T -> string -> 'T))
        | IntInput of (('T -> int) * ('T -> int -> 'T))
        | FloatInput of (('T -> float) * ('T -> float -> 'T))
        member this.changeAttrs (tbl : Table<_,'T>) updateFunc (t : 'T) =
            let change t =
                match tbl.RowData.UpdateFunc with
                | Some upd -> upd ( updateFunc t)
                | None -> ()
            [
                on.change (fun _ _ -> change t)
                //on.keyUp (fun _ _ -> change t)
                //on.paste (fun _ _ -> change t)
            ]
        member this.formWrapper label content =
            divAttr[ attr.``class`` "form-group"][
                labelAttr[attr.``for`` label][text label]
                content
            ]
        member this.show (tbl : Table<_,'T>) label =
            match this with
            | StringInput (f, f') ->
                (fun (t' : Var<'T option>) ->

                        let s = Var.Lens t' (fun t -> match t with |Some t' -> f t' | None -> "") (fun t s -> Some <| f' t.Value s)
                        Doc.Input(
                                [
                                    attr.id label
                                    attr.``class`` "form-control"

                                ]) s
                        |> this.formWrapper label
                )
            | IntInput (f, f')->
                (fun t' ->
                    let s = Var.Lens t' (fun t -> match t with |Some t' -> f t' | None -> 0) (fun t s -> Some <| f' t.Value s)
                    Doc.IntInputUnchecked(
                            [
                                attr.id label
                                attr.``class`` "form-control"

                            ]) s
                    |> this.formWrapper label
                )
            | FloatInput (f, f')->
                (fun t' ->
                    let s = Var.Lens t' (fun t -> match t with |Some t' -> f t' | None -> 0.) (fun t s -> Some <| f' t.Value s)
                    Doc.FloatInputUnchecked( //onchange @
                            [
                                attr.id label
                                attr.``class`` "form-control"

                            ]) s
                    |> this.formWrapper label
                    )

    and Column<'T> =
        {
            Name: string
            AttrList: ('T -> list<Attr>) option
            DocList: ('T -> list<Doc>)
            SortFunction: ('T -> SortableType) option
            EditField: ('T -> InputType<'T>) option
        }
        member this.showHeader index table =
            match this.SortFunction with
            | Some sortFunc ->
                tdAttr([
                        on.click ( fun _ _ ->
                            table.Direction <- table.Direction.Flip index

                            (sortFunc, table.RowData.Model.Value)
                            ||> table.Direction.SortFunc index
                            |> table.RowData.Model.Set
                        )
                ] )[
                    text this.Name
                    iAttr[
                        attr.style "color: #aaa;"
                        attr.``class`` <| table.Direction.FAClass index
                    ][] :> Doc
                ] :> Doc
            | None ->
                td [
                    text this.Name
                ] :> Doc

        member this.showRow item =
            match this.AttrList with
            | Some attrList ->
                tdAttr ( attrList item ) (this.DocList item)
            | None ->
                td (this.DocList item)
        static member empty =
            {Name = ""; AttrList = None; DocList = (fun t -> List.empty); SortFunction = None; EditField = None}
        static member SimpleColumn (name, fieldValueFunction : ('T -> obj)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << string <| fieldValueFunction t ])}
        static member SimpleColumn (name, fieldValueFunction : ('T -> int)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << string <| fieldValueFunction t ])}
        static member SimpleColumn (name, fieldValueFunction : ('T -> string)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text <| fieldValueFunction t ])}
        static member SimpleColumn (name, fieldValueFunction : ('T -> float)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << string <| fieldValueFunction t ])}
        static member SimpleColumn (name, fieldValueFunction : ('T -> decimal)) =
            { Column<'T>.empty with Name = name; DocList = (fun t -> [text << string <| fieldValueFunction t ])}
        static member SimpleSortColumn (name, fieldValueFunction : ('T -> int)) =
            { Column<'T>.SimpleColumn(name, fieldValueFunction) with SortFunction = Some (fun t -> I <| fieldValueFunction t )}
        static member SimpleSortColumn (name, fieldValueFunction : ('T -> string)) =
            { Column<'T>.SimpleColumn(name, fieldValueFunction) with SortFunction = Some (fun t -> S <| fieldValueFunction t )}
        static member SimpleSortColumn (name, fieldValueFunction : ('T -> float)) =
            { Column<'T>.SimpleColumn(name, fieldValueFunction) with SortFunction = Some (fun t -> F <| fieldValueFunction t )}
        static member SimpleSortColumn (name, fieldValueFunction : ('T -> decimal)) =
            { Column<'T>.SimpleColumn(name, fieldValueFunction) with SortFunction = Some (fun t -> F << float <| fieldValueFunction t )}

    and Table<'U, 'T> when 'U : equality  =
        {
            Id: string
            Class: TableClass []
            //RowIdFunc: ('T -> 'U)
            RowData: DataSource.DataSource<'U,'T> //ListModel<'U,'T>
            Columns: Column<'T> []
            mutable Direction: SortDirection
        }
        member this.isEditable =
            this.Columns
            |> Array.exists (fun c -> c.EditField.IsSome)
        member this.EditWindow (item : Var<'T option>) windowId =
            let editForm () =
//                //View.
//                match item.Value with
//                | Some v ->
                match Seq.tryHead this.RowData.Model.Value with
                | Some v ->
                    let editcolumns =
                        this.Columns
                        |> Array.filter(fun c -> c.EditField.IsSome)
                    // should be a Var of t which updates the entire form and is updated by the form
                    editcolumns
                    |> Array.map (fun c ->
                        (c.EditField.Value v).show this c.Name item :> Doc
                    )
                    |> Array.toList
                    |> form

                | None -> p [text "No item chosen"]
            let footer t =
                match t with
                | Some v ->
                    div[
                        buttonAttr[attr.``class`` "btn btn-secondary"; attr.``data-`` "dismiss" "modal"][text "Close"] :> Doc
                        (match this.RowData.UpdateFunc with
                        | Some f ->
                            buttonAttr[
                                attr.``class`` "btn btn-primary"
                                attr.``data-`` "dismiss" "modal"
                                on.click (fun el ev ->
                                    Console.Log <| sprintf "Save: %A" v
                                    this.RowData.Model.UpdateBy (fun _ -> Some v ) <| this.RowData.IdFunc v
                                    f v)][text "Save"] :> Doc
                        | None -> Doc.Empty)
                        (match this.RowData.DeleteFunc with
                        | Some f ->
                            buttonAttr[
                                attr.``class`` "btn btn-danger"
                                attr.``data-`` "dismiss" "modal"
                                on.click (fun el ev ->
                                    Console.Log <| sprintf "Remove: %A" v
                                    this.RowData.Model.Remove v
                                    f v)][text "Delete"] :> Doc
                        | None -> Doc.Empty)
                    ] :> Doc
                | None -> Doc.Empty

            Modal.Window.Create
                windowId
                (h2 [textView <| View.Map (fun t ->  (match t with | Some t' ->  sprintf "Item %A" (this.RowData.IdFunc t') | None -> "")) item.View ] )
                (editForm ())//(p [textView <| View.Map (fun t -> sprintf "%A" t) item.View])
                (Doc.BindView (fun t -> footer t) item.View)
                Modal.WindowSize.Normal

        member this.ShowHeader () =
            let extracol = if this.isEditable then [|td[] :> Doc|] else [||]
            this.Columns
            |> Array.indexed
            |> Array.map (fun (index, column) ->
                column.showHeader index this
            )
            |> Array.append extracol
            |> fun headerRow -> thead[tr headerRow] :> Doc
        member private this.ShowTableFilter (filter: 'T -> bool ) =
            let currentItem = Var.Create None
            let idCode = sprintf "%s-edit-%i" this.Id ( int <| (Math.Random() * 100000.) + 1.)
            let editColumn t =
                if this.isEditable
                then [ td[
                        Modal.Button idCode [on.click( fun el ev -> currentItem.Value <- Some t)] [
                                iAttr[ attr.``class`` "fa fa-edit"][]]] :> Doc]
                else List.empty
                |> List.toArray
            let rows view =
                view
                |> Seq.filter filter
                |> Seq.map ( fun t ->
                    let row =
                        this.Columns
                        |> Array.map (fun column ->
                            column.showRow t :> Doc
                        )
                        |> Array.append (editColumn t )
                    tr row :> Doc
                )
                |> Seq.toList
            let classes =
                if Array.isEmpty this.Class
                then
                    "table"
                else
                    this.Class
                    |> Array.map (fun cl -> cl.show)
                    |> Array.append [|"table"|]
                    |> String.concat " "
            div[
                (if this.isEditable
                then (this.EditWindow currentItem idCode).Show()
                else Doc.Empty)
                tableAttr [attr.``class`` classes ]
                    (this.ShowHeader() :: [tbody <| rows this.RowData.Model.Value])
            ]
        member this.ShowTable () =
            Doc.BindView ( fun rowdata ->
                this.ShowTableFilter (fun _ -> true)
            ) this.RowData.Model.View

        member this.ShowTableWithPages pageSize =
            let currentPage = Var.Create 0
            Doc.BindView ( fun rowdata ->
                let pages =
                    {0 ..  ((Seq.length rowdata - 1)/ pageSize)}
                    |> Seq.map (fun p ->
                        let dataList = rowdata |> Seq.indexed |> Seq.filter (fun (i,t) -> i / pageSize = p) |> Seq.map (fun (_,t) -> this.RowData.IdFunc t)
                        p, this.ShowTableFilter (fun t -> Seq.exists (fun d -> d = this.RowData.IdFunc t) dataList) :> Doc

                    )
                Pagination.show pages
            ) this.RowData.Model.View
        static member empty =
            {
                Id = ""
                Class = Array.empty
                Direction = Asc -1
                RowData = DataSource.DataSource<'U,'T>.Create (id, ListModel.FromSeq [])
                Columns = [||]
            }
        static member Create (Id, idFunc, columns, data) =
           {
                Id = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                Direction = Asc -1
                RowData = DataSource.DataSource<'U,'T>.Create (id, ListModel.Create idFunc data)
            }
        static member Create (Id, idFunc, columns, data, createFunc, updateFunc, deleteFunc) =
           {
                Id = Id
                Class = [| Striped; Bordered |]
                Columns = columns
                Direction = Asc -1
                RowData = DataSource.DataSource<'U,'T>.Create (id, ListModel.Create idFunc data, createFunc, updateFunc, deleteFunc)
            }





