﻿namespace WebSharper.Fruitlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

// Columns used in the Table module
[<JavaScript>]
module Column =
    open Input

    [<Inline "$obj[$field]">]
    let private JSGetIntField obj field = X<int>
    [<Inline "$obj[$field]">]
    let private JSGetFloatField obj field = X<float>
    [<Inline "$obj[$field]">]
    let private JSGetStringField obj field = X<string>
    [<Inline "$obj[$field]">]
    let private JSGetBoolField obj field = X<bool>
    [<Inline "$obj[$field]">]
    let private JSGetObjField obj field = X<obj>
    [<Inline "$obj[$field]">]
    let private JSGetTimeSpanField obj field = X<System.TimeSpan>
    [<Inline "$obj[$field]">]
    let private JSGetDateTimeField obj field = X<System.DateTime>

    [<Inline "$obj[$field]">]
    let private JSGetIntFieldOption obj field = X<int option>
    [<Inline "$obj[$field]">]
    let private JSGetFloatFieldOption obj field = X<float option>
    [<Inline "$obj[$field]">]
    let private JSGetStringFieldOption obj field = X<string option>
    [<Inline "$obj[$field]">]
    let private JSGetBoolFieldOption obj field = X<bool option>
    [<Inline "$obj[$field]">]
    let private JSGetObjFieldOption obj field = X<obj option>
    [<Inline "$obj[$field]">]
    let private JSGetTimeSpanFieldOption obj field = X<System.TimeSpan option>
    [<Inline "$obj[$field]">]
    let private JSGetDateTimeFieldOption obj field = X<System.DateTime option>

    [<Inline "$obj[$field] = $value">]
    let private JSSetField obj field value = X<Unit>

    let inline private getter name jSGet = (fun t -> jSGet t name)
    let private defaultSetter name t v = JSSetField t name v; t


//    type TableClass =
//        | Striped
//        | Bordered
//        | Custom of string
//        member this.show =
//            match this with
//            | Striped -> "table-striped"
//            | Bordered -> "table-bordered"
//            | Custom cl -> cl

    


    type FieldClass =
        | Int
        | Float
        | String
        | Text
        | Bool
        | Time
        | Date
        | Select of Map<int,string>
        | SelectDyn of Var<Map<int,string>>
        | Optional of FieldClass

    type ColumnPermission =
        {
            Table : Permission
            Form: Permission
        }
    and Permission =
        | Invisible
        | Read
        | ReadWrite

    type FormField<'DataType> =
        | InputField of InputType<'DataType>
        | DocField of (IRef<'DataType option> -> Doc)

    type Column<'DataType> =
        {
            Name: string
            Header: (unit -> Doc) option
            AttrList: ('DataType -> list<Attr>) option
            DocList: ('DataType -> list<Doc>)
            SortFunction: ('DataType -> Sort.SortableType) option
            // this should reference the var, so that any event handlers can influence the value of the item
            EditFieldAttrList: (IRef<'DataType option> -> list<Attr>) option
            EditField: FormField<'DataType> option
            Permission: ColumnPermission
        }
        member this.showHeader index (ds : DataSource.DS<_,'DataType>) =
            let headerField () =
                match this.Header with
                | Some header' -> header' ()
                | None -> text this.Name
            match this.SortFunction with
            | Some sortFunc ->
                thAttr([
                        on.click ( fun _ _ ->
                            match ds.SortFunction.Value with
                            | Some (Sort.AscByColumn index') when index' = index ->
                                ds.SortFunction.Value <-  Some <| Sort.DescByColumn index
                            | _ ->
                                ds.SortFunction.Value <-  Some <| Sort.AscByColumn index
                        )
                ] )[
                    headerField ()
                    iAttr[
                        attr.style "color: #aaa;"
                        attr.classDyn <| View.Map (function
                            | Some (Sort.AscByColumn index') -> (Sort.AscByColumn index').FAClass index
                            | Some (Sort.DescByColumn index') -> (Sort.DescByColumn index').FAClass index
                            | _ -> "fa fa-fw fa-sort") ds.SortFunction.View
                    ][] :> Doc
                ] :> Doc
            | None ->
                th [
                    headerField ()
                ] :> Doc

        member this.showRow (item: IRef<'DataType>) =
            match this.Permission.Table with
            | Invisible -> Doc.Empty
            | Read | ReadWrite -> 
                Doc.BindView(fun item'  ->
                    this.DocList item'
                    |>
                    match this.AttrList with
                    | Some attrList -> tdAttr ( attrList item' )
                    | None -> td) item.View
            /// Obs! should this trigger an update? But how? Maybe better from user code
            /// Having form fields in the table would allow updates without verification so leave it out for now
//            | ReadWrite -> 
//                let itemLens = Var.Lens item (fun i -> Some i) (fun i -> function None -> i | Some v -> v )
//                
//                match this.EditField with
//                | Some editfield -> 
//                    let cellContents = editfield.show "" (itemLens :?> Var<'DataType option>)
//                    match this.AttrList with
//                    | Some attrList -> tdAttr ( attrList item.Value ) [cellContents] :> Doc
//                    | None -> td [cellContents] :> Doc
//                | None -> Doc.Empty 
            

        static member empty =
            {Name = ""; Header = None; AttrList = None; DocList = (fun t -> List.empty); SortFunction = None; EditField = None; EditFieldAttrList = None; Permission = {Table = Read; Form = ReadWrite }}

        static member SimpleColumn (name, get : ('DataType -> obj)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('DataType -> int)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('DataType -> string)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text <| get t ])}
        static member SimpleColumn (name, get : ('DataType -> Date)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('DataType -> float)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('DataType -> decimal)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleColumn (name, get : ('DataType -> bool)) =
            { Column<'DataType>.empty with Name = name; DocList = (fun t -> [text << string <| get t ])}
        static member SimpleSortColumn (name, get : ('DataType -> int)) =
            { Column<'DataType>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.Int <| get t )}
        static member SimpleSortColumn (name, get : ('DataType -> string)) =
            { Column<'DataType>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.String <| get t )}
        static member SimpleSortColumn (name, get : ('DataType -> float)) =
            { Column<'DataType>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.Float <| get t )}
        static member SimpleSortColumn (name, get : ('DataType -> decimal)) =
            { Column<'DataType>.SimpleColumn(name, get) with SortFunction = Some (fun t -> Sort.Float << float <| get t )}
        
        /// <summary>  
        /// A string field
        /// </summary>  
        static member EditColumn (name, get : ('DataType -> string), set : ('DataType -> string -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text <| get t ])
                SortFunction = Some (fun t -> Sort.String <| get t )
                EditField = Some << InputField <| Input.String (get, set)
            }
        /// <summary>  
        /// A string option field
        /// </summary>  
        static member EditColumn (name, get : ('DataType -> string option), set : ('DataType -> string option -> 'DataType)) =
            let SomeOrDefault = function
                | Some t' -> t'
                | None -> "-"
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [ text << SomeOrDefault <| get t])
                SortFunction = Some (fun t -> Sort.String << SomeOrDefault <| get t )
                EditField = Some << InputField <| Input.StringOption ( get, set) }
        
        /// <summary>  
        /// An int field
        /// </summary>  
        static member EditColumn (name, get : ('DataType -> int), set : ('DataType -> int -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << string <| get t ])
                SortFunction = Some (fun t -> Sort.Int <| get t )
                EditField = Some << InputField <| Input.Int (get, set) }
        /// <summary>  
        /// An int option field
        /// </summary>  
        static member EditColumn (name, get : ('DataType -> int option), set : ('DataType -> int option -> 'DataType)) =
            let SomeOrDefaultString = function
                | Some t' -> string t'
                | None -> "-"
            let SomeOrDefaultSort = function
                | Some t' -> t'
                | None -> 0
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << SomeOrDefaultString <| get t ])
                SortFunction = Some (fun t -> Sort.Int << SomeOrDefaultSort <| get t )
                EditField = Some << InputField <| Input.IntOption (get, set) }

        static member EditColumn (name, get : ('DataType -> float), set : ('DataType -> float -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << string <| get t ])
                SortFunction = Some (fun t -> Sort.Float <| get t )
                EditField = Some << InputField <| Input.Float (get, set) }
        static member EditColumn (name, get : ('DataType -> float option), set : ('DataType -> float option -> 'DataType)) =
            let SomeOrDefaultString = function
                | Some t' -> string t'
                | None -> "-"
            let SomeOrDefaultSort = function
                | Some t' -> t'
                | None -> 0.
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << SomeOrDefaultString <| get t ])
                SortFunction = Some (fun t -> Sort.Float << SomeOrDefaultSort <| get t )
                EditField = Some << InputField <| Input.FloatOption (get, set) }

        static member EditColumn (name, get : ('DataType -> bool), set : ('DataType -> bool -> 'DataType)) =
            { Column<'DataType>.empty with 
                Name = name
                DocList = (fun t -> [text <| if get t then "\u00D7" else "" ])
                SortFunction = Some (fun t -> Sort.Bool <| get t )
                EditField = Some << InputField <| Input.Bool (get, set) }

        /// TimeSpan Field
        static member EditTimeSpanColumn (name, get : ('DataType -> System.TimeSpan), set : ('DataType -> System.TimeSpan -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << Time.showTime <| (get t).Ticks ])
                SortFunction = Some (fun t -> Sort.Int << int <| (get t).Ticks )
                EditField = Some << InputField <| Input.Time (get, set) }

        /// TimeSpan option Field
        static member EditTimeSpanColumn (name, get : ('DataType -> System.TimeSpan option), set : ('DataType -> System.TimeSpan option -> 'DataType)) =
            let SomeOrDefaultString (t: System.TimeSpan option) =
                match t with
                | Some t' -> Time.showTime (t'.Ticks)
                | None -> "-"
            let SomeOrDefaultSort (t: System.TimeSpan option) =
                match t with
                | Some t' -> int (t'.Ticks)
                | None -> 0
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << SomeOrDefaultString <| get t ])
                SortFunction = Some (fun t -> Sort.Int << SomeOrDefaultSort <| get t )
                EditField = Some << InputField <| Input.TimeOption (get, set)
            }
        /// <remarks>  
        /// Under construction
        /// </remarks>  
        static member EditDateColumn (name, get : ('DataType -> System.DateTime), set : ('DataType -> System.DateTime -> 'DataType)) =
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t ->
                    let dt = get t |> fun dt' -> new Date(dt'.Year,dt'.Month - 1, dt'.Day)
                    [text <| dt.ToDateString() ])
                SortFunction = Some (fun t -> Sort.DateTime <| (get t ))
                EditField = Some << InputField <| Input.Date (get, set) }
        /// <remarks>
        /// Under construction
        /// </remarks>
        static member EditDateColumn (name, get : ('DataType -> System.DateTime option), set : ('DataType -> System.DateTime option -> 'DataType)) =
            let SomeOrDefaultString (dt: System.DateTime option) =
                match dt with
                | Some dt' -> (new Date(dt'.Year,dt'.Month - 1, dt'.Day)).ToDateString()
                | None -> "-"
            let SomeOrDefaultSort = function
                | Some t' -> t'
                | None -> System.DateTime.Parse("01-01-1970")
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [text << SomeOrDefaultString <| get t ])
                SortFunction = Some (fun t -> Sort.DateTime << SomeOrDefaultSort <| get t )
                EditField = Some << InputField <| Input.DateOption (get, set) }
              
        /// <summary>  
        /// This column can be used to represent and change foreign keys
        /// </summary>
        static member EditSelectColumn (name, get : ('DataType -> int), set : ('DataType -> int -> 'DataType), optionMap : Var<Map<int,string>>) =
            let findInMap (map : Map<int,string>) key =
                    match map.TryFind key with
                    | Some v -> v
                    | None -> ""
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [textView <| View.Map (fun map -> findInMap map (get t)) optionMap.View])
                SortFunction = Some (fun t -> Sort.String <| findInMap optionMap.Value (get t) )
                EditField = Some << InputField <| Input.Select (get, set, optionMap) }
        /// <summary>
        /// This column can be used to represent and change foreign keys
        /// </summary>
        static member EditSelectColumn (name, get : ('DataType -> int), set : ('DataType -> int -> 'DataType), optionMap : Var<Map<int, unit -> Doc>>) =
            let findInMap (map : Map<int, unit -> Doc>) key =
                    match map.TryFind key with
                    | Some v -> v
                    | None -> fun () -> Doc.Empty
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [Doc.EmbedView <| View.Map (fun map -> findInMap map (get t) <| ()) optionMap.View])
                SortFunction = None
                EditField = Some << InputField <| Input.SelectDoc (get, set, optionMap) }
        /// <summary>
        /// This column can be used to represent and change foreign keys
        /// </summary>
        static member EditSelectColumn (name, get : ('DataType -> string), set : ('DataType -> string -> 'DataType), optionMap : Var<Map<string,string>>) =
            let findInMap (map : Map<string,string>) key =
                    match map.TryFind key with
                    | Some v -> v
                    | None -> ""
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [textView <| View.Map (fun map -> findInMap map (get t)) optionMap.View])
                SortFunction = Some (fun t -> Sort.String <| findInMap optionMap.Value (get t) )
                EditField = Some << InputField <| Input.SelectWithString (get, set, optionMap) }
                
        /// <summary>
        /// This column can be used to represent and change Nullable foreign keys
        /// </summary>
        static member EditSelectColumn (name, get : ('DataType -> int option), set : ('DataType -> int option -> 'DataType), optionMap : Var<Map<int,string>>) =
            let findInMap (map : Map<int,string>) (key: int option) =
                match key with
                | None -> "-"
                | Some k ->
                    match  map.TryFind k with
                    | Some v -> v
                    | None -> "-"
            { Column<'DataType>.empty with
                Name = name
                DocList = (fun t -> [textView <| View.Map (fun map -> findInMap map (get t)) optionMap.View])
                SortFunction = Some (fun t -> Sort.String <| findInMap optionMap.Value (get t) )
                EditField = Some << InputField <| Input.SelectOption (get, set, optionMap) }
                
        /// <summary>
        /// Use the parse functions with caution. They are not typesafe and meant to be used on the client side in combination with Reflection on the server side.
        /// </summary>
        static member Parse(name, _type) =
            let IntGetter = (fun (t: 'DataType) -> JSGetIntField t name)
            let IntOptionGetter = (fun (t: 'DataType) -> JSGetIntFieldOption t name)
            match _type with
            | Int -> Column<'DataType>.EditColumn(name, IntGetter, defaultSetter name)
            | String -> Column<'DataType>.EditColumn(name, getter name JSGetStringField, defaultSetter name)
            | Text ->
                let column = Column<'DataType>.EditColumn(name, getter name JSGetStringField, defaultSetter name)
                match column.EditField.Value with
                | InputField ( InputType.String obj ) -> {column with EditField = Some << InputField <| InputType.Text obj}
                | _ -> column
            | Bool -> Column.EditColumn(name, getter name JSGetBoolField, defaultSetter name)
            | Float -> Column.EditColumn(name, getter name JSGetFloatField, defaultSetter name)
            | Time -> Column.EditTimeSpanColumn(name, getter name JSGetTimeSpanField, defaultSetter name)
            | Date -> Column.EditDateColumn(name, getter name JSGetDateTimeField, defaultSetter name)
            | Select m ->
                let varmap = Var.Create m
                Column.EditSelectColumn(name, IntGetter, defaultSetter name, varmap)
            | SelectDyn m -> Column.EditSelectColumn(name, IntGetter, defaultSetter name, m)
            | Optional field ->
                match field with
                | Int -> Column.EditColumn(name, IntOptionGetter, defaultSetter name)
                | String -> Column.EditColumn(name, getter name JSGetStringFieldOption, defaultSetter  name)
                | Text ->
                    let column = Column.EditColumn(name, getter name JSGetStringFieldOption, defaultSetter name)
                    // ???
                    match column.EditField.Value with
                    | InputField ( InputType.String obj ) -> {column with EditField = Some << InputField <| InputType.Text obj}
                    | _ -> column
                | Bool -> Column.EditColumn(name, getter name JSGetBoolField, defaultSetter name)
                | Float -> Column.EditColumn(name, getter name JSGetFloatFieldOption, defaultSetter name)
                | Time -> Column.EditTimeSpanColumn(name, (fun t -> (JSGetTimeSpanFieldOption t name)),(fun t v -> JSSetField t name v; t))
                | Date -> Column.EditDateColumn(name, getter name JSGetDateTimeFieldOption, defaultSetter name)
                | Select m ->
                    let varmap = Var.Create m
                    Column.EditSelectColumn(name, IntOptionGetter, defaultSetter name, varmap)
                | SelectDyn m -> Column.EditSelectColumn(name, IntOptionGetter, defaultSetter name, m)
                | Optional field' -> Column<'DataType>.Parse (name, Optional field')
        /// <summary>
        /// Use the parse functions with caution. They are not typesafe and meant to be used on the client side in combination with Reflection on the server side.
        /// </summary>
        static member Parse(name, _type, header) =
            { Column<'DataType>.Parse(name, _type) with Header = header }
