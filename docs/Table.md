# WebSharper.Fruitlets.Table

## Structure

A table is defined as follows:

```fsharp
type Table<'Key, 'DataType> when 'Key : equality  =
    {
        /// the id of the table    
        Id': string
        /// The datasource for the table rows
        DataSource: DataSource.DS<'Key,'DataType>
        /// The Column type generates a column from type 'DataType
        Columns: Column<'DataType> []
        /// CustomSettings
        Settings: TableSettings<'DataType>
    }
```

## Columns

A column defines a map from the 'DataType to an array of Doc elements + a set of options defining visibility in the table, editability and sorting 

## Settings

Settings are all customizable features that have to do with presentation of the table and the accompanying editform

## DataSource

The datasource for the table can be either located locally (with Synchronous CRUD functions) or by communicating through Rpc. If a read function is defined, the result of this function is loaded into the ListModel.

In a future version a third option will be added, where CRUD will happen through an API. This will crave some additional setting such as authentication.

## Examples



