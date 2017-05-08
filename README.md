# WebSharper.Fruitlets


Library of UI components, built on WebSharper and UI.Next.

The components depend on [BootStrap](http://getbootstrap.com/) and [Font Awesome](http://fontawesome.io/), 
so remember to include the following in your header.

```html
<link rel="stylesheet" type="text/css" href="//maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" />
<script src="//maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
<link rel="stylesheet" href="//maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css" />   
```

## Versions
Current version is 0.2.7

### 0.2.7

* Fixed some issues with synchronization of dynamic dropdown lists

## Components
This library is built with an emphasis on data management and is focused on generating layouts that integrate CRUD operations on existing datastructures.

## Table

The Table module is currently the most important module, and eases representing existing data in a html-table. 
If create, update or delete functions are specified, corresponding buttons and forms are automatically added to the table.

## Form

The form module creates a group of input fields around lenses to a Var element.
The available input fields are:
    
    - StringInput (creates a text input)
    - TextInput (creates a textarea input)
    - IntInput
    - FloatInput
    - BoolInput (creates a checkbox)
    - SelectInput (creates a select box)
    - TimeInput (creates select boxes for hour and minute)
    - DateInput: (creates a date picker)

## Modal Window

Creates a BootStrap modal window, based on Doc components for header, body and footer.

## Tab

## Pagination

## Nuget

Create a new package by updating the Package.nuspec file with version and releaseNotes and running

    nuget pack

