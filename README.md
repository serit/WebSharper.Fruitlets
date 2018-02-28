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
Current version is 0.7.2

### 0.7.2

Main changes since the last version include changes in settings of the Table type


### 0.3.0

Optional reCaptcha check before submit. Find info [here](https://www.google.com/recaptcha). When using, don't forget to load the corresponding script:

```html
<script src='https://www.google.com/recaptcha/api.js' async="true" defer="true"></script>
```

### 0.2.8

Form validation can be asynchronous and error message can be a function

### 0.2.7

* Fixed some issues with synchronization of dynamic dropdown lists

## Components
This library is built with an emphasis on data management and is focused on generating layouts that integrate CRUD operations on existing datastructures.

## [Table](docs/Table.md)

The Table module is currently the most important module, and eases representing existing data in a html-table. 
If create, update or delete functions are specified, corresponding buttons and forms are automatically added to the table.

## [Form](docs/Form.md)

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

