# Matrix Module

Matrix Module

Matrix Server

## Usage

``` r
matrixUI(
  id,
  title = "Matrix",
  maxRows = 10,
  maxColumns = 5,
  defaultCellContent = "",
  exampleLabel = "Example Matrix"
)

matrixServer(
  id,
  exampleFunction,
  validateCellFunction = NULL,
  uploadedMatrix = reactiveVal(),
  ...
)
```

## Arguments

- id:

  namespace id

- title:

  title of the matrix UI

- maxRows:

  maximum number of rows

- maxColumns:

  maximum number of columns

- defaultCellContent:

  default cell content

- exampleLabel:

  example label

- exampleFunction:

  function to load example input

- validateCellFunction:

  function to validate the cell content

- uploadedMatrix:

  reactiveVal for uploaded matrix

- ...:

  additional arguments to example function
