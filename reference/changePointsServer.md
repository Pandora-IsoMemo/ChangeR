# Change Points UI

UI of the module

Server function of the module

## Usage

``` r
changePointsUI(id)

changePointsServer(
  id,
  file_data,
  uploaded_matrices = reactiveValues(),
  mcp_columns = c(x = "", y = "")
)
```

## Arguments

- id:

  The module id

- file_data:

  The reactive input data

- uploaded_matrices:

  (reactive) values of uploaded segments and priors matrices

- mcp_columns:

  (character) a character vector containing the default x and y columns
  for MCP modelling. The first entry corresponds to the x column name,
  and the second entry corresponds to the y column name.
