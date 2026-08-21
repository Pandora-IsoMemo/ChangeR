# UI function of mcpData module

UI function of mcpData module

Server function of mcpData module

## Usage

``` r
mcpDataUI(id)

mcpDataServer(id, file_data, mcp_columns = c(x = "", y = ""))
```

## Arguments

- id:

  module id

- file_data:

  reactive file data

- mcp_columns:

  (character) a character vector containing the default x and y columns
  for MCP modelling. The first entry corresponds to the x column name,
  and the second entry corresponds to the y column name.
