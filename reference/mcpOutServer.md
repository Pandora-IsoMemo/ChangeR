# MCP Output UI

MCP Output UI

MCP Output Server

## Usage

``` r
mcpOutUI(id, title, outFUN = verbatimTextOutput, showWidth = FALSE)

mcpOutServer(
  id,
  formulasAndPriors,
  mcpData,
  mcpFitList,
  mcpModelName,
  mcpOutFUN,
  renderFUN = renderPrint
)
```

## Arguments

- id:

  The module id

- title:

  The title of the output

- outFUN:

  The output function, either verbatimTextOutput or plotOutput

- showWidth:

  Show the width slider

- formulasAndPriors:

  The reactive formulas and priors

- mcpData:

  The reactive mcp data

- mcpFitList:

  The reactive mcp fit list

- mcpModelName:

  The reactive mcp model name

- mcpOutFUN:

  The output function, either "summary", "waic" or "plot"

- renderFUN:

  The render function, either renderPrint or renderPlot
