% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-changePointsModule.R
\name{changePointsUI}
\alias{changePointsUI}
\alias{changePointsServer}
\title{Change Points UI}
\usage{
changePointsUI(id)

changePointsServer(
  id,
  file_data,
  uploaded_matrices = reactiveValues(),
  mcp_columns = c(x = "", y = "")
)
}
\arguments{
\item{id}{The module id}

\item{file_data}{The reactive input data}

\item{uploaded_matrices}{(reactive) values of uploaded segments and priors matrices}

\item{mcp_columns}{(character) a character vector containing the default x and y columns for MCP modelling.
The first entry corresponds to the x column name, and the second entry corresponds to the y column name.}
}
\description{
UI of the module

Server function of the module
}
