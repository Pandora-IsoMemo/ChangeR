# Get the comb matrix

Get the combined matrix of segments and priors. Process the segments and
priors matrices, identify relevant rows and columns, concatenate them,
and generate possible cell combinations while ensuring data consistency.

## Usage

``` r
getComb(segments, priors)
```

## Arguments

- segments:

  A matrix containing the segmented formulas

- priors:

  A matrix containing the priors

## Value

A matrix containing the segmented formulas and priors
