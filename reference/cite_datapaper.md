# Cite data papers retrieved from get_datapaper

Identifies the data sources present in the output of
[`get_datapaper()`](https://wevertonbio.github.io/RuHere/reference/get_datapaper.md)
and prints the appropriate bibliographic citations for use in academic
and technical publications.

## Usage

``` r
cite_datapaper(data)
```

## Arguments

- data:

  A `data.frame` or `data.table` obtained from
  [`get_datapaper()`](https://wevertonbio.github.io/RuHere/reference/get_datapaper.md),
  containing the `data_source` column.

## Value

Prints the formatted references to the console and invisibly returns a
named character vector with the citations for the detected data sources.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Download data
dados <- get_datapaper(datapaper = "dryflor")

# 2. View citations
cite_datapaper(dados)
} # }
```
