# Remove accents and special characters from strings

This function removes accents and replaces special characters from
strings, returning a plain-text version suitable for data cleaning or
standardization.

## Usage

``` r
remove_accent(s)
```

## Arguments

- s:

  (character) a character vector containing the strings to process.

## Value

A vector string without accents or special characters.

## Examples

``` r
remove_accent(c("Colômbia", "São Paulo"))
#> [1] "Colombia"  "Sao Paulo"
```
