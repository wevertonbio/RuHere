# Flag name dictionary

A named character vector used to convert internal flag column names
(produced by the package's flagging functions) into human-readable
labels.

## Usage

``` r
flag_names
```

## Format

A named character vector of length 25. The names correspond to the
original flag codes (e.g., `"correct_country"`, `"duplicated_flag"`,
`".cen"`, `"consensus_flag"`), and the values are the cleaned,
human-readable labels (e.g., `"Wrong country"`, `"Duplicated"`,
`"Country/Province centroid"`, `"consensus"`).

## Details

This object is used internally by functions such as `mapview_here()` and
[`remove_flagged()`](https://wevertonbio.github.io/RuHere/reference/remove_flagged.md)to
display more intuitive flag names to users.
