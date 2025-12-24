# Fake occurrence data for testing coordinate validation functions

`fake_data` is a synthetic dataset created for testing functions that
validate and correct country- or state-level geographic coordinates.

Controlled coordinate errors were introduced (e.g., inverted signs,
swapped values, combinations of swaps and inversions) to simulate common
georeferencing mistakes.

This dataset is intended for automated testing of functions such as
[`check_countries()`](https://wevertonbio.github.io/RuHere/reference/check_countries.md)
and
[`check_states()`](https://wevertonbio.github.io/RuHere/reference/check_states.md).

## Usage

``` r
fake_data
```

## Format

A data frame with the same structure as `all_occ`, containing occurrence
records with intentionally manipulated coordinates. An additional column
`data_source = "fake_data"` identifies these records.

## Details

The coordinate errors include:

- **Inverted longitude**: multiplying longitude by −1.

- **Inverted latitude**: multiplying latitude by −1.

- **Both coordinates inverted**.

- **Swapped coordinates**: (`lon`, `lat`) → (`lat`, `lon`).

- **Swapped + inverted** in four combinations:

  - swapped only,

  - swapped + inverted longitude,

  - swapped + inverted latitude,

  - swapped + both inverted.

## Examples

``` r
data(fake_data)
```
