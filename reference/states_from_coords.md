# Extract state from coordinates

Extracts the state for each occurrence record based on coordinates.

## Usage

``` r
states_from_coords(
  occ,
  long = "decimalLongitude",
  lat = "decimalLatitude",
  from = "all",
  state_column = "stateProvince",
  output_column = "state_xy",
  append_source = FALSE
)
```

## Arguments

- occ:

  (data.frame) a dataset with occurrence records, preferably
  standardized using
  [`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md).

- long:

  (character) column name with longitude. Default is 'decimalLongitude'.

- lat:

  (character) column name with latitude. Default is 'decimalLatitude'.

- from:

  (character) whether to extract the state for all records ('all') or
  only for records missing state information ('na_only'). If 'na_only',
  you must provide the name of the column with state information.
  Default is 'all'.

- state_column:

  (character) the column name containing the state. Only applicable if
  `from = na_only`. Default is NULL.

- output_column:

  (character) column name created in `occ` to store the states
  extracted. Default is 'state_xy'.

- append_source:

  (logical) whether to create a new column in `occ` called
  'state_source', which indicates whether the state was derived from
  coordinates. Default is FALSE.

## Value

The original `occ` data.frame with an additional column containing the
states extracted from coordinates.

## Details

The states are extracted from coordinates using a map retrieved from
`rnaturalearthdata::states50`.

## Examples

``` r
# Import and standardize GBIF
data("occ_gbif", package = "RuHere") #Import data example
gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
gbif_states <- states_from_coords(occ = gbif_standardized)
```
