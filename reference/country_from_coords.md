# Extract country from coordinates

Extracts the country for each occurrence record based on coordinates.

## Usage

``` r
country_from_coords(
  occ,
  long = "decimalLongitude",
  lat = "decimalLatitude",
  country_column = NULL,
  from = "all",
  output_column = "country_xy",
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

- country_column:

  (character) the column name containing the country. Only applicable if
  `from = na_only`. Default is NULL.

- from:

  (character) whether to extract the country for all records ('all') or
  only for records missing country information ('na_only'). If
  'na_only', you must provide the name of the column with country
  information. Default is 'all'.

- output_column:

  (character) column name created in `occ` to store the countries
  extracted. Default is 'country_xy'.

- append_source:

  (logical) whether to create a new column in `occ` called
  'country_source', which indicates whether the country was derived from
  coordinates. Default is FALSE.

## Value

The original `occ` data.frame with an additional column containing the
countries extracted from coordinates.

## Details

The countries are extracted from coordinates using a map retrieved from
`rnaturalearthdata::map_units110`.

## Examples

``` r
# Import and standardize GBIF
data("occ_gbif", package = "RuHere") #Import data example
gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
gbif_countries <- country_from_coords(occ = gbif_standardized)
```
