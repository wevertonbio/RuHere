# Administrative Units (States, Provinces, and Regions)

A simplified `PackedSpatVector` containing state-level polygons (e.g.,
provinces, departments, regions) for countries worldwide. Names and
parent countries (`geonunit`) were cleaned (lowercase, accents removed).

## Usage

``` r
states
```

## Format

A `PackedSpatVector` object with polygons of administrative divisions
and one attribute:

- name:

  State/province/region name.

## Source

Natural Earth data, via **rnaturalearth**.

## Details

The dataset was generated from
[`rnaturalearth::ne_states()`](https://docs.ropensci.org/rnaturalearth/reference/ne_states.html).
The following processing steps were applied:

- kept only administrative types: `"Province"`, `"State"`,
  `"Department"`, `"Region"`, `"Federal District"`;

- selected only `"name"` and `"geonunit"` columns;

- both fields were cleaned via
  [`tolower()`](https://rdrr.io/r/base/chartr.html) and
  [`remove_accent()`](https://wevertonbio.github.io/RuHere/reference/remove_accent.md);

- records where state name = country name were removed;

- geometries were simplified using
  `terra::simplifyGeom(tolerance = 0.05)`;

- wrapped with
  [`terra::wrap()`](https://rspatial.github.io/terra/reference/wrap.html)
  for internal storage.

## Examples

``` r
data(states)
states <- terra::unwrap(states)
terra::plot(states)
```
