# World Countries

A `"PackedSpatVector"` containing country polygons from **Natural
Earth**, processed and cleaned for use within the package. Country names
were converted to lowercase and had accents removed.

## Usage

``` r
world
```

## Format

A `PackedSpatVector` object with country polygons and one attribute:

- name:

  Country name.

## Source

Natural Earth data, via **rnaturalearthdata**.

## Details

The dataset is sourced from `rnaturalearthdata::map_units110`, then:

- converted to a `SpatVector` using **terra**,

- attribute `"name"` cleaned
  ([`tolower()`](https://rdrr.io/r/base/chartr.html),
  [`remove_accent()`](https://wevertonbio.github.io/RuHere/reference/remove_accent.md)),

- wrapped using
  [`terra::wrap()`](https://rspatial.github.io/terra/reference/wrap.html)
  for robust internal storage.

## Examples

``` r
data(world)
world <- terra::unwrap(world)
terra::plot(world)
```
