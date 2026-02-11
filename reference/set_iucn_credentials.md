# Store SpeciesLink credential

This function sets the IUCN API key as an environment variable for the
current R session. This key is required to obtain distributional data
from IUCN.

## Usage

``` r
set_iucn_credentials(iucn_key, verbose = TRUE)
```

## Arguments

- iucn_key:

  (character) your IUCN API key. See Details.

- verbose:

  (logical) if `TRUE`, prints messages about the progress and the number
  of species being checked. Default is `TRUE`.

## Value

No return value.

## Details

To check your API key, visit: <https://api.iucnredlist.org/users/edit>.

## Examples

``` r
if (FALSE) { # \dontrun{
set_iucn_credentials(iucn_key = "my_key")
} # }
```
