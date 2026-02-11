# Store SpeciesLink credential

This function sets the SpeciesLink API key as an environment variable
for the current R session. This API key is required to retrieve
occurrence records from SpeciesLink.

## Usage

``` r
set_specieslink_credentials(specieslink_key, verbose = TRUE)
```

## Arguments

- specieslink_key:

  (character) your SpeciesLink API key.

- verbose:

  (logical) if `TRUE`, prints messages about the progress and the number
  of species being checked. Default is `TRUE`.

## Value

No return value.

## Details

To check your API key, visit:
<https://specieslink.net/aut/profile/apikeys>.

## Examples

``` r
if (FALSE) { # \dontrun{
set_specieslink_credentials(specieslink_key = "my_key")
} # }
```
