# Store SpeciesLink credential

This function stores the SpeciesLink credential (API key) in the R
environment. This API key is required to retrieve occurrence records
from SpeciesLink.

## Usage

``` r
set_specieslink_credentials(specieslink_key, overwrite = FALSE,
                                   open_Renviron = FALSE, verbose = TRUE)
```

## Arguments

- specieslink_key:

  (character) your SpeciesLink API key.

- overwrite:

  (logical) whether to overwrite SpeciesLink credential if it already
  exists. Default is FALSE.

- open_Renviron:

  (logical) whether to open the .Renviron file after saving the
  credentials. Default is FALSE.

- verbose:

  (logical) if `TRUE`, prints messages about the progress and the number
  of species being checked. Default is `TRUE`.

## Value

If `open_Renviron` is set to TRUE, it opens the .Renviron file.
Otherwise, the credentials are saved silently.

## Details

To check your API key, visit:
<https://specieslink.net/aut/profile/apikeys>.

## Examples

``` r
if (FALSE) { # \dontrun{
set_specieslink_credentials(specieslink_key = "my_key")
} # }
```
