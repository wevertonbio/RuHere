# Store SpeciesLink credential

This function sets the SpeciesLink API key as an environment variable in
the R environment. This API key is required to retrieve occurrence
records from SpeciesLink.

## Usage

``` r
set_specieslink_credentials(
  specieslink_key,
  permanently = FALSE,
  overwrite = FALSE,
  open_Renviron = FALSE,
  verbose = TRUE
)
```

## Arguments

- specieslink_key:

  (character) your SpeciesLink API key.

- permanently:

  (logical) whether to add the SpeciesLink API key permanently to the R
  environment. Default is `FALSE`, meaning it will be added only
  temporarily for the current session.

- overwrite:

  (logical) whether to overwrite SpeciesLink credential if it already
  exists. Only applicable if `permanently` is set to `TRUE`. Default is
  `FALSE`.

- open_Renviron:

  (logical) whether to open the .Renviron file after saving the
  credential. Only applicable if `permanently` is set to `TRUE`. Default
  is `FALSE`.

- verbose:

  (logical) if `TRUE`, prints messages about the progress and the number
  of species being checked. Default is `TRUE`.

## Value

If `permanently` and `open_Renviron` are set to TRUE, it opens the
.Renviron file. Otherwise, the credentials are saved silently.

## Details

To check your API key, visit:
<https://specieslink.net/aut/profile/apikeys>.

## Examples

``` r
if (FALSE) { # \dontrun{
set_specieslink_credentials(specieslink_key = "my_key")
} # }
```
