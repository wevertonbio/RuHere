# Store SpeciesLink credential

This function stores the IUCN credential (API key) in the R environment.
This API key is required to obtain distributional data from IUCN.

## Usage

``` r
set_iucn_credentials(iucn_key, overwrite = FALSE,
                            open_Renviron = FALSE)
```

## Arguments

- iucn_key:

  (character) your IUCN API key. See Details.

- overwrite:

  (logical) whether to overwrite IUCN credential if it already exists.
  Default is FALSE.

- open_Renviron:

  (logical) whether to open the .Renviron file after saving the
  credentials. Default is FALSE.

## Value

If `open_Renviron` is set to TRUE, it opens the .Renviron file.
Otherwise, the credentials are saved silently.

## Details

To check your API key, visit: <https://api.iucnredlist.org/users/edit>.

## Examples

``` r
if (FALSE) { # \dontrun{
set_iucn_credentials(iucn_key = "my_key")
} # }
```
