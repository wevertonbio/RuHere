# Store GBIF credentials

This function stores the GBIF credentials (username, email and password)
in the R environment. These credentials are required to retrieve
occurrence records from GBIF.

## Usage

``` r
set_gbif_credentials(gbif_username, gbif_email, gbif_password,
                           overwrite = FALSE, open_Renviron = FALSE,
                           verbose = TRUE)
```

## Arguments

- gbif_username:

  (character) your GBIF username.

- gbif_email:

  (character) your GBIF email address.

- gbif_password:

  (character) your GBIF password.

- overwrite:

  (logical) whether to overwrite GBIF credentials if they already exist.
  Default is FALSE.

- open_Renviron:

  (logical) whether to open the .Renviron file after saving the
  credentials. Default is FALSE.

- verbose:

  (logical) if `TRUE`, prints messages about the progress and the number
  of species being checked. Default is `TRUE`.

## Value

If `open_Renviron` is set to TRUE, it opens the .Renviron file.
Otherwise, the credentials are saved silently.

## Examples

``` r
if (FALSE) { # \dontrun{
set_gbif_credentials(gbif_username = "my_username",
                     gbif_email = "my_email@example.com",
                     gbif_password = "my_password")
} # }
```
