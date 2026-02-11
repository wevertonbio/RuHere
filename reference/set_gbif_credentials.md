# Store GBIF credentials

This function sets GBIF credentials (username, email and password) as
environment variables for the current R session. These credentials are
required to retrieve occurrence records from GBIF.

## Usage

``` r
set_gbif_credentials(gbif_username, gbif_email, gbif_password,
                           verbose = TRUE)
```

## Arguments

- gbif_username:

  (character) your GBIF username.

- gbif_email:

  (character) your GBIF email address.

- gbif_password:

  (character) your GBIF password.

- verbose:

  (logical) if `TRUE`, prints messages about the progress and the number
  of species being checked. Default is `TRUE`.

## Value

No return value.

## Details

To make these credentials permanent, you can manually add them to your
`.Renviron` file.

## Examples

``` r
if (FALSE) { # \dontrun{
set_gbif_credentials(gbif_username = "my_username",
                     gbif_email = "my_email@example.com",
                     gbif_password = "my_password")
} # }
```
