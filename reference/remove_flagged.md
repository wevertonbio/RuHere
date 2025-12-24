# Remove flagged records

This function removes occurrence records flagged as invalid by one or
more flagging functions. Additional manual control is available to force
keeping or removing specific records, regardless of their flag values.

## Usage

``` r
remove_flagged(
  occ,
  flags = "all",
  additional_flags = NULL,
  force_keep = NULL,
  force_remove = NULL,
  remove_NA = FALSE,
  column_id = "record_id",
  save_flagged = FALSE,
  output_dir = NULL,
  overwrite = FALSE,
  output_format = ".gz"
)
```

## Arguments

- occ:

  (data.frame or data.table) a dataset with occurrence records that has
  been processed by two or more flagging functions. See details.

- flags:

  (character) a character vector with the names of the flag columns to
  be used for filtering records. See *details* for the available
  options. Default is "all".

- additional_flags:

  (character) an optional named character vector with the names of
  additional logical columns to be used as flags. Default is `NULL`.

- force_keep:

  (character) an optional character vector with the IDs of records that
  were flagged but should still be kept. Default is `NULL`.

- force_remove:

  (character) an optional character vector with the IDs of records that
  were not flagged but should still be removed. Default is `NULL`.

- remove_NA:

  (logical) whether to remove records that have NA in the flags
  specified. Default is FALSE.

- column_id:

  (character) the name of the column containing unique record IDs.
  Required if `force_keep` or `force_remove` is used. Default is `NULL`.

- save_flagged:

  (logical) whether to save the flagged (removed) records. If `TRUE`, an
  `output_dir` must be provided. Default is `FALSE`.

- output_dir:

  (character) path to an existing directory where removed flagged
  records will be saved. Only used when `save_flagged = TRUE`.

- overwrite:

  (logical) whether to overwrite existing files in `output_dir`. Only
  used when `save_flagged = TRUE`. Default is `FALSE`.

- output_format:

  (character) output format for saving removed records. Options are
  `".csv"` or `".gz"`. Only used when `save_flagged = TRUE`. Default is
  `".gz"`.

## Value

A `data.frame` containing only the valid (kept) records according to the
flags and additional criteria.

## Details

The following flags are available: correct_country, correct_state,
cultivated, fossil, inaturalist, faunabr, florabr, wcvp, iucn,
duplicated, thin_geo, thin_env, .val, .equ, .zer, .cap, .cen, .sea,
.urb, .otl, .gbf, .inst, and .aohi.

## Examples

``` r
# Load example data
data("occ_flagged", package = "RuHere")

# Remove all flagged records
occ_valid <- remove_flagged(occ = occ_flagged)

# Remove flagged records and force removal of some unflagged records
to_remove <- c("gbif_5987", "specieslink_2301", "gbif_18761")
occ_valid2 <- remove_flagged(occ = occ_flagged,
                             force_remove = to_remove)

# Remove flagged records but keep some flagged ones
to_keep <- c("gbif_14501", "gbif_12002", "gbif_5168")
occ_valid3 <- remove_flagged(occ = occ_flagged,
                            force_keep = to_keep)
```
