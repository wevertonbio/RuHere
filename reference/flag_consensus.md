# Get consensus across multiple flags

This functions creates a new column representing the consensus across
multiple flag columns. The consensus can be computed in two ways:

- `"all_true"`: A record is considered valid (`TRUE`) only if **all**
  specified flag are valid (`TRUE`).

- `"any_true"`: A record is considered valid (`TRUE`) if **at least
  one** specified flag is valid (`TRUE`).

## Usage

``` r
flag_consensus(
  occ,
  flags,
  consensus_rule = "all_true",
  flag_name = "consensus_flag",
  remove_flag_columns = FALSE
)
```

## Arguments

- occ:

  (data.frame or data.table) a dataset with occurrence records that has
  been processed by two or more flagging functions.

- flags:

  (character) a string vector with the names of the flags to be used in
  the consensus evaluation. See details for see the options.

- consensus_rule:

  (character) A string specifying how the consensus should be computed.
  Options are `"all_true"` (record is considered valid only when **all**
  flags are `TRUE` or `"any_true"`(record is considered valid when **at
  least one** flag is `TRUE`. Default is `"all_true"`

- flag_name:

  (character) name of the column that will store the consensus result.
  Default is `"consensus_flag"`.

- remove_flag_columns:

  (logical) whether to remove the original flag columns specified in
  `flags` from the final output. Default is `FALSE`.

## Value

The original `occ` with an additional logical column defined by
`flag_name`, indicating the consensus result based on the selected
`consensus_rule`.

## Details

The following flags are available: correct_country, correct_state,
cultivated, fossil, inaturalist, faunabr, florabr, wcvp, iucn,
duplicated, thin_geo, thin_env, year, .val, .equ, .zer, .cap, .cen,
.sea, .urb, .otl, .gbf, .inst, and .aohi.

## Examples

``` r
# Load example data
data("occ_flagged", package = "RuHere")

# Get consensus using florabr, wcvp, and iucn flags
 # Valid (TRUE) only when all flags are TRUE
occ_consensus_all <- flag_consensus(occ = occ_flagged,
                                    flags = c("florabr", "wcvp", "iucn"),
                                    consensus_rule = "all_true")
# Valid (TRUE) when at least one flag is TRUE
occ_consensus_any <- flag_consensus(occ = occ_flagged,
                                    flags = c("florabr", "wcvp", "iucn"),
                                    consensus_rule = "any_true")
```
