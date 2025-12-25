# Flag occurrence records sourced from iNaturalist

This function identifies and flags occurrence records sourced from
iNaturalist. It can flag all iNaturalist records or only those that do
not have Research Grade status.

## Usage

``` r
flag_inaturalist(occ, columns = "datasetName", research_grade = FALSE)
```

## Arguments

- occ:

  (data.frame) a data frame containing the occurrence records to be
  examined, preferably standardized using
  [`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md).
  Must contain the columns specified in `columns`.

- columns:

  (character) column name in `occ` where the function will search for
  the term "iNaturalist". Default is "datasetName".

- research_grade:

  (logical) whether to flag *all* records from iNaturalist, including
  those with Research Grade status. Default is `FALSE`, meaning that
  only iNaturalist records **without** Research Grade will be flagged.

## Value

A `data.frame` that is the original `occ` data frame augmented with a
new column named `inaturalist_flag`. Flagged records receive `FALSE`,
while all other records receive `TRUE`.

## Details

According to
[iNaturalist](https://help.inaturalist.org/en/support/solutions/articles/151000169936-what-is-the-data-quality-assessment-and-how-do-observations-qualify-to-become-research-grade-),
Observations become Research Grade when:

- the iNaturalist community agrees on species-level ID or lower, i.e.
  when more than 2/3 of identifiers agree on a taxon;

- the community taxon and the observation taxon agree;

- or the community agrees on an ID between family and species and votes
  that the community taxon is as good as it can be.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
# Flag only iNaturalist records without Research Grade
occ_inat <- flag_inaturalist(occ = occurrences, research_grade = FALSE)
table(occ_inat$inaturalist_flag) # Number of records flagged (FALSE)
#> 
#> TRUE 
#> 4080 
# Flag all iNaturalist records (including Research Grade)
occ_inat <- flag_inaturalist(occ = occurrences, research_grade = TRUE)
table(occ_inat$inaturalist_flag) # Number of records flagged (FALSE)
#> 
#> FALSE  TRUE 
#>   869  3211 
```
