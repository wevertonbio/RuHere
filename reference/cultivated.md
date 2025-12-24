# Dictionary of terms used to flag cultivated individuals

`cultivated` is a list of character vectors containing keywords used to
identify whether an occurrence record refers to cultivated or
non-cultivated individuals.

This object is used internally by
[`flag_cultivated()`](https://wevertonbio.github.io/RuHere/reference/flag_cultivated.md)
to scan occurrence fields (such as notes, habitat descriptions, or
remarks) and classify records as *cultivated* or *not cultivated* based
on textual patterns.

The list combines terms from `plantR` (`plantR:::cultivated` and
`plantR:::notCultivated`) with additional multilingual variants commonly
found in herbarium metadata.

## Usage

``` r
cultivated
```

## Format

A named list with two elements:

- `cultivated`:

  Character vector. Terms that indicate an individual is cultivated.
  Imported from `plantR:::cultivated`.

- `not_cultivated`:

  Character vector. Terms suggesting an individual is *not* cultivated
  (e.g., “not cultivated”, “not planted”, “no plantada”, “no
  cultivada”), including terms from `plantR:::notCultivated`.

## Details

These terms are matched case-insensitively after text cleaning (e.g.,
lowercasing and accent removal).

## References

de Lima, Renato AF, et al. plantR: An R package and workflow for
managing species records from biological collections. **Methods in
Ecology and Evolution**, 14.2 (2023): 332-339.

## See also

`flag_cultivated`

## Examples

``` r
data(cultivated)

cultivated$cultivated
#>  [1] "cultivated"        "cultivada"         "cultivado"        
#>  [4] "cultivato"         "cultivad"          "under cultivation"
#>  [7] "exotic"            "exótica"           "plantada"         
#> [10] "plantado"          "planted"           "plantio"          
#> [13] "arboreto"          "arboretum"         "pomar"            
#> [16] "área de visitação" "cult\\."           "cant\\. [a-z]"    
#> [19] "cant [A-Z]"        "cant\\. [0-9]"     "cant \\. [0-9]"   
#> [22] "cant [0-9]"        "\\(cult\\)"        "\\(cult \\)"      
#> [25] "in cultivo"        "in cultis"         " quadra [a-z]"    
#> [28] "quadra [a-z] do"   "naturalised"       "em experimento de"
cultivated$not_cultivated
#>  [1] "nativa"               "espontânea"           "pastagem cultivada"  
#>  [4] "área do arboreto"     "presença de exóticas" " área cultivada"     
#>  [7] " cultivated area"     "no plantada"          "no cultivada"        
#> [10] "not cultivated"       "not planted"         
```
