# Occurrence records of azure jay from SpeciesLink

A cleaned dataset of occurrence records for azure jay (*Cyanocorax
caeruleus*) retrieved from the SpeciesLink using
[`get_specieslink()`](https://wevertonbio.github.io/RuHere/reference/get_specieslink.md).

Records were cleaned using the package's internal duplicate-flagging
workflow.

## Usage

``` r
occ_splink
```

## Format

A data frame containing georeferenced SpeciesLink occurrence records for
*C. caeruleus* after all cleaning steps.

## See also

[`get_specieslink()`](https://wevertonbio.github.io/RuHere/reference/get_specieslink.md),
[`flag_duplicates()`](https://wevertonbio.github.io/RuHere/reference/flag_duplicates.md),
[`remove_flagged()`](https://wevertonbio.github.io/RuHere/reference/remove_flagged.md)

## Examples

``` r
# First rows
head(occ_splink)
#>         scientificname collectioncode catalognumber decimallongitude
#> 1 Cyanocorax caeruleus          CSUEL       LDA 171                0
#> 2 Cyanocorax caeruleus          CSUEL       LDA 218                0
#> 3 Cyanocorax caeruleus          CSUEL       LDA 114                0
#> 4 Cyanocorax caeruleus          CSUEL       LDA 145                0
#> 5 Cyanocorax caeruleus          CSUEL       LDA 148                0
#> 6 Cyanocorax caeruleus          CSUEL       LDA 151                0
#>   decimallatitude maximumelevationinmeters country stateprovince   county
#> 1               0                     <NA>    <NA>          <NA>     <NA>
#> 2               0                     <NA>    <NA>          <NA>     <NA>
#> 3               0                     <NA>    <NA>            PR Palmeira
#> 4               0                     <NA>    <NA>          <NA>     <NA>
#> 5               0                     <NA>    <NA>            PR Palmeira
#> 6               0                     <NA>    <NA>            PR Palmeira
#>                             locality yearcollected recordedby identifiedby
#> 1                               <NA>          1986       <NA>         <NA>
#> 2                               <NA>          1986       <NA>         <NA>
#> 3 Fazenda Santa Rita - Palmeira - PR          1986       <NA>         <NA>
#> 4                               <NA>          1986       <NA>         <NA>
#> 5 Fazenda Santa Rita - Palmeira - PR          1986       <NA>         <NA>
#> 6 Fazenda Santa Rita - Palmeira - PR          1986       <NA>         <NA>
#>   basisofrecord
#> 1          <NA>
#> 2          <NA>
#> 3          <NA>
#> 4          <NA>
#> 5          <NA>
#> 6          <NA>
#>                                                                                                                                                                                                                                                          occurrenceremarks
#> 1                                                                                                                                                                               <nO>OBS.: Bel sendo acariciada no pescoço ; gritos de proximidade ; de reconhecimento</nO>
#> 2                                                                                                                                                                                                                                        <nO>OBS.: Bel ; movimentação</nO>
#> 3                                                                                                                                               <nO>HABITAT: Floresta de araucária ; estrato médio e superior OBS.: indivíduos adultos próximos do ninho ; espontâneo</nO>
#> 4                                                                                                                                                   <nO>HABITAT: cativeiro OBS.: a- vocalização de Sara recebendo comida do Michael ; b- soto-voce - Andrew (sozinho)</nO>
#> 5 <nO>HABITAT: a 100m do ninho OBS.: perseguiam um gavião (provavlemente Buteo magnirostris) ; as gralhas permaneceream em um pinheiro sobre o local onde estava escondido o gavião ; a última sequencia se refere à perseguição do gavião e teve início no momento d</nO>
#> 6                                                                                                                                                                                                                                                                     <NA>

# Number of cleaned records
nrow(occ_splink)
#> [1] 361
```
