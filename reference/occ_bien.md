# Occurrence records of Yellow Trumpet Tree from BIEN

A cleaned dataset of occurrence records for Yellow Trumpet Tree
(*Handroanthus serratifolius*) retrieved from the BIEN database. The raw
data were downloaded using
[`get_bien()`](https://wevertonbio.github.io/RuHere/reference/get_bien.md)

The dataset was subsequently processed with the package’s internal
flagging workflow
([`flag_duplicates()`](https://wevertonbio.github.io/RuHere/reference/flag_duplicates.md)
and
[`remove_flagged()`](https://wevertonbio.github.io/RuHere/reference/remove_flagged.md))
to remove duplicated records.

## Usage

``` r
occ_bien
```

## Format

A data frame containing spatial coordinates, taxonomic information, and
metadata returned by BIEN, after cleaning. Columns include (but may not
be limited to):

- `scrubbed_species_binomial`: Cleaned species name

- `longitude`, `latitude`: Geographic coordinates

- `country`, `state_province`, and other political boundary fields

## See also

[`get_bien()`](https://wevertonbio.github.io/RuHere/reference/get_bien.md)

## Examples

``` r
# View dataset
head(occ_bien)
#>    scrubbed_species_binomial native_status               native_status_reason
#> 1 Handroanthus serratifolius             N Native to region, as per checklist
#> 2 Handroanthus serratifolius             N Native to region, as per checklist
#> 3 Handroanthus serratifolius             N Native to region, as per checklist
#> 4 Handroanthus serratifolius             N Native to region, as per checklist
#> 5 Handroanthus serratifolius             N Native to region, as per checklist
#> 6 Handroanthus serratifolius             N Native to region, as per checklist
#>   native_status_sources is_introduced native_status_country
#> 1   conosur, flbr, powo             0                     N
#> 2        powo, tropicos             0                     N
#> 3                  powo             0                     N
#> 4   conosur, flbr, powo             0                     N
#> 5   conosur, flbr, powo             0                     N
#> 6                  powo             0                     N
#>   native_status_state_province native_status_county_parish  country
#> 1                            N                        <NA>   Brazil
#> 2                         <NA>                        <NA>  Bolivia
#> 3                         <NA>                        <NA> Colombia
#> 4                            N                        <NA>   Brazil
#> 5                            N                        <NA>   Brazil
#> 6                         <NA>                        <NA>     Peru
#>   state_province county                                            locality
#> 1       Amazonas   <NA> Reserva Florestal Ducke, Manaus-Itacoatiara, km 26.
#> 2           <NA>   <NA>                                                <NA>
#> 3          Cesar   <NA>                                         Campoalegre
#> 4      Tocantins   <NA>                Faz. São Judas - Porto Nacional - TO
#> 5          Goias   <NA>                                            Luziânia
#> 6           <NA>   <NA>                                        Puerto Ocopa
#>   elevation_m  latitude longitude date_collected datasource
#> 1          NA  -2.88000 -59.97000     1995-08-04       GBIF
#> 2          NA -14.55278 -68.68686           <NA>     traits
#> 3          NA  10.11333 -73.78639     2017-09-04       GBIF
#> 4          NA -10.80000 -48.41667     2016-08-29       GBIF
#> 5          NA -16.25250 -47.95030           <NA>       GBIF
#> 6         600 -11.22430 -74.41720           <NA>    dryflor
#>                                                             dataset
#> 1                                                                US
#> 2                                                            traits
#> 3 Patrimonio Natural Fondo para la Biodiversidad y Áreas Protegidas
#> 4                                                              UEFS
#> 5                                                               UnB
#> 6                                                           dryflor
#>                                                           dataowner
#> 1                                                                US
#> 2                                                            traits
#> 3 Patrimonio Natural Fondo para la Biodiversidad y Áreas Protegidas
#> 4                                                              UEFS
#> 5                                                               UnB
#> 6                                                           dryflor
#>                                         custodial_institution_codes
#> 1                                                                US
#> 2                                                              <NA>
#> 3 Patrimonio Natural Fondo para la Biodiversidad y Áreas Protegidas
#> 4                                                              UEFS
#> 5                                                               UnB
#> 6                                                              <NA>
#>   collection_code datasource_id catalog_number
#> 1          Botany          4885     2284210913
#> 2            <NA>            NA        1544595
#> 3            <NA>          1516     1829119958
#> 4           HUEFS          1909     1837839064
#> 5              UB          3029     1087243702
#> 6            <NA>            NA           <NA>
#>                                recorded_by record_number identified_by
#> 1 M. Costa, P. A. Assunçao & E. C. Pereira           333          <NA>
#> 2                                     <NA>          <NA>          <NA>
#> 3           Cesar Barbosa | Linda J. Ortíz          <NA>          <NA>
#> 4                          Chagas, D.B.das           132          <NA>
#> 5                                Incógnito         12149    Gentry, AH
#> 6                                     <NA>          <NA>          <NA>
#>   date_identified identification_remarks is_cultivated_observation
#> 1            <NA>                   <NA>                        NA
#> 2            <NA>                   <NA>                        NA
#> 3            <NA>                   <NA>                        NA
#> 4            <NA>                   <NA>                        NA
#> 5            <NA>                   <NA>                        NA
#> 6            <NA>                   <NA>                        NA
#>   is_cultivated_in_region is_location_cultivated  observation_type
#> 1                       0                     NA          specimen
#> 2                       0                     NA  trait occurrence
#> 3                       0                     NA human observation
#> 4                       0                     NA          specimen
#> 5                       0                     NA          specimen
#> 6                       0                     NA        occurrence

# Number of records
nrow(occ_bien)
#> [1] 1791

```
