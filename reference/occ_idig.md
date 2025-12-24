# Occurrence records of azure jay from iDigBio

A cleaned dataset of occurrence records for azure jay (*Cyanocorax
caeruleus*) retrieved from the iDigBio using
[`get_idigbio()`](https://wevertonbio.github.io/RuHere/reference/get_idigbio.md).

Records were cleaned using the package's internal duplicate-flagging
workflow.

## Usage

``` r
occ_idig
```

## Format

A data frame containing georeferenced iDigBio occurrence records for *C.
caeruleus* after all cleaning steps.

## See also

[`get_idigbio()`](https://wevertonbio.github.io/RuHere/reference/get_idigbio.md),
[`flag_duplicates()`](https://wevertonbio.github.io/RuHere/reference/flag_duplicates.md),
[`remove_flagged()`](https://wevertonbio.github.io/RuHere/reference/remove_flagged.md)

## Examples

``` r
# First rows
head(occ_idig)
#>   UberH3 aggs associatedsequences barcodevalue      basisofrecord bed
#> 1     NA   NA                NULL           NA machineobservation  NA
#> 2     NA   NA                NULL           NA  preservedspecimen  NA
#> 3     NA   NA                NULL           NA  preservedspecimen  NA
#> 4     NA   NA                NULL           NA  preservedspecimen  NA
#> 5     NA   NA                NULL           NA machineobservation  NA
#> 6     NA   NA                NULL           NA  preservedspecimen  NA
#>          canonicalname catalognumber class  collectioncode
#> 1 cyanocorax caeruleus       0005478  aves            fnjv
#> 2 cyanocorax caeruleus         29487  aves           birds
#> 3 cyanocorax caeruleus         52342  aves           birds
#> 4 cyanocorax caeruleus     12.crv.34  aves collection maës
#> 5 cyanocorax caeruleus       0050394  aves            fnjv
#> 6 cyanocorax caeruleus        178589  aves             orn
#>                                                                              collectionid
#> 1                                                         https://specieslink.net/col/238
#> 2                                                                                    <NA>
#> 3 https://scientific-collections.gbif.org/collection/bf635c34-0569-4fb7-ad9c-91d821bac9e4
#> 4                                                                                  1929.1
#> 5                                                         https://specieslink.net/col/238
#> 6                                                                                    <NA>
#>   collectionname                     collector commonname commonnames
#> 1             NA alexandre luis padovan aleixo       <NA>        NULL
#> 2             NA                          <NA>       <NA>        NULL
#> 3             NA                partridge, w h       <NA>        NULL
#> 4             NA               sans collecteur       <NA>        NULL
#> 5             NA                luiz dos anjos       <NA>        NULL
#> 6             NA                       camargo       <NA>        NULL
#>       continent coordinateuncertainty   country countrycode county
#> 1 south america                    NA    brazil         bra   <NA>
#> 2 south america                  1469  paraguay         pry   <NA>
#> 3 south america                    NA argentina         arg   <NA>
#> 4          <NA>                    NA      <NA>        <NA>   <NA>
#> 5 south america                    NA    brazil         bra   <NA>
#> 6 south america                  3036    brazil         bra   <NA>
#>                              datasetid             datecollected
#> 1 7ddf754f-d193-4cc9-b351-99906754a03b 1995-03-08T00:00:00+00:00
#> 2 7ddf754f-d193-4cc9-b351-99906754a03b 1901-08-23T00:00:00+00:00
#> 3 7ddf754f-d193-4cc9-b351-99906754a03b 1959-08-25T00:00:00+00:00
#> 4                                 <NA> 1909-06-25T00:00:00+00:00
#> 5 7ddf754f-d193-4cc9-b351-99906754a03b 2002-12-26T00:00:00+00:00
#> 6 7ddf754f-d193-4cc9-b351-99906754a03b 1934-10-10T00:00:00+00:00
#>                       datemodified       dqs earliestageorloweststage
#> 1 2025-05-23T18:36:26.002398+00:00 0.2318841                       NA
#> 2 2025-05-23T18:13:47.213183+00:00 0.2463768                       NA
#> 3 2025-07-24T17:48:49.086870+00:00 0.3188406                       NA
#> 4 2017-02-20T09:01:11.593455+00:00 0.2463768                       NA
#> 5 2025-05-23T18:36:26.002398+00:00 0.2463768                       NA
#> 6 2025-10-29T16:28:32.411713+00:00 0.2898551                       NA
#>   earliesteonorlowesteonothem earliestepochorlowestseries
#> 1                          NA                        <NA>
#> 2                          NA                        <NA>
#> 3                          NA                        <NA>
#> 4                          NA                        <NA>
#> 5                          NA                        <NA>
#> 6                          NA                        <NA>
#>   earliesteraorlowesterathem earliestperiodorlowestsystem
#> 1                         NA                         <NA>
#> 2                         NA                         <NA>
#> 3                         NA                         <NA>
#> 4                         NA                         <NA>
#> 5                         NA                         <NA>
#> 6                         NA                         <NA>
#>                                       etag  eventdate   family fieldnumber
#> 1 d4cd02bb4a594a94541a6af6dcad819a10015103 1995-03-08 corvidae        <NA>
#> 2 b6bf9b574b4c9b53e4de4f85a713b1c9cbb8f1c6 1901-08-23 corvidae        <NA>
#> 3 c886e564e36d73ce63fc051b52c121662d7b8c96 1959-08-25 corvidae        <NA>
#> 4 cfac449459e9f92f607d77141bea36968ed2da8d  1909-6-25 corvidae        <NA>
#> 5 8a84b83ceee954cc5f0a5a792f5904abf49312db 2002-12-26 corvidae        <NA>
#> 6 cd336080ae997a693fc65558fca43abd9f1311f6 1934-10-10 corvidae        <NA>
#>                                                                                                                                                                                                                                                                                                                                                                            flags
#> 1     dwc_taxonrank_added, dwc_multimedia_added, gbif_reference_added, idigbio_isocountrycode_added, gbif_canonicalname_added, dwc_taxonomicstatus_added, gbif_genericname_added, dwc_datasetid_added, dwc_continent_added, dwc_parentnameusageid_added, dwc_scientificnameauthorship_added, dwc_taxonid_added, gbif_vernacularname_added, gbif_taxon_corrected, dwc_order_added
#> 2                                        dwc_scientificnameauthorship_added, dwc_taxonrank_replaced, dwc_taxonomicstatus_added, gbif_genericname_added, dwc_datasetid_added, gbif_taxon_corrected, dwc_multimedia_added, dwc_taxonid_added, idigbio_isocountrycode_added, gbif_vernacularname_added, gbif_canonicalname_added, gbif_reference_added, dwc_parentnameusageid_added
#> 3                geopoint_low_precision, dwc_multimedia_added, dwc_taxonrank_replaced, dwc_taxonomicstatus_added, gbif_genericname_added, dwc_datasetid_added, gbif_taxon_corrected, dwc_parentnameusageid_added, dwc_scientificnameauthorship_added, dwc_taxonid_added, idigbio_isocountrycode_added, gbif_vernacularname_added, gbif_canonicalname_added, gbif_reference_added
#> 4                                                                                                                                                                                                                                                                                                                                                         geopoint_datum_missing
#> 5     dwc_taxonrank_added, dwc_multimedia_added, gbif_reference_added, dwc_scientificnameauthorship_added, idigbio_isocountrycode_added, gbif_canonicalname_added, dwc_taxonomicstatus_added, gbif_genericname_added, dwc_datasetid_added, dwc_continent_added, dwc_parentnameusageid_added, dwc_taxonid_added, gbif_vernacularname_added, gbif_taxon_corrected, dwc_order_added
#> 6 geopoint_datum_error, rev_geocode_eez, dwc_taxonrank_added, gbif_reference_added, gbif_genericname_added, dwc_datasetid_added, dwc_scientificnameauthorship_replaced, gbif_vernacularname_added, dwc_parentnameusageid_added, idigbio_isocountrycode_added, dwc_taxonomicstatus_added, dwc_taxonid_added, dwc_multimedia_added, gbif_taxon_corrected, gbif_canonicalname_added
#>   formation      genus geologicalcontextid       lon       lat group hasImage
#> 1        NA cyanocorax                  NA        NA        NA    NA    FALSE
#> 2        NA cyanocorax                  NA -56.95254 -25.66729    NA    FALSE
#> 3        NA cyanocorax                  NA -53.90000 -26.47000    NA    FALSE
#> 4        NA cyanocorax                  NA -46.63639 -23.54770    NA    FALSE
#> 5        NA cyanocorax                  NA -51.33667 -23.76056    NA    FALSE
#> 6        NA cyanocorax                  NA -47.95000 -25.01667    NA    FALSE
#>   hasMedia
#> 1    FALSE
#> 2    FALSE
#> 3    FALSE
#> 4    FALSE
#> 5    FALSE
#> 6    FALSE
#>                                                                                        highertaxon
#> 1                                                                                             <NA>
#> 2                                                                                             <NA>
#> 3                               animalia | chordata | aves | passeriformes | corvidae | cyanocorax
#> 4                                                                                             <NA>
#> 5                                                                                             <NA>
#> 6 animalia chordata vertebrata aves neornithes passeriformes oscines corvidae cyanocorax caeruleus
#>   highestbiostratigraphiczone individualcount infraspecificepithet
#> 1                          NA               1                   NA
#> 2                          NA              NA                   NA
#> 3                          NA              NA                   NA
#> 4                          NA               1                   NA
#> 5                          NA              NA                   NA
#> 6                          NA               1                   NA
#>   institutioncode
#> 1         unicamp
#> 2            ummz
#> 3            lacm
#> 4            boum
#> 5         unicamp
#> 6             mcz
#>                                                                              institutionid
#> 1                                                                                     <NA>
#> 2                                                                                     <NA>
#> 3 https://scientific-collections.gbif.org/institution/2fc908ba-e775-49e2-bffe-6b9041c3b70f
#> 4                                                                                     <NA>
#> 5                                                                                     <NA>
#> 6                                                     b4640710-8e03-11d8-b956-b8a03c50a862
#>   institutionname  kingdom latestageorhigheststage latesteonorhighesteonothem
#> 1              NA animalia                      NA                         NA
#> 2              NA animalia                      NA                         NA
#> 3              NA animalia                      NA                         NA
#> 4              NA animalia                      NA                         NA
#> 5              NA animalia                      NA                         NA
#> 6              NA animalia                      NA                         NA
#>   latestepochorhighestseries latesteraorhighesterathem
#> 1                         NA                        NA
#> 2                         NA                        NA
#> 3                         NA                        NA
#> 4                         NA                        NA
#> 5                         NA                        NA
#> 6                         NA                        NA
#>   latestperiodorhighestsystem lithostratigraphicterms
#> 1                          NA                      NA
#> 2                          NA                      NA
#> 3                          NA                      NA
#> 4                          NA                      NA
#> 5                          NA                      NA
#> 6                          NA                      NA
#>                                                        locality
#> 1                                    parque estadual intervales
#> 2                                                       sapucay
#> 3                                                       tobunas
#> 4                                                     sao paulo
#> 5 fragmento florestal próximo ao parque estadual mata dos godoy
#> 6                                                       cananea
#>   lowestbiostratigraphiczone maxdepth maxelevation mediarecords member mindepth
#> 1                         NA       NA           NA         NULL     NA       NA
#> 2                         NA       NA           NA         NULL     NA       NA
#> 3                         NA       NA           NA         NULL     NA       NA
#> 4                         NA       NA          760         NULL     NA       NA
#> 5                         NA       NA           NA         NULL     NA       NA
#> 6                         NA       NA           NA         NULL     NA       NA
#>   minelevation municipality                             occurrenceid
#> 1           NA  sete barras https://specieslink.net/guid/238/0005478
#> 2           NA         <NA>     b3101421-f924-11e2-b158-782bcb84bc75
#> 3           NA         <NA>     5bf19932-d3e8-46db-b996-53379ca09ca5
#> 4           NA         <NA>                                12.crv.34
#> 5           NA     londrina https://specieslink.net/guid/238/0050394
#> 6           NA         <NA>                           mcz:orn:178589
#>           order   phylum query
#> 1 passeriformes chordata    NA
#> 2 passeriformes chordata    NA
#> 3 passeriformes chordata    NA
#> 4 passeriformes chordata    NA
#> 5 passeriformes chordata    NA
#> 6 passeriformes chordata    NA
#>                                                                                                                                        recordids
#> 1                                                                 5caa5b0d-24b7-4c69-9c7b-089f343b7efc\\https://specieslink.net/guid/238/0005478
#> 2 4a32531f-1580-499f-bd48-5b0ef4cc5722\\urn:catalog:ummz:birds:29487, 4a32531f-1580-499f-bd48-5b0ef4cc5722\\b3101421-f924-11e2-b158-782bcb84bc75
#> 3                                                                     2d853a6d-50ec-4931-8e91-48fc2491fdee\\5bf19932-d3e8-46db-b996-53379ca09ca5
#> 4                                                                                                3172ee72-4b10-4d6b-b4b3-f3319b25c60d\\12.crv.34
#> 5                                                                 5caa5b0d-24b7-4c69-9c7b-089f343b7efc\\https://specieslink.net/guid/238/0050394
#> 6                                                                                           271a9ce9-c6d3-4b63-a722-cb0adc48863f\\mcz:orn:178589
#>   recordnumber                            recordset       scientificname size
#> 1         <NA> 5caa5b0d-24b7-4c69-9c7b-089f343b7efc Cyanocorax caeruleus   NA
#> 2         <NA> 4a32531f-1580-499f-bd48-5b0ef4cc5722 Cyanocorax caeruleus   NA
#> 3        19784 2d853a6d-50ec-4931-8e91-48fc2491fdee Cyanocorax caeruleus   NA
#> 4         <NA> 3172ee72-4b10-4d6b-b4b3-f3319b25c60d Cyanocorax caeruleus   NA
#> 5         <NA> 5caa5b0d-24b7-4c69-9c7b-089f343b7efc Cyanocorax caeruleus   NA
#> 6         <NA> 271a9ce9-c6d3-4b63-a722-cb0adc48863f Cyanocorax caeruleus   NA
#>   specificepithet startdayofyear stateprovince taxonid taxonomicstatus
#> 1       caeruleus             67     são paulo 2482560        accepted
#> 2       caeruleus            235     paraguari 2482560        accepted
#> 3       caeruleus            237 misiones prov 2482560        accepted
#> 4       caeruleus            176          <NA> 2482560        accepted
#> 5       caeruleus            360        paraná 2482560        accepted
#> 6       caeruleus            283     sao paulo 2482560        accepted
#>   taxonrank typestatus                                 uuid verbatimeventdate
#> 1   species       <NA> 000d87f1-db3e-4784-91d2-91aec34abf85              <NA>
#> 2   species       <NA> 02ffadca-502b-469f-8d4a-299a7d8e91a5              <NA>
#> 3   species       <NA> 0408655e-5896-4e2e-8507-becfd903a0e1       25 aug 1959
#> 4   species       <NA> 0538e125-8fa4-412b-89c8-a44f38f46731     1909, 25 juin
#> 5   species       <NA> 0ccf5483-4c82-4214-aa7b-3e2523fddde6              <NA>
#> 6   species       <NA> 0d86f91a-6916-4432-93b1-d18c36cf015d        10/10/1934
#>                                            verbatimlocality version waterbody
#> 1                                                      <NA>      NA      <NA>
#> 2                                                      <NA>      NA      <NA>
#> 3 | south america | argentina | misiones prov |  |  tobunas      NA      <NA>
#> 4                                                      <NA>      NA      <NA>
#> 5                                                      <NA>      NA      <NA>
#> 6                 south america, brazil, sao paulo, cananea      NA      <NA>

# Number of cleaned records
nrow(occ_idig)
#> [1] 33
```
