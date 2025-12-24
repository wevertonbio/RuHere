# Occurrence records of *Puma concolor* from AtlanticR

A subset of Atlantic mammals records obtained from the
`atlanticr::atlantic_mammals` dataset, containing occurrences of *Puma
concolor*.

This dataset is provided as an example to illustrate how to create
user-defined metadata templates for occurrence records from external
sources using the package’s
[`create_metadata()`](https://wevertonbio.github.io/RuHere/reference/create_metadata.md)
function.

## Usage

``` r
puma_atlanticr
```

## Format

A data frame where each row represents a single occurrence record of
*Puma concolor*. Columns include species name, location, and other
relevant metadata fields provided by the `atlantic_mammals` dataset.

## See also

[`create_metadata()`](https://wevertonbio.github.io/RuHere/reference/create_metadata.md),
[`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md)

## Examples

``` r
# Preview first rows
head(puma_atlanticr)
#> # A tibble: 6 × 40
#>   id    reference_paper_number country state         municipality study_location
#>   <chr>                  <dbl> <chr>   <chr>         <chr>        <chr>         
#> 1 AML03                      3 Brazil  sao_paulo     Botucatu     Fazenda Exper…
#> 2 AML04                      4 Brazil  santa_catari… Ipumirim     Atlantic fore…
#> 3 AML05                      5 Brazil  santa_catari… Sao Domingos Parque Estadu…
#> 4 AML07                      5 Brazil  santa_catari… Antonio Car… RPPN Caraguata
#> 5 AML10                      5 Brazil  santa_catari… Sideropolis  Reserva Parti…
#> 6 AML11                      5 Brazil  santa_catari… Urupema      RPPN Serra da…
#> # ℹ 34 more variables: latitude <dbl>, longitude <dbl>, precision <chr>,
#> #   size_ha <chr>, temperature <chr>, altitude <chr>, annual_rainfall <chr>,
#> #   vegetation_type <chr>, protect_area <chr>, matrix <chr>, reference <chr>,
#> #   publication_year <dbl>, type_of_publication <chr>, month_start <chr>,
#> #   year_start <dbl>, month_finish <chr>, year_finish <dbl>,
#> #   total_of_months <dbl>, sampling_habitat <chr>, effort <dbl>,
#> #   effort_method <chr>, method <chr>, order <chr>, genus_on_paper <chr>, …

# Count occurrences per year
table(puma_atlanticr$year)
#> Warning: Unknown or uninitialised column: `year`.
#> < table of extent 0 >

```
