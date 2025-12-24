# States dictionary for standardizing state and province names and codes

Provides lookup tables used to standardize subnational administrative
units (states and provinces) in occurrence datasets.

Generated from
[`rnaturalearth::ne_states()`](https://docs.ropensci.org/rnaturalearth/reference/ne_states.html),
it includes a wide range of name variants (in multiple languages,
transliterations, and common abbreviations), as well as postal codes for
each unit.

This dictionary allows consistent mapping of user-provided names such as
`"são paulo"`, `"sao paulo"`, `"SP"`, `"illinois"`, `"ill."`,
`"bayern"`, `"bavaria"` to a single standardized state or province name.

## Usage

``` r
states_dictionary
```

## Format

A named list with two data frames:

- states_name:

  A data frame with columns:

  state_name

  :   Character. Name variants of states or provinces from
      `ne_states()`, lowercased and accent-stripped.

  state_suggested

  :   Character. Standardized state/province name, also lowercased and
      accent-stripped.

  country

  :   Character. Country associated with the state/province, lowercased
      and accent-stripped.

- states_code:

  A data frame with columns:

  state_code

  :   Character. Postal codes from `ne_states()`, cleaned and converted
      to uppercase.

  state_suggested

  :   Character. Standardized state/province name corresponding to the
      code.

  country

  :   Character. Country associated with the code.

## Details

The dictionary is constructed by:

- selecting administrative units of type `"State"` or `"Province"`;

- extracting multiple name fields, including alternative names and
  multilingual fields;

- normalizing names to lowercase and removing accents;

- normalizing codes to uppercase;

- removing duplicates and ambiguous entries;

- removing rows with missing names or codes.

## Examples

``` r
data(states_dictionary)
head(states_dictionary$states_name)
#>         state_name  state_suggested   country
#> 1       entre rios       entre rios argentina
#> 2 kalimantan timur kalimantan timur indonesia
#> 3            sabah            sabah  malaysia
#> 4            salta            salta argentina
#> 5            jujuy            jujuy argentina
#> 6     an nabatiyah     an nabatiyah   lebanon
head(states_dictionary$states_code)
#>   state_code  state_suggested   country
#> 1         ER       entre rios argentina
#> 2         KI kalimantan timur indonesia
#> 3         SA            sabah  malaysia
#> 4         SA            salta argentina
#> 5         JY            jujuy argentina
#> 6         NA     an nabatiyah   lebanon
```
