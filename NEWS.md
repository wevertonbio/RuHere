# RuHere 1.1.2 (In development)

* Fix additional_predicates in `request_gbif()` to allow combinations of `pred_and()` and `pred_or()`.
* RuHere now depends on rgbif (>= 3.9.0) due to changes in taxon keys, which were switched from the GBIF Backbone to the COL (Catalogue of Life) Extended Release.
* In `standardize_countries()` and `standardize_states()`, sanitize non-alphabetic strings (e.g., "?") to NA before fuzzy matching to prevent row expansion on merge.
* New function `fix_species_names()`: replaces misspelled, outdated, or unstandardized species names in an occurrence dataset using a user-supplied reference dictionary.
* Handle NA coordinates, fix positional args, and optimize c`heck_countries()` and `check_states()`.
* In `flag_duplicates()`, `replace terra::extract()` with `terra::cellFromXY()`, deduplicate via `!duplicated(..., by = to_group)` on a lightweight subset, preserve original row order and input class, and avoid grouping NA coordinates/cells as duplicates.
* In `check_countries()` & `check_states()`: Use `data.table` radix grouping and two-stage buffering (applying `terra::buffer()` only when points fall outside the raw polygon), filter out NA coordinates before spatialization, and use `DT[, ("col") := ...]` with `setcolorder()` to avoid column reallocation errors.
* In `flag_fossil()`, `flag_cultivated()`, & `flag_inaturalist()`, perform case-insensitive regex matching on unique column values only to speed up performance.
* In `remove_invalid_coordinates()` & `flag_year()`: Standardize input argument checks, suppress numeric coercion warnings, and ensure consistent return types when `return_invalid = TRUE`.

# RuHere 1.1.1 (October 2026)

* Add datasets from GABI and from Vascular epiphytes of the South America Dry Diagonal to `get_datapaper()` and `cite_datapaper()`.
* In `faunabr_here()` and `florabr_here()`, add the `get_fixed_version` argument to allow users to download fixed versions of faunabr and florabr, with `TRUE` as the default.
* In `map_here()`, add the `map.types` argument to allow users to select which background map layers to display, with "OpenStreetMap" as the default layer.
* RuHere now depends on faunabr (>= 1.1.2) and florabr(>= 1.4.0.).
* Update datasets to be compatible with florabr 1.4.0.
* Fix bug in `flag_consensus()` when using "bien" as one of the flags.
* Fix bug in `map_here()` when the column flags have NA.
* Fix bug when using additional/user flags in `summarize_flags()`.
* Add new function: `request_gbif_specieslist()`.
* Fig bug in `remove_flagged()` when occ is a data.table.
* In `ggmap_here()`, add option to remove records with coordinates outside the Earth.

# RuHere 1.1.0 (August 2026)

* Fix bug in `remove_flagged()` when flag is not "all".
* Add new functions: `inventory_completeness()`, `count_flags()`, `get_datapaper()`, and `cite_datapaper()`.
* Add new dataset `atlantic_amphibians` to run example of `inventory_completeness()`
* `request_gbif()` now checks for GBIF credentials and returns a clear error pointing to `set_gbif_credentials()` if missing, instead of an uninformative error from rgbif.
* Clarify in the documentation that TRUE/FALSE semantics are consistent across all flagging functions: `TRUE` means the record passed the test and is eligible for retention.
* Standardize terminology to "specialists' range information" across function documentation.
* Clarify in `bien_here()` that BIEN range polygons are derived from ecological niche models, unlike the taxonomist-curated data from IUCN, WCVP, florabr, and faunabr.
* Add authentication notes and updated examples to `request_gbif()`, `get_specieslink()`, and `iucn_here()`.
* Fix incorrect documentation of the `verbose` argument in `flag_wcvp()`.
* RuHere now depends on **faunabr** (>= 1.1.1).
* Update vignette for obtaining data: add GBIF citation guidelines and instructions for using `get_datapaper()`.
* Add GBIF references in `occurrences`, `occ_gbif`, and `occ_flagged`.
* Fix bug in `flag_bien()` when `occ` is a `data.table` object.
* In `bind_here()`, add argument `ignore.attr` to allow binding columns with different attributes (e.g. class).
* In `format_columns()`, force column `eventDate` to be a character.
* Add new function `request_gbif_specieslist()`.

# RuHere 1.0.1 (February 2026)

* Initial CRAN submission.
