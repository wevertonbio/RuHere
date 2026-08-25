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

# RuHere 1.0.1 (February 2026)

* Initial CRAN submission.
