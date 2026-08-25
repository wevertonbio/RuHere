# Changelog

## RuHere 1.1.0 (August 2026)

- Fix bug in
  [`remove_flagged()`](https://wevertonbio.github.io/RuHere/reference/remove_flagged.md)
  when flag is not “all”.
- Add new functions:
  [`inventory_completeness()`](https://wevertonbio.github.io/RuHere/reference/inventory_completeness.md),
  [`count_flags()`](https://wevertonbio.github.io/RuHere/reference/count_flags.md),
  [`get_datapaper()`](https://wevertonbio.github.io/RuHere/reference/get_datapaper.md),
  and
  [`cite_datapaper()`](https://wevertonbio.github.io/RuHere/reference/cite_datapaper.md).
- Add new dataset `atlantic_amphibians` to run example of
  [`inventory_completeness()`](https://wevertonbio.github.io/RuHere/reference/inventory_completeness.md)
- [`request_gbif()`](https://wevertonbio.github.io/RuHere/reference/request_gbif.md)
  now checks for GBIF credentials and returns a clear error pointing to
  [`set_gbif_credentials()`](https://wevertonbio.github.io/RuHere/reference/set_gbif_credentials.md)
  if missing, instead of an uninformative error from rgbif.
- Clarify in the documentation that TRUE/FALSE semantics are consistent
  across all flagging functions: `TRUE` means the record passed the test
  and is eligible for retention.
- Standardize terminology to “specialists’ range information” across
  function documentation.
- Clarify in
  [`bien_here()`](https://wevertonbio.github.io/RuHere/reference/bien_here.md)
  that BIEN range polygons are derived from ecological niche models,
  unlike the taxonomist-curated data from IUCN, WCVP, florabr, and
  faunabr.
- Add authentication notes and updated examples to
  [`request_gbif()`](https://wevertonbio.github.io/RuHere/reference/request_gbif.md),
  [`get_specieslink()`](https://wevertonbio.github.io/RuHere/reference/get_specieslink.md),
  and
  [`iucn_here()`](https://wevertonbio.github.io/RuHere/reference/iucn_here.md).
- Fix incorrect documentation of the `verbose` argument in
  [`flag_wcvp()`](https://wevertonbio.github.io/RuHere/reference/flag_wcvp.md).
- RuHere now depends on **faunabr** (\>= 1.1.1).
- Update vignette for obtaining data: add GBIF citation guidelines and
  instructions for using
  [`get_datapaper()`](https://wevertonbio.github.io/RuHere/reference/get_datapaper.md).
- Add GBIF references in `occurrences`, `occ_gbif`, and `occ_flagged`.
- Fix bug in
  [`flag_bien()`](https://wevertonbio.github.io/RuHere/reference/flag_bien.md)
  when `occ` is a `data.table` object.
- In
  [`bind_here()`](https://wevertonbio.github.io/RuHere/reference/bind_here.md),
  add argument `ignore.attr` to allow binding columns with different
  attributes (e.g. class).
- In
  [`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md),
  force column `eventDate` to be a character.

## RuHere 1.0.1 (February 2026)

CRAN release: 2026-02-17

- Initial CRAN submission.
