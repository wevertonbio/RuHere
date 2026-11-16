#' Check if the records fall in the country assigned in the metadata
#'
#' @param occ (data.frame or data.table) a dataset with occurrence records,
#' preferably with country information standardized using
#' `standardize_countries()`.
#' @param long (character) column name with longitude. Default is
#' `'decimalLongitude'`.
#' @param lat (character) column name with latitude. Default is
#' `'decimalLatitude'`.
#' @param country_column (character) column name containing the country
#' information.
#' @param distance (numeric) maximum distance (in kilometers) a record can fall
#' outside the country assigned in the `country_column`. Default is `5`.
#' @param try_to_fix (logical) whether to check if coordinates are inverted or
#' transposed (see `fix_countries()` for details). If `TRUE`, coordinates
#' identified as inverted or transposed will be corrected. Default is `FALSE`.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If `TRUE`, the `'pbapply'` package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) whether to print messages about function progress.
#' Default is `TRUE`.
#'
#' @returns
#' The original `occ` object (`data.frame` or `data.table`) with an additional
#' column (`correct_country`) indicating whether each record falls within the
#' country specified in the metadata (`TRUE`) or not (`FALSE`). As with all
#' other flagging functions in RuHere, `TRUE` indicates that the record passed
#' this test and is eligible for retention; `FALSE` indicates it failed and is
#' flagged as potentially problematic, and can be removed using
#' `remove_flagged()`.
#'
#' @importFrom terra vect aggregate buffer is.related unwrap
#' @importFrom data.table is.data.table as.data.table copy set setcolorder :=
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Standardize country names
#' occ_country <- standardize_countries(occ = occurrences,
#'                                      return_dictionary = FALSE)
#' # Check whether records fall within assigned countries
#' occ_country_checked <- check_countries(occ = occ_country,
#'                                        country_column = "country_suggested")
check_countries <- function(occ,
                            long = "decimalLongitude",
                            lat = "decimalLatitude",
                            country_column,
                            distance = 5,
                            try_to_fix = FALSE,
                            progress_bar = FALSE,
                            verbose = TRUE) {
  # ---- ARGUMENT CHECKING ----
  is_single_str <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  is_single_bool <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }

  # 1. Check occ
  if (missing(occ) || !inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table containing occurrence records.",
         call. = FALSE)
  }
  if (nrow(occ) == 0L) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.",
         call. = FALSE)
  }

  # 2. Check long and lat
  if (!is_single_str(long) || !long %in% names(occ)) {
    stop(paste0("The longitude column '", long, "' was not found in 'occ'."),
         call. = FALSE)
  }
  if (!is_single_str(lat) || !lat %in% names(occ)) {
    stop(paste0("The latitude column '", lat, "' was not found in 'occ'."),
         call. = FALSE)
  }
  if (!is.numeric(occ[[long]]) || !is.numeric(occ[[lat]])) {
    stop("'long' and 'lat' columns must be numeric.", call. = FALSE)
  }

  # 3. Check country_column
  if (missing(country_column) || !is_single_str(country_column)) {
    stop("'country_column' must be a single character string with the name of the country column.",
         call. = FALSE)
  }
  if (!country_column %in% names(occ)) {
    stop(paste0("The country column '", country_column, "' was not found in 'occ'."),
         call. = FALSE)
  }

  # 4. Check distance
  if (!is.numeric(distance) || length(distance) != 1L || is.na(distance) || distance < 0) {
    stop("'distance' must be a single non-negative numeric value (in km).",
         call. = FALSE)
  }

  # 5. Check logical flags
  if (!is_single_bool(try_to_fix)) {
    stop("'try_to_fix' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }
  if (!is_single_bool(progress_bar)) {
    stop("'progress_bar' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }
  if (!is_single_bool(verbose)) {
    stop("'verbose' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }

  if (progress_bar) {
    if (requireNamespace("pbapply", quietly = TRUE)) {
      my_lapply <- pbapply::pblapply
    } else {
      stop("Package 'pbapply' is required if 'progress_bar = TRUE'. Run install.packages('pbapply')",
           call. = FALSE)
    }
  } else {
    my_lapply <- base::lapply
  }

  # Preserve input class while avoiding redundant copies
  was_dt <- data.table::is.data.table(occ)
  DT <- if (was_dt) data.table::copy(occ) else data.table::as.data.table(occ)

  # Get unique non-NA countries
  countries <- unique(DT[[country_column]])
  countries <- countries[!is.na(countries) & nzchar(trimws(as.character(countries)))]

  # Get shapefile with countries
  country_shp <- terra::unwrap(getExportedValue("RuHere", "world"))
  countries_in <- intersect(countries, country_shp$name)

  if (length(countries_in) == 0L) {
    stop("None of the countries listed in the '", country_column,
         "' column were found in the world map used for validation. Please check the column or run the 'standardize_countries()' function.",
         call. = FALSE)
  }

  if (length(countries_in) != length(countries) && verbose) {
    countries_out <- setdiff(countries, countries_in)
    warning("The following countries listed in the '", country_column,
            "' column were absent in the world map used for validation: ",
            paste(countries_out, collapse = ", "),
            call. = FALSE)
  }

  # Filter valid coordinates and deduplicate via data.table radix grouping
  valid_idx <- which(!is.na(DT[[long]]) & !is.na(DT[[lat]]) &
                       DT[[country_column]] %in% countries_in)

  res_full <- rep(NA, nrow(DT))

  if (length(valid_idx) > 0L) {
    sub_dt <- DT[valid_idx, c(long, lat, country_column), with = FALSE]
    data.table::set(sub_dt, j = "..orig_idx..", value = valid_idx)

    # Group identical (long, lat, country) tuples in C
    groups <- sub_dt[, list(..rows.. = list(.SD[["..orig_idx.."]])),
                     by = c(long, lat, country_column)]

    # Spatialize unique valid points
    dc <- terra::vect(as.data.frame(groups[, c(long, lat), with = FALSE]),
                      geom = c(x = long, y = lat),
                      crs = "EPSG:4326")

    if (verbose) {
      message("Testing countries...")
    }

    idx_by_country <- split(seq_len(nrow(groups)), groups[[country_column]])
    buf_width <- distance * 1000

    res_list <- my_lapply(countries_in, function(i) {
      occ_i <- idx_by_country[[i]]
      xy_i <- dc[occ_i]
      country_i <- country_shp[country_shp$name == i]

      # Stage 1: Fast direct intersection without buffer
      in_poly <- as.logical(terra::is.related(xy_i, country_i, "intersects"))

      # Stage 2: Apply buffer ONLY if distance > 0 and some points fell outside
      if (buf_width > 0 && any(!in_poly)) {
        outside_idx <- which(!in_poly)
        country_buf <- terra::aggregate(terra::buffer(country_i, width = buf_width))
        in_poly[outside_idx] <- as.logical(
          terra::is.related(xy_i[outside_idx], country_buf, "intersects")
        )
      }

      in_poly
    })

    unique_res <- rep(NA, nrow(groups))
    for (k in seq_along(countries_in)) {
      unique_res[idx_by_country[[countries_in[k]]]] <- res_list[[k]]
    }

    # Expand results back to original row indices
    res_full[unlist(groups[["..rows.."]], use.names = FALSE)] <-
      rep(unique_res, lengths(groups[["..rows.."]]))
  }

  # Insert 'correct_country' right after 'country_column'
  orig_names <- setdiff(names(DT), "correct_country")
  DT[, ("correct_country") := res_full]
  pos <- match(country_column, orig_names)
  new_order <- append(orig_names, "correct_country", after = pos)
  data.table::setcolorder(DT, new_order)

  if (verbose) {
    message(sum(!DT[["correct_country"]], na.rm = TRUE), " records fall in wrong countries")
  }

  out <- if (was_dt) DT else as.data.frame(DT)

  if (try_to_fix) {
    out <- fix_countries(occ = out,
                         long = long,
                         lat = lat,
                         country_column = country_column,
                         correct_country = "correct_country",
                         distance = distance,
                         progress_bar = progress_bar,
                         verbose = verbose)
    out$correct_country[!is.na(out$country_issues) & out$country_issues != "incorrect"] <- TRUE
  }

  return(out)
}
