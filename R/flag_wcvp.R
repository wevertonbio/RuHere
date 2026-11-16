#' Identify records outside natural ranges according to the World Checklist of Vascular Plants
#'
#' @description
#' Flags occurrence records based on known distribution data
#' from the World Checklist of Vascular Plants (WCVP) data. This function checks
#' if an occurrence point for a given species falls within its documented
#' distribution, allowing for user-defined buffers around the region. Records
#' are flagged as `TRUE` if they fall inside the documented
#' distribution (plus optional buffer) for the species in the WCVP dataset.
#'
#' @param data_dir (character) **Required** directory path where the `WCVP`
#' data is saved.
#' @param occ (data.frame or data.table) a data frame containing the occurrence
#' records to be flagged. Must contain columns for species, longitude, and
#' latitude.
#' @param species (character) the name of the column in `occ` that contains the
#' species scientific names. Default is `"species"`.
#' @param long (character) the name of the column in `occ` that contains the
#' longitude values. Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column in `occ` that contains the
#' latitude values. Default is `"decimalLatitude"`.
#' @param origin (character) vector specifying which origin categories should
#' be considered as part of the species' range. Options are: `"native"`,
#' `"introduced"`, `"extinct"`, `"location_doubtful"`, and `"all"`. Default is
#' `"native"`.
#' @param buffer (numeric) buffer distance (in kilometers) to be applied
#' around the region of distribution. Default is `20` km.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If `TRUE`, the `'pbapply'` package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `FALSE`.
#'
#' @returns
#' A \code{data.frame} that is the original \code{occ} data frame
#' augmented with a new column named \code{wcvp_flag}. This column is
#' logical (\code{TRUE}/\code{FALSE}) indicating whether the record falls
#' within the expected distribution (plus buffer) based on the \code{WCVP}
#' data. As with all other flagging functions in RuHere, \code{TRUE}
#' indicates that the record passed this test and is eligible for retention;
#' \code{FALSE} indicates it failed and is flagged as potentially problematic,
#' and can be removed using \code{remove_flagged()}. Records for species not
#' found in the \code{WCVP} data (or lacking distribution polygons for the
#' selected filters) will have \code{NA} in the \code{wcvp_flag} column.
#'
#' @importFrom data.table fread rbindlist
#' @importFrom terra vect aggregate buffer is.related
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Filter occurrences for Araucaria
#' occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
#' # Set folder where distributional datasets were saved
#' # Here, just a sample provided in the package
#' # You must run 'wcvp_here()' beforehand to download the necessary data files
#' dataset_dir <- system.file("extdata/datasets", package = "RuHere")
#'
#' # Flag records using WCVP specialist information
#' occ_wcvp <- flag_wcvp(data_dir = dataset_dir, occ = occ)
flag_wcvp <- function(data_dir, occ, species = "species",
                      long = "decimalLongitude", lat = "decimalLatitude",
                      origin = "native",
                      buffer = 20, progress_bar = FALSE, verbose = FALSE) {

  # ---- ARGUMENT CHECKING ----
  is_single_str <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  is_single_bool <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }

  # data_dir
  if (missing(data_dir) || is.null(data_dir) || !is_single_str(data_dir)) {
    stop("'data_dir' must be a single non-empty character string.", call. = FALSE)
  }
  if (!dir.exists(data_dir)) {
    stop("The directory provided in 'data_dir' does not exist.", call. = FALSE)
  }

  # occ
  if (missing(occ) || is.null(occ)) {
    stop("'occ' must be specified (must not be NULL or missing).", call. = FALSE)
  }
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table.", call. = FALSE)
  }
  if (nrow(occ) == 0L) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.",
         call. = FALSE)
  }
  if (inherits(occ, "data.table")) {
    occ <- as.data.frame(occ)
  }

  # species, long, lat
  for (arg in c("species", "long", "lat")) {
    value <- get(arg)
    if (!is_single_str(value)) {
      stop(sprintf("'%s' must be a single character string.", arg), call. = FALSE)
    }
    if (!value %in% names(occ)) {
      stop(sprintf("Column '%s' specified in '%s' is not present in 'occ'.", value, arg),
           call. = FALSE)
    }
  }
  if (!is.numeric(occ[[long]]) || !is.numeric(occ[[lat]])) {
    stop("'long' and 'lat' columns must be numeric.", call. = FALSE)
  }

  # origin (including "all" as documented)
  valid_origins <- c("native", "introduced", "extinct", "location_doubtful", "all")
  if (!is.character(origin) || length(origin) == 0L || any(is.na(origin))) {
    stop("'origin' must be a non-empty character vector without NAs.", call. = FALSE)
  }
  origin_low <- tolower(origin)
  if (!all(origin_low %in% valid_origins)) {
    stop(sprintf("'origin' contains invalid values. Allowed options are: %s.",
                 paste(valid_origins, collapse = ", ")), call. = FALSE)
  }

  # buffer
  if (!is.numeric(buffer) || length(buffer) != 1L || is.na(buffer) || buffer < 0) {
    stop("'buffer' must be a single non-negative numeric value.", call. = FALSE)
  }

  # verbose & progress_bar
  if (!is_single_bool(verbose)) {
    stop("'verbose' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (!is_single_bool(progress_bar)) {
    stop("'progress_bar' must be a single logical value (TRUE or FALSE).", call. = FALSE)
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

  # Check required files
  wcvp_file <- file.path(data_dir, "wcvp/wcvp.gz")
  wgsrpd_file <- file.path(data_dir, "wgsrpd/wgsrpd.gpkg")
  if (!file.exists(wcvp_file)) {
    stop("Data from WCVP necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'wcvp_here()' function.", call. = FALSE)
  }
  if (!file.exists(wgsrpd_file)) {
    stop("Data from WGSRPD necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'wcvp_here()' function.", call. = FALSE)
  }

  # Import data
  d <- data.table::fread(wcvp_file, data.table = FALSE)
  m <- terra::vect(wgsrpd_file)

  # Match species
  unique_occ_spp <- unique(occ[[species]])
  spp_in <- intersect(unique_occ_spp, unique(d$species))
  spp_out <- setdiff(unique_occ_spp, spp_in)

  if (length(spp_out) > 0L && verbose) {
    warning("Some species present in 'occ' will not be checked due to absence of information in WCVP: ",
            paste(spp_out, collapse = ", "), call. = FALSE)
  }

  if (length(spp_in) == 0L) {
    stop("None of the species in 'occ' has information in the WCVP data available.",
         call. = FALSE)
  }

  if (verbose) {
    message("Checking the distribution from ", length(spp_in), " of ",
            length(unique_occ_spp), " species")
  }

  # Pre-filter WCVP catalogue by species and origin once
  d_sub <- d[d$species %in% spp_in, , drop = FALSE]
  if (!("all" %in% origin_low)) {
    keep_row <- rep(FALSE, nrow(d_sub))
    if ("native" %in% origin_low) {
      keep_row <- keep_row | (d_sub$introduced == 0 & d_sub$extinct == 0 & d_sub$location_doubtful == 0)
    }
    if ("introduced" %in% origin_low) {
      keep_row <- keep_row | (d_sub$introduced == 1)
    }
    if ("extinct" %in% origin_low) {
      keep_row <- keep_row | (d_sub$extinct == 1)
    }
    if ("location_doubtful" %in% origin_low) {
      keep_row <- keep_row | (d_sub$location_doubtful == 1)
    }
    d_sub <- d_sub[keep_row, , drop = FALSE]
  }
  d_split <- split(d_sub, d_sub$species)

  occ_in <- occ[occ[[species]] %in% spp_in, , drop = FALSE]
  occ_split <- split(occ_in, occ_in[[species]])
  buf_width <- buffer * 1000

  res_flag <- my_lapply(spp_in, function(i) {
    occ_i <- occ_split[[i]]
    occ_i$wcvp_flag <- NA
    d_i <- d_split[[i]]

    if (is.null(d_i) || nrow(d_i) == 0L) {
      return(occ_i)
    }

    codes <- d_i$LEVEL3_COD[!is.na(d_i$LEVEL3_COD) & nzchar(trimws(d_i$LEVEL3_COD))]
    if (length(codes) == 0L) {
      return(occ_i)
    }

    m_i <- m[m$Level3_cod %in% codes]
    if (nrow(m_i) == 0L) {
      return(occ_i)
    }
    m_i <- terra::aggregate(m_i)

    valid_xy <- !is.na(occ_i[[long]]) & !is.na(occ_i[[lat]])
    if (!any(valid_xy)) {
      return(occ_i)
    }

    # Deduplicate coordinates within species
    sub_xy <- occ_i[valid_xy, c(long, lat), drop = FALSE]
    keys <- paste(sub_xy[[long]], sub_xy[[lat]], sep = "\r")
    uniq_idx <- !duplicated(keys)
    uniq_xy <- sub_xy[uniq_idx, , drop = FALSE]

    pts <- terra::vect(uniq_xy, geom = c(x = long, y = lat), crs = "EPSG:4326")

    # Stage 1: Direct intersection
    in_poly <- as.logical(terra::is.related(pts, m_i, "intersects"))

    # Stage 2: Apply buffer ONLY when buffer > 0 and points fell outside
    if (buf_width > 0 && any(!in_poly)) {
      outside_idx <- which(!in_poly)
      m_buf <- terra::buffer(m_i, width = buf_width)
      in_poly[outside_idx] <- as.logical(
        terra::is.related(pts[outside_idx], m_buf, "intersects")
      )
    }

    occ_i$wcvp_flag[valid_xy] <- in_poly[match(keys, keys[uniq_idx])]
    return(occ_i)
  })

  res_flag <- as.data.frame(data.table::rbindlist(res_flag, fill = TRUE))

  if (length(spp_out) > 0L) {
    occ_out <- occ[occ[[species]] %in% spp_out, , drop = FALSE]
    if (nrow(occ_out) > 0L) {
      occ_out$wcvp_flag <- NA
      res_flag <- as.data.frame(data.table::rbindlist(list(res_flag, occ_out), fill = TRUE))
    }
  }

  return(res_flag)
}
