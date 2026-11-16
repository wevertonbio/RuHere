#' Identify records outside natural ranges according to BIEN
#'
#' @description
#' Flags occurrence records based on known distribution data
#' from the Botanical Information and Ecology Network (BIEN) data. This function
#' checks if an occurrence point for a given species falls within its documented
#' distribution, allowing for user-defined buffers around the region. Records
#' are flagged as `TRUE` if they fall inside the documented
#' distribution (plus optional buffer) for the species in the BIEN dataset.
#'
#' @param data_dir (character) **Required** directory path where the `BIEN`
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
#' @param buffer (numeric) buffer distance (in kilometers) to be applied
#' around the region of distribution. Default is `10` km.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If `TRUE`, the `'pbapply'` package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `TRUE`.
#'
#' @returns
#' A \code{data.frame} that is the original \code{occ} data frame
#' augmented with a new column named \code{bien_flag}. This column is
#' logical (\code{TRUE}/\code{FALSE}) indicating whether the record falls
#' within the expected distribution (plus buffer) based on the \code{BIEN}
#' data. As with all other flagging functions in RuHere, \code{TRUE}
#' indicates that the record passed this test and is eligible for retention;
#' \code{FALSE} indicates it failed and is flagged as potentially problematic,
#' and can be removed using \code{remove_flagged()}. Records for species not
#' found in the \code{BIEN} data (or with missing coordinates) will have
#' \code{NA} in the \code{bien_flag} column.
#'
#' @importFrom terra vect buffer is.related
#' @importFrom data.table rbindlist
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Filter occurrences for golden trumpet tree
#' occ <- occurrences[occurrences$species == "Handroanthus serratifolius", ]
#' # Set folder where distributional datasets were saved
#' # Here, just a sample provided in the package
#' # You must run 'bien_here()' beforehand to download the necessary data files
#' dataset_dir <- system.file("extdata/datasets", package = "RuHere")
#'
#' # Flag records using BIEN specialist information
#' occ_bien <- flag_bien(data_dir = dataset_dir, occ = occ)
flag_bien <- function(data_dir, occ, species = "species",
                      long = "decimalLongitude", lat = "decimalLatitude",
                      buffer = 10, progress_bar = FALSE, verbose = TRUE) {

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

  bien_dir <- file.path(data_dir, "bien")
  if (!dir.exists(bien_dir)) {
    stop("Data from BIEN necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'bien_here()' function.", call. = FALSE)
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

  # Match species to .gpkg files
  all_spp <- unique(occ[[species]])
  all_spp <- all_spp[!is.na(all_spp) & nzchar(trimws(as.character(all_spp)))]
  names(all_spp) <- gsub(" ", "_", all_spp)

  v_files <- list.files(bien_dir, pattern = "\\.gpkg$")
  spp_files <- sub("\\.gpkg$", "", v_files)
  spp_in <- intersect(names(all_spp), spp_files)
  spp_out <- setdiff(names(all_spp), spp_files)

  if (length(spp_out) > 0L && verbose) {
    warning("Some species present in 'occ' will not be checked due to absence of information in BIEN: ",
            paste(all_spp[spp_out], collapse = ", "), call. = FALSE)
  }

  if (length(spp_in) == 0L) {
    stop("None of the species in 'occ' has information in the BIEN data available.",
         call. = FALSE)
  }

  if (verbose) {
    message("Checking the distribution from ", length(spp_in), " of ",
            length(all_spp), " species")
  }

  spp_in_names <- unname(all_spp[spp_in])
  occ_in <- occ[occ[[species]] %in% spp_in_names, , drop = FALSE]
  occ_split <- split(occ_in, occ_in[[species]])
  buf_width <- buffer * 1000

  res_flag <- my_lapply(spp_in, function(i) {
    sp_name <- unname(all_spp[i])
    occ_i <- occ_split[[sp_name]]
    occ_i$bien_flag <- NA

    spp_path <- file.path(bien_dir, paste0(i, ".gpkg"))
    m_i <- terra::vect(spp_path)

    if (nrow(m_i) == 0L) {
      return(occ_i)
    }

    valid_xy <- !is.na(occ_i[[long]]) & !is.na(occ_i[[lat]])
    if (!any(valid_xy)) {
      return(occ_i)
    }

    # Deduplicate coordinates within species before spatial testing
    sub_xy <- occ_i[valid_xy, c(long, lat), drop = FALSE]
    keys <- paste(sub_xy[[long]], sub_xy[[lat]], sep = "\r")
    uniq_idx <- !duplicated(keys)
    uniq_xy <- sub_xy[uniq_idx, , drop = FALSE]

    pts <- terra::vect(uniq_xy, geom = c(x = long, y = lat), crs = "EPSG:4326")

    # Stage 1: Direct intersection without buffer
    in_poly <- as.logical(terra::is.related(pts, m_i, "intersects"))

    # Stage 2: Apply buffer ONLY if buffer > 0 and some points fell outside
    if (buf_width > 0 && any(!in_poly)) {
      outside_idx <- which(!in_poly)
      m_buf <- terra::buffer(m_i, width = buf_width)
      in_poly[outside_idx] <- as.logical(
        terra::is.related(pts[outside_idx], m_buf, "intersects")
      )
    }

    occ_i$bien_flag[valid_xy] <- in_poly[match(keys, keys[uniq_idx])]
    return(occ_i)
  })

  res_flag <- as.data.frame(data.table::rbindlist(res_flag, fill = TRUE))

  # Append occurrences of spp_out (or rows with NA species)
  spp_out_names <- unname(all_spp[spp_out])
  occ_out <- occ[!(occ[[species]] %in% spp_in_names), , drop = FALSE]
  if (nrow(occ_out) > 0L) {
    occ_out$bien_flag <- NA
    res_flag <- as.data.frame(data.table::rbindlist(list(res_flag, occ_out), fill = TRUE))
  }

  return(res_flag)
}
