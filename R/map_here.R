#' Interactive Visualization of Occurrence Flags with mapview
#'
#' @description
#' This function creates an interactive map of occurrence records using
#' **mapview**, visually highlighting flags. This tool helps users explore which
#' records were flagged by one or more validation functions and inspect them
#' directly on the map.
#'
#' @param occ (data.frame or data.table) a dataset containing occurrence records
#' that has been processed by one or more flagging functions. See *Details* for
#' available flag types.
#' @param species (character) name(s) of the species to subset and plot. Default
#' is `NULL`, meaning that all records for all species are plotted.
#' @param long (character) the name of the column in `occ` that contains the
#' longitude values. Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column in `occ` that contains the
#' latitude values. Default is `"decimalLatitude"`.
#' @param flags (character) the flags to be used for coloring the records. Use
#' `"all"` to display all available flags. See *Details* for all options.
#' Default is `"all"`.
#' @param additional_flags (character) an optional character vector with the
#' names of additional logical columns in `occ` to be used as flags. Default is
#' `NULL`.
#' @param names_additional_flags (character) display names for the flags
#' provided in `additional_flags` to be shown in the map legend. Required when
#' `additional_flags` is not `NULL` and must have the same length as
#' `additional_flags`. Default is `NULL`.
#' @param col_additional_flags (character) colors for the records flagged in
#' `additional_flags`. Required when `additional_flags` is not `NULL` and must
#' have the same length as `additional_flags`. Default is `NULL`.
#' @param show_no_flagged (logical) whether to display records that did not
#' receive any flag. Default is `TRUE`.
#' @param cex (numeric) point size for plotting occurrences. Default is `6`.
#' @param lwd (numeric) line width for point borders. Default is `2`.
#' @param col_points (character) a **named vector** assigning colors to each
#' flag. If `NULL`, default colors from the internal object
#' `RuHere::flag_colors` are used.
#' @param label (character) column name in `occ` to use as the mouseover label.
#' Default is `NULL`, in which case the record's row number is displayed.
#' @param map.types (character) background map layers to display. Defaults to
#' `c("OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap", "CartoDB.Positron", "CartoDB.DarkMatter")`.
#' See `?mapview::mapview` for additional options.
#' @param ... additional arguments passed to `mapview::mapview()`.
#'
#' @details
#' The following built-in flags are recognized when `flags = "all"` (or can be
#' passed individually without the `_flag` suffix): `correct_country`,
#' `correct_state`, `cultivated`, `fossil`, `inaturalist`, `faunabr`,
#' `florabr`, `wcvp`, `iucn`, `bien`, `duplicated`, `thin_geo`, `thin_env`,
#' `consensus`, `year`, as well as `CoordinateCleaner` flags (`.val`, `.equ`,
#' `.zer`, `.cap`, `.cen`, `.sea`, `.urb`, `.otl`, `.gbf`, `.inst`, and
#' `.aohi`).
#'
#' Users may also supply additional logical columns using `additional_flags`,
#' providing corresponding display names (`names_additional_flags`) and colors
#' (`col_additional_flags`).
#'
#' @returns
#' An interactive `mapview` object displaying flagged and optionally unflagged
#' occurrence records.
#'
#' @export
#' @importFrom mapview mapview
#'
#' @examples
#' \donttest{
#' # Load example data
#' data("occ_flagged", package = "RuHere")
#' # Visualize flags interactively
#' map_here(occ = occ_flagged, label = "record_id")
#' }
map_here <- function(occ,
                     species = NULL,
                     long = "decimalLongitude",
                     lat = "decimalLatitude",
                     flags = "all",
                     additional_flags = NULL,
                     names_additional_flags = NULL,
                     col_additional_flags = NULL,
                     show_no_flagged = TRUE,
                     cex = 6,
                     lwd = 2,
                     col_points = NULL,
                     label = NULL,
                     map.types = c("OpenStreetMap",
                                   "Esri.WorldImagery", "OpenTopoMap",
                                   "CartoDB.Positron", "CartoDB.DarkMatter"),
                     ...) {

  # ---- ARGUMENT CHECKING ----
  is_single_str <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  is_single_bool <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }

  # 1. Check occ
  if (missing(occ) || is.null(occ)) {
    stop("'occ' must be provided (cannot be NULL or missing).", call. = FALSE)
  }
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("`occ` must be a data.frame or data.table.", call. = FALSE)
  }
  if (nrow(occ) == 0L) {
    stop("`occ` is empty. Please provide a dataset with occurrence records.",
         call. = FALSE)
  }

  # Force occ to be a data.frame
  if (inherits(occ, "data.table") || inherits(occ, "spatialvalid")) {
    occ <- as.data.frame(occ)
  }

  # Ensure case-insensitive unique column names to prevent mapview/sf popup errors
  if (anyDuplicated(tolower(names(occ))) > 0L) {
    dup_lower <- duplicated(tolower(names(occ)))
    names(occ)[dup_lower] <- make.unique(tolower(names(occ)))[dup_lower]
  }

  # 2. Check long & lat
  if (!is_single_str(long) || !long %in% names(occ)) {
    stop("The column specified in `long` ('", long, "') was not found in `occ`.",
         call. = FALSE)
  }
  if (!is_single_str(lat) || !lat %in% names(occ)) {
    stop("The column specified in `lat` ('", lat, "') was not found in `occ`.",
         call. = FALSE)
  }

  # 3. Check flags
  if (!is.character(flags) || length(flags) == 0L || any(is.na(flags))) {
    stop("`flags` must be a non-empty character vector without NAs.", call. = FALSE)
  }

  # 4. Check species
  if (!is.null(species)) {
    if (!is.character(species) || length(species) == 0L || any(is.na(species))) {
      stop("`species` must be a non-empty character vector or NULL.", call. = FALSE)
    }
    if (!"species" %in% names(occ)) {
      stop("Column 'species' was not found in `occ`.", call. = FALSE)
    }
    if (!all(species %in% unique(occ$species))) {
      missing_sp <- setdiff(species, unique(occ$species))
      stop("The following species provided in `species` are not present in `occ`: ",
           paste(missing_sp, collapse = ", "), call. = FALSE)
    }
    occ <- occ[occ$species %in% species, , drop = FALSE]
  }

  # 5. Check additional_flags & related arguments
  if (!is.null(additional_flags)) {
    if (!is.character(additional_flags) || length(additional_flags) == 0L || any(is.na(additional_flags))) {
      stop("`additional_flags` must be a non-empty character vector or NULL.", call. = FALSE)
    }

    missing_extra <- setdiff(additional_flags, names(occ))
    if (length(missing_extra) > 0L) {
      stop("These additional flag columns do not exist in `occ`: ",
           paste(missing_extra, collapse = ", "), call. = FALSE)
    }

    non_logical <- additional_flags[
      !vapply(occ[, additional_flags, drop = FALSE], is.logical, logical(1L))
    ]
    if (length(non_logical) > 0L) {
      stop("These additional flag columns are not logical: ",
           paste(non_logical, collapse = ", "), call. = FALSE)
    }

    if (is.null(names_additional_flags) || !is.character(names_additional_flags) ||
        length(names_additional_flags) != length(additional_flags)) {
      stop("`names_additional_flags` must be a character vector with the same length as `additional_flags`.",
           call. = FALSE)
    }

    if (is.null(col_additional_flags) || !is.character(col_additional_flags) ||
        length(col_additional_flags) != length(additional_flags)) {
      stop("`col_additional_flags` must be a character vector with the same length as `additional_flags`.",
           call. = FALSE)
    }
  }

  # 6. Check visual & label parameters
  if (!is_single_bool(show_no_flagged)) {
    stop("`show_no_flagged` must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (!is.numeric(cex) || length(cex) != 1L || is.na(cex) || cex <= 0) {
    stop("`cex` must be a single positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(lwd) || length(lwd) != 1L || is.na(lwd) || lwd <= 0) {
    stop("`lwd` must be a single positive numeric value.", call. = FALSE)
  }

  if (!is.null(col_points)) {
    if (!is.character(col_points) || is.null(names(col_points))) {
      stop("`col_points` must be a named character vector of colors.", call. = FALSE)
    }
  }

  if (!is.null(label)) {
    if (!is_single_str(label)) {
      stop("`label` must be a single character string.", call. = FALSE)
    }
    if (!label %in% names(occ)) {
      stop("The column specified in `label` ('", label, "') does not exist in `occ`.",
           call. = FALSE)
    }
  }

  # ---- PREPARE FLAGS & LAYERS ----
  if (all(flags == "all")) {
    flags <- c("correct_country", "correct_state", "florabr", "faunabr",
               "wcvp", "iucn", "bien", "cultivated", "inaturalist",
               "duplicated", "thin_env", "thin_geo", "consensus",
               "fossil", "year",
               # From CoordinateCleaner
               ".val", ".equ", ".zer", ".cap", ".cen", ".sea", ".urb", ".otl",
               ".gbf", ".inst", ".aohi")
  }

  # Add _flag suffix for applicable columns
  to_paste <- c("florabr", "faunabr", "wcvp", "iucn", "bien", "cultivated",
                "inaturalist", "duplicated", "thin_env", "thin_geo",
                "consensus", "fossil", "year")
  flags[flags %in% to_paste] <- paste0(flags[flags %in% to_paste], "_flag")

  if (!is.null(additional_flags)) {
    flags <- unique(c(flags, additional_flags))
  }

  # Subset existing flag columns
  flags <- intersect(flags, colnames(occ))

  # Remove flag columns that are all NA
  if (length(flags) > 0L) {
    all_na_flags <- flags[colSums(is.na(occ[, flags, drop = FALSE])) == nrow(occ)]
    if (length(all_na_flags) > 0L) {
      occ <- occ[, !(names(occ) %in% all_na_flags), drop = FALSE]
      flags <- setdiff(flags, all_na_flags)
    }
  }

  # Display names for flags
  flag_names <- getExportedValue("RuHere", "flag_names")
  if (!is.null(additional_flags)) {
    names(names_additional_flags) <- additional_flags
    flag_names <- c(flag_names, names_additional_flags)
  }

  # Split points by flag
  occ_list <- lapply(flags, function(i) {
    occ[!occ[[i]] & !is.na(occ[[i]]), , drop = FALSE]
  })
  names(occ_list) <- ifelse(is.na(flag_names[flags]), flags, flag_names[flags])

  # Remove flag layers with 0 records
  occ_list <- occ_list[vapply(occ_list, function(x) nrow(x) > 0L, logical(1L))]

  # Add unflagged occurrences if requested
  if (show_no_flagged) {
    if (length(flags) > 0L) {
      no_flagged <- rowSums(occ[, flags, drop = FALSE] |
                              is.na(occ[, flags, drop = FALSE]), na.rm = TRUE) == length(flags)
      occ_list[["No flagged"]] <- occ[no_flagged, , drop = FALSE]
    } else {
      occ_list[["No flagged"]] <- occ
    }
    occ_list <- occ_list[vapply(occ_list, function(x) nrow(x) > 0L, logical(1L))]
  }

  if (length(occ_list) == 0L) {
    stop("No occurrence records available to plot.", call. = FALSE)
  }

  # Spatialize non-empty layers and filter out layers with 0 valid geometries
  occ_list <- lapply(occ_list, spatialize, long = long, lat = lat)
  occ_list <- occ_list[vapply(occ_list, function(x) length(x) > 0L, logical(1L))]

  if (length(occ_list) == 0L) {
    stop("No valid occurrence coordinates available to plot.", call. = FALSE)
  }

  # Update active layer names after spatialization
  active_layers <- names(occ_list)

  # Colors
  if (is.null(col_points)) {
    col_points <- getExportedValue("RuHere", "flag_colors")
  }
  if (!is.null(additional_flags)) {
    col_additional <- col_additional_flags
    names(col_additional) <- names_additional_flags
    col_points <- c(col_points, col_additional)
  }

  # Plot first map layer
  first_map <- active_layers[1L]
  mapa <- mapview::mapview(occ_list[[first_map]],
                           col.regions = col_points[[first_map]],
                           cex = cex, lwd = lwd,
                           layer.name = first_map,
                           label = label,
                           map.types = map.types,
                           ...)

  # Add remaining map layers
  if (length(active_layers) > 1L) {
    for (i in 2:length(active_layers)) {
      flag_i <- active_layers[i]
      mapa <- mapa + mapview::mapview(occ_list[[flag_i]],
                                      col.regions = col_points[[flag_i]],
                                      cex = cex, lwd = lwd,
                                      layer.name = flag_i,
                                      label = label, ...)
    }
  }

  return(mapa)
}
