#' Count the number of flags for each record
#'
#' @description
#' This function returns the original data frame with an additional column
#' summarizing the total number of flags triggered by each record.
#'
#'
#' @param occ (data.frame or data.table) a dataset containing occurrence records
#' that has been processed by one or more flagging functions. See *Details* for
#' available flag types.
#' @param flagged_dir (character) optional path to a directory containing files
#' with flagged records saved using the `remove_flagged()` function. Default is
#' `NULL`.
#' @param output_format (character) output format used to read the removed records.
#' Options are `".csv"` or `".gz"`. Only used when `flagged_dir` is not `NULL`.
#' Default is `".gz"`.
#' @param flags (character) the flags to be summarized. Use `"all"` to display
#' all available flags. See *Details* for all options. Default is `"all"`.
#' @param additional_flags (character) an optional named character vector with
#' the names of additional logical columns to be used as flags. Default is `NULL`.
#'
#' @returns
#' The original data frame with an additional column summarizing the total
#' number of flags triggered by each record.
#'
#' @details
#' This function expects an occurrence dataset that has already been processed
#' by one or more flagging routines from **RuHere** or related packages such as
#' **CoordinateCleaner**. Any logical column in `occ` can be used as a flag.
#'
#' The following built-in flag names are recognized:
#'
#' *From RuHere*:
#' `correct_country`, `correct_state`, `cultivated`, `florabr`, `faunabr`,
#' `wcvp`, `iucn`, `bien`, `duplicated`, `thin_geo`, `thin_env`, `consensus`
#'
#' *From CoordinateCleaner* :
#' `.val`, `.equ`, `.zer`, `.cap`, `.cen`, `.sea`, `.urb`, `.otl`, `.gbf`,
#' `.inst`, `.aohi`
#'
#' Users may also supply additional logical columns using
#' `additional_flags`.
#'
#' @importFrom data.table fread rbindlist
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occ_flagged", package = "RuHere")
#'
#' # Count flags
#' sum_flags <- count_flags(occ = occ_flagged)
#'
#' # Check the distribution of flags per record
#' table(sum_flags$total_flags)
#'
#' # Plot histogram
#' hist(sum_flags$total_flags,
#'      main = "Distribution of Flags",
#'      xlab = "Number of Flags",
#'      col = "steelblue",
#'      breaks = seq(-0.5, max(sum_flags$total_flags) + 0.5, by = 1))
count_flags <- function(occ = NULL,
                        flagged_dir = NULL,
                        output_format = ".gz",
                        flags = "all",
                        additional_flags = NULL){

  ## ---- Argument checking ----
  # occ / flagged_dir
  if (is.null(occ) && is.null(flagged_dir)) {
    stop("`occ` or `flagged_dir` must be provided.", call. = FALSE)
  }

  if (!is.null(occ)) {
    if (!inherits(occ, c("data.frame", "data.table"))) {
      stop("`occ` must be a data.frame or data.table.", call. = FALSE)
    }
    # Force occ to be a dataframe
    if(length(class(occ)) > 1)
      occ <- as.data.frame(occ)
  }

  if (!is.null(flagged_dir)) {
    if (!inherits(flagged_dir, "character") || length(flagged_dir) != 1) {
      stop("`flagged_dir` must be a single character string.", call. = FALSE)
    }
    if (!dir.exists(flagged_dir)) {
      stop("`flagged_dir` does not exist.", call. = FALSE)
    }
  }

  # output_format
  if (!inherits(output_format, "character") || length(output_format) != 1) {
    stop("`output_format` must be a single character string.", call. = FALSE)
  }
  if (!output_format %in% c(".csv", ".gz")) {
    stop("`output_format` must be either '.csv' or '.gz'.", call. = FALSE)
  }

  # flags
  if (!inherits(flags, "character")) {
    stop("`flags` must be a character vector.", call. = FALSE)
  }
  if (length(flags) == 0) {
    stop("`flags` must have at least one element.", call. = FALSE)
  }

  # additional_flags
  if (!is.null(additional_flags)) {
    if (!inherits(additional_flags, "character")) {
      stop("`additional_flags` must be a character vector.", call. = FALSE)
    }
    if (is.null(names(additional_flags))) {
      stop("`additional_flags` must be a named character vector.", call. = FALSE)
    }
  }

  # Set flags
  if(all(flags == "all")){
    flags <- c("correct_country", "correct_state", "florabr", "faunabr",
               "wcvp", "iucn", "bien", "cultivated", "inaturalist",
               "duplicated", "thin_env", "thin_geo", "consensus",
               "fossil", "year", "invalid_coordinates",
               # From CoordinateCleaner
               ".val", ".equ", ".zer", ".cap", ".cen", ".sea", ".urb", ".otl",
               ".gbf", ".inst", ".aohi")
  }

  # Add _flags for some columns
  to_paste <- c("florabr", "faunabr", "wcvp", "iucn", "bien", "cultivated",
                "inaturalist", "duplicated", "thin_env", "thin_geo",
                "consensus", "fossil", "year")

  flags[flags %in% to_paste] <- paste0(flags[flags %in% to_paste], "_flag")

  # Additional flags
  if(!is.null(additional_flags)){
    flags <- c(flags, additional_flags)
  }

  # Get records from directory
  if(is.null(occ) & !is.null(flagged_dir)){
    occ_files <- list.files(flagged_dir, pattern = paste0(output_format, "$"),
                            full.names = TRUE)
    if(length(occ_files) == 0){
      stop("There are no files with the ", output_format, " format in the specified 'flagged_dir' directory")
    }

    # Reas files
    occ_list <- lapply(occ_files, function(x) data.table::fread(x, data.table = FALSE))
    occ <- data.table::rbindlist(occ_list, fill = TRUE)
    occ <- as.data.frame(occ)
  }

  ##### Count flags ####

  # Subset flags
  existing_flags <- intersect(flags, colnames(occ))

  if(length(existing_flags) == 0) {
    warning("None of the specified flag columns were found in 'occ'.")
    return(occ)
  }

  # SUbset flags
  flag_matrix <- occ[, existing_flags, drop = FALSE]

  # Invert TRUE/FALSe to count
  # Set NA as FALSE
  flag_matrix_inverted <- !flag_matrix
  flag_matrix_inverted[is.na(flag_matrix_inverted)] <- FALSE

  # Sum number of flags
  occ$total_flags <- rowSums(flag_matrix_inverted)
  return(occ)
}
