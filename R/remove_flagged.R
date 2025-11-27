#' Remove flagged records
#'
#' @description
#' This function removes occurrence records flagged as invalid by one or more
#' flagging functions. Additional manual control is available to force keeping
#' or removing specific records, regardless of their flag values.
#'
#' @param occ (data.frame or data.table) a dataset with occurrence records that
#' has been processed by two or more flagging functions. See details.
#' @param flags (character) a character vector with the names of the flag
#' columns to be used for filtering records. See *details* for the available
#' options. Default is "all".
#' @param additional_flags (character) an optional named character vector with
#' the names of additional logical columns to be used as flags. Default is `NULL`.
#' @param force_keep (character) an optional character vector with the IDs of
#' records that were flagged but should still be kept. Default is `NULL`.
#' @param force_remove (character) an optional character vector with the IDs of
#' records that were not flagged but should still be removed. Default is `NULL`.
#' @param column_id (character) the name of the column containing unique record
#' IDs. Required if `force_keep` or `force_remove` is used. Default is `NULL`.
#' @param save_flagged (logical) whether to save the flagged (removed) records.
#' If `TRUE`, an `output_dir` must be provided. Default is `FALSE`.
#' @param output_dir (character) path to an existing directory where removed
#' flagged records will be saved. Only used when `save_flagged = TRUE`.
#' @param overwrite (logical)  whether to overwrite existing files in
#' `output_dir`. Only used when `save_flagged = TRUE`. Default is `FALSE`.
#' @param output_format (character) output format for saving removed records.
#' Options are `".csv"` or `".gz"`. Only used when `save_flagged = TRUE`.
#' Default is `".gz"`.
#'
#' @details
#' The following flags are available: correct_country, correct_state, cultivated,
#' fossil, inaturalist, faunabr, florabr, wcvp, iucn, duplicated, thin, .val,
#' .equ, .zer, .cap, .cen, .sea, .urb, .otl, .gbf, .inst, and .aohi.
#'
#'
#' @returns
#' A \code{data.frame} containing only the valid (kept) records according to the
#' flags and additional criteria.
#'
#' @importFrom stats setNames
#' @importFrom data.table fwrite
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occ_flagged", package = "RuHere")
#'
#' # Remove all flagged records
#' occ_valid <- remove_flagged(occ = occ_flagged)
#'
#' # Remove flagged records and force removal of some unflagged records
#' to_remove <- c("gbif_5987", "specieslink_2301", "gbif_18761")
#' occ_valid2 <- remove_flagged(occ = occ_flagged,
#'                              force_remove = to_remove)
#'
#' # Remove flagged records but keep some flagged ones
#' to_keep <- c("gbif_14501", "gbif_12002", "gbif_5168")
#' occ_valid3 <- remove_flagged(occ = occ_flagged,
#'                             force_keep = to_keep)
remove_flagged <- function(occ,
                           flags = "all",
                           additional_flags = NULL,
                           force_keep = NULL,
                           force_remove = NULL,
                           column_id = "record_id",
                           save_flagged = FALSE,
                           output_dir = NULL,
                           overwrite = FALSE,
                           output_format = ".gz"){
  ## --- Argument checking ------------------------------------------------------

  # occ must be a data.frame or data.table
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table.", call. = FALSE)
  }

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  # flags must be NULL or a character vector
  if (!is.null(flags) && !is.character(flags)) {
    stop("'flags' must be a character vector.", call. = FALSE)
  }

  # if flags provided, ensure they exist in occ
  if (flags != "all") {
    missing_flags <- flags[!flags %in% names(occ)]
    if (length(missing_flags) > 0) {
      stop(
        "The following flags do not exist in 'occ': ",
        paste(missing_flags, collapse = ", "),
        call. = FALSE
      )
    }
  }

  if(flags == "all"){
    flags <- c("correct_country", "correct_state", "florabr", "faunabr",
               "wcvp", "iucn", "bien", "cultivated", "inaturalist",
               "duplicated", "thin", "consensus",
               # Froom CoordinateCleaner
               ".val", ".equ", ".zer", ".cap", ".cen", ".sea", ".urb", ".otl",
               ".gbf", ".inst", ".aohi")
  }

  # additional_flags must be NULL or a character vector
  if (!is.null(additional_flags) && !is.character(additional_flags)) {
    stop("'additional_flags' must be a character vector.", call. = FALSE)
  }

  # if additional_flags provided, ensure they exist and are logical
  if (!is.null(additional_flags)) {
    if(is.null(names(additional_flags))){
      stop("'additional_flags' must be a named character (i.e., 'user_flag' = 'User flag')")
    }
    missing_extra <- additional_flags[!additional_flags %in% names(occ)]
    if (length(missing_extra) > 0) {
      stop(
        "These additional flag columns do not exist in 'occ': ",
        paste(missing_extra, collapse = ", "),
        call. = FALSE
      )
    }

    # ensure they are logical
    non_logical <- additional_flags[
      !sapply(occ[, additional_flags, drop = FALSE], is.logical)
    ]
    if (length(non_logical) > 0) {
      stop(
        "These additional flag columns are not logical: ",
        paste(non_logical, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # force_keep must be NULL or character
  if (!is.null(force_keep) && !is.character(force_keep)) {
    stop("'force_keep' must be a character vector.", call. = FALSE)
  }

  # force_remove must be NULL or character
  if (!is.null(force_remove) && !is.character(force_remove)) {
    stop("'force_remove' must be a character vector.", call. = FALSE)
  }

  # if forcing keep/remove, column_id is required
  if (( !is.null(force_keep) || !is.null(force_remove) ) && is.null(column_id)) {
    stop(
      "You must specify 'column_id' when using 'force_keep' or 'force_remove'.",
      call. = FALSE
    )
  }

  # check column_id
  if (!is.null(column_id)) {
    if (!is.character(column_id) || length(column_id) != 1) {
      stop("'column_id' must be a single character string.", call. = FALSE)
    }
    if (!column_id %in% names(occ)) {
      stop("'column_id' does not exist in 'occ'.", call. = FALSE)
    }
  }

  # save_flagged must be logical
  if (!is.logical(save_flagged) || length(save_flagged) != 1) {
    stop("'save_flagged' must be TRUE or FALSE.", call. = FALSE)
  }

  # check saving options
  if (save_flagged) {

    # output_dir must exist
    if (is.null(output_dir) || !is.character(output_dir)) {
      stop("You must provide a valid 'output_dir' when 'save_flagged = TRUE'.", call. = FALSE)
    }
    if (!dir.exists(output_dir)) {
      stop("'output_dir' does not exist: ", output_dir, call. = FALSE)
    }

    # check output_format
    if (!output_format %in% c(".csv", ".gz")) {
      stop("'output_format' must be either '.csv' or '.gz'.", call. = FALSE)
    }

    # overwrite must be logical
    if (!is.logical(overwrite) || length(overwrite) != 1) {
      stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
    }
  }




  # Add _flags for some columns
  to_paste <- c("florabr", "faunabr", "wcvp", "iucn", "bien", "cultivated",
                "inaturalist", "duplicated", "thin", "consensus")

  flags[flags %in% to_paste] <- paste0(flags[flags %in% to_paste], "_flag")

  # Subset columns
  flags <- intersect(flags, colnames(occ))

  # Change names of flags
  flag_names <- getExportedValue("RuHere", "flag_names")

  # Additional flags
  if(!is.null(additional_flags)){
    flag_names <- c(flag_names, additional_flags)
  }

  if(!is.null(force_remove)){
    # flag_names <- c(flag_names, "force_remove" = "Forcibly removed")
    # Create columns
    occ$force_remove <- TRUE
    # Get IDs
    occ$force_remove[occ[[column_id]] %in% force_remove] <- FALSE
  }

  # Exceptions...
  if(!is.null(force_keep)){
    occ_keep <- occ[occ[[column_id]] %in% force_keep,]
    occ <- occ[!(occ[[column_id]] %in% force_keep),]
  }

  # Create empty list to save flagged results
  occ_flagged <- list()
  # Identify flags
   for(i in flags){
    occ_flagged[[i]] <- occ[!occ[[i]],]
   }

  # Update flag names
  match_flags <- flag_names[match(names(occ_flagged), names(flag_names))]
  names(occ_flagged) <- match_flags

  # Remove flags with 0 records
  occ_flagged <- occ_flagged[sapply(occ_flagged, function(i) nrow(i) > 0)]


  # Filter
  occ_final <- occ[rowSums(occ[flags]) == length(flags), ]

  if(!is.null(force_keep)){
    if(nrow(occ_keep) > 0){
      occ_final <- rbind(occ_final, occ_keep)
    } else {
      warning("None of the IDs provided in 'force_keep' were found in the dataset.\n",
              "Keeping only unflagged records.")
    }
  }

  if(save_flagged){
    # Update flags
    flags <- names(occ_flagged)

    # Build path to sabe
    p <- lapply(flags, function(i){
      file_i <- file.path(output_dir, paste0(i, output_format))
    })

    #Check if files exists
    if(!overwrite){
      f_exists <- sapply(p, file.exists)
      if(sum(f_exists) > 0){
        stop("Some flagged occurrences already exists in '", output_dir, "'.\n",
             "Delete the file, change the folder or set 'overwrite = TRUE'")
      }
    }
  #Save
    for(y in 1:length(flags)){
      data.table::fwrite(x = occ_flagged[[flags[y]]],
                         p[[y]])
    }
  }
  return(occ_final)
}
