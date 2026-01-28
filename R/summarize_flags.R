#' Summarize flags
#'
#' @description
#' This functions returns a dataframe and a bar plot summarizing the number of
#' records flagged by each flagging function.
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
#' @param names_additional_flags (character) an optional different name to the
#' flag provided in `additional_flags` to be shown in the map. Only applicable
#' if `additional_flags` is not NULL.
#' @param plot (logical) whether to return a `ggplot2` bar plot showing the
#' number of flagged records. Default is `TRUE`.
#' @param show_no_flagged (logical) whether to include the number of unflagged
#' records in the plot. Default is `TRUE`.
#' @param fill (character) fill color for the bar plot. Default is `"#0072B2"`.
#' @param sort (logical) whether to sort bars according to the number of records.
#' Default is `TRUE`.
#' @param decreasing (logical) whether to sort bars in decreasing order (flags
#' with more records appear at the top of the plot). Default is `TRUE`.
#' @param add_n (logical) whether to display the number of flagged records on
#' the bars. Default is `TRUE`.
#' @param size_n (numeric) size of the text showing the number of records. Only
#' used when `add_n = TRUE`. Default is `3.5`.
#' @param theme_plot (theme) a `ggplot2` theme object. Default is
#' `ggplot2::theme_minimal()`.
#' @param ... additional arguments passed to `ggplot2::theme()`.
#'
#' @returns
#' If `plot = TRUE`, a list with two elements:
#' \describe{
#'   \item{df_summary}{A data frame summarizing the number of records per flag.}
#'   \item{plot_summary}{A `ggplot2` object showing the summary as a bar plot.}
#' }
#' If `plot = FALSE`, only the summary data frame is returned.
#'
#' @details
#' This function expects an occurrence dataset that has already been processed
#' by one or more flagging routines from **RuHere** or related packages such as
#' **CoordinateCleaner**. Any logical column in `occ` can be used as a flag.
#'
#' The following built-in flag names are recognized:
#' *From RuHere*:
#' `correct_country`, `correct_state`, `cultivated`, `florabr`, `faunabr`,
#' `wcvp`, `iucn`, `bien`, `duplicated`, `thin_geo`, `thin_env`, `consensus`
#'
#' *From CoordinateCleaner*:
#' `.val`, `.equ`, `.zer`, `.cap`, `.cen`, `.sea`, `.urb`, `.otl`, `.gbf`,
#' `.inst`, `.aohi`
#'
#' Users may also supply additional logical columns using
#' `additional_flags`, optionally providing alternative display names
#' (`names_additional_flags`) and colors (`col_additional_flags`).
#'
#' @importFrom data.table fread rbindlist
#' @importFrom ggplot2 theme_minimal ggplot geom_bar xlab ylab aes coord_flip
#' theme geom_text scale_y_continuous expansion
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occ_flagged", package = "RuHere")
#' # Summarize flags
#' sum_flags <- summarize_flags(occ = occ_flagged)
#' # Plot
#' sum_flags$plot_summary
summarize_flags <- function(occ = NULL,
                            flagged_dir = NULL,
                            output_format = ".gz",
                            flags = "all",
                            additional_flags = NULL,
                            names_additional_flags = NULL,
                            plot = TRUE,
                            show_no_flagged = TRUE,
                            fill = "#0072B2",
                            sort = TRUE,
                            decreasing = TRUE,
                            add_n = TRUE,
                            size_n = 3.5,
                            theme_plot = ggplot2::theme_minimal(),
                            ...){

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

  # names_additional_flags
  if (!is.null(names_additional_flags)) {
    if (is.null(additional_flags)) {
      stop(
        "`names_additional_flags` can only be used when `additional_flags` is provided.",
        call. = FALSE
      )
    }
    if (!inherits(names_additional_flags, "character")) {
      stop("`names_additional_flags` must be a character vector.", call. = FALSE)
    }
  }

  # logical arguments
  logical_args <- list(
    plot = plot,
    show_no_flagged = show_no_flagged,
    sort = sort,
    decreasing = decreasing,
    add_n = add_n
  )

  for (arg in names(logical_args)) {
    if (!inherits(logical_args[[arg]], "logical") ||
        length(logical_args[[arg]]) != 1L) {
      stop(sprintf("`%s` must be a single logical value.", arg), call. = FALSE)
    }
  }

  # fill
  if (!inherits(fill, "character") || length(fill) != 1L) {
    stop("`fill` must be a single character string.", call. = FALSE)
  }

  # size_n
  if (!inherits(size_n, "numeric") ||
      length(size_n) != 1L || size_n <= 0) {
    stop("`size_n` must be a single positive numeric value.", call. = FALSE)
  }

  # theme_plot
  if (!inherits(theme_plot, "theme")) {
    stop("`theme_plot` must be a ggplot2 theme object.", call. = FALSE)
  }


  if(is.null(occ) & !is.null(flagged_dir)){
    occ_list <- list.files(flagged_dir, pattern = output_format,
                           full.names = TRUE)
    if(length(occ_list) == 0){
      stop("There are no files with the ", output_format, " format in the specified 'flagged_dir' directory")
    }

    occ_list <- lapply(occ_list, data.table::fread)
    occ <- data.frame(data.table::rbindlist(occ_list, fill = TRUE))
  }


  if(all(flags == "all")){
    flags <- c("correct_country", "correct_state", "florabr", "faunabr",
               "wcvp", "iucn", "bien", "cultivated", "inaturalist",
               "duplicated", "thin_env", "thin_geo", "consensus",
               "Invalid coordinates",
               # Froom CoordinateCleaner
               ".val", ".equ", ".zer", ".cap", ".cen", ".sea", ".urb", ".otl",
               ".gbf", ".inst", ".aohi")
  }

  # Add _flags for some columns
  to_paste <- c("florabr", "faunabr", "wcvp", "iucn", "bien", "cultivated",
                "inaturalist", "duplicated", "thin_env", "thin_geo", "consensus")

  flags[flags %in% to_paste] <- paste0(flags[flags %in% to_paste], "_flag")

  # Additional flags
  if(!is.null(additional_flags)){
    flags <- c(flags, additional_flags)
  }

  # Subset columns
  flags <- intersect(flags, colnames(occ))

  # Names of flags
  flag_names <- getExportedValue("RuHere", "flag_names")

  # Names of additional flags
  # Get name
  if(!is.null(additional_flags)){
    if(is.null(names_additional_flags)){
      names_additional_flags <- additional_flags
    }
    names(names_additional_flags) <- additional_flags
    flag_names <- c(flag_names, names_additional_flags)
  }

  # Create dataframe with flags
  d <- data.frame(
    Flag = flags,
    n = colSums(!occ[, flags], na.rm = TRUE),
    row.names = NULL)


  if(show_no_flagged){
    d <- rbind(d,
               data.frame(Flag = "Unflagged",
                          n = nrow(occ[rowSums(occ[flags]) == length(flags), ])))
    flag_names <- c(flag_names, "Unflagged" = "Valid")
  }

  # Remove flags with 0 records
  d <- d[d$n > 0,]

  # Update flag names
  match_flags <- flag_names[match(unique(d$Flag), names(flag_names))]
  d$Flag <- match_flags[d$Flag]

  # Sort
  if(sort){
    d <- d[order(d$n, decreasing = FALSE), ]
  }

  #Plot
  if(plot){
    x_flag <- factor(x = d$Flag, levels = unique(d$Flag))

    # Basic plot
    g <- ggplot2::ggplot(data = d, ggplot2::aes(x = x_flag, y = .data[["n"]])) +
      ggplot2::geom_bar(stat = "identity", fill = fill) +
      ggplot2::xlab("Issue") + ggplot2::ylab("Number of records") +
      ggplot2::coord_flip() +
      ggplot2::theme(...) +
      theme_plot

    # If add number to plot
    if(add_n){
    g <- g +
      ggplot2::geom_text(ggplot2::aes(label = .data[["n"]]), hjust = -0.2,
                         size = size_n) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))
    }

  }

  # Return plot
  if(plot){
    return(list("df_summary" = d, "plot_summary" = g))
  } else {
    return(d)
  }

}


