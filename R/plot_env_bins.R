#' Plot Environmental Bins (2D Projection)
#'
#' @description
#' Visualize the output of `get_env_bins()` by plotting environmental blocks
#' (bins) along two selected environmental variables. Each block is shown as
#' a colored rectangle, and points falling inside the same rectangle share the
#' same `block_id`.
#'
#' @param env_bins (list) output list from `get_env_bins()`. Must contain:
#'   - `data`: data.frame with environmental values, bin indices, and block_id
#'   - `breaks`: named list with breakpoints for each variable
#' @param x_var (character) name of the environmental variable used on the
#' x-axis.
#' @param y_var (character) name of the environmental variable used on the
#' y-axis.
#' @param alpha_blocks (numeric) transparency level of the block rectangles.
#' Must be between 0 and 1. Default is 0.3.
#' @param color_points (character) color of the points representing occurrence
#' records. Default is `"black"`.
#' @param size_points (numeric) size of the points representing occurrence
#' records. Default is 2.
#' @param alpha_points (numeric) transparency level of the points. Must be
#' between 0 and 1. Default is 0.5..
#' @param stroke_points (numeric) size of the border of the points. Default is
#' 1.
#' @param xlab (character) label for the x-axis. Default is `NULL`, meaning
#' the name provided in `x_var` will be used.
#' @param ylab (character) label for the y-axis. Default is `NULL`, meaning
#' the name provided in `y_var` will be used.
#' @param theme_plot (theme) a `ggplot2` theme object. Default is
#' `ggplot2::theme_minimal()`.
#'
#' @return
#' A ggplot object showing the environmental blocks (colored rectangles) and
#' the occurrence records in the selected environmental space.
#'
#' @importFrom ggplot2 ggplot aes geom_rect geom_point geom_vline geom_hline
#'     labs theme_minimal theme
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Get only occurrences from Araucaria
#' occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
#' # Import environmental layers
#' r <- terra::rast(system.file("extdata", "worldclim.tif",
#'                              package = "RuHere"))
#' # Get bins
#' b <- get_env_bins(occ = occ, env_layers = r, n_bins = 10)
#' # Plot
#' plot_env_bins(b, x_var = "bio_1", y_var = "bio_12",
#'               xlab = "Temperature", ylab = "Precipitation")
plot_env_bins <- function(env_bins, x_var, y_var,
                          alpha_blocks = 0.3,
                          color_points = "black",
                          size_points = 2,
                          alpha_points = 0.5,
                          stroke_points = 1,
                          xlab = NULL,
                          ylab = NULL,
                          theme_plot = ggplot2::theme_minimal()) {

  #==============================#
  #        ARGUMENT CHECKING     #
  #==============================#

  ## Check env_bins structure
  if (!is.list(env_bins) ||
      !all(c("data", "breaks") %in% names(env_bins))) {
    stop("'env_bins' must be the output list from get_env_bins(), containing elements 'data' and 'breaks'.")
  }

  df <- env_bins$data
  br <- env_bins$breaks

  if (!is.data.frame(df)) {
    stop("'env_bins$data' must be a data.frame.")
  }

  if (!is.list(br)) {
    stop("'env_bins$breaks' must be a list of breakpoint vectors.")
  }

  ## Check x_var and y_var
  if (!is.character(x_var) || length(x_var) != 1) {
    stop("'x_var' must be a single character string.")
  }
  if (!is.character(y_var) || length(y_var) != 1) {
    stop("'y_var' must be a single character string.")
  }

  if (!x_var %in% names(df)) {
    stop(paste0("Variable '", x_var, "' not found in env_bins$data."))
  }
  if (!y_var %in% names(df)) {
    stop(paste0("Variable '", y_var, "' not found in env_bins$data."))
  }

  ## Check corresponding bin columns
  bin_x <- paste0(x_var, "_bin")
  bin_y <- paste0(y_var, "_bin")

  if (!bin_x %in% names(df)) {
    stop(paste0("Bin column '", bin_x,
                "' not found in env_bins$data. Check that 'x_var' matches a variable used in get_env_bins()."))
  }
  if (!bin_y %in% names(df)) {
    stop(paste0("Bin column '", bin_y,
                "' not found in env_bins$data. Check that 'y_var' matches a variable used in get_env_bins()."))
  }

  ## Check breakpoints
  if (!x_var %in% names(br)) {
    stop(paste0("Breakpoints for '", x_var, "' not found in env_bins$breaks."))
  }
  if (!y_var %in% names(br)) {
    stop(paste0("Breakpoints for '", y_var, "' not found in env_bins$breaks."))
  }

  ## Check numeric aesthetics
  if (!is.numeric(alpha_blocks) || alpha_blocks < 0 || alpha_blocks > 1) {
    stop("'alpha_blocks' must be a numeric value between 0 and 1.")
  }

  if (!is.numeric(size_points) || size_points <= 0) {
    stop("'size_points' must be a positive numeric value.")
  }

  if (!is.numeric(alpha_points) || alpha_points < 0 || alpha_points > 1) {
    stop("'alpha_points' must be a numeric value between 0 and 1.")
  }

  if (!is.numeric(stroke_points) || stroke_points < 0) {
    stop("'stroke_points' must be a non-negative numeric value.")
  }

  ## Check labels
  if (!is.null(xlab) && (!is.character(xlab) || length(xlab) != 1)) {
    stop("'xlab' must be a single character string or NULL.")
  }
  if (!is.null(ylab) && (!is.character(ylab) || length(ylab) != 1)) {
    stop("'ylab' must be a single character string or NULL.")
  }

  ## Check theme
  if (!inherits(theme_plot, "theme")) {
    stop("'theme_plot' must be a ggplot2 theme object.")
  }


  breaks_x <- br[[x_var]]
  breaks_y <- br[[y_var]]

  #==============================#
  #  PREPARE RECTANGLES          #
  #==============================#

  df$xmin <- breaks_x[df[[bin_x]]]
  df$xmax <- breaks_x[df[[bin_x]] + 1]
  df$ymin <- breaks_y[df[[bin_y]]]
  df$ymax <- breaks_y[df[[bin_y]] + 1]

  # Get x and ylab
  if(is.null(xlab)){
    xlab = x_var
  }
  if(is.null(ylab)){
    ylab = y_var
  }

  #==============================#
  #  PLOT                        #
  #==============================#

  p <- ggplot2::ggplot(df, aes(x = .data[[x_var]],
                               y = .data[[y_var]])) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data[["xmin"]],
                   xmax = .data[["xmax"]],
                   ymin = .data[["ymin"]],
                   ymax = .data[["ymax"]],
                   fill = .data[["block_id"]]),
      alpha = alpha_blocks, color = "black"
    ) +
    ggplot2::geom_point(colour = color_points, alpha = alpha_points, size = size_points,
               stroke = stroke_points) +
    ggplot2::geom_vline(xintercept = breaks_x, linetype = "dashed", color = "gray50") +
    ggplot2::geom_hline(yintercept = breaks_y, linetype = "dashed", color = "gray50") +
    theme_plot +
    ggplot2::labs(
      title = "Environmental Blocks",
      subtitle = paste0("2D projection across '", xlab, "' and '", ylab, "'"),
      x = xlab,
      y = ylab
    ) +
    ggplot2::theme(legend.position = "none",
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "italic"))

  return(p)
}
