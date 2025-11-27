#' Relocate a column in a data frame
#'
#' @description
#' These functions move one column to a new position in a data frame,
#' either immediately *after* or *before* another column, while preserving
#' the order of all remaining columns. They are lightweight base-R utilities
#' equivalent to `dplyr::relocate()`, but without external dependencies.
#'
#' @param df (data.frame) a data.frame whose columns will be reordered.
#' @param col (character) the name of the column to move.
#' @param after (character) for `relocate_after()`: the column after which
#'   `col` will be placed.
#' @param before (character) for `relocate_before()`: the column before which
#'   `col` will be placed.
#'
#' @return A data.frame with columns reordered.
#' @export
relocate_after <- function(df, col, after) {
  nm <- names(df)

  i <- match(after, nm)     # position after which to relocate
  j <- match(col, nm)       # position of column to move

  if (is.na(i) || is.na(j)) return(df)

  new_order <- append(nm[-j], nm[j], after = i)
  df[, new_order, drop = FALSE]
}


#' @rdname relocate_after
#' @export
relocate_before <- function(df, col, before) {
  nm <- names(df)

  i <- match(before, nm)    # position before which to relocate
  j <- match(col, nm)       # position of column to move

  if (is.na(i) || is.na(j)) return(df)

  # append inserts after a position; to move before, insert at i - 1
  new_order <- append(nm[-j], nm[j], after = i - 1)
  df[, new_order, drop = FALSE]
}
