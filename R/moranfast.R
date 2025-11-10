#' Title
#'
#' @param x
#' @param weight
#' @param na_rm
#' @param scaled
#' @param alternative
#'
#' @returns
#' @export
#'
#' @examples
moranfast <- function(x, weight, na_rm = FALSE, scaled = FALSE,
                      alternative = c("two.sided", "less", "greater")) {

  # Verificação de argumento (boa prática do R)
  alternative <- match.arg(alternative)

  # Call C++ function
  .Call(`_RuHere_moranfast`, x, weight, na_rm, scaled, alternative)
}
