#' Remove accents and special characters from strings
#'
#' @description
#' This function removes accents and replaces special characters from strings,
#' returning a plain-text version suitable for data cleaning or standardization.
#'
#' @param s (character) a character vector containing the strings to process.
#'
#' @returns
#' A vector string without accents or special characters
#'
#' @export
#'
#' @examples
#' remove_accent(c("Colômbia", "São Paulo"))
remove_accent <- function(s) {
  chartr(
    "áéóūáéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
    "aeouaeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC",
    s)
}
