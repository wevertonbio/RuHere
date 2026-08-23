#' Cite data papers retrieved from get_datapaper
#'
#' @description
#' Identifies the data sources present in the output of \code{get_datapaper()}
#' and prints the appropriate bibliographic citations for use in academic and
#' technical publications.
#'
#' @param data A \code{data.frame} or \code{data.table} obtained from
#'   \code{get_datapaper()}, containing the \code{data_source} column.
#'
#' @return Prints the formatted references to the console and invisibly returns
#'   a named character vector with the citations for the detected data sources.
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. Download data
#' dados <- get_datapaper(datapaper = "dryflor")
#'
#' # 2. View citations
#' cite_datapaper(dados)
#' }
cite_datapaper <- function(data) {

  if (missing(data) || is.null(data) || !is.data.frame(data)) {
    stop("The 'data' argument must be a valid data.frame or data.table.", call. = FALSE)
  }

  if (!("data_source" %in% colnames(data))) {
    stop("The column 'data_source' was not found in the provided data object.", call. = FALSE)
  }

  # =========================================================================
  # 2. DICTIONARY
  # =========================================================================
  ref_dict <- c(
    # --- DryFlor ---
    "dryflor" = "Dryflor, et al. (2016). Plant diversity patterns in neotropical dry forests and their conservation implications. Science, 353(6306), 1383-1387. https://doi.org/10.1126/science.aaf5080",

    # --- NeoTropTree ---
    "NeoTropTree" = "Oliveira-Filho, A. T. (2017). NeoTropTree, Flora arb\u00f3rea da Regi\u00e3o Neotropical: Um banco de dados envolvendo biogeografia, diversidade e conserva\u00e7\u00e3o. Universidade Federal de Minas Gerais. http://www.neotroptree.info",

    # --- Brazil Road-Kill ---
    "Brazil road-kill" = "Grilo, C., Coimbra, M. R., Cerqueira, R. C., Barbosa, P., Dornas, R. A. P., Gon\u00e7alves, L. O., Teixeira, F. Z., Coelho, I. P., Schmidt, B. R., Pacheco, D. L. K., Schuck, G., Esperandio, I. B., Anza, J. A., Beduschi, J., Oliveira, N. R., Pinheiro, P. F., Bager, A., Secco, H., Guerreiro, M., ... Kindel, A. (2018). BRAZIL ROAD-KILL: A data set of wildlife terrestrial vertebrate road-kills. Ecology, 99(11), 2625-2625. https://doi.org/10.1002/ecy.2464",

    # --- Neotropical Xenarthrans ---
    "Neotropical Xenarthrans" = "Santos, P. M., Bocchiglieri, A., Chiarello, A. G., Paglia, A. P., Moreira, A., De Souza, A. C., Abba, A. M., Paviolo, A., Gatica, A., Medeiro, A. Z., Costa, A. N., Gallina, A. G., Yanosky, A. A., Jesus, A., Bertassoni, A., Rocha, A., Bovo, A. A. A., Bager, A., Mol, A. C., ... Galetti, M. (2019). NEOTROPICAL XENARTHRANS: A data set of occurrence of xenarthran species in the Neotropics. Ecology, 100(7), e02663. https://doi.org/10.1002/ecy.2663",

    # --- Atlantic Data Papers ---
    "atlantic_amphibians" = "Vancine, M. H., Duarte, K. D. S., De Souza, Y. S., Giovanelli, J. G. R., Martins-Sobrinho, P. M., L\u00f3pez, A., Bovo, R. P., Maffei, F., Lion, M. B., Ribeiro J\u00fanior, J. W., Brassaloti, R., Da Costa, C. O. R., Sawakuchi, H. O., Forti, L. R., Cacciali, P., Bertoluci, J., Haddad, C. F. B., & Ribeiro, M. C. (2018). ATLANTIC AMPHIBIANS: A data set of amphibian communities from the Atlantic Forests of South America. Ecology, 99(7), 1692-1692. https://doi.org/10.1002/ecy.239",

    "atlantic_ants" = "Silva, R. R., Martello, F., Feitosa, R. M., Silva, O. G. M., Do Prado, L. P., Brand\u00e3o, C. R. F., De Albuquerque, E. Z., Morini, M. S. C., Delabie, J. H. C., Dos Santos Monteiro, E. C., Emanuel Oliveira Alves, A., Wild, A. L., Christianini, A. V., Arnhold, A., Casadei Ferreira, A., Oliveira, A. M., Santos, A. D., Galb\u00e1n, A., De Oliveira, A. A., ... Ribeiro, M. C. (2022). ATLANTIC ANTS: A data set of ants in Atlantic Forests of South America. Ecology, 103(2), e03580. https://doi.org/10.1002/ecy.3580",

    "atlantic_bats" = "Muylaert, R. D. L., Stevens, R. D., Esb\u00e9rard, C. E. L., Mello, M. A. R., Garbino, G. S. T., Varzinczak, L. H., Faria, D., Weber, M. D. M., Kerches Rogeri, P., Regolin, A. L., Oliveira, H. F. M. D., Costa, L. D. M., Barros, M. A. S., Sabino-Santos, G., Crepaldi De Morais, M. A., Kavagutti, V. S., Passos, F. C., Marjakangas, E., Maia, F. G. M., ... Galetti, M. (2017). ATLANTIC BATS: A data set of bat communities from the Atlantic Forests of South America. Ecology, 98(12), 3227-3227. https://doi.org/10.1002/ecy.2007",

    "atlantic_birds" = "Hasui, \u00c9., Metzger, J. P., Pimentel, R. G., Silveira, L. F., Bovo, A. A. D. A., Martensen, A. C., Uezu, A., Regolin, A. L., Bispo De Oliveira, A. \u00c2., Gatto, C. A. F. R., Duca, C., Andretti, C. B., Banks-Leite, C., Luz, D., Mariz, D., Alexandrino, E. R., De Barros, F. M., Martello, F., Pereira, I. M. D. S., ... Ribeiro, M. C. (2018). ATLANTIC BIRDS: A data set of bird species from the Brazilian Atlantic Forest. Ecology, 99(2), 497-497. https://doi.org/10.1002/ecy.2119",

    "atlantic_butterflies" = "Santos, J. P. D., Freitas, A. V. L., Brown, K. S., Carreira, J. Y. O., Gueratto, P. E., Rosa, A. H. B., Louren\u00e7o, G. M., Accacio, G. M., Uehara-Prado, M., Iserhard, C. A., Richter, A., Gawlinski, K., Romanowski, H. P., Mega, N. O., Teixeira, M. O., Moser, A., Ribeiro, D. B., Araujo, P. F., Filgueiras, B. K. C., ... Ribeiro, M. C. (2018). Atlantic butterflies: A data set of fruit-feeding butterfly communities from the Atlantic forests. Ecology, 99(12), 2875-2875. https://doi.org/10.1002/ecy.2507",

    "atlantic_camera_trap_mammals" = "Franceschi, I. C., Dornas, R. A. D. P., Lermen, I. S., Coelho, A. V. P., Vilas Boas, A. H., Chiarello, A. G., Paglia, A. P., De Souza, A. C., Borsekowsky, A. R., Rocha, A., Bager, A., De Souza, A. Z., Lopes, A. M. C., De Moura, A. S., Ferreira, A. S., Garc\u00eda-Olaechea, A., Delciellos, A. C., Bacellar, A. E. D. F., Campelo, A. K. N., ... Coelho, I. P. (2024). Camera trap surveys of Atlantic Forest mammals: A data set for analyses considering imperfect detection (2004-2020). Ecology, 105(5), e4298. https://doi.org/10.1002/ecy.4298",

    "atlantic_camtrap" = "Lima, F., Beca, G., Muylaert, R. L., Jenkins, C. N., Perilli, M. L. L., Paschoal, A. M. O., Massara, R. L., Paglia, A. P., Chiarello, A. G., Graipel, M. E., Cherem, J. J., Regolin, A. L., Oliveira Santos, L. G. R., Brocardo, C. R., Paviolo, A., Di Bitetti, M. S., Scoss, L. M., Rocha, F. L., Fusco-Costa, R., ... Galetti, M. (2017). ATLANTIC - CAMTRAPS: A dataset of medium and large terrestrial mammal communities in the Atlantic Forest of South America. Ecology, 98(11), 2979-2979. https://doi.org/10.1002/ecy.1998",

    "atlantic_epiphyte" = "Ramos, F. N., Mortara, S. R., Monalisa-Francisco, N., Elias, J. P. C., Neto, L. M., Freitas, L., Kersten, R., Amorim, A. M., Matos, F. B., Nunes-Freitas, A. F., Alcantara, S., Alexandre, M. H. N., De Almeida-Scabbia, R. J., De Almeida, O. J. G., Alves, F. E., De Oliveira Alves, R. M., Alvim, F. S., De Andrade, A. C. S., De Andrade, S., ... Ribeiro, M. C. (2019). ATLANTIC EPIPHYTES: A data set of vascular and non-vascular epiphyte plants and lichens from the Atlantic Forest. Ecology, 100(2), e02541. https://doi.org/10.1002/ecy.2541",

    "atlantic_flower_invertebrate_interactions" = "Boscolo, D., Nobrega Rodrigues, B., Ferreira, P. A., Lopes, L. E., Tonetti, V. R., Reis Dos Santos, I. C., Hiruma-Lima, J. A., Nery, L., Baptista De Lima, K., Perozi, J., Freitas, A. V. L., Viana, B. F., Antunes-Carvalho, C., Amorim, D. D. S., Freitas De Oliveira, F., Groppo, M., Absy, M. L., De Almeida-Scabbia, R. J., Alves-Ara\u00fajo, A., ... Ribeiro, M. C. (2023). Atlantic flower-invertebrate interactions: A data set of occurrence and frequency of floral visits. Ecology, 104(3), e3900. https://doi.org/10.1002/ecy.3900",

    "atlantic_frugivory" = "Bello, C., Galetti, M., Montan, D., Pizo, M. A., Mariguela, T. C., Culot, L., Bufalo, F., Labecca, F., Pedrosa, F., Constantini, R., Emer, C., Silva, W. R., Da Silva, F. R., Ovaskainen, O., & Jordano, P. (2017). Atlantic frugivory: A plant-frugivore interaction data set for the Atlantic Forest. Ecology, 98(6), 1729-1729. https://doi.org/10.1002/ecy.1818",

    "atlantic_mammals" = "Souza, Y., Gon\u00e7alves, F., Lautenschlager, L., Akkawi, P., Mendes, C., Carvalho, M. M., Bovendorp, R. S., Fernandes-Ferreira, H., Rosa, C., Graipel, M. E., Peroni, N., Cherem, J. J., Bogoni, J. A., Brocardo, C. R., Miranda, J., Zago Da Silva, L., Melo, G., C\u00e1ceres, N., Sponchiado, J., ... Galetti, M. (2019). ATLANTIC MAMMALS: A data set of assemblages of medium- and large-sized mammals of the Atlantic Forest of South America. Ecology, 100(10), e02785. https://doi.org/10.1002/ecy.02785",

    "atlantic_nonvolant_mammals" = "Gon\u00e7alves, F., Hannibal, W., Godoi, M. N., Martins, F. I., Oliveira, R. F., Figueiredo, V. V., Casella, J., & De S\u00e1, \u00c9. F. G. G. (2018). Non-volant mammals from the Upper Paran\u00e1 River Basin: A data set from a critical region for conservation in Brazil. Ecology, 99(2), 499-499. https://doi.org/10.1002/ecy.2107",

    "atlantic_pollination" = "Iamara-Nogueira, J., Targhetta, N., Allain, G., Gambarini, A., Pinto, A. R., Rui, A. M., Ara\u00fajo, A. C., Lopes, A., Pereira-Silva, B., De Camargo, B. B., Machado, C. G., Missagia, C., Scultori, C., Boscolo, D., Fischer, E., Ara\u00fajo-Oliveira, E. S., Gava, H., Paulino-Neto, H. F., Machado, I. C., ... Buzato, S. (2022). ATLANTIC POLLINATION: A data set of flowers and interaction with nectar-feeding vertebrates from the Atlantic Forest. Ecology, 103(2), e03595. https://doi.org/10.1002/ecy.3595",

    "atlantic_primates" = "Culot, L., Pereira, L. A., Agostini, I., De Almeida, M. A. B., Alves, R. S. C., Aximoff, I., Bager, A., Baldovino, M. C., Bella, T. R., Bicca-Marques, J. C., Braga, C., Brocardo, C. R., Campelo, A. K. N., Canale, G. R., Cardoso, J. D. C., Carrano, E., Casanova, D. C., Cassano, C. R., Castro, E., ... Galetti, M. (2019). ATLANTIC - PRIMATES: A dataset of communities and occurrences of primates in the Atlantic Forests of South America. Ecology, 100(1), e02525. https://doi.org/10.1002/ecy.2525",

    "atlantic_small_mammal" = "Bovendorp, R. S., Villar, N., De Abreu-Junior, E. F., Bello, C., Regolin, A. L., Percequillo, A. R., & Galetti, M. (2017). Atlantic small-mammal: A dataset of communities of rodents and marsupials of the Atlantic forests of South America. Ecology, 98(8), 2226-2226. https://doi.org/10.1002/ecy.1893"
  )

  # Extract sources
  sources_present <- unique(as.character(stats::na.omit(data$data_source)))
  if (length(sources_present) == 0) {
    message("No data sources were found in the dataset.")
    return(invisible(character(0)))
  }

  matched_refs <- ref_dict[names(ref_dict) %in% sources_present]
  unmatched_sources <- setdiff(sources_present, names(ref_dict))

  # Build message
  msg <- paste0(
    "===============================================================================\n",
    "  PLEASE CITE THE FOLLOWING DATA PAPERS IN YOUR WORK:\n",
    "===============================================================================\n\n"
  )

  for (i in seq_along(matched_refs)) {
    src_name <- names(matched_refs)[i]
    citation_text <- matched_refs[i]
    msg <- paste0(msg, "[", i, "] Data Source: '", src_name, "'\n", citation_text, "\n\n")
  }

  has_atlantic <- any(grepl("^atlantic_", sources_present, ignore.case = TRUE))
  if (has_atlantic) {
    msg <- paste0(
      msg,
      "-------------------------------------------------------------------------------\n",
      "ATLANTIC Series Overview:\n",
      "Data Papers from a biodiversity hotspot: https://esajournals.onlinelibrary.wiley.com/doi/toc/10.1002/(ISSN)1939-9170.AtlanticPapers\n",
      "-------------------------------------------------------------------------------\n\n"
    )
  }

  # Warning
  if (length(unmatched_sources) > 0) {
    msg <- paste0(
      msg,
      "Warning: The following data_source(s) were not recognized in the reference database:\n",
      paste(" -", unmatched_sources, collapse = "\n"),
      "\n\n"
    )
  }

  message(msg)

  return(invisible(matched_refs))
}
