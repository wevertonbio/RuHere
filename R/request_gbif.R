request_gbif <- function(gbif_info, hasCoordinate = TRUE,
                         hasGeospatialIssue = FALSE, format = "DWCA",
                         gbif_user = NULL, gbif_pwd = NULL, gbif_email = NULL,
                         additional_predicates = NULL){
  # Get key
  k <- gbif_info$usageKey

  # Get list of arguments
  p_list <- pred_and(rgbif::pred_in("taxonKey", k), #Taxonkey
                 rgbif::pred("hasCoordinate", hasCoordinate),
                 rgbif::pred("hasGeospatialIssue", hasGeospatialIssue))

  if(!is.null(additional_predicates)){
    # If additional_predicates is a single pred
    if(inherits(additional_predicates[[1]], "character")){
      p_list2 <- c(p_list, list(additional_predicates))
      # If additional_predicates is a multiple pred
    } else if (inherits(additional_predicates[[1]], "occ_predicate")){
      p_list2 <- c(p_list, additional_predicates)
    }
    p_list <- do.call(
      what = rgbif::pred_and,
      args = p_list2)
  }

  # Get request
  occ_request <- rgbif::occ_download(
    p_list,
    format = format,
    user = gbif_user,
    pwd = gbif_pwd,
    email = gbif_email) # Other arguments
  request_key <- occ_request[1]
  class(request_key) <- "request_key"

  return(request_key)
}

# # Test
# devtools::load_all()
# # Import data
# data("gbif_prepared", package = "RuHere")
# gbif_info = gbif_prepared
# hasCoordinate = TRUE
# hasGeospatialIssue = FALSE
# format = "DWCA"
# gbif_user = NULL
# gbif_pwd = NULL
# gbif_email = NULL
# additional_predicates <- pred_and(
#   pred_in("country", "BR"),
#   pred_gte("year", "1999")
# )
#
#
# req_gbif <- request_gbif(gbif_info = gbif_prepared)
# # Save as RDS file
# dir.create("../RuHere_test/")
# saveRDS(req_gbif, "../RuHere_test/req_gbif.rds")
#
# additional_predicates <-  pred_gte("year", "1999")
#
# additional_predicates <- pred_and(
#   pred_in("country", "BR"),
#   pred_gte("year", "1999")
# )
#
# occ_download_wait(req_gbif)
