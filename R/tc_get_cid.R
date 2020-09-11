#' get cid data
#'
#' @param borough
#'
#'
#'
#' @return
#' @export
#'
#' @examples
tc_get_cid = function(borough = NULL) {
  # download CID traffic calming data using the Cycle Infra Lnd package
  traffic_calming = get_cid_points(type = "traffic_calming")

  # convert certain columns to factors for analysis
  # NB BOROUGH needs doing separately as it has some NAs in, CLT_ACCESS not converted as 721 different values
  # f_variables = c("TRF_RAISED", "TRF_ENTRY", "TRF_CUSHI", "TRF_HUMP", "TRF_SINUSO",
                  # "TRF_BARIER", "TRF_NAROW", "TRF_CALM")

  # f_traffic_calming = traffic_calming %>%
  #   # mutate_at(f_variables, as.factor)
  # f_traffic_calming$BOROUGH = factor(traffic_calming$BOROUGH, exclude = NULL)
  #
  # glimpse(f_traffic_calming) # check converted ok
  # levels(f_traffic_calming$BOROUGH) # have 33 and no NA value
  #
  # # select traffic calming measures
  if(!is.null(borough)) {
    traffic_calming = traffic_calming %>%
      dplyr::filter(BOROUGH == borough)
  }


  # select traffic calming barriers
  tc_barriers = traffic_calming %>%
    dplyr::filter(TRF_BARIER == "TRUE")
}
