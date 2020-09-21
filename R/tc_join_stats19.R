#' Join STATS19 tables
#'
#' @param crashes The accidents table of crashes from STATS19 data
#' @param casualties The casualty table from STATS19 data
#' @param vehicles The vehicles table from STATS19 data
#' @inheritParams tc_recode
#' @export
tc_join_stats19 = function(crashes,
                           casualties,
                           vehicles,
                           pattern_match = NULL
) {

  # recode casualties and crashes
  if(is.null(casualties$casualty_type_simple)) {
    if (is.null(pattern_match)) {
      casualties$casualty_type_simple =
        tc_recode_casualties(casualties$casualty_type)
    } else {
      casualties$casualty_type_simple =
        tc_recode_casualties(casualties$casualty_type, pattern_match = pattern_match)
    }
  }

  if(is.null(vehicles$vehicle_type_simple)) {
    vehicles$vehicle_type_simple =
      tc_recode_vehicle_type(vehicles$vehicle_type)
  }

  casualties = casualties %>%
    dplyr::mutate(
      casualty_type_simple = dplyr::case_when(
        casualty_type_simple == "Cyclist" ~ "Bicycle",
        casualty_type_simple == "Motorcyclist" ~ "Motorcycle",
        TRUE ~ casualty_type_simple
      )
    )
  crash_cas = dplyr::inner_join(
    vehicles %>% dplyr::select(accident_index, vehicle_type_simple),
    casualties %>% dplyr::select(accident_index, casualty_type_simple),
    by = "accident_index"
  ) %>%
    dplyr::inner_join(crashes %>%  dplyr::select(accident_index, accident_severity),
                      by = "accident_index")
  crash_cas =
    crash_cas %>% tidyr::pivot_longer(
      -c(accident_index, accident_severity),
      names_to = "cas_veh",
      values_to = "type"
    )
  # # For the UpSet plot we do not differentiate number of vehicles involved in single crash.
  # # As this is a set visualization we don't want to double count crashes.
  crash_summary =
    crash_cas %>%
    dplyr::select(accident_index, type) %>%
    unique %>%
    dplyr::mutate(is_present = TRUE) %>%
    tidyr::pivot_wider(
      id = accident_index,
      names_from = type,
      values_from = is_present,
      values_fill = list(is_present = FALSE)
    )
  crash_summary = crash_summary %>%
    dplyr::inner_join(crash_cas %>%
                        dplyr::select(accident_index, accident_severity) %>%
                        unique
    )
  crash_summary
}
