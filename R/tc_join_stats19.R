#' Join STATS19 tables to generate UpSet plots with. In the case of vehicle
#' type UpSet plots, the columns will be types of vehicles and a boolean
#' to indicate if they were involved in the particular crash. The accident
#' severity will remain as column too.
#'
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

  # change casualty_type_simple "Cyclist" and "Motorcyclist" to
  # match values in vehicle_type_simple to join them
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

#' Basic draft of tc_join_stats19_ac whilst I try to understand issue #24.
#'
#' @inheritParams tc_join_stats19
#' do not export
#' TODO: understand the requirements first
tc_join_stats19_ac = function(crashes = NULL,
                              casualties = NULL,
                              vehicles = NULL,
                              only_counts = FALSE) {
  if(is.null(crashes) || is.null(casualties) ||
     is.null(vehicles)) {
    stop("All three tables are required for the join.")
  }

  if(is.null(casualties$accident_index) ||
     is.null(casualties$casualty_type) ||
     is.null(vehicles$accident_index) ||
     is.null(vehicles$vehicle_type)) {
    stop("This function needs required columns within stats19 tables.")
  }
  # assuming accident_index is unique across the tables and
  # vehicles and casualties have matching columns
  # crash_summary = crashes %>%
  #   group_by(accident_index) %>% summarise(crash_records = n())

  crash_summary = casualties %>%
    group_by(accident_index, casualty_type) %>%
    summarise(casualty_counts = n()) %>%
    select(accident_index, casualty_counts) %>%
    inner_join(crashes, by = "accident_index")

  crash_summary = vehicles %>%
    group_by(accident_index, vehicle_type) %>%
    summarise(vehicle_counts = n()) %>%
    select(accident_index, vehicle_counts) %>%
    inner_join(crash_summary, by = "accident_index")

  if(only_counts) {
    crash_summary = crash_summary %>%
      select(accident_index, casualty_counts, vehicle_counts)
  }
  crash_summary
}
