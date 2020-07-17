#' Create 'who-hit-who' visualisations
#'
#' @param crash_summary crashes dataset with vehicles and casuatlties variables
#' @param family font family
#' @param casualty_type The casualty types to plot
#' @export
#' @examples
#' crash_summary = tc_join_stats19(crashes_wf, casualties_wf, vehicles_wf)
#' names(crash_summary)
#' summary(crash_summary)
#' tc_upset(crash_summary)
#' tc_upset(crash_summary, casualty_type = c("Car", "Pedestrian", "Bicycle"))
#' # create plot with 'Other' category
#' table(casualties_wf$casualty_type)
#' casualties_wf2 = dplyr::mutate(
#'   casualties_wf,
#'   casualty_type_simple = dplyr::case_when(
#'      casualty_type == "Car occupant" ~ "Car",
#'      casualty_type == "Pedestrian" ~ "Pedestrian",
#'      casualty_type == "Cyclist" ~ "Cyclist",
#'      TRUE ~ "Other"
#'     )
#'   )
#' table(casualties_wf2$casualty_type_simple)
#' table(vehicles_wf$vehicle_type)
#' vehicles_wf2 = dplyr::mutate(
#'   vehicles_wf,
#'   vehicle_type_simple = dplyr::case_when(
#'      vehicle_type == "Car" ~ "Car",
#'      vehicle_type == "Cyclist" ~ "Pedal cycle",
#'      TRUE ~ "Other"
#'     )
#'   )
#' crash_summary = tc_join_stats19(crashes_wf, casualties_wf2, vehicles_wf2)
#' tc_upset(crash_summary, casualty_type = c("Car", "Pedestrian", "Bicycle", "Other"))
tc_upset = function(crash_summary,
                    casualty_type = c(
                      "Car",
                      "Pedestrian",
                      "Van",
                      "Bicycle",
                      "Motorcycle",
                      "Bus",
                      "HGV",
                      "Taxi"
                    ),
                    family = "") {
  # code resulting in upset plot

  # For the UpSet plot we do not differentiate number of vehicles involved in single crash.
  # As this is a set visualization we don't want to double count crashes.

  if (!requireNamespace("ComplexUpset", quietly = TRUE)) {
    stop("Install the ComplexUpset package")
  }

  ComplexUpset::upset(
    crash_summary,
    casualty_type,
    # split out as separate function?
    annotations = list("KSI" = list(
      aes = ggplot2::aes(x = intersection, fill = accident_severity),
      geom = list(
        ggplot2::geom_bar(stat = 'count', position = 'fill'),
        ggplot2::scale_y_continuous(labels = scales::percent_format()),
        ggplot2::scale_fill_manual(
          values = c(
            "Slight" = "#fee0d2",
            "Serious" = "#fc9272",
            "Fatal" = "#de2d26"
          )
        )
      )
    )),
    base_annotations = list(
      'Intersection size' = ComplexUpset::intersection_size(
        text = ggplot2::element_text(size = 3)
        )
    ),
    name = "Combinations of casualty types",
    width_ratio = 0.1,
    min_size = 50,
    themes = ComplexUpset::upset_modify_themes(
      list(
        'KSI' = ggplot2::theme(
          text = ggplot2::element_text(family = family),
          axis.text.x = ggplot2::element_blank()
        ),
        'Intersection size' = ggplot2::theme(text = ggplot2::element_text(family = family)),
        'intersections_matrix' = ggplot2::theme(text = ggplot2::element_text(family = family)),
        'overall_sizes' = ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          text = ggplot2::element_text(family = family)
        )
      )
    )
  )
}

#' Join stats19 tables
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


# original code -----------------------------------------------------------
# casualties_lookup_2 = c(
#   "otorcyc" = "Motorcyclist",
#   "7.5" = "HGV",
#   "Goods" = "HGV",
#   "Car occupant" = "Car",
#   "Van" = "Van",
#   "coach" = "Bus",
#   "Minibus" = "Minibus",
#   "Taxi" = "Taxi",
#   "Agri" = "Other",
#   "Missing" = "Other",
#   "Mobility" = "Other",
#   "Tram" = "Other",
#   "Horse" = "Other",
#   "Other" = "Other",
#   "issing" = "Other"
# )
#
# # Recode casualty types and vehicle types.
# casualties_all$casualty_type_simple <- tc_recode_casualties(casualties_all$casualty_type, pattern_match = casualties_lookup_2)
# vehicles_all$vehicle_type_simple <- tc_recode_vehicle_type(vehicles_all$vehicle_type)
# casualties_all <- casualties_all %>%
#   mutate(
#     casualty_type_simple=case_when(
#       casualty_type_simple=="Cyclist" ~ "Bicycle",
#       casualty_type_simple=="Motorcyclist" ~ "Motorcycle",
#       TRUE ~ casualty_type_simple
#     )
#   )
#
# # Join casualties to crashes using accident index
# family = "Avenir Book"
# crash_cas <- inner_join(
#   vehicles_all %>% select(accident_index, vehicle_type_simple),
#   casualties_all %>% select(accident_index, casualty_type_simple), by="accident_index") %>%
#   inner_join(crashes_all %>% select(accident_index, accident_severity), by = "accident_index")
#
# crash_cas <- crash_cas %>% pivot_longer(-c(accident_index, accident_severity), names_to="cas_veh", values_to="type")
# # For the UpSet plot we do not differentiate number of vehicles involved in single crash.
# # As this is a set visualization we don't want to double count crashes.
# crash_summary <- crash_cas %>%  select(accident_index, type) %>% unique %>% mutate(is_present=TRUE) %>%
#   pivot_wider(id=accident_index, names_from=type, values_from=is_present, values_fill=list(is_present=FALSE) )
# casualty_type <- colnames(crash_summary[2:11])
# crash_summary  <- crash_summary %>% inner_join(crash_cas %>% select(accident_index, accident_severity) %>% unique)
#
# # Plot
# plot <- upset(
#   crash_summary,
#   casualty_type,
#   annotations=list(
#     "KSI"=list(
#       aes=aes(x=intersection, fill=accident_severity),
#       geom=list(
#         geom_bar(stat='count', position='fill'),
#         scale_y_continuous(labels=scales::percent_format()),
#         scale_fill_manual(values=c(
#           "Slight"="#fee0d2", "Serious"="#fc9272", "Fatal"="#de2d26"
#         ))
#       )
#     )
#   ),
#   base_annotations=list('Intersection size'=intersection_size(text=element_text(size=3))),
#   name="Combinations of casualty types",
#   width_ratio=0.1,
#   min_size=50,
#   themes=upset_modify_themes(
#     list(
#       'KSI'=theme(text=element_text(family = family), axis.text.x=element_blank()),
#       'Intersection size'=theme(text=element_text(family = family)),
#       'intersections_matrix'=theme(text=element_text(family = family)),
#       'overall_sizes'=theme(axis.text.x=element_blank(), text=element_text(family = family))
#     )
#   )
# )
#
# ggsave("./figures/upset_stats19.png", plot=plot, width=12, height=9, dpi=600)
