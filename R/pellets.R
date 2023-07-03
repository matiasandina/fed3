#' @title Filter Pellets
#' @description This function filters the pellet events
#' @export
filter_pellets <- function(df){
  df %>% dplyr::filter(Event == "Pellet")
}

#' @title Recalculate Pellets
#' @description This function recalculates pellets if given a FED_data `data.frame` that contains an identifier column. The main reason behind it is that one animal can receive more than one FED device. This might happen due to the experiment design or because a FED needed to be replaced during the experiment. Alternatively, it could be used to analyze several datasets coming from different animals independent of `Device_Number`.
#' @param df A data frame containing FED data.
#' @param group_var A string specifying the column to group by. If NULL (default), no grouping is performed.
#' @return A data frame identical to `df` but with recalculated pellet counts.
#' @export
recalculate_pellets <- function(df, group_var = NULL) {
  if (!is.null(group_var)) {
    group_var_enquo <- rlang::enquo(group_var)
    if (rlang::as_string(group_var_enquo) %in% names(df)) {
      df %>%
        filter_pellets() %>%
        dplyr::arrange({{group_var}}, datetime) %>%
        dplyr::group_by({{group_var}}) %>%
        dplyr::mutate(pellets = 1:length(Pellet_Count)) %>%
        ungroup()
    }
  } else {
    df %>%
      filter_pellets() %>%
      dplyr::arrange(datetime) %>%
      dplyr::mutate(pellets = 1:length(Pellet_Count))
  }
}

#' @title Bin Pellets
#'
#' @description Bins the pellets based on the specified time interval in `bin` and the data range.
#'
#' @param data A data frame containing the pellet data.
#' @param time_col The `datetime` column to use as
#' @param bin A character string specifying the time interval for binning (e.g., "1 hour", "30 min").
#' @param label_first_break Logical indicating whether to label the first break as the start time (default is TRUE).
#'
#' @return A data frame with binned pellet counts and corresponding bin timestamps.
#' @seealso [recalculate_pellets()]
#' @export
bin_pellets <- function(data, time_col, bin, label_first_break = TRUE) {
  # get the proper column
  time_column <- pull(data, {{time_col}})

  # get grouping variables
  groups <- dplyr::group_vars(data)

  # generate the breaks
  breaks <- seq(from = lubridate::floor_date(min(time_column), bin),
                to = lubridate::ceiling_date(max(time_column), bin),
                by = bin)

  # breaks will be [From, To)
  if (label_first_break) {
    # we label From
    labels <- breaks[-length(breaks)]
  } else {
    # we label To
    labels <- breaks[-1]
  }
  # bin the time column on common breaks
  data <- data %>%
    dplyr::mutate(bin = cut({{time_col}}, breaks = breaks, labels = labels, right = FALSE))
  # nest to perform calculation
  data_nested <- data %>%
    dplyr::group_by(across(all_of(groups)), bin) %>%
    tidyr::nest()
  # calculate last - first pellet count on cummulative column
  data_nested <- data_nested %>%
    dplyr::mutate(data = purrr::map(data, ~dplyr::summarise(.x, pellet_rate = dplyr::last(pellets) - dplyr::first(pellets)))) %>%
    tidyr::unnest(data) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(bin = as.POSIXct(as.character(bin)))

  # create a data frame that includes all possible combinations of bins and groups
  unique_groups <- data %>%
    dplyr::select(all_of(groups)) %>%
    dplyr::distinct()
  # Generate possible combinations of bin and groups without duplicates
  complete_data <- tidyr::crossing(bin = as.POSIXct(as.character(labels)), unique_groups)

  # join the computed data with the complete data
  complete_data %>%
    dplyr::left_join(data_nested, by = c("bin", groups)) %>%
    tidyr::replace_na(list(pellet_rate = 0)) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(groups)), bin)
  return(complete_data)
}
