#' @title Filter Pellets
#' @description This function filters the pellet events
#' @export
filter_pellets <- function(df){
  df %>% dplyr::filter(Event == "Pellet")
}

#' @title Recalculate Pellets
#' @description This function recalculates pellets if given a FED_data `data.frame` that contains an identifier column. The main reason behind it is that one animal can receive more than one FED device. This might happen due to the experiment design or because a FED needed to be replaced during the experiment. Alternatively, it could be used to analyze several datasets coming from different animals independent of `Device_Number`.
#' @param df A data frame containing FED data. If your `data.frame` is grouped, `group_var` will be disregarded.
#' @param group_var A string specifying the column to group by. If NULL (default), no grouping is performed.
#' @return A data frame identical to `df` but with recalculated pellet counts and arranged by `datetime`.
#' @export
recalculate_pellets <- function(df, group_var = NULL) {
  if (dplyr::is_grouped_df(df)) {
    if (!rlang::is_null(rlang::enexpr(group_var))) {
      warning("Ignoring 'group_var' argument as the input dataframe is already grouped.")
    }
    groups <- dplyr::group_vars(df)
    output <- df %>%
        filter_pellets() %>%
        dplyr::arrange(dplyr::across(dplyr::all_of(groups)), datetime) %>%
        dplyr::mutate(pellets = 1:length(Pellet_Count))
  } else {
    # Case: Not grouped but group_var provided
    if (!rlang::is_null(rlang::enexpr(group_var))) {
      group_var_enquo <- rlang::enquo(group_var)
      group_var_name <- rlang::quo_name(group_var_enquo)
      if (group_var_name %in% names(df)) {
        output <- df %>%
          filter_pellets() %>%
          dplyr::arrange(!!group_var_enquo, datetime) %>%
          dplyr::group_by(!!group_var_enquo) %>%
          dplyr::mutate(pellets = 1:length(Pellet_Count)) %>%
          dplyr::ungroup()
      }
    } else {
      # Case: Not grouped, no group_var provided
      output <- df %>%
        filter_pellets() %>%
        dplyr::arrange(datetime) %>%
        dplyr::mutate(pellets = 1:length(Pellet_Count))
    }
  }
  return(output)

}

#' @title Bin Pellets
#'
#' @description Bins the pellets based on the specified time interval in `bin` and the data range.
#'
#' @param data A data frame containing the pellet data.
#' @param time_col The `datetime` column to use as
#' @param bin A character string specifying the time interval for binning (e.g., "1 hour", "30 minunte").
#' @param label_first_break Logical indicating whether to label the first break as the start time (default is TRUE).
#'
#' @return A data frame with binned pellet counts and corresponding bin timestamps.
#' @seealso [recalculate_pellets()], [clock::time_point_round()]
#' @export
bin_pellets <- function(data, time_col, bin, label_first_break = TRUE) {
  # check if we have 100% pellet events
  if (fed3:::check_pellets(data)) {
    stop("Data contains Events other than Pellets.")
  }

  # get the proper column
  time_column <- dplyr::pull(data, {{time_col}})

  # split bin into n and precision
  bin_components <- fed3:::parse_bin(bin)
  n <- bin_components$n
  precision <- bin_components$precision

  # get grouping variables
  groups <- dplyr::group_vars(data)

  # generate the breaks using clock package
  breaks <- seq(from = clock::date_floor(min(time_column), n = n, precision = precision),
                to = clock::date_ceiling(max(time_column), n = n, precision = precision),
                by = paste(n, fed3:::standardize_unit(precision)))

  # We still want to keep the labels as a POSIXct object for later merging
  if (label_first_break) {
    labels <- breaks[-length(breaks)]
  } else {
    labels <- breaks[-1]
  }

  # bin the time column using clock::date_group
  data <- data %>%
    dplyr::mutate(bin = clock::date_group({{time_col}}, n = n, precision = precision))

  # nest to perform calculation
  data_nested <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups)), bin) %>%
    tidyr::nest()

  data_nested <- data_nested %>%
    dplyr::mutate(data = purrr::map(data, ~dplyr::summarise(.x, pellet_rate = n()))) %>%
    tidyr::unnest(data) %>%
    dplyr::ungroup()

  # create a data frame that includes all possible combinations of bins and groups
  unique_groups <- data %>%
    dplyr::select(all_of(groups)) %>%
    dplyr::distinct()

  # Generate possible combinations of bin and groups without duplicates
  complete_data <- tidyr::crossing(bin = as.POSIXct(labels, tz = "UTC"), unique_groups)

  # join the computed data with the complete data
  return(
    complete_data %>%
      dplyr::left_join(data_nested, by = c(groups, "bin")) %>%
      tidyr::replace_na(list(pellet_rate = 0)) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(groups)), bin)
  )
}

#' @title Bin Pellet Events According to Light Cycle
#'
#' @description This function assigns each event to a light or dark period, then counts the number of events in each period.
#' It requires a data frame that contains a datetime column and only pellet events.
#'
#' Because dates and light cycles are not aligned (dark phase often spans more than one date), it is recommended that users call `add_zt()` first and bin by `zt` instead of `datetime`.
#'
#' @param data A data frame that contains a datetime column and only pellet events.
#' @param time_col The datetime column in your data frame. You can use a bare column name.
#' It is recommended to use the "zt" column created by `add_zt()` to ensure that the light and dark periods align correctly.
#' @param lights_on_hour The hour (0-23) when the light period starts. Default is 7.
#' @param lights_off_hour The hour (0-23) when the light period ends. Default is 19.
#'
#' @return A `data.frame` grouped by the original grouping variables, date and light cycle period,
#' with an additional column `pellets` indicating the number of events in each period.
#'
#' @seealso [fed3::bin_pellets()], [fed3::add_zt()], [fed3::filter_pellets()], [fed3::recalculate_pellets()]
#' @examples
#'
#' \dontrun{
#' # will have light cycle split by date
#' read_fed(path) %>% recalculate_pellets() %>% add_zt(datetime) %>% bin_pellets_lightcycle(datetime)
#' read_fed(path) %>% recalculate_pellets() %>% add_zt(datetime) %>% bin_pellets_lightcycle(datetime)
#' # zt date will only contain full light/dark periods
#' read_fed(path) %>% recalculate_pellets() %>% add_zt(datetime) %>% bin_pellets_lightcycle(zt)
#' }
#'
#' @export
bin_pellets_lightcycle <- function(data, time_col, lights_on_hour = 7, lights_off_hour = 19) {
    # Check if we have 100% pellet events
    if (fed3:::check_pellets(data)) {
      stop("Data contains Events other than Pellets.\nUse filter_pellets() or recalculate_pellets() as needed.")
    }
    # intercept wrong use of zt
    time_col_name <- rlang::as_name(rlang::enquo(time_col))

    if (time_col_name == "zt") {
      usethis::ui_warn("Using `zt`, adjusting `lights_on_hours`=0 and `lights_off_hour`=12")
      lights_on_hour <- 0
      lights_off_hour <- 12
    }

    # Get grouping variables
    groups <- dplyr::group_vars(data)

    # Create a new column to indicate whether it's light or dark
    data <- data %>%
      dplyr::mutate(light_cycle =
                      purrr::map_chr(.x = {{time_col}},
                                     .f = ~ {
                                       if (lights_off_hour > lights_on_hour) {
                                         if (lubridate::hour(.x) >= lights_on_hour &
                                             lubridate::hour(.x) < lights_off_hour) {"light"}
                                         else {"dark"}
                                       } else {
                                         if (lubridate::hour(.x) >= lights_on_hour |
                                             lubridate::hour(.x) < lights_off_hour) {"light"}
                                         else {"dark"}
                                       }
                                     }))
    # light_cycle should be a factor with levels c("light", "dark")
    data <- dplyr::mutate(data, light_cycle = factor(light_cycle, levels = c("light", "dark")))

    # Count the events
    data <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(groups)),
                      date = lubridate::date({{time_col}}),
                      light_cycle) %>%
      dplyr::count(name = "pellets")

    # rename if needed
    if (time_col_name == "zt") {
      data <- dplyr::rename(data, zt_date = date)
    }

    return(data)
}

check_pellets <- function(data) {
  return(any(data$Event != "Pellet"))
}

# This function comes handy to use the units from the clock package with seq()
standardize_unit <- function(unit) {
  switch(unit,
         minute = "min",
         hour = "hour",
         day = "day",
         week = "week",
         month = "month",
         year = "year",
         stop("Invalid unit.")
  )
}

# This is a nice helper to parse a bin written as "1 hour" into n and precision
# the idea is to feed this
parse_bin <- function(bin) {
  # Split bin argument by spaces
  bin_components <- strsplit(bin, " ")[[1]]
  # Error handling
  if (length(bin_components) != 2) {
    stop("bin argument must be a string in the format of 'n unit', where n is a number and unit is a time unit (e.g., '1 hour').")
  }
  n <- bin_components[1]
  if (!is.numeric(as.numeric(n)) || as.numeric(n) <= 0) {
    stop("The first part of the `bin` argument must be a positive number.")
  }
  n <- as.integer(n)
  precision <- bin_components[2]
  valid_units <- c("second", "minute", "hour", "day", "week", "month", "year")
  if (!(precision %in% valid_units)) {
    stop(paste("The second part of the `bin` argument must be a valid time unit.\nChoose from:", paste(valid_units, collapse = ", "), "."))
  }
  return(list(n = n, precision = precision))
}
