#' Set Alignment
#'
#' This function is used to adjust the timestamps in a dataset by shifting them
#' either to match the first event in the entire dataset ('time') or to show
#' elapsed time since the first event in each group ('elapsed').
#' Note that this function assumes that the dataframe is already grouped.
#' It is recommended to use `df %>% group_by() %>% set_alignment(...)` when calling this function.
#'
#' @param df A grouped dataframe that contains the datetime data to be aligned.
#' @param datetime_col The name of the datetime column to be aligned.
#' @param alignment The type of alignment to be done. It can be either 'time' or 'elapsed'.
#'
#' @return A dataframe with the new alignment.
#'
#' @examples
#' \dontrun{
#' df %>% dplyr::group_by(group_col) %>% set_alignment(datetime_col = your_datetime_column, alignment = "time")
#' }
#' @export
#'
set_alignment <- function(df, datetime_col, alignment){

  options <- c('midnight', 'time', 'elapsed')

  if(!(alignment %in% options)){
    stop(paste("`alignment` must be one of", toString(options), "not", alignment))
  }

  min_datetime <- df %>% dplyr::pull({{datetime_col}}) %>% min()
  # This will keep HH:MM:SS as 00:00:00
  min_date_midnight <- lubridate::floor_date(min_datetime, unit = "day")
  # This will not contain HH:MM:SS info
  min_date <- as.Date(min_datetime)

  if(!dplyr::is_grouped_df(df)){
    warning("Input dataframe must be grouped.\nYou should use `df %>% group_by() %>% set_alignment(...)`")
  }

  if(alignment == 'midnight'){
    df <- df %>%
      # Calculate the elapsed time for each timestamp relative to the first timestamp in its group
      dplyr::mutate(elapsed = difftime({{datetime_col}},
                                       min({{datetime_col}}),
                                       units = "secs")) %>%
      # Shift each timestamp by adding the elapsed time to the min_date
      # The min_datetime will become midnight in the new dataset
      # This will distort the data when the different groups have different starting HH:MM:SS
      # or the starting HH:MM:SS of the different events varies because animals did not interact with fed3
      dplyr::mutate(dplyr::across(
        .cols = dplyr::where(lubridate::is.POSIXct),
        .fns = ~ min_date_midnight + as.difftime(elapsed, units = "secs"),
        .names = "aligned_{.col}")) %>%
      # Clean up the intermediate elapsed column
      dplyr::select(-elapsed)
  } else if (alignment == "time"){
    df <- df %>%
      dplyr::mutate(date_diff = difftime(as.Date({{datetime_col}}),
                                         min_date, units = "days")) %>%
      dplyr::mutate(dplyr::across(
      .cols = dplyr::where(lubridate::is.POSIXct),
      .fns = ~ {{datetime_col}} - date_diff,
      .names = "aligned_{.col}"
    ))  %>%
      dplyr::select(-date_diff)
  }  else if(alignment == 'elapsed'){
    df <- dplyr::mutate(df, dplyr::across(
      .cols = dplyr::where(lubridate::is.POSIXct),
      .fns = ~ difftime(.x, min(.x), units = "hours"),
      .names = "aligned_{.col}"
    ))
  }

  return(df)
}


#' @title Add Zeitgeber Time (ZT)
#' @description
#'
#' This function adds a new column `zt` to the input `data.frame`, which contains
#' the datetime values in `datetime_col` shifted so that `lights_on_hour` becomes the new midnight (00:00:00).
#'
#' @param df A `data.frame`
#' @param datetime_col The name of the column containing the datetime values to be shifted
#' @param lights_on_hour A numeric value between 1 and 23 representing the hour at which lights are turned on,
#' and which should become the new midnight (00:00:00)
#' @seealso [set_alignment()], [bin_pellets_lightcycle()]
#' @return A dataframe with the new `zt` column
#' @export
#'
#' @examples
#' df <- data.frame(
#'   datetime_col = seq(from = as.POSIXct("2023-07-20 00:00:00", tz = "UTC"),
#'                      to = as.POSIXct("2023-07-21 00:00:00", tz = "UTC"),
#'                      by = "hour"),
#'   value = runif(25)
#' )
#'
#' lights_on_hour = 6
#' df <- add_zt(df, datetime_col, lights_on_hour)
#'
add_zt <- function(df, datetime_col, lights_on_hour){
  if(!is.numeric(lights_on_hour) || lights_on_hour < 1 || lights_on_hour > 23){
    stop("lights_on_hour must be a numeric value between 1 and 23")
  }

  df <- df %>%
    dplyr::mutate(zt = lubridate::add_with_rollback(
      {{datetime_col}},
      -lubridate::hours(lights_on_hour)))

  return(df)
}
