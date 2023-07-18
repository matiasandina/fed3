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

  options <- c('time', 'elapsed')

  if(!(alignment %in% options)){
    stop(paste("`alignment` must be one of", toString(options), "not", alignment))
  }

  min_datetime <- df %>% dplyr::pull({{datetime_col}}) %>% min()
  min_date <- lubridate::floor_date(min_datetime, unit = "day")

  if(!dplyr::is_grouped_df(df)){
    warning("Input dataframe must be grouped.\nYou should use `df %>% group_by() %>% set_alignment(...)`")
  }

  if(alignment == 'time'){
    df <- df %>%
      # Calculate the elapsed time for each timestamp relative to the first timestamp in its group
      dplyr::mutate(elapsed = difftime({{datetime_col}},
                                       min({{datetime_col}}),
                                       units = "secs")) %>%
      # Shift each timestamp by adding the elapsed time to the min_date
      dplyr::mutate(dplyr::across(
        .cols = dplyr::where(lubridate::is.POSIXct),
        .fns = ~ min_date + as.difftime(elapsed, units = "secs"),
        .names = "aligned_{.col}")) %>%
      # Clean up the intermediate elapsed column
      dplyr::select(-elapsed)
  } else if(alignment == 'elapsed'){
    df <- dplyr::mutate(df, dplyr::across(
      .cols = dplyr::where(lubridate::is.POSIXct),
      .fns = ~ difftime(.x, min(.x), units = "hours"),
      .names = "aligned_{.col}"
    ))
  }

  return(df)
}
