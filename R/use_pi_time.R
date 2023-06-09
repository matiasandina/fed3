#' Adjust datetime in FED data using Pi_Time from FEDWatcher
#'
#' This function adjusts the time in `fed_df` dataset
#' using a linear model trained on data coming from FEDWatcher (where `Pi_Time` is not NA).
#' You can use this function even if `fed_df` and `fw_df` do not fully overlap
#' in time.
#'
#' @param fed_df A data frame, the result of fed3::read_fed(...).
#' @param fw_df A data frame, the result of fed3::read_fed(..., lib_version = "fw").
#'
#' @return A data frame identical to `fed_df` but with `Pi_Time` adjusted based
#' on the linear model trained on `fw_df`.
#' @export
#'
#' @examples
#' \dontrun{
#' fed_df <- fed3::read_fed(...)
#' fw_df <- fed3::read_fed(..., lib_version = "fw")
#' fed_df_adjusted <- use_pi_time(fed_df, fw_df)
#' }
use_pi_time <- function(fed_df, fw_df){
  # Join and calculate 'dt'
  df <- dplyr::left_join(
    fed_df %>% dplyr::select(`MM:DD:YYYY hh:mm:ss`, datetime, Event),
    fw_df %>% dplyr::select(Pi_Time, datetime, Event),
    by = dplyr::join_by(datetime, Event)
  ) %>%
    dplyr::mutate(
      dt = as.numeric(base::difftime(Pi_Time, datetime, units = "secs"))
    )

  # Fit a linear model
  fit <- stats::lm(dt ~ datetime, data = df, na.action = stats::na.exclude)

  # Predict missing Pi_Time values
  df <- df %>%
    dplyr::mutate(
      Pi_Time_predicted = as.POSIXct(datetime + lubridate::seconds(stats::predict(fit, newdata = df)), origin = "1970-01-01"),
      Pi_Time = dplyr::if_else(is.na(Pi_Time), Pi_Time_predicted, Pi_Time)
    )

  fed_df$Pi_Time <- df$Pi_Time
  return(fed_df)
}
