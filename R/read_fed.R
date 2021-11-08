#' @title "Read Fed Files"
#' @name read_fed
#' @param filename file path of csv file produced by FED3
#' @details This function will read the raw data with column specifications (see `fed_col_types()`). It will append the `datetime` by formatting the FED's clock datetime as `yyyy-mm-dd HH:MM:SS`, it will also extract `year`, `month`, and `day` from that date. It will parse the session from the `filename` and append it to the resulting `data.frame`.
#' @export
read_fed <- function(filename, lib_version = NULL){

  col_types <- fed_col_types(lib_version = lib_version)
  X <- readr::read_csv(filename, col_types = col_types) %>%
    # add datetime, parse session from filename
    dplyr::mutate(
      datetime = lubridate::parse_date_time(`MM:DD:YYYY hh:mm:ss`, "mdy HMS"),
      session = stringr::str_extract(tolower(filename), pattern="_[0-9]+\\.csv$"),
      session = stringr::str_extract(session, pattern="[0-9]+"))


  # Add year, day month columns
  # add fed number
  X <- X %>%
    dplyr::mutate(day = lubridate::day(datetime),
                  month = lubridate::month(datetime),
                  year = lubridate::year(datetime),
                  FED = paste0("FED", str_pad(Device_Number, width=3, pad=0)))
  return(X)
}
