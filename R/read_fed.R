#' @title "Read Fed Files"
#' @name read_fed
#' @param filename file path of csv file produced by FED3
#' @details This function will read the raw data with column specifications (see `fed_col_types()`). It will append the `datetime` by formatting the FED's clock datetime as `yyyy-mm-dd HH:MM:SS`, it will also extract `year`, `month`, and `day` from that date. It will parse the session from the `filename` and append it to the resulting `data.frame`.
#' @export
read_fed <- function(filename, lib_version = NULL, deduplicate_method = "offset"){

  col_types <- fed_col_types(lib_version = lib_version)
  X <- readr::read_csv(filename, col_types = col_types) %>%
    # add datetime, parse session from filename
    dplyr::mutate(
      datetime = lubridate::parse_date_time(`MM:DD:YYYY hh:mm:ss`, "mdy HMS"),
      session = stringr::str_extract(tolower(filename), pattern="_[0-9]+\\.csv$"),
      session = stringr::str_extract(session, pattern="[0-9]+"))

  # datetime might parse wrong or with duplicates
  X <- deduplicate_datetime(X, method = deduplicate_method)
  # Add year, day month columns
  # add fed number
  X <- X %>%
    dplyr::mutate(day = lubridate::day(datetime),
                  month = lubridate::month(datetime),
                  year = lubridate::year(datetime),
                  FED = paste0("FED", stringr::str_pad(Device_Number, width=3, pad=0)))
  return(X)
}


# Function to check for duplicated entries
check_duplicated <- function(data, column) {
  return(anyDuplicated(data[[column]]))
}


deduplicate_datetime <- function(data, method = 'offset', offset = '0.1 sec',
                        reset_counts = FALSE,
                        reset_columns = c('Pellet_Count', 'Left_Poke_Count', 'Right_Poke_Count')) {

  # Warn if there are NAs in the parsing
  if (any(is.na(data$datetime))){
    warning("NA values found in `datetime` column after parsing.\n Filling NAs with last observation carried forward.")
    data$datetime <- zoo::na.locf(data$datetime)
  }

  # Deduplicate based on selected method
  if (method == 'keep_first') {
    data <- dplyr::distinct(data, datetime, .keep_all = TRUE)
  } else if (method == 'keep_last') {
    data <- data %>%
      dplyr::arrange(dplyr::desc(datetime)) %>%
      dplyr::distinct(datetime, .keep_all = TRUE) %>%
      dplyr::arrange(datetime)
  } else if (method == 'remove') {
    dup_rows <- duplicated(data$datetime) | duplicated(data$datetime, fromLast = TRUE)
    data <- data[!dup_rows, ]
  } else if (method == 'offset') {
    dt <- as.numeric(lubridate::duration(offset))
    while(any(duplicated(data$datetime))) {
      dupes <- which(duplicated(data$datetime))
      data$datetime[dupes] <- data$datetime[dupes] + dt
      data <- dplyr::arrange(data, datetime)
    }
  } else if (method == 'interpolate') {
    dt <- as.numeric(lubridate::duration(offset))
    rle_dup <- rle(duplicated(data$datetime))
    for(i in which(rle_dup$values)) {
      dup_range <- sum(rle_dup$lengths[1:i])
      time_range <- data$datetime[dup_range + rle_dup$lengths[i]] - data$datetime[dup_range + 1]
      data$datetime[(dup_range + 1):(dup_range + rle_dup$lengths[i])] <-
        data$datetime[dup_range + 1] + (1:rle_dup$lengths[i]) * as.numeric(time_range) / (rle_dup$lengths[i] + 1)
    }
  } else {
    stop(paste0("`method` must be one of 'keep_first', 'keep_last', 'remove', or 'offset', or 'interpolate' not ", "'", method, "'"))
  }

  # Reset counts if needed
  if (reset_counts) {
    for (column in reset_columns) {
      data[[column]] <- cumsum(!is.na(data[[column]]))
    }
  }

  return(data)
}
