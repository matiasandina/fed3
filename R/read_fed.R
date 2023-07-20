#' @title Read Fed Files
#' @name read_fed
#' @param filename file path of csv file produced by FED3
#' @param lib_version `character` with library version to be used for specifying the columns
#' @param deduplicate_method Method to be implemented by `deduplicate_datetime()` to deal with duplicate timestamps recorded by FED machines.
#' @details This function will read the raw data with column specifications (see `fed_col_types()`). It will append the `datetime` by formatting the FED's clock datetime as `yyyy-mm-dd HH:MM:SS`. It will parse the session from the `filename` and append it to the resulting `data.frame`.
#' @return A `data.frame` with parsed columns added to the original FED data.
#' @seealso [fed_col_types()], [deduplicate_datetime()], [readr::read_csv()]
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
    dplyr::mutate(FED = paste0("FED", stringr::str_pad(Device_Number, width=3, pad=0)))
  return(X)
}

#' @title Check Duplicated
#' @description
#' Helper function to check for duplicated entries.
check_duplicated <- function(data, column) {
  return(anyDuplicated(data[[column]]))
}

#' @title Check Duplicated
#' @param data FED data frame as read by `read_fed()`
#' @param method The method to deduplicate the identical timestamps (default is 'offset'). `method` must be one of 'keep_first', 'keep_last', 'remove', or 'offset', or 'interpolate'.
#' @param offset The offset to be added to duplicate datetimes (e.g., '0.1 sec').
#' @param reset_counts whether to reset the `reset_columns` or not (default = FALSE).
#' @param reset_columns The columns to be reset if `reset_counts = TRUE`.
#' @description
#' The precision of the FED clock is in seconds.
#' It might happen that two events have the same exact timestamp.
#' This helper function is used to check for duplicated entries and deduplicate them using methods.
#' This function is internally called by `read_fed()` and the user is advised to first try several examples before modifying `deduplicate_method` in `read_fed()`
#' @examples
#' # data contains datetimes that will fail to parse and are duplicated
#' fed3::duplicate_test_data
#' fed3:::deduplicate_datetime(duplicate_test_data, method = 'keep_first')
#' fed3:::deduplicate_datetime(duplicate_test_data, method = 'offset', offset = "1 sec")
deduplicate_datetime <- function(data, method = 'offset', offset = '0.1 sec',
                        reset_counts = FALSE,
                        reset_columns = c('Pellet_Count', 'Left_Poke_Count', 'Right_Poke_Count')) {
  # This would only happen with test data, FED data should have datetime column
  if(is.character(duplicate_test_data$datetime)){
    data$datetime <- lubridate::parse_date_time(data$datetime, "ymd HMS")
  }

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
