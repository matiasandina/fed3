#' @title Find Files
#' @description This function finds all FED files using target dates
#' @md
#' @param target_dates The dates you want to retrieve from `directory`
#' @param directory The directory path where FED3 data is located (default = NULL)
#' @seealso `fed3::copy_files()`
#' @export

find_files <- function(target_dates = NULL,
                       directory = NULL) {
  # Check date input --------------------------------------------------------
  if (is.null(target_dates)) {
    # read config
    usethis::ui_info("No `target_dates`, select config file manually")
    Sys.sleep(1)
    config_path <-
      choices::choose_files(title = "Select config.yaml", method = "rstudioapi")
    config <- yaml::read_yaml(config_path)
    target_dates <-
      c(config$habituation_dates, config$experimental_dates)
    # convert to date format, we assume the config files are written properly
    target_dates <- as.POSIXct(target_dates)
  } else {
    are_dates <- sapply(target_dates, is_date)
    parsed_dates_fail <- any(are_dates == FALSE)
    if (parsed_dates_fail) {
      usethis::ui_stop(
        "Please provide dates in `Date` or `POSIXt` format. Check `target_dates` below\n{target_dates} which were provided as `{class(target_dates)}`"
      )
    }
  }

  # check directory input ---------------------------------------------------
  if (is.null(directory)) {
    usethis::ui_info("Directory was not provided, choose manually")
    directory <- choices::choose_directory(title = "Select FED backup folder",
                                           method = "rstudioapi")
  } else {
    if (fs::dir_exists(directory)) {
      usethis::ui_info("Searching FED3 files in\n{directory}")
    } else {
      usethis::ui_stop("{directory} is not a directory")
    }
  }


  # format dates ------------------------------------------------------------
  # fed dates are given in mmddyy
  fed_dates <- format(target_dates, "%m%d%y")
  coll_fed_dates <- paste(fed_dates, collapse = "|")

  # scan folders -------------------------------------------------------------------------
  # files are stored in year/month subdir
  folders_to_scan <- file.path(
    directory,
    lubridate::year(target_dates),
    stringr::str_pad(
      lubridate::month(target_dates),
      pad = 0,
      width = 2
    )
  )
  folder_to_scan <- unique(folders_to_scan)
  all_files <- list.files(folder_to_scan,
                          pattern = coll_fed_dates,
                          full.names = T)

  return(all_files)

}

#' @title Is Date
#' @description
#' Helper function to check if things are of class `Date` or `POSIXt`.
#' Equivalent to `function(x) {lubridate::is.Date(x) | lubridate::is.POSIXt(x)}`
#' @return `boolean`
is_date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}
