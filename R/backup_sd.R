#' @title Backup SD card
#' @name backup_sd
#' @details This function finds FED3 files on a selected folder (SD card) and stores them on a user-defined folder.
#' @seealso `fed3::find_files()`, `fed3::copy_files()`
#'
#' This needs interactive session for user to choose folders
#' We assume interactive session likely through Rstudio
#' The core of this function is a moving files function
#' It uses the pattern "FED"
#' It uses the date as MMDDYY to create folders on the backup location

#' @export
backup_sd <- function(sd_card = NULL, backup_dest = NULL){

  # check sd and provide feedback ####
  if (is.null(sd_card) == TRUE) {
    usethis::ui_info("No SD card directory provided, please select manually")
    Sys.sleep(1)
    sd_card <- choices::choose_directory(title = "Select sd card directory")
    if(length(sd_card) == 0) {usethis::ui_stop("No directory selected, run `backup_sd()` again")}
  } else {
    if (fs::dir_exists(sd_card)){
      usethis::ui_info("Searching FED3 files in\n{sd_card}")
    } else {
      usethis::ui_stop("{sd_card} is not a directory")
    }
  }
  # check backup_dest and provide feedback ####
  if (is.null(backup_dest) == TRUE) {
    usethis::ui_info("No backup destination directory provided, please select manually")
    Sys.sleep(1)
    backup_dest <- choices::choose_directory(title = "Select backup folder")
    if(length(backup_dest) == 0) {usethis::ui_stop("No directory selected, run `backup_sd()` again")}
  } else {
    if (fs::dir_exists(sd_card)){
      usethis::ui_info("Checking destination folder\n{backup_dest}")
    } else {
      usethis::ui_stop("{backup_dest} is not a directory")
    }
  }


  # Search files ------------------------------------------------------------
  fed_files <- list.files(path=sd_card,
                          pattern = "FED[0-9]+.+CSV",
                          full.names = T)
  if (length(fed_files) == 0) {usethis::ui_stop("No FED3 files found in {sd_card}\nCheck manually")}
  dates <- stringr::str_extract(fed_files, "_[0-9]+_")
  dates <- stringr::str_remove_all(dates, "_")

  # Create new names --------------------------------------------------------
  df <- tibble::tibble(
    old_files = fed_files,
    # dates are stored as month day year
    dates_parsed = lubridate::mdy(dates),
    year = lubridate::year(dates_parsed),
    mon = lubridate::month(dates_parsed)) %>%
    # generate paths from existing data
    dplyr::mutate(mon = stringr::str_pad(mon, width = 2, pad = 0),
           new_path = file.path(backup_dest, year, mon)) %>%
    # only keep those that actually happened
    dplyr::distinct() %>%
    # generate new files
    dplyr::mutate(new_files = file.path(new_path, basename(old_files)))

  # get the dirs to create
  dirs_to_create <- unique(df$new_path)
  # create them and provide some feedback
  fs::dir_create(dirs_to_create, recurse = T)
  usethis::ui_info("Directories are set up")
  # move files --------------------------------------
  fs::file_move(df$old_files, df$new_files)
  usethis::ui_done("{length(df$old_files)} files were moved to\n{backup_dest}")
}
