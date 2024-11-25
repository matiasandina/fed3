#' Find FED3 Files
#'
#' Searches for FED3 files that conform to a specific naming pattern within the specified directory.
#' Validates date format in file names.
#'
#' @param inputdir Directory to search for FED3 files.
#' @return A vector of file paths that match the naming pattern and have valid dates.
#' @examples
#' \dontrun{
#' find_fed3_files("path/to/directory")
#' }
#' @export
find_fed3_files <- function(inputdir) {
  pattern <- "^FED[0-9]+_[0-9]{4}[0-9]{2}_[0-9]+\\.CSV$"
  all_files <- list.files(inputdir, pattern = pattern, full.names = TRUE)

  valid_files <- sapply(all_files, function(file) {
    parts <- strsplit(basename(file), "_")[[1]]
    if (length(parts) == 3 && !is.na(lubridate::mdy(parts[2]))) {
      return(file)
    } else {
      message("Invalid date format or structure in file name: ", file)
      return(NULL)
    }
  })

  return(stats::na.omit(valid_files))
}

#' Move and Clean FED3 Files
#'
#' Removes files smaller than 300 bytes and moves remaining files to a specified subdirectory ('FED_data/FED_data').
#'
#' @param files Vector of FED3 file paths to process.
#' @param inputdir Base directory from which files are processed.
#' @param remove_small_files Logical indicating whether to remove small files.
#' @return A list of processed file paths.
move_and_clean_files <- function(files, inputdir, remove_small_files = TRUE) {
  if (remove_small_files) {
    file_sizes <- file.info(files)$size
    small_files <- files[file_sizes < 300]
    if (length(small_files) > 0) {
      cli::cli_alert_warning("Removing {length(small_files)} files smaller than 300 bytes.")
      cli::cli_alert_info("If you don't want this behavior set `remove_small_files` to FALSE.")
      file.remove(small_files)
      files <- setdiff(files, small_files)
    }
  }

  target_dir <- file.path(inputdir, "FED_data", "FED_data")
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Moving files instead of copying
  cli::cli_inform("Moving {length(files)} FED files to {target_dir}")
  sapply(files, function(file) {
    new_path <- file.path(target_dir, basename(file))
    if (!file.rename(file, new_path)) {
      message("Failed to move file: ", file)
    }
    return(new_path)
  }, simplify = FALSE, USE.NAMES = TRUE)

  invisible(files)
}

#' Tidy FED3 Files
#'
#' Handles directory selection, file finding, and file management operations for FED3 files.
#'
#' @param inputdir Optional directory to search for FED3 files.
#'                 If not provided, a directory selection dialog will open.
#' @param remove_small_files Logical indicating whether files smaller than 300 bytes should be removed.
#' @export
#' @examples
#' \dontrun{
#' tidy_fed3_files()
#' }
tidy_fed3_files <- function(inputdir = NULL, remove_small_files = TRUE) {

  if (is.null(inputdir)) {
    inputdir <- choices::choose_directory(initial_dir = "/", title = "Choose directory with FED files")
    if (is.null(inputdir) | is.na(inputdir)) return(invisible(NULL)) # User cancelled selection
  }

  files <- find_fed3_files(inputdir)
  if (length(files) == 0) {
    message("No valid FED3 files found.")
    return(invisible(NULL))
  }

  move_and_clean_files(files, inputdir, remove_small_files)
}
