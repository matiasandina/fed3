#' @title Copy Files
#' @description This function copies files from a path to a new folder at destiny. This function will also delete empty files, but it will not delete very small files.
#' @seealso `[find_files()]`
#' @export

copy_files <- function(file_list = NULL, from_dir = NULL, dest_dir = NULL) {

  # check file list ---------------------------------------------------------
  if (is.null(file_list)){
    usethis::ui_info("File list not provided, calling find_files()")
    Sys.sleep(1)
    file_list <- find_files()
  }

  # check source directory  ------------------------------------------------------------
  if (is.null(dest_dir)) {
    usethis::ui_info("Destination not provided, choose manually")
    dest_dir <- choices::choose_directory(title = "Choose destination directory",
                                          method = "rstudioapi")
  }

  # check destination -------------------------------------------------------
  if (is.null(dest_dir)) {
    usethis::ui_info("Destination not provided, choose manually")
    dest_dir <- choices::choose_directory(title = "Choose destination directory",
                                            method = "rstudioapi")
  }
  # create new folder called FED_data
  new_fed <- file.path(target_dir, "FED_data")
  fs::dir_create(new_fed)
  new_files <- file.path(new_fed, basename(file_list))

  file.copy(from = file_list, new_fed)
  # Feedback
  usethis::ui_done(glue::glue("{length(new_files)} FED files were copied"))
  # remove empty files
  file_df <- data.frame(
    new_files = new_files,
    size = file.size(new_files),
    stringsAsFactors = FALSE
  )

  files_to_remove <- file_df[file_df$size == 0,]$new_files
  file.remove(files_to_remove)
  usethis::ui_info(glue::glue("{length(files_to_remove)} files were empty and thus removed from selected folder"))
  usethis::ui_line("You can find the data at:")
  usethis::ui_path(glue::glue("{new_fed}"))
}
