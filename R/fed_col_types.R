#' @title "Fed Column Types"
#' @name fed_col_types
#' @param version character version of FED_library used to collect data
fed_col_types <- function(lib_version=NULL){

  # this list is hardcoded
  col_classes <-
    list(
      # this is for fedwatcher
      list(version = "fw",
           columns = readr::cols(
             `MM:DD:YYYY hh:mm:ss` = readr::col_character(),
             Library_Version = readr::col_character(),
             Session_type = readr::col_character(),
             Device_Number = readr::col_double(),
             Battery_Voltage = readr::col_double(),
             Motor_Turns = readr::col_double(),
             FR = readr::col_double(),
             Event = readr::col_character(),
             Active_Poke = readr::col_character(),
             Left_Poke_Count = readr::col_double(),
             Right_Poke_Count = readr::col_double(),
             Pellet_Count = readr::col_double(),
             Block_Pellet_Count = readr::col_double(),
             Retrieval_Time = readr::col_double(),
             # this is the one we changed so it doesn't have Timed_out
             InterPellet_Retrieval_Time = readr::col_double(),
             Poke_Time = readr::col_double())
      ),
      list(version = "1.8.1",
           columns = readr::cols(
             `MM:DD:YYYY hh:mm:ss` = readr::col_character(),
             Library_Version = readr::col_character(),
             Session_type = readr::col_character(),
             Device_Number = readr::col_double(),
             Battery_Voltage = readr::col_double(),
             Motor_Turns = readr::col_double(),
             FR = readr::col_double(),
             Event = readr::col_character(),
             Active_Poke = readr::col_character(),
             Left_Poke_Count = readr::col_double(),
             Right_Poke_Count = readr::col_double(),
             Pellet_Count = readr::col_double(),
             Block_Pellet_Count = readr::col_double(),
             Retrieval_Time = readr::col_double(),
             InterPelletInterval = readr::col_double(),
             Poke_Time = readr::col_double())
      ),
      list(version = "1.9.2",
           columns = readr::cols(
             `MM:DD:YYYY hh:mm:ss` = readr::col_character(),
             Library_Version = readr::col_character(),
             Session_type = readr::col_character(),
             Device_Number = readr::col_double(),
             Battery_Voltage = readr::col_double(),
             Motor_Turns = readr::col_double(),
             FR = readr::col_double(),
             Event = readr::col_character(),
             Active_Poke = readr::col_character(),
             Left_Poke_Count = readr::col_double(),
             Right_Poke_Count = readr::col_double(),
             Pellet_Count = readr::col_double(),
             Block_Pellet_Count = readr::col_double(),
             Retrieval_Time = readr::col_double(),
             InterPelletInterval = readr::col_double(),
             Poke_Time = readr::col_double())
      )
    )

  if(is.null(lib_version) == FALSE && is.character(lib_version) == FALSE){
    stop(glue::glue("Version must be NULL or character, it was {class(lib_version)}"))
  }
  # gather available versions in hardcoded list
  versions <- unlist(purrr::map(col_classes, "version"))
  if(is.null(lib_version)) {
    # default to last one if we can't find the version
    message(glue::glue("FED lib_version not provided, using {dplyr::last(versions)}"))
    return(dplyr::last(col_classes)$columns)
  } else if(lib_version %in% versions == FALSE){
    message(glue::glue("FED lib_version {lib_version} not found in verions, using {dplyr::last(versions)}"))
    return(dplyr::last(col_classes)$columns)
  } else {
    version_index <- stringr::str_which(string = versions, pattern = lib_version)
    return(col_classes[[version_index]]$columns)
  }

}

