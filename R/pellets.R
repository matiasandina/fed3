#' @export
filter_pellets <- function(df){
  df %>% dplyr::filter(Event == "Pellet")
}

#' This function recalculates pellets if given a FED_data `data.frame` that contains an identifier column `ID`
#' The basic thought behind it is that one animal can receive more than one FED device
#' This might happen due to the experiment design or because a FED needed to be replaced during the experiment
#' @export
recalculate_pellets <- function(df){
  df %>%
    filter_pellets() %>%
    dplyr::arrange(ID, datetime) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(pellets = 1:length(Pellet_Count))
}
