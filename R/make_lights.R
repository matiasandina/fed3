#' @title Make Lights
#' @description
#'  `r lifecycle::badge("experimental")`
#'  This function will make light shading pattern for ggplot plots
#' @param params A `list` that contains a `lights` `vector` with the ON-OFF light cycle
#' @param df A `data.frame` containing a `datetime` column that will provide the ranges of the experimental data
#' @export
make_lights <- function(params, df) {
  # This uses the range in the data
  # If no event is registered for a long time, the range might be wrong
  experiment_range <- range(df$datetime)

  params$fed_dates <- seq(lubridate::date(experiment_range[1]),
                          lubridate::date(experiment_range[2]),
                          "1 day")

  #make the dates
  # this vector will have on, off, on, off..
  # the first value might be before the experiment starts, the last value after experiment ends
  light_changes <-
    lapply(params$fed_dates, function(tt)
      paste(tt, params$lights)) %>%
    unlist()
  #make it datetime
  light_changes <- lubridate::as_datetime(light_changes)
  # subset
  light_changes <-
    light_changes[dplyr::between(light_changes, experiment_range[1], experiment_range[2])]

  light_diff <- diff(hms::as_hms(params$lights))

  # we need even number light changes to make rectangles
  # light changes could be odd when starting and finishing animals at different light cycles
  if (length(light_changes) %% 2 == 1) {
    # It might happen that the first light change is lights-on because
    # the animals started during on lights off
    # check for that
    # if the first time is lights on, add the previous lights off
    if (hms::as_hms(params$lights[1]) == hms::as_hms(light_changes[1])) {
      # calculate the beginning of the lights-off
      # first lights off will be the previous day
      first_lights_off <- dplyr::first(light_changes) - light_diff
      # it was done like this previously "light_changes[1] - lubridate::hours(12)"
      # but it might not be a 12 hs cycle
      light_changes <-
        purrr::prepend(light_changes, first_lights_off)
      print(glue::glue("Adding {first_lights_off}"))
      # the xmin should be the even one in this case
      full_exp_shade <-
        ggplot2::annotate(
          "rect",
          xmin = light_changes[seq_along(light_changes) %% 2 == 0],
          xmax = light_changes[seq_along(light_changes) %% 2 > 0],
          ymin = 0,
          ymax = Inf,
          fill = "gray80",
          alpha = 0.5
        )
    } else {
      # if the first value is lights-off, add the last lights-on
      last_lights_on <- dplyr::last(light_changes) + light_diff
      light_changes <-
        append(light_changes, values = last_lights_on)
      full_exp_shade <-ggplot2::annotate(
          "rect",
          xmin = light_changes[seq_along(light_changes) %% 2 > 0],
          xmax = light_changes[seq_along(light_changes) %% 2 == 0],
          ymin = 0,
          ymax = Inf,
          fill = "gray80",
          alpha = 0.5
        )
    }
  } else {
    full_exp_shade <-
      ggplot2::annotate(
        "rect",
        # the xmin should be the odd one in this case
        xmin = light_changes[seq_along(light_changes) %% 2 > 0],
        xmax = light_changes[seq_along(light_changes) %% 2 == 0],
        ymin = 0,
        ymax = Inf,
        fill = "gray80",
        alpha = 0.5
      )
  }


  return(full_exp_shade)
}


#' StatLights
#'
#' @description
#'  `r lifecycle::badge("experimental")`
#'  A custom ggplot2 extension to shade areas in a plot according to provided light hours.
#'  It shades from lights off to lights on. Usage through [geom_lights()] or [stat_ligths()]
#'
#' @param lights_on_hour An integer representing the hour of the day (0-23) when the lights turn on.
#' @param lights_off_hour An integer representing the hour of the day (0-23) when the lights turn off.
#' @param fill A string representing the color of the shading.
#' @param alpha A numeric representing the transparency of the shading, between 0 (transparent) and 1 (opaque).
#'
#' @return A ggplot2 layer to be added to a ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = Pi_Time, y = Pellet_Count)) +
#'   stat_lights(fill = "grey80", alpha = 0.5, lights_on_hour = 7, lights_off_hour = 19) +
#'   geom_line()
#'
#' }
StatLights <- ggplot2::ggproto("StatLights", ggplot2::Stat,
                               compute_panel = function(self, data, scales, lights_on_hour = 7, lights_off_hour = 19, time_zone = Sys.timezone()) {
                                 # Create a copy of data$x converted to POSIXct
                                 datetime_x <- as.POSIXct(data$x, origin = "1970-01-01", tz = time_zone)

                                 experiment_range <- range(datetime_x)

                                 # Expand the date range to include one day before and after
                                 extended_dates <- seq(lubridate::date(experiment_range[1]) - lubridate::days(1),
                                                       lubridate::date(experiment_range[2]) + lubridate::days(1), "1 day")

                                 # Generate the complete sequence of light changes
                                 light_on_times <- as.POSIXct(glue::glue("{extended_dates} {lights_on_hour}:00:00"), tz = time_zone)
                                 light_off_times <- as.POSIXct(glue::glue("{extended_dates} {lights_off_hour}:00:00"), tz = time_zone)

                                 # If the off hour is before the on hour, shift the off times to the next day
                                 if (lights_off_hour < lights_on_hour) {
                                   light_off_times <- light_off_times + lubridate::days(1)
                                 }

                                 # Combine and sort the light changes
                                 light_changes <- sort(c(light_on_times, light_off_times))

                                 # Filter to match the actual experiment range, including the nearest on/off times
                                 light_changes <- light_changes[dplyr::between(light_changes,
                                                                               experiment_range[1] - lubridate::days(1),
                                                                               experiment_range[2] + lubridate::days(1))]

                                 # Make sure the sequence starts with an off time and ends with an on time
                                 if (!length(light_changes) || (lubridate::hour(light_changes[1]) == lights_on_hour && lights_on_hour != lights_off_hour)) {
                                   light_changes <- light_changes[-1]
                                 }
                                 if (!length(light_changes) || (lubridate::hour(tail(light_changes, 1)) == lights_off_hour && lights_on_hour != lights_off_hour)) {
                                   light_changes <- light_changes[-length(light_changes)]
                                 }

                                 # Extract the shading starts and ends
                                 shading_starts <- light_changes[seq(1, length(light_changes), by = 2)]
                                 shading_ends <- light_changes[seq(2, length(light_changes), by = 2)]

                                 # Create the data frame
                                 df <- data.frame(
                                   xmin = shading_starts,
                                   xmax = shading_ends,
                                   ymin = -Inf,
                                   ymax = Inf
                                 )

                                 # Filter to include shading that starts within the actual experiment range
                                 # or ends within the range
                                 df <- df %>%
                                   dplyr::filter((dplyr::between(xmin, experiment_range[1], experiment_range[2])) |
                                                   (dplyr::between(xmax, experiment_range[1], experiment_range[2])))

                                 return(df)
                               },
                               required_aes = c("x")
)

#' @export
geom_lights <- function(mapping = NULL, data = NULL, position = "identity", ..., fill = "gray60", alpha = 0.5, lights_on_hour = 7, lights_off_hour = 19, time_zone = Sys.timezone()) {
  ggplot2::layer(
    stat = StatLights,
    data = data,
    mapping = mapping,
    geom = ggplot2::GeomRect,
    position = position,
    show.legend = FALSE,
    inherit.aes = TRUE,
    params = list(
      fill = fill,
      alpha = alpha,
      lights_on_hour = lights_on_hour,
      lights_off_hour = lights_off_hour,
      time_zone = time_zone,
      ...
    )
  )
}

stat_lights <- function(mapping = NULL, data = NULL, geom = "lights", position = "identity", ..., fill = "gray60", alpha = 0.5, lights_on_hour = 7, lights_off_hour = 19) {
  ggplot2::layer(
    stat = StatLights,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = FALSE,
    inherit.aes = TRUE,
    params = list(
      fill = fill,
      alpha = alpha,
      lights_on_hour = lights_on_hour,
      lights_off_hour = lights_off_hour,
      ...
    )
  )
}
