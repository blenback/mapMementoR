#' Save Powerof10 athlete race data to YAML
#' @param first_name Athlete's first name
#' @param surname Athlete's surname
#' @param club Athlete's club
#' @param yaml_path Path to save the YAML file (default: "<first_name>_<surname>_races.yaml")
#' @param event Optional vector of events to filter e.g "HM", "Mar", "5K "parkrun" etc.
#' @param year Optional vector of years to filter
#' @return Saves a YAML file with the athlete's race data
#' @export
save_powerof10_to_yaml <- function(
  first_name,
  surname,
  club,
  yaml_path = NULL,
  event = NULL,
  year = NULL
) {
  if (base::is.null(yaml_path)) {
    yaml_path <- base::tolower(base::paste0(
      first_name,
      "_",
      surname,
      "_races.yaml"
    ))
  }
  athlete <- get_athlete(fn = first_name, sn = surname, club = club)
  if (base::is.null(athlete)) {
    base::stop("No athlete found.")
  }
  df <- athlete
  df$race_year <- base::as.integer(base::paste0(
    "20",
    base::sub(".* (\\d{2})$", "\\1", df$date)
  ))
  if (!base::is.null(event)) {
    df <- df[df$event %in% event, ]
  }
  if (!base::is.null(year)) {
    df <- df[df$race_year %in% year, ]
  }
  df$event_location <- base::paste(df$event, df$venue, sep = "__SEP__")
  split_list <- base::split(df, df$event_location)
  race_list <- base::unname(base::lapply(split_list, function(event_races) {
    event_name <- base::unique(event_races$event)[1]
    location_name <- base::unique(event_races$venue)[1]
    base::list(
      gpx_file = "",
      competitor_name = base::paste(first_name, surname),
      event = event_name,
      location = location_name,
      entries = base::lapply(
        base::seq_len(base::nrow(event_races)),
        function(i) {
          base::list(
            race_year = base::as.character(event_races$race_year[i]),
            race_time = base::as.character(event_races$perf[i])
          )
        }
      )
    )
  }))
  yaml::write_yaml(base::list(races = race_list), yaml_path)
}
