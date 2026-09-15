# Port x week panel

shock_date         <- as.Date("2023-12-18")  # carriers suspend Red Sea transits
first_attacks_date <- as.Date("2023-11-20")
us_uk_strikes_date <- as.Date("2024-01-08")
end_date           <- as.Date("2025-01-01")  # before the 2025 US tariffs

cape_countries  <- c("ZAF", "NAM", "MUS", "REU", "MOZ", "AGO",
                     "TGO", "SEN", "CPV", "GHA", "CIV")
rich_asia       <- c("JPN", "KOR", "TWN", "HKG", "MAC", "SGP", "BRN", "AUS", "NZL")
east_africa     <- c("KEN", "TZA")
central_america <- c("MEX", "PAN", "CRI", "GTM", "HND", "SLV", "NIC")


assign_groups <- function(ports) {

  in_red_sea   <- ports$lon >= 32 & ports$lon <= 43.6 & ports$lat >= 12.4 & ports$lat <= 30.1
  in_gulf_aden <- ports$lon >= 43 & ports$lon <= 51.5 & ports$lat >= 10 & ports$lat <= 15.5
  in_suez      <- ports$lon >= 32 & ports$lon <= 32.7 & ports$lat > 30.1 & ports$lat <= 31.6
  is_red <- in_red_sea | in_gulf_aden | in_suez

  is_emed <- !is_red & ports$lon >= 22 & ports$lon <= 36.5 &
    ports$lat >= 30 & ports$lat <= 38

  is_cape <- !is_red & !is_emed &
    (ports$ISO3 %in% cape_countries | grepl("^Las Palmas", ports$portname))

  is_control <- !is_red & !is_emed & !is_cape &
    ((ports$continent == "Asia & Pacific" & !ports$ISO3 %in% rich_asia) |
       ports$ISO3 %in% east_africa)

  is_latam <- ports$continent == "South America" | ports$ISO3 %in% central_america

  is_terminal <- grepl("Offshore|Terminal|LNG", ports$portname)

  ports$group <- case_when(
    is_terminal ~ NA_character_,
    is_red      ~ "Red Sea",
    is_emed     ~ "Eastern Mediterranean",
    is_cape     ~ "Cape route",
    is_control  ~ "Control",
    is_latam    ~ "Latin America",
    TRUE        ~ NA_character_
  )
  ports
}


build_panel <- function(weekly_file, ports_file) {

  ports <- read.csv(ports_file)
  ports <- assign_groups(ports)
  ports <- filter(ports, !is.na(group))

  weekly <- read.csv(weekly_file)
  weekly$week <- as.Date(weekly$week)

  panel <- inner_join(weekly, ports, by = "portid")

  # active ports: at least 2 calls per week in 2019-2022
  port_size <- panel |>
    filter(week < as.Date("2023-01-01")) |>
    group_by(portid) |>
    summarise(pre_calls = mean(calls),
              pre_container = mean(calls_container))

  panel <- panel |>
    inner_join(port_size, by = "portid") |>
    filter(pre_calls >= 2)

  panel |>
    mutate(
      woy    = as.integer(strftime(week, "%V")),
      red    = as.integer(group == "Red Sea"),
      emed   = as.integer(group == "Eastern Mediterranean"),
      cape   = as.integer(group == "Cape route"),
      zaf    = as.integer(ISO3 == "ZAF"),
      volume = import + export,
      rel4   = floor(as.numeric(week - shock_date) / 28),  # 4-week periods
      rel4   = pmin(pmax(rel4, -13), 13)
    )
}


main_sample <- function(panel) {
  filter(panel, group != "Latin America", week < end_date)
}


summarise_groups <- function(panel) {
  panel |>
    filter(week < end_date) |>
    group_by(group) |>
    summarise(
      ports      = n_distinct(portid),
      countries  = n_distinct(ISO3),
      calls_pre  = mean(calls[week < shock_date]),
      calls_post = mean(calls[week >= shock_date])
    )
}
