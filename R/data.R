# Port x week panel and group definitions.

SHOCK_DATE <- as.Date("2023-12-18")      # week Maersk, MSC, CMA CGM... suspend Red Sea transits
SHOCK_ALT  <- c(first_attacks = as.Date("2023-11-20"),  # Galaxy Leader seized on 19 Nov 2023
                us_uk_strikes = as.Date("2024-01-08"))  # strikes of 12 Jan 2024
END_MAIN   <- as.Date("2025-01-01")      # before the 2025 US tariffs

CAPE_ISO <- c("ZAF", "NAM", "MUS", "REU", "MOZ", "AGO", "TGO", "SEN", "CPV", "GHA", "CIV")
ADVANCED_ASIA <- c("JPN", "KOR", "TWN", "HKG", "MAC", "SGP", "BRN", "AUS", "NZL")
EAST_AFRICA_CTRL <- c("KEN", "TZA")
LATAM_EXTRA <- c("MEX", "PAN", "CRI", "GTM", "HND", "SLV", "NIC")

GROUP_LEVELS <- c("Red Sea", "Eastern Mediterranean", "Cape route", "Control", "Latin America")

# Groups defined by geography (PortWatch coordinates) and country.
assign_groups <- function(meta) {
  lat <- meta$lat
  lon <- meta$lon
  red <- (lon >= 32 & lon <= 43.6 & lat >= 12.4 & lat <= 30.1) |  # Red Sea
         (lon >= 43 & lon <= 51.5 & lat >= 10 & lat <= 15.5) |    # Gulf of Aden
         (lon >= 32 & lon <= 32.7 & lat > 30.1 & lat <= 31.6)     # Suez Canal (Port Said)
  emed <- !red & lon >= 22 & lon <= 36.5 & lat >= 30 & lat <= 38
  cape <- !red & !emed & (meta$ISO3 %in% CAPE_ISO | grepl("^Las Palmas", meta$portname))
  latam <- meta$continent == "South America" | meta$ISO3 %in% LATAM_EXTRA
  ctrl <- !red & !emed & !cape &
    ((meta$continent == "Asia & Pacific" & !meta$ISO3 %in% ADVANCED_ASIA) |
       meta$ISO3 %in% EAST_AFRICA_CTRL)
  # Offshore oil/gas terminals: dedicated export points, not port calls.
  terminal <- grepl("Offshore|Terminal|LNG", meta$portname)
  meta$group <- dplyr::case_when(
    terminal ~ NA_character_,
    red ~ "Red Sea",
    emed ~ "Eastern Mediterranean",
    cape ~ "Cape route",
    ctrl ~ "Control",
    latam ~ "Latin America",
    TRUE ~ NA_character_
  )
  meta
}

build_panel <- function(weekly_file, meta_file, min_calls = 2) {
  meta <- assign_groups(utils::read.csv(meta_file))
  w <- utils::read.csv(weekly_file)
  w$week <- as.Date(w$week)
  df <- dplyr::inner_join(w, meta[!is.na(meta$group), ], by = "portid")

  # Active ports: at least `min_calls` calls per week on average in 2019-2022.
  size <- df |>
    dplyr::filter(week < as.Date("2023-01-01")) |>
    dplyr::group_by(portid) |>
    dplyr::summarise(pre_calls = mean(calls), pre_container = mean(calls_container))
  df <- dplyr::inner_join(df, size, by = "portid") |>
    dplyr::filter(pre_calls >= min_calls)

  df |>
    dplyr::mutate(
      woy = as.integer(strftime(week, "%V")),
      red = as.integer(group == "Red Sea"),
      emed = as.integer(group == "Eastern Mediterranean"),
      cape = as.integer(group == "Cape route"),
      zaf = as.integer(ISO3 == "ZAF"),
      volume = import + export,
      rel4 = pmin(pmax(floor(as.numeric(week - SHOCK_DATE) / 28), -13), 13)
    )
}

# Main sample: 4 groups, 2019-2024, Latin America excluded (Panama drought).
main_sample <- function(panel) {
  panel[panel$group != "Latin America" & panel$week < END_MAIN, ]
}

summarise_groups <- function(panel) {
  main <- panel[panel$week < END_MAIN, ]
  main |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      ports = dplyr::n_distinct(portid),
      countries = dplyr::n_distinct(ISO3),
      calls_pre = mean(calls[week < SHOCK_DATE]),
      calls_post = mean(calls[week >= SHOCK_DATE]),
      .groups = "drop"
    )
}
