# Regressions
# Fixed effects: port x week of year (seasonality) and week (global shocks).
# Standard errors clustered by country.

add_post <- function(df, shock = shock_date) {
  df$post      <- as.integer(df$week >= shock)
  df$post_red  <- df$post * df$red
  df$post_emed <- df$post * df$emed
  df$post_cape <- df$post * df$cape
  df
}


estimate_event_study <- function(panel) {
  feols(
    log1p(calls) ~ i(rel4, red, ref = -1) + i(rel4, emed, ref = -1) + i(rel4, cape, ref = -1) |
      portid^woy + week,
    data = main_sample(panel),
    cluster = ~ISO3
  )
}


estimate_did <- function(panel) {

  main <- add_post(main_sample(panel))

  m_calls <- feols(log1p(calls) ~ post_red + post_emed + post_cape | portid^woy + week,
                   data = main, cluster = ~ISO3)

  m_container <- feols(log1p(calls_container) ~ post_red + post_emed + post_cape | portid^woy + week,
                       data = main, cluster = ~ISO3)

  m_volumes <- feols(log1p(volume) ~ post_red + post_emed + post_cape | portid^woy + week,
                     data = main, cluster = ~ISO3)

  since_2022 <- filter(main, week >= as.Date("2022-01-01"))
  m_2022 <- feols(log1p(calls) ~ post_red + post_emed + post_cape | portid^woy + week,
                  data = since_2022, cluster = ~ISO3)

  with_latam <- add_post(filter(panel, week < end_date))
  m_latam <- feols(log1p(calls) ~ post_red + post_emed + post_cape | portid^woy + week,
                   data = with_latam, cluster = ~ISO3)

  shock_nov <- add_post(main_sample(panel), shock = first_attacks_date)
  m_nov <- feols(log1p(calls) ~ post_red + post_emed + post_cape | portid^woy + week,
                 data = shock_nov, cluster = ~ISO3)

  shock_jan <- add_post(main_sample(panel), shock = us_uk_strikes_date)
  m_jan <- feols(log1p(calls) ~ post_red + post_emed + post_cape | portid^woy + week,
                 data = shock_jan, cluster = ~ISO3)

  list("Calls"           = m_calls,
       "Container"       = m_container,
       "Volumes"         = m_volumes,
       "Since 2022"      = m_2022,
       "With Lat. Am."   = m_latam,
       "Shock Nov. 2023" = m_nov,
       "Shock Jan. 2024" = m_jan)
}


# Matching PortWatch names to CPPI names

cppi_name_fixes <- c("El Sokhna"                    = "SOKHNA",
                     "Las Palmas (de Gran Canaria)" = "LAS PALMAS",
                     "Porto Da Praia"               = "PRAIA")

clean_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("\\s*\\(.*\\)", "", x)
  x <- gsub("[^A-Z ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

match_cppi <- function(panel, cppi) {
  ports <- distinct(panel, portid, portname, ISO3, group)

  ports$key <- ports$portname
  needs_fix <- ports$portname %in% names(cppi_name_fixes)
  ports$key[needs_fix] <- cppi_name_fixes[ports$portname[needs_fix]]
  ports$key <- clean_name(ports$key)

  cppi$key <- clean_name(cppi$cppi_name)
  cppi <- cppi[!duplicated(cppi$key), ]

  matched <- left_join(ports, cppi, by = "key")
  select(matched, portid, portname, ISO3, group, cppi_name, cppi_rank)
}


estimate_heterogeneity <- function(panel, cppi_match) {

  df <- add_post(main_sample(panel))
  df <- filter(df, group %in% c("Cape route", "Control"))
  df$post_cape_zaf <- df$post_cape * df$zaf

  m_zaf <- feols(log1p(calls) ~ post_cape + post_cape_zaf | portid^woy + week,
                 data = df, cluster = ~ISO3)

  m_zaf_container <- feols(log1p(calls_container) ~ post_cape + post_cape_zaf | portid^woy + week,
                           data = df, cluster = ~ISO3)

  # standardised CPPI rank: higher = less efficient
  ranked <- cppi_match |>
    filter(!is.na(cppi_rank)) |>
    select(portid, cppi_rank)
  df_cppi <- inner_join(df, ranked, by = "portid")
  df_cppi$cppi_z         <- as.numeric(scale(df_cppi$cppi_rank))
  df_cppi$post_cppi      <- df_cppi$post * df_cppi$cppi_z
  df_cppi$post_cape_cppi <- df_cppi$post_cape * df_cppi$cppi_z

  m_cppi <- feols(log1p(calls_container) ~ post_cape + post_cppi + post_cape_cppi | portid^woy + week,
                  data = df_cppi, cluster = ~ISO3)

  df_no_pe <- filter(df, portname != "Port Elizabeth")
  m_no_pe <- feols(log1p(calls) ~ post_cape + post_cape_zaf | portid^woy + week,
                   data = df_no_pe, cluster = ~ISO3)

  list("Calls"                = m_zaf,
       "Container"            = m_zaf_container,
       "Container (CPPI)"     = m_cppi,
       "Excl. Port Elizabeth" = m_no_pe)
}


# South Africa is a single cluster, so its standard error is unreliable.
# Rerun the model with each Cape-route country in South Africa's place.
placebo_countries <- function(panel) {

  df <- add_post(main_sample(panel))
  df <- filter(df, group %in% c("Cape route", "Control"))

  countries <- sort(unique(df$ISO3[df$group == "Cape route"]))
  coefs <- numeric(length(countries))

  for (i in seq_along(countries)) {
    df$post_cape_c <- df$post_cape * as.integer(df$ISO3 == countries[i])
    m <- feols(log1p(calls) ~ post_cape + post_cape_c | portid^woy + week,
               data = df, notes = FALSE)
    coefs[i] <- coef(m)["post_cape_c"]
  }

  result <- data.frame(ISO3 = countries, coef = coefs)
  result$rank <- rank(result$coef)
  result
}


port_changes <- function(panel, cppi_match) {

  df <- main_sample(panel)
  df <- filter(df, week >= as.Date("2022-01-03"))
  df$period <- ifelse(df$week >= shock_date, "post", "pre")

  by_port <- df |>
    group_by(portid, portname, ISO3, group, period) |>
    summarise(calls = mean(calls), .groups = "drop") |>
    pivot_wider(names_from = period, values_from = calls) |>
    mutate(dlog = 100 * (log1p(post) - log1p(pre)))

  control_change <- mean(by_port$dlog[by_port$group == "Control"])
  by_port$dlog_net <- by_port$dlog - control_change

  by_port <- left_join(by_port, select(cppi_match, portid, cppi_rank), by = "portid")
  attr(by_port, "control_change") <- control_change
  by_port
}


# revenue 2024 = revenue 2023 x (traffic 2024 / traffic 2023)
suez_revenue <- function(choke_file, revenue_file) {

  suez <- read.csv(choke_file) |>
    filter(portname == "Suez Canal") |>
    mutate(year = as.integer(substr(date, 1, 4))) |>
    filter(year %in% c(2023, 2024)) |>
    group_by(year) |>
    summarise(transits = sum(n_total), capacity = sum(capacity))

  revenue <- read.csv(revenue_file)
  rev_2023 <- revenue$revenue_bn_usd[revenue$year == 2023]
  rev_2024 <- revenue$revenue_bn_usd[revenue$year == 2024]

  transits_ratio <- suez$transits[suez$year == 2024] / suez$transits[suez$year == 2023]
  capacity_ratio <- suez$capacity[suez$year == 2024] / suez$capacity[suez$year == 2023]

  data.frame(
    year            = 2024,
    actual          = rev_2024,
    pred_count      = rev_2023 * transits_ratio,
    pred_capacity   = rev_2023 * capacity_ratio,
    transits_change = transits_ratio - 1,
    capacity_change = capacity_ratio - 1,
    revenue_2023    = rev_2023
  )
}
