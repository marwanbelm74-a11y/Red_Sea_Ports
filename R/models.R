# Estimation. Port x week-of-year fixed effects (port-specific seasonality) and
# week fixed effects (global shocks). Standard errors clustered by country.

FE <- "portid^woy + week"

add_post <- function(df, shock = SHOCK_DATE) {
  post <- as.integer(df$week >= shock)
  df$post_red <- post * df$red
  df$post_emed <- post * df$emed
  df$post_cape <- post * df$cape
  df$post <- post
  df
}

# Event study in 4-week periods, reference = the 4 weeks before the shock.
# The endpoints (-13, +13) bin the more distant periods.
estimate_event_study <- function(panel) {
  fixest::feols(
    stats::as.formula(paste(
      "log1p(calls) ~ i(rel4, red, ref = -1) + i(rel4, emed, ref = -1) +",
      "i(rel4, cape, ref = -1) |", FE)),
    data = main_sample(panel), cluster = ~ISO3
  )
}

# Difference-in-differences and robustness checks.
estimate_did <- function(panel) {
  rhs <- paste("~ post_red + post_emed + post_cape |", FE)
  f <- function(y) stats::as.formula(paste(y, rhs))
  base <- add_post(main_sample(panel))
  with_latam <- panel[panel$week < END_MAIN, ]  # Latin America added to the controls
  list(
    "Calls" = fixest::feols(f("log1p(calls)"), base, cluster = ~ISO3),
    "Container" = fixest::feols(f("log1p(calls_container)"), base, cluster = ~ISO3),
    "Volumes" = fixest::feols(f("log1p(volume)"), base, cluster = ~ISO3),
    "Since 2022" = fixest::feols(f("log1p(calls)"),
                                 base[base$week >= as.Date("2022-01-01"), ], cluster = ~ISO3),
    "With Lat. Am." = fixest::feols(f("log1p(calls)"), add_post(with_latam), cluster = ~ISO3),
    "Shock Nov. 2023" = fixest::feols(f("log1p(calls)"),
                                      add_post(main_sample(panel), SHOCK_ALT[[1]]), cluster = ~ISO3),
    "Shock Jan. 2024" = fixest::feols(f("log1p(calls)"),
                                      add_post(main_sample(panel), SHOCK_ALT[[2]]), cluster = ~ISO3)
  )
}

# PortWatch <-> CPPI matching: normalised names + manual correspondences.
CPPI_MANUAL <- c("El Sokhna" = "SOKHNA",
                 "Las Palmas (de Gran Canaria)" = "LAS PALMAS",
                 "Porto Da Praia" = "PRAIA")

normalise_name <- function(x) {
  x <- toupper(iconv(x, to = "ASCII//TRANSLIT"))
  x <- gsub("\\s*\\(.*\\)", "", x)
  trimws(gsub("[^A-Z ]", " ", x)) |> gsub(pattern = "\\s+", replacement = " ")
}

match_cppi <- function(panel, cppi) {
  ports <- unique(panel[, c("portid", "portname", "ISO3", "group")])
  key <- ifelse(ports$portname %in% names(CPPI_MANUAL),
                CPPI_MANUAL[ports$portname], ports$portname)
  ports$key <- normalise_name(key)
  cppi$key <- normalise_name(cppi$cppi_name)
  cppi <- cppi[!duplicated(cppi$key), ]
  out <- merge(ports, cppi[, c("key", "cppi_name", "cppi_rank")], by = "key", all.x = TRUE)
  out[, c("portid", "portname", "ISO3", "group", "cppi_name", "cppi_rank")]
}

# Did diverted traffic stop in South Africa? Does port efficiency matter?
estimate_heterogeneity <- function(panel, cppi_match) {
  d <- add_post(main_sample(panel))
  d <- d[d$group %in% c("Cape route", "Control"), ]
  d$post_cape_zaf <- d$post_cape * d$zaf
  m1 <- fixest::feols(stats::as.formula(paste("log1p(calls) ~ post_cape + post_cape_zaf |", FE)),
                      d, cluster = ~ISO3)
  m2 <- fixest::feols(stats::as.formula(paste("log1p(calls_container) ~ post_cape + post_cape_zaf |", FE)),
                      d, cluster = ~ISO3)
  # Container ports ranked in the CPPI 2023 (standardised rank: + = less efficient)
  dc <- merge(d, cppi_match[!is.na(cppi_match$cppi_rank), c("portid", "cppi_rank")], by = "portid")
  dc$cppi_z <- as.numeric(scale(dc$cppi_rank))
  dc$post_cppi <- dc$post * dc$cppi_z
  dc$post_cape_cppi <- dc$post_cape * dc$cppi_z
  m3 <- fixest::feols(stats::as.formula(paste(
    "log1p(calls_container) ~ post_cape + post_cppi + post_cape_cppi |", FE)),
    dc, cluster = ~ISO3)
  # Robustness: without Port Elizabeth, the largest drop in the region.
  m4 <- fixest::feols(stats::as.formula(paste("log1p(calls) ~ post_cape + post_cape_zaf |", FE)),
                      d[d$portname != "Port Elizabeth", ], cluster = ~ISO3)
  list("Calls" = m1, "Container" = m2, "Container (CPPI)" = m3,
       "Excl. Port Elizabeth" = m4)
}

# Permutation inference: South Africa is a single country (a single cluster), so
# the clustered standard error of post_cape_zaf is unreliable. We re-estimate the
# model assigning South Africa's role to each Cape-route country in turn, and
# locate the South African coefficient in that distribution.
placebo_countries <- function(panel) {
  d <- add_post(main_sample(panel))
  d <- d[d$group %in% c("Cape route", "Control"), ]
  countries <- sort(unique(d$ISO3[d$group == "Cape route"]))
  coefs <- vapply(countries, function(c) {
    d$post_cape_c <- d$post_cape * as.integer(d$ISO3 == c)
    m <- fixest::feols(stats::as.formula(paste("log1p(calls) ~ post_cape + post_cape_c |", FE)),
                       d, notes = FALSE)
    stats::coef(m)[["post_cape_c"]]
  }, numeric(1))
  out <- data.frame(ISO3 = countries, coef = coefs)
  out$rank <- rank(out$coef)
  out
}

# Change by port, net of the average change in control ports (log x 100).
port_changes <- function(panel, cppi_match) {
  d <- main_sample(panel)
  d <- d[d$week >= as.Date("2022-01-03"), ]
  d$period <- ifelse(d$week >= SHOCK_DATE, "post", "pre")
  agg <- d |>
    dplyr::group_by(portid, portname, ISO3, group, period) |>
    dplyr::summarise(calls = mean(calls), .groups = "drop") |>
    tidyr::pivot_wider(names_from = period, values_from = calls) |>
    dplyr::mutate(dlog = 100 * (log1p(post) - log1p(pre)))
  ctrl <- mean(agg$dlog[agg$group == "Control"])
  agg$dlog_net <- agg$dlog - ctrl
  agg <- merge(agg, cppi_match[, c("portid", "cppi_rank")], by = "portid", all.x = TRUE)
  attr(agg, "control_change") <- ctrl
  agg
}

# Suez Canal revenue reconstructed from PortWatch transits.
suez_revenue <- function(choke_file, revenue_file) {
  ch <- utils::read.csv(choke_file)
  ch$date <- as.Date(ch$date)
  s <- ch[ch$portname == "Suez Canal", ]
  s$year <- as.integer(format(s$date, "%Y"))
  yr <- stats::aggregate(cbind(n_total, capacity) ~ year, data = s[s$year %in% 2023:2024, ], FUN = sum)
  rev <- utils::read.csv(revenue_file)
  r23 <- rev$revenue_bn_usd[rev$year == 2023]
  data.frame(
    year = 2024,
    actual = rev$revenue_bn_usd[rev$year == 2024],
    pred_count = r23 * yr$n_total[yr$year == 2024] / yr$n_total[yr$year == 2023],
    pred_capacity = r23 * yr$capacity[yr$year == 2024] / yr$capacity[yr$year == 2023],
    transits_change = yr$n_total[yr$year == 2024] / yr$n_total[yr$year == 2023] - 1,
    capacity_change = yr$capacity[yr$year == 2024] / yr$capacity[yr$year == 2023] - 1,
    revenue_2023 = r23
  )
}
