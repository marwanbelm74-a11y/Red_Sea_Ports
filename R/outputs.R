# Figures, tables and numbers for the paper

group_colors <- c("Red Sea"               = "#C0392B",
                  "Eastern Mediterranean" = "#E67E22",
                  "Cape route"            = "#2471A3",
                  "Control"               = "#7F8C8D")

my_theme <- function() {
  theme_minimal(base_size = 10) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank())
}


plot_chokepoints <- function(choke_file, file) {

  choke <- read.csv(choke_file)
  choke$date <- as.Date(choke$date)

  chokepoints <- c("Suez Canal", "Bab el-Mandeb Strait", "Cape of Good Hope")

  weekly <- choke |>
    filter(portname %in% chokepoints) |>
    mutate(week = date - (as.integer(format(date, "%u")) - 1)) |>
    group_by(portname, week) |>
    summarise(transits = sum(n_total), days = n(), .groups = "drop") |>
    filter(days == 7)

  weekly$portname <- factor(weekly$portname,
                            levels = chokepoints,
                            labels = c("Suez Canal", "Bab el-Mandeb", "Cape of Good Hope"))

  p <- ggplot(weekly, aes(week, transits, colour = portname)) +
    geom_line(linewidth = 0.4) +
    geom_vline(xintercept = shock_date, linetype = 2) +
    scale_colour_manual(values = c("#C0392B", "#E67E22", "#2471A3")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(x = NULL, y = "Transits per week", colour = NULL) +
    my_theme()

  ggsave(file, p, width = 6.5, height = 3.4)
  file
}


plot_event_study <- function(event_study, file) {

  # coefficient names look like "rel4::-5:red"
  coefs <- as.data.frame(coeftable(event_study))
  coefs$term <- rownames(coefs)
  coefs$period <- as.integer(sub("^rel4::(-?\\d+):.*$", "\\1", coefs$term))
  coefs$var <- sub("^.*:(\\w+)$", "\\1", coefs$term)
  coefs$estimate <- coefs$Estimate
  coefs$se <- coefs$`Std. Error`

  reference <- data.frame(period = -1, var = c("red", "emed", "cape"), estimate = 0, se = 0)
  coefs <- rbind(coefs[, c("period", "var", "estimate", "se")], reference)
  coefs <- filter(coefs, abs(period) <= 12)

  coefs$group <- factor(coefs$var, levels = c("red", "emed", "cape"),
                        labels = c("Red Sea", "Eastern Mediterranean", "Cape route"))

  p <- ggplot(coefs, aes(period, estimate, colour = group, fill = group)) +
    geom_hline(yintercept = 0, colour = "grey40") +
    geom_vline(xintercept = -0.5, linetype = 2) +
    geom_ribbon(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                alpha = 0.15, colour = NA) +
    geom_line() +
    geom_point(size = 1) +
    facet_wrap(~group, nrow = 1) +
    scale_colour_manual(values = group_colors, guide = "none") +
    scale_fill_manual(values = group_colors, guide = "none") +
    labs(x = "4-week periods since 18 December 2023",
         y = "Effect on log(1 + port calls)") +
    my_theme()

  ggsave(file, p, width = 6.5, height = 3)
  file
}


key_ports <- c("Djibouti", "Jeddah", "King Abdullah Port", "Aqaba", "Port Sudan",
               "El Sokhna", "Port Said", "Hudaydah (Hodeidah)", "Aden", "Piraeus",
               "Mersin", "Damietta", "Alexandria", "Haifa", "Durban", "Cape Town",
               "Ngqura", "Port Elizabeth", "Port Louis", "Walvis Bay", "Lome",
               "Las Palmas (de Gran Canaria)", "Abidjan", "Tema", "Dakar", "Luanda",
               "Maputo", "Porto Da Praia")

plot_winners_losers <- function(changes, file) {

  df <- filter(changes, portname %in% key_ports)

  df$label <- sub(" \\(.*\\)", "", df$portname)
  has_rank <- !is.na(df$cppi_rank)
  df$label[has_rank] <- paste0(df$label[has_rank], " [", df$cppi_rank[has_rank], "]")
  df$label[df$ISO3 == "ZAF"] <- paste0(df$label[df$ISO3 == "ZAF"], " *")

  df$label <- factor(df$label, levels = df$label[order(df$dlog_net)])

  p <- ggplot(df, aes(dlog_net, label, fill = group)) +
    geom_col() +
    geom_vline(xintercept = 0) +
    scale_fill_manual(values = group_colors) +
    labs(x = "Change in port calls, net of control ports (log points x 100)",
         y = NULL, fill = NULL) +
    my_theme()

  ggsave(file, p, width = 6.5, height = 5.5)
  file
}


variable_names <- c(
  post_red       = "Post $\\times$ Red Sea",
  post_emed      = "Post $\\times$ E. Mediterranean",
  post_cape      = "Post $\\times$ Cape route",
  post_cape_zaf  = "Post $\\times$ Cape $\\times$ South Africa",
  post_cppi      = "Post $\\times$ CPPI rank (std.)",
  post_cape_cppi = "Post $\\times$ Cape $\\times$ CPPI rank (std.)",
  "log1p(calls)"           = "log(1 + calls)",
  "log1p(calls_container)" = "log(1 + container calls)",
  "log1p(volume)"          = "log(1 + volumes)",
  portid = "Port", woy = "Week of year", week = "Week", ISO3 = "country"
)

save_regression_table <- function(models, file) {
  etable(models,
         tex = TRUE, file = file, replace = TRUE,
         dict = variable_names,
         headers = names(models),
         digits = 3,
         fitstat = ~n + r2,
         signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
         style.tex = style.tex("aer"))
  file
}


save_groups_table <- function(groups, file) {

  order <- c("Red Sea", "Eastern Mediterranean", "Cape route", "Control", "Latin America")
  g <- groups[match(order, groups$group), ]
  change <- 100 * (g$calls_post / g$calls_pre - 1)

  rows <- sprintf("%s & %d & %d & %.1f & %.1f & %+.1f\\%% \\\\",
                  g$group, g$ports, g$countries, g$calls_pre, g$calls_post, change)

  lines <- c("\\begin{tabular}{lrrrrr}",
             "\\toprule",
             "Group & Ports & Countries & \\multicolumn{2}{c}{Calls per week} & Change \\\\",
             " & & & before & after & \\\\",
             "\\midrule",
             rows,
             "\\bottomrule",
             "\\end{tabular}")
  writeLines(lines, file, useBytes = TRUE)
  file
}


save_suez_table <- function(suez, file) {
  lines <- c(
    "\\begin{tabular}{lr}",
    "\\toprule",
    "& USD billion \\\\",
    "\\midrule",
    sprintf("2023 revenue (observed) & %.2f \\\\", suez$revenue_2023),
    sprintf("2024 predicted: number of transits (%+.0f\\%%) & %.2f \\\\",
            100 * suez$transits_change, suez$pred_count),
    sprintf("2024 predicted: vessel capacity (%+.0f\\%%) & %.2f \\\\",
            100 * suez$capacity_change, suez$pred_capacity),
    sprintf("2024 revenue (observed) & %.2f \\\\", suez$actual),
    "\\bottomrule",
    "\\end{tabular}"
  )
  writeLines(lines, file, useBytes = TRUE)
  file
}


# Numbers quoted in main.tex, written as LaTeX macros (e.g. \SuezPre)

fmt_number  <- function(x, digits = 0) paste0("$", formatC(x, format = "f", digits = digits, big.mark = "{,}"), "$")
fmt_signed  <- function(x) sprintf("$%+.0f$", x)
fmt_percent <- function(b) sprintf("$%.0f$", 100 * (exp(b) - 1))
latex_macro <- function(name, value) paste0("\\newcommand{\\", name, "}{", value, "}")

save_numbers <- function(choke_file, did, het, changes, suez, groups, placebo, file) {

  choke <- read.csv(choke_file)
  choke$date <- as.Date(choke$date)

  weekly_avg <- function(chokepoint, from, to) {
    x <- filter(choke, portname == chokepoint, date >= as.Date(from), date <= as.Date(to))
    7 * mean(x$n_total)
  }

  suez_2023 <- weekly_avg("Suez Canal", "2023-01-01", "2023-11-30")
  suez_2024 <- weekly_avg("Suez Canal", "2024-01-01", "2024-12-31")
  cape_2023 <- weekly_avg("Cape of Good Hope", "2023-01-01", "2023-11-30")
  cape_2024 <- weekly_avg("Cape of Good Hope", "2024-01-01", "2024-12-31")

  last_day <- max(choke$date)
  last_day_text <- paste(as.integer(format(last_day, "%d")),
                         month.name[as.integer(format(last_day, "%m"))],
                         format(last_day, "%Y"))

  did_coefs  <- coef(did[["Calls"]])
  zaf_coefs  <- coef(het[["Calls"]])
  zafc_coefs <- coef(het[["Container"]])
  nope_coefs <- coef(het[["Excl. Port Elizabeth"]])

  quoted_ports <- c(PortPE = "Port Elizabeth", PortCT = "Cape Town", PortDUR = "Durban",
                    PortNGQ = "Ngqura", PortLP = "Las Palmas (de Gran Canaria)",
                    PortPL = "Port Louis", PortWB = "Walvis Bay", PortDJ = "Djibouti",
                    PortKAP = "King Abdullah Port", PortJED = "Jeddah", PortAQ = "Aqaba",
                    PortPS = "Port Sudan", PortSOK = "El Sokhna",
                    PortHOD = "Hudaydah (Hodeidah)", PortEIL = "Eilat")
  port_lines <- character(0)
  for (macro_name in names(quoted_ports)) {
    value <- changes$dlog_net[changes$portname == quoted_ports[[macro_name]]]
    text <- if (length(value) > 0) fmt_signed(value[1]) else "n/a"
    port_lines <- c(port_lines, latex_macro(macro_name, text))
  }

  zaf_rank <- placebo$rank[placebo$ISO3 == "ZAF"]
  main_groups <- filter(groups, group != "Latin America")

  lines <- c(
    port_lines,
    latex_macro("SuezPre",    fmt_number(suez_2023)),
    latex_macro("SuezPost",   fmt_number(suez_2024)),
    latex_macro("CapePre",    fmt_number(cape_2023)),
    latex_macro("CapePost",   fmt_number(cape_2024)),
    latex_macro("SuezChange", fmt_number(100 * (suez_2024 / suez_2023 - 1))),
    latex_macro("CapeChange", sprintf("%.0f", 100 * (cape_2024 / cape_2023 - 1))),
    latex_macro("SuezLast",   fmt_number(weekly_avg("Suez Canal", last_day - 27, last_day))),
    latex_macro("CapeLast",   fmt_number(weekly_avg("Cape of Good Hope", last_day - 27, last_day))),
    latex_macro("LastDate",   last_day_text),
    latex_macro("EffRed",  fmt_percent(did_coefs[["post_red"]])),
    latex_macro("EffEmed", fmt_percent(did_coefs[["post_emed"]])),
    latex_macro("EffCape", fmt_percent(did_coefs[["post_cape"]])),
    latex_macro("EffCapeHet",  fmt_percent(zaf_coefs[["post_cape"]])),
    latex_macro("EffZafHet",   fmt_percent(zaf_coefs[["post_cape"]] + zaf_coefs[["post_cape_zaf"]])),
    latex_macro("EffCapeCont", fmt_percent(zafc_coefs[["post_cape"]])),
    latex_macro("EffZafCont",  fmt_percent(zafc_coefs[["post_cape"]] + zafc_coefs[["post_cape_zaf"]])),
    latex_macro("EffZafNoPE",  fmt_percent(nope_coefs[["post_cape"]] + nope_coefs[["post_cape_zaf"]])),
    latex_macro("PlaceboRank", zaf_rank),
    latex_macro("PlaceboN",    nrow(placebo)),
    latex_macro("PlaceboP",    fmt_number(zaf_rank / nrow(placebo), 2)),
    latex_macro("NPorts",        sum(main_groups$ports)),
    latex_macro("NCountries",    sum(main_groups$countries)),
    latex_macro("ControlChange", fmt_number(attr(changes, "control_change"), 1)),
    latex_macro("SuezActual",         fmt_number(suez$actual, 2)),
    latex_macro("SuezPredCount",      fmt_number(suez$pred_count, 2)),
    latex_macro("SuezPredCap",        fmt_number(suez$pred_capacity, 2)),
    latex_macro("SuezRevTwentyThree", fmt_number(suez$revenue_2023, 2)),
    latex_macro("SuezTransitChange",  fmt_number(100 * suez$transits_change)),
    latex_macro("SuezCapChange",      fmt_number(100 * suez$capacity_change))
  )

  writeLines(lines, file, useBytes = TRUE)
  file
}


# The extra arguments only tell targets to rebuild when a figure or table changes.
compile_paper <- function(sources, ...) {
  if (Sys.which("pdflatex") == "") {
    stop("pdflatex not found. Install LaTeX with tinytex::install_tinytex().")
  }
  tex_file <- sources[grepl("\\.tex$", sources)][1]

  old_dir <- setwd(dirname(tex_file))
  on.exit(setwd(old_dir))
  tinytex::latexmk(basename(tex_file), engine = "pdflatex", bib_engine = "bibtex")

  sub("\\.tex$", ".pdf", tex_file)
}
