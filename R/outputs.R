# Outputs: each function writes a file and returns its path (format = "file").

GROUP_COLORS <- c("Red Sea" = "#C0392B", "Eastern Mediterranean" = "#E67E22",
                  "Cape route" = "#2471A3", "Control" = "#7F8C8D")

theme_paper <- function() {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())
}

ensure_dir <- function(file) dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

# Figure 1: weekly transits through Suez / Bab el-Mandeb / the Cape.
plot_chokepoints <- function(choke_file, file) {
  ensure_dir(file)
  ch <- utils::read.csv(choke_file)
  ch$date <- as.Date(ch$date)
  ch <- ch[ch$portname %in% c("Suez Canal", "Bab el-Mandeb Strait", "Cape of Good Hope"), ]
  ch$week <- ch$date - (as.integer(format(ch$date, "%u")) - 1)
  wk <- stats::aggregate(n_total ~ portname + week, data = ch, FUN = sum)
  n_days <- stats::aggregate(n_total ~ portname + week, data = ch, FUN = length)
  wk <- wk[n_days$n_total == 7, ]  # complete weeks only
  wk$portname <- factor(wk$portname,
                        levels = c("Suez Canal", "Bab el-Mandeb Strait", "Cape of Good Hope"),
                        labels = c("Suez Canal", "Bab el-Mandeb", "Cape of Good Hope"))
  p <- ggplot2::ggplot(wk, ggplot2::aes(week, n_total, colour = portname)) +
    ggplot2::geom_line(linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = SHOCK_DATE, linetype = 2) +
    ggplot2::scale_colour_manual(values = c("#C0392B", "#E67E22", "#2471A3")) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::labs(x = NULL, y = "Transits per week", colour = NULL) +
    theme_paper()
  ggplot2::ggsave(file, p, width = 6.5, height = 3.4)
  file
}

# Figure 2: event study by group.
plot_event_study <- function(es, file) {
  ensure_dir(file)
  ct <- as.data.frame(fixest::coeftable(es))
  ct$term <- rownames(ct)
  ct$rel4 <- as.integer(sub("^rel4::(-?\\d+):.*$", "\\1", ct$term))
  ct$var <- sub("^.*:(\\w+)$", "\\1", ct$term)
  ct <- rbind(ct[, c("rel4", "var", "Estimate", "Std. Error")],
              data.frame(rel4 = -1, var = c("red", "emed", "cape"), Estimate = 0, `Std. Error` = 0,
                         check.names = FALSE))
  ct <- ct[abs(ct$rel4) <= 12, ]  # endpoints -13/+13 bin the distant periods
  ct$group <- factor(ct$var, levels = c("red", "emed", "cape"),
                     labels = c("Red Sea", "Eastern Mediterranean", "Cape route"))
  p <- ggplot2::ggplot(ct, ggplot2::aes(rel4, Estimate, colour = group, fill = group)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
    ggplot2::geom_vline(xintercept = -0.5, linetype = 2) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Estimate - 1.96 * `Std. Error`,
                                      ymax = Estimate + 1.96 * `Std. Error`),
                         alpha = 0.15, colour = NA) +
    ggplot2::geom_line() + ggplot2::geom_point(size = 1) +
    ggplot2::facet_wrap(~group, nrow = 1) +
    ggplot2::scale_colour_manual(values = GROUP_COLORS, guide = "none") +
    ggplot2::scale_fill_manual(values = GROUP_COLORS, guide = "none") +
    ggplot2::labs(x = "4-week periods since 18 December 2023",
                  y = "Effect on log(1 + port calls)") +
    theme_paper()
  ggplot2::ggsave(file, p, width = 6.5, height = 3)
  file
}

KEY_PORTS <- c("Djibouti", "Jeddah", "King Abdullah Port", "Aqaba", "Port Sudan", "El Sokhna",
               "Port Said", "Hudaydah (Hodeidah)", "Aden", "Piraeus", "Mersin", "Damietta",
               "Alexandria", "Haifa", "Durban", "Cape Town", "Ngqura", "Port Elizabeth",
               "Port Louis", "Walvis Bay", "Lome", "Las Palmas (de Gran Canaria)", "Abidjan",
               "Tema", "Dakar", "Luanda", "Maputo", "Porto Da Praia")

# Figure 3: winners and losers (change net of control ports).
plot_winners_losers <- function(changes, file) {
  ensure_dir(file)
  d <- changes[changes$portname %in% KEY_PORTS, ]
  d$label <- sub(" \\(.*\\)", "", d$portname)
  d$label <- ifelse(is.na(d$cppi_rank), d$label, sprintf("%s [%d]", d$label, d$cppi_rank))
  d$label <- ifelse(d$ISO3 == "ZAF", paste0(d$label, " *"), d$label)
  d$label <- factor(d$label, levels = d$label[order(d$dlog_net)])
  p <- ggplot2::ggplot(d, ggplot2::aes(dlog_net, label, fill = group)) +
    ggplot2::geom_col() +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_fill_manual(values = GROUP_COLORS) +
    ggplot2::labs(x = "Change in port calls, net of control ports (log points x 100)",
                  y = NULL, fill = NULL) +
    theme_paper()
  ggplot2::ggsave(file, p, width = 6.5, height = 5.5)
  file
}

DICT <- c(post_red = "Post $\\times$ Red Sea", post_emed = "Post $\\times$ E. Mediterranean",
          post_cape = "Post $\\times$ Cape route", post_cape_zaf = "Post $\\times$ Cape $\\times$ South Africa",
          post_cppi = "Post $\\times$ CPPI rank (std.)",
          post_cape_cppi = "Post $\\times$ Cape $\\times$ CPPI rank (std.)",
          "log1p(calls)" = "log(1 + calls)", "log1p(calls_container)" = "log(1 + container calls)",
          "log1p(volume)" = "log(1 + volumes)", portid = "Port", woy = "Week of year",
          week = "Week", ISO3 = "country")

save_etable <- function(models, file) {
  ensure_dir(file)
  fixest::etable(models, tex = TRUE, file = file, replace = TRUE, dict = DICT,
                 headers = names(models), digits = 3, fitstat = ~n + r2,
                 signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                 style.tex = fixest::style.tex("aer"))
  file
}

save_groups_table <- function(groups, file) {
  ensure_dir(file)
  g <- groups[match(GROUP_LEVELS, groups$group), ]
  rows <- sprintf("%s & %d & %d & %.1f & %.1f & %+.1f\\%% \\\\", g$group, g$ports, g$countries,
                  g$calls_pre, g$calls_post, 100 * (g$calls_post / g$calls_pre - 1))
  writeLines(c("\\begin{tabular}{lrrrrr}", "\\toprule",
               "Group & Ports & Countries & \\multicolumn{2}{c}{Calls per week} & Change \\\\",
               " & & & before & after & \\\\", "\\midrule", rows, "\\bottomrule", "\\end{tabular}"),
             file, useBytes = TRUE)
  file
}

save_suez_table <- function(sz, file) {
  ensure_dir(file)
  writeLines(c("\\begin{tabular}{lr}", "\\toprule",
               "& USD billion \\\\", "\\midrule",
               sprintf("2023 revenue (observed) & %.2f \\\\", sz$revenue_2023),
               sprintf("2024 predicted: number of transits (%+.0f\\%%) & %.2f \\\\", 100 * sz$transits_change, sz$pred_count),
               sprintf("2024 predicted: vessel capacity (%+.0f\\%%) & %.2f \\\\", 100 * sz$capacity_change, sz$pred_capacity),
               sprintf("2024 revenue (observed) & %.2f \\\\", sz$actual),
               "\\bottomrule", "\\end{tabular}"), file, useBytes = TRUE)
  file
}

# Numbers quoted in the text, as LaTeX macros: they update automatically with the data.
save_numbers <- function(choke_file, did, het, changes, sz, groups, placebo, file) {
  ensure_dir(file)
  ch <- utils::read.csv(choke_file)
  ch$date <- as.Date(ch$date)
  avg <- function(name, from, to) {
    x <- ch[ch$portname == name & ch$date >= as.Date(from) & ch$date <= as.Date(to), ]
    7 * mean(x$n_total)
  }
  last <- max(ch$date)
  pct <- function(b) sprintf("$%.0f$", 100 * (exp(b) - 1))
  b <- stats::coef(did[[1]])
  hb <- stats::coef(het[[1]])
  hc <- stats::coef(het[[2]])
  macro <- function(name, value) sprintf("\\newcommand{\\%s}{%s}", name, value)
  num <- function(x, d = 0) paste0("$", formatC(x, format = "f", digits = d, big.mark = "{,}"), "$")
  # Net change (vs. controls) for a few ports quoted in the text.
  key <- c(PortPE = "Port Elizabeth", PortCT = "Cape Town", PortDUR = "Durban",
           PortNGQ = "Ngqura", PortLP = "Las Palmas (de Gran Canaria)", PortPL = "Port Louis",
           PortWB = "Walvis Bay", PortDJ = "Djibouti", PortKAP = "King Abdullah Port",
           PortJED = "Jeddah", PortAQ = "Aqaba", PortPS = "Port Sudan", PortSOK = "El Sokhna",
           PortHOD = "Hudaydah (Hodeidah)", PortEIL = "Eilat")
  port_lines <- vapply(names(key), function(k) {
    v <- changes$dlog_net[changes$portname == key[[k]]]
    macro(k, if (length(v)) sprintf("$%+.0f$", v[1]) else "n/a")
  }, "")
  last_date <- sprintf("%d %s %s", as.integer(format(last, "%d")),
                       month.name[as.integer(format(last, "%m"))], format(last, "%Y"))
  lines <- c(
    port_lines,
    macro("SuezPre", num(s0 <- avg("Suez Canal", "2023-01-01", "2023-11-30"))),
    macro("SuezPost", num(s1 <- avg("Suez Canal", "2024-01-01", "2024-12-31"))),
    macro("CapePre", num(c0 <- avg("Cape of Good Hope", "2023-01-01", "2023-11-30"))),
    macro("CapePost", num(c1 <- avg("Cape of Good Hope", "2024-01-01", "2024-12-31"))),
    macro("SuezChange", num(100 * (s1 / s0 - 1))),
    macro("CapeChange", sprintf("%.0f", 100 * (c1 / c0 - 1))),  # used after "rose by"
    macro("SuezLast", num(avg("Suez Canal", last - 27, last))),
    macro("CapeLast", num(avg("Cape of Good Hope", last - 27, last))),
    macro("LastDate", last_date),
    macro("EffRed", pct(b[["post_red"]])),
    macro("EffEmed", pct(b[["post_emed"]])),
    macro("EffCape", pct(b[["post_cape"]])),
    macro("EffCapeHet", pct(hb[["post_cape"]])),
    macro("EffZafHet", pct(hb[["post_cape"]] + hb[["post_cape_zaf"]])),
    macro("EffCapeCont", pct(hc[["post_cape"]])),
    macro("EffZafCont", pct(hc[["post_cape"]] + hc[["post_cape_zaf"]])),
    macro("EffZafNoPE", pct(stats::coef(het[[4]])[["post_cape"]] + stats::coef(het[[4]])[["post_cape_zaf"]])),
    macro("PlaceboRank", placebo$rank[placebo$ISO3 == "ZAF"]),
    macro("PlaceboN", nrow(placebo)),
    macro("PlaceboP", num(placebo$rank[placebo$ISO3 == "ZAF"] / nrow(placebo), 2)),
    macro("NPorts", sum(groups$ports[groups$group != "Latin America"])),
    macro("NCountries", sum(groups$countries[groups$group != "Latin America"])),
    macro("ControlChange", num(attr(changes, "control_change"), 1)),
    macro("SuezActual", num(sz$actual, 2)),
    macro("SuezPredCount", num(sz$pred_count, 2)),
    macro("SuezPredCap", num(sz$pred_capacity, 2)),
    macro("SuezRevTwentyThree", num(sz$revenue_2023, 2)),
    macro("SuezTransitChange", num(100 * sz$transits_change)),
    macro("SuezCapChange", num(100 * sz$capacity_change))
  )
  writeLines(lines, file, useBytes = TRUE)
  file
}

# Compile paper/main.tex with latexmk (TinyTeX or MiKTeX/TeX Live).
compile_paper <- function(sources, ...) {
  tex <- sources[grepl("\\.tex$", sources)][1]
  if (!nzchar(Sys.which("pdflatex"))) {
    stop("No LaTeX distribution found (pdflatex not on PATH). ",
         "Tables and figures are up to date in output/; to build the paper, ",
         "install LaTeX with tinytex::install_tinytex().", call. = FALSE)
  }
  pdf <- sub("\\.tex$", ".pdf", tex)
  old <- setwd(dirname(tex))
  on.exit(setwd(old), add = TRUE)
  tinytex::latexmk(basename(tex), engine = "pdflatex", bib_engine = "bibtex")
  setwd(old)
  pdf
}
