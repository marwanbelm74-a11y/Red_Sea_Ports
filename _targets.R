# Pipeline: the Red Sea crisis and emerging-market ports
# Run: targets::tar_make()   Visualise: targets::tar_visnetwork()

library(targets)

tar_option_set(packages = c("dplyr", "ggplot2", "fixest"))
tar_source("R")

list(
  # 1. Raw data (downloaded once, then read from data/raw/)
  tar_target(choke_file, download_chokepoints(), format = "file"),
  tar_target(meta_file, download_ports_meta(), format = "file"),
  tar_target(weekly_file, download_ports_weekly(), format = "file"),
  tar_target(cppi_pdf, download_cppi(), format = "file"),
  tar_target(revenue_file, "data/manual/suez_revenue.csv", format = "file"),

  # 2. Preparation
  tar_target(panel, build_panel(weekly_file, meta_file)),
  tar_target(groups, summarise_groups(panel)),
  tar_target(cppi, parse_cppi(cppi_pdf)),
  tar_target(cppi_match, match_cppi(panel, cppi)),

  # 3. Estimation
  tar_target(event_study, estimate_event_study(panel)),
  tar_target(did, estimate_did(panel)),
  tar_target(heterogeneity, estimate_heterogeneity(panel, cppi_match)),
  tar_target(placebo, placebo_countries(panel)),
  tar_target(changes, port_changes(panel, cppi_match)),
  tar_target(suez, suez_revenue(choke_file, revenue_file)),

  # 4. Paper outputs
  tar_target(fig_chokepoints, plot_chokepoints(choke_file, "output/figures/fig1_chokepoints.pdf"), format = "file"),
  tar_target(fig_event, plot_event_study(event_study, "output/figures/fig2_event_study.pdf"), format = "file"),
  tar_target(fig_ports, plot_winners_losers(changes, "output/figures/fig3_winners_losers.pdf"), format = "file"),
  tar_target(tab_groups, save_groups_table(groups, "output/tables/tab_groups.tex"), format = "file"),
  tar_target(tab_did, save_etable(did, "output/tables/tab_did.tex"), format = "file"),
  tar_target(tab_het, save_etable(heterogeneity, "output/tables/tab_het.tex"), format = "file"),
  tar_target(tab_suez, save_suez_table(suez, "output/tables/tab_suez.tex"), format = "file"),
  tar_target(numbers, save_numbers(choke_file, did, heterogeneity, changes, suez, groups,
                                   placebo, "output/tables/numbers.tex"), format = "file"),
  tar_target(cppi_match_csv, {
    f <- "output/tables/cppi_match.csv"
    utils::write.csv(cppi_match[order(cppi_match$group, cppi_match$portname), ], f, row.names = FALSE)
    f
  }, format = "file"),

  # 5. Paper
  tar_target(paper_sources, c("paper/main.tex", "paper/references.bib"), format = "file"),
  tar_target(paper, compile_paper(paper_sources, fig_chokepoints, fig_event, fig_ports,
                                  tab_groups, tab_did, tab_het, tab_suez, numbers),
             format = "file")
)
