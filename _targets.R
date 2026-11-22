library(targets)

tar_option_set(packages = c("dplyr", "tidyr", "ggplot2", "fixest", "jsonlite", "pdftools"))
tar_source("R")

list(
  # data
  tar_target(choke_file,   download_chokepoints(),  format = "file"),
  tar_target(ports_file,   download_ports(),        format = "file"),
  tar_target(weekly_file,  download_ports_weekly(), format = "file"),
  tar_target(cppi_pdf,     download_cppi(),         format = "file"),
  tar_target(revenue_file, "data/manual/suez_revenue.csv", format = "file"),

  tar_target(panel,      build_panel(weekly_file, ports_file)),
  tar_target(groups,     summarise_groups(panel)),
  tar_target(cppi,       read_cppi(cppi_pdf)),
  tar_target(cppi_match, match_cppi(panel, cppi)),

  # models
  tar_target(event_study,   estimate_event_study(panel)),
  tar_target(did,           estimate_did(panel)),
  tar_target(heterogeneity, estimate_heterogeneity(panel, cppi_match)),
  tar_target(placebo,       placebo_countries(panel)),
  tar_target(changes,       port_changes(panel, cppi_match)),
  tar_target(suez,          suez_revenue(choke_file, revenue_file)),

  # figures and tables
  tar_target(fig_chokepoints,
             plot_chokepoints(choke_file, "output/figures/fig1_chokepoints.pdf"),
             format = "file"),
  tar_target(fig_event,
             plot_event_study(event_study, "output/figures/fig2_event_study.pdf"),
             format = "file"),
  tar_target(fig_ports,
             plot_winners_losers(changes, "output/figures/fig3_winners_losers.pdf"),
             format = "file"),
  tar_target(tab_groups,
             save_groups_table(groups, "output/tables/tab_groups.tex"),
             format = "file"),
  tar_target(tab_did,
             save_regression_table(did, "output/tables/tab_did.tex"),
             format = "file"),
  tar_target(tab_het,
             save_regression_table(heterogeneity, "output/tables/tab_het.tex"),
             format = "file"),
  tar_target(tab_suez,
             save_suez_table(suez, "output/tables/tab_suez.tex"),
             format = "file"),
  tar_target(numbers,
             save_numbers(choke_file, did, heterogeneity, changes, suez, groups, placebo,
                          "output/tables/numbers.tex"),
             format = "file"),
  tar_target(cppi_match_csv, {
    out <- "output/tables/cppi_match.csv"
    write.csv(arrange(cppi_match, group, portname), out, row.names = FALSE)
    out
  }, format = "file"),

  # paper (only if paper/ exists)
  if (file.exists("paper/main.tex")) list(
    tar_target(paper_sources, c("paper/main.tex", "paper/references.bib"), format = "file"),
    tar_target(paper,
               compile_paper(paper_sources, fig_chokepoints, fig_event, fig_ports,
                             tab_groups, tab_did, tab_het, tab_suez, numbers),
               format = "file")
  )
)
