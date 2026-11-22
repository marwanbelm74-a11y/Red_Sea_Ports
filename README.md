# The Red Sea Crisis and Emerging-Market ports

Who won and who lost among emerging-market ports when carriers abandoned the
Red Sea for the Cape of Good Hope (December 2023)? And did port efficiency
determine who captured the diverted traffic?

## Questions

1. Size and duration of the shock: transits through Suez, Bab el-Mandeb and the Cape (2019–2026).
2. Who lost: ports in the Red Sea, the Gulf of Aden and the Eastern Mediterranean.
3. Who gained: Cape-route ports, and the role of efficiency (CPPI 2023).
4. Egypt: Suez Canal revenue reconstructed from transits.

## Reproduce

```r
renv::restore()
targets::tar_make()   # about 1 hour the first time (PortWatch download), then a few seconds
```

The PDF is written to `paper/main.pdf`. Numbers in the text come from
`output/tables/numbers.tex`, regenerated on every run.

## Data

| Source | File | How obtained |
|---|---|---|
| IMF PortWatch, daily transits by chokepoint | `data/raw/portwatch_chokepoints.csv` | ArcGIS API, automatic |
| IMF PortWatch, daily port calls by port, aggregated by week | `data/raw/portwatch_ports_weekly.csv` | ArcGIS API, server-side aggregation |
| IMF PortWatch, port table | `data/raw/portwatch_ports.csv` | ArcGIS API, automatic |
| CPPI 2023 (World Bank / S&P), Table E.1 | `data/raw/cppi2023.pdf` | PDF, automatic extraction |
| Suez Canal revenue 2023–2024 | `data/manual/suez_revenue.csv` | Entered by hand, source in the file |

To update the PortWatch data, delete the files in `data/raw/` (and the
`data/raw/ports_weekly/` folder), then run `tar_make()` again.

## Methodological choices (read before modifying)

- **Groups** (`R/data.R`): defined by port coordinates (Red Sea, Gulf of Aden,
  Eastern Mediterranean) and by country (Cape route). Controls: ports in
  emerging Asia, the Persian Gulf and East Africa. Latin America is excluded
  (Panama Canal drought) except in a robustness check.
- **Offshore terminals** excluded. **Active ports**: at least 2 calls per week in 2019–2022.
- **Main period**: 2019–2024, to avoid the 2025 US tariffs.
- **Specification**: port × week-of-year and week fixed effects; standard errors
  clustered by country. Few countries in the treated groups: interpret p-values
  with caution.
- **CPPI**: name matching in `R/models.R` (`CPPI_MANUAL`). The list of matches
  is exported to `output/tables/cppi_match.csv`: check it.

## Layout

| File | Contents |
|---|---|
| `R/download.R` | PortWatch download (pagination, resume) |
| `R/cppi.R` | CPPI ranking extracted from the PDF |
| `R/data.R` | Groups, port × week panel |
| `R/models.R` | Event study, DiD, heterogeneity, Suez |
| `R/outputs.R` | Figures, LaTeX tables, numbers for the text |
| `paper/main.tex` | The paper |
