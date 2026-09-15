# Container Port Performance Index 2023 (World Bank / S&P Global, 2024).
# Global ranking of 405 ports (Table E.1, executive summary) extracted from the PDF.
# Rank 1 = most efficient. NB: the report is not fully consistent across its
# tables (e.g. Durban is 398 in E.1 but 399 in 4.7); we use Table E.1.
# Measured in the PRE-shock period (calendar year 2023): a heterogeneity variable
# that is exogenous to the 2024 diversion.

CPPI_URL <- "https://documents1.worldbank.org/curated/en/099060324114539683/pdf/P17583313892300871be641a5ea7b90e0e6.pdf"

download_cppi <- function(file = "data/raw/cppi2023.pdf") {
  if (!file.exists(file)) utils::download.file(CPPI_URL, file, mode = "wb", quiet = TRUE)
  file
}

parse_cppi <- function(pdf) {
  tx <- pdftools::pdf_text(pdf)
  # Pages of Table E.1: two-column "Port Name ... Overall Ranking" header, located
  # before chapter 4 (whose tables use other layouts).
  ch4 <- grep("TABLE 4.1", tx, fixed = TRUE)
  ch4 <- ch4[length(ch4)]
  pages <- grep("Port Name\\s+Overall Ranking", tx)
  pages <- pages[pages < ch4]
  lines <- unlist(strsplit(tx[pages], "\n"))
  pat <- "([A-Z][A-Z0-9 ().,'&/-]*?[A-Z)])\\s{2,}(\\d{1,3})(?=\\s|$)"
  m <- regmatches(lines, gregexpr(pat, lines, perl = TRUE))
  pairs <- unlist(m)
  out <- data.frame(
    cppi_name = trimws(sub(paste0("^", pat, "$"), "\\1", pairs, perl = TRUE)),
    cppi_rank = as.integer(sub(paste0("^", pat, "$"), "\\2", pairs, perl = TRUE))
  )
  out <- out[!duplicated(out$cppi_rank) & out$cppi_rank >= 1, ]
  out[order(out$cppi_rank), ]
}
