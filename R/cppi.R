# CPPI 2023 ranking (World Bank), Table E.1 of the report

cppi_url <- "https://documents1.worldbank.org/curated/en/099060324114539683/pdf/P17583313892300871be641a5ea7b90e0e6.pdf"

download_cppi <- function(file = "data/raw/cppi2023.pdf") {
  if (!file.exists(file)) {
    download.file(cppi_url, file, mode = "wb", quiet = TRUE)
  }
  file
}


read_cppi <- function(pdf_file) {

  pages <- pdf_text(pdf_file)

  # Table E.1 pages come before chapter 4
  chapter4_page <- max(grep("TABLE 4.1", pages, fixed = TRUE))
  table_pages <- grep("Port Name\\s+Overall Ranking", pages)
  table_pages <- table_pages[table_pages < chapter4_page]

  lines <- unlist(strsplit(pages[table_pages], "\n"))

  # lines look like "YANGSHAN      1      GEMLIK      33"
  pattern <- "([A-Z][A-Z0-9 ().,'&/-]*?[A-Z)])\\s{2,}(\\d{1,3})(?=\\s|$)"
  pairs <- unlist(regmatches(lines, gregexpr(pattern, lines, perl = TRUE)))

  cppi <- data.frame(
    cppi_name = trimws(sub(paste0("^", pattern, "$"), "\\1", pairs, perl = TRUE)),
    cppi_rank = as.integer(sub(paste0("^", pattern, "$"), "\\2", pairs, perl = TRUE))
  )

  cppi <- cppi[!duplicated(cppi$cppi_rank) & cppi$cppi_rank >= 1, ]
  cppi[order(cppi$cppi_rank), ]
}
