# IMF PortWatch downloads (public ArcGIS API)

portwatch_url <- "https://services9.arcgis.com/weJ1QsnbMYJlCHdG/arcgis/rest/services"


query_portwatch <- function(dataset, params, page_size = 1000) {

  url <- paste0(portwatch_url, "/", dataset, "/FeatureServer/0/query")
  all_pages <- list()
  offset <- 0

  repeat {
    params_page <- c(params, f = "json",
                     resultOffset = offset, resultRecordCount = page_size)
    values <- sapply(params_page, function(v) URLencode(as.character(v), reserved = TRUE))
    query_string <- paste(names(params_page), values, sep = "=", collapse = "&")

    answer <- NULL
    for (attempt in 1:5) {
      answer <- tryCatch(fromJSON(paste0(url, "?", query_string)),
                         error = function(e) NULL)
      if (!is.null(answer) && is.null(answer$error)) break
      Sys.sleep(2 * attempt)
    }
    if (is.null(answer) || !is.null(answer$error)) {
      stop("PortWatch query failed: ", dataset)
    }

    rows <- answer$features$attributes
    if (is.null(rows) || nrow(rows) == 0) break

    all_pages[[length(all_pages) + 1]] <- rows

    if (!isTRUE(answer$exceededTransferLimit)) break
    offset <- offset + nrow(rows)
  }

  do.call(rbind, all_pages)
}


download_chokepoints <- function(file = "data/raw/portwatch_chokepoints.csv") {
  if (file.exists(file)) return(file)

  choke <- query_portwatch(
    "Daily_Chokepoints_Data",
    c(where = "1=1",
      outFields = "date,portid,portname,n_total,n_container,n_tanker,n_dry_bulk,capacity,capacity_container",
      orderByFields = "ObjectId"),
    page_size = 10000
  )
  choke$date <- as.Date(choke$date)
  choke <- choke[order(choke$portname, choke$date), ]
  write.csv(choke, file, row.names = FALSE)
  file
}


download_ports <- function(file = "data/raw/portwatch_ports.csv") {
  if (file.exists(file)) return(file)

  ports <- query_portwatch(
    "PortWatch_ports_database",
    c(where = "1=1",
      outFields = "portid,portname,country,ISO3,continent,lat,lon,vessel_count_total,LOCODE",
      orderByFields = "portid")
  )
  write.csv(ports, file, row.names = FALSE)
  file
}


# The daily table is ~6 million rows, so the server sums each week for us.
# One file per week in data/raw/ports_weekly/ so the download can resume.
download_ports_weekly <- function(file = "data/raw/portwatch_ports_weekly.csv",
                                  first_week = as.Date("2019-01-07"),
                                  last_week = as.Date("2024-12-30")) {
  if (file.exists(file)) return(file)

  week_folder <- file.path(dirname(file), "ports_weekly")
  dir.create(week_folder, recursive = TRUE, showWarnings = FALSE)

  sums <- list(
    list(statisticType = "sum", onStatisticField = "portcalls",           outStatisticFieldName = "calls"),
    list(statisticType = "sum", onStatisticField = "portcalls_container", outStatisticFieldName = "calls_container"),
    list(statisticType = "sum", onStatisticField = "portcalls_tanker",    outStatisticFieldName = "calls_tanker"),
    list(statisticType = "sum", onStatisticField = "portcalls_dry_bulk",  outStatisticFieldName = "calls_dry_bulk"),
    list(statisticType = "sum", onStatisticField = "import",              outStatisticFieldName = "import"),
    list(statisticType = "sum", onStatisticField = "export",              outStatisticFieldName = "export"),
    list(statisticType = "count", onStatisticField = "portcalls",         outStatisticFieldName = "ndays")
  )
  sums_json <- as.character(toJSON(sums, auto_unbox = TRUE))

  weeks <- seq(first_week, last_week, by = "week")

  for (i in seq_along(weeks)) {
    monday <- weeks[i]
    week_file <- file.path(week_folder, paste0(monday, ".csv"))
    if (file.exists(week_file)) next

    one_week <- query_portwatch(
      "Daily_Ports_Data",
      c(where = paste0("date >= DATE '", monday, "' AND date < DATE '", monday + 7, "'"),
        groupByFieldsForStatistics = "portid",
        outStatistics = sums_json,
        orderByFields = "portid")
    )
    one_week$week <- monday
    write.csv(one_week, week_file, row.names = FALSE)

    if (i %% 25 == 0) message("PortWatch: ", i, "/", length(weeks), " weeks")
  }

  week_files <- file.path(week_folder, paste0(weeks, ".csv"))
  all_weeks <- do.call(rbind, lapply(week_files, read.csv))
  write.csv(all_weeks, file, row.names = FALSE)
  file
}
