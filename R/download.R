# Download IMF PortWatch data (public ArcGIS API, no key required).
# Each function writes a CSV to data/raw/ and does not download again if the
# file already exists: delete the file to force an update.

PW_BASE <- "https://services9.arcgis.com/weJ1QsnbMYJlCHdG/arcgis/rest/services"

# ArcGIS query with pagination (resultOffset) and retries.
pw_query <- function(service, params, page_size = 1000) {
  url <- sprintf("%s/%s/FeatureServer/0/query", PW_BASE, service)
  out <- list()
  offset <- 0
  repeat {
    p <- c(params, f = "json", resultOffset = offset, resultRecordCount = page_size)
    qs <- paste(names(p), vapply(p, function(v) utils::URLencode(as.character(v), reserved = TRUE), ""),
                sep = "=", collapse = "&")
    res <- NULL
    for (attempt in 1:5) {
      res <- tryCatch(jsonlite::fromJSON(paste0(url, "?", qs)), error = function(e) NULL)
      if (!is.null(res) && is.null(res$error)) break
      Sys.sleep(2 * attempt)
    }
    if (is.null(res) || !is.null(res$error)) stop("PortWatch query failed: ", service)
    page <- res$features$attributes
    if (is.null(page) || nrow(page) == 0) break
    out[[length(out) + 1]] <- page
    if (!isTRUE(res$exceededTransferLimit)) break
    offset <- offset + nrow(page)
  }
  do.call(rbind, out)
}

# Daily transits by chokepoint (full available period).
download_chokepoints <- function(file = "data/raw/portwatch_chokepoints.csv") {
  if (!file.exists(file)) {
    x <- pw_query("Daily_Chokepoints_Data",
                  c(where = "1=1",
                    outFields = "date,portid,portname,n_total,n_container,n_tanker,n_dry_bulk,capacity,capacity_container",
                    orderByFields = "ObjectId"),
                  page_size = 10000)
    x$date <- as.Date(x$date)
    utils::write.csv(x[order(x$portname, x$date), ], file, row.names = FALSE)
  }
  file
}

# Port table: coordinates, country, continent.
download_ports_meta <- function(file = "data/raw/portwatch_ports.csv") {
  if (!file.exists(file)) {
    x <- pw_query("PortWatch_ports_database",
                  c(where = "1=1",
                    outFields = "portid,portname,country,ISO3,continent,lat,lon,vessel_count_total,LOCODE",
                    orderByFields = "portid"))
    utils::write.csv(x, file, row.names = FALSE)
  }
  file
}

# Weekly port calls by port, aggregated server-side (the daily table has ~6
# million rows). Weeks run Monday to Sunday. One cache file per week in
# data/raw/ports_weekly/, so an interrupted download can resume.
download_ports_weekly <- function(file = "data/raw/portwatch_ports_weekly.csv",
                                  start = as.Date("2019-01-07"),
                                  end = as.Date("2024-12-30")) {
  if (file.exists(file)) return(file)
  cache <- file.path(dirname(file), "ports_weekly")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  vars <- c(calls = "portcalls", calls_container = "portcalls_container",
            calls_tanker = "portcalls_tanker", calls_dry_bulk = "portcalls_dry_bulk",
            import = "import", export = "export")
  stats <- jsonlite::toJSON(c(
    lapply(names(vars), function(k) list(statisticType = "sum",
                                         onStatisticField = vars[[k]],
                                         outStatisticFieldName = k)),
    list(list(statisticType = "count", onStatisticField = "portcalls",
              outStatisticFieldName = "ndays"))
  ), auto_unbox = TRUE)

  weeks <- seq(start, end, by = "week")
  for (i in seq_along(weeks)) {
    w <- weeks[i]
    f <- file.path(cache, paste0(w, ".csv"))
    if (file.exists(f)) next
    x <- pw_query("Daily_Ports_Data",
                  c(where = sprintf("date >= DATE '%s' AND date < DATE '%s'", w, w + 7),
                    groupByFieldsForStatistics = "portid",
                    outStatistics = as.character(stats),
                    orderByFields = "portid"))
    x$week <- w
    utils::write.csv(x, f, row.names = FALSE)
    if (i %% 25 == 0) message("PortWatch: ", i, "/", length(weeks), " weeks")
  }
  all <- do.call(rbind, lapply(file.path(cache, paste0(weeks, ".csv")), utils::read.csv))
  utils::write.csv(all, file, row.names = FALSE)
  file
}
