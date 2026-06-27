#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(readxl)
})

options(encoding = "UTF-8")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
raw_dir <- file.path(root, "atlas", "data", "raw")
out_path <- file.path(root, "atlas", "data", "bcrd-live-data.json")

month_lookup <- c(
  "ene" = 1, "enero" = 1,
  "feb" = 2, "febrero" = 2,
  "mar" = 3, "marzo" = 3,
  "abr" = 4, "abril" = 4,
  "may" = 5, "mayo" = 5,
  "jun" = 6, "junio" = 6,
  "jul" = 7, "julio" = 7,
  "ago" = 8, "agosto" = 8,
  "sep" = 9, "sept" = 9, "septiembre" = 9,
  "oct" = 10, "octubre" = 10,
  "nov" = 11, "noviembre" = 11,
  "dic" = 12, "diciembre" = 12
)

month_labels <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

required_files <- c(
  imae = file.path(raw_dir, "bcrd-sector-real", "imae_2018.xlsx"),
  pib_origin = file.path(raw_dir, "bcrd-sector-real", "pib_origen_2018.xlsx"),
  ipc = file.path(raw_dir, "bcrd-precios", "ipc_base_2019-2020.xls"),
  ipc_grupos = file.path(raw_dir, "bcrd-precios", "ipc_grupos_base_2019-2020.xls"),
  ipc_core = file.path(raw_dir, "bcrd-precios", "ipc_subyacente_base_2019-2020.xlsx"),
  dollar = file.path(raw_dir, "bcrd-mercado-cambiario", "TASA_DOLAR_REFERENCIA_MC.xlsx"),
  tpm = file.path(raw_dir, "bcrd-monetario", "Serie_TPM.xlsx"),
  exports = file.path(raw_dir, "bcrd-sector-externo", "Exportaciones_Trimestrales_6.xls"),
  imports = file.path(raw_dir, "bcrd-sector-externo", "Importaciones_Trimestrales_6.xls"),
  labor_indicators = file.path(raw_dir, "bcrd-encft", "00_Indicadores.xlsx"),
  labor_sectors = file.path(raw_dir, "bcrd-encft", "1_1_Ocupados_Rama.xlsx")
)

missing <- required_files[!file.exists(required_files)]
if (length(missing)) {
  stop("Faltan fuentes BCRD en atlas/data/raw: ", paste(names(missing), collapse = ", "), call. = FALSE)
}

optional_files <- c(
  dga_exports = file.path(raw_dir, "dga-exportaciones-capitulo", "exportaciones_por_capitulos_2017_2026.csv"),
  dga_imports = file.path(raw_dir, "dga-importaciones-capitulo", "importaciones_por_capitulos_2017_2026.csv"),
  tss_jobs = file.path(raw_dir, "tss-empleos-cotizantes", "empleos-cotizantes-2003-2026.csv"),
  tss_employers = file.path(raw_dir, "tss-empleadores-cotizantes", "empleadores-cotizantes-2003-2026.csv"),
  services = file.path(raw_dir, "bcrd-sector-externo", "Balanza-de-Servicios-anual.xlsx")
)

to_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
}

fill_down_year <- function(values) {
  out <- rep(NA_integer_, length(values))
  current <- NA_integer_
  for (i in seq_along(values)) {
    candidate <- suppressWarnings(as.integer(as.character(values[[i]])))
    if (!is.na(candidate) && candidate >= 1900 && candidate <= 2100) current <- candidate
    out[[i]] <- current
  }
  out
}

month_number <- function(value) {
  key <- tolower(trimws(as.character(value)))
  key <- sub("[.].*$", "", key)
  key <- iconv(key, to = "ASCII//TRANSLIT")
  if (!key %in% names(month_lookup)) return(NA_integer_)
  as.integer(unname(month_lookup[[key]]))
}

normalize_text <- function(value) {
  text <- iconv(as.character(value), to = "ASCII//TRANSLIT")
  text <- tolower(trimws(text))
  gsub("[^a-z0-9]+", " ", text)
}

norm01 <- function(value) {
  value <- as.numeric(value)
  if (!length(value) || all(is.na(value))) return(value)
  max_value <- max(value, na.rm = TRUE)
  if (!is.finite(max_value) || max_value == 0) return(rep(0, length(value)))
  round(value / max_value * 100, 1)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

read_monthly_series <- function(path, sheet, year_col, month_col, value_col, scale = 1, start_row = 1) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  if (nrow(raw) < start_row) stop("Archivo sin filas suficientes: ", path, call. = FALSE)
  rows <- raw[start_row:nrow(raw), , drop = FALSE]
  years <- fill_down_year(rows[[year_col]])
  months <- vapply(rows[[month_col]], month_number, integer(1))
  values <- to_num(rows[[value_col]]) * scale
  data.frame(
    year = years,
    month = months,
    value = values,
    stringsAsFactors = FALSE
  ) |>
    subset(!is.na(year) & !is.na(month) & !is.na(value))
}

latest_monthly <- function(df, value_name) {
  df <- df[order(df$year, df$month), ]
  names(df)[names(df) == "value"] <- value_name
  df
}

imae <- latest_monthly(read_monthly_series(required_files[["imae"]], "IMAE", 1, 2, 4, start_row = 9), "imae")
inflation <- latest_monthly(read_monthly_series(required_files[["ipc"]], "IPC base 2019-2020", 1, 2, 6, start_row = 7), "inflacion")
core <- latest_monthly(read_monthly_series(required_files[["ipc_core"]], "ipc subyacente base 2019-2020", 1, 2, 6, start_row = 5), "core")
dollar <- latest_monthly(read_monthly_series(required_files[["dollar"]], "PromMensual", 1, 2, 4, start_row = 4), "dolar")
tpm <- latest_monthly(read_monthly_series(required_files[["tpm"]], "Tasas", 1, 2, 3, scale = 100, start_row = 7), "tpm")

macro <- Reduce(function(left, right) merge(left, right, by = c("year", "month"), all = FALSE), list(dollar, inflation, imae, tpm))
macro <- macro[order(macro$year, macro$month), ]
if (nrow(macro) < 6) stop("La serie macro BCRD tiene menos de seis observaciones completas.", call. = FALSE)
macro <- tail(macro, 10)
macro$period <- paste(month_labels[macro$month], macro$year)
macro <- macro[, c("period", "year", "month", "dolar", "inflacion", "imae", "tpm")]

price_timeline <- merge(inflation, core, by = c("year", "month"), all = FALSE)
price_timeline <- price_timeline[order(price_timeline$year, price_timeline$month), ]
price_timeline <- tail(price_timeline, 10)
price_timeline$period <- paste(month_labels[price_timeline$month], price_timeline$year)
price_timeline <- data.frame(
  period = price_timeline$period,
  year = price_timeline$year,
  month = price_timeline$month,
  headline = price_timeline$inflacion,
  core = price_timeline$core,
  stringsAsFactors = FALSE
)

price_components <- function(path) {
  raw <- read_excel(path, sheet = " Grupos Base 2019-2020", col_names = FALSE, .name_repair = "minimal")
  years <- fill_down_year(raw[[1]])
  months <- vapply(raw[[1]], month_number, integer(1))
  rows <- which(!is.na(years) & !is.na(months))
  latest_row <- tail(rows, 1)
  labels <- as.character(unlist(raw[3, ], use.names = FALSE))
  values <- to_num(unlist(raw[latest_row, ], use.names = FALSE))
  var_cols <- seq(3, ncol(raw), by = 2)
  out <- data.frame(
    component = labels[var_cols - 1],
    contribution = values[var_cols],
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$contribution) & nzchar(out$component), ]
  out$pressure <- abs(out$contribution)
  out <- out[order(-out$pressure), ]
  out <- head(out, 7)
  out$pressure <- round(out$pressure / max(out$pressure, na.rm = TRUE) * 100, 1)
  out$contribution <- round(out$contribution, 2)
  out
}

quarterly_totals <- function(path, total_row, year_row, quarter_row) {
  raw <- read_excel(path, sheet = excel_sheets(path)[1], col_names = FALSE, .name_repair = "minimal")
  years <- to_num(unlist(raw[year_row, ], use.names = FALSE))
  values <- to_num(unlist(raw[total_row, ], use.names = FALSE))
  quarters <- as.character(unlist(raw[quarter_row, ], use.names = FALSE))
  keep <- !is.na(years) & !is.na(values) & nzchar(trimws(quarters))
  data.frame(year = as.integer(years[keep]), value = values[keep], stringsAsFactors = FALSE)
}

exports_q <- quarterly_totals(required_files[["exports"]], total_row = 70, year_row = 6, quarter_row = 7)
imports_q <- quarterly_totals(required_files[["imports"]], total_row = 63, year_row = 8, quarter_row = 9)
annual <- merge(
  aggregate(value ~ year, exports_q, sum, na.rm = TRUE),
  aggregate(value ~ year, imports_q, sum, na.rm = TRUE),
  by = "year",
  suffixes = c("_exports", "_imports")
)
annual <- tail(annual[order(annual$year), ], 5)
trade_flows <- data.frame(
  period = as.character(annual$year),
  exports = round(annual$value_exports, 1),
  imports = round(annual$value_imports, 1),
  stringsAsFactors = FALSE
)

export_categories <- function(path) {
  raw <- read_excel(path, sheet = excel_sheets(path)[1], col_names = FALSE, .name_repair = "minimal")
  years <- to_num(unlist(raw[6, ], use.names = FALSE))
  value_cols <- which(!is.na(years))
  latest_year <- max(years[value_cols], na.rm = TRUE)
  latest_cols <- value_cols[years[value_cols] == latest_year]
  labels <- as.character(raw[[2]])
  candidate_rows <- grep("^\\s*[0-9]+[.]\\s", labels)
  out <- lapply(candidate_rows, function(row) {
    value <- sum(to_num(unlist(raw[row, latest_cols], use.names = FALSE)), na.rm = TRUE)
    data.frame(
      name = sub("^\\s*[0-9]+[.]\\s*", "", labels[[row]]),
      share = value,
      complexity = NA_real_,
      signal = paste("Exportaciones", latest_year),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  out <- out[is.finite(out$share) & out$share > 0, ]
  out <- out[order(-out$share), ]
  total <- sum(out$share, na.rm = TRUE)
  out <- head(out, 8)
  out$share <- round(out$share / total * 100, 1)
  out$complexity <- round(out$share, 1)
  out
}

dga_export_categories <- function(exports_path, imports_path) {
  if (!file.exists(exports_path) || !file.exists(imports_path)) return(NULL)
  read_dga <- function(path, value_name) {
    raw <- read.csv(path, sep = ";", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE)
    if (ncol(raw) < 6) return(NULL)
    out <- data.frame(
      chapter = trimws(as.character(raw[[2]])),
      value = to_num(raw[[3]]),
      year = suppressWarnings(as.integer(raw[[6]])),
      stringsAsFactors = FALSE
    )
    out <- out[!is.na(out$year) & is.finite(out$value) & nzchar(out$chapter), ]
    names(out)[names(out) == "value"] <- value_name
    out
  }
  exports <- read_dga(exports_path, "exports")
  imports <- read_dga(imports_path, "imports")
  if (is.null(exports) || is.null(imports) || !nrow(exports)) return(NULL)
  latest_year <- max(exports$year, na.rm = TRUE)
  exports <- aggregate(exports ~ chapter, exports[exports$year == latest_year, ], sum, na.rm = TRUE)
  imports <- aggregate(imports ~ chapter, imports[imports$year == latest_year, ], sum, na.rm = TRUE)
  out <- merge(exports, imports, by = "chapter", all.x = TRUE)
  out$imports[is.na(out$imports)] <- 0
  out <- out[order(-out$exports), ]
  out <- head(out, 8)
  total <- sum(exports$exports, na.rm = TRUE)
  out$share <- round(out$exports / total * 100, 1)
  out$coverage <- round(ifelse(out$imports > 0, out$exports / out$imports * 100, 100), 1)
  data.frame(
    name = sub("[.]$", "", out$chapter),
    share = out$share,
    complexity = pmin(out$coverage, 999),
    signal = paste("DGA", latest_year),
    stringsAsFactors = FALSE
  )
}

labor_indicators <- function(path) {
  raw <- read_excel(path, sheet = "Indicadores", col_names = FALSE, .name_repair = "minimal")
  years <- fill_down_year(unlist(raw[31, ], use.names = FALSE))
  quarters <- as.character(unlist(raw[32, ], use.names = FALSE))
  cols <- which(!is.na(years) & nzchar(trimws(quarters)))
  latest_col <- tail(cols, 1)
  rows <- c(33, 34, 39, 40, 46)
  data.frame(
    group = as.character(raw[[1]][rows]),
    value = round(to_num(unlist(raw[rows, latest_col], use.names = FALSE)), 1),
    stringsAsFactors = FALSE
  )
}

labor_sectors <- function(path) {
  raw <- read_excel(path, sheet = "Total", col_names = FALSE, .name_repair = "minimal")
  latest_row <- max(which(!is.na(to_num(raw[[3]]))))
  labels <- as.character(unlist(raw[9, ], use.names = FALSE))
  values <- to_num(unlist(raw[latest_row, ], use.names = FALSE))
  out <- data.frame(name = labels[4:ncol(raw)], jobs = values[4:ncol(raw)], stringsAsFactors = FALSE)
  out <- out[!is.na(out$jobs) & nzchar(out$name), ]
  total <- sum(out$jobs, na.rm = TRUE)
  out$jobs <- round(out$jobs / total * 100, 1)
  out <- out[order(-out$jobs), ]
  head(out, 7)
}

tss_labor_trend <- function(jobs_path, employers_path) {
  if (!file.exists(jobs_path) || !file.exists(employers_path)) return(NULL)
  jobs <- read.csv(jobs_path, fileEncoding = "latin1", stringsAsFactors = FALSE, check.names = FALSE)
  employers <- read.csv(employers_path, fileEncoding = "latin1", stringsAsFactors = FALSE, check.names = FALSE)
  pick_col <- function(data, pattern) {
    normalized <- normalize_text(names(data))
    names(data)[grepl(pattern, normalized)][1]
  }
  jobs_year_col <- pick_col(jobs, "^ano$")
  jobs_total_col <- pick_col(jobs, "^total$")
  employers_year_col <- pick_col(employers, "^ano$")
  employer_col <- pick_col(employers, "empleadores")
  needed <- c(jobs_year_col, jobs_total_col, employers_year_col, employer_col)
  if (any(is.na(needed))) return(NULL)
  jobs_year <- aggregate(jobs[[jobs_total_col]] ~ jobs[[jobs_year_col]], jobs, mean, na.rm = TRUE)
  names(jobs_year) <- c("year", "jobs")
  emp_year <- aggregate(employers[[employer_col]] ~ employers[[employers_year_col]], employers, mean, na.rm = TRUE)
  names(emp_year) <- c("year", "employers")
  out <- merge(jobs_year, emp_year, by = "year", all = FALSE)
  out <- tail(out[order(out$year), ], 7)
  data.frame(
    period = as.character(out$year),
    employment = round(out$jobs / 1000000, 2),
    employers = round(out$employers / 1000, 1),
    stringsAsFactors = FALSE
  )
}

external_services <- function(path) {
  if (!file.exists(path)) return(NULL)
  raw <- read_excel(path, sheet = "Anual", col_names = FALSE, .name_repair = "minimal")
  years <- suppressWarnings(as.integer(unlist(raw[6, ], use.names = FALSE)))
  year_cols <- which(!is.na(years))
  if (!length(year_cols)) return(NULL)
  latest_cols <- tail(year_cols, 6)
  credit <- to_num(unlist(raw[9, latest_cols], use.names = FALSE))
  timeline <- data.frame(
    period = as.character(years[latest_cols]),
    pressure = norm01(credit),
    stringsAsFactors = FALSE
  )
  labels <- normalize_text(raw[[1]])
  latest_col <- tail(year_cols, 1)
  driver_patterns <- c(
    "Viajes" = "^c viajes$",
    "Transporte" = "^b transporte$",
    "Otros servicios" = "otros servicios empresariales",
    "Financieros" = "servicios financieros"
  )
  drivers <- lapply(names(driver_patterns), function(name) {
    row <- grep(driver_patterns[[name]], labels)[1]
    value <- if (is.na(row)) NA_real_ else to_num(raw[[latest_col]][row])
    data.frame(driver = name, raw = value, stringsAsFactors = FALSE)
  })
  drivers <- do.call(rbind, drivers)
  drivers <- drivers[is.finite(drivers$raw), ]
  drivers$value <- norm01(drivers$raw)
  list(timeline = timeline, drivers = drivers[, c("driver", "value")])
}

sector_model <- function(path) {
  raw <- read_excel(path, sheet = "PIB$_Trim", col_names = FALSE, .name_repair = "minimal")
  years <- suppressWarnings(as.integer(unlist(raw[7, ], use.names = FALSE)))
  latest_year <- max(years, na.rm = TRUE)
  latest_cols <- which(years == latest_year)
  labels <- normalize_text(raw[[1]])
  sector_specs <- list(
    list(sector = "Turismo", pattern = "hoteles.*restaurantes", driver = "Viajes", direction = "Servicios"),
    list(sector = "Industria", pattern = "^industrias$", driver = "Produccion", direction = "PIB"),
    list(sector = "Servicios financieros", pattern = "intermediacion financiera", driver = "Financiamiento", direction = "Credito"),
    list(sector = "Transporte", pattern = "transporte.*almacenamiento", driver = "Servicios", direction = "PIB"),
    list(sector = "Construccion", pattern = "construccion", driver = "Financiamiento", direction = "Credito"),
    list(sector = "Agropecuaria", pattern = "agropecuario", driver = "Produccion", direction = "PIB"),
    list(sector = "Comercio", pattern = "^comercio$", driver = "Demanda interna", direction = "PIB")
  )
  rows <- lapply(sector_specs, function(spec) {
    row <- grep(spec$pattern, labels)[1]
    value <- if (is.na(row)) NA_real_ else sum(to_num(unlist(raw[row, latest_cols], use.names = FALSE)), na.rm = TRUE)
    data.frame(sector = spec$sector, raw = value, driver = spec$driver, direction = spec$direction, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out <- out[is.finite(out$raw) & out$raw > 0, ]
  out$pressure <- norm01(out$raw)
  out <- out[order(-out$pressure), ]
  out[, c("sector", "pressure", "driver", "direction")]
}

latest <- tail(macro, 1)
previous <- tail(macro, 2)[1, ]
inflation_latest <- tail(price_timeline, 1)
price_components_data <- price_components(required_files[["ipc_grupos"]])
trade_products <- export_categories(required_files[["exports"]])
dga_products <- dga_export_categories(optional_files[["dga_exports"]], optional_files[["dga_imports"]])
if (!is.null(dga_products) && nrow(dga_products)) trade_products <- dga_products
labor_indicators_data <- labor_indicators(required_files[["labor_indicators"]])
labor_sectors_data <- labor_sectors(required_files[["labor_sectors"]])
tss_trend_data <- tss_labor_trend(optional_files[["tss_jobs"]], optional_files[["tss_employers"]])
external_data <- external_services(optional_files[["services"]])
sector_data <- sector_model(required_files[["pib_origin"]])
latest_trade <- tail(trade_flows, 1)
latest_informal <- labor_indicators_data[grepl("Ocupacion Informal|Ocupación Informal", labor_indicators_data$group), ][1, ]

payload <- list(
  generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  dataCutoff = list(
    macro = paste(latest$period),
    prices = paste(inflation_latest$period),
    trade = max(as.integer(trade_flows$period))
  ),
  series = list(
    macro = macro,
    external = if (!is.null(external_data)) external_data$timeline else NULL,
    drivers = if (!is.null(external_data)) external_data$drivers else NULL,
    sectors = sector_data,
    prices = list(timeline = price_timeline, components = price_components_data),
    trade = list(flows = trade_flows, products = trade_products),
    labor = list(
      indicators = labor_indicators_data,
      sectors = labor_sectors_data,
      trend = tss_trend_data
    )
  ),
  metrics = list(
    usd = list(
      value = sprintf("RD$ %.2f", latest$dolar),
      delta = sprintf("%+.2f vs. mes anterior", latest$dolar - previous$dolar),
      meta = paste("Venta promedio mensual BCRD,", latest$period),
      tone = if ((latest$dolar - previous$dolar) <= 0) "good" else "warn"
    ),
    inflation = list(
      value = sprintf("%.2f%%", latest$inflacion),
      delta = if (latest$inflacion >= 5) "Fuera de meta" else "Dentro del rango meta",
      meta = paste("IPC interanual BCRD,", latest$period),
      tone = if (latest$inflacion >= 5) "warn" else "good"
    ),
    trade = list(
      label = "Exportaciones totales",
      value = sprintf("US$ %.1f MM", latest_trade$exports),
      delta = paste("Exportaciones", latest_trade$period),
      meta = "BCRD sector externo",
      tone = "neutral"
    ),
    labor = list(
      label = "Ocupación informal",
      value = sprintf("%.1f%%", latest_informal$value),
      delta = "Ocupación informal",
      meta = "ENCFT, ultimo trimestre disponible",
      tone = "warn"
    )
  )
)

write_json(payload, out_path, pretty = TRUE, auto_unbox = TRUE, na = "null")

message("BCRD live data built: ", out_path)
message("Macro cutoff: ", latest$period, " | IPC: ", round(latest$inflacion, 2), "% | USD: ", round(latest$dolar, 2))
