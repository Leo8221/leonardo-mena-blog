#!/usr/bin/env Rscript

options(encoding = "UTF-8", scipen = 999)
.libPaths(unique(c(Sys.getenv("R_LIBS_USER"), .libPaths())))
if (!isTRUE(l10n_info()[["UTF-8"]])) stop("R no está leyendo UTF-8.", call. = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(readxl)
  library(scales)
  library(stringr)
  library(tidyr)
  library(svglite)
  library(pdftools)
  library(sf)
  library(patchwork)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]
script_path <- sub("^--file=", "", script_arg)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/")
setwd(repo_root)
sf::sf_use_s2(FALSE)

pal <- list(
  crema = "#F7F1E7", tinta = "#252525", terracota = "#B65C43",
  azul = "#4F789F", oliva = "#76825A", ocre = "#C79B53",
  gris = "#9B9B94", gris_claro = "#DED9D0", blanco = "#FFFDFC"
)

theme_editorial <- function(grid = "y") {
  theme_minimal(base_size = 12, base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = pal$crema, colour = NA),
      panel.background = element_rect(fill = pal$crema, colour = NA),
      panel.grid.major = if (grid == "y") element_line(colour = pal$gris_claro, linewidth = 0.35) else element_blank(),
      panel.grid.minor = element_blank(), axis.title = element_text(colour = pal$tinta),
      axis.text = element_text(colour = pal$tinta), plot.title = element_text(face = "bold", size = 17, colour = pal$tinta),
      plot.subtitle = element_text(colour = "#5A5A55", size = 11), plot.caption = element_text(colour = "#65655F", size = 8.5, hjust = 0),
      legend.position = "top", legend.title = element_blank(), plot.margin = margin(14, 22, 14, 14)
    )
}

assert_true <- function(x, msg) if (!isTRUE(x)) stop(msg, call. = FALSE)
norm_text <- function(x) {
  y <- iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT")
  tolower(str_squish(y))
}
to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
fill_year <- function(x) {
  out <- rep(NA_integer_, length(x)); current <- NA_integer_
  for (i in seq_along(x)) {
    candidate <- suppressWarnings(as.integer(as.character(x[[i]])))
    if (!is.na(candidate) && candidate >= 1900 && candidate <= 2100) current <- candidate
    out[[i]] <- current
  }
  out
}
month_num <- function(x) {
  key <- norm_text(x)
  unname(c(enero = 1, ene = 1, febrero = 2, feb = 2, marzo = 3, mar = 3, abril = 4, abr = 4, mayo = 5, may = 5, junio = 6, jun = 6, julio = 7, jul = 7, agosto = 8, ago = 8, septiembre = 9, sep = 9, septiembre = 9, sept = 9, octubre = 10, oct = 10, noviembre = 11, nov = 11, diciembre = 12, dic = 12)[key])
}
quarter_num <- function(x) {
  key <- norm_text(gsub("\\s+1/.*$", "", as.character(x)))
  unname(c(i = 1, ii = 2, iii = 3, iv = 4)[key])
}
quarter_label <- function(year, quarter) paste0(year, " T", quarter)

export_plot <- function(plot, dir, slug, width = 10, height = 6.5) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(dir, paste0(slug, ".svg")), plot, device = svglite::svglite, width = width, height = height, bg = pal$crema)
  if (requireNamespace("ragg", quietly = TRUE)) ggsave(file.path(dir, paste0(slug, ".png")), plot, device = ragg::agg_png, width = width, height = height, dpi = 320, bg = pal$crema) else ggsave(file.path(dir, paste0(slug, ".png")), plot, width = width, height = height, dpi = 320, bg = pal$crema)
}

read_encft_indicators <- function(path, sheet = "Indicadores") {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  labels <- norm_text(raw[[1]]); header <- which(labels == "indicador")[1]
  assert_true(!is.na(header), paste("No se encontró encabezado ENCFT en", sheet))
  years <- fill_year(unlist(raw[header, ], use.names = FALSE)); quarters <- vapply(unlist(raw[header + 1, ], use.names = FALSE), quarter_num, numeric(1)); cols <- which(!is.na(years) & !is.na(quarters))
  wanted <- c(participacion = "participacion", ocupacion = "tasa de ocupacion", informalidad = "ocupacion informal", desempleo = "tasa de desocupacion 4")
  out <- lapply(names(wanted), function(name) {
    row <- which(str_detect(labels, fixed(wanted[[name]])))[1]; assert_true(!is.na(row), paste("No se encontró indicador", name))
    tibble(indicador = name, anio = years[cols], trimestre = quarters[cols], fecha = as.Date(sprintf("%04d-%02d-01", years[cols], (quarters[cols] - 1) * 3 + 1)), periodo = quarter_label(years[cols], quarters[cols]), valor = to_num(unlist(raw[row, cols], use.names = FALSE)))
  }) |> bind_rows()
  assert_true(n_distinct(out$fecha) == 47, "La ENCFT no contiene los 47 trimestres esperados."); out
}

read_formal_informal_rama <- function(path) {
  raw <- read_excel(path, sheet = "Rama de Actividad", col_names = FALSE, .name_repair = "minimal"); numeric_rows <- which(!is.na(to_num(raw[[3]]))); latest <- max(numeric_rows)
  formal_cols <- 4:15; informal_cols <- 17:28
  formal <- tibble(rama = as.character(unlist(raw[9, formal_cols])), formal = to_num(unlist(raw[latest, formal_cols])))
  informal <- tibble(rama = as.character(unlist(raw[9, informal_cols])), informal = to_num(unlist(raw[latest, informal_cols])))
  out <- inner_join(formal, informal, by = "rama") |> mutate(rama = str_squish(rama), ocupados = formal + informal, pct_informal = 100 * informal / ocupados) |> filter(!is.na(rama), nzchar(rama), !is.na(pct_informal))
  total_formal <- to_num(raw[[3]][latest]); total_informal <- to_num(raw[[16]][latest])
  assert_true(abs(sum(out$formal) - total_formal) < 0.5, "Las ramas formales ENCFT no reconcilian."); assert_true(abs(sum(out$informal) - total_informal) < 0.5, "Las ramas informales ENCFT no reconcilian."); out
}

read_sector_dimension <- function(path, formal_cols, informal_cols, label_cols) {
  raw <- read_excel(path, sheet = 1, col_names = FALSE, .name_repair = "minimal"); numeric_rows <- which(!is.na(to_num(raw[[3]]))); latest <- max(numeric_rows)
  labels <- as.character(unlist(raw[9, label_cols]))
  out <- tibble(dimension = labels, formal = to_num(unlist(raw[latest, formal_cols])), informal = to_num(unlist(raw[latest, informal_cols]))) |> mutate(dimension = str_squish(dimension), total = formal + informal, pct_informal = 100 * informal / total) |> filter(!is.na(dimension), nzchar(dimension), !is.na(pct_informal))
  assert_true(nrow(out) == length(label_cols), paste("Dimensión ENCFT inesperada en", basename(path))); out
}

read_encft_regions <- function(path) {
  raw <- read_excel(path, sheet = "Regiones", col_names = FALSE, .name_repair = "minimal")
  labels <- as.character(raw[[1]])
  year_rows <- which(str_detect(str_squish(labels), "^20[0-9]{2}$"))
  assert_true(length(year_rows) > 0, "La hoja regional ENCFT no contiene años.")
  year_row <- year_rows[[length(year_rows)]]
  su4_rows <- which(str_detect(norm_text(labels), "^su4:"))
  su4_row <- su4_rows[which(su4_rows > year_row)[1]]
  header_row <- which(str_detect(norm_text(labels), "^indicador$"))[which(which(str_detect(norm_text(labels), "^indicador$")) > year_row)[1]]
  assert_true(!is.na(su4_row) && !is.na(header_row), "No se pudo ubicar SU4 en la hoja regional ENCFT.")
  region_names <- norm_text(unlist(raw[header_row, 2:6], use.names = FALSE))
  values <- to_num(unlist(raw[su4_row, 2:6], use.names = FALSE))
  out <- tibble(region = region_names, su4 = values, anio = as.integer(labels[[year_row]])) |>
    mutate(region = case_when(str_detect(region, "ozama|metropolitana") ~ "ozama", str_detect(region, "norte|cibao") ~ "cibao", str_detect(region, "sur") ~ "sur", str_detect(region, "este") ~ "este", TRUE ~ region)) |>
    filter(region != "total pais")
  assert_true(nrow(out) == 4 && all(is.finite(out$su4)), "Los valores regionales SU4 ENCFT no son completos.")
  out
}

parse_ipc_items <- function(path, item_codes = c("0112201", "0112203")) {
  raw <- read_excel(path, sheet = "2020-2026", col_names = FALSE, .name_repair = "minimal")
  periods <- detect_ipc_periods(raw); years <- periods$years; months <- periods$months; cols <- which(!is.na(years) & !is.na(months))
  codes <- str_extract(as.character(raw[[5]]), "^[0-9]+"); rows <- match(item_codes, codes); assert_true(!anyNA(rows), "No se encontraron todas las series solicitadas en IPC.")
  bind_rows(lapply(seq_along(rows), function(i) tibble(codigo = item_codes[[i]], producto = as.character(raw[[5]][rows[[i]]]), anio = years[cols], mes = months[cols], fecha = as.Date(sprintf("%04d-%02d-01", years[cols], months[cols])), indice = to_num(unlist(raw[rows[[i]], cols], use.names = FALSE)))))
}

detect_ipc_periods <- function(raw) {
  year_scores <- vapply(seq_len(nrow(raw)), function(i) sum(str_detect(as.character(unlist(raw[i, ], use.names = FALSE)), "^20[0-9]{2}$"), na.rm = TRUE), numeric(1))
  month_scores <- vapply(seq_len(nrow(raw)), function(i) sum(!is.na(month_num(unlist(raw[i, ], use.names = FALSE)))), numeric(1))
  year_row <- which.max(year_scores); month_row <- which.max(month_scores)
  assert_true(year_scores[[year_row]] > 0 && month_scores[[month_row]] > 0, "No se pudieron detectar los encabezados temporales del IPC.")
  list(years = fill_year(unlist(raw[year_row, ], use.names = FALSE)), months = month_num(unlist(raw[month_row, ], use.names = FALSE)))
}

parse_ipc_groups <- function(path) {
  raw <- read_excel(path, sheet = "2020-2026", col_names = FALSE, .name_repair = "minimal")
  periods <- detect_ipc_periods(raw); years <- periods$years; months <- periods$months; cols <- which(!is.na(years) & !is.na(months))
  group_rows <- which(str_detect(str_squish(as.character(raw[[1]])), "^[0-9]{2} "))
  assert_true(length(group_rows) == 12, "El IPC no contiene los 12 grupos esperados.")
  out <- bind_rows(lapply(group_rows, function(row) tibble(codigo = str_extract(as.character(raw[[1]][row]), "^[0-9]{2}"), grupo = str_remove(str_squish(as.character(raw[[1]][row])), "^[0-9]{2} "), ponderacion = to_num(raw[[6]][row]), anio = years[cols], mes = months[cols], fecha = as.Date(sprintf("%04d-%02d-01", years[cols], months[cols])), indice = to_num(unlist(raw[row, cols], use.names = FALSE)))))
  assert_true(max(out$fecha, na.rm = TRUE) >= as.Date("2026-06-01"), "La base de grupos IPC no llega a junio de 2026.")
  out
}

parse_ipc_general <- function(path) {
  raw <- read_excel(path, sheet = 1, col_names = FALSE, .name_repair = "minimal"); years <- fill_year(unlist(raw[, 1], use.names = FALSE)); months <- month_num(unlist(raw[, 2], use.names = FALSE))
  fecha <- as.Date(rep(NA_character_, length(years))); ok <- !is.na(years) & !is.na(months); fecha[ok] <- as.Date(sprintf("%04d-%02d-01", years[ok], months[ok]))
  out <- tibble(anio = years, mes = months, fecha = fecha, ipc = to_num(unlist(raw[, 3], use.names = FALSE))) |> filter(!is.na(fecha), !is.na(ipc)); assert_true(max(out$fecha) >= as.Date("2026-01-01"), "La serie IPC general no llega al corte esperado."); out
}

parse_exchange_monthly <- function(path) {
  raw <- read_excel(path, sheet = "PromMensual", col_names = FALSE, .name_repair = "minimal")
  anio <- to_num(unlist(raw[-(1:3), 1])); mes <- month_num(unlist(raw[-(1:3), 2])); fecha <- as.Date(rep(NA_character_, length(anio))); ok <- !is.na(anio) & !is.na(mes); fecha[ok] <- as.Date(sprintf("%04d-%02d-01", anio[ok], mes[ok]))
  out <- tibble(anio = anio, mes = mes, compra = to_num(unlist(raw[-(1:3), 3])), venta = to_num(unlist(raw[-(1:3), 4])), fecha = fecha) |> filter(!is.na(fecha), !is.na(compra)); assert_true(max(out$fecha) >= as.Date("2026-01-01"), "La serie cambiaria no llega al corte esperado."); out
}

parse_remittances <- function(path) {
  raw <- read_excel(path, sheet = 1, col_names = FALSE, .name_repair = "minimal"); years <- to_num(unlist(raw[8, -1], use.names = FALSE)); months <- month_num(unlist(raw[9:20, 1], use.names = FALSE))
  out <- bind_rows(lapply(seq_along(years), function(j) tibble(anio = years[[j]], mes = months, remesas_usd = to_num(unlist(raw[9:20, j + 1], use.names = FALSE)))))
  fecha <- as.Date(rep(NA_character_, nrow(out))); ok <- !is.na(out$anio) & !is.na(out$mes); fecha[ok] <- as.Date(sprintf("%04d-%02d-01", out$anio[ok], out$mes[ok])); mutate(out, fecha = fecha) |> filter(!is.na(fecha), !is.na(remesas_usd))
}

write_chart_map <- function(dir, rows) write_csv(bind_rows(rows), file.path(dir, "chart-map.csv"))

# 1. Trampa del empleo informal
informal_dir <- file.path(repo_root, "research", "trampa-empleo-informal")
informal_fig <- file.path(informal_dir, "figuras")
dir.create(file.path(informal_dir, "data", "procesados"), recursive = TRUE, showWarnings = FALSE)
encft_dir <- file.path(repo_root, "atlas", "data", "raw", "bcrd-encft")
ind <- read_encft_indicators(file.path(encft_dir, "00_Indicadores.xlsx"))
ind_plot <- ind |> filter(indicador %in% c("informalidad", "ocupacion")) |> select(fecha, periodo, indicador, valor) |> pivot_wider(names_from = indicador, values_from = valor) |> mutate(formalidad = 100 - informalidad)
write_csv(ind_plot, file.path(informal_dir, "data", "procesados", "01_tendencia_formal_informal.csv"))
p_informal_trend <- ggplot(ind_plot, aes(fecha)) +
  geom_ribbon(aes(ymin = 0, ymax = informalidad), fill = pal$terracota, alpha = .86) +
  geom_ribbon(aes(ymin = informalidad, ymax = 100), fill = pal$azul, alpha = .80) +
  geom_line(aes(y = informalidad, colour = "Informal"), linewidth = 1.1) + geom_line(aes(y = formalidad, colour = "Formal"), linewidth = 1.1) +
  scale_colour_manual(values = c(Informal = pal$terracota, Formal = pal$azul)) + scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(.01, .05))) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = "Composición del empleo formal e informal", subtitle = "Población ocupada · ENCFT · 2014 T3–2026 T1", x = NULL, y = NULL, caption = "Fuente: BCRD, ENCFT · 2026 T1 preliminar · Elaboración: Leonardo Mena") + theme_editorial()
export_plot(p_informal_trend, informal_fig, "01_formalidad_informalidad_tendencia", 10.5, 6.4)

rama <- read_formal_informal_rama(file.path(encft_dir, "2_1_Sectores_Rama.xlsx")) |> mutate(rama = factor(rama, levels = rama[order(pct_informal)]))
write_csv(rama, file.path(informal_dir, "data", "procesados", "02_informalidad_rama.csv"))
p_rama <- ggplot(rama, aes(pct_informal, rama)) + geom_col(fill = pal$terracota, colour = pal$tinta, linewidth = .25, width = .68) + geom_text(aes(label = sprintf("%.1f%%", pct_informal)), hjust = -.12, size = 3.2, fontface = "bold") + scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, 108), breaks = seq(0, 100, 20)) + labs(title = "Tasa de informalidad por rama de actividad", subtitle = "Porcentaje de ocupados informales dentro de cada rama · último trimestre disponible", x = NULL, y = NULL, caption = "Fuente: BCRD, ENCFT · cálculo sobre sector formal e informal · Elaboración: Leonardo Mena") + theme_editorial("x")
export_plot(p_rama, informal_fig, "02_informalidad_por_rama", 10.5, 7.2)
rama_heat <- rama |> mutate(rama = as.character(rama)) |> select(rama, formal, informal) |> pivot_longer(c(formal, informal), names_to = "estado", values_to = "ocupados") |> mutate(estado = recode(estado, formal = "Formal", informal = "Informal"), ocupados_millones = ocupados / 1e6)
write_csv(rama_heat, file.path(informal_dir, "data", "procesados", "05_matriz_formal_informal_rama.csv"))
p_rama_heat <- ggplot(rama_heat, aes(estado, rama, fill = ocupados_millones)) + geom_tile(colour = pal$crema, linewidth = .8) + geom_text(aes(label = sprintf("%.1f M", ocupados_millones)), fontface = "bold", size = 3.3) + scale_fill_gradient(low = pal$crema, high = pal$azul, labels = label_number(suffix = " M", accuracy = .1)) + labs(title = "Ocupados formales e informales por rama", subtitle = "Población ocupada · último trimestre disponible · ENCFT", x = NULL, y = NULL, fill = "Millones", caption = "Fuente: BCRD, ENCFT · Elaboración: Leonardo Mena") + theme_editorial("none")
export_plot(p_rama_heat, informal_fig, "05_matriz_formal_informal_rama", 9, 7.2)

educ <- read_sector_dimension(file.path(encft_dir, "2_5_Sectores_Educacion.xlsx"), 4:7, 9:12, 4:7) |> mutate(dimension = recode(dimension, `Primario /2` = "Primario", `Secundario /2` = "Secundario"), dimension = factor(dimension, levels = dimension[order(pct_informal)]))
write_csv(educ, file.path(informal_dir, "data", "procesados", "03_informalidad_educacion.csv"))
p_educ <- ggplot(educ, aes(pct_informal, dimension)) + geom_col(fill = pal$oliva, width = .62, colour = pal$tinta, linewidth = .25) + geom_text(aes(label = sprintf("%.1f%%", pct_informal)), hjust = -.12, size = 3.4, fontface = "bold") + scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, 108), breaks = seq(0, 100, 20)) + labs(title = "Tasa de informalidad por nivel educativo", subtitle = "Porcentaje de ocupados informales dentro de cada nivel educativo · ENCFT · último trimestre disponible", x = NULL, y = NULL, caption = "Fuente: BCRD, ENCFT · porcentajes calculados con población ocupada formal e informal") + theme_editorial("x")
export_plot(p_educ, informal_fig, "03_informalidad_por_educacion", 9, 5.8)

topes_tss_2026 <- list(sfs = 232230, pension = 464460, riesgos = 92892)
formalization <- tibble(salario = seq(17701.25, 120000, by = 500)) |> mutate(
  sfs_empleador = .0709 * pmin(salario, topes_tss_2026$sfs), pension_empleador = .0710 * pmin(salario, topes_tss_2026$pension), riesgos_bajo = .011 * pmin(salario, topes_tss_2026$riesgos), infotep = .01 * salario,
  salario_neto_sin_isr = salario - .0304 * pmin(salario, 232230) - .0287 * pmin(salario, 464460), costo_directo_bajo = salario + sfs_empleador + pension_empleador + riesgos_bajo + infotep,
  sueldo_13 = salario / 12, vacaciones_14_dias = salario * 14 / 360, costo_anualizado_bajo = costo_directo_bajo + sueldo_13 + vacaciones_14_dias)
write_csv(formalization, file.path(informal_dir, "data", "procesados", "04_costo_formalizacion.csv"))
p_cost <- ggplot(formalization, aes(salario)) + geom_line(aes(y = costo_directo_bajo, colour = "Costo mensual directo"), linewidth = 1.1) + geom_line(aes(y = costo_anualizado_bajo, colour = "Costo con beneficios anualizados"), linewidth = 1.1) + geom_line(aes(y = salario, colour = "Salario bruto"), linewidth = .8, linetype = "dashed") + scale_colour_manual(values = c("Costo mensual directo" = pal$terracota, "Costo con beneficios anualizados" = pal$ocre, "Salario bruto" = pal$azul)) + scale_x_continuous(labels = label_number(prefix = "RD$ ", big.mark = ".")) + scale_y_continuous(labels = label_number(prefix = "RD$ ", big.mark = ".")) + labs(title = "Costo mensual equivalente de formalizar a un empleado", subtitle = "Escenario 2026 · riesgo laboral bajo · sin ISR, cesantía ni costos de contratación", x = "Salario bruto mensual", y = "Costo mensual equivalente", caption = "Fuentes: TSS, Guía del usuario 2024 y Resolución TSS 01-2025; INFOTEP · Elaboración: Leonardo Mena") + theme_editorial()
export_plot(p_cost, informal_fig, "04_costo_legal_formalizacion", 10.5, 6.4)

latest_informal <- ind |> filter(indicador == "informalidad") |> slice_max(fecha, n = 1, with_ties = FALSE)
waffle <- tidyr::expand_grid(col = 1:10, row = 1:10) |> arrange(desc(row), col) |> mutate(
  periodo = latest_informal$periodo[[1]], porcentaje_total = latest_informal$valor[[1]],
  estado = if_else(row_number() <= round(porcentaje_total), "Informal", "Resto de ocupados"))
write_csv(waffle, file.path(informal_dir, "data", "procesados", "06_proporcion_informal_100.csv"))
p_waffle <- ggplot(waffle, aes(col, row, fill = estado)) +
  geom_tile(colour = pal$crema, linewidth = .85, width = .9, height = .9) +
  scale_fill_manual(values = c("Informal" = pal$terracota, "Resto de ocupados" = pal$gris_claro)) +
  coord_equal() + scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
  labs(title = sprintf("%.1f%% de la población ocupada es informal", latest_informal$valor[[1]]),
       subtitle = paste0("Cada celda representa aproximadamente 1% del total · ENCFT · ", latest_informal$periodo[[1]]),
       caption = "Fuente: BCRD, ENCFT · la cuadrícula representa la proporción observada, no un conteo de personas") +
  theme_editorial("none") + theme(legend.position = "none", axis.title = element_blank(), plot.margin = margin(22, 26, 18, 26)) +
  annotate("text", x = 10, y = 10.8, label = "celdas iluminadas = empleo informal", hjust = 1, colour = pal$terracota, size = 3.4, fontface = "bold")
export_plot(p_waffle, informal_fig, "06_proporcion_informal_100", 8.5, 7.2)

regional <- read_encft_regions(file.path(encft_dir, "00_Indicadores.xlsx"))
region_key <- tibble(region_code = sprintf("%02d", 1:10), region_macro = c(rep("cibao", 4), rep("sur", 3), rep("este", 2), "ozama"))
rd_regions <- st_read(file.path(repo_root, "mapa_rd", "region", "REGCenso2010.shp"), quiet = TRUE) |>
  mutate(region_code = as.character(REG)) |>
  left_join(region_key, by = "region_code") |>
  left_join(regional |> rename(region_macro = region), by = "region_macro")
assert_true(nrow(rd_regions) == 10 && all(!is.na(rd_regions$su4)), "El mapa regional ENCFT quedo incompleto.")
rd_macro <- rd_regions |> group_by(region_macro) |> summarise(su4 = first(su4), .groups = "drop") |> mutate(region_label = c("Cibao", "Este", "Ozama", "Sur")[match(region_macro, c("cibao", "este", "ozama", "sur"))])
write_csv(st_drop_geometry(rd_regions) |> select(region_code, region_macro, su4), file.path(informal_dir, "data", "procesados", "07_mapa_rd_presion_laboral.csv"))
su4_limits <- c(floor(min(regional$su4)) - 1, ceiling(max(regional$su4)) + 1)
su4_breaks <- pretty(su4_limits, n = 5)
p_rd_su4 <- ggplot(rd_regions) + geom_sf(aes(fill = su4), colour = pal$crema, linewidth = .42) +
  geom_sf_text(data = st_point_on_surface(rd_macro), aes(label = sprintf("%s\n%.1f%%", region_label, su4)), colour = pal$tinta, fontface = "bold", size = 3.4, lineheight = .9) +
  scale_fill_gradient(low = pal$crema, high = pal$terracota, limits = su4_limits, breaks = su4_breaks, labels = label_percent(scale = 1), name = "SU4") +
  labs(title = "Índice SU4 por macroregión", subtitle = "Presión laboral ampliada por macroregión · escala cromática ajustada al rango observado", caption = sprintf("Fuente: BCRD, ENCFT, corte regional %s · SU4 no es una tasa de informalidad · escala: %.0f–%.0f%%", regional$anio[[1]], su4_limits[[1]], su4_limits[[2]])) +
  theme_editorial("none") + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "right")
export_plot(p_rd_su4, informal_fig, "07_mapa_rd_presion_laboral", 8.5, 7.3)

scenario_cost <- tibble(escenario = c("Salario minimo", "Salario bajo", "Salario medio", "Salario alto", "Salario muy alto"), salario = c(17701.25, 30000, 50000, 250000, 500000)) |> mutate(
  sfs_empleador = .0709 * pmin(salario, topes_tss_2026$sfs), pension_empleador = .0710 * pmin(salario, topes_tss_2026$pension), riesgos_bajo = .011 * pmin(salario, topes_tss_2026$riesgos), infotep = .01 * salario,
  costo_directo_bajo = salario + sfs_empleador + pension_empleador + riesgos_bajo + infotep,
  sueldo_13 = salario / 12, vacaciones_14_dias = salario * 14 / 360,
  costo_anualizado_bajo = costo_directo_bajo + sueldo_13 + vacaciones_14_dias,
  sobrecosto_directo = costo_directo_bajo - salario, sobrecosto_anualizado = costo_anualizado_bajo - salario,
  sobrecosto_directo_pct = 100 * sobrecosto_directo / salario, sobrecosto_anualizado_pct = 100 * sobrecosto_anualizado / salario,
  tope_sfs_2026 = topes_tss_2026$sfs, tope_pension_2026 = topes_tss_2026$pension, tope_riesgos_2026 = topes_tss_2026$riesgos,
  escenario = factor(escenario, levels = c("Salario minimo", "Salario bajo", "Salario medio", "Salario alto", "Salario muy alto")))
write_csv(scenario_cost, file.path(informal_dir, "data", "procesados", "08_costo_formalizacion_escenarios.csv"))
scenario_long <- scenario_cost |> select(escenario, sobrecosto_directo_pct, sobrecosto_anualizado_pct) |> pivot_longer(-escenario, names_to = "concepto", values_to = "porcentaje") |> mutate(concepto = recode(concepto, sobrecosto_directo_pct = "Costo directo adicional", sobrecosto_anualizado_pct = "Costo anualizado adicional"))
p_scenario_cost <- ggplot(scenario_long, aes(escenario, porcentaje, fill = concepto)) + geom_col(position = position_dodge(width = .78), width = .68, colour = pal$tinta, linewidth = .2) +
  geom_text(aes(label = sprintf("%.1f%%", porcentaje)), position = position_dodge(width = .78), vjust = -.35, size = 2.7, fontface = "bold") +
  scale_fill_manual(values = c("Costo directo adicional" = pal$terracota, "Costo anualizado adicional" = pal$ocre)) + scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, .18))) +
  labs(title = "Costo adicional de formalización como porcentaje del salario", subtitle = "Escenarios con topes de cotización · 2026 · sin ISR ni cesantía", x = NULL, y = "Costo adicional sobre el salario", caption = "Fuentes: TSS, Resolución 01-2025 e INFOTEP · topes 2026: SFS RD$232,230; pensiones RD$464,460; riesgos RD$92,892") + theme_editorial("none") + theme(axis.text.x = element_text(size = 8.5))
export_plot(p_scenario_cost, informal_fig, "08_costo_formalizacion_escenarios", 10.5, 6.6)
write_chart_map(informal_dir, list(
  tibble(id = "01_formalidad_informalidad_tendencia", pregunta = "¿Cómo cambió la composición formal/informal?", familia = "Composición temporal", fuente = "BCRD ENCFT", advertencia = "2026 T1 preliminar"),
  tibble(id = "02_informalidad_por_rama", pregunta = "¿Dónde se concentra la informalidad?", familia = "Barras ordenadas", fuente = "BCRD ENCFT", advertencia = "Corte más reciente"),
  tibble(id = "03_informalidad_por_educacion", pregunta = "¿Qué relación tiene la educación con la formalidad?", familia = "Barras ordenadas", fuente = "BCRD ENCFT", advertencia = "No es efecto causal"),
  tibble(id = "04_costo_legal_formalizacion", pregunta = "¿Qué suma la legislación al costo de un empleado?", familia = "Líneas de escenarios", fuente = "TSS/INFOTEP", advertencia = "Vacaciones anualizadas; no incluye cesantía ni ISR"),
  tibble(id = "06_proporcion_informal_100", pregunta = "¿Qué proporción del empleo es informal?", familia = "Cuadrícula proporcional", fuente = "BCRD ENCFT", advertencia = "100 celdas; cada celda equivale aproximadamente a 1%"),
  tibble(id = "07_mapa_rd_presion_laboral", pregunta = "¿Dónde es más amplia la presión laboral?", familia = "Mapa de RD", fuente = "BCRD ENCFT", advertencia = "SU4 regional; no es informalidad provincial"),
  tibble(id = "08_costo_formalizacion_escenarios", pregunta = "¿Cuánto cuesta formalizar distintos salarios?", familia = "Escenarios comparables", fuente = "TSS/INFOTEP", advertencia = "Supuestos ilustrativos; no incluye ISR ni cesantía")
))

informal_map <- read_csv(file.path(informal_dir, "chart-map.csv"), show_col_types = FALSE) |> bind_rows(tibble(id = "05_matriz_formal_informal_rama", pregunta = "Composición del empleo por rama", familia = "Mapa de calor", fuente = "BCRD ENCFT", advertencia = "Intensidad: millones de ocupados")); write_csv(informal_map, file.path(informal_dir, "chart-map.csv"))

# 2. Peso fuerte, remesas que rinden menos
remesas_dir <- file.path(repo_root, "research", "peso-fuerte-remesas"); remesas_fig <- file.path(remesas_dir, "figuras"); dir.create(file.path(remesas_dir, "data", "procesados"), recursive = TRUE, showWarnings = FALSE)
ipc <- parse_ipc_general(file.path(repo_root, "atlas", "data", "raw", "bcrd-precios", "ipc_base_2019-2020.xls")); fx <- parse_exchange_monthly(file.path(repo_root, "atlas", "data", "raw", "bcrd-mercado-cambiario", "TASA_DOLAR_REFERENCIA_MC.xlsx")); rem <- parse_remittances(file.path(repo_root, "atlas", "data", "raw", "bcrd-sector-externo", "Remesas_6.xlsx"))
rem_data <- rem |> inner_join(fx |> select(fecha, compra), by = "fecha") |> inner_join(ipc |> select(fecha, ipc), by = "fecha") |> mutate(remesas_millones_usd = remesas_usd / 1e6, dops_por_100usd = 100 * compra, dops_reales_por_100usd = dops_por_100usd / ipc * 100, remesas_reales_millones_dop = remesas_usd * compra / ipc * 100 / 1e6)
assert_true(nrow(rem_data) >= 150, "La serie de remesas no tiene cobertura mensual suficiente."); write_csv(rem_data, file.path(remesas_dir, "data", "procesados", "01_remesas_tipo_cambio_ipc.csv"))
indexed <- rem_data |> filter(fecha >= as.Date("2010-01-01")) |> mutate(across(c(remesas_millones_usd, dops_por_100usd, dops_reales_por_100usd), ~ 100 * .x / .x[which.min(fecha)])) |> select(fecha, remesas_millones_usd, dops_por_100usd, dops_reales_por_100usd) |> pivot_longer(-fecha, names_to = "serie", values_to = "indice")
p_rem <- ggplot(indexed, aes(fecha, indice, colour = serie)) + geom_line(linewidth = 1) + scale_colour_manual(values = c(remesas_millones_usd = pal$oliva, dops_por_100usd = pal$azul, dops_reales_por_100usd = pal$terracota), labels = c(remesas_millones_usd = "Remesas en USD", dops_por_100usd = "RD$ por US$100", dops_reales_por_100usd = "Poder de compra de US$100")) + scale_x_date(date_breaks = "2 years", date_labels = "%Y") + labs(title = "Remesas en dólares, pesos recibidos y poder de compra", subtitle = "Índice: enero de 2010 = 100 · remesas nominales, tipo de cambio y poder de compra", x = NULL, y = "Índice", caption = "Fuentes: BCRD, remesas, tipo de cambio e IPC nacional · Elaboración: Leonardo Mena") + theme_editorial()
export_plot(p_rem, remesas_fig, "01_remesas_tipo_cambio_poder_compra", 10.5, 6.4)
power <- rem_data |> filter(fecha >= as.Date("2010-01-01")) |> mutate(real_vs_nominal = 100 * dops_reales_por_100usd / dops_por_100usd); write_csv(power, file.path(remesas_dir, "data", "procesados", "02_poder_compra_remesa.csv"))
p_power <- ggplot(power, aes(fecha, real_vs_nominal)) + geom_hline(yintercept = 100, linetype = "dashed", colour = pal$gris) + geom_line(colour = pal$terracota, linewidth = 1.1) + scale_x_date(date_breaks = "2 years", date_labels = "%Y") + scale_y_continuous(labels = label_percent(scale = 1), breaks = seq(70, 110, 10)) + labs(title = "Poder de compra real de US$100 enviados", subtitle = "Pesos recibidos deflactados por IPC nacional · enero de 2010 = 100", x = NULL, y = "Poder de compra relativo", caption = "Cálculo: tipo de cambio de compra deflactado por IPC nacional · Fuente: BCRD") + theme_editorial()
power <- power |> mutate(real_vs_nominal = 100 * dops_reales_por_100usd / dops_reales_por_100usd[which.min(fecha)])
p_power <- p_power %+% power
write_csv(power, file.path(remesas_dir, "data", "procesados", "02_poder_compra_remesa.csv"))
export_plot(p_power, remesas_fig, "02_poder_compra_remesas", 10.5, 5.8)
power_heat <- power |> mutate(anio = as.integer(format(fecha, "%Y")), mes = as.integer(format(fecha, "%m"))) |> select(anio, mes, real_vs_nominal)
write_csv(power_heat, file.path(remesas_dir, "data", "procesados", "03_mapa_calor_poder_compra.csv"))
p_power_heat <- ggplot(power_heat, aes(mes, anio, fill = real_vs_nominal)) + geom_tile(colour = pal$crema, linewidth = .35) + geom_text(aes(label = sprintf("%.0f", real_vs_nominal)), size = 2.4, colour = pal$tinta) + scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"), expand = c(0, 0)) + scale_y_continuous(breaks = sort(unique(power_heat$anio)), expand = c(0, 0)) + scale_fill_gradient2(low = pal$terracota, mid = pal$crema, high = pal$azul, midpoint = 100, labels = label_percent(scale = 1), name = "Poder relativo") + labs(title = "Poder de compra de US$100 por mes y año", subtitle = "Índice relativo · enero de 2010 = 100", x = NULL, y = NULL, caption = "Cálculo: pesos recibidos deflactados por IPC nacional · Fuente: BCRD") + theme_editorial("none")
export_plot(p_power_heat, remesas_fig, "03_mapa_calor_poder_compra", 10.5, 7.2)
write_chart_map(remesas_dir, list(tibble(id = "01_remesas_tipo_cambio_poder_compra", pregunta = "¿Qué ocurre con una remesa cuando cambia el peso?", familia = "Índice temporal", fuente = "BCRD", advertencia = "Deflactor: IPC nacional"), tibble(id = "02_poder_compra_remesas", pregunta = "¿Cuánto poder de compra queda?", familia = "Línea con referencia", fuente = "BCRD", advertencia = "No estima la canasta de cada hogar")))

# 3. Trampa estructural del sector eléctrico
remesas_map <- read_csv(file.path(remesas_dir, "chart-map.csv"), show_col_types = FALSE) |> bind_rows(tibble(id = "03_mapa_calor_poder_compra", pregunta = "Mapa mensual del poder de compra", familia = "Mapa de calor", fuente = "BCRD", advertencia = "Índice relativo, no canasta individual")); write_csv(remesas_map, file.path(remesas_dir, "chart-map.csv"))

electric_dir <- file.path(repo_root, "research", "sector-electrico-dominicano"); electric_fig <- file.path(electric_dir, "figuras"); electric_raw <- file.path(electric_dir, "data", "raw", "mem"); electric_proc <- file.path(electric_dir, "data", "procesados"); dir.create(electric_proc, recursive = TRUE, showWarnings = FALSE)
gen_pdf <- file.path(electric_raw, "boletin-generacion-2024.pdf"); dist_pdf <- file.path(electric_raw, "boletin-distribucion-diciembre-2024.pdf"); gen_text <- paste(pdf_text(gen_pdf), collapse = "\n"); dist_text <- paste(pdf_text(dist_pdf), collapse = "\n"); assert_true(str_detect(gen_text, "23,056"), "No se pudo validar el total de generación del PDF MEM."); assert_true(str_detect(dist_text, "37.6%"), "No se pudo validar la pérdida de energía del PDF MEM.")
generation <- tibble(fuente = c("Gas natural", "Carbón mineral", "Fuel Oil No. 6", "Solar", "Hidro", "Eólica", "Biomasa", "Fuel Oil No. 2"), gwh = c(9554, 6851, 2480, 1470, 1422, 1061, 207, 11), pagina = 3) |> mutate(pct = 100 * gwh / sum(gwh), fuente = factor(fuente, levels = fuente[order(gwh)])); assert_true(sum(generation$gwh) == 23056, "La matriz de generación no suma el total del boletín MEM."); write_csv(generation, file.path(electric_proc, "01_generacion_2024.csv"))
p_gen <- ggplot(generation, aes(gwh, fuente)) + geom_col(fill = pal$azul, colour = pal$tinta, linewidth = .25, width = .68) + geom_text(aes(label = sprintf("%.0f GWh · %.0f%%", gwh, pct)), hjust = -.08, size = 3.2, fontface = "bold") + scale_x_continuous(labels = label_number(big.mark = "."), limits = c(0, 11000), expand = expansion(mult = c(0, .05))) + labs(title = "Generación eléctrica del SENI por combustible", subtitle = "Generación acumulada · 2024", x = "GWh", y = NULL, caption = "Fuente: MEM, Boletín anual de Generación y Gestión de Energía 2024, p. 3 · Elaboración: Leonardo Mena") + theme_editorial("x")
export_plot(p_gen, electric_fig, "01_matriz_generacion_2024", 10.5, 6.8)
losses <- tibble(indicador = c("Energía comprada", "Energía facturada", "Energía cobrada"), gwh = c(1596.7, 990.4, 1035.2), pagina = 3) |> mutate(indicador = factor(indicador, levels = indicador)); ratios <- tibble(indicador = c("Pérdida de energía", "Recuperación de energía", "Recuperación de efectivo", "Cobranza"), porcentaje = c(37.6, 59.5, 60.9, 95.3), pagina = 5); write_csv(losses, file.path(electric_proc, "02_energia_distribucion_diciembre_2024.csv")); write_csv(ratios, file.path(electric_proc, "03_indicadores_distribucion_diciembre_2024.csv"))
p_loss <- ggplot(ratios, aes(porcentaje, reorder(indicador, porcentaje))) + geom_col(fill = pal$terracota, colour = pal$tinta, width = .62) + geom_text(aes(label = sprintf("%.1f%%", porcentaje)), hjust = -.12, size = 3.4, fontface = "bold") + scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, 108), breaks = seq(0, 100, 20)) + labs(title = "Indicadores agregados de pérdidas y cobros de las EDE", subtitle = "Indicadores agregados · diciembre de 2024", x = NULL, y = NULL, caption = "Fuente: MEM, Boletín de Distribución y Comercialización, pp. 4–5 · Elaboración: Leonardo Mena") + theme_editorial("x")
export_plot(p_loss, electric_fig, "02_perdidas_recuperacion_cobranza", 10, 5.8)
gestion_heat <- tibble(indicador = rep(c("Cobranza", "Recuperación de energía", "Recuperación de efectivo"), each = 2), periodo = rep(c("Dic 2023", "Dic 2024"), 3), porcentaje = c(95.1, 95.3, 61.0, 59.5, 59.4, 60.9))
write_csv(gestion_heat, file.path(electric_proc, "05_mapa_calor_indicadores_gestion.csv"))
p_gestion_heat <- ggplot(gestion_heat, aes(periodo, indicador, fill = porcentaje)) + geom_tile(colour = pal$crema, linewidth = .8) + geom_text(aes(label = sprintf("%.1f%%", porcentaje)), fontface = "bold", size = 3.5) + scale_fill_gradient(low = pal$crema, high = pal$terracota, limits = c(0, 100), labels = label_percent(scale = 1), name = "Porcentaje") + labs(title = "Indicadores de gestión de las EDE", subtitle = "Comparación de diciembre de 2023 y diciembre de 2024", x = NULL, y = NULL, caption = "Fuente: MEM, Boletín de Distribución y Comercialización · Elaboración: Leonardo Mena") + theme_editorial("none")
export_plot(p_gestion_heat, electric_fig, "05_mapa_calor_indicadores_gestion", 8.5, 5.8)
deficit <- tibble(concepto = c("Balance comercial", "Balance corriente", "Balance global", "Subsidio del Gobierno Central"), valor = c(1025.3, 1518.7, 1735.4, 1554.6), pagina = 7) |> mutate(concepto = factor(concepto, levels = concepto[order(valor)])); write_csv(deficit, file.path(electric_proc, "04_deficit_distribuidoras_2024.csv"))
p_deficit <- ggplot(deficit, aes(valor, concepto)) + geom_col(fill = pal$ocre, colour = pal$tinta, width = .62) + geom_text(aes(label = sprintf("US$ %.1f MM", valor)), hjust = -.08, size = 3.3, fontface = "bold") + scale_x_continuous(labels = label_number(prefix = "US$ ", suffix = " MM"), limits = c(0, 1900), expand = expansion(mult = c(0, .05))) + labs(title = "Déficits acumulados y subsidios reportados por las EDE", subtitle = "Montos reportados · 2024", x = NULL, y = NULL, caption = "Fuente: MEM, Boletín de Distribución y Comercialización, p. 7 · Elaboración: Leonardo Mena") + theme_editorial("x")
export_plot(p_deficit, electric_fig, "03_deficit_y_subsidio_electricidad", 10.5, 5.8)
electric_pdf_candidates <- list.files(electric_raw, pattern = "gestion-comercial.*2026.*abril.*pdf$", full.names = TRUE, ignore.case = TRUE)
assert_true(length(electric_pdf_candidates) > 0, "No se encontro el informe MEM 2026 para el mapa provincial.")
electric_pdf <- electric_pdf_candidates[[1]]
electric_pages <- pdf_text(electric_pdf)
electric_page <- electric_pages[[max(which(str_detect(electric_pages, "Indicadores por Provincia")) )]]
assert_true(str_detect(electric_page, "Total Provincias") && str_detect(electric_page, "EdeEste"), "No se pudo validar la tabla provincial del informe MEM.")
province_loss_raw <- tribble(
  ~provincia, ~energia_suministrada_gwh, ~perdida_gwh,
  "Dajabon", 102.2, 15.05, "Duarte", 549.8, 169.42, "Espaillat", 314.1, 89.86, "La Vega", 695.2, 189.05,
  "Maria Trinidad Sanchez", 262.6, 110.52, "Monseñor Nouel", 365.7, 177.25, "Monte Cristi", 151.3, 37.81, "Puerto Plata", 728.9, 95.44,
  "Hermanas Mirabal", 130.9, 47.34, "Samana", 157.1, 72.46, "Sanchez Ramirez", 208.0, 89.69, "Santiago", 2033.8, 400.87,
  "Santiago Rodriguez", 139.9, 10.94, "Valverde", 247.6, 52.43,
  "Azua", 335.37, 161.08, "Baoruco", 246.93, 162.47, "Barahona", 288.23, 152.94, "Distrito Nacional", 2536.47, 323.11,
  "Elias Pina", 36.56, 12.53, "Independencia", 35.53, 17.48, "Pedernales", 33.94, 16.84, "Peravia", 406.52, 202.85,
  "San Cristobal", 1127.72, 468.91, "San Jose de Ocoa", 48.96, 15.04, "San Juan", 218.51, 52.19, "Santo Domingo", 1665.46, 596.64,
  "Distrito Nacional", 878.2, 326.84, "El Seibo", 107.3, 59.43, "Hato Mayor", 139.9, 74.14, "La Altagracia", 651.5, 435.57,
  "La Romana", 556.7, 343.38, "Monte Plata", 368.8, 252.33, "San Pedro de Macoris", 733.2, 431.52, "Santo Domingo", 4054.1, 2254.68
) |> mutate(provincia_key = norm_text(provincia))
assert_true(nrow(province_loss_raw) == 34 && str_detect(norm_text(electric_page), "dajabon") && str_detect(norm_text(electric_page), "total provincias"), "No se pudo validar la cobertura de la tabla provincial del PDF MEM.")
province_loss <- province_loss_raw |> group_by(provincia_key) |> summarise(provincia = first(provincia), energia_suministrada_gwh = sum(energia_suministrada_gwh), perdida_gwh = sum(perdida_gwh), .groups = "drop") |> mutate(perdida_pct = 100 * perdida_gwh / energia_suministrada_gwh, corte = "may 2025 - abr 2026", fuente_pdf = basename(electric_pdf), definicion_perdida = "Energia suministrada menos energia facturada; perdida total reportada", componentes = "El informe no desagrega perdidas tecnicas y no tecnicas")
rd_prov <- st_read(file.path(repo_root, "mapa_rd", "provincia", "PROVCenso2010.shp"), quiet = TRUE) |> mutate(provincia_key = norm_text(TOPONIMIA)) |> left_join(province_loss, by = "provincia_key")
assert_true(nrow(rd_prov) == 32 && all(!is.na(rd_prov$perdida_pct)), "El join provincial de electricidad dejo geometrias sin datos.")
write_csv(st_drop_geometry(rd_prov) |> select(PROV, TOPONIMIA, provincia, energia_suministrada_gwh, perdida_gwh, perdida_pct, corte, fuente_pdf, definicion_perdida, componentes), file.path(electric_proc, "06_mapa_rd_perdidas_electricas_provincia.csv"))
electric_extremes <- bind_rows(slice_max(rd_prov, perdida_pct, n = 2, with_ties = FALSE), slice_min(rd_prov, perdida_pct, n = 2, with_ties = FALSE)) |> distinct(PROV, .keep_all = TRUE) |> mutate(label_text = sprintf("%s\n%.1f%%", provincia, perdida_pct))
p_electric_map <- ggplot(rd_prov) + geom_sf(aes(fill = perdida_pct), colour = pal$crema, linewidth = .42) +
  geom_sf_label(data = st_point_on_surface(electric_extremes), aes(label = label_text), size = 2.6, fontface = "bold", linewidth = .15, label.padding = unit(.12, "lines"), colour = pal$tinta, fill = pal$blanco) +
  scale_fill_gradient(low = pal$crema, high = pal$terracota, limits = c(0, 70), breaks = seq(0, 70, 10), labels = label_percent(scale = 1), name = "Pérdida") +
  labs(title = "Pérdida total de energía reportada por provincia", subtitle = "Porcentaje de la energía suministrada · corte mayo 2025–abril 2026", caption = "Fuente: MEM, Informe de Gestión Comercial EDE 2026 · no separa pérdidas técnicas y no técnicas") +
  theme_editorial("none") + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "right")
electric_inset <- ggplot(rd_prov |> filter(provincia_key %in% c("distrito nacional", "santo domingo"))) + geom_sf(aes(fill = perdida_pct), colour = pal$crema, linewidth = .45) + geom_sf_label(aes(label = sprintf("%s\n%.1f%%", provincia, perdida_pct)), size = 2.3, fontface = "bold", linewidth = .1, label.padding = unit(.1, "lines"), colour = pal$tinta, fill = pal$blanco) + scale_fill_gradient(low = pal$crema, high = pal$terracota, limits = c(0, 70), guide = "none") + labs(title = "Santo Domingo") + theme_void() + theme(plot.title = element_text(size = 8, face = "bold", colour = pal$tinta), plot.background = element_rect(fill = pal$crema, colour = pal$tinta, linewidth = .4), panel.background = element_rect(fill = pal$crema, colour = NA), plot.margin = margin(3, 3, 3, 3))
p_electric_map <- p_electric_map + inset_element(electric_inset, left = .70, bottom = .08, right = .98, top = .36, align_to = "panel", on_top = TRUE)
export_plot(p_electric_map, electric_fig, "06_mapa_rd_perdidas_electricas_provincia", 8.5, 7.3)
write_chart_map(electric_dir, list(tibble(id = "01_matriz_generacion_2024", pregunta = "¿De dónde sale la energía?", familia = "Barras ordenadas", fuente = "MEM", advertencia = "Cifras acumuladas 2024"), tibble(id = "02_perdidas_recuperacion_cobranza", pregunta = "¿Qué parte de la energía se pierde o recupera?", familia = "Barras comparables", fuente = "MEM", advertencia = "Indicadores no son partes aditivas"), tibble(id = "03_deficit_y_subsidio_electricidad", pregunta = "¿Cómo llega la ineficiencia al fisco?", familia = "Barras monetarias", fuente = "MEM", advertencia = "Valores acumulados 2024")))

# 4. Pollo y dólar
electric_map <- read_csv(file.path(electric_dir, "chart-map.csv"), show_col_types = FALSE) |> bind_rows(tibble(id = "05_mapa_calor_indicadores_gestion", pregunta = "¿Cómo cambiaron los indicadores de gestión?", familia = "Mapa de calor", fuente = "MEM", advertencia = "Solo indicadores con comparación explícita en el boletín"), tibble(id = "06_mapa_rd_perdidas_electricas_provincia", pregunta = "¿Dónde se concentra la energía perdida?", familia = "Mapa de RD", fuente = "MEM", advertencia = "Corte móvil; EDE superpuestas agregadas por GWh")); write_csv(electric_map, file.path(electric_dir, "chart-map.csv"))

pollo_dir <- file.path(repo_root, "research", "pollo-inflacion"); pollo_fig <- file.path(pollo_dir, "figuras"); dir.create(file.path(pollo_dir, "data", "procesados"), recursive = TRUE, showWarnings = FALSE)
pollo <- parse_ipc_items(file.path(repo_root, "atlas", "data", "raw", "bcrd-precios", "ipc_articulos_base_2019-2020.xlsx")) |> mutate(producto = recode(codigo, `0112201` = "Pollo fresco", `0112203` = "Pechuga de pollo"))
carnes <- read_excel(file.path(repo_root, "atlas", "data", "raw", "bcrd-precios", "ipc_articulos_base_2019-2020.xlsx"), sheet = "2020-2026", col_names = FALSE, .name_repair = "minimal"); years <- fill_year(unlist(carnes[1, ], use.names = FALSE)); months <- month_num(unlist(carnes[3, ], use.names = FALSE)); cols <- which(!is.na(years) & !is.na(months)); row_carnes <- which(as.character(carnes[[3]]) == "0112 Carnes")[1]
carnes_long <- tibble(fecha = as.Date(sprintf("%04d-%02d-01", years[cols], months[cols])), producto = "Carnes", indice = to_num(unlist(carnes[row_carnes, cols], use.names = FALSE))); pollo <- bind_rows(pollo |> select(fecha, producto, indice), carnes_long) |> inner_join(fx |> select(fecha, compra), by = "fecha"); write_csv(pollo, file.path(pollo_dir, "data", "procesados", "01_pollo_carnes_dolar.csv"))
pollo_index <- pollo |> group_by(producto) |> arrange(fecha, .by_group = TRUE) |> mutate(indice_base = 100 * indice / indice[which.min(fecha)], dolar_base = 100 * compra / compra[which.min(fecha)]) |> ungroup()
pollo_full <- pollo_index
pollo_plot <- pollo_index |> filter(fecha >= as.Date("2020-10-01"))
pollo_index <- pollo_plot
p_pollo <- ggplot(pollo_index, aes(fecha)) + geom_line(aes(y = indice_base, colour = producto), linewidth = 1) + geom_line(aes(y = dolar_base, colour = "Dólar"), linewidth = 1, linetype = "dashed") + scale_colour_manual(values = c("Pollo fresco" = pal$terracota, "Pechuga de pollo" = pal$ocre, "Carnes" = pal$oliva, "Dólar" = pal$azul)) + scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = expansion(mult = c(.01, .04))) + labs(title = "Índices de precios del pollo, carnes y tipo de cambio", subtitle = "Octubre de 2020 = 100 · pollo fresco, pechuga, carnes y dólar", x = NULL, y = "Índice", caption = "Fuentes: BCRD, IPC por artículos y tipo de cambio de referencia · Elaboración: Leonardo Mena") + theme_editorial()
export_plot(p_pollo, pollo_fig, "01_pollo_carnes_y_dolar", 11, 6.6)
pollo_index <- pollo_full
yoy <- pollo_index |> group_by(producto) |> arrange(fecha, .by_group = TRUE) |> mutate(inflacion_interanual = 100 * (indice / lag(indice, 12) - 1)) |> ungroup() |> filter(!is.na(inflacion_interanual)) |> distinct(fecha, producto, inflacion_interanual) |> left_join(fx |> select(fecha, compra), by = "fecha") |> group_by(producto) |> arrange(fecha, .by_group = TRUE) |> mutate(dolar_interanual = 100 * (compra / lag(compra, 12) - 1)) |> ungroup(); write_csv(yoy, file.path(pollo_dir, "data", "procesados", "02_inflacion_pollo_y_dolar.csv"))
p_yoy <- ggplot(yoy, aes(fecha)) + geom_hline(yintercept = 0, colour = pal$gris, linewidth = .5) + geom_line(aes(y = inflacion_interanual, colour = producto), linewidth = .9) + geom_line(aes(y = dolar_interanual, colour = "Dólar"), linewidth = .9, linetype = "dashed") + scale_colour_manual(values = c("Pollo fresco" = pal$terracota, "Pechuga de pollo" = pal$ocre, "Carnes" = pal$oliva, "Dólar" = pal$azul)) + scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") + scale_y_continuous(labels = label_percent(scale = 1), breaks = seq(-20, 80, 20)) + labs(title = "Variación interanual del pollo, carnes y tipo de cambio", subtitle = "Comparación de precios y tipo de cambio · variación interanual", x = NULL, y = "Variación interanual", caption = "Fuente: BCRD, IPC por artículos y mercado cambiario · Elaboración: Leonardo Mena") + theme_editorial()
export_plot(p_yoy, pollo_fig, "02_inflacion_interanual_pollo_dolar", 11, 6.6)
pollo_heat <- pollo_index |> filter(fecha >= as.Date("2020-10-01")) |> mutate(anio = as.integer(format(fecha, "%Y")), mes = as.integer(format(fecha, "%m"))) |> select(producto, anio, mes, indice_base)
write_csv(pollo_heat, file.path(pollo_dir, "data", "procesados", "03_mapa_calor_indice_pollo.csv"))
p_pollo_heat <- ggplot(pollo_heat, aes(mes, anio, fill = indice_base)) + geom_tile(colour = pal$crema, linewidth = .3) + facet_grid(producto ~ .) + scale_x_continuous(breaks = seq(1, 12, 2), labels = c("Ene", "Mar", "May", "Jul", "Sep", "Nov"), expand = c(0, 0)) + scale_y_continuous(breaks = sort(unique(pollo_heat$anio)), expand = c(0, 0)) + scale_fill_gradient2(low = pal$azul, mid = pal$crema, high = pal$terracota, midpoint = 100, labels = label_number(accuracy = 1), name = "Índice") + labs(title = "El índice del pollo por mes y año", subtitle = "Octubre de 2020 = 100 · lectura estacional de precios", x = NULL, y = NULL, caption = "Fuente: BCRD, IPC por artículos · Elaboración: Leonardo Mena") + theme_editorial("none")
export_plot(p_pollo_heat, pollo_fig, "03_mapa_calor_indice_pollo", 10.5, 8.5)
write_chart_map(pollo_dir, list(tibble(id = "01_pollo_carnes_y_dolar", pregunta = "¿Se comporta el pollo como el dólar?", familia = "Índice temporal", fuente = "BCRD", advertencia = "Índices de precios, no precios en RD$"), tibble(id = "02_inflacion_interanual_pollo_dolar", pregunta = "¿Qué shocks son propios del pollo?", familia = "Líneas interanuales", fuente = "BCRD", advertencia = "No identifica causalidad por sí solo")))

# 5. Camus, Sísifo y el emprendedor MIPYME
pollo_map <- read_csv(file.path(pollo_dir, "chart-map.csv"), show_col_types = FALSE) |> bind_rows(tibble(id = "03_mapa_calor_indice_pollo", pregunta = "¿Cómo se distribuye el índice del pollo por mes?", familia = "Mapa de calor", fuente = "BCRD", advertencia = "Índice de precios, no precio en RD$")); write_csv(pollo_map, file.path(pollo_dir, "chart-map.csv"))

mipyme_dir <- file.path(repo_root, "research", "camus-mipyme"); mipyme_fig <- file.path(mipyme_dir, "figuras"); dir.create(file.path(mipyme_dir, "data", "procesados"), recursive = TRUE, showWarnings = FALSE)
mipyme_path <- file.path(repo_root, "posts", "republica-en-un-grafico", "2026-02-14-mipymes-rd", "Encuesta-Nacional-a-las-MIPYMES-2023-Base-de-datos.xlsx"); mip <- read_excel(mipyme_path, sheet = 1, .name_repair = "unique")
pick <- function(pattern) names(mip)[which(str_detect(norm_text(names(mip)), norm_text(pattern)))[1]]
wcol <- pick("FACTOR"); formcol <- pick("FORMALIDAD"); sizecol <- pick("CLASIFICACIÓN MIPYMES"); inc_col <- pick("INGRESOS TOTALES"); emp_col <- pick("EMPLEO TOTAL"); contrib_col <- pick("CONTRIBUCIONES SOCIALES"); assert_true(all(!is.na(c(wcol, formcol, sizecol, inc_col, emp_col, contrib_col))), "La base MIPYME cambió columnas clave.")
mip2 <- mip |> mutate(peso = to_num(.data[[wcol]]), formalidad = as.character(.data[[formcol]]), tamano = as.character(.data[[sizecol]]), ingresos = to_num(.data[[inc_col]]), empleo = to_num(.data[[emp_col]]), contribuciones = to_num(.data[[contrib_col]]), ingreso_por_empleado = ingresos / pmax(empleo, 1)) |> filter(!is.na(peso), peso > 0)
wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
mipyme_productivity <- mip2 |> filter(is.finite(ingreso_por_empleado), ingreso_por_empleado > 0, ingreso_por_empleado < quantile(ingreso_por_empleado, .99, na.rm = TRUE)) |> group_by(formalidad) |> summarise(ingreso_por_empleado = wmean(ingreso_por_empleado, peso), .groups = "drop"); write_csv(mipyme_productivity, file.path(mipyme_dir, "data", "procesados", "01_ingreso_por_empleado_formalidad.csv"))
p_mip_prod <- ggplot(mipyme_productivity, aes(ingreso_por_empleado, reorder(formalidad, ingreso_por_empleado), fill = formalidad)) + geom_col(width = .58, show.legend = FALSE, colour = pal$tinta, linewidth = .25) + geom_text(aes(label = label_number(prefix = "RD$ ", big.mark = ".")(ingreso_por_empleado)), hjust = -.08, fontface = "bold", size = 3.3) + scale_fill_manual(values = c(Formal = pal$oliva, Informal = pal$terracota)) + scale_x_continuous(labels = label_number(prefix = "RD$ ", big.mark = "."), expand = expansion(mult = c(0, .12))) + labs(title = "Ingreso promedio por trabajador en MIPYMES formales e informales", subtitle = "Ingreso total promedio por trabajador · ENMIPYMES 2023 · estimación ponderada", x = NULL, y = NULL, caption = "Fuente: ONE/BCRD, ENMIPYMES 2023 · Elaboración: Leonardo Mena") + theme_editorial("x")
p_mip_prod <- p_mip_prod + scale_x_continuous(labels = label_number(prefix = "RD$ ", big.mark = "."), expand = expansion(mult = c(0, .25)))
export_plot(p_mip_prod, mipyme_fig, "01_ingreso_por_empleado_formalidad", 10.5, 5.3)

problem_cols <- names(mip)[str_detect(norm_text(names(mip)), "p5_3_")]; problem_data <- bind_rows(lapply(problem_cols, function(col) tibble(problema = str_trunc(str_remove(col, ".*: "), 42), tasa = 100 * wmean(as.numeric(mip2[[col]] %in% c(1, "1", "Sí", "Si", "SI")), mip2$peso)))) |> arrange(tasa) |> slice_max(order_by = tasa, n = 8)
write_csv(problem_data, file.path(mipyme_dir, "data", "procesados", "02_problemas_mipyme.csv")); p_mip_prob <- ggplot(problem_data, aes(tasa, reorder(problema, tasa))) + geom_col(fill = pal$terracota, width = .62, colour = pal$tinta, linewidth = .25) + geom_text(aes(label = sprintf("%.1f%%", tasa)), hjust = -.1, size = 3.2, fontface = "bold") + scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, max(problem_data$tasa) * 1.2)) + labs(title = "Problemas declarados por las MIPYMES", subtitle = "Porcentaje ponderado de empresas que declara cada problema", x = NULL, y = NULL, caption = "Fuente: ENMIPYMES 2023 · selección de problemas del cuestionario P5.3 · Elaboración: Leonardo Mena") + theme_editorial("x")
export_plot(p_mip_prob, mipyme_fig, "02_problemas_de_supervivencia_mipyme", 10, 6.2)

credit_col <- pick("durante el año 2022, ha recibido.*financiamiento"); credit <- mip2 |> mutate(tiene_credito = as.character(.data[[credit_col]])) |> group_by(formalidad, tamano) |> summarise(tasa = 100 * wmean(tiene_credito %in% c("1", "Sí", "Si", "SI"), peso), .groups = "drop"); write_csv(credit, file.path(mipyme_dir, "data", "procesados", "03_credito_formalidad_tamano.csv"))
p_mip_credit <- ggplot(credit, aes(tamano, tasa, fill = formalidad)) + geom_col(position = position_dodge(width = .78), width = .68, colour = pal$tinta, linewidth = .25) + geom_text(aes(label = sprintf("%.1f%%", tasa)), position = position_dodge(width = .78), vjust = -.45, size = 2.7, fontface = "bold") + scale_fill_manual(values = c(Formal = pal$oliva, Informal = pal$terracota)) + scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, max(credit$tasa, na.rm = TRUE) * 1.25)) + labs(title = "Acceso a crédito por tamaño y formalidad de la MIPYME", subtitle = "Porcentaje de empresas con acceso a crédito · ENMIPYMES 2023", x = NULL, y = "Porcentaje de empresas", caption = "Fuente: ENMIPYMES 2023 · estimación ponderada · Elaboración: Leonardo Mena") + theme_editorial()
export_plot(p_mip_credit, mipyme_fig, "03_credito_formalidad_tamano", 10, 6.2)
tesis_mip_path <- file.path(mipyme_dir, "data", "raw", "tesis", "enmipymes_r.csv")
tesis_mip <- read_csv(tesis_mip_path, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) |> mutate(peso = to_num(factor_exp), grupo = paste(clasif, formalidad, sep = " / ")) |> filter(is.finite(peso), peso > 0)
bar_cols <- names(tesis_mip)[str_detect(names(tesis_mip), "^bar_")]
tesis_bar <- tesis_mip |> select(peso, grupo, all_of(bar_cols)) |> pivot_longer(all_of(bar_cols), names_to = "problema", values_to = "respuesta") |> mutate(respuesta = as.integer(norm_text(respuesta) == "si"), problema = str_to_title(str_replace_all(str_remove(problema, "^bar_"), "_", " ")))
top_tesis_bar <- tesis_bar |> group_by(problema) |> summarise(tasa = wmean(respuesta, peso), .groups = "drop") |> slice_max(tasa, n = 8) |> pull(problema)
tesis_heat <- tesis_bar |> filter(problema %in% top_tesis_bar) |> group_by(problema, grupo) |> summarise(tasa = 100 * wmean(respuesta, peso), n_ponderado = sum(peso[!is.na(respuesta)], na.rm = TRUE), .groups = "drop") |> filter(n_ponderado > 0)
write_csv(tesis_heat, file.path(mipyme_dir, "data", "procesados", "04_mapa_calor_problemas_tamano_formalidad.csv"))
p_tesis_heat <- ggplot(tesis_heat, aes(grupo, problema, fill = tasa)) + geom_tile(colour = pal$crema, linewidth = .7) + geom_text(aes(label = sprintf("%.0f%%", tasa)), size = 2.8, fontface = "bold") + scale_fill_gradient(low = pal$crema, high = pal$terracota, limits = c(0, 100), labels = label_percent(scale = 1), name = "Porcentaje") + labs(title = "Problemas declarados por tamaño y formalidad", subtitle = "Porcentaje de empresas que declara cada problema · datos procesados de la tesis MIPYME", x = NULL, y = NULL, caption = "Fuente: ENMIPYMES 2022–2023, archivo reproducible de tesis · estimación ponderada") + theme_editorial("none") + theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 9))
export_plot(p_tesis_heat, mipyme_fig, "04_mapa_calor_problemas_tamano_formalidad", 11, 7.4)
province_mipyme <- tesis_mip |> mutate(provincia_key = norm_text(provincia), es_formal = norm_text(formalidad) == "formal", tiene_cuenta = norm_text(cuenta_bancaria) == "si") |> group_by(provincia_key) |> summarise(provincia = first(provincia), n_ponderado = sum(peso, na.rm = TRUE), formalidad_pct = 100 * wmean(es_formal, peso), cuenta_bancaria_pct = 100 * wmean(tiene_cuenta, peso), .groups = "drop") |> filter(n_ponderado > 0)
rd_prov_mipyme <- st_read(file.path(repo_root, "mapa_rd", "provincia", "PROVCenso2010.shp"), quiet = TRUE) |> mutate(provincia_key = norm_text(TOPONIMIA)) |> left_join(province_mipyme, by = "provincia_key")
assert_true(nrow(rd_prov_mipyme) == 32 && all(!is.na(rd_prov_mipyme$formalidad_pct)), "El mapa MIPYME dejo provincias sin observaciones ponderadas.")
write_csv(st_drop_geometry(rd_prov_mipyme) |> select(PROV, TOPONIMIA, provincia, n_ponderado, formalidad_pct, cuenta_bancaria_pct), file.path(mipyme_dir, "data", "procesados", "05_mapa_rd_formalidad_mipyme.csv"))
formal_extremes <- bind_rows(slice_max(rd_prov_mipyme, formalidad_pct, n = 2, with_ties = FALSE), slice_min(rd_prov_mipyme, formalidad_pct, n = 2, with_ties = FALSE)) |> distinct(PROV, .keep_all = TRUE) |> mutate(label_text = sprintf("%s\n%.1f%%", provincia, formalidad_pct))
p_mipyme_map <- ggplot(rd_prov_mipyme) + geom_sf(aes(fill = formalidad_pct), colour = pal$crema, linewidth = .42) +
  geom_sf_label(data = st_point_on_surface(formal_extremes), aes(label = label_text), size = 2.55, fontface = "bold", linewidth = .15, label.padding = unit(.12, "lines"), colour = pal$tinta, fill = pal$blanco) +
  scale_fill_gradient(low = pal$crema, high = pal$oliva, limits = c(0, 60), breaks = seq(0, 60, 10), labels = label_percent(scale = 1), name = "Formalidad") +
  labs(title = "Porcentaje de MIPYMES formales por provincia", subtitle = "Estimación ponderada · escala cromática 0–60%", caption = "Fuente: ENMIPYMES 2023 · archivo reproducible de tesis MIPYME · estimación ponderada por factor de expansión") +
  theme_editorial("none") + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "right")
export_plot(p_mipyme_map, mipyme_fig, "05_mapa_rd_formalidad_mipyme", 8.5, 7.3)
write_chart_map(mipyme_dir, list(tibble(id = "01_ingreso_por_empleado_formalidad", pregunta = "¿Cuál es el ingreso promedio por trabajador según la formalidad?", familia = "Barras comparables", fuente = "ENMIPYMES 2023", advertencia = "Promedios ponderados; no es causalidad"), tibble(id = "02_problemas_de_supervivencia_mipyme", pregunta = "¿Qué problemas declaran las MIPYMES?", familia = "Barras ordenadas", fuente = "ENMIPYMES 2023", advertencia = "Respuesta declarada"), tibble(id = "03_credito_formalidad_tamano", pregunta = "¿Cómo varía el acceso a crédito por tamaño y formalidad?", familia = "Barras agrupadas", fuente = "ENMIPYMES 2023", advertencia = "No es efecto causal")))

problem_data <- problem_data |> mutate(problema = case_when(str_detect(norm_text(problema), "servicios y combustibles") ~ "Servicios y combustibles", str_detect(norm_text(problema), "materias primas") ~ "Materias primas e insumos", str_detect(norm_text(problema), "inseguridad publica") ~ "Inseguridad pública", TRUE ~ problema)); p_mip_prob <- p_mip_prob %+% problem_data; write_csv(problem_data, file.path(mipyme_dir, "data", "procesados", "02_problemas_mipyme.csv")); export_plot(p_mip_prob, mipyme_fig, "02_problemas_de_supervivencia_mipyme", 10, 6.2)
mipyme_map <- read_csv(file.path(mipyme_dir, "chart-map.csv"), show_col_types = FALSE) |> bind_rows(tibble(id = "04_mapa_calor_problemas_tamano_formalidad", pregunta = "¿Qué problemas se concentran por grupo?", familia = "Mapa de calor", fuente = "Tesis MIPYME/ENMIPYMES", advertencia = "Respuestas declaradas; no es efecto causal"), tibble(id = "05_mapa_rd_formalidad_mipyme", pregunta = "¿Dónde se concentran las MIPYMES formales?", familia = "Mapa de RD", fuente = "ENMIPYMES/Tesis MIPYME", advertencia = "Proporción ponderada; no es causalidad")); write_csv(mipyme_map, file.path(mipyme_dir, "chart-map.csv"))
message("Gráficos generados en cinco módulos de research.")
