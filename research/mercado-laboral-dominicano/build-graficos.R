#!/usr/bin/env Rscript

options(encoding = "UTF-8", scipen = 999)

if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop(
    "R no esta leyendo UTF-8. Ejecute render-graficos.ps1 para limpiar el locale heredado de esta sesion.",
    call. = FALSE
  )
}

required_packages <- c(
  "dplyr", "ggplot2", "readr", "readxl", "scales", "stringr", "svglite", "tidyr"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Faltan paquetes R: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(readxl)
  library(scales)
  library(stringr)
  library(tidyr)
})

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_argument)) stop("No se pudo localizar el script.", call. = FALSE)
module_dir <- dirname(normalizePath(sub("^--file=", "", script_argument[[1]]), winslash = "/"))
repo_root <- normalizePath(file.path(module_dir, "..", ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "tema_graficos.R"), local = globalenv(), encoding = "UTF-8")

figure_dir <- file.path(module_dir, "figuras")
processed_dir <- file.path(module_dir, "data", "procesados")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

encft_dir <- file.path(repo_root, "atlas", "data", "raw", "bcrd-encft")
tss_jobs_path <- file.path(
  repo_root,
  "atlas", "data", "raw", "tss-empleos-cotizantes", "empleos-cotizantes-2003-2026.csv"
)
census_dir <- file.path(module_dir, "data", "censo_2022")

paths_required <- c(
  indicadores = file.path(encft_dir, "00_Indicadores.xlsx"),
  ramas = file.path(encft_dir, "1_1_Ocupados_Rama.xlsx"),
  sectores = file.path(encft_dir, "2_1_Sectores_Rama.xlsx"),
  informalidad = file.path(encft_dir, "Informalidad_total.xlsx"),
  tss = tss_jobs_path,
  censo_calificacion = file.path(census_dir, "calificacion_25_34_educacion.csv"),
  censo_ocupaciones = file.path(census_dir, "top_ocupaciones_superior_25_34.csv"),
  censo_cobertura = file.path(census_dir, "resumen_cobertura.csv"),
  censo_campos = file.path(census_dir, "campos_estudio_graduados_20_65.csv")
)

missing_files <- paths_required[!file.exists(paths_required)]
if (length(missing_files)) {
  stop("Faltan fuentes: ", paste(names(missing_files), collapse = ", "), call. = FALSE)
}

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

to_num <- function(value) {
  suppressWarnings(as.numeric(gsub(",", ".", as.character(value), fixed = TRUE)))
}

normalize_ascii <- function(value) {
  value <- iconv(as.character(value), from = "UTF-8", to = "ASCII//TRANSLIT")
  value <- tolower(trimws(value))
  gsub("[^a-z0-9]+", " ", value)
}

fill_down_year <- function(values) {
  output <- rep(NA_integer_, length(values))
  current <- NA_integer_
  for (index in seq_along(values)) {
    candidate <- suppressWarnings(as.integer(as.character(values[[index]])))
    if (!is.na(candidate) && candidate >= 1900 && candidate <= 2100) current <- candidate
    output[[index]] <- current
  }
  output
}

quarter_number <- function(value) {
  clean <- trimws(gsub("\\s+1/.*$", "", as.character(value)))
  unname(c("I" = 1L, "II" = 2L, "III" = 3L, "IV" = 4L)[clean])
}

quarter_label <- function(year, quarter) paste0(year, " T", quarter)

metric_labels <- c(
  "Total Global de Participación" = "Participación",
  "Tasa de Ocupación" = "Ocupación",
  "Ocupación Informal" = "Ocupación informal",
  "SU1: Tasa de Desocupación 4/" = "Desocupación abierta"
)

read_indicator_sheet <- function(path, sheet) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  labels <- trimws(as.character(raw[[1]]))
  header_row <- which(labels == "Indicador")
  assert_true(length(header_row) == 1, paste("No se ubico el encabezado de indicadores en", sheet))
  quarter_row <- header_row + 1L
  years <- fill_down_year(unlist(raw[header_row, ], use.names = FALSE))
  quarters <- vapply(unlist(raw[quarter_row, ], use.names = FALSE), quarter_number, integer(1))
  columns <- which(!is.na(years) & !is.na(quarters))

  rows <- match(names(metric_labels), labels)
  assert_true(!anyNA(rows), paste("Faltan indicadores esperados en", sheet))

  bind_rows(lapply(seq_along(rows), function(index) {
    values <- to_num(unlist(raw[rows[[index]], columns], use.names = FALSE))
    tibble(
      sexo = sheet,
      indicador_fuente = names(metric_labels)[[index]],
      indicador = unname(metric_labels[[index]]),
      anio = years[columns],
      trimestre = quarters[columns],
      periodo_indice = anio * 4L + trimestre,
      fecha = as.Date(sprintf("%04d-%02d-01", anio, (trimestre - 1L) * 3L + 1L)),
      periodo = quarter_label(anio, trimestre),
      valor = values
    )
  }))
}

clean_sector <- function(value) {
  value <- str_squish(as.character(value))
  value <- str_replace(value, "^Agrícultura", "Agricultura")
  value <- str_replace(value, "^Industrias 2/$", "Industrias")
  value <- str_replace(value, "Intermediación Financieras", "Intermediación financiera")
  value <- str_replace(value, "Administración Pública", "Administración pública")
  value <- str_replace(value, "Otros Servicios", "Otros servicios")
  value
}

read_branch_sheet <- function(path, sheet) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  candidate_rows <- which(!is.na(to_num(raw[[3]])))
  latest_row <- max(candidate_rows)
  sector_columns <- 4:ncol(raw)
  labels <- clean_sector(unlist(raw[9, sector_columns], use.names = FALSE))
  values <- to_num(unlist(raw[latest_row, sector_columns], use.names = FALSE))
  output <- tibble(sector = labels, ocupados = values) |>
    filter(!is.na(ocupados), !is.na(sector), nzchar(sector))
  attr(output, "total") <- to_num(raw[[3]][latest_row])
  attr(output, "periodo") <- paste(trimws(as.character(raw[[1]][latest_row])), trimws(as.character(raw[[2]][latest_row])))
  output
}

read_sector_informality <- function(path) {
  raw <- read_excel(path, sheet = "Rama de Actividad", col_names = FALSE, .name_repair = "minimal")
  latest_row <- max(which(!is.na(to_num(raw[[3]]))))
  formal_columns <- 4:15
  informal_columns <- 17:28

  formal <- tibble(
    sector = clean_sector(unlist(raw[9, formal_columns], use.names = FALSE)),
    formales = to_num(unlist(raw[latest_row, formal_columns], use.names = FALSE))
  )
  informal <- tibble(
    sector = clean_sector(unlist(raw[9, informal_columns], use.names = FALSE)),
    informales = to_num(unlist(raw[latest_row, informal_columns], use.names = FALSE))
  )
  output <- inner_join(formal, informal, by = "sector") |>
    mutate(
      ocupados = formales + informales,
      pct_informal = 100 * informales / ocupados
    )

  supplied_formal <- to_num(raw[[3]][latest_row])
  supplied_informal <- to_num(raw[[16]][latest_row])
  assert_true(abs(sum(output$formales) - supplied_formal) < 0.5, "Las ramas formales no suman al total ENCFT.")
  assert_true(abs(sum(output$informales) - supplied_informal) < 0.5, "Las ramas informales no suman al total ENCFT.")
  attr(output, "periodo") <- paste(trimws(as.character(raw[[1]][latest_row])), trimws(as.character(raw[[2]][latest_row])))
  output
}

read_latest_regions <- function(path) {
  raw <- read_excel(path, sheet = "Regiones", col_names = FALSE, .name_repair = "minimal")
  labels <- trimws(as.character(raw[[1]]))
  header_row <- tail(which(labels == "Indicador"), 1)
  region_names <- as.character(unlist(raw[header_row, 2:6], use.names = FALSE))
  row_su1 <- which(seq_len(nrow(raw)) > header_row & labels == "SU1: Tasa de Desocupación 1/")[[1]]
  row_su4 <- which(seq_len(nrow(raw)) > header_row & str_starts(labels, "SU4:"))[[1]]
  year_row <- max(which(seq_len(nrow(raw)) < header_row & grepl("^[0-9]{4}$", labels)))
  output <- tibble(
    region = str_replace(region_names, "^Región ", ""),
    desocupacion_abierta = to_num(unlist(raw[row_su1, 2:6], use.names = FALSE)),
    subutilizacion_amplia = to_num(unlist(raw[row_su4, 2:6], use.names = FALSE))
  )
  attr(output, "anio") <- labels[[year_row]]
  output
}

read_tss_jobs <- function(path) {
  output <- read_csv(path, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  assert_true(ncol(output) == 6, "El archivo TSS cambio de columnas.")
  names(output) <- c("mes", "anio", "privada", "publica_central", "publica_descentral", "total")
  month_map <- c(
    "enero" = 1L, "febrero" = 2L, "marzo" = 3L, "abril" = 4L,
    "mayo" = 5L, "junio" = 6L, "julio" = 7L, "agosto" = 8L,
    "septiembre" = 9L, "octubre" = 10L, "noviembre" = 11L, "diciembre" = 12L
  )
  output |>
    mutate(
      mes_numero = unname(month_map[normalize_ascii(mes)]),
      fecha = as.Date(sprintf("%04d-%02d-01", anio, mes_numero)),
      total_componentes = privada + publica_central + publica_descentral
    ) |>
    arrange(fecha)
}

export_plot <- function(plot, slug, width, height) {
  svg_path <- file.path(figure_dir, paste0(slug, ".svg"))
  png_path <- file.path(figure_dir, paste0(slug, ".png"))
  ggsave(svg_path, plot = plot, device = svglite::svglite, width = width, height = height, bg = pal$crema)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ggsave(png_path, plot = plot, device = ragg::agg_png, width = width, height = height, dpi = 320, bg = pal$crema)
  } else {
    ggsave(png_path, plot = plot, width = width, height = height, dpi = 320, bg = pal$crema)
  }
  invisible(c(svg_path, png_path))
}

data_blue <- "#4f789f"

# -----------------------------------------------------------------------------
# Datos y validaciones
# -----------------------------------------------------------------------------

national <- read_indicator_sheet(paths_required[["indicadores"]], "Indicadores")
male <- read_indicator_sheet(paths_required[["indicadores"]], "Masculino") |>
  mutate(sexo = "Hombres")
female <- read_indicator_sheet(paths_required[["indicadores"]], "Femenino") |>
  mutate(sexo = "Mujeres")
gender <- bind_rows(male, female)

assert_true(n_distinct(national$periodo_indice) == 47, "ENCFT nacional no tiene los 47 trimestres esperados.")
assert_true(all(diff(sort(unique(national$periodo_indice))) == 1), "ENCFT nacional tiene huecos trimestrales.")
assert_true(all(national$valor >= 0 & national$valor <= 100), "ENCFT contiene tasas fuera de 0-100.")
assert_true(setequal(male$periodo_indice, female$periodo_indice), "Las series por sexo no cubren los mismos trimestres.")

branch_total <- read_branch_sheet(paths_required[["ramas"]], "Total")
branch_male <- read_branch_sheet(paths_required[["ramas"]], "Masculina") |>
  rename(hombres = ocupados)
branch_female <- read_branch_sheet(paths_required[["ramas"]], "Femenina") |>
  rename(mujeres = ocupados)
branch_gender <- branch_total |>
  rename(total = ocupados) |>
  inner_join(branch_male, by = "sector") |>
  inner_join(branch_female, by = "sector") |>
  mutate(
    diferencia_reconciliacion = hombres + mujeres - total,
    pct_mujeres = 100 * mujeres / total
  )
assert_true(max(abs(branch_gender$diferencia_reconciliacion)) < 0.5, "Las ramas por sexo no reconcilian con el total.")

sector_informality <- read_sector_informality(paths_required[["sectores"]])
latest_national_informal <- national |>
  filter(indicador == "Ocupación informal") |>
  slice_max(periodo_indice, n = 1, with_ties = FALSE) |>
  pull(valor)

regions <- read_latest_regions(paths_required[["indicadores"]])
tss_jobs <- read_tss_jobs(paths_required[["tss"]])
assert_true(all(!is.na(tss_jobs$fecha)), "TSS contiene meses sin fecha interpretable.")
assert_true(max(abs(tss_jobs$total_componentes - tss_jobs$total)) == 0, "Los componentes TSS no suman al total.")
assert_true(all(diff(tss_jobs$fecha) > 0), "TSS tiene fechas duplicadas o desordenadas.")

census_coverage <- read_csv(paths_required[["censo_cobertura"]], show_col_types = FALSE)
census_qualification <- read_csv(paths_required[["censo_calificacion"]], show_col_types = FALSE)
census_occupations <- read_csv(paths_required[["censo_ocupaciones"]], show_col_types = FALSE)
census_fields <- read_csv(paths_required[["censo_campos"]], show_col_types = FALSE)

qualification_sums <- census_qualification |>
  group_by(sexo, educacion) |>
  summarise(total_pct = sum(pct_dentro_sexo_educacion), .groups = "drop")
assert_true(max(abs(qualification_sums$total_pct - 100)) <= 0.02, "Las composiciones ocupacionales del Censo no suman 100%.")
assert_true(!anyNA(census_occupations$ocupacion_desc), "Hay ocupaciones censales sin etiqueta.")

higher_ed_coverage <- with(
  census_coverage,
  100 * ocupados_25_34_educ_superior_con_ocupacion_desc / ocupados_25_34_educ_superior
)

# -----------------------------------------------------------------------------
# Figura 1: panorama trimestral
# -----------------------------------------------------------------------------

indicator_order <- c("Participación", "Ocupación", "Desocupación abierta", "Ocupación informal")
overview <- national |>
  mutate(indicador = factor(indicador, levels = indicator_order))
overview_latest <- overview |>
  group_by(indicador) |>
  slice_max(periodo_indice, n = 1, with_ties = FALSE) |>
  ungroup()

p1 <- ggplot(overview, aes(fecha, valor, group = indicador)) +
  annotate("rect", xmin = as.Date("2020-04-01"), xmax = as.Date("2020-12-31"), ymin = -Inf, ymax = Inf, fill = pal$gris_claro, alpha = 0.65) +
  geom_line(color = pal$terracota, linewidth = 1.05) +
  geom_point(data = overview_latest, color = pal$plomo, size = 2.5) +
  geom_text(
    data = overview_latest,
    aes(label = sprintf("%.1f%%", valor)),
    nudge_x = 150,
    hjust = 0,
    color = pal$plomo,
    fontface = "bold",
    size = 3.4
  ) +
  facet_wrap(~indicador, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.1))) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Principales indicadores del mercado laboral",
    subtitle = "Personas de 15 años y más · 2014 T3–2026 T1 · escalas verticales independientes",
    caption = "Fuente: BCRD, ENCFT · 2026 T1 preliminar · Elaboración: Leonardo Mena",
    x = NULL,
    y = NULL
  ) +
  theme_lm(grid = "y") +
  theme(panel.grid.major.x = element_blank(), plot.margin = margin(15, 48, 15, 15))
export_plot(p1, "01_panorama_indicadores_encft", 10, 6.8)

# -----------------------------------------------------------------------------
# Figura 2: brecha por sexo
# -----------------------------------------------------------------------------

gender_plot_data <- gender |>
  filter(indicador %in% c("Participación", "Ocupación")) |>
  mutate(
    indicador = factor(indicador, levels = c("Participación", "Ocupación")),
    sexo = factor(sexo, levels = c("Hombres", "Mujeres"))
  )
gender_latest <- gender_plot_data |>
  group_by(indicador, sexo) |>
  slice_max(periodo_indice, n = 1, with_ties = FALSE) |>
  ungroup()

p2 <- ggplot(gender_plot_data, aes(fecha, valor, color = sexo)) +
  geom_line(linewidth = 1.05) +
  geom_point(data = gender_latest, size = 2.5) +
  geom_text(
    data = gender_latest,
    aes(label = sprintf("%s  %.1f%%", sexo, valor)),
    nudge_x = 150,
    hjust = 0,
    fontface = "bold",
    size = 3.2,
    show.legend = FALSE
  ) +
  facet_wrap(~indicador, ncol = 1) +
  scale_color_manual(values = c("Hombres" = pal$plomo, "Mujeres" = pal$terracota), name = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.12))) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1), breaks = seq(40, 80, 10)) +
  coord_cartesian(ylim = c(38, 80), clip = "off") +
  labs(
    title = "Participación y ocupación por sexo",
    subtitle = "Personas de 15 años y más · 2014 T3–2026 T1 · misma escala en ambos paneles",
    caption = "Fuente: BCRD, ENCFT · 2026 T1 preliminar · Elaboración: Leonardo Mena",
    x = NULL,
    y = NULL
  ) +
  theme_lm(grid = "y") +
  theme(legend.position = "top", panel.grid.major.x = element_blank(), plot.margin = margin(15, 95, 15, 15))
export_plot(p2, "02_brecha_genero_participacion_ocupacion", 10, 7.4)

# -----------------------------------------------------------------------------
# Figura 3: participación femenina por rama
# -----------------------------------------------------------------------------

branch_gender <- branch_gender |>
  mutate(
    sector = factor(sector, levels = sector[order(pct_mujeres)]),
    posicion = if_else(pct_mujeres >= 50, "Mayoría de mujeres", "Menos de la mitad")
  )

p3 <- ggplot(branch_gender, aes(pct_mujeres, sector, fill = posicion)) +
  geom_col(width = 0.68, color = pal$plomo, linewidth = 0.25) +
  geom_vline(xintercept = 50, color = pal$plomo, linetype = "dashed", linewidth = 0.55) +
  geom_text(aes(label = sprintf("%.1f%%", pct_mujeres)), hjust = -0.15, color = pal$texto, fontface = "bold", size = 3.2) +
  scale_fill_manual(values = c("Mayoría de mujeres" = pal$terracota, "Menos de la mitad" = data_blue), name = NULL) +
  scale_x_continuous(labels = label_percent(scale = 1), breaks = seq(0, 100, 20), limits = c(0, 106)) +
  labs(
    title = "Participación de mujeres por rama de actividad",
    subtitle = "Mujeres como proporción de cada rama · ocupados de 15 años y más · 2026 T1",
    caption = "Fuente: BCRD, ENCFT · 2026 T1 preliminar · Elaboración: Leonardo Mena",
    x = NULL,
    y = NULL
  ) +
  theme_lm(grid = "x") +
  theme(panel.grid.major.y = element_blank(), legend.position = "top")
export_plot(p3, "03_participacion_mujeres_por_rama", 10, 7.2)

# -----------------------------------------------------------------------------
# Figura 4: informalidad por rama
# -----------------------------------------------------------------------------

sector_informality <- sector_informality |>
  mutate(sector = factor(sector, levels = sector[order(pct_informal)]))

p4 <- ggplot(sector_informality, aes(pct_informal, sector)) +
  geom_col(width = 0.68, fill = pal$terracota, color = pal$plomo, linewidth = 0.25) +
  geom_vline(xintercept = latest_national_informal, color = pal$plomo, linetype = "dashed", linewidth = 0.6) +
  annotate(
    "text",
    x = latest_national_informal,
    y = Inf,
    label = sprintf("Promedio nacional %.1f%%", latest_national_informal),
    hjust = -0.08,
    vjust = 1.3,
    color = pal$plomo,
    fontface = "bold",
    size = 3.1
  ) +
  geom_text(aes(label = sprintf("%.1f%%", pct_informal)), hjust = -0.15, color = pal$texto, fontface = "bold", size = 3.2) +
  scale_x_continuous(labels = label_percent(scale = 1), breaks = seq(0, 100, 20), limits = c(0, 106)) +
  labs(
    title = "Ocupación informal por rama de actividad",
    subtitle = "Personas ocupadas de 15 años y más · 2026 T1 · ramas excluyen servicio doméstico",
    caption = "Fuente: BCRD, ENCFT · promedio nacional incluye servicio doméstico · 2026 T1 preliminar",
    x = NULL,
    y = NULL
  ) +
  theme_lm(grid = "x") +
  theme(panel.grid.major.y = element_blank())
export_plot(p4, "04_informalidad_por_rama", 10, 7.2)

# -----------------------------------------------------------------------------
# Figura 5: subutilización regional
# -----------------------------------------------------------------------------

regions <- regions |>
  mutate(region = factor(region, levels = region[order(subutilizacion_amplia)]))

p5 <- ggplot(regions, aes(y = region)) +
  geom_segment(
    aes(x = desocupacion_abierta, xend = subutilizacion_amplia, yend = region),
    color = pal$border_dark,
    linewidth = 1.4
  ) +
  geom_point(aes(x = desocupacion_abierta, color = "Desocupación abierta (SU1)"), size = 3.4) +
  geom_point(aes(x = subutilizacion_amplia, color = "Subutilización amplia (SU4)"), size = 3.4) +
  geom_text(aes(x = desocupacion_abierta, label = sprintf("%.1f", desocupacion_abierta)), nudge_x = -0.35, hjust = 1, size = 3.1, color = pal$plomo) +
  geom_text(aes(x = subutilizacion_amplia, label = sprintf("%.1f", subutilizacion_amplia)), nudge_x = 0.35, hjust = 0, size = 3.1, color = pal$terracota) +
  scale_color_manual(
    values = c("Desocupación abierta (SU1)" = pal$plomo, "Subutilización amplia (SU4)" = pal$terracota),
    name = NULL
  ) +
  scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, 16), breaks = seq(0, 16, 4)) +
  labs(
    title = "Desocupación y subutilización laboral por macroregión",
    subtitle = "SU4 añade subocupación por horas y fuerza de trabajo potencial · promedio 2025",
    caption = "Fuente: BCRD, ENCFT · Elaboración: Leonardo Mena",
    x = NULL,
    y = NULL
  ) +
  theme_lm(grid = "x") +
  theme(panel.grid.major.y = element_blank(), legend.position = "top")
export_plot(p5, "05_subutilizacion_por_macroregion", 10, 5.8)

# -----------------------------------------------------------------------------
# Figura 6: empleo formal cotizante
# -----------------------------------------------------------------------------

tss_latest <- slice_tail(tss_jobs, n = 1)
p6 <- ggplot(tss_jobs, aes(fecha, total / 1e6)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-12-31"), ymin = -Inf, ymax = Inf, fill = pal$gris_claro, alpha = 0.8) +
  geom_line(color = pal$terracota, linewidth = 1.05) +
  geom_point(data = tss_latest, color = pal$plomo, size = 2.8) +
  geom_text(
    data = tss_latest,
    aes(label = sprintf("%.2f millones", total / 1e6)),
    nudge_x = -60,
    nudge_y = 0.14,
    hjust = 1,
    color = pal$plomo,
    fontface = "bold",
    size = 3.3
  ) +
  annotate("text", x = as.Date("2020-07-01"), y = 0.28, label = "Choque de 2020", color = pal$texto_muted, size = 3.1) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_continuous(labels = label_number(suffix = " M", accuracy = 0.1), limits = c(0, NA), breaks = seq(0, 3, 0.5)) +
  labs(
    title = "Empleos cotizantes en la seguridad social",
    subtitle = "Registros mensuales del régimen contributivo · junio de 2003–abril de 2026",
    caption = "Fuente: Tesorería de la Seguridad Social (TSS) · Elaboración: Leonardo Mena",
    x = NULL,
    y = NULL
  ) +
  theme_lm(grid = "y") +
  theme(panel.grid.major.x = element_blank())
export_plot(p6, "06_empleos_cotizantes_tss", 10, 5.8)

# -----------------------------------------------------------------------------
# Figura 7: calificación ocupacional por educación
# -----------------------------------------------------------------------------

qualification_order <- c("Ocupaciones elementales", "Media calificacion", "Alta calificacion")
census_qualification_plot <- census_qualification |>
  mutate(
    calificacion = recode(
      grupo_calificacion_ocupacion,
      "Media calificacion" = "Calificación media",
      "Alta calificacion" = "Alta calificación",
      "Ocupaciones elementales" = "Ocupaciones elementales"
    ),
    calificacion = factor(
      calificacion,
      levels = c("Ocupaciones elementales", "Calificación media", "Alta calificación")
    ),
    grupo = paste0(if_else(sexo == "Hombre", "Hombres", "Mujeres"), " · ", educacion),
    grupo = factor(
      grupo,
      levels = c("Hombres · No superior", "Hombres · Superior", "Mujeres · No superior", "Mujeres · Superior")
    ),
    etiqueta = sprintf("%.1f%%", pct_dentro_sexo_educacion)
  )

p7 <- ggplot(census_qualification_plot, aes(pct_dentro_sexo_educacion, grupo, fill = calificacion)) +
  geom_col(width = 0.7, color = pal$crema, linewidth = 0.7, position = position_stack(reverse = TRUE)) +
  geom_text(
    aes(label = etiqueta),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    color = pal$texto,
    fontface = "bold",
    size = 3.1
  ) +
  scale_fill_manual(
    values = c(
      "Ocupaciones elementales" = pal$gris_claro,
      "Calificación media" = pal$oliva,
      "Alta calificación" = pal$terracota
    ),
    name = NULL
  ) +
  scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)) +
  labs(
    title = "Calificación de la ocupación por educación y sexo",
    subtitle = sprintf("Ocupados de 25–34 años con código CNO clasificado · Censo 2022 · cobertura superior %.1f%%", higher_ed_coverage),
    caption = "Fuente: ONE, X Censo Nacional 2022 · enlace CNO 2019 · porcentajes condicionados a ocupación clasificada",
    x = NULL,
    y = NULL
  ) +
  theme_lm(grid = "x") +
  theme(panel.grid.major.y = element_blank(), legend.position = "top")
export_plot(p7, "07_calificacion_ocupacional_educacion_sexo", 10, 5.8)

# -----------------------------------------------------------------------------
# Figura 8: ocupaciones de jóvenes con educación superior
# -----------------------------------------------------------------------------

top_occupations <- census_occupations |>
  group_by(sexo) |>
  arrange(desc(personas), .by_group = TRUE) |>
  slice_head(n = 6) |>
  ungroup() |>
  mutate(
    sexo = recode(sexo, "Hombre" = "Hombres", "Mujer" = "Mujeres"),
    ocupacion_corta = str_wrap(ocupacion_desc, width = 36)
  ) |>
  arrange(sexo, personas) |>
  mutate(
    ocupacion_panel = paste(sexo, ocupacion_corta, sep = "|"),
    ocupacion_panel = factor(ocupacion_panel, levels = unique(ocupacion_panel))
  )

p8 <- ggplot(top_occupations, aes(personas, ocupacion_panel)) +
  geom_col(width = 0.68, fill = data_blue, color = pal$plomo, linewidth = 0.25) +
  geom_text(aes(label = label_comma()(personas)), hjust = -0.12, color = pal$texto, fontface = "bold", size = 3) +
  facet_wrap(~sexo, scales = "free_y", ncol = 2) +
  scale_y_discrete(labels = function(value) sub("^[^|]+\\|", "", value)) +
  scale_x_continuous(labels = label_number(scale = 1 / 1000, suffix = " mil", accuracy = 1), expand = expansion(mult = c(0, 0.16))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Ocupaciones más frecuentes entre jóvenes con educación superior",
    subtitle = "Seis primeras por sexo · ocupados de 25–34 años con ocupación clasificada · Censo 2022",
    caption = "Fuente: ONE, X Censo Nacional 2022 · enlace CNO 2019 · conteos censales",
    x = NULL,
    y = NULL
  ) +
  theme_lm(grid = "x") +
  theme(panel.grid.major.y = element_blank(), plot.margin = margin(15, 35, 15, 15))
export_plot(p8, "08_top_ocupaciones_superior_25_34", 12, 7.6)

# -----------------------------------------------------------------------------
# Figura 9: campos de estudio (opcional)
# -----------------------------------------------------------------------------

fields_plot <- census_fields |>
  select(carrera, sexo, pct_sobre_validos) |>
  pivot_wider(names_from = sexo, values_from = pct_sobre_validos) |>
  mutate(
    carrera = recode(
      carrera,
      "Educacion/Docencia" = "Educación/Docencia",
      "Psicologia/Social" = "Psicología/Social",
      "Enfermeria/Salud" = "Enfermería/Salud",
      "Administracion" = "Administración",
      "Ciencias Agricolas" = "Ciencias agrícolas",
      "Informatica/TIC" = "Informática/TIC",
      "Ingenieria" = "Ingeniería"
    ),
    brecha = Mujer - Hombre,
    carrera = factor(carrera, levels = carrera[order(brecha)])
  )

fields_labels <- fields_plot |>
  select(carrera, Hombres = Hombre, Mujeres = Mujer) |>
  pivot_longer(c(Hombres, Mujeres), names_to = "sexo", values_to = "valor") |>
  group_by(carrera) |>
  mutate(
    es_menor = valor == min(valor),
    etiqueta_x = valor + if_else(es_menor, -0.45, 0.45),
    etiqueta_hjust = if_else(es_menor, 1, 0)
  ) |>
  ungroup()

p9 <- ggplot(fields_plot, aes(y = carrera)) +
  geom_segment(aes(x = Hombre, xend = Mujer, yend = carrera), color = pal$border_dark, linewidth = 1.25) +
  geom_point(aes(x = Hombre, color = "Hombres"), size = 3.3) +
  geom_point(aes(x = Mujer, color = "Mujeres"), size = 3.3) +
  geom_text(
    data = fields_labels,
    aes(x = etiqueta_x, y = carrera, label = sprintf("%.1f", valor), color = sexo, hjust = etiqueta_hjust),
    inherit.aes = FALSE,
    size = 2.9,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Hombres" = pal$plomo, "Mujeres" = pal$terracota), name = NULL) +
  scale_x_continuous(labels = label_percent(scale = 1), breaks = seq(0, 30, 5)) +
  coord_cartesian(xlim = c(0, 31), clip = "off") +
  labs(
    title = "Campos de estudio de la población graduada por sexo",
    subtitle = "Participación dentro de todos los campos válidos · personas graduadas de 20–65 años · Censo 2022",
    caption = "Fuente: ONE, X Censo Nacional 2022 · campos homologados a ISCED-F 2013 · no normalizado al top mostrado",
    x = NULL,
    y = NULL
  ) +
  theme_lm(grid = "x") +
  theme(panel.grid.major.y = element_blank(), legend.position = "top", plot.margin = margin(15, 15, 15, 22))
export_plot(p9, "09_campos_estudio_por_sexo_opcional", 10.5, 7.2)

# -----------------------------------------------------------------------------
# Evidencia procesada y QA
# -----------------------------------------------------------------------------

write_csv(national, file.path(processed_dir, "01_indicadores_encft_nacional.csv"))
write_csv(gender, file.path(processed_dir, "02_indicadores_encft_sexo.csv"))
write_csv(branch_gender, file.path(processed_dir, "03_ramas_ocupacion_sexo.csv"))
write_csv(sector_informality, file.path(processed_dir, "04_informalidad_rama.csv"))
write_csv(regions, file.path(processed_dir, "05_subutilizacion_macroregion.csv"))
write_csv(tss_jobs, file.path(processed_dir, "06_empleos_cotizantes_tss.csv"))
write_csv(census_qualification_plot, file.path(processed_dir, "07_calificacion_ocupacional.csv"))
write_csv(top_occupations, file.path(processed_dir, "08_top_ocupaciones.csv"))
write_csv(fields_plot, file.path(processed_dir, "09_campos_estudio.csv"))

qa <- tibble(
  chequeo = c(
    "UTF-8 de R",
    "ENCFT nacional: 47 trimestres consecutivos",
    "ENCFT por sexo: mismos periodos",
    "Ramas por sexo reconcilian con total",
    "Ramas formal/informal reconcilian con totales",
    "TSS: total igual a suma de componentes",
    "Censo: composiciones suman 100%",
    "Censo: ocupaciones con etiquetas",
    "Cobertura CNO entre ocupados 25-34 con superior"
  ),
  estado = "OK",
  detalle = c(
    Sys.getlocale(),
    paste(min(national$periodo), "a", max(national$periodo)),
    paste(n_distinct(gender$periodo), "periodos por sexo"),
    sprintf("diferencia maxima %.3f personas", max(abs(branch_gender$diferencia_reconciliacion))),
    "diferencia menor de 0.5 personas por redondeo",
    paste(nrow(tss_jobs), "meses; diferencia maxima 0"),
    paste(nrow(qualification_sums), "grupos validados"),
    paste(nrow(census_occupations), "filas etiquetadas"),
    sprintf("%.2f%%", higher_ed_coverage)
  )
)
write_csv(qa, file.path(module_dir, "qa-validacion.csv"))

sources <- tibble(
  fuente = c("BCRD ENCFT", "TSS", "ONE Censo 2022"),
  corte_datos = c("2026 T1 preliminar", "abril de 2026", "levantamiento censal 2022"),
  fecha_tecnica_extracto = c(
    as.character(file.info(paths_required[["indicadores"]])$mtime),
    as.character(file.info(paths_required[["tss"]])$mtime),
    "2026-06-20"
  ),
  uso = c(
    "indicadores, sexo, rama, informalidad y macroregión",
    "empleos cotizantes mensuales",
    "educación, calificación ocupacional, ocupaciones y campos de estudio"
  ),
  advertencia = c(
    "2026 T1 es preliminar",
    "mide registros cotizantes, no toda la ocupación",
    sprintf("gráficos ocupacionales condicionados a código CNO válido; cobertura superior %.1f%%", higher_ed_coverage)
  )
)
write_csv(sources, file.path(module_dir, "fuentes-y-cortes.csv"))

message("Graficos generados en: ", figure_dir)
message("Validacion: ", file.path(module_dir, "qa-validacion.csv"))
