options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readxl)
  library(stringr)
  library(tidyr)
  library(patchwork)
})

if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop("R no está leyendo UTF-8; se detiene para no generar texto roto.")
}

script_arg <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", script_arg[grep("^--file=", script_arg)[1]]), winslash = "/")
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/")
source(file.path(repo_root, "tema_graficos.R"), encoding = "UTF-8")

raw_dir <- file.path(repo_root, "atlas", "data", "raw", "bcrd-precios")
out_dir <- file.path(repo_root, "research", "canasta-familiar")
fig_dir <- file.path(out_dir, "figuras")
data_dir <- file.path(out_dir, "data", "procesados")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

month_lookup <- c(
  Enero = 1, Febrero = 2, Marzo = 3, Abril = 4, Mayo = 5, Junio = 6,
  Julio = 7, Agosto = 8, Septiembre = 9, Octubre = 10, Noviembre = 11,
  Diciembre = 12
)

clean_label <- function(x) str_squish(str_replace_all(as.character(x), "[\r\n]+", " "))

period_rows <- function(x, year_col, month_col, start_row) {
  current_year <- NA_integer_
  rows <- vector("list", nrow(x))
  for (i in seq.int(start_row, nrow(x))) {
    year_value <- suppressWarnings(as.integer(as.character(x[[year_col]][i])))
    if (!is.na(year_value) && year_value >= 1990 && year_value <= 2030) {
      current_year <- year_value
    }
    month_label <- clean_label(x[[month_col]][i])
    month_number <- unname(month_lookup[month_label])
    if (!is.na(current_year) && length(month_number) == 1 && !is.na(month_number)) {
      rows[[i]] <- tibble(
        row_id = i,
        year = current_year,
        month = as.integer(month_number),
        period = as.Date(sprintf("%04d-%02d-01", current_year, month_number))
      )
    }
  }
  bind_rows(rows)
}

read_cost <- function(path, label_row, first_row, year_col = 1, month_col = 2, value_cols) {
  x <- read_excel(path, col_names = FALSE)
  labels <- clean_label(unlist(x[label_row, value_cols], use.names = FALSE))
  periods <- period_rows(x, year_col, month_col, first_row)
  out <- lapply(seq_along(value_cols), function(k) {
    tibble(
      row_id = periods$row_id,
      period = periods$period,
      year = periods$year,
      month = periods$month,
      series = labels[[k]],
      value = suppressWarnings(as.numeric(x[[value_cols[[k]]]][periods$row_id]))
    )
  })
  bind_rows(out) |>
    filter(!is.na(value)) |>
    arrange(period, series)
}

cost_quintiles <- read_cost(
  file.path(raw_dir, "Costo_Canasta_quintiles_base_2019-2020.xlsx"),
  label_row = 3, first_row = 5, value_cols = 3:8
)
cost_regions <- read_cost(
  file.path(raw_dir, "Costo_Canasta_regiones_base_2019-2020.xls"),
  label_row = 3, first_row = 4, value_cols = 3:6
)

quintile_levels <- c("Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5", "Nacional")
region_levels <- c("Región Ozama*", "Región Norte o Cibao", "Región Este", "Región Sur")
cost_quintiles$series <- factor(cost_quintiles$series, levels = quintile_levels)
cost_regions$series <- factor(cost_regions$series, levels = region_levels)

stopifnot(nrow(cost_quintiles) >= 200, nrow(cost_regions) >= 100)
stopifnot(all(cost_quintiles$series != "NA"), all(cost_regions$series != "NA"))

read_group_inflation <- function(path) {
  x <- read_excel(path, col_names = FALSE)
  periods <- period_rows(x, year_col = 1, month_col = 1, start_row = 7)
  index_cols <- seq(2, 24, by = 2)
  inflation_cols <- index_cols + 1
  out <- lapply(seq_along(index_cols), function(k) {
    tibble(
      row_id = periods$row_id,
      period = periods$period,
      year = periods$year,
      month = periods$month,
      group = clean_label(x[[index_cols[[k]]]][3]),
      inflation = suppressWarnings(as.numeric(x[[inflation_cols[[k]]]][periods$row_id]))
    )
  })
  bind_rows(out) |>
    filter(!is.na(inflation), !is.na(group), group != "NA") |>
    arrange(period, group)
}

group_inflation <- read_group_inflation(file.path(raw_dir, "ipc_grupos_base_2019-2020.xls"))
latest_period <- max(group_inflation$period, na.rm = TRUE)
latest_groups <- group_inflation |>
  filter(period == latest_period) |>
  arrange(inflation) |>
  mutate(group = factor(group, levels = group))
stopifnot(nrow(latest_groups) >= 10)

month_name <- names(month_lookup)[match(as.integer(format(latest_period, "%m")), month_lookup)]
latest_label <- paste(month_name, format(latest_period, "%Y"))

write.csv(cost_quintiles, file.path(data_dir, "costo_canasta_quintiles.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(cost_regions, file.path(data_dir, "costo_canasta_regiones.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(latest_groups, file.path(data_dir, "inflacion_grupos_ultimo_mes.csv"), row.names = FALSE, fileEncoding = "UTF-8")

series_quintile <- c(
  "Quintil 1" = pal$oliva,
  "Quintil 2" = pal$ocre,
  "Quintil 3" = pal$terracota,
  "Quintil 4" = pal$plomo,
  "Quintil 5" = "#8e4b35",
  "Nacional" = pal$texto
)
series_region <- c(
  "Región Ozama*" = pal$terracota,
  "Región Norte o Cibao" = pal$oliva,
  "Región Este" = pal$ocre,
  "Región Sur" = pal$plomo
)

plot_save <- function(plot, name, width = 11, height = 6.6) {
  ggsave(file.path(fig_dir, paste0(name, ".svg")), plot, width = width, height = height, device = "svg", bg = pal$crema)
  ggsave(file.path(fig_dir, paste0(name, ".png")), plot, width = width, height = height, dpi = 180, bg = pal$crema)
}

latest_quintiles <- cost_quintiles |>
  filter(period == max(period))
puntos_anuales <- seq(as.Date("2020-01-01"), as.Date("2026-01-01"), by = "1 year")
p_quintiles <- ggplot(cost_quintiles, aes(period, value, color = series, group = series)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = latest_quintiles, size = 2.2) +
  geom_text(
    data = latest_quintiles,
    aes(label = paste0(series, " · RD$ ", format(round(value), big.mark = ",", trim = TRUE))),
    hjust = 0, nudge_x = 60, size = 3.2, family = "sans", show.legend = FALSE
  ) +
  scale_color_manual(values = series_quintile) +
  scale_x_date(breaks = puntos_anuales, date_labels = "%Y", limits = c(min(cost_quintiles$period), max(cost_quintiles$period) + 180), expand = c(0.01, 0)) +
  scale_y_continuous(labels = function(x) paste0("RD$ ", format(round(x / 1000), big.mark = ","), " mil"), expand = expansion(mult = c(0.03, 0.1))) +
  labs(
    title = "Costo mensual de la canasta familiar por quintil",
    subtitle = "República Dominicana · octubre de 2020 a junio de 2026 · pesos corrientes",
    x = NULL, y = NULL, color = NULL,
    caption = "Fuente: BCRD, Costo de la Canasta Familiar por Quintiles de Ingresos · base octubre 2019–septiembre 2020"
  ) +
  coord_cartesian(clip = "off") +
  theme_lm() +
  theme(legend.position = "top", plot.margin = margin(15, 200, 15, 15))
plot_save(p_quintiles, "01_costo_canasta_quintiles_2020_2026")

latest_regions <- cost_regions |>
  filter(period == max(period)) |>
  mutate(series_display = recode(as.character(series),
    "Región Ozama*" = "Ozama*", "Región Norte o Cibao" = "Norte/Cibao",
    "Región Este" = "Este", "Región Sur" = "Sur"
  ))
p_regions <- ggplot(cost_regions, aes(period, value, color = series, group = series)) +
  geom_line(linewidth = 1) +
  geom_point(data = latest_regions, size = 2.3) +
  geom_text(
    data = latest_regions,
    aes(label = paste0(series_display, " · RD$ ", format(round(value), big.mark = ",", trim = TRUE))),
    hjust = 0, nudge_x = 60, size = 3.2, family = "sans", show.legend = FALSE
  ) +
  scale_color_manual(values = series_region) +
  scale_x_date(breaks = puntos_anuales, date_labels = "%Y", limits = c(min(cost_regions$period), max(cost_regions$period) + 180), expand = c(0.01, 0)) +
  scale_y_continuous(labels = function(x) paste0("RD$ ", format(round(x / 1000), big.mark = ","), " mil"), expand = expansion(mult = c(0.03, 0.1))) +
  labs(
    title = "Costo mensual de la canasta familiar por región",
    subtitle = "República Dominicana · octubre de 2020 a junio de 2026 · pesos corrientes",
    x = NULL, y = NULL, color = NULL,
    caption = "Fuente: BCRD, Costo de la Canasta Familiar por Regiones Geográficas · *Ozama comprende Distrito Nacional y Santo Domingo"
  ) +
  coord_cartesian(clip = "off") +
  theme_lm() +
  theme(legend.position = "top", plot.margin = margin(15, 210, 15, 15))
plot_save(p_regions, "02_costo_canasta_regiones_2020_2026")

latest_groups$label <- ifelse(abs(latest_groups$inflation) < 0.005, "0.00", sprintf("%.2f", latest_groups$inflation))
p_groups <- ggplot(latest_groups, aes(inflation, group)) +
  geom_vline(xintercept = 0, color = pal$border_dark, linewidth = 0.5) +
  geom_segment(aes(x = 0, xend = inflation, y = group, yend = group), color = pal$terracota, linewidth = 5, lineend = "butt") +
  geom_point(aes(fill = inflation >= 0), shape = 21, color = pal$texto, size = 3.2, stroke = 0.35) +
  geom_text(
    aes(x = ifelse(inflation >= 0, inflation + 0.05, inflation - 0.05), label = label, hjust = ifelse(inflation >= 0, 0, 1)),
    size = 3.5, color = pal$texto, family = "sans"
  ) +
  scale_fill_manual(values = c(`TRUE` = pal$terracota, `FALSE` = pal$oliva), guide = "none") +
  scale_x_continuous(labels = function(x) paste0(format(round(x, 1), nsmall = 1), "%"), expand = expansion(mult = c(0.08, 0.18))) +
  labs(
    title = "Variación mensual del IPC por grupo",
    subtitle = paste0(latest_label, " · porcentajes del índice nacional; cada hogar pondera estos movimientos de forma distinta"),
    x = "Variación mensual", y = NULL,
    caption = "Fuente: BCRD, IPC nacional por grupos de bienes y servicios · base octubre 2019–septiembre 2020"
  ) +
  theme_lm(grid = "x") +
  theme(legend.position = "none", plot.margin = margin(15, 25, 15, 25))
plot_save(p_groups, "03_inflacion_grupos_ultimo_mes", width = 11, height = 8)

example_prices <- c(Arroz = 10, Gasolina = -5, Cable = 2)
example_weights <- tibble(
  hogar = c("Pedro y María", "Pedro y María", "Pedro y María", "Vecino sin carro", "Vecino sin carro", "Vecino sin carro"),
  producto = rep(names(example_prices), 2),
  peso = c(50, 30, 20, 60, 0, 40)
) |>
  mutate(
    cambio_precio = unname(example_prices[producto]),
    contribucion = peso / 100 * cambio_precio,
    producto = factor(producto, levels = names(example_prices)),
    hogar = factor(hogar, levels = c("Pedro y María", "Vecino sin carro"))
  )
example_totals <- example_weights |>
  group_by(hogar) |>
  summarise(inflacion = sum(contribucion), .groups = "drop")
write.csv(example_weights, file.path(data_dir, "inflacion_personal_ejemplo.csv"), row.names = FALSE, fileEncoding = "UTF-8")

p_weights <- ggplot(example_weights, aes(hogar, peso, fill = producto)) +
  geom_col(width = 0.62, color = pal$crema, linewidth = 0.3) +
  geom_text(aes(label = ifelse(peso >= 10, paste0(peso, "%"), "")), position = position_stack(vjust = 0.5), size = 3.4, color = pal$texto) +
  scale_fill_manual(values = c(Arroz = pal$terracota, Gasolina = pal$oliva, Cable = pal$ocre)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100), expand = c(0, 0)) +
  labs(title = "La misma variación de precios no afecta igual", subtitle = "Ponderación ilustrativa del gasto mensual", x = NULL, y = NULL, fill = NULL) +
  theme_lm(grid = "y") +
  theme(legend.position = "top")

p_totals <- ggplot(example_totals, aes(inflacion, hogar)) +
  geom_vline(xintercept = 0, color = pal$border_dark, linewidth = 0.5) +
  geom_col(width = 0.55, fill = pal$plomo) +
  geom_text(aes(label = paste0(format(round(inflacion, 1), nsmall = 1), "%"), x = inflacion + 0.18), hjust = 0, size = 4.3, fontface = "bold", color = pal$texto) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Inflación personal resultante", subtitle = "Ejemplo didáctico: mismos precios, distinta canasta", x = NULL, y = NULL) +
  theme_lm(grid = "x") +
  theme(legend.position = "none")

p_personal <- p_weights / p_totals + plot_layout(heights = c(1.15, 0.85)) +
  plot_annotation(caption = "Ejemplo didáctico del artículo: no es una estimación del IPC oficial. La inflación personal se obtiene como suma de ponderaciones por variaciones de precios.") &
  theme(plot.caption = element_text(size = 9, color = pal$texto_muted, hjust = 1, margin = margin(t = 10)))
plot_save(p_personal, "04_inflacion_personal_ejemplo", width = 10.5, height = 8)

writeLines(
  c(
    "id,pregunta,familia,fuente,advertencia",
    '01_costo_canasta_quintiles_2020_2026,¿La canasta cuesta lo mismo para todos los ingresos?,Serie temporal por quintil,BCRD/ENIGH,"Costo nominal; no es ingreso disponible"',
    '02_costo_canasta_regiones_2020_2026,¿La canasta cuesta lo mismo en todas las regiones?,Serie temporal regional,BCRD/ENIGH,"Ozama comprende Distrito Nacional y Santo Domingo"',
    '03_inflacion_grupos_ultimo_mes,¿Qué grupos movieron el IPC en el último mes?,Dumbbell de variaciones,BCRD IPC,"La contribución al IPC depende también de ponderaciones"',
    '04_inflacion_personal_ejemplo,¿Por qué dos hogares sienten distinta inflación?,Composición y comparación,Ejemplo didáctico,"No es una estimación oficial"'
  ),
  file.path(out_dir, "chart-map.csv"), useBytes = TRUE
)

cat("Generados 4 visuales de canasta familiar para", latest_label, "\\n")
