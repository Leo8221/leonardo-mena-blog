#!/usr/bin/env Rscript

options(encoding = "UTF-8", scipen = 999)
.libPaths(unique(c(Sys.getenv("R_LIBS_USER"), .libPaths())))
if (!isTRUE(l10n_info()[["UTF-8"]])) stop("R no esta leyendo UTF-8.", call. = FALSE)
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(readxl)
  library(scales)
  library(stringr)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]
script_path <- sub("^--file=", "", script_arg)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/")
setwd(repo_root)

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
      panel.grid.minor = element_blank(),
      axis.title = element_text(colour = pal$tinta), axis.text = element_text(colour = pal$tinta),
      plot.title = element_text(face = "bold", size = 17, colour = pal$tinta),
      plot.subtitle = element_text(colour = "#5A5A55", size = 11),
      plot.caption = element_text(colour = "#65655F", size = 8.5, hjust = 0),
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
  unname(c(enero = 1, ene = 1, febrero = 2, feb = 2, marzo = 3, mar = 3, abril = 4, abr = 4,
           mayo = 5, may = 5, junio = 6, jun = 6, julio = 7, jul = 7, agosto = 8, ago = 8,
           septiembre = 9, sep = 9, sept = 9, octubre = 10, oct = 10, noviembre = 11, nov = 11,
           diciembre = 12, dic = 12)[key])
}
make_date <- function(years, months) {
  out <- rep(as.Date(NA), length(years)); ok <- !is.na(years) & !is.na(months)
  out[ok] <- as.Date(sprintf("%04d-%02d-01", years[ok], months[ok])); out
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
  bind_rows(lapply(group_rows, function(row) tibble(
    codigo = str_extract(as.character(raw[[1]][row]), "^[0-9]{2}"),
    grupo = str_remove(str_squish(as.character(raw[[1]][row])), "^[0-9]{2} "),
    ponderacion = to_num(raw[[6]][row]), anio = years[cols], mes = months[cols],
    fecha = as.Date(sprintf("%04d-%02d-01", years[cols], months[cols])),
    indice = to_num(unlist(raw[row, cols], use.names = FALSE))
  )))
}

parse_ipc_items <- function(path, item_codes) {
  raw <- read_excel(path, sheet = "2020-2026", col_names = FALSE, .name_repair = "minimal")
  periods <- detect_ipc_periods(raw); years <- periods$years; months <- periods$months; cols <- which(!is.na(years) & !is.na(months))
  codes <- str_extract(as.character(raw[[5]]), "^[0-9]+")
  rows <- match(item_codes, codes); assert_true(!anyNA(rows), "No se encontraron las series de artículos solicitadas.")
  bind_rows(lapply(seq_along(rows), function(i) tibble(
    codigo = item_codes[[i]], producto = as.character(raw[[5]][rows[[i]]]), anio = years[cols], mes = months[cols],
    fecha = as.Date(sprintf("%04d-%02d-01", years[cols], months[cols])), indice = to_num(unlist(raw[rows[[i]], cols], use.names = FALSE))
  )))
}

parse_subyacente <- function(path) {
  raw <- read_excel(path, sheet = 1, col_names = FALSE, .name_repair = "minimal")
  years <- fill_year(unlist(raw[[1]], use.names = FALSE)); months <- month_num(unlist(raw[[2]], use.names = FALSE))
  out <- tibble(anio = years, mes = months, fecha = make_date(years, months), subyacente_mensual = to_num(unlist(raw[[4]], use.names = FALSE)), subyacente_interanual = to_num(unlist(raw[[6]], use.names = FALSE))) |>
    filter(!is.na(fecha), is.finite(subyacente_mensual), is.finite(subyacente_interanual))
  bind_rows(out, tibble(anio = 2026, mes = 6, fecha = as.Date("2026-06-01"), subyacente_mensual = 0.37, subyacente_interanual = 4.96))
}

parse_transables <- function(path) {
  raw <- read_excel(path, sheet = 1, col_names = FALSE, .name_repair = "minimal")
  years <- fill_year(unlist(raw[[1]], use.names = FALSE)); months <- month_num(unlist(raw[[2]], use.names = FALSE))
  tibble(anio = years, mes = months, fecha = make_date(years, months), ipc_mensual = to_num(unlist(raw[[4]], use.names = FALSE)), transables_mensual = to_num(unlist(raw[[7]], use.names = FALSE)), transables_indice = to_num(unlist(raw[[6]], use.names = FALSE))) |>
    filter(!is.na(fecha), is.finite(transables_mensual), is.finite(transables_indice))
}

parse_exchange_monthly <- function(path) {
  raw <- read_excel(path, sheet = "PromMensual", col_names = FALSE, .name_repair = "minimal")
  anio <- to_num(unlist(raw[-(1:3), 1], use.names = FALSE)); mes <- month_num(unlist(raw[-(1:3), 2], use.names = FALSE))
  tibble(fecha = make_date(anio, mes), compra = to_num(unlist(raw[-(1:3), 3], use.names = FALSE))) |>
    filter(!is.na(fecha), is.finite(compra))
}

export_plot <- function(plot, dir, slug, width, height) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(dir, paste0(slug, ".svg")), plot, width = width, height = height, bg = pal$crema)
  ggsave(file.path(dir, paste0(slug, ".png")), plot, width = width, height = height, dpi = 320, bg = pal$crema)
}

ipc_path <- file.path(repo_root, "atlas", "data", "raw", "bcrd-precios", "ipc_articulos_base_2019-2020.xlsx")
out_dir <- file.path(repo_root, "research", "pollo-inflacion")
fig_dir <- file.path(out_dir, "figuras")
data_dir <- file.path(out_dir, "data", "procesados")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

groups <- parse_ipc_groups(ipc_path) |>
  group_by(grupo) |>
  arrange(fecha, .by_group = TRUE) |>
  mutate(variacion_mensual = 100 * (indice / lag(indice) - 1), contribucion_pp = ponderacion / 100 * variacion_mensual) |>
  ungroup()
assert_true(max(groups$fecha, na.rm = TRUE) == as.Date("2026-06-01"), "La base oficial de IPC no llega a junio de 2026.")

groups <- groups |>
  mutate(componente = case_when(
    str_detect(norm_text(grupo), "alimentos") ~ "Alimentos y bebidas",
    str_detect(norm_text(grupo), "transporte") ~ "Transporte",
    str_detect(norm_text(grupo), "vivienda") ~ "Vivienda",
    str_detect(norm_text(grupo), "restaurantes") ~ "Restaurantes y hoteles",
    str_detect(norm_text(grupo), "bienes y servicios") ~ "Bienes y servicios diversos",
    TRUE ~ "Resto"
  ))

total_monthly <- groups |>
  group_by(fecha) |>
  summarise(inflacion_total = sum(contribucion_pp, na.rm = TRUE), .groups = "drop")
recent <- as.Date("2025-01-01")

decomp <- groups |>
  filter(fecha >= recent, !is.na(contribucion_pp)) |>
  group_by(fecha, componente) |>
  summarise(contribucion_pp = sum(contribucion_pp), .groups = "drop") |>
  left_join(total_monthly, by = "fecha")
write_csv(decomp, file.path(data_dir, "01_descomposicion_ipc.csv"))

p_decomp <- ggplot(decomp, aes(fecha, contribucion_pp, fill = componente)) +
  geom_col(width = 25, colour = pal$crema, linewidth = 0.15) +
  geom_line(data = total_monthly |> filter(fecha >= recent), aes(fecha, inflacion_total), inherit.aes = FALSE, colour = pal$tinta, linewidth = 0.8) +
  geom_hline(yintercept = 0, colour = pal$gris, linewidth = 0.45) +
  scale_fill_manual(values = c("Alimentos y bebidas" = pal$terracota, "Transporte" = pal$azul, "Vivienda" = pal$ocre, "Restaurantes y hoteles" = pal$oliva, "Bienes y servicios diversos" = "#8B6F8E", "Resto" = pal$gris)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y", expand = expansion(mult = c(0.01, 0.03))) +
  labs(title = "Contribución de los grupos del IPC a la inflación mensual", subtitle = "Contribución mensual de cada componente al IPC · la línea negra es el total", x = NULL, y = "Puntos porcentuales", caption = "Fuente: BCRD, IPC por artículos · cálculo aproximado con ponderaciones oficiales · corte junio de 2026") +
  theme_editorial()
export_plot(p_decomp, fig_dir, "01_descomposicion_ipc", 11, 7)

heat <- groups |> filter(fecha >= recent, !is.na(contribucion_pp)) |> mutate(grupo = reorder(grupo, ponderacion))
write_csv(heat |> select(fecha, grupo, ponderacion, variacion_mensual, contribucion_pp), file.path(data_dir, "02_heatmap_contribuciones_ipc.csv"))
p_heat <- ggplot(heat, aes(fecha, grupo, fill = contribucion_pp)) +
  geom_tile(colour = pal$crema, linewidth = 0.35) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y", expand = c(0, 0)) +
  scale_fill_gradient2(low = pal$azul, mid = pal$crema, high = pal$terracota, midpoint = 0, labels = label_number(accuracy = 0.01), name = "p.p.") +
  labs(title = "Contribuciones positivas y negativas al IPC mensual", subtitle = "Contribución de cada grupo al cambio mensual del IPC · azul resta, terracota suma", x = NULL, y = NULL, caption = "Fuente: BCRD, IPC por artículos · ponderaciones oficiales · corte junio de 2026") +
  theme_editorial("none") + theme(axis.text.y = element_text(size = 8.5), legend.position = "right")
export_plot(p_heat, fig_dir, "02_heatmap_contribuciones_ipc", 11, 7.5)

pollo <- parse_ipc_items(ipc_path, c("0112201", "0112203")) |>
  mutate(producto = recode(codigo, `0112201` = "Pollo fresco", `0112203` = "Pechuga de pollo"), peso_item = case_when(codigo == "0112201" ~ 2.32990649, codigo == "0112203" ~ 0.066972124, TRUE ~ NA_real_)) |>
  group_by(codigo) |>
  arrange(fecha, .by_group = TRUE) |>
  mutate(variacion_mensual = 100 * (indice / lag(indice) - 1), contribucion_pp = peso_item / 100 * variacion_mensual) |>
  ungroup()
pollo_case <- pollo |> filter(fecha >= recent) |> left_join(total_monthly, by = "fecha")
write_csv(pollo_case, file.path(data_dir, "03_caso_instrumental_pollo.csv"))
p_case <- ggplot(pollo_case |> filter(producto == "Pollo fresco"), aes(fecha)) +
  geom_col(aes(y = contribucion_pp), fill = pal$terracota, width = 25) +
  geom_line(aes(y = inflacion_total), colour = pal$azul, linewidth = 1) +
  geom_hline(yintercept = 0, colour = pal$gris, linewidth = 0.45) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y", expand = expansion(mult = c(0.01, 0.03))) +
  labs(title = "Contribución del pollo fresco frente a la inflación mensual total", subtitle = "Barras: contribución del pollo fresco · línea azul: inflación mensual total · peso del pollo: 2.33%", x = NULL, y = "Puntos porcentuales", caption = "Fuente: BCRD, IPC por artículos · la contribución combina variación del precio y ponderación en la canasta · corte junio de 2026") +
  theme_editorial()
export_plot(p_case, fig_dir, "03_caso_instrumental_pollo", 11, 6.6)

subyacente <- parse_subyacente(file.path(repo_root, "atlas", "data", "raw", "bcrd-precios", "ipc_subyacente_base_2019-2020.xlsx")) |> filter(fecha >= as.Date("2024-01-01"))
general <- parse_transables(file.path(repo_root, "atlas", "data", "raw", "bcrd-precios", "ipc_tnt_base_2019-2020.xls")) |> transmute(fecha, inflacion_general = ipc_mensual) |> filter(fecha >= as.Date("2024-01-01"))
headline_core <- full_join(general, subyacente |> select(fecha, subyacente_mensual), by = "fecha") |> arrange(fecha)
write_csv(headline_core, file.path(data_dir, "04_general_vs_subyacente.csv"))
p_core <- ggplot(headline_core, aes(fecha)) +
  geom_hline(yintercept = 0, colour = pal$gris, linewidth = 0.45) +
  geom_line(data = headline_core |> filter(!is.na(inflacion_general)), aes(y = inflacion_general, colour = "IPC general"), linewidth = 1) +
  geom_line(data = headline_core |> filter(!is.na(subyacente_mensual)), aes(y = subyacente_mensual, colour = "IPC subyacente"), linewidth = 1) +
  scale_colour_manual(values = c("IPC general" = pal$terracota, "IPC subyacente" = pal$azul)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y", expand = expansion(mult = c(0.01, 0.03))) +
  labs(title = "Inflación mensual general y subyacente", subtitle = "IPC general frente a inflación subyacente · junio de 2026 incorpora el dato oficial del BCRD", x = NULL, y = "Variación mensual (%)", caption = "Fuente: BCRD, IPC general y subyacente · la subyacente excluye artículos volátiles y regulados") +
  theme_editorial()
export_plot(p_core, fig_dir, "04_general_vs_subyacente", 11, 6.6)

official_june <- 0.51
latest_raw <- groups |> filter(fecha == as.Date("2026-06-01"), !is.na(contribucion_pp)) |> group_by(grupo) |> summarise(contribucion_pp = sum(contribucion_pp), .groups = "drop") |> arrange(desc(contribucion_pp))
latest_bridge <- bind_rows(slice_head(latest_raw, n = 5), tibble(grupo = "Resto de grupos", contribucion_pp = sum(latest_raw$contribucion_pp[-seq_len(min(5, nrow(latest_raw)))])))
residual <- official_june - sum(latest_bridge$contribucion_pp)
latest_bridge <- bind_rows(latest_bridge, tibble(grupo = "Ajuste de enlace", contribucion_pp = residual)) |> mutate(
  grupo = reorder(grupo, contribucion_pp),
  label_value = sprintf("%.2f", if_else(abs(contribucion_pp) < 0.005, 0, contribucion_pp)),
  label_x = if_else(contribucion_pp < 0, contribucion_pp - 0.006, contribucion_pp + 0.006),
  label_hjust = if_else(contribucion_pp < 0, 1, 0)
)
write_csv(latest_bridge, file.path(data_dir, "05_puente_junio_2026.csv"))
p_bridge <- ggplot(latest_bridge, aes(contribucion_pp, grupo, fill = grupo == "Ajuste de enlace")) +
  geom_col(width = 0.68, colour = pal$tinta, linewidth = 0.2) +
  geom_text(aes(x = label_x, label = label_value, hjust = label_hjust), size = 3.1, fontface = "bold") +
  scale_fill_manual(values = c(`FALSE` = pal$terracota, `TRUE` = pal$gris), guide = "none") +
  scale_x_continuous(limits = c(min(0, min(latest_bridge$label_x) - 0.012), max(latest_bridge$label_x) + 0.025), expand = c(0, 0)) +
  labs(title = "Descomposición de la inflación mensual de junio de 2026", subtitle = "Principales incidencias, resto de grupos y ajuste hasta el total oficial de 0.51%", x = "Puntos porcentuales", y = NULL, caption = "Fuente: BCRD, IPC de junio de 2026 e índices por grupo · el ajuste recoge redondeos y diferencias de enlace; no es un componente económico") +
  theme_editorial("x")
export_plot(p_bridge, fig_dir, "05_puente_junio_2026", 10.5, 7.2)

tnt <- parse_transables(file.path(repo_root, "atlas", "data", "raw", "bcrd-precios", "ipc_tnt_base_2019-2020.xls")) |> select(fecha, transables_indice)
fx <- parse_exchange_monthly(file.path(repo_root, "atlas", "data", "raw", "bcrd-mercado-cambiario", "TASA_DOLAR_REFERENCIA_MC.xlsx"))
dollar_transables <- tnt |> left_join(fx, by = "fecha") |> arrange(fecha) |> mutate(transables_interanual = 100 * (transables_indice / lag(transables_indice, 12) - 1), dolar_interanual = 100 * (compra / lag(compra, 12) - 1)) |> filter(fecha >= as.Date("2024-01-01"), !is.na(transables_interanual), !is.na(dolar_interanual)) |> select(fecha, transables_interanual, dolar_interanual)
write_csv(dollar_transables, file.path(data_dir, "06_dolar_vs_transables.csv"))
p_dollar <- ggplot(dollar_transables, aes(fecha)) +
  geom_hline(yintercept = 0, colour = pal$gris, linewidth = 0.45) +
  geom_line(aes(y = transables_interanual, colour = "Bienes transables"), linewidth = 1) +
  geom_line(aes(y = dolar_interanual, colour = "Dólar"), linewidth = 1, linetype = "dashed") +
  scale_colour_manual(values = c("Bienes transables" = pal$terracota, "Dólar" = pal$azul)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y", expand = expansion(mult = c(0.01, 0.03))) +
  labs(title = "Bienes transables y tipo de cambio no se mueven uno a uno", subtitle = "Variación interanual de bienes transables frente al tipo de cambio de compra", x = NULL, y = "Variación interanual (%)", caption = "Fuente: BCRD, IPC de bienes transables y mercado cambiario · comparación descriptiva, no identificación causal") +
  theme_editorial()
export_plot(p_dollar, fig_dir, "06_dolar_vs_transables", 11, 6.6)

write_csv(tibble(
  id = c("01_descomposicion_ipc", "02_heatmap_contribuciones_ipc", "03_caso_instrumental_pollo", "04_general_vs_subyacente", "05_puente_junio_2026", "06_dolar_vs_transables"),
  pregunta = c("¿De dónde sale la inflación mensual?", "¿Qué grupos suman o restan cada mes?", "¿Por qué el pollo es un buen ejemplo, pero no toda la explicación?", "¿Qué parte de la inflación es más persistente?", "¿Cómo se cierra el dato oficial de junio?", "¿Hasta dónde llega el dólar?"),
  familia = c("Descomposición apilada", "Mapa de calor", "Contribución vs total", "General vs subyacente", "Puente de incidencias", "Comparación interanual"), fuente = "BCRD IPC",
  advertencia = c("Contribuciones aproximadas a partir de ponderaciones oficiales", "Azul resta; terracota suma", "El pollo fresco pesa 2.33% de la canasta", "Junio 2026: dato subyacente oficial", "El residuo no es un componente económico", "Comparación descriptiva, no causal")
), file.path(out_dir, "chart-map.csv"))

message("Graficos de descomposicion de inflacion generados hasta junio de 2026.")
