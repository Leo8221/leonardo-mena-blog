#!/usr/bin/env Rscript

options(encoding = "UTF-8", scipen = 999)
if (!isTRUE(l10n_info()[["UTF-8"]])) stop("R no está leyendo UTF-8.", call. = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(svglite)
  library(tibble)
  library(tidyr)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]
script_path <- sub("^--file=", "", script_arg)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/")
setwd(repo_root)

pal <- list(
  crema = "#F7F1E7",
  tinta = "#252525",
  terracota = "#B65C43",
  terracota_clara = "#E7C5B8",
  azul = "#4F789F",
  azul_claro = "#C8D6E2",
  gris = "#8E8B84",
  gris_claro = "#DED9D0",
  blanco = "#FFFDFC"
)

theme_remesas <- function(grid = "y") {
  theme_minimal(base_size = 12, base_family = "Arial") +
    theme(
      text = element_text(colour = pal$tinta),
      plot.background = element_rect(fill = pal$crema, colour = NA),
      panel.background = element_rect(fill = pal$blanco, colour = NA),
      plot.title = element_text(
        face = "bold", size = 18, colour = pal$tinta,
        hjust = 0, margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        size = 11, colour = "#5A5A55", hjust = 0,
        margin = margin(b = 16)
      ),
      plot.caption = element_text(
        size = 8.5, colour = "#65655F", hjust = 0,
        margin = margin(t = 14)
      ),
      axis.title = element_text(size = 10.5, colour = "#5A5A55"),
      axis.text = element_text(size = 9.5, colour = "#5A5A55"),
      axis.line = element_line(colour = pal$gris_claro, linewidth = .45),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = if (grid %in% c("x", "both")) element_line(colour = "#ECE7DE", linewidth = .45) else element_blank(),
      panel.grid.major.y = if (grid %in% c("y", "both")) element_line(colour = "#ECE7DE", linewidth = .45) else element_blank(),
      legend.position = "none",
      plot.margin = margin(18, 22, 16, 18)
    )
}

export_plot <- function(plot, directory, slug, width, height) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    file.path(directory, paste0(slug, ".svg")), plot,
    device = svglite::svglite, width = width, height = height, bg = pal$crema
  )
  if (requireNamespace("ragg", quietly = TRUE)) {
    ggsave(
      file.path(directory, paste0(slug, ".png")), plot,
      device = ragg::agg_png, width = width, height = height,
      dpi = 320, bg = pal$crema
    )
  } else {
    ggsave(
      file.path(directory, paste0(slug, ".png")), plot,
      width = width, height = height, dpi = 320, bg = pal$crema
    )
  }
}

module_dir <- file.path(repo_root, "research", "peso-fuerte-remesas")
figure_dir <- file.path(module_dir, "figuras")
data_dir <- file.path(module_dir, "data", "procesados")
input_path <- file.path(data_dir, "01_remesas_tipo_cambio_ipc.csv")

if (!file.exists(input_path)) stop("Falta la serie procesada de remesas, cambio e IPC.", call. = FALSE)

series <- read_csv(input_path, show_col_types = FALSE) |>
  transmute(
    fecha = as.Date(fecha),
    dops_por_100usd = as.numeric(dops_por_100usd),
    ipc = as.numeric(ipc)
  ) |>
  arrange(fecha)

required <- c("fecha", "dops_por_100usd", "ipc")
if (!all(required %in% names(series))) stop("La serie no contiene las columnas requeridas.", call. = FALSE)
if (anyDuplicated(series$fecha)) stop("La serie contiene meses duplicados.", call. = FALSE)
if (nrow(series) < 150 || anyNA(series)) stop("La serie no tiene cobertura o valores suficientes.", call. = FALSE)

base <- slice_head(series, n = 1)
latest <- slice_tail(series, n = 1)

series <- series |>
  mutate(
    cambio_indice = 100 * dops_por_100usd / base$dops_por_100usd,
    precios_indice = 100 * ipc / base$ipc,
    poder_indice = 100 * cambio_indice / precios_indice,
    segmento = cumsum(c(0, as.numeric(diff(fecha)) > 45))
  )

latest <- slice_tail(series, n = 1)
missing_2024_start <- as.Date("2024-01-01")
missing_2024_end <- as.Date("2025-01-01")

# Figura 1: las dos fuerzas que determinan el poder de compra de US$100.
forces <- series |>
  select(fecha, segmento, `Pesos recibidos` = cambio_indice, `Precios al consumidor` = precios_indice) |>
  pivot_longer(c(`Pesos recibidos`, `Precios al consumidor`), names_to = "serie", values_to = "indice")

force_ends <- forces |>
  filter(fecha == max(fecha)) |>
  mutate(
    label = if_else(
      serie == "Precios al consumidor",
      sprintf("Precios\n%.0f", indice),
      sprintf("Cambio\n%.0f", indice)
    )
  )

p_forces <- ggplot(forces, aes(fecha, indice, group = interaction(serie, segmento))) +
  annotate(
    "rect", xmin = missing_2024_start, xmax = missing_2024_end,
    ymin = -Inf, ymax = Inf, fill = "#EEE9E1", colour = NA
  ) +
  geom_line(aes(colour = serie, linetype = serie), linewidth = 1.25) +
  geom_point(
    data = force_ends, aes(colour = serie),
    size = 3.2, shape = 21, fill = pal$blanco, stroke = 1
  ) +
  geom_text(
    data = force_ends,
    aes(x = fecha + 60, y = indice, label = label, colour = serie),
    hjust = 0, size = 3.15, fontface = "bold", lineheight = .92
  ) +
  annotate(
    "text", x = as.Date("2024-07-01"), y = 118,
    label = "2024\nsin datos\ncombinados",
    colour = "#6B6862", size = 2.85, lineheight = .9
  ) +
  scale_colour_manual(values = c("Pesos recibidos" = pal$azul, "Precios al consumidor" = pal$terracota)) +
  scale_linetype_manual(values = c("Pesos recibidos" = "solid", "Precios al consumidor" = "22")) +
  scale_x_date(
    breaks = as.Date(paste0(seq(2010, 2026, 2), "-01-01")), date_labels = "%Y",
    limits = c(as.Date("2010-01-01"), as.Date("2028-01-01")),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(breaks = seq(100, 200, 20), limits = c(92, 208)) +
  labs(
    title = "Tipo de cambio e inflación desde 2010",
    subtitle = "Índices comparables · enero de 2010 = 100 · corte: mayo de 2026",
    x = NULL, y = "Índice",
    caption = "Las líneas se interrumpen cuando faltan meses: agosto de 2019 y todo 2024. Fuente: BCRD."
  ) +
  theme_remesas("y")

export_plot(p_forces, figure_dir, "01_remesas_tipo_cambio_poder_compra", 9.6, 6.5)

# Figura 2: resultado neto de ambas fuerzas.
latest_label <- sprintf("%.0f\nmayo 2026\n−%.0f%%\nvs base", latest$poder_indice, 100 - latest$poder_indice)

p_power <- ggplot(series, aes(fecha, poder_indice, group = segmento)) +
  annotate(
    "rect", xmin = missing_2024_start, xmax = missing_2024_end,
    ymin = -Inf, ymax = Inf, fill = "#EEE9E1", colour = NA
  ) +
  geom_hline(yintercept = 100, colour = pal$gris, linetype = "32", linewidth = .75) +
  geom_ribbon(
    aes(ymin = pmin(poder_indice, 100), ymax = pmax(poder_indice, 100)),
    fill = pal$terracota_clara, alpha = .52, colour = NA
  ) +
  geom_line(colour = pal$terracota, linewidth = 1.35) +
  geom_point(
    data = latest, aes(fecha, poder_indice),
    size = 3.4, shape = 21, fill = pal$blanco,
    colour = pal$terracota, stroke = 1.1
  ) +
  annotate(
    "text", x = latest$fecha + 60, y = latest$poder_indice,
    label = latest_label, hjust = 0, vjust = .5,
    colour = pal$terracota, size = 3.2, fontface = "bold", lineheight = .95
  ) +
  annotate(
    "text", x = as.Date("2024-07-01"), y = 111.5,
    label = "2024\nsin datos\ncombinados",
    colour = "#6B6862", size = 2.85, lineheight = .9
  ) +
  annotate(
    "text", x = as.Date("2010-04-01"), y = 100.7,
    label = "base = 100", hjust = 0,
    colour = "#6B6862", size = 2.8, fontface = "bold"
  ) +
  scale_x_date(
    breaks = as.Date(paste0(seq(2010, 2026, 2), "-01-01")), date_labels = "%Y",
    limits = c(as.Date("2010-01-01"), as.Date("2028-01-01")),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(breaks = seq(80, 115, 5), limits = c(79, 115)) +
  labs(
    title = "Poder de compra de US$100 enviados",
    subtitle = "Pesos recibidos ajustados por IPC nacional · enero de 2010 = 100",
    x = NULL, y = "Poder de compra relativo",
    caption = "No representa la canasta particular de cada hogar. Fuente: BCRD."
  ) +
  theme_remesas("y")

export_plot(p_power, figure_dir, "02_poder_compra_remesas", 9.6, 6.3)

# Figura 3: traducción del índice a pesos corrientes del último mes disponible.
pesos_necesarios <- base$dops_por_100usd * latest$ipc / base$ipc
brecha <- pesos_necesarios - latest$dops_por_100usd

summary_data <- tribble(
  ~concepto, ~valor, ~fill, ~outline,
  "Pesos recibidos\npor US$100", latest$dops_por_100usd, pal$azul, pal$azul,
  "Pesos necesarios para conservar\nel poder de compra de ene. 2010", pesos_necesarios, pal$terracota_clara, pal$terracota
) |>
  mutate(concepto = factor(concepto, levels = rev(concepto)))

write_csv(
  tibble(
    fecha_corte = latest$fecha,
    pesos_recibidos_por_100usd = latest$dops_por_100usd,
    pesos_necesarios_poder_compra_base = pesos_necesarios,
    brecha_pesos = brecha,
    poder_compra_relativo = latest$poder_indice
  ),
  file.path(data_dir, "04_resumen_poder_compra_ultimo_mes.csv")
)

p_summary <- ggplot(summary_data, aes(valor, concepto)) +
  geom_col(aes(fill = fill, colour = outline), width = .56, linewidth = .85) +
  geom_text(
    aes(label = paste0("RD$ ", label_number(big.mark = ",", accuracy = 1)(valor))),
    hjust = -0.1, colour = pal$tinta, size = 3.45, fontface = "bold"
  ) +
  annotate(
    "segment", x = latest$dops_por_100usd, xend = pesos_necesarios,
    y = 1.5, yend = 1.5, colour = pal$tinta, linewidth = .8,
    arrow = grid::arrow(ends = "both", length = grid::unit(.11, "inches"), type = "closed")
  ) +
  annotate(
    "text", x = mean(c(latest$dops_por_100usd, pesos_necesarios)), y = 1.63,
    label = paste0("brecha  RD$ ", label_number(big.mark = ",", accuracy = 1)(brecha)),
    colour = pal$tinta, size = 3.25, fontface = "bold"
  ) +
  annotate(
    "text", x = 8000, y = .63,
    label = sprintf("Poder de compra retenido\n%.0f%%", latest$poder_indice),
    hjust = 1, colour = pal$terracota, size = 4.0, fontface = "bold", lineheight = .95
  ) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(
    labels = label_number(prefix = "RD$ ", big.mark = ",", accuracy = 1),
    breaks = seq(0, 8000, 2000), limits = c(0, 8400),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Los pesos recibidos no alcanzan el poder de compra base",
    subtitle = "Comparación para US$100 · mayo de 2026 · IPC nacional",
    x = NULL, y = NULL,
    caption = "La referencia actualiza por IPC los RD$3,611 recibidos por US$100 en enero de 2010. Fuente: BCRD."
  ) +
  theme_remesas("x") +
  theme(axis.text.y = element_text(size = 10.5, colour = pal$tinta, lineheight = .92))

export_plot(p_summary, figure_dir, "03_mapa_calor_poder_compra", 9.6, 6.1)

write_csv(
  bind_rows(
    tibble(
      id = "01_remesas_tipo_cambio_poder_compra",
      pregunta = "¿El tipo de cambio compensó el aumento de los precios?",
      familia = "Líneas indexadas con brechas explícitas",
      fuente = "BCRD",
      advertencia = "Faltan agosto de 2019 y todo 2024 en la serie combinada"
    ),
    tibble(
      id = "02_poder_compra_remesas",
      pregunta = "¿Cuánto poder de compra conserva US$100?",
      familia = "Línea con referencia",
      fuente = "BCRD",
      advertencia = "IPC nacional; no representa la canasta de cada hogar"
    ),
    tibble(
      id = "03_mapa_calor_poder_compra",
      pregunta = "¿Cuántos pesos faltan para conservar el poder de compra base?",
      familia = "Comparación de barras con brecha",
      fuente = "BCRD",
      advertencia = "Corte en mayo de 2026; comparación relativa a enero de 2010"
    )
  ),
  file.path(module_dir, "chart-map.csv")
)

message("Visuales de peso fuerte y remesas actualizados en: ", figure_dir)
