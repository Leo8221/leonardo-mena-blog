#!/usr/bin/env Rscript

options(encoding = "UTF-8", scipen = 999)
if (!isTRUE(l10n_info()[["UTF-8"]])) stop("R no está leyendo UTF-8.", call. = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(tidyr)
  library(svglite)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]
script_path <- sub("^--file=", "", script_arg)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/")
setwd(repo_root)

pal <- list(
  crema = "#F7F1E7",
  tinta = "#252525",
  terracota = "#B65C43",
  azul = "#4F789F",
  gris = "#9B9B94",
  gris_claro = "#DED9D0",
  blanco = "#FFFDFC"
)

theme_epicteto <- function() {
  theme_minimal(base_size = 12, base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = pal$crema, colour = NA),
      panel.background = element_rect(fill = pal$crema, colour = NA),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(colour = pal$tinta),
      axis.text.x = element_text(size = 9, colour = "#6A6761"),
      axis.text.y = element_text(size = 10, face = "bold", hjust = 1),
      plot.title = element_text(face = "bold", size = 18, colour = pal$tinta, margin = margin(b = 6)),
      plot.subtitle = element_text(size = 11, colour = "#5A5A55", margin = margin(b = 18)),
      plot.caption = element_text(size = 8.5, colour = "#65655F", hjust = 0, margin = margin(t = 14)),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(size = 9.5, face = "bold", colour = pal$tinta),
      legend.key = element_blank(),
      legend.spacing.x = unit(8, "pt"),
      plot.margin = margin(18, 28, 16, 18)
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
      device = ragg::agg_png, width = width, height = height, dpi = 320, bg = pal$crema
    )
  } else {
    ggsave(
      file.path(directory, paste0(slug, ".png")), plot,
      width = width, height = height, dpi = 320, bg = pal$crema
    )
  }
}

module_dir <- file.path(repo_root, "research", "epicteto-fondo-emergencia")
figure_dir <- file.path(module_dir, "figuras")
processed_dir <- file.path(module_dir, "data", "procesados")
data_path <- file.path(processed_dir, "01_enief_emergencia_2023.csv")

if (!file.exists(data_path)) stop("Falta el archivo procesado de ENIEF 2023.", call. = FALSE)

low_name <- "Ingresos RD$0–31,200"
high_name <- "Ingresos RD$31,201 o más"

enief <- read_csv(data_path, show_col_types = FALSE) |>
  mutate(
    estrato = as.character(estrato),
    respuesta = as.character(respuesta),
    porcentaje = as.numeric(porcentaje)
  )

if (nrow(enief) != 20L || any(!is.finite(enief$porcentaje))) {
  stop("La tabla ENIEF debe contener 20 porcentajes válidos.", call. = FALSE)
}
if (!setequal(unique(enief$estrato), c(low_name, high_name))) {
  stop("Los estratos de ingreso no coinciden con los esperados.", call. = FALSE)
}

wide <- enief |>
  select(respuesta, estrato, porcentaje) |>
  pivot_wider(names_from = estrato, values_from = porcentaje) |>
  mutate(
    ingreso_bajo = .data[[low_name]],
    ingreso_alto = .data[[high_name]],
    diferencia_pp = ingreso_alto - ingreso_bajo,
    maximo = pmax(ingreso_bajo, ingreso_alto)
  )

if (nrow(wide) != 10L || any(!is.finite(wide$diferencia_pp))) {
  stop("No se pudo construir la comparación entre los diez mecanismos.", call. = FALSE)
}

write_csv(
  wide |> select(respuesta, all_of(low_name), all_of(high_name), diferencia_pp),
  file.path(processed_dir, "02_diferencia_estratos.csv")
)

# Figura 1: pendientes compactas. La forma y la posición distinguen los estratos,
# por lo que el gráfico sigue siendo legible sin depender únicamente del color.
response_order <- wide |> arrange(maximo) |> pull(respuesta)
slope_data <- wide |>
  mutate(respuesta = factor(respuesta, levels = response_order))

p_response <- ggplot(slope_data, aes(y = respuesta)) +
  geom_hline(
    yintercept = seq_len(nrow(slope_data)), colour = "#E8E1D7", linewidth = .45
  ) +
  geom_segment(
    aes(x = ingreso_bajo, xend = ingreso_alto, yend = respuesta),
    colour = "#B8B2A9", linewidth = 2.2, lineend = "round"
  ) +
  geom_point(
    aes(x = ingreso_bajo, shape = low_name),
    size = 4.3, stroke = .75, fill = pal$terracota, colour = pal$tinta
  ) +
  geom_point(
    aes(x = ingreso_alto, shape = high_name),
    size = 4.3, stroke = .75, fill = pal$azul, colour = pal$tinta
  ) +
  geom_text(
    aes(
      x = ingreso_bajo, label = sprintf("%.1f%%", ingreso_bajo),
      hjust = if_else(ingreso_bajo < 4, 0, .5)
    ),
    nudge_y = .22, colour = pal$terracota, size = 3.05, fontface = "bold"
  ) +
  geom_text(
    aes(
      x = ingreso_alto, label = sprintf("%.1f%%", ingreso_alto),
      hjust = if_else(ingreso_alto < 4, 0, .5)
    ),
    nudge_y = -.22, colour = pal$azul, size = 3.05, fontface = "bold"
  ) +
  scale_shape_manual(values = setNames(c(21, 22), c(low_name, high_name))) +
  scale_x_continuous(
    breaks = seq(0, 50, 10), labels = label_percent(scale = 1),
    limits = c(0, 53), expand = c(0, 0)
  ) +
  guides(shape = guide_legend(override.aes = list(
    size = 4.5,
    fill = c(pal$terracota, pal$azul),
    colour = pal$tinta
  ))) +
  labs(
    title = "Primera respuesta ante una emergencia financiera",
    subtitle = "Porcentaje de personas por grupo de ingreso · ENIEF 2023",
    caption = "Fuente: BCRD, ENIEF 2023 · primera respuesta declarada; porcentajes sin no sabe/no contesta"
  ) +
  theme_epicteto() +
  theme(axis.text.y = element_text(size = 9.5, face = "bold"))

export_plot(p_response, figure_dir, "01_respuesta_emergencia_ingreso", 8.6, 8.2)

# Figura 2: diferencia divergente con jerarquía visual. Se mantienen todos los
# mecanismos, pero los cambios pequeños reciben menor peso gráfico.
diff_order <- wide |> arrange(diferencia_pp) |> pull(respuesta)
diff_data <- wide |>
  mutate(
    respuesta = factor(respuesta, levels = diff_order),
    direccion = if_else(diferencia_pp >= 0, "Mayor ingreso", "Menor ingreso"),
    destacado = abs(diferencia_pp) >= 5,
    etiqueta = sprintf("%+.1f pp", diferencia_pp),
    label_hjust = if_else(diferencia_pp >= 0, -.16, 1.16)
  )

p_difference <- ggplot(diff_data, aes(y = respuesta)) +
  geom_hline(
    yintercept = seq_len(nrow(diff_data)), colour = "#E8E1D7", linewidth = .45
  ) +
  geom_segment(
    aes(
      x = 0, xend = diferencia_pp, yend = respuesta,
      colour = direccion, alpha = destacado, linewidth = destacado
    ),
    lineend = "butt"
  ) +
  geom_vline(xintercept = 0, colour = pal$tinta, linewidth = .55) +
  geom_point(
    aes(x = diferencia_pp, colour = direccion, alpha = destacado),
    size = 3.2
  ) +
  geom_text(
    aes(
      x = diferencia_pp, label = etiqueta, hjust = label_hjust,
      alpha = destacado
    ),
    colour = pal$tinta, size = 3.15, fontface = "bold"
  ) +
  annotate(
    "text", x = -19.3, y = 10.85, label = "MÁS FRECUENTE EN INGRESOS BAJOS",
    hjust = 0, colour = pal$terracota, size = 3.05, fontface = "bold"
  ) +
  annotate(
    "text", x = 19.3, y = 10.85, label = "MÁS FRECUENTE EN INGRESOS ALTOS",
    hjust = 1, colour = pal$azul, size = 3.05, fontface = "bold"
  ) +
  scale_colour_manual(values = c("Mayor ingreso" = pal$azul, "Menor ingreso" = pal$terracota)) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = .42), guide = "none") +
  scale_linewidth_manual(values = c(`TRUE` = 4.8, `FALSE` = 2.2), guide = "none") +
  scale_x_continuous(
    breaks = c(-20, -10, 0, 10, 20),
    labels = function(x) sprintf("%+d pp", x),
    limits = c(-20, 20), expand = expansion(mult = c(.01, .01))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Diferencia entre grupos de ingreso",
    subtitle = "Ingresos RD$31,201 o más menos RD$0–31,200 · puntos porcentuales · ENIEF 2023",
    caption = "Fuente: BCRD, ENIEF 2023 · primera respuesta declarada; no mide riqueza ni saldo de ahorro"
  ) +
  theme_epicteto() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 9.5, face = "bold"),
    plot.margin = margin(26, 42, 16, 18)
  )

export_plot(p_difference, figure_dir, "02_diferencia_respuesta_emergencia", 8.6, 8.0)

write_csv(
  bind_rows(
    tibble(
      id = "01_respuesta_emergencia_ingreso",
      pregunta = "¿Cómo cubren una emergencia los hogares?",
      familia = "Pendientes comparables",
      fuente = "BCRD ENIEF 2023",
      advertencia = "Primera respuesta declarada; no mide saldo de ahorro"
    ),
    tibble(
      id = "02_diferencia_respuesta_emergencia",
      pregunta = "¿Qué respuestas separan a los estratos?",
      familia = "Lollipop divergente",
      fuente = "BCRD ENIEF 2023",
      advertencia = "Diferencias descriptivas en puntos porcentuales"
    )
  ),
  file.path(module_dir, "chart-map.csv")
)

message("Visuales de Epicteto actualizados en: ", figure_dir)
