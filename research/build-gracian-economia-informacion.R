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
  oliva = "#76825A",
  ocre = "#C79B53",
  gris = "#9B9B94",
  gris_claro = "#DED9D0",
  blanco = "#FFFDFC"
)

theme_gracian <- function() {
  theme_void(base_size = 12, base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = pal$crema, colour = NA),
      panel.background = element_rect(fill = pal$crema, colour = NA),
      plot.title = element_text(
        face = "bold", size = 18, colour = pal$tinta, hjust = 0,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        size = 11, colour = "#5A5A55", hjust = 0,
        margin = margin(b = 16)
      ),
      plot.caption = element_text(
        size = 8.5, colour = "#65655F", hjust = 0,
        margin = margin(t = 14)
      ),
      plot.margin = margin(18, 20, 16, 18)
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

module_dir <- file.path(repo_root, "research", "gracian-economia-informacion")
figure_dir <- file.path(module_dir, "figuras")
# Figura 1: matriz editorial. Las celdas comparten una superficie neutra y el
# color queda reservado a la clasificación, evitando cuatro grandes bloques.
cells <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~titulo, ~detalle, ~acento,
  22, 58, 54, 82, "Riesgo moral", "la conducta cambia\ndespués del intercambio", pal$ocre,
  61, 97, 54, 82, "Señalización", "reputación o señal\ncostosa", pal$oliva,
  22, 58, 16, 44, "Mercado con información\nobservable", "la calidad puede\ndistinguirse", pal$azul,
  61, 97, 16, 44, "Selección adversa", "el comprador no\ndistingue la calidad", pal$terracota
) |>
  mutate(
    x = (xmin + xmax) / 2,
    y_title = ymax - 10,
    y_detail = ymin + 9
  )

column_headers <- tribble(
  ~x, ~label, ~colour,
  40, "LA INFORMACIÓN SE OBSERVA", pal$azul,
  79, "LA INFORMACIÓN SE OCULTA", pal$terracota
)

row_headers <- tribble(
  ~y, ~label,
  68, "DESPUÉS DEL\nINTERCAMBIO",
  30, "ANTES DEL\nINTERCAMBIO"
)

p_matrix <- ggplot() +
  geom_segment(
    data = column_headers,
    aes(x = x - 8, xend = x + 8, y = 91, yend = 91, colour = colour),
    linewidth = 2.2, lineend = "butt"
  ) +
  geom_text(
    data = column_headers,
    aes(x = x, y = 94, label = label, colour = colour),
    size = 3.15, fontface = "bold"
  ) +
  geom_segment(
    x = 19, xend = 19, y = 13, yend = 85,
    colour = "#CFC7BC", linewidth = .6
  ) +
  geom_segment(
    data = row_headers,
    aes(x = 17.5, xend = 20.5, y = y, yend = y),
    colour = pal$tinta, linewidth = .7
  ) +
  geom_text(
    data = row_headers,
    aes(x = 15.5, y = y, label = label),
    hjust = 1, size = 3.15, fontface = "bold",
    colour = pal$tinta, lineheight = .9
  ) +
  geom_rect(
    data = cells,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = pal$blanco, colour = "#D8D0C4", linewidth = .65
  ) +
  geom_rect(
    data = cells,
    aes(xmin = xmin, xmax = xmax, ymin = ymax - 2.1, ymax = ymax, fill = acento),
    colour = NA
  ) +
  geom_text(
    data = cells,
    aes(x = x, y = y_title, label = titulo),
    size = 4.15, fontface = "bold", colour = pal$tinta,
    lineheight = .92
  ) +
  geom_text(
    data = cells,
    aes(x = x, y = y_detail, label = detalle),
    size = 3.25, colour = "#5A5752", lineheight = .95
  ) +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(8, 98), expand = c(0, 0)) +
  coord_fixed(clip = "off") +
  labs(
    title = "Problemas de información asimétrica",
    subtitle = "Matriz conceptual según momento del intercambio y visibilidad de la información",
    caption = "Esquema de teoría económica · no representa una estimación estadística"
  ) +
  theme_gracian()

export_plot(p_matrix, figure_dir, "01_matriz_informacion_asimetrica", 9.2, 8.4)

# Figura 2: la señal funciona como filtro. Las tres bandas muestran qué parte de
# la información privada se convierte en evidencia observable para decidir.
signal_rows <- tribble(
  ~y, ~privada, ~senal, ~recibida,
  55, "calidad", "prueba", "evidencia sobre calidad",
  45, "esfuerzo", "historial", "conducta observada",
  35, "riesgo", "garantía", "riesgo acotado"
)

observable <- tibble(
  x = c(14, 33, 52),
  label = c("precio", "contrato", "reputación")
)

p_signal <- ggplot() +
  annotate(
    "rect",
    xmin = 4, xmax = 36, ymin = 26, ymax = 64,
    fill = alpha(pal$terracota, .08), colour = pal$terracota, linewidth = .7
  ) +
  annotate(
    "rect",
    xmin = 40, xmax = 60, ymin = 23, ymax = 67,
    fill = pal$blanco, colour = pal$oliva, linewidth = .9
  ) +
  annotate(
    "rect",
    xmin = 40, xmax = 60, ymin = 63.5, ymax = 67,
    fill = pal$oliva, colour = NA
  ) +
  annotate(
    "segment",
    x = 5, xend = 92, y = 80, yend = 80,
    colour = pal$azul, linewidth = 1.3, lineend = "butt"
  ) +
  annotate(
    "text",
    x = 5, y = 85, label = "INFORMACIÓN YA OBSERVABLE",
    hjust = 0, colour = pal$azul, size = 3.35, fontface = "bold"
  ) +
  geom_text(
    data = observable,
    aes(x = x, y = 77, label = label),
    colour = pal$tinta, size = 3.45, fontface = "bold"
  ) +
  annotate(
    "text",
    x = 92, y = 85, label = "LLEGA SIN FILTRO",
    hjust = 1, colour = pal$azul, size = 3.15, fontface = "bold"
  ) +
  annotate(
    "text",
    x = 20, y = 69, label = "INFORMACIÓN PRIVADA",
    colour = pal$terracota, size = 3.25, fontface = "bold"
  ) +
  annotate(
    "text",
    x = 50, y = 69.5, label = "SEÑAL VERIFICABLE",
    colour = pal$oliva, size = 3.25, fontface = "bold"
  ) +
  annotate(
    "text",
    x = 76, y = 69, label = "INFORMACIÓN RECIBIDA",
    colour = pal$azul, size = 3.25, fontface = "bold"
  ) +
  geom_segment(
    data = signal_rows,
    aes(x = 15, xend = 40, y = y, yend = y),
    colour = pal$terracota, linewidth = 3.2, lineend = "butt"
  ) +
  geom_segment(
    data = signal_rows,
    aes(x = 40, xend = 60, y = y, yend = y),
    colour = pal$oliva, linewidth = 3.2, lineend = "butt"
  ) +
  geom_segment(
    data = signal_rows,
    aes(x = 60, xend = 92, y = y, yend = y),
    colour = pal$azul, linewidth = 3.2, lineend = "butt"
  ) +
  geom_text(
    data = signal_rows,
    aes(x = 7, y = y, label = privada),
    hjust = 0, colour = pal$tinta, size = 3.45, fontface = "bold"
  ) +
  geom_text(
    data = signal_rows,
    aes(x = 50, y = y + 2.3, label = senal),
    colour = pal$oliva, size = 3.05, fontface = "bold"
  ) +
  geom_text(
    data = signal_rows,
    aes(x = 63, y = y + 2.3, label = recibida),
    hjust = 0, colour = pal$tinta, size = 3.05
  ) +
  annotate(
    "segment",
    x = 92, xend = 92, y = 80, yend = 20,
    colour = pal$tinta, linewidth = .75,
    arrow = grid::arrow(length = grid::unit(.16, "cm"), type = "closed")
  ) +
  annotate(
    "rect",
    xmin = 62, xmax = 96, ymin = 8, ymax = 20,
    fill = alpha(pal$azul, .10), colour = pal$azul, linewidth = .75
  ) +
  annotate(
    "text",
    x = 79, y = 15.3, label = "INFORMACIÓN PARA DECIDIR",
    colour = pal$azul, size = 3.35, fontface = "bold"
  ) +
  annotate(
    "text",
    x = 79, y = 11.7, label = "selección y conducta",
    colour = pal$tinta, size = 3.15
  ) +
  annotate(
    "text", x = 4, y = 14, label = "La señal revela evidencia; no elimina toda la incertidumbre.",
    hjust = 0, colour = "#66625C", size = 3.1
  ) +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(4, 88), expand = c(0, 0)) +
  coord_fixed(clip = "off") +
  labs(
    title = "Mecanismo de señalización",
    subtitle = "Cómo una señal convierte información privada en evidencia disponible para decidir",
    caption = "Esquema conceptual de teoría económica · no representa una estimación"
  ) +
  theme_gracian()

export_plot(p_signal, figure_dir, "02_flujo_senal_informacion", 9.4, 8.3)

write_csv(
  bind_rows(
    tibble(
      id = "01_matriz_informacion_asimetrica",
      pregunta = "¿Qué ocurre cuando una parte sabe más que la otra?",
      familia = "Matriz editorial",
      fuente = "Teoría económica",
      advertencia = "No es evidencia estadística"
    ),
    tibble(
      id = "02_flujo_senal_informacion",
      pregunta = "¿Cómo convierte una señal información privada en evidencia observable?",
      familia = "Filtro de información",
      fuente = "Esquema editorial",
      advertencia = "No estima efectos de señales"
    )
  ),
  file.path(module_dir, "chart-map.csv")
)

message("Visuales de Gracián actualizados en: ", figure_dir)
