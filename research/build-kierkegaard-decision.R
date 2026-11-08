#!/usr/bin/env Rscript

options(encoding = "UTF-8", scipen = 999)
if (!isTRUE(l10n_info()[["UTF-8"]])) stop("R no está leyendo UTF-8.", call. = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
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
  gris = "#9B9B94",
  gris_claro = "#DED9D0",
  blanco = "#FFFDFC"
)

theme_kierkegaard <- function() {
  theme_void(base_size = 12, base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = pal$crema, colour = NA),
      panel.background = element_rect(fill = pal$crema, colour = NA),
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

module_dir <- file.path(repo_root, "research", "kierkegaard-decision")
figure_dir <- file.path(module_dir, "figuras")
illustration_path <- file.path(module_dir, "assets", "ilustracion-decision-costo-oportunidad.png")

if (!file.exists(illustration_path)) stop("Falta la ilustración de la bifurcación.", call. = FALSE)

cards <- tribble(
  ~ymin, ~ymax, ~titulo, ~acento,
  56, 86, "DECIDIR AHORA", pal$terracota,
  14, 44, "ESPERAR", pal$azul
) |>
  mutate(y = (ymin + ymax) / 2)

details <- tribble(
  ~route, ~x, ~y, ~label, ~detail,
  "now", 49, 68, "COMPROMISO", "menor\nreversibilidad",
  "now", 67, 68, "RENUNCIA", "otras opciones\nse cierran",
  "now", 85, 68, "RESULTADO", "permanece\nincierto",
  "wait", 49, 26, "OPCIÓN", "permanece\nabierta",
  "wait", 67, 26, "INFORMACIÓN", "puede\nmejorar",
  "wait", 85, 26, "RETRASO", "también\ntiene costo"
)

p_decision <- ggplot() +
  annotate(
    "rect", xmin = 4, xmax = 18, ymin = 43, ymax = 57,
    fill = pal$tinta, colour = NA
  ) +
  annotate(
    "text", x = 11, y = 50, label = "MOMENTO\nDE ELEGIR",
    colour = pal$blanco, size = 3.3, fontface = "bold", lineheight = .9
  ) +
  annotate(
    "segment", x = 18, xend = 28, y = 50, yend = 50,
    colour = pal$tinta, linewidth = .8
  ) +
  annotate(
    "segment", x = 28, xend = 28, y = 29, yend = 71,
    colour = pal$tinta, linewidth = .8
  ) +
  annotate(
    "segment", x = 28, xend = 36, y = 71, yend = 71,
    colour = pal$terracota, linewidth = 2.2, lineend = "butt"
  ) +
  annotate(
    "segment", x = 28, xend = 36, y = 29, yend = 29,
    colour = pal$azul, linewidth = 2.2, lineend = "butt"
  ) +
  geom_rect(
    data = cards,
    aes(xmin = 36, xmax = 96, ymin = ymin, ymax = ymax),
    fill = pal$blanco, colour = "#D8D0C4", linewidth = .7
  ) +
  geom_rect(
    data = cards,
    aes(xmin = 36, xmax = 96, ymin = ymax - 3, ymax = ymax, fill = acento),
    colour = NA
  ) +
  geom_text(
    data = cards,
    aes(x = 41, y = ymax - 9, label = titulo, colour = acento),
    hjust = 0, size = 3.75, fontface = "bold"
  ) +
  geom_segment(
    x = 57, xend = 57, y = 58, yend = 75,
    colour = pal$gris_claro, linewidth = .7
  ) +
  geom_segment(
    x = 76, xend = 76, y = 58, yend = 75,
    colour = pal$gris_claro, linewidth = .7
  ) +
  geom_segment(
    x = 57, xend = 57, y = 16, yend = 33,
    colour = pal$gris_claro, linewidth = .7
  ) +
  geom_segment(
    x = 76, xend = 76, y = 16, yend = 33,
    colour = pal$gris_claro, linewidth = .7
  ) +
  geom_text(
    data = details,
    aes(x = x, y = y + 3.2, label = label),
    colour = pal$tinta, size = 3.15, fontface = "bold"
  ) +
  geom_text(
    data = details,
    aes(x = x, y = y - 2, label = detail),
    colour = "#5A5752", size = 3.0, lineheight = .92
  ) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(8, 92), expand = c(0, 0)) +
  coord_fixed(clip = "off") +
  labs(
    title = "Decidir ahora o esperar",
    subtitle = "Dos rutas con costos distintos · esquema conceptual, no una estimación",
    caption = "La decisión depende de incertidumbre, tiempo y reversibilidad"
  ) +
  theme_kierkegaard()

export_plot(p_decision, figure_dir, "01_arbol_decision_compromiso", 9.4, 7.9)

write_csv(
  bind_rows(
    tibble(
      id = "00_ilustracion_decision_costo_oportunidad",
      pregunta = "¿Qué representa visualmente una bifurcación?",
      familia = "Ilustración editorial",
      fuente = "Activo editorial del módulo",
      advertencia = "No es evidencia empírica"
    ),
    tibble(
      id = "01_arbol_decision_compromiso",
      pregunta = "¿Qué cambia entre decidir ahora y esperar?",
      familia = "Rutas comparables",
      fuente = "Esquema editorial",
      advertencia = "No es una estimación empírica"
    )
  ),
  file.path(module_dir, "chart-map.csv")
)

message("Visuales de Kierkegaard actualizados en: ", figure_dir)
