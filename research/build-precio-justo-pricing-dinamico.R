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
  terracota_clara = "#E7C5B8",
  azul = "#4F789F",
  azul_claro = "#C8D6E2",
  gris = "#8E8B84",
  gris_claro = "#DED9D0",
  blanco = "#FFFDFC"
)

theme_price <- function() {
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

module_dir <- file.path(repo_root, "research", "precio-justo-pricing-dinamico")
figure_dir <- file.path(module_dir, "figuras")
illustration_path <- file.path(module_dir, "assets", "ilustracion-precio-justo-dinamico.png")

if (!file.exists(illustration_path)) stop("Falta la ilustración del precio justo.", call. = FALSE)

# Figura 1: dos pistas separadas para no mezclar unidades ni duplicar ejes.
dynamic <- tibble(
  hora = 1:12,
  demanda = c(35, 40, 44, 49, 55, 64, 77, 92, 88, 70, 52, 41)
) |>
  mutate(
    precio = 25 + demanda * .55,
    demanda_y = rescale(demanda, to = c(58, 84)),
    precio_y = rescale(precio, to = c(16, 38))
  )

p_dynamic <- ggplot(dynamic, aes(hora)) +
  annotate(
    "rect", xmin = .45, xmax = 12.55, ymin = 51, ymax = 89,
    fill = pal$blanco, colour = pal$gris_claro, linewidth = .7
  ) +
  annotate(
    "rect", xmin = .45, xmax = 12.55, ymin = 9, ymax = 45,
    fill = pal$blanco, colour = pal$gris_claro, linewidth = .7
  ) +
  geom_ribbon(
    aes(ymin = 55, ymax = demanda_y),
    fill = pal$terracota_clara, alpha = .72
  ) +
  geom_line(aes(y = demanda_y), colour = pal$terracota, linewidth = 1.35) +
  geom_point(
    aes(y = demanda_y), shape = 21, size = 2.4, stroke = .8,
    fill = pal$blanco, colour = pal$terracota
  ) +
  geom_step(
    aes(y = precio_y), colour = pal$azul,
    linewidth = 1.35, direction = "mid"
  ) +
  geom_point(
    aes(y = precio_y), shape = 22, size = 2.25, stroke = .8,
    fill = pal$blanco, colour = pal$azul
  ) +
  annotate(
    "segment", x = 8, xend = 8, y = 44, yend = 51,
    colour = pal$tinta, linewidth = .65,
    arrow = grid::arrow(length = grid::unit(0.11, "inches"), type = "closed")
  ) +
  annotate(
    "text", x = .8, y = 86.5, label = "DEMANDA",
    hjust = 0, colour = pal$terracota, size = 3.35, fontface = "bold"
  ) +
  annotate(
    "text", x = .8, y = 42.5, label = "PRECIO",
    hjust = 0, colour = pal$azul, size = 3.35, fontface = "bold"
  ) +
  annotate(
    "text", x = 8.18, y = 86.4, label = "pico de demanda",
    hjust = 0, vjust = .5, colour = pal$terracota, size = 3.0, fontface = "bold"
  ) +
  annotate(
    "text", x = 8.18, y = 40.3, label = "precio más alto",
    hjust = 0, vjust = .5, colour = pal$azul, size = 3.0, fontface = "bold"
  ) +
  annotate(
    "text", x = 8.2, y = 47.4, label = "misma hora",
    hjust = 0, colour = "#5A5A55", size = 2.7
  ) +
  annotate(
    "text", x = 1:12, y = 6.5, label = paste0("H", 1:12),
    colour = "#65655F", size = 2.8
  ) +
  scale_x_continuous(limits = c(.3, 12.7), expand = c(0, 0)) +
  scale_y_continuous(limits = c(3, 92), expand = c(0, 0)) +
  labs(
    title = "Demanda y respuesta del precio por hora",
    subtitle = "Dos pistas sincronizadas · cada una conserva su propia escala conceptual",
    caption = "Ejemplo ilustrativo; no son observaciones ni precios de una plataforma dominicana."
  ) +
  theme_price()

export_plot(p_dynamic, figure_dir, "01_demanda_precio_dinamico", 9.4, 7.2)

# Figura 2: la negociación puede acotar precios posibles, no uno moralmente único.
possible_prices <- tibble(
  x = c(38, 52, 68),
  label = c("precio A", "precio B", "precio C")
)

p_fair <- ggplot() +
  annotate(
    "rect", xmin = 3, xmax = 97, ymin = 16, ymax = 84,
    fill = pal$blanco, colour = pal$gris_claro, linewidth = .7
  ) +
  annotate(
    "rect", xmin = 24, xmax = 80, ymin = 42, ymax = 58,
    fill = "#ECE7DE", colour = NA
  ) +
  annotate(
    "segment", x = 8, xend = 92, y = 50, yend = 50,
    colour = pal$tinta, linewidth = .8, lineend = "butt"
  ) +
  annotate(
    "segment", x = 24, xend = 24, y = 35, yend = 68,
    colour = pal$terracota, linewidth = 1.5, lineend = "butt"
  ) +
  annotate(
    "segment", x = 80, xend = 80, y = 35, yend = 68,
    colour = pal$azul, linewidth = 1.5, lineend = "butt"
  ) +
  annotate(
    "text", x = 24, y = 75,
    label = "MÍNIMO QUE\nACEPTA EL VENDEDOR",
    colour = pal$terracota, size = 3.2, fontface = "bold", lineheight = .92
  ) +
  annotate(
    "text", x = 80, y = 75,
    label = "MÁXIMO QUE\nACEPTA EL COMPRADOR",
    colour = pal$azul, size = 3.2, fontface = "bold", lineheight = .92
  ) +
  annotate(
    "text", x = 52, y = 63.5, label = "INTERVALO DE ACUERDOS POSIBLES",
    colour = pal$tinta, size = 3.2, fontface = "bold"
  ) +
  geom_point(
    data = possible_prices, aes(x = x, y = 50),
    shape = 21, size = 5.2, stroke = 1,
    fill = pal$blanco, colour = pal$tinta
  ) +
  geom_text(
    data = possible_prices, aes(x = x, y = 36.5, label = label),
    colour = "#5A5A55", size = 3.0, fontface = "bold"
  ) +
  annotate(
    "text", x = 16, y = 27, label = "NO HAY ACUERDO",
    colour = pal$terracota, size = 2.75, fontface = "bold"
  ) +
  annotate(
    "text", x = 88, y = 27, label = "NO HAY ACUERDO",
    colour = pal$azul, size = 2.75, fontface = "bold"
  ) +
  annotate(
    "text", x = 52, y = 23,
    label = "El intervalo admite más de un precio; ninguno viene marcado como «el justo»",
    colour = pal$tinta, size = 3.15, fontface = "bold"
  ) +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(9, 90), expand = c(0, 0)) +
  labs(
    title = "No hay un punto único llamado «precio justo»",
    subtitle = "El intercambio puede delimitar precios posibles, pero no identifica uno como moralmente correcto",
    caption = "Esquema conceptual de intercambio bilateral; no representa precios observados ni todos los mercados."
  ) +
  theme_price()

export_plot(p_fair, figure_dir, "02_matriz_eficiencia_justicia", 9.4, 7.1)

write_csv(
  bind_rows(
    tibble(
      id = "00_ilustracion_precio_justo_dinamico",
      pregunta = "¿Qué tensión visual existe entre precio justo y precio dinámico?",
      familia = "Ilustración editorial",
      fuente = "Activo editorial del módulo",
      advertencia = "No es evidencia empírica"
    ),
    tibble(
      id = "01_demanda_precio_dinamico",
      pregunta = "¿Cómo cambia el precio cuando cambia la demanda?",
      familia = "Pistas sincronizadas",
      fuente = "Ejemplo ilustrativo",
      advertencia = "No son precios observados"
    ),
    tibble(
      id = "02_matriz_eficiencia_justicia",
      pregunta = "¿El intercambio identifica un precio moralmente justo?",
      familia = "Intervalo de negociación",
      fuente = "Esquema editorial",
      advertencia = "Intercambio bilateral conceptual; no representa todos los mercados"
    )
  ),
  file.path(module_dir, "chart-map.csv")
)

message("Visuales de precio justo actualizados en: ", figure_dir)
