# TEMA GRÁFICOS UNIVERSAL - LEONARDO MENA

library(ggplot2)
library(scales)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# PALETA DE COLORES

pal <- list(

  terracota  = "#c86448",
  plomo      = "#343a40",
  oliva      = "#6b7554",
  ocre       = "#d4ac0d",

  texto      = "#1a1512",
  texto_soft = "#4a443e",
  texto_muted= "#73695f",

  crema      = "#faf8f3",
  blanco     = "#ffffff",
  gris_claro = "#f5f1e8",

  border     = "#e6dfd5",
  border_dark= "#d4c9ba",

  verde      = "#2e7d32",
  rojo       = "#c62828"
)

# grid: "x", "y", "both" o "n"; no modifica los datos del gráfico.

theme_lm <- function(grid = "y") {
  theme_minimal(base_size = 13) +
    theme(

      text = element_text(color = pal$texto, family = "sans"),

      plot.title = element_text(
        size = 18, face = "bold", color = pal$texto,
        hjust = 0, margin = margin(b = 8)
      ),

      plot.subtitle = element_text(
        size = 13, color = pal$texto_soft,
        hjust = 0, margin = margin(b = 15)
      ),

      plot.caption = element_text(
        size = 9, color = pal$texto_muted,
        hjust = 1, margin = margin(t = 12)
      ),

      axis.title = element_text(size = 11, color = pal$texto_soft),
      axis.text = element_text(size = 10, color = pal$texto_muted),
      axis.line = element_line(color = pal$border_dark, linewidth = 0.4),
      axis.ticks = element_line(color = pal$border, linewidth = 0.3),

      panel.grid.major.y = if(grid %in% c("y", "both")) {
        element_line(color = pal$border, linewidth = 0.3, linetype = "dotted")
      } else element_blank(),

      panel.grid.major.x = if(grid %in% c("x", "both")) {
        element_line(color = pal$border, linewidth = 0.3, linetype = "dotted")
      } else element_blank(),

      panel.grid.minor = element_blank(),

      plot.background = element_rect(fill = pal$crema, color = NA),
      panel.background = element_rect(
        fill = pal$blanco,
        color = pal$border_dark,
        linewidth = 0.4
      ),

      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(size = 10, color = pal$texto_soft),
      legend.text = element_text(size = 9, color = pal$texto_muted),
      legend.key = element_blank(),
      legend.background = element_blank(),

      strip.background = element_rect(fill = pal$gris_claro, color = pal$border_dark),
      strip.text = element_text(size = 11, face = "bold", color = pal$texto),

      plot.margin = margin(15, 15, 15, 15)
    )
}

# Anotaciones

# Línea de referencia (ej: promedio, meta puntual)
linea_ref <- function(y, label = NULL, color = NULL) {
  color <- color %||% pal$terracota

  list(
    geom_hline(yintercept = y, linetype = "dashed",
               color = color, linewidth = 0.5, alpha = 0.7),
    if(!is.null(label)) {
      annotate("text",
        x = -Inf, y = y, label = label,
        hjust = -0.1, vjust = -0.4, size = 3, color = color, fontface = "bold"
      )
    }
  )
}

# Evento vertical (ej: COVID-19, elecciones)
evento <- function(x, label, color = NULL) {
  color <- color %||% pal$terracota

  list(
    geom_vline(xintercept = x, linetype = "dashed",
               color = color, linewidth = 0.5, alpha = 0.6),
    annotate("text",
      x = x, y = Inf, label = label,
      hjust = -0.1, vjust = 1.2, size = 3, color = color, fontface = "bold"
    )
  )
}

# FORMATEADORES

fmt_pct <- function(x, d = 1) paste0(format(round(x, d), nsmall = d), "%")

# Escala y tema para mapas

# 1. Escala Continua Personalizada (Gradiente Flexible)

scale_fill_lm_cont <- function(reverse = FALSE, values = NULL, ...) {  mis_colores <- c(pal$oliva, pal$gris_claro, pal$ocre, pal$terracota)

  if (reverse) {
    mis_colores <- rev(mis_colores)
  }  if (is.null(values)) {
    values <- scales::rescale(c(0, 0.5, 1))
  }  scale_fill_gradientn(
    colors = mis_colores,
    values = values,
    guide = guide_colorbar(
      barwidth = 12,
      barheight = 0.4,
      title.position = "top",
      frame.colour = pal$border_dark,
      ticks.colour = pal$border_dark
    ),
    ...
  )
}

# 2. Tema Específico para Mapas
# Hereda theme_lm() pero limpia ejes, coordenadas y bordes innecesarios.
# Uso: + theme_lm_map()

theme_lm_map <- function() {
  theme_lm(grid = "n") +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      legend.margin = margin(b = 10)
    )
}
