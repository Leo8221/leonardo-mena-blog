# ==============================================================================
# TEMA GRÁFICOS UNIVERSAL - LEONARDO MENA
# ==============================================================================

library(ggplot2)
library(scales)

# ------------------------------------------------------------------------------
# PALETA DE COLORES
# ------------------------------------------------------------------------------

pal <- list(
  # Principales
  terracota  = "#c86448",
  plomo      = "#343a40",
  oliva      = "#6b7554",
  ocre       = "#d4ac0d",
  
  # Texto
  texto      = "#1a1512",
  texto_soft = "#4a443e",
  texto_muted= "#73695f",
  
  # Backgrounds
  crema      = "#faf8f3",
  blanco     = "#ffffff",
  gris_claro = "#f5f1e8",
  
  # Bordes
  border     = "#e6dfd5",
  border_dark= "#d4c9ba",
  
  # Funcionales
  verde      = "#2e7d32",
  rojo       = "#c62828"
)

# Vectores para múltiples series
colores_primarios <- c(pal$plomo, pal$terracota, pal$oliva, pal$ocre)
colores_divergente <- c(pal$rojo, pal$gris_claro, pal$verde)

# ------------------------------------------------------------------------------
# TEMA BASE UNIVERSAL
# ------------------------------------------------------------------------------

theme_lm <- function(grid = "y") {
  theme_minimal(base_size = 13) +
    theme(
      # Tipografía
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
      
      # Ejes
      axis.title = element_text(size = 11, color = pal$texto_soft),
      axis.text = element_text(size = 10, color = pal$texto_muted),
      axis.line = element_line(color = pal$border_dark, linewidth = 0.4),
      axis.ticks = element_line(color = pal$border, linewidth = 0.3),
      
      # Grilla (adaptable)
      panel.grid.major.y = if(grid %in% c("y", "both")) {
        element_line(color = pal$border, linewidth = 0.3, linetype = "dotted")
      } else element_blank(),
      
      panel.grid.major.x = if(grid %in% c("x", "both")) {
        element_line(color = pal$border, linewidth = 0.3, linetype = "dotted")
      } else element_blank(),
      
      panel.grid.minor = element_blank(),
      
      # Fondo
      plot.background = element_rect(fill = pal$crema, color = NA),
      panel.background = element_rect(
        fill = pal$blanco, 
        color = pal$border_dark, 
        linewidth = 0.4
      ),
      
      # Leyenda
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(size = 10, color = pal$texto_soft),
      legend.text = element_text(size = 9, color = pal$texto_muted),
      legend.key = element_blank(),
      legend.background = element_blank(),
      
      # Facets
      strip.background = element_rect(fill = pal$gris_claro, color = pal$border_dark),
      strip.text = element_text(size = 11, face = "bold", color = pal$texto),
      
      # Márgenes
      plot.margin = margin(15, 15, 15, 15)
    )
}

# ------------------------------------------------------------------------------
# ESCALAS DE COLOR
# ------------------------------------------------------------------------------

scale_color_lm <- function(...) {
  scale_color_manual(values = colores_primarios, ...)
}

scale_fill_lm <- function(...) {
  scale_fill_manual(values = colores_primarios, ...)
}

# ------------------------------------------------------------------------------
# HELPERS PARA ANOTACIONES
# ------------------------------------------------------------------------------

# Banda de referencia (ej: meta de inflación 3-5%)
banda_meta <- function(ymin, ymax, label = NULL) {
  list(
    annotate("rect", 
      xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax,
      fill = pal$gris_claro, alpha = 0.5
    ),
    if(!is.null(label)) {
      annotate("text", 
        x = Inf, y = (ymin + ymax)/2, label = label,
        hjust = 1.1, size = 3.2, color = pal$texto_muted, fontface = "italic"
      )
    }
  )
}

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

# ------------------------------------------------------------------------------
# FORMATEADORES
# ------------------------------------------------------------------------------

fmt_pct <- function(x, d = 1) paste0(format(round(x, d), nsmall = d), "%")
fmt_rd <- function(x, d = 0) paste0("RD$ ", format(round(x, d), big.mark = ","))
fmt_num <- function(x, d = 0) format(round(x, d), big.mark = ",")

# ------------------------------------------------------------------------------
# EJEMPLO DE USO COMPLETO
# ------------------------------------------------------------------------------

# Descomentar para probar:
# library(lubridate)
# 
# datos <- data.frame(
#   fecha = seq.Date(as.Date("2020-01-01"), as.Date("2025-12-01"), by = "month"),
#   inflacion = c(3.2, 3.5, 2.8, 2.1, 2.5, 3.8, 4.5, 5.2, 6.8, 8.5, 9.2, 8.7,
#                 7.5, 6.8, 5.9, 5.2, 4.8, 4.3, 4.1, 3.9, 3.8, 4.2, 4.5, 4.3,
#                 4.1, 3.8, 3.6, 3.9, 4.2, 4.4, 4.6, 4.8, 4.5, 4.3, 4.1, 3.9,
#                 3.7, 4.0, 4.3, 4.5, 4.7, 4.9, 4.6, 4.4, 4.2, 4.0, 3.8, 4.1,
#                 4.4, 4.6, 4.8, 5.0, 4.7, 4.5, 4.3, 4.1, 3.9, 4.2, 4.5, 4.7,
#                 4.9, 5.1, 4.8, 4.6, 4.4, 4.2, 4.0, 4.3, 4.6, 4.8, 5.0, 5.2)
# )
# 
# ggplot(datos, aes(fecha, inflacion)) +
#   banda_meta(3, 5, "Meta 3-5%") +
#   geom_line(color = pal$plomo, linewidth = 1.1) +
#   geom_point(color = pal$terracota, size = 1.8) +
#   evento(as.Date("2020-03-01"), "COVID-19", pal$rojo) +
#   scale_y_continuous(labels = fmt_pct, breaks = seq(0, 10, 2)) +
#   scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
#   labs(
#     title = "Inflación Interanual",
#     subtitle = "República Dominicana 2020-2025",
#     caption = "Fuente: BCRD | Elaboración: Leonardo Mena",
#     x = NULL, y = NULL
#   ) +
#   theme_lm()

# ==============================================================================
# GUÍA RÁPIDA:
#
# 1. Agregar al inicio de cada post: source("tema_graficos.R")
# 2. Usar theme_lm() al final de cualquier ggplot
# 3. Helpers disponibles:
#    - banda_meta(min, max, "label")
#    - linea_ref(y, "label")
#    - evento(x, "label")
# 4. Formateadores:
#    - fmt_pct() para porcentajes
#    - fmt_rd() para pesos
#    - fmt_num() para números grandes
# 5. Colores directos: pal$terracota, pal$plomo, etc.
# ==============================================================================