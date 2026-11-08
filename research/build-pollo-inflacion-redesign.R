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
  oliva = "#76825A",
  gris = "#8E8B84",
  gris_claro = "#DED9D0",
  blanco = "#FFFDFC"
)

theme_inflacion <- function(grid = "none") {
  theme_minimal(base_size = 12, base_family = "Arial") +
    theme(
      text = element_text(colour = pal$tinta),
      plot.background = element_rect(fill = pal$crema, colour = NA),
      panel.background = element_rect(fill = pal$blanco, colour = NA),
      plot.title = element_text(face = "bold", size = 18, hjust = 0, margin = margin(b = 6)),
      plot.subtitle = element_text(size = 11, colour = "#5A5A55", hjust = 0, margin = margin(b = 15)),
      plot.caption = element_text(size = 8.5, colour = "#65655F", hjust = 0, margin = margin(t = 13)),
      axis.title = element_text(size = 10.5, colour = "#5A5A55"),
      axis.text = element_text(size = 9.5, colour = "#5A5A55"),
      axis.line = element_line(colour = pal$gris_claro, linewidth = .4),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = if (grid %in% c("x", "both")) element_line(colour = "#ECE7DE", linewidth = .4) else element_blank(),
      panel.grid.major.y = if (grid %in% c("y", "both")) element_line(colour = "#ECE7DE", linewidth = .4) else element_blank(),
      legend.position = "none",
      plot.margin = margin(18, 24, 16, 18)
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

assert_true <- function(x, message) if (!isTRUE(x)) stop(message, call. = FALSE)
label_month <- function(x) {
  month_names <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
  paste0(month_names[as.integer(format(x, "%m"))], "\n", format(x, "%y"))
}

module_dir <- file.path(repo_root, "research", "pollo-inflacion")
data_dir <- file.path(module_dir, "data", "procesados")
figure_dir <- file.path(module_dir, "figuras")

paths <- file.path(data_dir, c(
  "01_descomposicion_ipc.csv",
  "03_caso_representativo_alimentos.csv",
  "04_general_vs_subyacente.csv",
  "05_puente_junio_2026.csv",
  "06_dolar_vs_transables.csv"
))
assert_true(all(file.exists(paths)), "Faltan datos procesados del módulo de inflación.")

# 1. Panorama compacto: seis familias que sí caben en lectura móvil.
decomp <- read_csv(paths[[1]], show_col_types = FALSE) |>
  transmute(
    fecha = as.Date(fecha),
    componente = as.character(componente),
    contribucion_pp = as.numeric(contribucion_pp),
    inflacion_total = as.numeric(inflacion_total)
  ) |>
  arrange(fecha, componente)

assert_true(!anyNA(decomp), "La descomposición contiene valores faltantes.")
assert_true(!anyDuplicated(decomp[c("fecha", "componente")]), "La descomposición contiene celdas duplicadas.")
assert_true(n_distinct(decomp$componente) == 6, "La descomposición no contiene las seis familias esperadas.")

component_order <- c(
  "Alimentos y bebidas", "Transporte", "Bienes y servicios diversos",
  "Restaurantes y hoteles", "Vivienda", "Resto"
)
decomp <- decomp |>
  mutate(
    componente = factor(componente, levels = rev(component_order)),
    cell_label = if_else(abs(contribucion_pp) >= .045, sprintf("%.2f", contribucion_pp), ""),
    label_colour = if_else(abs(contribucion_pp) >= .14, pal$blanco, pal$tinta)
  )

heat_limit <- max(abs(decomp$contribucion_pp))
p_heat <- ggplot(decomp, aes(fecha, componente, fill = contribucion_pp)) +
  geom_tile(colour = pal$crema, linewidth = .7) +
  geom_text(aes(label = cell_label, colour = label_colour), size = 2.65, fontface = "bold") +
  scale_colour_identity() +
  scale_fill_gradient2(
    low = pal$azul, mid = pal$blanco, high = pal$terracota,
    midpoint = 0, limits = c(-heat_limit, heat_limit),
    labels = label_number(accuracy = .01), name = "p.p."
  ) +
  scale_x_date(
    breaks = seq(min(decomp$fecha), max(decomp$fecha), by = "2 months"),
    labels = label_month, expand = expansion(add = 15)
  ) +
  labs(
    title = "Qué grupos sumaron o restaron cada mes",
    subtitle = "Incidencia aproximada en puntos porcentuales · ene. 2025–jun. 2026",
    x = NULL, y = NULL,
    caption = "Solo se rotulan incidencias de al menos 0.045 p.p. · azul resta; terracota suma. Fuente: BCRD."
  ) +
  theme_inflacion("none") +
  theme(
    panel.background = element_rect(fill = pal$crema, colour = NA),
    axis.text.y = element_text(size = 10, face = "bold", colour = pal$tinta),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = grid::unit(36, "pt"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8.5)
  ) +
  guides(fill = guide_colourbar(title.position = "top", barwidth = grid::unit(150, "pt"), barheight = grid::unit(7, "pt")))

export_plot(p_heat, figure_dir, "02_heatmap_contribuciones_ipc", 10.4, 6.3)

# 2. El componente más representativo: alimentos y bebidas.
food <- read_csv(paths[[2]], show_col_types = FALSE) |>
  transmute(
    fecha = as.Date(fecha),
    variacion_mensual = as.numeric(variacion_mensual),
    peso_grupo = as.numeric(ponderacion),
    contribucion_pp = as.numeric(contribucion_pp),
    inflacion_total = as.numeric(inflacion_total)
  ) |>
  arrange(fecha)

assert_true(nrow(food) >= 18 && !anyNA(food), "La serie de alimentos y bebidas está incompleta.")
assert_true(!anyDuplicated(food$fecha), "La serie de alimentos y bebidas contiene meses duplicados.")
assert_true(max(abs(food$contribucion_pp - food$variacion_mensual * food$peso_grupo / 100)) < 1e-10,
            "La incidencia de alimentos no coincide con variación × peso.")

latest_food <- slice_tail(food, n = 1)
assert_true(latest_food$fecha == as.Date("2026-06-01"), "La serie de alimentos no termina en junio de 2026.")

component_stats <- decomp |>
  mutate(componente = as.character(componente)) |>
  group_by(componente) |>
  summarise(
    incidencia_acumulada = sum(contribucion_pp),
    incidencia_media_abs = mean(abs(contribucion_pp)),
    .groups = "drop"
  )
monthly_leaders <- decomp |>
  mutate(componente = as.character(componente)) |>
  group_by(fecha) |>
  slice_max(abs(contribucion_pp), n = 1, with_ties = FALSE) |>
  ungroup()
food_stats <- component_stats |> filter(componente == "Alimentos y bebidas")
food_lead_months <- sum(monthly_leaders$componente == "Alimentos y bebidas")
assert_true(food_stats$incidencia_media_abs == max(component_stats$incidencia_media_abs),
            "Alimentos dejó de ser el grupo con mayor incidencia absoluta media.")
assert_true(food_lead_months == max(table(monthly_leaders$componente)),
            "Alimentos dejó de ser el grupo que lidera más meses.")

timeline <- food |>
  mutate(
    x = seq(5, 95, length.out = n()),
    baseline = 18,
    tip = baseline + 10 * contribucion_pp / max(abs(contribucion_pp)),
    bar_colour = if_else(contribucion_pp >= 0, pal$terracota, pal$azul)
  )

timeline_ticks <- timeline |>
  filter(fecha %in% as.Date(c("2025-01-01", "2025-07-01", "2026-01-01", "2026-06-01"))) |>
  mutate(label = c("ene. 25", "jul. 25", "ene. 26", "jun. 26"))

formula_cards <- tibble(
  xmin = c(3, 37, 71), xmax = c(29, 63, 97),
  ymin = 55, ymax = 89,
  fill = c(pal$azul_claro, "#E8E3D9", pal$terracota),
  title = c("PRECIO DE ALIMENTOS", "PESO EN LA CANASTA", "INCIDENCIA EN EL IPC"),
  value = c(
    sprintf("%+.2f%%", latest_food$variacion_mensual),
    sprintf("%.2f%%", latest_food$peso_grupo),
    sprintf("%+.3f p.p.", latest_food$contribucion_pp)
  ),
  detail = c("variación mensual", "ponderación fija", "aporte mensual"),
  text_colour = c(pal$tinta, pal$tinta, pal$blanco)
)

p_chicken <- ggplot() +
  geom_rect(
    data = formula_cards,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
    colour = NA, inherit.aes = FALSE
  ) +
  scale_fill_identity() +
  geom_text(
    data = formula_cards,
    aes(x = (xmin + xmax) / 2, y = 82, label = title, colour = text_colour),
    size = 2.9, fontface = "bold", inherit.aes = FALSE
  ) +
  geom_text(
    data = formula_cards,
    aes(x = (xmin + xmax) / 2, y = 71.5, label = value, colour = text_colour),
    size = 6, fontface = "bold", inherit.aes = FALSE
  ) +
  geom_text(
    data = formula_cards,
    aes(x = (xmin + xmax) / 2, y = 62, label = detail, colour = text_colour),
    size = 3.1, inherit.aes = FALSE
  ) +
  scale_colour_identity() +
  annotate("text", x = 33, y = 71.5, label = "×", size = 8, colour = pal$tinta) +
  annotate("text", x = 67, y = 71.5, label = "=", size = 8, colour = pal$tinta) +
  annotate("text", x = 3, y = 34, label = "La misma fórmula, mes a mes", hjust = 0, size = 3.6, fontface = "bold", colour = pal$tinta) +
  annotate("segment", x = 5, xend = 95, y = 18, yend = 18, linewidth = .55, colour = pal$gris) +
  geom_segment(
    data = timeline,
    aes(x = x, xend = x, y = baseline, yend = tip, colour = bar_colour),
    linewidth = 4.2, lineend = "butt", inherit.aes = FALSE
  ) +
  geom_text(
    data = timeline_ticks,
    aes(x = x, y = 4.7, label = label),
    size = 2.7, colour = "#65655F", inherit.aes = FALSE
  ) +
  annotate(
    "text", x = 97, y = 26.5,
    label = sprintf("junio\n%+.3f p.p.", latest_food$contribucion_pp),
    hjust = 1, size = 3, fontface = "bold", colour = pal$terracota
  ) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), clip = "off") +
  labs(
    title = "Cómo alimentos y bebidas entran al IPC",
    subtitle = sprintf(
      "El grupo más representativo: lideró %d de 18 meses · incidencia absoluta media %.3f p.p.",
      food_lead_months, food_stats$incidencia_media_abs
    ),
    caption = "Arriba: fórmula para junio de 2026. Abajo: incidencia mensual desde enero de 2025. Fuente: BCRD."
  ) +
  theme_void(base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = pal$crema, colour = NA),
    plot.title = element_text(face = "bold", size = 18, colour = pal$tinta, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11, colour = "#5A5A55", margin = margin(b = 12)),
    plot.caption = element_text(size = 8.5, colour = "#65655F", hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(18, 20, 15, 18)
  )

export_plot(p_chicken, figure_dir, "03_caso_representativo_alimentos", 10.4, 7.1)

# 3. General frente a subyacente, con divergencia visible y rótulos directos.
core <- read_csv(paths[[3]], show_col_types = FALSE) |>
  transmute(
    fecha = as.Date(fecha),
    inflacion_general = as.numeric(inflacion_general),
    subyacente_mensual = as.numeric(subyacente_mensual)
  ) |>
  arrange(fecha)

assert_true(!anyDuplicated(core$fecha) && !anyNA(core), "La comparación general-subyacente está incompleta.")
core <- core |> mutate(brecha = inflacion_general - subyacente_mensual)
max_gap <- core |> slice_max(abs(brecha), n = 1, with_ties = FALSE)
end_core <- core |> slice_tail(n = 1)

p_core <- ggplot(core, aes(fecha)) +
  geom_hline(yintercept = 0, colour = pal$gris, linewidth = .5) +
  geom_ribbon(
    aes(ymin = pmin(inflacion_general, subyacente_mensual), ymax = pmax(inflacion_general, subyacente_mensual)),
    fill = pal$terracota_clara, alpha = .55, colour = NA
  ) +
  geom_line(aes(y = inflacion_general), colour = pal$terracota, linewidth = 1.25) +
  geom_line(aes(y = subyacente_mensual), colour = pal$azul, linewidth = 1.15, linetype = "22") +
  geom_segment(
    data = max_gap,
    aes(x = fecha, xend = fecha, y = inflacion_general, yend = subyacente_mensual),
    colour = pal$tinta, linewidth = .65, inherit.aes = FALSE
  ) +
  geom_point(data = max_gap, aes(fecha, inflacion_general), colour = pal$terracota, size = 2.5, inherit.aes = FALSE) +
  geom_point(data = max_gap, aes(fecha, subyacente_mensual), colour = pal$azul, size = 2.5, inherit.aes = FALSE) +
  annotate(
    "text", x = max_gap$fecha + 18,
    y = mean(c(max_gap$inflacion_general, max_gap$subyacente_mensual)),
    label = sprintf("brecha %.2f p.p.", abs(max_gap$brecha)),
    hjust = 0, size = 2.9, fontface = "bold", colour = pal$tinta
  ) +
  annotate(
    "text", x = end_core$fecha + 28, y = end_core$inflacion_general + .025,
    label = sprintf("General %.2f%%", end_core$inflacion_general),
    hjust = 0, size = 3.1, fontface = "bold", colour = pal$terracota
  ) +
  annotate(
    "text", x = end_core$fecha + 28, y = end_core$subyacente_mensual - .025,
    label = sprintf("Subyacente %.2f%%", end_core$subyacente_mensual),
    hjust = 0, vjust = 1, size = 3.1, fontface = "bold", colour = pal$azul
  ) +
  scale_x_date(
    date_breaks = "3 months", labels = label_month,
    limits = c(min(core$fecha), max(core$fecha) + 205), expand = c(0, 0)
  ) +
  scale_y_continuous(labels = label_number(accuracy = .1, suffix = "%"), expand = expansion(mult = c(.08, .12))) +
  labs(
    title = "General y subyacente: el ruido alrededor del núcleo",
    subtitle = "Variación mensual · el área marca la distancia entre ambas medidas",
    x = NULL, y = NULL,
    caption = "La subyacente excluye artículos volátiles y regulados. Fuente: BCRD."
  ) +
  theme_inflacion("y")

export_plot(p_core, figure_dir, "04_general_vs_subyacente", 10.4, 6.4)

# 4. Puente horizontal: cada incidencia comienza donde terminó la anterior.
bridge_raw <- read_csv(paths[[4]], show_col_types = FALSE) |>
  transmute(contribucion_pp = as.numeric(contribucion_pp))

assert_true(nrow(bridge_raw) == 7 && !anyNA(bridge_raw), "El puente de junio no contiene siete partidas válidas.")
assert_true(abs(sum(bridge_raw$contribucion_pp) - .51) < 1e-10, "Las incidencias de junio no cierran en 0.51.")

bridge_labels <- c(
  "Transporte", "Alimentos y bebidas", "Bienes y servicios diversos",
  "Restaurantes y hoteles", "Muebles y artículos del hogar",
  "Resto de grupos", "Ajuste técnico"
)
bridge <- bridge_raw |>
  mutate(
    grupo = bridge_labels,
    start = lag(cumsum(contribucion_pp), default = 0),
    end = cumsum(contribucion_pp),
    row = rev(seq_len(n())),
    kind = if_else(grupo == "Ajuste técnico", "Ajuste técnico", "Incidencia")
  )
bridge_total <- tibble(
  grupo = "TOTAL OFICIAL", start = 0, end = .51, row = 0,
  contribucion_pp = .51, kind = "Total"
)

p_bridge <- ggplot() +
  geom_segment(
    data = bridge |> slice_head(n = nrow(bridge) - 1),
    aes(x = end, xend = end, y = row - .32, yend = row - .68),
    colour = pal$gris, linewidth = .55, inherit.aes = FALSE
  ) +
  geom_rect(
    data = bridge,
    aes(xmin = start, xmax = end, ymin = row - .31, ymax = row + .31, fill = kind),
    colour = pal$tinta, linewidth = .18, inherit.aes = FALSE
  ) +
  geom_rect(
    data = bridge_total,
    aes(xmin = start, xmax = end, ymin = row - .31, ymax = row + .31, fill = kind),
    colour = pal$tinta, linewidth = .18, inherit.aes = FALSE
  ) +
  geom_text(
    data = bridge,
    aes(x = (start + end) / 2, y = row, label = sprintf("+%.2f", contribucion_pp)),
    colour = pal$blanco, size = 2.8, fontface = "bold", inherit.aes = FALSE
  ) +
  annotate("text", x = .255, y = 0, label = "0.51%", colour = pal$blanco, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Incidencia" = pal$terracota, "Ajuste técnico" = pal$gris, "Total" = pal$tinta), guide = "none") +
  scale_x_continuous(
    limits = c(0, .54), breaks = seq(0, .5, .1),
    labels = label_number(accuracy = .1), expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = c(bridge$row, 0), labels = c(bridge$grupo, bridge_total$grupo),
    limits = c(-.65, 7.45), expand = c(0, 0)
  ) +
  labs(
    title = "Cómo se forma el 0.51% de junio",
    subtitle = "Cada barra empieza donde terminó la anterior · incidencias en puntos porcentuales",
    x = "Acumulado (p.p.)", y = NULL,
    caption = "El ajuste técnico cierra redondeos y enlaces; no es un componente económico. Fuente: BCRD."
  ) +
  theme_inflacion("x") +
  theme(axis.text.y = element_text(size = 9.5, colour = pal$tinta))

export_plot(p_bridge, figure_dir, "05_puente_junio_2026", 10.4, 6.7)

# 5. Dos carriles sincronizados: misma escala, sin sugerir causalidad.
dollar <- read_csv(paths[[5]], show_col_types = FALSE) |>
  transmute(
    fecha = as.Date(fecha),
    `Bienes transables` = as.numeric(transables_interanual),
    `Tipo de cambio` = as.numeric(dolar_interanual)
  ) |>
  arrange(fecha)

assert_true(!anyDuplicated(dollar$fecha) && !anyNA(dollar), "La comparación dólar-transables está incompleta.")
dollar_long <- dollar |>
  pivot_longer(-fecha, names_to = "serie", values_to = "variacion") |>
  mutate(
    serie = factor(serie, levels = c("Bienes transables", "Tipo de cambio")),
    colour = if_else(serie == "Bienes transables", pal$terracota, pal$azul)
  )
dollar_end <- dollar_long |> group_by(serie) |> slice_max(fecha, n = 1, with_ties = FALSE) |> ungroup()
lane_labels <- tibble(
  serie = factor(c("Bienes transables", "Tipo de cambio"), levels = levels(dollar_long$serie)),
  fecha = min(dollar$fecha) + 14,
  variacion = max(dollar_long$variacion) + .45,
  label = c("BIENES TRANSABLES", "TIPO DE CAMBIO"),
  colour = c(pal$terracota, pal$azul)
)

p_dollar <- ggplot(dollar_long, aes(fecha, variacion, group = serie)) +
  annotate(
    "rect", xmin = as.Date("2026-01-01"), xmax = as.Date("2026-05-31"),
    ymin = -Inf, ymax = Inf, fill = "#EEE8DE", colour = NA
  ) +
  geom_hline(yintercept = 0, colour = pal$gris, linewidth = .5) +
  geom_line(aes(colour = colour, linetype = serie), linewidth = 1.25) +
  geom_point(
    data = dollar_end, aes(x = fecha, y = variacion, colour = colour),
    size = 3, shape = 21, fill = pal$blanco, stroke = 1, inherit.aes = FALSE
  ) +
  geom_text(
    data = dollar_end,
    aes(x = fecha + 28, y = variacion, label = sprintf("%.1f%%", variacion), colour = colour),
    hjust = 0, size = 3.3, fontface = "bold", inherit.aes = FALSE
  ) +
  geom_text(
    data = lane_labels,
    aes(x = fecha, y = variacion, label = label, colour = colour),
    hjust = 0, size = 3.15, fontface = "bold", inherit.aes = FALSE
  ) +
  scale_colour_identity() +
  scale_linetype_manual(values = c("Bienes transables" = "solid", "Tipo de cambio" = "22"), guide = "none") +
  facet_grid(serie ~ .) +
  scale_x_date(
    date_breaks = "3 months", labels = label_month,
    limits = c(min(dollar$fecha), max(dollar$fecha) + 90), expand = c(0, 0)
  ) +
  scale_y_continuous(labels = label_number(accuracy = 1, suffix = "%"), expand = expansion(mult = c(.1, .14))) +
  labs(
    title = "Dólar y bienes transables, en carriles separados",
    subtitle = "Variación interanual · misma escala · la zona sombreada destaca 2026",
    x = NULL, y = NULL,
    caption = "Comparación descriptiva: la coincidencia temporal no identifica causalidad. Fuente: BCRD."
  ) +
  theme_inflacion("y") +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing.y = grid::unit(9, "pt")
  )

export_plot(p_dollar, figure_dir, "06_dolar_vs_transables", 10.4, 7.2)

write_csv(tibble(
  id = c(
    "02_heatmap_contribuciones_ipc", "03_caso_representativo_alimentos",
    "04_general_vs_subyacente", "05_puente_junio_2026", "06_dolar_vs_transables"
  ),
  pregunta = c(
    "¿Qué grupos suman o restan cada mes?",
    "¿Cuál es el grupo más representativo y cómo entra al IPC?",
    "¿Cuándo se separan la inflación general y la subyacente?",
    "¿Cómo cierran las incidencias en el 0.51% oficial de junio?",
    "¿El dólar y los bienes transables se mueven uno a uno?"
  ),
  familia = c(
    "Mapa de calor agrupado", "Fórmula visual con historial", "Líneas con brecha",
    "Puente acumulativo", "Carriles sincronizados"
  ),
  fuente = "BCRD IPC",
  advertencia = c(
    "Contribuciones aproximadas a partir de ponderaciones oficiales",
    "Alimentos lidera por incidencia absoluta media y frecuencia mensual",
    "La subyacente excluye artículos volátiles y regulados",
    "El ajuste técnico no es un componente económico",
    "Comparación descriptiva, no causal"
  )
), file.path(module_dir, "chart-map.csv"))

message("Cinco visuales revisados de componentes de inflación generados y validados.")
