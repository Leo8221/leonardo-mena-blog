#!/usr/bin/env Rscript

options(encoding = "UTF-8", scipen = 999)
if (!isTRUE(l10n_info()[["UTF-8"]])) stop("R no está leyendo UTF-8.", call. = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(readxl)
  library(scales)
  library(stringr)
  library(tidyr)
  library(svglite)
  library(jsonlite)
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
      panel.grid.major = if (grid == "y") element_line(colour = pal$gris_claro, linewidth = .35) else element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(colour = pal$tinta), axis.text = element_text(colour = pal$tinta),
      plot.title = element_text(face = "bold", size = 17, colour = pal$tinta),
      plot.subtitle = element_text(colour = "#5A5A55", size = 11),
      plot.caption = element_text(colour = "#65655F", size = 8.5, hjust = 0),
      legend.position = "top", legend.title = element_blank(),
      plot.margin = margin(14, 24, 14, 14)
    )
}

export_plot <- function(plot, dir, slug, width = 10, height = 6.5) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(dir, paste0(slug, ".svg")), plot, device = svglite::svglite, width = width, height = height, bg = pal$crema)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ggsave(file.path(dir, paste0(slug, ".png")), plot, device = ragg::agg_png, width = width, height = height, dpi = 320, bg = pal$crema)
  } else {
    ggsave(file.path(dir, paste0(slug, ".png")), plot, width = width, height = height, dpi = 320, bg = pal$crema)
  }
}

write_map <- function(dir, rows) write_csv(bind_rows(rows), file.path(dir, "chart-map.csv"))
assert_true <- function(x, msg) if (!isTRUE(x)) stop(msg, call. = FALSE)
month_num <- function(x) {
  key <- str_to_lower(iconv(str_squish(as.character(x)), from = "UTF-8", to = "ASCII//TRANSLIT"))
  unname(c(enero = 1, ene = 1, febrero = 2, feb = 2, marzo = 3, mar = 3, abril = 4, abr = 4, mayo = 5, may = 5, junio = 6, jun = 6, julio = 7, jul = 7, agosto = 8, ago = 8, septiembre = 9, sep = 9, sept = 9, octubre = 10, oct = 10, noviembre = 11, nov = 11, diciembre = 12, dic = 12)[key])
}
fill_year <- function(x) {
  out <- rep(NA_integer_, length(x)); current <- NA_integer_
  for (i in seq_along(x)) {
    candidate <- suppressWarnings(as.integer(as.character(x[[i]])))
    if (!is.na(candidate) && candidate >= 1900 && candidate <= 2100) current <- candidate
    out[[i]] <- current
  }
  out
}

# 1. La cuesta de agosto: datos oficiales del IPC por grupo.
dir.create(file.path(repo_root, "research", "cuesta-agosto", "data", "procesados"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(repo_root, "research", "cuesta-agosto", "figuras"), recursive = TRUE, showWarnings = FALSE)
ipc_path <- file.path(repo_root, "atlas", "data", "raw", "bcrd-precios", "ipc_articulos_base_2019-2020.xlsx")
raw_ipc <- read_excel(ipc_path, sheet = "2020-2026", col_names = FALSE, .name_repair = "minimal")
year_scores <- vapply(seq_len(nrow(raw_ipc)), function(i) sum(str_detect(as.character(unlist(raw_ipc[i, ], use.names = FALSE)), "^20[0-9]{2}$"), na.rm = TRUE), numeric(1))
month_scores <- vapply(seq_len(nrow(raw_ipc)), function(i) sum(!is.na(month_num(unlist(raw_ipc[i, ], use.names = FALSE)))), numeric(1))
year_row <- which.max(year_scores); month_row <- which.max(month_scores)
years <- fill_year(unlist(raw_ipc[year_row, ], use.names = FALSE)); months <- month_num(unlist(raw_ipc[month_row, ], use.names = FALSE))
cols <- which(!is.na(years) & !is.na(months))
group_rows <- which(str_detect(str_squish(as.character(raw_ipc[[1]])), "^[0-9]{2} "))
assert_true(length(group_rows) == 12, "El IPC por grupos no contiene 12 grupos.")
ipc_groups <- bind_rows(lapply(group_rows, function(row) tibble(
  grupo = str_remove(str_squish(as.character(raw_ipc[[1]][row])), "^[0-9]{2} "),
  anio = years[cols], mes = months[cols],
  fecha = as.Date(sprintf("%04d-%02d-01", years[cols], months[cols])),
  indice = suppressWarnings(as.numeric(unlist(raw_ipc[row, cols], use.names = FALSE)))
))) |> filter(!is.na(fecha), is.finite(indice))
assert_true(max(ipc_groups$fecha) >= as.Date("2026-06-01"), "El IPC no llega al corte oficial esperado.")

aug <- ipc_groups |>
  filter(anio >= 2020, anio <= 2025) |>
  group_by(grupo, anio) |>
  mutate(indice_rel_anual = 100 * indice / mean(indice, na.rm = TRUE)) |>
  ungroup() |>
  group_by(grupo, mes) |>
  summarise(indice_estacional = mean(indice_rel_anual, na.rm = TRUE), .groups = "drop")
write_csv(aug, file.path(repo_root, "research", "cuesta-agosto", "data", "procesados", "01_indice_estacional_ipc.csv"))
aug_order <- aug |> group_by(grupo) |> summarise(agosto = indice_estacional[mes == 8], .groups = "drop") |> arrange(agosto) |> pull(grupo)
aug_heat <- aug |> mutate(grupo = factor(grupo, levels = aug_order))
p_aug_heat <- ggplot(aug_heat, aes(mes, grupo, fill = indice_estacional)) +
  geom_tile(colour = pal$crema, linewidth = .35) +
  scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"), expand = c(0, 0)) +
  scale_fill_gradient2(low = pal$azul, mid = pal$crema, high = pal$terracota, midpoint = 100, name = "Índice") +
  labs(title = "La presión de precios cambia según el mes", subtitle = "Índice estacional del IPC por grupo · promedio 2020–2025 = 100", x = NULL, y = NULL, caption = "Fuente: BCRD, IPC por grupos · cada grupo se normaliza contra su propio promedio anual") +
  theme_editorial("none") + theme(axis.text.y = element_text(size = 8.5))
export_plot(p_aug_heat, file.path(repo_root, "research", "cuesta-agosto", "figuras"), "01_indice_estacional_ipc", 11, 7.7)
p_aug_bar <- aug |> filter(mes == 8) |> arrange(indice_estacional) |> mutate(grupo = factor(grupo, levels = grupo), desviacion_pct = indice_estacional - 100) |>
  ggplot(aes(desviacion_pct, grupo)) + geom_segment(aes(x = 0, xend = desviacion_pct, y = grupo, yend = grupo), colour = pal$terracota, linewidth = 5, lineend = "butt") +
  geom_vline(xintercept = 0, colour = pal$gris, linetype = "dashed") + geom_text(aes(label = sprintf("%+.1f%%", desviacion_pct)), hjust = -.12, size = 3.1, fontface = "bold") +
  scale_x_continuous(labels = function(x) sprintf("%+.1f%%", x), limits = c(min(aug$indice_estacional) - 100 - .15, max(aug$indice_estacional) - 100 + .55), expand = c(0, 0)) +
  labs(title = "Agosto no presiona todos los rubros por igual", subtitle = "Desviación del índice de agosto frente al promedio anual del mismo grupo · 2020–2025", x = "Diferencia frente al promedio anual", y = NULL, caption = "Fuente: BCRD, IPC por grupos · 0% equivale al promedio anual del grupo") + theme_editorial("x")
export_plot(p_aug_bar, file.path(repo_root, "research", "cuesta-agosto", "figuras"), "02_agosto_por_grupo", 10.5, 7.4)
write_map(file.path(repo_root, "research", "cuesta-agosto"), list(
  tibble(id = "01_indice_estacional_ipc", pregunta = "¿La presión de precios tiene un patrón mensual?", familia = "Mapa de calor estacional", fuente = "BCRD IPC", advertencia = "Índice descriptivo, no causal"),
  tibble(id = "02_agosto_por_grupo", pregunta = "¿Qué grupos se encarecen relativamente en agosto?", familia = "Barras ordenadas", fuente = "BCRD IPC", advertencia = "Promedio 2020–2025")
))

# 2. Epicteto: fondo de emergencia sin moralizar la restricción material.
dir.create(file.path(repo_root, "research", "epicteto-fondo-emergencia", "data", "procesados"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(repo_root, "research", "epicteto-fondo-emergencia", "figuras"), recursive = TRUE, showWarnings = FALSE)
enief <- tribble(
  ~estrato, ~respuesta, ~porcentaje,
  "Ingresos RD$0–31,200", "Sus ahorros", 31.0, "Ingresos RD$31,201 o más", "Sus ahorros", 49.3,
  "Ingresos RD$0–31,200", "Adelanto de sueldo", 9.2, "Ingresos RD$31,201 o más", "Adelanto de sueldo", 3.0,
  "Ingresos RD$0–31,200", "Venta o empeño de un bien", 5.3, "Ingresos RD$31,201 o más", "Venta o empeño de un bien", 2.9,
  "Ingresos RD$0–31,200", "Familia, amigos o conocidos", 28.4, "Ingresos RD$31,201 o más", "Familia, amigos o conocidos", 13.9,
  "Ingresos RD$0–31,200", "Tarjeta de crédito", 3.0, "Ingresos RD$31,201 o más", "Tarjeta de crédito", 12.4,
  "Ingresos RD$0–31,200", "Préstamo financiero", 9.0, "Ingresos RD$31,201 o más", "Préstamo financiero", 11.5,
  "Ingresos RD$0–31,200", "Trabajo temporal o venta", 2.2, "Ingresos RD$31,201 o más", "Trabajo temporal o venta", 1.1,
  "Ingresos RD$0–31,200", "Remesas", 3.7, "Ingresos RD$31,201 o más", "Remesas", 3.0,
  "Ingresos RD$0–31,200", "No podría cubrirla", 5.4, "Ingresos RD$31,201 o más", "No podría cubrirla", 0.3,
  "Ingresos RD$0–31,200", "Otro", 2.8, "Ingresos RD$31,201 o más", "Otro", 2.7
)
write_csv(enief, file.path(repo_root, "research", "epicteto-fondo-emergencia", "data", "procesados", "01_enief_emergencia_2023.csv"))
enief_order <- enief |> filter(estrato == "Ingresos RD$0–31,200") |> arrange(porcentaje) |> pull(respuesta)
p_enief <- enief |> mutate(respuesta = factor(respuesta, levels = enief_order)) |>
  ggplot(aes(porcentaje, respuesta, fill = estrato)) + geom_col(position = position_dodge(width = .78), width = .68, colour = pal$tinta, linewidth = .2) +
  geom_text(aes(label = sprintf("%.1f%%", porcentaje)), position = position_dodge(width = .78), hjust = -.12, size = 2.7, fontface = "bold") +
  scale_fill_manual(values = c("Ingresos RD$0–31,200" = pal$terracota, "Ingresos RD$31,201 o más" = pal$azul)) +
  scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, 58), expand = c(0, 0)) +
  labs(title = "Una emergencia no se financia igual en todos los hogares", subtitle = "Primera respuesta ante una emergencia financiera por grupo de ingresos · ENIEF 2023", x = NULL, y = NULL, caption = "Fuente: BCRD, Encuesta Nacional de Inclusión y Educación Financiera 2023 · porcentajes sin no sabe/no contesta") +
  theme_editorial("x") + theme(axis.text.y = element_text(size = 9))
export_plot(p_enief, file.path(repo_root, "research", "epicteto-fondo-emergencia", "figuras"), "01_respuesta_emergencia_ingreso", 11, 7.2)
write_map(file.path(repo_root, "research", "epicteto-fondo-emergencia"), list(tibble(id = "01_respuesta_emergencia_ingreso", pregunta = "¿Cómo cubren una emergencia los hogares?", familia = "Barras agrupadas", fuente = "BCRD ENIEF 2023", advertencia = "Primera respuesta declarada; no mide saldo de ahorro")))

# 3. Visuales conceptuales: no son estimaciones y se etiquetan como tales.
decision_dir <- file.path(repo_root, "research", "kierkegaard-decision")
decision <- tibble(x = c(0, 1, 1, 2, 2, 2, 2), y = c(0, 0, 1, 1.65, .45, -.8, .15), xend = c(1, 1, 2, 2, 2, 2, 2), yend = c(0, 1, 1.65, .45, -.8, .15, .15), etiqueta = c("Elegir", "comprometerse", "esperar", "resultado favorable", "opción preservada", "costo del retraso", "información nueva"))
p_decision <- ggplot() + geom_segment(data = decision, aes(x, y, xend = xend, yend = yend), colour = pal$azul, linewidth = 1, arrow = grid::arrow(length = grid::unit(.16, "cm"))) +
  geom_point(data = tibble(x = c(0, 1, 2, 2, 2, 2), y = c(0, 0, 1.65, .45, -.8, .15)), aes(x, y), colour = pal$terracota, size = 3) +
  geom_text(data = tibble(x = c(.45, 1.5, 1.5, 1.5, 1.5), y = c(.08, 1.78, .58, -.98, .28), label = c("decisión", "compromiso", "espera", "retraso", "nueva información")), aes(x, y, label = label), size = 3.4, fontface = "bold", colour = pal$tinta) +
  scale_x_continuous(limits = c(-.2, 2.35), breaks = NULL) + scale_y_continuous(limits = c(-1.2, 2.1), breaks = NULL) +
  labs(title = "Decidir también es renunciar a caminos", subtitle = "Esquema conceptual inspirado en Kierkegaard · no representa una estimación", x = NULL, y = NULL, caption = "El valor económico de una opción depende de la incertidumbre, el tiempo y la posibilidad de revertir la decisión.") + theme_editorial("none") + theme(axis.title = element_blank(), axis.text = element_blank(), panel.grid = element_blank(), legend.position = "none")
export_plot(p_decision, file.path(decision_dir, "figuras"), "01_arbol_decision_compromiso", 10, 6.4)
write_map(decision_dir, list(tibble(id = "01_arbol_decision_compromiso", pregunta = "¿Qué se pierde al decidir?", familia = "Árbol conceptual", fuente = "Esquema editorial", advertencia = "No es una estimación empírica")))

gym_dir <- file.path(repo_root, "research", "sunk-cost-gimnasio")
gym <- tibble(visitas_futuras = 0:20, costo_promedio = 2500 / pmax(1, 2 + visitas_futuras), costo_futuro_relevante = 80 + 0 * visitas_futuras)
p_gym <- ggplot(gym, aes(visitas_futuras, costo_promedio)) + geom_line(colour = pal$terracota, linewidth = 1.15) + geom_point(data = gym |> filter(visitas_futuras %in% c(2, 10, 20)), size = 2.8, colour = pal$terracota) +
  geom_hline(yintercept = 80, colour = pal$azul, linetype = "dashed", linewidth = .8) + geom_text(data = tibble(x = 14, y = 87, label = "costo futuro relevante"), aes(x, y, label = label), colour = pal$azul, size = 3.3, hjust = 0) +
  scale_y_continuous(labels = label_number(prefix = "RD$ ", accuracy = 1), expand = expansion(mult = c(0, .12))) +
  labs(title = "La cuota pagada cambia el promedio, no la decisión futura", subtitle = "Ejemplo ilustrativo: membresía de RD$2,500 y costo por visita futura de RD$80", x = "Visitas futuras", y = "Costo promedio por visita", caption = "El costo hundido ya ocurrió; la decisión racional mira beneficios y costos que todavía pueden cambiar.") + theme_editorial()
export_plot(p_gym, file.path(gym_dir, "figuras"), "01_costo_hundido_vs_costo_futuro", 10, 6.2)
write_map(gym_dir, list(tibble(id = "01_costo_hundido_vs_costo_futuro", pregunta = "¿Qué parte del costo debe afectar la decisión?", familia = "Línea conceptual", fuente = "Ejemplo ilustrativo", advertencia = "No representa precios promedio de gimnasios")))

info_dir <- file.path(repo_root, "research", "gracian-economia-informacion")
info_cells <- tribble(~x, ~y, ~titulo, ~descripcion, ~color, 1, 1, "Mercado sano", "calidad observable", "azul", 2, 1, "Selección adversa", "el comprador no distingue", "terracota", 1, 2, "Riesgo moral", "la conducta cambia después", "ocre", 2, 2, "Señalización", "reputación o señal costosa", "oliva")
p_info <- ggplot(info_cells, aes(x, y, fill = color)) + geom_tile(colour = pal$crema, linewidth = 1.1) + geom_text(aes(label = paste0(titulo, "\n", descripcion)), size = 4, fontface = "bold", lineheight = .92) + scale_fill_manual(values = c(azul = pal$azul, terracota = pal$terracota, ocre = pal$ocre, oliva = pal$oliva), guide = "none") + scale_x_continuous(breaks = c(1, 2), labels = c("La información se observa", "La información se oculta"), expand = c(0, 0)) + scale_y_continuous(breaks = c(1, 2), labels = c("Antes del intercambio", "Después del intercambio"), expand = c(0, 0)) + labs(title = "La prudencia empieza donde la información no está repartida", subtitle = "Mapa conceptual de problemas de información asimétrica", x = NULL, y = NULL, caption = "Esquema de teoría económica; Gracián funciona aquí como lente literario, no como autor de la teoría moderna.") + theme_editorial("none") + theme(panel.grid = element_blank(), axis.text = element_text(size = 9))
export_plot(p_info, file.path(info_dir, "figuras"), "01_matriz_informacion_asimetrica", 10, 7)
write_map(info_dir, list(tibble(id = "01_matriz_informacion_asimetrica", pregunta = "¿Qué ocurre cuando una parte sabe más que la otra?", familia = "Matriz conceptual", fuente = "Teoría económica", advertencia = "No es evidencia estadística")))

price_dir <- file.path(repo_root, "research", "precio-justo-pricing-dinamico")
dynamic <- tibble(hora = 1:12, demanda = c(35, 40, 44, 49, 55, 64, 77, 92, 88, 70, 52, 41), precio = 25 + demanda * .55)
p_dynamic <- ggplot(dynamic, aes(hora)) + geom_col(aes(y = demanda), fill = pal$gris_claro, colour = pal$gris, width = .72) + geom_line(aes(y = precio, colour = "Precio dinámico"), linewidth = 1.15) + geom_point(aes(y = precio, colour = "Precio dinámico"), size = 2.5) + geom_hline(yintercept = 25 + mean(dynamic$demanda) * .55, colour = pal$azul, linetype = "dashed", linewidth = .8) + scale_colour_manual(values = c("Precio dinámico" = pal$terracota)) + scale_y_continuous(name = "Demanda (índice) / precio ilustrativo", sec.axis = dup_axis(name = NULL)) + scale_x_continuous(breaks = 1:12, labels = paste0("H", 1:12)) + labs(title = "El precio dinámico sigue la demanda, pero no resuelve la justicia", subtitle = "Esquema conceptual: barras = demanda; línea = precio que cambia con ella", x = NULL, caption = "Ejemplo ilustrativo; no son precios observados de una plataforma dominicana.") + theme_editorial()
export_plot(p_dynamic, file.path(price_dir, "figuras"), "01_demanda_precio_dinamico", 10, 6.2)
fair <- tribble(~x, ~y, ~label, ~fill, 1, 1, "Baja eficiencia\nBaja percepción de justicia", "terracota", 2, 1, "Alta eficiencia\nBaja percepción de justicia", "ocre", 1, 2, "Baja eficiencia\nAlta percepción de justicia", "gris", 2, 2, "Alta eficiencia\nAlta percepción de justicia", "azul")
p_fair <- ggplot(fair, aes(x, y, fill = fill)) + geom_tile(colour = pal$crema, linewidth = 1.1) + geom_text(aes(label = label), size = 4, fontface = "bold", lineheight = .92) + scale_fill_manual(values = c(terracota = pal$terracota, ocre = pal$ocre, gris = pal$gris_claro, azul = pal$azul), guide = "none") + scale_x_continuous(breaks = c(1, 2), labels = c("Menor eficiencia", "Mayor eficiencia"), expand = c(0, 0)) + scale_y_continuous(breaks = c(1, 2), labels = c("Menor justicia percibida", "Mayor justicia percibida"), expand = c(0, 0)) + labs(title = "Eficiencia y justicia pueden moverse por separado", subtitle = "Matriz conceptual para discutir el precio justo", x = NULL, y = NULL, caption = "Esquema editorial; no asigna una medida objetiva de justicia.") + theme_editorial("none") + theme(panel.grid = element_blank(), axis.text = element_text(size = 9))
export_plot(p_fair, file.path(price_dir, "figuras"), "02_matriz_eficiencia_justicia", 9.5, 6.8)
write_map(price_dir, list(tibble(id = "01_demanda_precio_dinamico", pregunta = "¿Cómo cambia el precio cuando cambia la demanda?", familia = "Serie conceptual", fuente = "Ejemplo ilustrativo", advertencia = "No son precios observados"), tibble(id = "02_matriz_eficiencia_justicia", pregunta = "¿Eficiencia y justicia son la misma cosa?", familia = "Matriz conceptual", fuente = "Esquema editorial", advertencia = "Justicia percibida no observada")))

# 4. Trampa de ingresos medios: primer gráfico comparativo con WDI descargado.
dir.create(file.path(repo_root, "research", "trampa-ingresos-medios", "data", "procesados"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(repo_root, "research", "trampa-ingresos-medios", "figuras"), recursive = TRUE, showWarnings = FALSE)
middle_dir <- file.path(repo_root, "research", "trampa-ingresos-medios")
read_wdi <- function(path) {
  x <- jsonlite::fromJSON(path, flatten = TRUE)
  as_tibble(x[[2]]) |> transmute(pais = country.value, codigo = countryiso3code, anio = as.integer(date), valor = as.numeric(value)) |> filter(!is.na(valor))
}
gdp <- read_wdi(file.path(middle_dir, "data", "raw", "worldbank_gdp_pc_ppp.json")) |> filter(codigo %in% c("DOM", ""), anio >= 1990)
gdp$grupo <- if_else(gdp$codigo == "DOM", "República Dominicana", "Ingreso mediano alto")
gdp <- gdp |> group_by(grupo) |> mutate(indice_1990 = 100 * valor / valor[anio == 1990][1]) |> ungroup()
write_csv(gdp, file.path(middle_dir, "data", "procesados", "01_ingreso_pc_ppp_wdi.csv"))
p_middle <- ggplot(gdp, aes(anio, indice_1990, colour = grupo)) + geom_line(linewidth = 1.1) + scale_colour_manual(values = c("República Dominicana" = pal$terracota, "Ingreso mediano alto" = pal$azul)) + scale_x_continuous(breaks = seq(1990, 2025, 5)) + scale_y_continuous(labels = label_number(accuracy = 1)) + labs(title = "El ingreso puede crecer sin cerrar la brecha de convergencia", subtitle = "PIB per cápita PPP constante: índice 1990 = 100 · República Dominicana frente a economías de ingreso mediano alto", x = NULL, y = "Índice", caption = "Fuente: Banco Mundial, World Development Indicators · comparación descriptiva; no prueba por sí sola una trampa de ingresos medios.") + theme_editorial()
export_plot(p_middle, file.path(middle_dir, "figuras"), "01_trayectoria_ingreso_pc_ppp", 10.5, 6.3)
write_map(middle_dir, list(tibble(id = "01_trayectoria_ingreso_pc_ppp", pregunta = "¿La convergencia del ingreso es automática?", familia = "Líneas comparables", fuente = "Banco Mundial WDI", advertencia = "Índice PPP; requiere complementar con productividad e instituciones")))

message("Visuales pendientes generados: cuesta de agosto, ENIEF/emergencia, cuatro paquetes conceptuales y primer comparativo de ingresos medios.")
