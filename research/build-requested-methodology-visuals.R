#!/usr/bin/env Rscript

options(encoding = "UTF-8", scipen = 999)
if (!isTRUE(l10n_info()[["UTF-8"]])) stop("R no esta leyendo UTF-8.", call. = FALSE)

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

pal <- list(crema = "#F7F1E7", tinta = "#252525", terracota = "#B65C43",
            azul = "#4F789F", oliva = "#76825A", ocre = "#C79B53",
            gris = "#9B9B94", gris_claro = "#DED9D0", blanco = "#FFFDFC")

theme_editorial <- function(grid = "y") {
  theme_minimal(base_size = 12, base_family = "Arial") +
    theme(plot.background = element_rect(fill = pal$crema, colour = NA),
          panel.background = element_rect(fill = pal$crema, colour = NA),
          panel.grid.major = if (grid == "y") element_line(colour = pal$gris_claro, linewidth = .35) else element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_text(colour = pal$tinta), axis.text = element_text(colour = pal$tinta),
          plot.title = element_text(face = "bold", size = 17, colour = pal$tinta),
          plot.subtitle = element_text(colour = "#5A5A55", size = 11),
          plot.caption = element_text(colour = "#65655F", size = 8.5, hjust = 0),
          legend.position = "top", legend.title = element_blank(),
          plot.margin = margin(14, 28, 14, 14))
}

export_plot <- function(plot, dir, slug, width = 10, height = 6.5) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(dir, paste0(slug, ".svg")), plot, device = svglite::svglite,
         width = width, height = height, bg = pal$crema)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ggsave(file.path(dir, paste0(slug, ".png")), plot, device = ragg::agg_png,
           width = width, height = height, dpi = 320, bg = pal$crema)
  } else {
    ggsave(file.path(dir, paste0(slug, ".png")), plot, width = width, height = height,
           dpi = 320, bg = pal$crema)
  }
}

assert_true <- function(x, msg) if (!isTRUE(x)) stop(msg, call. = FALSE)
write_map <- function(dir, rows) write_csv(bind_rows(rows), file.path(dir, "chart-map.csv"))
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
  key <- str_to_lower(iconv(str_squish(as.character(x)), from = "UTF-8", to = "ASCII//TRANSLIT"))
  unname(c(enero = 1, ene = 1, febrero = 2, feb = 2, marzo = 3, mar = 3,
           abril = 4, abr = 4, mayo = 5, may = 5, junio = 6, jun = 6,
           julio = 7, jul = 7, agosto = 8, ago = 8, septiembre = 9, sep = 9,
           sept = 9, octubre = 10, oct = 10, noviembre = 11, nov = 11,
           diciembre = 12, dic = 12)[key])
}

# 14. La cuesta de agosto: robustez año a año, no solo promedio agregado.
cuesta_dir <- file.path(repo_root, "research", "cuesta-agosto")
cuesta_data <- read_csv(file.path(cuesta_dir, "data", "procesados", "01_indice_estacional_ipc.csv"), show_col_types = FALSE)
ipc_path <- file.path(repo_root, "atlas", "data", "raw", "bcrd-precios", "ipc_articulos_base_2019-2020.xlsx")
raw_ipc <- read_excel(ipc_path, sheet = "2020-2026", col_names = FALSE, .name_repair = "minimal")
year_scores <- vapply(seq_len(nrow(raw_ipc)), function(i) sum(str_detect(as.character(unlist(raw_ipc[i, ], use.names = FALSE)), "^20[0-9]{2}$"), na.rm = TRUE), numeric(1))
month_scores <- vapply(seq_len(nrow(raw_ipc)), function(i) sum(!is.na(month_num(unlist(raw_ipc[i, ], use.names = FALSE)))), numeric(1))
year_row <- which.max(year_scores); month_row <- which.max(month_scores)
years <- fill_year(unlist(raw_ipc[year_row, ], use.names = FALSE)); months <- month_num(unlist(raw_ipc[month_row, ], use.names = FALSE))
cols <- which(!is.na(years) & !is.na(months))
group_rows <- which(str_detect(str_squish(as.character(raw_ipc[[1]])), "^[0-9]{2} "))
ipc_groups <- bind_rows(lapply(group_rows, function(row) tibble(
  grupo = str_remove(str_squish(as.character(raw_ipc[[1]][row])), "^[0-9]{2} "),
  anio = years[cols], mes = months[cols],
  indice = suppressWarnings(as.numeric(unlist(raw_ipc[row, cols], use.names = FALSE)))
))) |> filter(anio >= 2021, anio <= 2025, !is.na(indice))
aug_year <- ipc_groups |> group_by(grupo, anio) |> mutate(indice_rel_anual = 100 * indice / mean(indice, na.rm = TRUE)) |> ungroup() |> filter(mes == 8) |> select(grupo, anio, indice_agosto = indice_rel_anual)
write_csv(aug_year, file.path(cuesta_dir, "data", "procesados", "03_agosto_por_anio.csv"))
aug_year_plot <- aug_year |> group_by(grupo) |> summarise(var = var(indice_agosto), .groups = "drop") |> arrange(desc(var)) |> pull(grupo)
p_cuesta_year <- aug_year |> mutate(grupo = factor(grupo, levels = aug_year_plot)) |> ggplot(aes(anio, grupo, fill = indice_agosto)) +
  geom_tile(colour = pal$crema, linewidth = .4) + geom_text(aes(label = sprintf("%.1f", indice_agosto)), size = 2.7, fontface = "bold") +
  scale_x_continuous(breaks = 2021:2025) + scale_fill_gradient2(low = pal$azul, mid = pal$crema, high = pal$terracota, midpoint = 100, name = "Indice") +
  labs(title = "La cuesta de agosto no aparece todos los años igual", subtitle = "Indice de agosto frente al promedio anual de cada grupo · 100 = promedio del año", x = NULL, y = NULL, caption = "Fuente: BCRD, IPC por grupos · cifras descriptivas; no identifican una causa estacional") + theme_editorial("none") + theme(axis.text.y = element_text(size = 8.5))
export_plot(p_cuesta_year, file.path(cuesta_dir, "figuras"), "03_agosto_por_anio", 10.5, 7.4)
write_map(cuesta_dir, list(tibble(id = "01_indice_estacional_ipc", pregunta = "¿La presion de precios tiene un patron mensual?", familia = "Mapa de calor estacional", fuente = "BCRD IPC", advertencia = "Indice descriptivo, no causal"), tibble(id = "02_agosto_por_grupo", pregunta = "¿Que grupos se encarecen relativamente en agosto?", familia = "Barras ordenadas", fuente = "BCRD IPC", advertencia = "Promedio 2021-2025"), tibble(id = "03_agosto_por_anio", pregunta = "¿La cuesta se repite con la misma intensidad?", familia = "Mapa de calor por año", fuente = "BCRD IPC", advertencia = "Agosto se compara con el promedio de su propio año")))

# 15. Ingreso mediano: complementar ingreso con productividad y apertura.
middle_dir <- file.path(repo_root, "research", "trampa-ingresos-medios")
read_wdi <- function(path) {
  x <- jsonlite::fromJSON(path, flatten = TRUE)
  as_tibble(x[[2]]) |> transmute(pais = country.value, codigo = countryiso3code, anio = as.integer(date), valor = as.numeric(value)) |> filter(!is.na(valor))
}
index_first_available <- function(x, y) {
  base <- y[which(is.finite(y))[1]]
  100 * y / base
}
prod <- read_wdi(file.path(middle_dir, "data", "raw", "worldbank_productivity.json")) |> filter((pais == "Dominican Republic" | pais == "Upper middle income"), anio >= 1990)
exports <- read_wdi(file.path(middle_dir, "data", "raw", "worldbank_exports_gdp.json")) |> filter((pais == "Dominican Republic" | pais == "Upper middle income"), anio >= 1990)
prod <- prod |> mutate(grupo = if_else(pais == "Dominican Republic", "Rep\u00fablica Dominicana", "Ingreso mediano alto")) |> group_by(grupo) |> arrange(anio) |> mutate(indice = index_first_available(valor, valor)) |> ungroup() |> mutate(metrica = "Productividad por trabajador")
exports <- exports |> mutate(grupo = if_else(pais == "Dominican Republic", "Rep\u00fablica Dominicana", "Ingreso mediano alto")) |> group_by(grupo) |> arrange(anio) |> mutate(indice = index_first_available(valor, valor)) |> ungroup() |> mutate(metrica = "Exportaciones (% del PIB)")
middle_diag <- bind_rows(prod, exports) |> filter(is.finite(indice))
write_csv(middle_diag, file.path(middle_dir, "data", "procesados", "02_productividad_apertura_wdi.csv"))
p_middle_diag <- ggplot(middle_diag, aes(anio, indice, colour = grupo)) + geom_line(linewidth = 1.05) + facet_wrap(~metrica, scales = "free_y") +
  scale_colour_manual(values = c("Rep\u00fablica Dominicana" = pal$terracota, "Ingreso mediano alto" = pal$azul)) + scale_x_continuous(breaks = seq(1990, 2025, 10)) +
  labs(title = "El ingreso no basta para diagnosticar una trampa", subtitle = "Productividad y exportaciones, primer año disponible = 100 · Republica Dominicana frente al agregado de ingreso mediano alto", x = NULL, y = "Indice", caption = "Fuente: Banco Mundial, WDI · diagnostico descriptivo; la base puede variar por indicador") + theme_editorial()
export_plot(p_middle_diag, file.path(middle_dir, "figuras"), "02_productividad_apertura", 11, 7)
write_map(middle_dir, list(tibble(id = "01_trayectoria_ingreso_pc_ppp", pregunta = "¿La convergencia del ingreso es automatica?", familia = "Lineas comparables", fuente = "Banco Mundial WDI", advertencia = "Indice PPP; requiere complementar con productividad e instituciones"), tibble(id = "02_productividad_apertura", pregunta = "¿Que hay detras de la trayectoria del ingreso?", familia = "Panel de diagnostico", fuente = "Banco Mundial WDI", advertencia = "No es prueba causal de una trampa")))

# 16. Fondo de emergencia: diferencia entre estratos, manteniendo la encuesta como unidad.
emergency_dir <- file.path(repo_root, "research", "epicteto-fondo-emergencia")
enief <- read_csv(file.path(emergency_dir, "data", "procesados", "01_enief_emergencia_2023.csv"), show_col_types = FALSE)
low_name <- "Ingresos RD$0–31,200"; high_name <- "Ingresos RD$31,201 o más"
emergency_diff <- enief |> select(respuesta, estrato, porcentaje) |> pivot_wider(names_from = estrato, values_from = porcentaje) |> mutate(diferencia_pp = .data[[high_name]] - .data[[low_name]])
write_csv(emergency_diff, file.path(emergency_dir, "data", "procesados", "02_diferencia_estratos.csv"))
diff_order <- emergency_diff |> arrange(diferencia_pp) |> pull(respuesta)
p_emergency_diff <- emergency_diff |> mutate(respuesta = factor(respuesta, levels = diff_order), color = if_else(diferencia_pp >= 0, "Mayor ingreso", "Menor ingreso")) |> ggplot(aes(diferencia_pp, respuesta, colour = color)) + geom_vline(xintercept = 0, colour = pal$gris, linetype = "dashed") + geom_segment(aes(x = 0, xend = diferencia_pp, y = respuesta, yend = respuesta), linewidth = 4.5) + geom_text(aes(label = sprintf("%+.1f pp", diferencia_pp), hjust = if_else(diferencia_pp >= 0, -.12, 1.12)), colour = pal$tinta, size = 3, fontface = "bold") + scale_colour_manual(values = c("Mayor ingreso" = pal$azul, "Menor ingreso" = pal$terracota), guide = "none") + scale_x_continuous(labels = function(x) sprintf("%+.0f pp", x), expand = expansion(mult = c(.08, .15))) + labs(title = "La capacidad de absorber un shock cambia por estrato", subtitle = "Diferencia en puntos porcentuales: ingresos altos menos ingresos bajos · ENIEF 2023", x = "Diferencia frente al estrato de menor ingreso", y = NULL, caption = "Fuente: BCRD, ENIEF 2023 · primera respuesta declarada; no mide riqueza ni saldo de ahorro") + theme_editorial("x") + theme(axis.text.y = element_text(size = 9))
export_plot(p_emergency_diff, file.path(emergency_dir, "figuras"), "02_diferencia_respuesta_emergencia", 10.5, 7.2)
write_map(emergency_dir, list(tibble(id = "01_respuesta_emergencia_ingreso", pregunta = "¿Como cubren una emergencia los hogares?", familia = "Barras agrupadas", fuente = "BCRD ENIEF 2023", advertencia = "Primera respuesta declarada; no mide saldo de ahorro"), tibble(id = "02_diferencia_respuesta_emergencia", pregunta = "¿Que respuestas separan a los estratos?", familia = "Barras divergentes", fuente = "BCRD ENIEF 2023", advertencia = "Diferencias descriptivas en puntos porcentuales")))

# 17. Gracian: mecanismo conceptual para pasar de prudencia a identificacion.
info_dir <- file.path(repo_root, "research", "gracian-economia-informacion")
info_path <- file.path(info_dir, "figuras")
info_steps <- tibble(x = c(1, 2, 3, 4), y = 1, titulo = c("Lo observable", "Lo oculto", "La señal", "El resultado"), detalle = c("precio, contrato\ny reputacion", "calidad, esfuerzo\ny riesgo", "garantia, prueba\no historial", "seleccion y\nconducta"))
p_info_flow <- ggplot() + geom_segment(data = info_steps[1:3, ], aes(x = x + .18, xend = x + .82, y = y, yend = y), arrow = grid::arrow(length = grid::unit(.18, "cm")), colour = pal$gris, linewidth = 1) + geom_point(data = info_steps, aes(x, y), size = 20, colour = c(pal$azul, pal$terracota, pal$ocre, pal$oliva)) + geom_text(data = info_steps, aes(x, y + .08, label = titulo), colour = pal$blanco, fontface = "bold", size = 3.3) + geom_text(data = info_steps, aes(x, y - .28, label = detalle), colour = pal$tinta, size = 3.1, lineheight = .9) + scale_x_continuous(limits = c(.35, 4.65), breaks = NULL) + scale_y_continuous(limits = c(.3, 1.35), breaks = NULL) + labs(title = "La prudencia no elimina la asimetria: busca senales", subtitle = "Mapa conceptual del mecanismo economico · no es una estimacion", x = NULL, y = NULL, caption = "Gracian es una lente literaria; la secuencia traduce el problema a un diseño empirico posible.") + theme_editorial("none") + theme(axis.text = element_blank(), panel.grid = element_blank(), legend.position = "none")
export_plot(p_info_flow, info_path, "02_flujo_senal_informacion", 11, 5.5)
write_map(info_dir, list(tibble(id = "01_matriz_informacion_asimetrica", pregunta = "¿Que ocurre cuando una parte sabe mas que la otra?", familia = "Matriz conceptual", fuente = "Teoria economica", advertencia = "No es evidencia estadistica"), tibble(id = "02_flujo_senal_informacion", pregunta = "¿Como puede una senal reducir la incertidumbre?", familia = "Flujo conceptual", fuente = "Esquema editorial", advertencia = "No estima efectos de senales")))

# 20. Gimnasio: matriz de decision centrada en costos y beneficios futuros.
gym_dir <- file.path(repo_root, "research", "sunk-cost-gimnasio")
gym_cells <- tribble(~x, ~y, ~accion, ~regla, ~fill, 1, 2, "Continuar", "beneficio futuro > costo futuro", "azul", 2, 2, "Salir", "beneficio futuro < costo futuro", "terracota", 1, 1, "Comparar", "otra alternativa domina", "ocre", 2, 1, "Ignorar", "cuota ya pagada = costo hundido", "gris")
p_gym_matrix <- ggplot(gym_cells, aes(x, y, fill = fill)) + geom_tile(colour = pal$crema, linewidth = 1.1) + geom_text(aes(label = paste0(accion, "\n", regla)), size = 3.8, fontface = "bold", lineheight = .92) + scale_fill_manual(values = c(azul = pal$azul, terracota = pal$terracota, ocre = pal$ocre, gris = pal$gris_claro), guide = "none") + scale_x_continuous(breaks = c(1, 2), labels = c("Beneficio futuro alto", "Beneficio futuro bajo"), expand = c(0, 0)) + scale_y_continuous(breaks = c(1, 2), labels = c("Alternativa futura mejor", "Alternativa futura peor"), expand = c(0, 0)) + labs(title = "La membresia pagada no decide por ti", subtitle = "Matriz conceptual: la decision mira costos y beneficios que aun pueden cambiar", x = NULL, y = NULL, caption = "Ejemplo conceptual; no representa conducta observada de usuarios de gimnasios.") + theme_editorial("none") + theme(panel.grid = element_blank(), axis.text = element_text(size = 9))
export_plot(p_gym_matrix, file.path(gym_dir, "figuras"), "02_matriz_decision_futura", 10.5, 6.8)
write_map(gym_dir, list(tibble(id = "01_costo_hundido_vs_costo_futuro", pregunta = "¿Que parte del costo debe afectar la decision?", familia = "Linea conceptual", fuente = "Ejemplo ilustrativo", advertencia = "No representa precios promedio de gimnasios"), tibble(id = "02_matriz_decision_futura", pregunta = "¿Que deberia comparar el usuario?", familia = "Matriz conceptual", fuente = "Economia conductual", advertencia = "No es evidencia de comportamiento")))

# 21. Crecimiento: PIB oficial por incidencia e IMAE mensual. No se afirma llegada a hogares.
growth_dir <- file.path(repo_root, "research", "crecimiento-6-4-hogares")
dir.create(file.path(growth_dir, "data", "procesados"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(growth_dir, "figuras"), recursive = TRUE, showWarnings = FALSE)
pib <- read_excel(file.path(repo_root, "atlas", "data", "raw", "bcrd-sector-real", "pib_origen_2018.xlsx"), sheet = "PIBK_Trim_Acum", col_names = FALSE, .name_repair = "minimal")
labels <- str_squish(as.character(pib[[1]])); top <- c("Agropecuario", "Industrias", "Construccion", "Servicios")
labels_norm <- str_replace_all(iconv(labels, from = "UTF-8", to = "ASCII//TRANSLIT"), "[^A-Za-z ]", "") |> str_squish()
find_label <- function(value) which(labels_norm == value)
blocks <- find_label("Agropecuario")
assert_true(length(blocks) >= 3, "No se encontraron los tres bloques del PIBK acumulado.")
block_rows <- function(start, end) which(seq_along(labels) >= start & seq_along(labels) < end & labels_norm %in% top)
ends <- c(blocks[2], blocks[3], length(labels) + 1)
level_rows <- block_rows(blocks[1], ends[1]); growth_rows <- block_rows(blocks[2], ends[2]); incidence_rows <- block_rows(blocks[3], ends[3])
assert_true(length(level_rows) == 4 && length(growth_rows) == 4 && length(incidence_rows) == 4, "La estructura sectorial del PIBK cambio; revisar antes de publicar.")
years_pib <- suppressWarnings(as.integer(str_extract(as.character(unlist(pib[7, ], use.names = FALSE)), "20[0-9]{2}"))); years_pib <- fill_year(years_pib); periods <- str_squish(as.character(unlist(pib[8, ], use.names = FALSE)))
full_cols <- which(periods %in% c("E-D", "Ene-Dic", "Enero-Diciembre") & years_pib %in% c(2024, 2025))
if (length(full_cols) < 2) full_cols <- which(years_pib %in% c(2024, 2025) & periods == periods[which(years_pib == 2025)[1]])
assert_true(sum(years_pib[full_cols] == 2024) >= 1 && sum(years_pib[full_cols] == 2025) >= 1, "No se encontraron columnas anuales 2024 y 2025 en PIBK_Trim_Acum.")
col24 <- full_cols[which(years_pib[full_cols] == 2024)[1]]; col25 <- full_cols[which(years_pib[full_cols] == 2025)[1]]
to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
sector_data <- tibble(actividad = labels_norm[level_rows], indice_volumen_2024 = to_num(unlist(pib[level_rows, col24], use.names = FALSE)), crecimiento_2025 = to_num(unlist(pib[growth_rows, col25], use.names = FALSE)), incidencia_2025 = to_num(unlist(pib[incidence_rows, col25], use.names = FALSE)))
assert_true(all(is.finite(sector_data$incidencia_2025)), "La incidencia sectorial contiene valores no numericos.")
sector_data <- sector_data |> mutate(actividad = recode(actividad, Construccion = "Construcci\u00f3n"), lado = if_else(incidencia_2025 >= 0, "Aporta", "Resta"))
write_csv(sector_data, file.path(growth_dir, "data", "procesados", "01_pib_sectores_2024_2025.csv"))
p_growth <- sector_data |> arrange(incidencia_2025) |> mutate(actividad = factor(actividad, levels = actividad), hjust = if_else(incidencia_2025 >= 0, -.12, 1.12)) |> ggplot(aes(incidencia_2025, actividad, fill = lado)) + geom_vline(xintercept = 0, colour = pal$gris) + geom_col(width = .58) + geom_text(aes(label = sprintf("%+.2f pp", incidencia_2025), hjust = hjust), size = 3.3, fontface = "bold", colour = pal$tinta) + scale_fill_manual(values = c(Aporta = pal$terracota, Resta = pal$azul), guide = "none") + scale_x_continuous(expand = expansion(mult = c(.16, .2))) + labs(title = "El crecimiento agregado tiene una composición", subtitle = "Incidencia oficial por actividad económica en el PIB acumulado de 2025", x = "Puntos porcentuales de incidencia", y = NULL, caption = "Fuente: BCRD, PIB por actividad económica · 2025 preliminar si así figura en la hoja oficial") + theme_editorial("x")
export_plot(p_growth, file.path(growth_dir, "figuras"), "01_incidencia_pib_2025", 10.5, 6.3)
sector_scatter <- sector_data |> mutate(hjust = if_else(crecimiento_2025 < 0, 0, .5))
p_scatter <- ggplot(sector_scatter, aes(crecimiento_2025, incidencia_2025, colour = actividad, label = actividad)) + geom_hline(yintercept = 0, colour = pal$gris, linetype = "dashed") + geom_point(size = 4) + geom_text(aes(hjust = hjust), nudge_y = .04, show.legend = FALSE, fontface = "bold", size = 3.5) + scale_colour_manual(values = c(Agropecuario = pal$oliva, Industrias = pal$ocre, `Construcción` = pal$terracota, Servicios = pal$azul), guide = "none") + scale_x_continuous(limits = c(-2.35, 4.1), expand = c(0, 0)) + labs(title = "La tasa de crecimiento no cuenta toda la historia", subtitle = "Crecimiento acumulado sectorial frente a incidencia oficial en 2025", x = "Crecimiento acumulado del sector (%)", y = "Incidencia en 2025 (puntos porcentuales)", caption = "Fuente: BCRD · lectura descriptiva; no conecta todavía el crecimiento sectorial con ingresos de hogares") + theme_editorial()
export_plot(p_scatter, file.path(growth_dir, "figuras"), "02_crecimiento_vs_incidencia_sectorial", 10, 6.4)
imae_raw <- read_excel(file.path(repo_root, "atlas", "data", "raw", "bcrd-sector-real", "imae_2018.xlsx"), sheet = "IMAE", col_names = FALSE, .name_repair = "minimal")
imae_rows <- imae_raw[9:nrow(imae_raw), , drop = FALSE]
imae <- tibble(anio = fill_year(imae_rows[[1]]), mes = month_num(imae_rows[[2]]), variacion_interanual = to_num(imae_rows[[4]])) |> filter(!is.na(anio), !is.na(mes), is.finite(variacion_interanual)) |> arrange(anio, mes) |> mutate(fecha = as.Date(sprintf("%04d-%02d-01", anio, mes))) |> filter(fecha >= as.Date("2024-01-01"))
assert_true(nrow(imae) >= 12, "La serie IMAE no tiene suficientes observaciones recientes.")
write_csv(imae, file.path(growth_dir, "data", "procesados", "02_imae_mensual.csv"))
p_imae <- ggplot(imae, aes(fecha, variacion_interanual)) + geom_hline(yintercept = 0, colour = pal$gris) + geom_col(fill = pal$azul, width = 20) + geom_text(data = imae |> filter(!is.na(variacion_interanual), variacion_interanual == max(variacion_interanual, na.rm = TRUE)), aes(label = sprintf("%+.1f%%", variacion_interanual)), vjust = -0.5, fontface = "bold", size = 3.2) + scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") + scale_y_continuous(labels = function(x) sprintf("%+.0f%%", x), expand = expansion(mult = c(.08, .16))) + labs(title = "El IMAE cambia de ritmo mes a mes", subtitle = "Tasa interanual publicada del indice mensual de actividad economica", x = NULL, y = "Variacion interanual", caption = "Fuente: BCRD, IMAE · se utiliza la tasa publicada en la hoja oficial; ultima observacion disponible en el archivo local") + theme_editorial("y") + theme(axis.text.x = element_text(size = 8))
export_plot(p_imae, file.path(growth_dir, "figuras"), "03_imae_mensual", 11, 6.2)
write_map(growth_dir, list(tibble(id = "01_incidencia_pib_2025", pregunta = "¿Que sectores explican el crecimiento agregado?", familia = "Incidencia sectorial", fuente = "BCRD PIBK_Trim_Acum", advertencia = "2025 preliminar; no mide distribucion del ingreso"), tibble(id = "02_crecimiento_vs_incidencia_sectorial", pregunta = "¿La tasa sectorial cuenta todo el aporte?", familia = "Dispersión sectorial", fuente = "BCRD PIBK_Trim_Acum", advertencia = "Comparacion descriptiva, no causal"), tibble(id = "03_imae_mensual", pregunta = "¿El crecimiento mantiene el mismo ritmo?", familia = "Serie mensual", fuente = "BCRD IMAE", advertencia = "No equivale a ingreso de los hogares")))

# 22. Baade: diseño de identificacion, no resultado observado.
baade_dir <- file.path(repo_root, "research", "efecto-baade")
baade_fig <- file.path(baade_dir, "figuras")
dir.create(file.path(baade_dir, "data"), recursive = TRUE, showWarnings = FALSE)
event <- tibble(periodo = rep(-6:6, times = 2), zona = rep(c("Zona tratada", "Zona comparable"), each = 13), indice = c(100, 101, 99, 100, 102, 101, 100, 101, 102, 101, 100, 101, 100, 100, 100, 101, 100, 101, 99, 100, 101, 100, 102, 101, 101, 102), ilustrativo = TRUE)
event <- event |> mutate(indice = if_else(zona == "Zona tratada" & periodo >= 0, indice + (periodo + 1) * .55, indice))
write_csv(event, file.path(baade_dir, "data", "diseno_event_study_ilustrativo.csv"))
p_event <- ggplot(event, aes(periodo, indice, colour = zona)) + geom_vline(xintercept = 0, linetype = "dashed", colour = pal$gris) + geom_line(linewidth = 1.1) + geom_point(size = 2.4) + scale_colour_manual(values = c("Zona tratada" = pal$terracota, "Zona comparable" = pal$azul)) + scale_x_continuous(breaks = -6:6) + labs(title = "Un estadio exige comparar la zona con su contrafactual", subtitle = "Diseño ilustrativo de evento: no es una estimacion del efecto de un estadio dominicano", x = "Periodos relativos a la apertura", y = "Indice de actividad economica", caption = "La prueba requiere pre-tendencias comparables y resultados netos: empleo, establecimientos, ventas o valor del suelo.") + theme_editorial()
export_plot(p_event, baade_fig, "01_diseno_event_study", 10.5, 6.3)
write_map(baade_dir, list(tibble(id = "01_diseno_event_study", pregunta = "¿Como se identifica un spillover neto?", familia = "Diseño de evento", fuente = "Diseño metodologico inspirado en Baade y Dye", advertencia = "Serie ilustrativa; no es evidencia dominicana")))

message("Visuales y metodologia preparados para 14, 15, 16, 17, 20, 21 y 22.")
