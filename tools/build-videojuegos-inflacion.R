args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Uso: Rscript build-videojuegos-inflacion.R <directorio_salida>")
}
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop("R no está leyendo UTF-8 correctamente; detenga la generación.")
}

out_dir <- normalizePath(args[[1]], winslash = "/", mustWork = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, "eve-inflacion.png")

W <- 1800
H <- 900
alpha <- function(col, value) grDevices::adjustcolor(col, alpha.f = value)

if (identical(.Platform$OS.type, "windows")) {
  grDevices::windowsFonts(
    videojuegos_ui = grDevices::windowsFont("Segoe UI"),
    videojuegos_display = grDevices::windowsFont("Georgia")
  )
  ui_family <- "videojuegos_ui"
} else {
  ui_family <- "sans"
}

diamond <- function(x, y, radius, fill, border = NA, lwd = 1) {
  polygon(
    c(x, x + radius, x, x - radius),
    c(y - radius, y, y + radius, y),
    col = fill,
    border = border,
    lwd = lwd
  )
}

circle <- function(x, y, radius, fill = NA, border = NA, lwd = 1) {
  symbols(x, y, circles = radius, inches = FALSE, add = TRUE, bg = fill, fg = border, lwd = lwd)
}

angular_panel <- function(x1, y1, x2, y2, fill = "#101f2b", border = "#46616e", cut = 12, lwd = 1.2) {
  polygon(
    c(x1 + cut, x2 - cut, x2, x2, x2 - cut, x1 + cut, x1, x1),
    c(y1, y1, y1 + cut, y2 - cut, y2, y2, y2 - cut, y1 + cut),
    col = fill,
    border = border,
    lwd = lwd
  )
}

draw_plex <- function(x, y, scale = 1) {
  dark <- "#07111b"
  hexagon <- function(cx, cy, radius, fill, border, lwd = 1) {
    angle <- seq(0, 2 * pi, length.out = 7)[-7] + pi / 6
    polygon(cx + cos(angle) * radius, cy + sin(angle) * radius, col = fill, border = border, lwd = lwd)
  }

  # Tres placas facetadas: un activo de inventario, no una moneda terrestre.
  hexagon(x - 11 * scale, y + 11 * scale, 49 * scale, "#6f4829", dark, 1.5)
  hexagon(x - 5 * scale, y + 5 * scale, 49 * scale, "#b87636", dark, 1.5)
  hexagon(x, y, 49 * scale, "#dfaa47", dark, 2)
  hexagon(x, y, 39 * scale, "#f0c86c", "#8a622c", 1.5)
  hexagon(x, y, 27 * scale, "#c78d39", "#f7dfa0", 1)
  circle(x, y, 10 * scale, fill = dark, border = "#f5dc95", lwd = 1)
  for (angle in c(-pi / 2, pi / 6, 5 * pi / 6)) {
    segments(
      x + cos(angle) * 12 * scale,
      y + sin(angle) * 12 * scale,
      x + cos(angle) * 31 * scale,
      y + sin(angle) * 31 * scale,
      col = "#f8e1a0",
      lwd = 2
    )
  }
  polygon(
    c(x - 34, x - 9, x + 25, x + 12) * scale + x * (1 - scale),
    c(y - 23, y - 39, y - 28, y - 20) * scale + y * (1 - scale),
    col = alpha("#fff4c7", .24), border = NA
  )
  text(x, y + 69 * scale, "PLEX", col = "#f2d98e", cex = .58 * scale, font = 2)
}

draw_fleet <- function(x, y, scale = 1) {
  dark <- "#07111b"
  hull <- "#86abb5"
  light <- "#bdd6d8"
  orange <- "#d77c5b"

  # Crucero cenital: morro largo, alas barridas y tres motores visibles.
  polygon(
    c(x, x + 18, x + 29, x + 74, x + 62, x + 25, x + 17, x, x - 17, x - 25, x - 62, x - 74, x - 29, x - 18) * scale + x * (1 - scale),
    c(y - 92, y - 54, y - 25, y + 5, y + 28, y + 20, y + 62, y + 74, y + 62, y + 20, y + 28, y + 5, y - 25, y - 54) * scale + y * (1 - scale),
    col = hull,
    border = dark,
    lwd = 2
  )
  polygon(
    c(x, x + 12, x + 18, x, x - 18, x - 12) * scale + x * (1 - scale),
    c(y - 78, y - 43, y + 48, y + 67, y + 48, y - 43) * scale + y * (1 - scale),
    col = "#36586a",
    border = dark,
    lwd = 1.5
  )
  polygon(
    c(x - 9, x, x + 9, x + 6, x - 6) * scale + x * (1 - scale),
    c(y - 58, y - 79, y - 58, y - 31, y - 31) * scale + y * (1 - scale),
    col = orange,
    border = dark,
    lwd = 1
  )
  segments(x - 50 * scale, y + 5 * scale, x - 18 * scale, y - 17 * scale, col = light, lwd = 1.5)
  segments(x + 50 * scale, y + 5 * scale, x + 18 * scale, y - 17 * scale, col = light, lwd = 1.5)
  for (engine_x in c(-14, 0, 14)) {
    circle(x + engine_x * scale, y + 60 * scale, 5 * scale, fill = "#bcecf0", border = dark, lwd = 1)
    segments(x + engine_x * scale, y + 66 * scale, x + engine_x * scale, y + 82 * scale, col = alpha("#8fd5df", .75), lwd = 2)
  }

  # Dos escoltas pequeñas refuerzan la lectura de flota.
  for (escort_x in c(-92, 92)) {
    polygon(
      c(x + (escort_x - 8) * scale, x + escort_x * scale, x + (escort_x + 8) * scale, x + escort_x * scale),
      c(y + 43 * scale, y + 25 * scale, y + 43 * scale, y + 60 * scale),
      col = "#6f929e", border = dark, lwd = 1
    )
    segments(x + escort_x * scale, y + 60 * scale, x + escort_x * scale, y + 70 * scale, col = alpha("#8fd5df", .65), lwd = 1.5)
  }
}

draw_status_row <- function(x, y, label, value, color) {
  segments(x, y - 17, x, y + 17, col = color, lwd = 3)
  text(x + 16, y - 9, label, adj = c(0, .5), col = "#8faeb9", cex = .68, font = 2)
  text(x + 16, y + 12, value, adj = c(0, .5), col = "#e5ecec", cex = .88, font = 2)
}

draw_bar <- function(y, label, value, color, baseline, scale_x) {
  x_end <- baseline + value * scale_x
  text(557, y - 18, label, adj = c(0, .5), col = "#a9bec7", cex = .72, font = 2)
  segments(557, y + 16, 1242, y + 16, col = "#243c49", lwd = 1)
  segments(baseline, y, x_end, y, col = alpha(color, .20), lwd = 22)
  segments(baseline, y, x_end, y, col = color, lwd = 9)
  diamond(x_end, y, 7, "#f1e6c8", color, 1)
  value_label <- paste0(if (value > 0) "+" else "−", format(abs(value), decimal.mark = ",", nsmall = 1), "%")
  if (value > 0) {
    text(x_end + 18, y, value_label, adj = c(0, .5), col = color, cex = 1.08, font = 2)
  } else {
    text(x_end - 18, y, value_label, adj = c(1, .5), col = color, cex = 1.08, font = 2)
  }
}

png(out_file, width = W, height = H, res = 150, bg = "#07101a")
par(mar = rep(0, 4), xaxs = "i", yaxs = "i", family = ui_family)
plot.new()
plot.window(c(0, W), c(H, 0), asp = 1)

# Fondo de terminal con gradiente, retícula y estrellas discretas.
top_col <- grDevices::col2rgb("#132837")
bottom_col <- grDevices::col2rgb("#060c15")
gradient <- vapply(seq(0, 1, length.out = 600), function(t) {
  mixed <- (top_col * (1 - t) + bottom_col * t) / 255
  grDevices::rgb(mixed[[1]], mixed[[2]], mixed[[3]])
}, character(1))
rasterImage(as.raster(matrix(gradient, ncol = 1)), 0, 0, W, H, interpolate = TRUE)

for (x in seq(70, W - 70, by = 82)) segments(x, 118, x, 830, col = alpha("#557381", .055), lwd = 1)
for (y in seq(135, 820, by = 58)) segments(55, y, W - 55, y, col = alpha("#557381", .050), lwd = 1)
set.seed(2710)
stars <- data.frame(x = runif(55, 30, W - 30), y = runif(55, 25, H - 40), size = sample(c(.35, .45, .65), 55, TRUE))
points(stars$x, stars$y, pch = 16, cex = stars$size, col = alpha("#d9e3df", .58))

# Cabecera del Neocom y ventana única de mercado.
text(55, 31, "NEOCOM  //  REGIONAL MARKET", adj = c(0, .5), col = "#88aeb8", cex = .74, font = 2)
text(55, 68, "PLEX / PRICE HISTORY", adj = c(0, .5), col = "#e1c36f", cex = 1.32, font = 2)
text(1745, 31, "THE FORGE", adj = c(1, .5), col = "#9cb1b8", cex = .70, font = 2)
text(1745, 67, "MARKET DATA  ·  Q4 2010", adj = c(1, .5), col = "#d6e2e3", cex = .78, font = 2)

angular_panel(45, 100, 1755, 835, fill = "#091722", border = "#526e79", cut = 13, lwd = 1.4)
rect(46, 113, 1754, 157, col = "#142936", border = NA)
segments(45, 157, 1755, 157, col = "#5b7781", lwd = 1)
text(70, 135, "REGIONAL MARKET", adj = c(0, .5), col = "#c9d7d9", cex = .78, font = 2)
text(1728, 135, "STATUS: CONNECTED  ·  JITA IV", adj = c(1, .5), col = "#80aab5", cex = .66, font = 2)

# Navegación lateral: estructura de categorías propia de una ventana de mercado.
rect(46, 158, 330, 834, col = "#08141e", border = NA)
segments(330, 157, 330, 835, col = "#45616d", lwd = 1)
text(68, 184, "MARKET GROUPS", adj = c(0, .5), col = "#809faa", cex = .64, font = 2)
rect(66, 202, 308, 236, col = "#10222e", border = "#38535f")
text(82, 219, "SEARCH MARKET", adj = c(0, .5), col = "#6f8993", cex = .62)
segments(285, 211, 296, 222, col = "#78949d", lwd = 1.2)
circle(280, 207, 6, fill = NA, border = "#78949d", lwd = 1.2)

nav_rows <- list(
  list(y = 270, label = "▾  PILOT SERVICES", indent = 0, selected = FALSE),
  list(y = 310, label = "PLEX", indent = 22, selected = TRUE),
  list(y = 355, label = "▾  SHIP EQUIPMENT", indent = 0, selected = FALSE),
  list(y = 395, label = "TECH II", indent = 22, selected = FALSE),
  list(y = 440, label = "▾  ECONOMIC INDEX", indent = 0, selected = FALSE),
  list(y = 480, label = "CPI GENERAL", indent = 22, selected = FALSE)
)
for (row in nav_rows) {
  if (row$selected) {
    rect(54, row$y - 18, 322, row$y + 18, col = "#243443", border = NA)
    rect(54, row$y - 18, 59, row$y + 18, col = "#d6b85e", border = NA)
  }
  text(70 + row$indent, row$y, row$label, adj = c(0, .5), col = if (row$selected) "#f0d891" else "#b2c2c7", cex = if (row$selected) .76 else .68, font = if (row$selected) 2 else 1)
}
segments(66, 525, 308, 525, col = "#314b57", lwd = 1)
text(68, 552, "QUICKBAR", adj = c(0, .5), col = "#809faa", cex = .64, font = 2)
text(90, 592, "Q4 2010", adj = c(0, .5), col = "#c3d0d2", cex = .68)
text(90, 630, "PRICE LEVELS", adj = c(0, .5), col = "#c3d0d2", cex = .68)
text(90, 668, "EVENT LOG", adj = c(0, .5), col = "#c3d0d2", cex = .68)
text(68, 805, "4 ITEMS  ·  1 SELECTED", adj = c(0, .5), col = "#617d87", cex = .58)

# Encabezado del activo seleccionado.
draw_plex(392, 218, .43)
text(462, 191, "PLEX", adj = c(0, .5), col = "#f0dda3", cex = 1.26, font = 2)
text(462, 220, "Pilot License Extension", adj = c(0, .5), col = "#8fa8b1", cex = .70)
segments(640, 176, 640, 248, col = "#36515d", lwd = 1)
draw_status_row(670, 202, "CREACIÓN DIARIA", "+18%  NOV → DIC", "#d2a95e")
draw_status_row(890, 202, "PRECIO Q4", "−3,4%", "#8fc0cb")
draw_status_row(1075, 202, "OCTUBRE", "+6%", "#d78261")

# Pestañas y ticker de acontecimientos.
segments(346, 261, 1232, 261, col = "#3b5965", lwd = 1)
text(370, 279, "DETAILS", adj = c(0, .5), col = "#718d97", cex = .62, font = 2)
text(472, 279, "PRICE HISTORY", adj = c(0, .5), col = "#e3c571", cex = .68, font = 2)
text(617, 279, "MARKET ORDERS", adj = c(0, .5), col = "#718d97", cex = .62, font = 2)
segments(468, 291, 592, 291, col = "#e3c571", lwd = 2)
text(1225, 279, "OCT  PLEX +6%   ·   NOV  FUEL/RIGS ↑   ·   14 DIC  VETERANS RETURN", adj = c(1, .5), col = "#849da5", cex = .57)

# Historial trimestral: barras observadas sobre el eje cero.
text(360, 320, "CHANGE %  //  OBSERVED Q4 MOVEMENT", adj = c(0, .5), col = "#85acb7", cex = .66, font = 2)
chart_left <- 650
baseline <- 850
scale_x <- 50
for (tick in c(-4, -2, 0, 2, 4, 6)) {
  x <- baseline + tick * scale_x
  segments(x, 342, x, 525, col = if (tick == 0) "#78939d" else alpha("#587582", .24), lwd = if (tick == 0) 1.4 else 1)
  text(x, 544, paste0(if (tick > 0) "+" else "", tick, "%"), col = "#69858f", cex = .54)
}
market_bars <- list(
  list(y = 370, item = "CPI GENERAL", signal = "PRICE INDEX", value = 4.7, color = "#8fc0cb"),
  list(y = 430, item = "TECH II", signal = "MODULES / SHIPS", value = 4.6, color = "#d78261"),
  list(y = 490, item = "PLEX", signal = "MARKET PRICE", value = -3.4, color = "#d7b65e")
)
for (bar in market_bars) {
  text(370, bar$y - 8, bar$item, adj = c(0, .5), col = "#d7e2e3", cex = .72, font = 2)
  text(370, bar$y + 14, bar$signal, adj = c(0, .5), col = "#6f8a94", cex = .52)
  x_end <- baseline + bar$value * scale_x
  segments(baseline, bar$y, x_end, bar$y, col = alpha(bar$color, .18), lwd = 20)
  segments(baseline, bar$y, x_end, bar$y, col = bar$color, lwd = 7)
  diamond(x_end, bar$y, 6, "#f0e6c9", bar$color, 1)
  value_label <- paste0(if (bar$value > 0) "+" else "−", format(abs(bar$value), decimal.mark = ",", nsmall = 1), "%")
  text(x_end + if (bar$value > 0) 15 else -15, bar$y, value_label, adj = c(if (bar$value > 0) 0 else 1, .5), col = bar$color, cex = .82, font = 2)
}

# Tabla inferior, análoga a un libro de órdenes pero sin cifras inventadas.
rect(346, 570, 1232, 603, col = "#142833", border = "#3f5c67")
columns <- c(365, 600, 810, 940, 1110)
headers <- c("ITEM", "SIGNAL", "Q4", "DIRECTION", "READOUT")
for (i in seq_along(headers)) text(columns[[i]], 586, headers[[i]], adj = c(0, .5), col = "#819da6", cex = .57, font = 2)
table_rows <- list(
  c("PLEX", "DAILY CREATION", "+18%", "SUPPLY ↑", "MORE UNITS"),
  c("PLEX", "MARKET PRICE", "−3,4%", "PRICE ↓", "SUPPLY EFFECT"),
  c("TECH II", "MARKET PRICE", "+4,6%", "PRICE ↑", "FLEET DEMAND"),
  c("CPI GENERAL", "PRICE INDEX", "+4,7%", "INDEX ↑", "BROAD BASKET")
)
row_y <- c(625, 669, 713, 757)
for (i in seq_along(table_rows)) {
  rect(346, row_y[[i]] - 20, 1232, row_y[[i]] + 20, col = if (i %% 2) "#0c1b26" else "#10212c", border = "#263f4a")
  if (i == 1L) rect(346, row_y[[i]] - 20, 351, row_y[[i]] + 20, col = "#d6b85e", border = NA)
  row <- table_rows[[i]]
  for (j in seq_along(row)) {
    value_col <- if (j == 3L) if (startsWith(row[[j]], "−")) "#d7b65e" else "#8fc0cb" else "#c6d3d5"
    text(columns[[j]], row_y[[i]], row[[j]], adj = c(0, .5), col = value_col, cex = .58, font = if (j %in% c(1, 3)) 2 else 1)
  }
}

# Panel de detalle y traza causal de la demanda.
rect(1250, 158, 1754, 834, col = "#0a1721", border = NA)
segments(1250, 157, 1250, 835, col = "#45616d", lwd = 1)
text(1276, 184, "MARKET DETAILS", adj = c(0, .5), col = "#809faa", cex = .64, font = 2)
text(1728, 184, "D-14", adj = c(1, .5), col = "#617d87", cex = .58)
draw_fleet(1500, 258, .54)
text(1500, 330, "RETURNING FLEETS", col = "#d8e3e4", cex = .76, font = 2)

trace_y <- c(390, 468, 546, 624)
trace_title <- c("14 DIC", "DEMANDA", "DESTINO", "RESULTADO")
trace_value <- c("REGRESAN VETERANOS", "MÁS ISK GASTADO", "RECONSTRUIR FLOTAS", "CPI / TECH II ↑")
trace_color <- c("#d78261", "#e0bd62", "#8fc0cb", "#8fc0cb")
segments(1302, min(trace_y), 1302, max(trace_y), col = "#3e5b67", lwd = 2)
for (i in seq_along(trace_y)) {
  circle(1302, trace_y[[i]], 8, fill = "#091722", border = trace_color[[i]], lwd = 2)
  diamond(1302, trace_y[[i]], 3, "#e9e0c7")
  text(1330, trace_y[[i]] - 10, trace_title[[i]], adj = c(0, .5), col = "#78949e", cex = .55, font = 2)
  text(1330, trace_y[[i]] + 13, trace_value[[i]], adj = c(0, .5), col = trace_color[[i]], cex = .70, font = 2)
}
segments(1276, 682, 1728, 682, col = "#314b57", lwd = 1)
text(1276, 710, "COUNTER-SIGNAL", adj = c(0, .5), col = "#78949e", cex = .55, font = 2)
text(1276, 742, "PLEX PRICE  −3,4%", adj = c(0, .5), col = "#d7b65e", cex = .78, font = 2)
text(1276, 773, "más oferta puede bajar un precio", adj = c(0, .5), col = "#bac9cc", cex = .65)
text(1276, 806, "NO TODO SUBE", adj = c(0, .5), col = "#e7c66f", cex = .66, font = 2)

# Procedencia y fechas fuera de la ventana.
text(45, 865, "Fuente: CCP · Quarterly Economic Newsletter, Q4 2010 · PLEX = Pilot License Extension", adj = c(0, .5), col = "#81969e", cex = .58)
text(1755, 865, paste0("DATOS: Q4 2010  ·  GENERADO: ", format(Sys.Date(), "%Y-%m-%d")), adj = c(1, .5), col = "#81969e", cex = .58)

dev.off()
message("EVE_R_OK: ", out_file)
