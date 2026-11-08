args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Uso: Rscript build-constellation.R <directorio_salida>")
}
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop("R no está leyendo UTF-8 correctamente; detenga la generación.")
}

out_dir <- normalizePath(args[[1]], winslash = "/", mustWork = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, "constellation-decisions.png")

W <- 1800
H <- 975
alpha <- function(col, value) grDevices::adjustcolor(col, alpha.f = value)

if (identical(.Platform$OS.type, "windows")) {
  grDevices::windowsFonts(
    videojuegos_ui = grDevices::windowsFont("Segoe UI"),
    videojuegos_display = grDevices::windowsFont("Georgia")
  )
  ui_family <- "videojuegos_ui"
  display_family <- "videojuegos_display"
} else {
  ui_family <- "sans"
  display_family <- "serif"
}

circle <- function(x, y, radius, fill = NA, border = NA, lwd = 1) {
  symbols(
    x,
    y,
    circles = radius,
    inches = FALSE,
    add = TRUE,
    bg = fill,
    fg = border,
    lwd = lwd
  )
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

star_polygon <- function(x, y, outer, inner, points = 8L, rotation = -pi / 2) {
  # Alternar radios largos y cortos crea las puntas; seno y coseno las sitúan
  # alrededor del centro (x, y).
  angle <- rotation + seq(0, 2 * pi, length.out = points * 2L + 1L)[-(points * 2L + 1L)]
  radius <- rep(c(outer, inner), points)
  cbind(x + cos(angle) * radius, y + sin(angle) * radius)
}

draw_icon <- function(x, y, radius, kind, accent) {
  ink <- "#e8e3d3"
  muted <- "#9caab5"
  dark <- "#09111e"
  s <- radius / 55

  if (kind == "resource") {
    polygon(
      c(x - 19, x + 19, x + 8, x - 8) * s + x * (1 - s),
      c(y - 24, y - 24, y - 3, y - 3) * s + y * (1 - s),
      col = alpha(ink, .92), border = dark, lwd = 1.5
    )
    polygon(
      c(x - 8, x + 8, x + 19, x - 19) * s + x * (1 - s),
      c(y + 3, y + 3, y + 24, y + 24) * s + y * (1 - s),
      col = alpha(ink, .92), border = dark, lwd = 1.5
    )
    segments(x - 23 * s, y - 29 * s, x + 23 * s, y - 29 * s, col = accent, lwd = 4)
    segments(x - 23 * s, y + 29 * s, x + 23 * s, y + 29 * s, col = accent, lwd = 4)
    polygon(c(x - 8 * s, x + 8 * s, x), c(y - 3 * s, y - 3 * s, y + 10 * s), col = accent, border = NA)
  }

  if (kind == "study") {
    polygon(c(x - 31 * s, x - 4 * s, x - 4 * s, x - 31 * s), c(y - 19 * s, y - 25 * s, y + 24 * s, y + 17 * s), col = accent, border = dark, lwd = 1.5)
    polygon(c(x + 4 * s, x + 31 * s, x + 31 * s, x + 4 * s), c(y - 25 * s, y - 19 * s, y + 17 * s, y + 24 * s), col = alpha(accent, .78), border = dark, lwd = 1.5)
    segments(x, y - 24 * s, x, y + 24 * s, col = ink, lwd = 2)
    segments(x - 23 * s, y - 8 * s, x - 9 * s, y - 11 * s, col = ink, lwd = 1)
    segments(x + 9 * s, y - 11 * s, x + 23 * s, y - 8 * s, col = ink, lwd = 1)
  }

  if (kind == "diploma") {
    rect(x - 29 * s, y - 22 * s, x + 29 * s, y + 18 * s, col = ink, border = dark, lwd = 1.5)
    segments(x - 20 * s, y - 10 * s, x + 18 * s, y - 10 * s, col = accent, lwd = 2)
    segments(x - 20 * s, y, x + 11 * s, y, col = muted, lwd = 1.5)
    circle(x + 16 * s, y + 15 * s, 10 * s, fill = accent, border = dark, lwd = 1)
    polygon(c(x + 9 * s, x + 15 * s, x + 19 * s, x + 24 * s), c(y + 21 * s, y + 34 * s, y + 23 * s, y + 33 * s), col = accent, border = dark)
  }

  if (kind == "specialize") {
    pts <- star_polygon(x, y, 32 * s, 14 * s, points = 8L)
    polygon(pts[, 1], pts[, 2], col = accent, border = ink, lwd = 1.5)
    circle(x, y, 7 * s, fill = dark, border = ink, lwd = 1)
  }

  if (kind == "work") {
    rect(x - 32 * s, y - 13 * s, x + 32 * s, y + 25 * s, col = alpha(accent, .82), border = dark, lwd = 2)
    lines(c(x - 17 * s, x - 11 * s, x + 11 * s, x + 17 * s), c(y - 13 * s, y - 28 * s, y - 28 * s, y - 13 * s), col = ink, lwd = 3)
    segments(x - 32 * s, y + 1 * s, x + 32 * s, y + 1 * s, col = ink, lwd = 2)
    rect(x - 5 * s, y - 3 * s, x + 5 * s, y + 7 * s, col = accent, border = dark)
  }

  if (kind == "experience") {
    circle(x, y - 22 * s, 9 * s, fill = accent, border = dark)
    circle(x - 25 * s, y + 18 * s, 9 * s, fill = ink, border = dark)
    circle(x + 25 * s, y + 18 * s, 9 * s, fill = ink, border = dark)
    segments(x, y - 13 * s, x - 20 * s, y + 10 * s, col = accent, lwd = 3)
    segments(x, y - 13 * s, x + 20 * s, y + 10 * s, col = accent, lwd = 3)
    segments(x - 16 * s, y + 18 * s, x + 16 * s, y + 18 * s, col = muted, lwd = 2)
  }

  if (kind == "promote") {
    rect(x - 31 * s, y + 6 * s, x - 13 * s, y + 28 * s, col = muted, border = dark)
    rect(x - 8 * s, y - 8 * s, x + 10 * s, y + 28 * s, col = alpha(accent, .72), border = dark)
    rect(x + 15 * s, y - 25 * s, x + 33 * s, y + 28 * s, col = accent, border = dark)
    arrows(x - 29 * s, y + 18 * s, x + 28 * s, y - 33 * s, col = ink, lwd = 2.5, length = .10)
  }

  if (kind == "business") {
    rect(x - 30 * s, y - 4 * s, x + 30 * s, y + 29 * s, col = ink, border = dark, lwd = 1.5)
    polygon(c(x - 36 * s, x, x + 36 * s), c(y - 6 * s, y - 34 * s, y - 6 * s), col = accent, border = dark, lwd = 2)
    rect(x - 8 * s, y + 6 * s, x + 8 * s, y + 29 * s, col = accent, border = dark)
    rect(x - 25 * s, y + 3 * s, x - 15 * s, y + 15 * s, col = alpha(accent, .7), border = dark)
    rect(x + 15 * s, y + 3 * s, x + 25 * s, y + 15 * s, col = alpha(accent, .7), border = dark)
  }

  if (kind == "invest") {
    for (offset in c(16, 5, -6)) {
      ellipse <- seq(0, 2 * pi, length.out = 80)
      polygon(x - 12 * s + cos(ellipse) * 22 * s, y + offset * s + sin(ellipse) * 8 * s, col = alpha(accent, .82), border = dark)
    }
    arrows(x + 8 * s, y + 25 * s, x + 31 * s, y - 28 * s, col = ink, lwd = 3, length = .11)
  }

  if (kind == "scale") {
    rect(x - 30 * s, y + 13 * s, x - 10 * s, y + 30 * s, col = muted, border = dark)
    rect(x - 6 * s, y - 3 * s, x + 14 * s, y + 30 * s, col = alpha(accent, .75), border = dark)
    rect(x + 18 * s, y - 21 * s, x + 38 * s, y + 30 * s, col = accent, border = dark)
    arrows(x - 28 * s, y + 20 * s, x + 34 * s, y - 31 * s, col = ink, lwd = 2.5, length = .10)
  }
}

draw_medallion <- function(x, y, radius, accent, kind, primary = FALSE) {
  circle(x, y, radius * 1.30, fill = alpha(accent, .035), border = NA)
  circle(x, y, radius * 1.17, fill = alpha(accent, .065), border = NA)
  circle(x, y, radius, fill = "#050a13", border = "#02050a", lwd = 3)
  circle(x, y, radius * .91, fill = "#101b2b", border = accent, lwd = if (primary) 3 else 2)
  circle(x, y, radius * .72, fill = "#142238", border = "#697887", lwd = 1)
  mark <- radius * .10
  diamond(x, y - radius * .88, mark, accent, "#050a13", 1)
  diamond(x, y + radius * .88, mark, "#7d8b98", "#050a13", 1)
  diamond(x - radius * .88, y, mark, "#7d8b98", "#050a13", 1)
  diamond(x + radius * .88, y, mark, "#7d8b98", "#050a13", 1)
  draw_icon(x, y, radius * .74, kind, accent)
}

draw_label <- function(x, y, title, subtitle, side, primary = FALSE) {
  offset <- if (primary) 80 else 69
  if (identical(side, "left")) {
    tx <- x - offset
    adj <- 1
  } else {
    tx <- x + offset
    adj <- 0
  }
  text(tx, y - 10, title, adj = c(adj, .5), col = "#f0e8d4", cex = if (primary) 1.00 else .88, font = 2, family = if (primary) display_family else ui_family)
  text(tx, y + 20, subtitle, adj = c(adj, .5), col = "#b2c0ca", cex = .70, family = ui_family)
}

png(out_file, width = W, height = H, res = 150, bg = "#09111f")
par(mar = rep(0, 4), xaxs = "i", yaxs = "i", family = ui_family)
plot.new()
plot.window(c(0, W), c(H, 0), asp = 1)

# Cielo vertical.
top_col <- grDevices::col2rgb("#172a45")
bottom_col <- grDevices::col2rgb("#070d18")
steps <- 600L
gradient <- vapply(
  seq(0, 1, length.out = steps),
  function(t) {
    mixed <- (top_col * (1 - t) + bottom_col * t) / 255
    grDevices::rgb(mixed[[1]], mixed[[2]], mixed[[3]])
  },
  character(1)
)
rasterImage(as.raster(matrix(gradient, ncol = 1)), 0, 0, W, H, interpolate = TRUE)

# Aurora en bandas, construida sin imágenes externas.
xspline(c(-120, 250, 620, 1020, 1460, 1920), c(250, 150, 245, 125, 210, 105), shape = -.35, border = alpha("#83c6b7", .08), lwd = 62)
xspline(c(-160, 180, 540, 900, 1320, 1900), c(300, 190, 285, 170, 250, 145), shape = -.30, border = alpha("#79a8c8", .10), lwd = 35)
xspline(c(900, 1150, 1420, 1710, 1900), c(160, 100, 170, 125, 190), shape = -.25, border = alpha("#c58a88", .07), lwd = 26)

# Campo estelar.
set.seed(1507)
stars <- data.frame(
  x = runif(125, 30, W - 30),
  y = runif(125, 35, H - 65),
  size = sample(c(.35, .45, .60, .85, 1.15), 125, replace = TRUE, prob = c(25, 30, 25, 15, 5))
)
points(stars$x, stars$y, pch = 16, cex = stars$size, col = sample(c(alpha("#e7eadf", .72), alpha("#a9bdcb", .68), alpha("#efdca7", .68)), 125, replace = TRUE))

# Cordillera nórdica en silueta.
mountain_x <- c(0, 135, 245, 390, 520, 690, 830, 1010, 1190, 1370, 1510, 1665, 1800, 1800, 0)
mountain_y <- c(735, 610, 690, 520, 700, 575, 710, 555, 695, 535, 685, 585, 720, 975, 975)
polygon(mountain_x, mountain_y, col = "#08111c", border = NA)
lines(mountain_x[1:13], mountain_y[1:13], col = "#2a3d50", lwd = 1.5)

# Marco angular y cabecera.
lines(c(42, 42, 120), c(112, 38, 38), col = alpha("#aebdca", .55), lwd = 1.3)
lines(c(W - 42, W - 42, W - 120), c(112, 38, 38), col = alpha("#aebdca", .55), lwd = 1.3)
segments(72, 111, 1728, 111, col = "#61788b", lwd = 1)
diamond(72, 111, 5, "#e1be65")
text(72, 44, "CONSTELACIÓN DE ELECCIONES", adj = c(0, .5), col = "#f0ddb0", cex = 1.45, font = 2, family = display_family)
text(72, 81, "No puedes desbloquear todos los caminos con los mismos recursos.", adj = c(0, .5), col = "#b8c5cf", cex = .98)
text(1728, 39, "PUNTOS DE DECISIÓN", adj = c(1, .5), col = "#9fb4c4", cex = .78, font = 2)
text(1728, 73, "◆  3 DISPONIBLES", adj = c(1, .5), col = "#f0ddb0", cex = .90, font = 2, family = display_family)

accent <- c(study = "#e1bd62", work = "#82b4c3", business = "#d78261")
root <- c(900, 805)
branches <- list(
  study = list(
    path = "SENDA DEL SABER",
    side = "left",
    benefit = "CAPITAL HUMANO",
    cost = "INGRESO PRESENTE",
    nodes = data.frame(
      x = c(640, 430, 270), y = c(620, 450, 250),
      icon = c("study", "diploma", "specialize"),
      title = c("ESTUDIAR", "TÍTULO", "ESPECIALIZACIÓN"),
      subtitle = c("capital humano", "más opciones laborales", "salario potencial")
    )
  ),
  work = list(
    path = "SENDA DEL OFICIO",
    side = "left",
    benefit = "INGRESO + EXPERIENCIA",
    cost = "TIEMPO DE FORMACIÓN",
    nodes = data.frame(
      x = c(900, 900, 900), y = c(600, 415, 240),
      icon = c("work", "experience", "promote"),
      title = c("TRABAJAR", "EXPERIENCIA", "ASCENSO"),
      subtitle = c("ingreso y experiencia", "red laboral", "estabilidad futura")
    )
  ),
  business = list(
    path = "SENDA DEL RIESGO",
    side = "right",
    benefit = "AUTONOMÍA + CONTROL",
    cost = "ESTABILIDAD",
    nodes = data.frame(
      x = c(1160, 1370, 1530), y = c(620, 450, 250),
      icon = c("business", "invest", "scale"),
      title = c("EMPRENDER", "INVERTIR", "ESCALAR"),
      subtitle = c("autonomía y control", "control del proyecto", "riesgo y retorno")
    )
  )
)

# Conexiones luminosas.
for (kind in names(branches)) {
  branch <- branches[[kind]]
  x <- c(root[[1]], branch$nodes$x)
  y <- c(root[[2]], branch$nodes$y)
  segments(head(x, -1), head(y, -1), tail(x, -1), tail(y, -1), col = alpha(accent[[kind]], .08), lwd = 22)
  segments(head(x, -1), head(y, -1), tail(x, -1), tail(y, -1), col = "#26364d", lwd = 7)
  segments(head(x, -1), head(y, -1), tail(x, -1), tail(y, -1), col = accent[[kind]], lwd = 2.5)
  mid_x <- (head(x, -1) + tail(x, -1)) / 2
  mid_y <- (head(y, -1) + tail(y, -1)) / 2
  for (i in seq_along(mid_x)) diamond(mid_x[[i]], mid_y[[i]], 5, "#f1e7c8", accent[[kind]], 1)
}

# Nodos, etiquetas y coste de oportunidad.
for (kind in names(branches)) {
  branch <- branches[[kind]]
  for (i in seq_len(nrow(branch$nodes))) {
    radius <- if (i == 1L) 61 else 48
    draw_medallion(branch$nodes$x[[i]], branch$nodes$y[[i]], radius, accent[[kind]], branch$nodes$icon[[i]], primary = i == 1L)
    node_side <- branch$side
    if (identical(kind, "study") && i == 3L) node_side <- "right"
    draw_label(branch$nodes$x[[i]], branch$nodes$y[[i]], branch$nodes$title[[i]], branch$nodes$subtitle[[i]], node_side, primary = i == 1L)
  }

  tip <- branch$nodes[3, ]
  if (identical(branch$side, "left")) {
    text(tip$x - 68, tip$y - 67, branch$path, adj = c(1, .5), col = accent[[kind]], cex = .72, font = 2)
  } else {
    text(tip$x + 68, tip$y - 67, branch$path, adj = c(0, .5), col = accent[[kind]], cex = .72, font = 2)
  }

  main <- branch$nodes[1, ]
  trade_x <- unname(c(study = 500, work = 900, business = 1400)[[kind]])
  text(trade_x, 704, paste0("+  ", branch$benefit), adj = c(.5, .5), col = accent[[kind]], cex = .68, font = 2)
  text(trade_x, 731, paste0("×  COSTE: ", branch$cost), adj = c(.5, .5), col = "#8f9aa5", cex = .60, font = 2)
}

# Recurso común y fichas disponibles.
draw_medallion(root[[1]], root[[2]], 63, "#e2bd63", "resource", primary = TRUE)
text(root[[1]], 884, "RECURSOS ESCASOS", col = "#f0ddb0", cex = .88, font = 2, family = display_family)
for (item in list(c(785, "TIEMPO"), c(900, "DINERO"), c(1025, "ATENCIÓN"))) {
  x <- as.numeric(item[[1]])
  rect(x - 50, 910, x + 50, 940, col = "#121d2d", border = "#6f8090", lwd = 1)
  text(x, 925, item[[2]], col = "#c7d1d7", cex = .64, font = 2)
}

text(1728, 956, "Elegir también es renunciar.", adj = c(1, .5), col = "#9fb4c4", cex = .75)
dev.off()

message("CONSTELLATION_R_OK: ", out_file)
