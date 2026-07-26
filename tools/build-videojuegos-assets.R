args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("Uso: Rscript build-videojuegos-assets.R <directorio_salida>")

out_dir <- normalizePath(args[[1]], winslash = "/", mustWork = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

draw_hex <- function(fill, border = "#16252b") {
  polygon(
    c(.50, .86, .86, .50, .14, .14),
    c(.06, .27, .73, .94, .73, .27),
    col = "#111c21", border = NA
  )
  polygon(
    c(.50, .84, .84, .50, .16, .16),
    c(.09, .29, .71, .91, .71, .29),
    col = fill, border = border, lwd = 3
  )
  segments(.25, .23, .75, .23, col = grDevices::adjustcolor("#ffffff", alpha.f = .17), lwd = 2)
}

draw_sprite <- function(kind, file) {
  png(file, width = 192, height = 192, res = 144, bg = "transparent")
  par(mar = rep(0, 4), xaxs = "i", yaxs = "i", bg = "transparent")
  plot.new()
  plot.window(c(0, 1), c(0, 1), asp = 1)

  fills <- c(
    agriculture = "#5c7b4d",
    commerce = "#5a8594",
    public = "#8c887c",
    tourism = "#bc704e",
    manufacturing = "#8d6d51"
  )
  draw_hex(fills[[kind]])

  if (kind == "agriculture") {
    # Tierra y haz de trigo.
    polygon(c(.25, .75, .69, .31), c(.21, .21, .31, .31), col = "#304b32", border = NA)
    for (x in c(.37, .50, .63)) {
      segments(x, .29, x + (x - .50) * .18, .72, col = "#263d2b", lwd = 5)
      segments(x + (x - .50) * .18, .56, x - .08, .65, col = "#d9bd59", lwd = 4)
      segments(x + (x - .50) * .18, .64, x + .08, .73, col = "#f0d47c", lwd = 4)
      points(x + (x - .50) * .18, .77, pch = 16, cex = 1.6, col = "#f0d47c")
    }
    segments(.25, .31, .75, .31, col = "#d6b85d", lwd = 3)
  }

  if (kind == "commerce") {
    # Puesto de mercado, toldo y cajas.
    polygon(c(.24, .76, .69, .31), c(.63, .63, .78, .78), col = "#e0ba64", border = "#17252a", lwd = 2)
    rect(.27, .29, .73, .63, col = "#d7d6c4", border = "#17252a", lwd = 3)
    rect(.43, .29, .57, .55, col = "#6f8f98", border = "#17252a", lwd = 2)
    rect(.29, .35, .40, .48, col = "#d78c54", border = "#17252a", lwd = 2)
    rect(.60, .35, .71, .48, col = "#d1ad58", border = "#17252a", lwd = 2)
    segments(.31, .70, .69, .70, col = "#fff0c4", lwd = 3)
  }

  if (kind == "public") {
    # Edificio publico reconocible.
    polygon(c(.22, .78, .50), c(.64, .64, .82), col = "#e2d0a2", border = "#17252a", lwd = 3)
    rect(.27, .27, .73, .64, col = "#d5d1c5", border = "#17252a", lwd = 3)
    for (x in c(.35, .50, .65)) {
      rect(x - .035, .34, x + .035, .61, col = "#f3e6bf", border = "#5c645e", lwd = 1)
    }
    segments(.23, .27, .77, .27, col = "#e4bd66", lwd = 4)
    segments(.50, .82, .50, .88, col = "#17252a", lwd = 2)
    polygon(c(.50, .89, .50), c(.88, .85, .82), col = "#c66b47", border = NA)
  }

  if (kind == "tourism") {
    # Hotel y palmera: una silueta distinta al comercio.
    rect(.34, .25, .70, .67, col = "#e3d6b7", border = "#17252a", lwd = 3)
    polygon(c(.30, .74, .67, .37), c(.67, .67, .80, .80), col = "#e0b15d", border = "#17252a", lwd = 3)
    for (x in c(.43, .55, .65)) {
      rect(x - .035, .48, x + .035, .58, col = "#6f9aab", border = "#17252a", lwd = 1)
    }
    rect(.49, .25, .57, .42, col = "#c66b47", border = "#17252a", lwd = 2)
    segments(.27, .26, .27, .69, col = "#263d2b", lwd = 5)
    segments(.27, .62, .16, .73, col = "#6d9a5c", lwd = 4)
    segments(.27, .66, .39, .76, col = "#6d9a5c", lwd = 4)
    segments(.27, .56, .14, .60, col = "#6d9a5c", lwd = 4)
  }

  if (kind == "manufacturing") {
    # Fabrica, chimeneas y rueda dentada.
    rect(.24, .27, .76, .63, col = "#c9c2ae", border = "#17252a", lwd = 3)
    polygon(c(.24, .76, .76, .24), c(.63, .63, .79, .79), col = "#b97755", border = "#17252a", lwd = 3)
    rect(.31, .63, .39, .86, col = "#6b746d", border = "#17252a", lwd = 3)
    rect(.61, .63, .69, .82, col = "#6b746d", border = "#17252a", lwd = 3)
    points(.52, .48, pch = 16, cex = 2.4, col = "#e1b45d")
    points(.52, .48, pch = 1, cex = 1.8, col = "#17252a", lwd = 2)
    for (x in c(.35, .50, .65)) rect(x - .035, .31, x + .035, .42, col = "#6f9aab", border = "#17252a", lwd = 1)
  }

  dev.off()
}

draw_sprite("agriculture", file.path(out_dir, "sprite-agriculture.png"))
draw_sprite("commerce", file.path(out_dir, "sprite-commerce.png"))
draw_sprite("public", file.path(out_dir, "sprite-public.png"))
draw_sprite("tourism", file.path(out_dir, "sprite-tourism.png"))
draw_sprite("manufacturing", file.path(out_dir, "sprite-manufacturing.png"))
