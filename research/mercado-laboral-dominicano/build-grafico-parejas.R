#!/usr/bin/env Rscript

options(encoding = "UTF-8", scipen = 999)

if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop(
    "R no esta leyendo UTF-8. Ejecute render-grafico-parejas.ps1 para limpiar el locale heredado.",
    call. = FALSE
  )
}

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_argument)) stop("No se pudo localizar el script.", call. = FALSE)
module_dir <- dirname(normalizePath(sub("^--file=", "", script_argument[[1]]), winslash = "/"))

sql_path <- file.path(module_dir, "sql", "grafico_parejas_similitud.sql")
figure_dir <- file.path(module_dir, "figuras")
processed_dir <- file.path(module_dir, "data", "procesados")
qa_path <- file.path(module_dir, "qa-parejas.csv")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

password <- Sys.getenv("PG_RECOVERY_PASSWORD", unset = "")
assert_true(nzchar(password), "Defina PG_RECOVERY_PASSWORD para consultar PostgreSQL.")

psql <- Sys.getenv("PSQL", unset = "C:/Program Files/PostgreSQL/18/bin/psql.exe")
assert_true(file.exists(psql), paste("No se encontro psql:", psql))
assert_true(file.exists(sql_path), paste("No se encontro la consulta:", sql_path))

old_pgpassword <- Sys.getenv("PGPASSWORD", unset = NA_character_)
Sys.setenv(PGPASSWORD = password)
on.exit({
  if (is.na(old_pgpassword)) {
    Sys.unsetenv("PGPASSWORD")
  } else {
    Sys.setenv(PGPASSWORD = old_pgpassword)
  }
}, add = TRUE)

stderr_path <- tempfile(fileext = ".log")
psql_args <- c(
  "-X", "-w",
  "-h", "127.0.0.1",
  "-p", "5433",
  "-U", "postgres",
  "-d", "censo_2022",
  "--csv",
  "-f", shQuote(sql_path)
)

output <- system2(psql, args = psql_args, stdout = TRUE, stderr = stderr_path)
exit_status <- attr(output, "status")
if (is.null(exit_status)) exit_status <- 0L
if (exit_status != 0L) {
  error_text <- if (file.exists(stderr_path)) paste(readLines(stderr_path, warn = FALSE), collapse = "\n") else ""
  stop("psql fallo al preparar el grafico:\n", error_text, call. = FALSE)
}

datos <- read.csv(
  text = paste(output, collapse = "\n"),
  stringsAsFactors = FALSE,
  check.names = FALSE,
  encoding = "UTF-8"
)

assert_true(nrow(datos) == 4L, "La consulta debe devolver exactamente cuatro indicadores.")
assert_true(identical(sort(datos$orden), 1:4), "El orden de indicadores cambio.")
assert_true(all(datos$errores_qa == 0L), "Hay controles de calidad en estado error.")
assert_true(length(unique(datos$parejas_base)) == 1L, "El universo base no es consistente.")
assert_true(unique(datos$parejas_base) == 1731814, "Cambio el universo validado de parejas con edades plausibles.")
assert_true(all(datos$iguales >= 0 & datos$iguales <= datos$denominador), "Numeradores fuera del denominador.")
assert_true(all(datos$denominador > 0 & datos$denominador <= datos$parejas_base), "Denominadores invalidos.")
assert_true(all(datos$porcentaje >= 0 & datos$porcentaje <= 100), "Porcentajes fuera de 0-100.")
assert_true(all(datos$cobertura > 0 & datos$cobertura <= 100), "Coberturas fuera de 0-100.")

etiquetas <- c(
  situacion_ocupacional = "Misma situación ocupacional",
  nivel_educativo = "Mismo nivel educativo",
  campo_estudio = "Mismo campo amplio de estudio",
  grupo_ocupacional = "Mismo gran grupo ocupacional"
)

datos$indicador <- unname(etiquetas[datos$indicador_id])
assert_true(!anyNA(datos$indicador), "Aparecio un indicador sin etiqueta editorial.")

format_num <- function(value) {
  format(round(value), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

format_pct <- function(value, digits = 1L) {
  paste0(formatC(value, digits = digits, format = "f", decimal.mark = ","), "%")
}

datos$porcentaje_etiqueta <- vapply(datos$porcentaje, format_pct, character(1))
datos$cobertura_etiqueta <- vapply(datos$cobertura, format_pct, character(1))
datos$denominador_etiqueta <- vapply(datos$denominador, format_num, character(1))
datos$fecha_tecnica <- as.character(Sys.Date())

data_path <- file.path(processed_dir, "10_similitud_parejas.csv")
write.csv(datos, data_path, row.names = FALSE, fileEncoding = "UTF-8", na = "")

qa <- data.frame(
  chequeo = c(
    "R lee UTF-8",
    "Controles PostgreSQL sin errores",
    "Cuatro indicadores presentes",
    "Universo de edades plausibles",
    "Numeradores dentro de denominadores",
    "Porcentajes y coberturas en rango"
  ),
  estado = "OK",
  detalle = c(
    "l10n_info UTF-8 = TRUE",
    "meta.controles_calidad_analitica: 0 errores",
    "situacion ocupacional, educacion, campo y ocupacion",
    format_num(unique(datos$parejas_base)),
    "0 <= iguales <= denominador",
    "0-100"
  ),
  stringsAsFactors = FALSE
)
write.csv(qa, qa_path, row.names = FALSE, fileEncoding = "UTF-8", na = "")

pal <- list(
  terracota = "#c86448",
  plomo = "#343a40",
  oliva = "#6b7554",
  texto = "#1a1512",
  texto_soft = "#4a443e",
  texto_muted = "#73695f",
  crema = "#faf8f3",
  blanco = "#ffffff",
  gris_claro = "#f5f1e8",
  border = "#e6dfd5",
  border_dark = "#d4c9ba"
)

render_chart <- function(open_device) {
  open_device()
  on.exit(dev.off(), add = TRUE)

  par(
    mar = c(2.2, 1.0, 1.0, 1.0),
    oma = c(0, 0, 0, 0),
    xaxs = "i",
    yaxs = "i",
    family = "sans",
    bg = pal$crema
  )

  plot.new()
  plot.window(xlim = c(-4, 104), ylim = c(0, 6.4), xaxs = "i", yaxs = "i")

  text(
    -1.5, 6.05,
    "Similitud educativa y laboral en parejas convivientes",
    adj = c(0, 0.5), cex = 1.65, font = 2, col = pal$texto, xpd = NA
  )
  text(
    -1.5, 5.62,
    "Porcentaje con la misma característica · parejas jefatura–cónyuge con edades plausibles · Censo 2022",
    adj = c(0, 0.5), cex = 0.92, col = pal$texto_soft, xpd = NA
  )

  rect(-1.5, 0.75, 101.5, 5.18, col = pal$blanco, border = pal$border_dark, lwd = 0.7)

  guide_values <- c(0, 25, 50, 75, 100)
  for (guide in guide_values) {
    segments(guide, 1.05, guide, 4.72, col = pal$border, lty = "dotted", lwd = 0.8)
    text(guide, 0.91, paste0(guide, "%"), cex = 0.68, col = pal$texto_muted)
  }

  y_positions <- c(4.45, 3.40, 2.35, 1.30)
  bar_half_height <- 0.20

  for (index in seq_len(nrow(datos))) {
    y <- y_positions[[index]]
    pct <- datos$porcentaje[[index]]

    text(
      0, y + 0.38, datos$indicador[[index]],
      adj = c(0, 0.5), cex = 0.91, font = 2, col = pal$texto
    )
    coverage_note <- paste0(
      "n = ", datos$denominador_etiqueta[[index]],
      " · cobertura ", datos$cobertura_etiqueta[[index]]
    )
    text(
      100, y + 0.38, coverage_note,
      adj = c(1, 0.5), cex = 0.69, col = pal$texto_muted
    )

    rect(
      0, y - bar_half_height, 100, y + bar_half_height,
      col = pal$gris_claro, border = pal$border_dark, lwd = 0.7
    )
    rect(
      0, y - bar_half_height, pct, y + bar_half_height,
      col = pal$terracota, border = pal$plomo, lwd = 0.55
    )
    text(
      pct - 1.3, y, datos$porcentaje_etiqueta[[index]],
      adj = c(1, 0.5), cex = 0.88, font = 2, col = pal$blanco
    )
  }

  text(
    -1.5, 0.49,
    "La parte clara representa parejas que no coinciden en la característica. Cada fila usa sólo casos con datos para ambos miembros.",
    adj = c(0, 0.5), cex = 0.66, col = pal$texto_soft, xpd = NA
  )
  text(
    -1.5, 0.26,
    "Situación ocupacional: ambas personas ocupadas o ambas no ocupadas.",
    adj = c(0, 0.5), cex = 0.64, col = pal$texto_soft, xpd = NA
  )
  text(
    -1.5, 0.04,
    paste0(
      "Fuente: ONE, X Censo Nacional de Población y Vivienda 2022 · ",
      "universo base: ", format_num(unique(datos$parejas_base)), " parejas · elaboración: Leonardo Mena"
    ),
    adj = c(0, 0.5), cex = 0.62, col = pal$texto_muted, xpd = NA
  )
}

png_path <- file.path(figure_dir, "10_similitud_parejas.png")
svg_path <- file.path(figure_dir, "10_similitud_parejas.svg")

render_chart(function() {
  png(
    filename = png_path,
    width = 3200,
    height = 2080,
    units = "px",
    res = 320,
    type = if (capabilities("cairo")) "cairo" else "windows",
    bg = pal$crema
  )
})

render_chart(function() {
  svg(
    filename = svg_path,
    width = 10,
    height = 6.5,
    pointsize = 12,
    family = "sans",
    bg = pal$crema,
    onefile = TRUE
  )
})

message("Grafico generado: ", png_path)
message("Grafico generado: ", svg_path)
message("Datos procesados: ", data_path)
message("QA: ", qa_path)
