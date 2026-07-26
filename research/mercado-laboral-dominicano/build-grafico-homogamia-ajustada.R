#!/usr/bin/env Rscript

options(encoding = "UTF-8", scipen = 999)

if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop(
    "R no esta leyendo UTF-8. Ejecute render-grafico-homogamia-ajustada.ps1.",
    call. = FALSE
  )
}

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_argument)) stop("No se pudo localizar el script.", call. = FALSE)
module_dir <- dirname(normalizePath(sub("^--file=", "", script_argument[[1]]), winslash = "/"))

sql_path <- file.path(module_dir, "sql", "grafico_homogamia_ajustada.sql")
figure_dir <- file.path(module_dir, "figuras")
processed_dir <- file.path(module_dir, "data", "procesados")
qa_path <- file.path(module_dir, "qa-homogamia-ajustada.csv")
data_path <- file.path(processed_dir, "11_homogamia_ajustada.csv")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

use_cache <- identical(Sys.getenv("HOMOGAMIA_USE_CACHE", unset = "0"), "1")

if (use_cache) {
  assert_true(file.exists(data_path), paste("No existe el extracto cacheado:", data_path))
  datos <- read.csv(
    data_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    encoding = "UTF-8"
  )
} else {
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
    "-q", "-X", "-w",
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
    error_text <- if (file.exists(stderr_path)) {
      paste(readLines(stderr_path, warn = FALSE), collapse = "\n")
    } else {
      ""
    }
    stop("psql fallo al calcular la homogamia ajustada:\n", error_text, call. = FALSE)
  }

  datos <- read.csv(
    text = paste(output, collapse = "\n"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    encoding = "UTF-8"
  )
}

assert_true(nrow(datos) == 12L, "La consulta debe devolver 12 indicadores.")
assert_true(!anyDuplicated(datos$indicador_id), "Hay indicadores duplicados.")
assert_true(all(datos$errores_qa == 0L), "Hay controles PostgreSQL en estado error.")
assert_true(length(unique(datos$parejas_base)) == 1L, "El universo base no es consistente.")
assert_true(unique(datos$parejas_base) == 1731814, "Cambio el universo de parejas con edades plausibles.")
assert_true(all(datos$n > 0 & datos$n <= datos$parejas_base), "Denominadores invalidos.")
assert_true(all(datos$observado_n >= 0 & datos$observado_n <= datos$n), "Conteos observados invalidos.")
assert_true(all(datos$observado_pct >= 0 & datos$observado_pct <= 100), "Observados fuera de rango.")
assert_true(all(datos$esperado_pct >= 0 & datos$esperado_pct <= 100), "Esperados fuera de rango.")
assert_true(all(datos$kappa >= -1 & datos$kappa <= 1), "Kappa fuera de rango.")
assert_true(all(datos$cobertura_pct > 0 & datos$cobertura_pct <= 100), "Cobertura fuera de rango.")
assert_true(all(datos$categorias_jefatura >= 2 & datos$categorias_pareja >= 2), "Indicador sin variacion.")

controles_replicados <- data.frame(
  indicador_id = c(
    "situacion_ocupacional",
    "nivel_educativo",
    "campo_estudio",
    "grupo_ocupacional"
  ),
  observado_pct_control = c(60.2628, 55.4944, 32.0277, 21.3359),
  esperado_pct_control = c(56.7852, 31.2154, 21.4162, 14.7189),
  stringsAsFactors = FALSE
)

replica <- merge(
  controles_replicados,
  datos[, c("indicador_id", "observado_pct", "esperado_pct")],
  by = "indicador_id",
  all.x = TRUE,
  sort = FALSE
)
assert_true(nrow(replica) == 4L && !anyNA(replica$observado_pct), "Faltan controles replicados.")
assert_true(
  all(abs(replica$observado_pct - replica$observado_pct_control) < 0.0001),
  "No coincide la homogamia observada previamente validada."
)
assert_true(
  all(abs(replica$esperado_pct - replica$esperado_pct_control) < 0.0001),
  "No coincide la homogamia esperada previamente validada."
)

etiquetas <- c(
  grupo_edad_10 = "Grupo de edad",
  alfabetismo = "Sabe leer y escribir",
  trayectoria_escolar = "Asistencia o trayectoria escolar",
  nivel_educativo = "Nivel educativo",
  nivel_curso_aprobado = "Nivel y curso aprobado",
  campo_estudio = "Campo de estudio superior*",
  graduacion_superior = "Graduación en educación superior",
  anos_educacion_superior = "Años de educación superior",
  dificultad_funcional = "Alguna dificultad funcional",
  uso_escritorio = "Uso de computadora de escritorio",
  uso_portatil = "Uso de computadora portátil",
  uso_tableta = "Uso de tableta",
  uso_smartphone = "Uso de smartphone",
  uso_internet = "Uso de internet",
  situacion_ocupacional = "Condición de ocupación",
  busqueda_trabajo = "Búsqueda de trabajo",
  actividad_principal = "Actividad principal si no trabaja",
  disponibilidad_trabajo = "Disponibilidad para trabajar",
  experiencia_laboral = "Experiencia laboral previa",
  grupo_ocupacional = "Gran grupo ocupacional",
  categoria_ocupacional = "Categoría ocupacional†",
  autoidentificacion = "Autoidentificación etnorracial"
)

datos$indicador_editorial <- unname(etiquetas[datos$indicador_id])
assert_true(!anyNA(datos$indicador_editorial), "Hay indicadores sin etiqueta editorial.")

# Seleccion sustantiva: evita que practicas susceptibles a la convivencia y
# familias de preguntas casi equivalentes dominen la figura.
# El CSV conserva los 12 indicadores calculados y permite auditar la seleccion.
representativos_ids <- c(
  "grupo_edad_10",
  "nivel_educativo",
  "campo_estudio",
  "dificultad_funcional",
  "categoria_ocupacional",
  "autoidentificacion"
)

seleccion <- datos[
  datos$indicador_id %in% representativos_ids,
  ,
  drop = FALSE
]
seleccion <- seleccion[order(-seleccion$kappa, -seleccion$cobertura_pct), , drop = FALSE]

assert_true(nrow(seleccion) == 6L, "La seleccion editorial debe contener seis indicadores.")
assert_true(all(seleccion$kappa > 0), "La figura debe mostrar asociaciones positivas.")

datos$seleccion_grafico <- datos$indicador_id %in% seleccion$indicador_id
datos$fecha_tecnica <- as.character(Sys.Date())

write.csv(datos, data_path, row.names = FALSE, fileEncoding = "UTF-8", na = "")

format_pct_es <- function(value, digits = 1L) {
  paste0(formatC(value, digits = digits, format = "f", decimal.mark = ","), "%")
}

format_num_es <- function(value) {
  format(round(value), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

seleccion <- seleccion[order(-seleccion$kappa), , drop = FALSE]
seleccion$etiqueta_esperado <- paste0(
  "E ", vapply(seleccion$esperado_pct, format_pct_es, character(1))
)
seleccion$etiqueta_observado <- paste0(
  "O ", vapply(seleccion$observado_pct, format_pct_es, character(1))
)
seleccion$etiqueta_indice <- paste0(
  "κ = ", formatC(seleccion$kappa, digits = 2, format = "f", decimal.mark = ","),
  "  |  cob. ", vapply(seleccion$cobertura_pct, format_pct_es, character(1))
)

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
    mar = c(0.5, 0.5, 0.5, 0.5),
    oma = c(0, 0, 0, 0),
    xaxs = "i",
    yaxs = "i",
    family = "sans",
    bg = pal$crema
  )

  plot.new()
  plot.window(xlim = c(-43, 125), ylim = c(-1.05, 12.35), xaxs = "i", yaxs = "i")

  text(
    -40.5, 11.95,
    "Características personales que comparten las parejas",
    adj = c(0, 0.5), cex = 1.65, font = 2, col = pal$texto, xpd = NA
  )
  text(
    -40.5, 11.42,
    "Cuatro rasgos generales y dos subuniversos específicos  |  observado (O) frente a esperado por los marginales (E)",
    adj = c(0, 0.5), cex = 0.92, col = pal$texto_soft, xpd = NA
  )

  rect(-41, 0.72, 122.5, 10.98, col = pal$blanco, border = pal$border_dark, lwd = 0.7)

  guide_values <- c(0, 25, 50, 75, 100)
  for (guide in guide_values) {
    segments(guide, 1.28, guide, 10.55, col = pal$border, lty = "dotted", lwd = 0.85)
    text(guide, 0.99, paste0(guide, "%"), cex = 0.67, col = pal$texto_muted)
  }
  segments(100, 0.86, 100, 10.82, col = pal$border_dark, lwd = 0.55)

  y_positions <- c(9.65, 8.15, 6.65, 5.15, 3.25, 1.75)
  text(
    -39.2, 10.48, "RASGOS GENERALES",
    adj = c(0, 0.5), cex = 0.60, font = 2, col = pal$oliva
  )
  segments(-39.2, 4.28, 119.5, 4.28, col = pal$border, lwd = 0.8)
  text(
    -39.2, 4.56, "SUBUNIVERSOS ESPECÍFICOS",
    adj = c(0, 0.5), cex = 0.60, font = 2, col = pal$terracota
  )
  for (index in seq_len(nrow(seleccion))) {
    row <- seleccion[index, ]
    y <- y_positions[[index]]

    text(
      -1.7, y, row$indicador_editorial,
      adj = c(1, 0.5), cex = 0.82, font = 2, col = pal$texto
    )
    segments(
      row$esperado_pct, y, row$observado_pct, y,
      col = pal$border_dark, lwd = 3.2, lend = 1
    )
    points(
      row$esperado_pct, y,
      pch = 21, cex = 1.48, lwd = 1.25,
      bg = pal$crema, col = pal$oliva
    )
    points(
      row$observado_pct, y,
      pch = 21, cex = 1.6, lwd = 1.0,
      bg = pal$terracota, col = pal$plomo
    )
    text(
      row$esperado_pct, y + 0.25, row$etiqueta_esperado,
      cex = 0.66, font = 2, col = pal$oliva
    )
    text(
      row$observado_pct, y - 0.25, row$etiqueta_observado,
      cex = 0.66, font = 2, col = pal$terracota
    )
    text(
      103.5, y, row$etiqueta_indice,
      adj = c(0, 0.5), cex = 0.68, col = pal$texto_muted
    )
  }

  text(
    50, 0.43, "Parejas con la misma categoría",
    cex = 0.72, col = pal$texto_soft
  )
  text(
    -40.5, -0.04,
    "Kappa descuenta la coincidencia esperada por la distribución marginal; cobertura = dato válido para ambos miembros.",
    adj = c(0, 0.5), cex = 0.61, col = pal$texto_soft, xpd = NA
  )
  text(
    -40.5, -0.35,
    "Selección: excluye señales κ < 0,10, uso digital, atributos compartidos por hogar y proxies educativos redundantes.",
    adj = c(0, 0.5), cex = 0.61, col = pal$texto_soft, xpd = NA
  )
  text(
    -40.5, -0.64,
    "Subuniversos: * campo superior válido en ambos (10,0%); † categoría laboral válida en ambos (55,3%). Edad en bandas de diez años.",
    adj = c(0, 0.5), cex = 0.59, col = pal$texto_soft, xpd = NA
  )
  text(
    -40.5, -0.94,
    paste0(
      "Fuente: ONE, X Censo Nacional de Población y Vivienda 2022  |  universo base: ",
      format_num_es(unique(datos$parejas_base)), " parejas jefatura–cónyuge con edades plausibles  |  elaboración: Leonardo Mena"
    ),
    adj = c(0, 0.5), cex = 0.57, col = pal$texto_muted, xpd = NA
  )
}

png_path <- file.path(figure_dir, "11_homogamia_ajustada.png")
svg_path <- file.path(figure_dir, "11_homogamia_ajustada.svg")

render_chart(function() {
  png(
    filename = png_path,
    width = 4320,
    height = 2816,
    units = "px",
    res = 320,
    type = if (capabilities("cairo")) "cairo" else "windows",
    bg = pal$crema
  )
})

render_chart(function() {
  svg(
    filename = svg_path,
    width = 13.5,
    height = 8.8,
    pointsize = 12,
    family = "sans",
    bg = pal$crema,
    onefile = TRUE
  )
})

qa <- data.frame(
  chequeo = c(
    "R lee UTF-8",
    "Controles PostgreSQL sin errores",
    "Universo de parejas unico",
    "Doce indicadores calculados",
    "Cuatro resultados previos replicados",
    "Marginales y porcentajes validos",
    "Seleccion editorial de seis variables"
  ),
  estado = "OK",
  detalle = c(
    "l10n_info UTF-8 = TRUE",
    "meta.controles_calidad_analitica: 0 errores",
    format_num_es(unique(datos$parejas_base)),
    "12 atributos individuales calculados; sin variables compartidas por hogar",
    "ocupacion, educacion, campo y grupo ocupacional",
    "observado, esperado, kappa y cobertura dentro de rango",
    paste(seleccion$indicador_id, collapse = "; ")
  ),
  stringsAsFactors = FALSE
)
write.csv(qa, qa_path, row.names = FALSE, fileEncoding = "UTF-8", na = "")

message("Grafico generado: ", png_path)
message("Grafico generado: ", svg_path)
message("Datos procesados: ", data_path)
message("QA: ", qa_path)
