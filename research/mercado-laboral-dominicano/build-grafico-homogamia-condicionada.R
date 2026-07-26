#!/usr/bin/env Rscript

options(encoding = "UTF-8", scipen = 999)

if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop(
    "R no está leyendo UTF-8. Ejecute render-grafico-homogamia-condicionada.ps1.",
    call. = FALSE
  )
}

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_argument)) stop("No se pudo localizar el script.", call. = FALSE)
module_dir <- dirname(normalizePath(sub("^--file=", "", script_argument[[1]]), winslash = "/"))

sql_dir <- file.path(module_dir, "sql")
figure_dir <- file.path(module_dir, "figuras")
processed_dir <- file.path(module_dir, "data", "procesados")
main_path <- file.path(processed_dir, "12_homogamia_condicionada.csv")
validation_path <- file.path(processed_dir, "12_homogamia_validacion.csv")
qa_path <- file.path(module_dir, "qa-homogamia-condicionada.csv")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

format_pct_es <- function(value, digits = 1L) {
  paste0(formatC(value, digits = digits, format = "f", decimal.mark = ","), "%")
}

format_kappa_es <- function(value, digits = 2L) {
  formatC(value, digits = digits, format = "f", decimal.mark = ",")
}

format_num_es <- function(value) {
  format(round(value), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

read_csv_text <- function(output, label) {
  assert_true(length(output) > 0L, paste("La consulta no devolvió datos:", label))
  result <- read.csv(
    text = paste(output, collapse = "\n"),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    encoding = "UTF-8"
  )
  assert_true(nrow(result) > 0L, paste("La consulta devolvió una tabla vacía:", label))
  result
}

use_cache <- identical(Sys.getenv("HOMOGAMIA_RIGOR_USE_CACHE", unset = "0"), "1")

if (!use_cache) {
  password <- Sys.getenv("PG_RECOVERY_PASSWORD", unset = "")
  assert_true(nzchar(password), "Defina PG_RECOVERY_PASSWORD para consultar PostgreSQL.")

  psql_default <- c(
    "C:/Program Files/PostgreSQL/17/bin/psql.exe",
    "C:/Program Files/PostgreSQL/18/bin/psql.exe"
  )
  psql <- Sys.getenv("PSQL", unset = "")
  if (!nzchar(psql)) {
    candidates <- psql_default[file.exists(psql_default)]
    assert_true(length(candidates) > 0L, "No se encontró psql 17 ni 18.")
    psql <- candidates[[1]]
  }
  assert_true(file.exists(psql), paste("No se encontró psql:", psql))

  old_pgpassword <- Sys.getenv("PGPASSWORD", unset = NA_character_)
  Sys.setenv(PGPASSWORD = password)
  on.exit({
    if (is.na(old_pgpassword)) Sys.unsetenv("PGPASSWORD") else Sys.setenv(PGPASSWORD = old_pgpassword)
  }, add = TRUE)

  run_sql <- function(database, filename) {
    path <- file.path(sql_dir, filename)
    assert_true(file.exists(path), paste("No existe la consulta:", path))
    stderr_path <- tempfile(fileext = ".log")
    args <- c(
      "-q", "-X", "-w",
      "-h", "127.0.0.1", "-p", "5433", "-U", "postgres",
      "-d", database, "--csv", "-f", shQuote(path)
    )
    output <- system2(psql, args = args, stdout = TRUE, stderr = stderr_path)
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    if (status != 0L) {
      error_text <- if (file.exists(stderr_path)) {
        paste(readLines(stderr_path, warn = FALSE), collapse = "\n")
      } else ""
      stop("psql falló en ", filename, ":\n", error_text, call. = FALSE)
    }
    read_csv_text(output, filename)
  }

  main <- run_sql("censo_2022", "grafico_homogamia_condicionada.sql")
  validation_2010 <- run_sql("censo_2010", "validacion_homogamia_2010.sql")
  validation_2022 <- run_sql("censo_2022", "validacion_homogamia_2022_armonizada.sql")
  enhogar <- run_sql("enhogar_2024", "extraer_parejas_enhogar_2024.sql")

  age_band <- function(x) {
    cut(
      x,
      breaks = c(15, 24, 34, 44, 54, 64, 74, Inf),
      labels = as.character(seq_len(7)),
      right = TRUE
    )
  }

  weighted_metric <- function(data, indicator, weight_column = "peso") {
    if (indicator == "grupo_edad_10") {
      category_j <- age_band(data$j_edad)
      category_c <- age_band(data$c_edad)
      stratum <- interaction(
        data$region, data$j_sexo, data$c_sexo, data$j_union, data$c_union,
        drop = TRUE, lex.order = TRUE
      )
    } else if (indicator == "nivel_educativo_4") {
      category_j <- data$j_educacion_4
      category_c <- data$c_educacion_4
      stratum <- interaction(
        data$j_sexo, data$c_sexo, data$j_union, data$c_union,
        age_band(data$j_edad), age_band(data$c_edad),
        drop = TRUE, lex.order = TRUE
      )
    } else {
      stop("Indicador ENHOGAR no reconocido.", call. = FALSE)
    }

    weights <- data[[weight_column]]
    valid <- !is.na(category_j) & !is.na(category_c) & !is.na(weights) & weights > 0
    category_j <- as.character(category_j[valid])
    category_c <- as.character(category_c[valid])
    stratum <- as.character(stratum[valid])
    weights <- weights[valid]

    total_weight <- sum(weights)
    observed <- sum(weights * (category_j == category_c)) / total_weight

    marginal_j <- tapply(weights, category_j, sum)
    marginal_c <- tapply(weights, category_c, sum)
    categories <- union(names(marginal_j), names(marginal_c))
    probability_j <- setNames(rep(0, length(categories)), categories)
    probability_c <- probability_j
    probability_j[names(marginal_j)] <- marginal_j / total_weight
    probability_c[names(marginal_c)] <- marginal_c / total_weight
    expected_national <- sum(probability_j * probability_c)

    work <- data.frame(
      stratum = stratum,
      category_j = category_j,
      category_c = category_c,
      weight = weights,
      stringsAsFactors = FALSE
    )
    stratum_total <- aggregate(weight ~ stratum, work, sum)
    names(stratum_total)[[2]] <- "stratum_weight"
    marginal_j_s <- aggregate(weight ~ stratum + category_j, work, sum)
    names(marginal_j_s)[[3]] <- "weight_j"
    marginal_c_s <- aggregate(weight ~ stratum + category_c, work, sum)
    names(marginal_c_s)[[3]] <- "weight_c"
    matched <- merge(
      marginal_j_s,
      marginal_c_s,
      by.x = c("stratum", "category_j"),
      by.y = c("stratum", "category_c"),
      all = FALSE,
      sort = FALSE
    )
    matched <- merge(matched, stratum_total, by = "stratum", all.x = TRUE, sort = FALSE)
    expected_conditioned <- sum(
      matched$weight_j * matched$weight_c / matched$stratum_weight
    ) / total_weight

    stratum_n <- table(stratum)
    data.frame(
      indicador_id = indicator,
      n = sum(valid),
      expandido = total_weight,
      observado_pct = 100 * observed,
      esperado_nacional_pct = 100 * expected_national,
      esperado_condicionado_pct = 100 * expected_conditioned,
      kappa_nacional = (observed - expected_national) / (1 - expected_national),
      kappa_condicionado = (observed - expected_conditioned) / (1 - expected_conditioned),
      cobertura_pct = 100 * sum(valid) / nrow(data),
      n_estratos = length(stratum_n),
      mediana_n_estrato = unname(median(stratum_n)),
      pct_filas_estratos_menor_10 = 100 * sum(stratum_n[stratum_n < 10]) / sum(stratum_n),
      stringsAsFactors = FALSE
    )
  }

  bootstrap_enhogar <- function(data, indicator, replicates = 399L, seed = 20260718L) {
    set.seed(seed)
    design_stratum <- as.character(data$estratoreg)
    psu <- paste(design_stratum, data$upm, sep = "|")
    psus_by_stratum <- split(psu, design_stratum)
    psus_by_stratum <- lapply(psus_by_stratum, unique)
    estimates <- matrix(NA_real_, nrow = replicates, ncol = 2L)
    colnames(estimates) <- c("kappa_nacional", "kappa_condicionado")

    for (index in seq_len(replicates)) {
      multiplier <- numeric(nrow(data))
      for (stratum_name in names(psus_by_stratum)) {
        available <- psus_by_stratum[[stratum_name]]
        number_psus <- length(available)
        if (number_psus == 1L) {
          draw <- available
          rescale <- 1
        } else {
          draw <- sample(available, number_psus - 1L, replace = TRUE)
          rescale <- number_psus / (number_psus - 1L)
        }
        counts <- table(draw)
        rows <- design_stratum == stratum_name
        matched <- as.numeric(counts[match(psu[rows], names(counts))])
        matched[is.na(matched)] <- 0
        multiplier[rows] <- rescale * matched
      }
      data$bootstrap_weight <- data$peso * multiplier
      estimate <- try(weighted_metric(data, indicator, "bootstrap_weight"), silent = TRUE)
      if (!inherits(estimate, "try-error")) {
        estimates[index, ] <- c(estimate$kappa_nacional, estimate$kappa_condicionado)
      }
    }

    successful <- sum(complete.cases(estimates))
    assert_true(successful >= floor(0.95 * replicates), "Fallaron demasiadas réplicas ENHOGAR.")

    list(
      national = unname(quantile(estimates[, "kappa_nacional"], c(0.025, 0.975), na.rm = TRUE)),
      conditioned = unname(quantile(estimates[, "kappa_condicionado"], c(0.025, 0.975), na.rm = TRUE)),
      standard_error = apply(estimates, 2L, sd, na.rm = TRUE),
      replicates = replicates,
      successful = successful,
      psus = length(unique(psu)),
      strata = length(unique(design_stratum))
    )
  }

  enhogar_results <- lapply(c("grupo_edad_10", "nivel_educativo_4"), function(indicator) {
    point <- weighted_metric(enhogar, indicator)
    bootstrap <- bootstrap_enhogar(enhogar, indicator)
    point$anio <- 2024L
    point$fuente <- "ENHOGAR 2024"
    point$ci_nacional_bajo <- bootstrap$national[[1]]
    point$ci_nacional_alto <- bootstrap$national[[2]]
    point$ci_condicionado_bajo <- bootstrap$conditioned[[1]]
    point$ci_condicionado_alto <- bootstrap$conditioned[[2]]
    point$se_nacional <- bootstrap$standard_error[["kappa_nacional"]]
    point$se_condicionado <- bootstrap$standard_error[["kappa_condicionado"]]
    point$bootstrap_replicas <- bootstrap$replicates
    point$bootstrap_exitosas <- bootstrap$successful
    point$psus <- bootstrap$psus
    point$estratos_diseno <- bootstrap$strata
    point$metodo_condicionado <- if (indicator == "grupo_edad_10") {
      "Márgenes ponderados por región, sexo y unión"
    } else {
      "Márgenes ponderados por sexo, unión y edades; estratos reducidos"
    }
    point
  })
  enhogar_results <- do.call(rbind, enhogar_results)

  census_columns <- c(
    "anio", "fuente", "indicador_id", "n", "observado_pct",
    "esperado_nacional_pct", "esperado_condicionado_pct",
    "kappa_nacional", "kappa_condicionado", "cobertura_pct",
    "n_estratos", "mediana_n_estrato", "pct_filas_estratos_menor_10"
  )
  validation_2010 <- validation_2010[, census_columns]
  validation_2022 <- validation_2022[, census_columns]

  age_2022 <- main[main$indicador_id == "grupo_edad_10", ]
  age_2022 <- data.frame(
    anio = 2022L,
    fuente = "Censo 2022",
    indicador_id = "grupo_edad_10",
    n = age_2022$n,
    observado_pct = age_2022$observado_pct,
    esperado_nacional_pct = age_2022$esperado_nacional_pct,
    esperado_condicionado_pct = age_2022$esperado_principal_pct,
    kappa_nacional = age_2022$kappa_nacional,
    kappa_condicionado = age_2022$kappa_principal,
    cobertura_pct = age_2022$cobertura_pct,
    n_estratos = age_2022$n_estratos_principal,
    mediana_n_estrato = age_2022$mediana_n_estrato_principal,
    pct_filas_estratos_menor_10 = age_2022$pct_filas_estratos_menor_10_principal,
    stringsAsFactors = FALSE
  )

  census_validation <- rbind(validation_2010, age_2022, validation_2022)
  for (name in setdiff(names(enhogar_results), names(census_validation))) {
    census_validation[[name]] <- NA
  }
  for (name in setdiff(names(census_validation), names(enhogar_results))) {
    enhogar_results[[name]] <- NA
  }
  validation <- rbind(
    census_validation[, names(enhogar_results), drop = FALSE],
    enhogar_results
  )

  main$fecha_tecnica <- as.character(Sys.Date())
  validation$fecha_tecnica <- as.character(Sys.Date())
  write.csv(main, main_path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  write.csv(validation, validation_path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
} else {
  assert_true(file.exists(main_path), paste("No existe el extracto cacheado:", main_path))
  assert_true(file.exists(validation_path), paste("No existe el extracto cacheado:", validation_path))
  main <- read.csv(main_path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
  validation <- read.csv(validation_path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
}

assert_true(nrow(main) == 6L, "La tabla principal debe contener seis rasgos.")
assert_true(!anyDuplicated(main$indicador_id), "Hay rasgos duplicados.")
assert_true(all(main$errores_qa == 0L), "PostgreSQL reporta controles de calidad en error.")
assert_true(length(unique(main$parejas_base)) == 1L, "El universo base no es consistente.")
assert_true(unique(main$parejas_base) == 1731814, "Cambió el universo canónico de parejas 2022.")
assert_true(all(main$n > 0 & main$n <= main$parejas_base), "Hay denominadores inválidos.")
assert_true(all(main$observado_pct >= 0 & main$observado_pct <= 100), "Observados fuera de rango.")
expected_columns <- c("esperado_nacional_pct", "esperado_region_pct", "esperado_principal_pct")
assert_true(all(unlist(main[expected_columns]) >= 0 & unlist(main[expected_columns]) <= 100), "Esperados fuera de rango.")
kappa_columns <- c("kappa_nacional", "kappa_region", "kappa_principal")
assert_true(all(unlist(main[kappa_columns]) >= -1 & unlist(main[kappa_columns]) <= 1), "Kappa fuera de rango.")

controls <- data.frame(
  indicador_id = c(
    "grupo_edad_10", "nivel_educativo", "dificultad_funcional",
    "autoidentificacion", "categoria_ocupacional", "campo_estudio"
  ),
  observado_control = c(47.1502, 55.4944, 76.5259, 46.3399, 63.6037, 32.0277),
  esperado_control = c(18.1690, 31.2154, 64.3377, 23.8304, 54.9516, 21.4162),
  stringsAsFactors = FALSE
)
replicated <- merge(controls, main, by = "indicador_id", sort = FALSE)
assert_true(nrow(replicated) == 6L, "Faltan controles nacionales.")
assert_true(all(abs(replicated$observado_pct - replicated$observado_control) < 0.0001), "No se replicó el observado nacional.")
assert_true(all(abs(replicated$esperado_nacional_pct - replicated$esperado_control) < 0.0001), "No se replicó el esperado nacional.")

assert_true(all(c(2010L, 2022L, 2024L) %in% validation$anio), "Falta una fuente de validación.")
assert_true(sum(validation$fuente == "ENHOGAR 2024") == 2L, "ENHOGAR debe aportar dos indicadores.")
assert_true(all(validation$kappa_condicionado > 0), "Una réplica produjo asociación condicionada no positiva.")

labels <- c(
  grupo_edad_10 = "Grupo de edad",
  nivel_educativo = "Nivel educativo",
  autoidentificacion = "Autoidentificación etnorracial",
  dificultad_funcional = "Alguna dificultad funcional",
  categoria_ocupacional = "Categoría ocupacional†",
  campo_estudio = "Campo de estudio superior*"
)
main$label <- unname(labels[main$indicador_id])
assert_true(!anyNA(main$label), "Hay rasgos sin etiqueta editorial.")

main$order_group <- match(
  main$indicador_id,
  c(
    "grupo_edad_10", "nivel_educativo", "autoidentificacion",
    "dificultad_funcional", "categoria_ocupacional", "campo_estudio"
  )
)
main <- main[order(main$order_group), ]

pal <- list(
  terracota = "#c86448",
  azul = "#45748c",
  oliva = "#6b7554",
  plomo = "#343a40",
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
  par(mar = rep(0.5, 4), oma = rep(0, 4), xaxs = "i", yaxs = "i", family = "sans", bg = pal$crema)

  plot.new()
  plot.window(xlim = c(-0.245, 0.575), ylim = c(-0.1, 14.0), xaxs = "i", yaxs = "i")

  text(-0.225, 13.55, "Las similitudes persisten dentro de perfiles comparables",
    adj = c(0, 0.5), cex = 1.55, font = 2, col = pal$texto, xpd = NA)
  text(-0.225, 13.02,
    "Coincidencia entre parejas por encima del azar (κ), Censo 2022",
    adj = c(0, 0.5), cex = 0.94, col = pal$texto_soft, xpd = NA)

  points(-0.214, 12.45, pch = 21, cex = 1.15, lwd = 1.2, bg = pal$crema, col = pal$oliva, xpd = NA)
  text(-0.198, 12.45, "Solo márgenes nacionales", adj = c(0, 0.5), cex = 0.67, col = pal$texto_soft, xpd = NA)
  segments(-0.045, 12.45, -0.017, 12.45, col = pal$azul, lwd = 3, xpd = NA)
  text(-0.007, 12.45, "Sensibilidad región–provincia", adj = c(0, 0.5), cex = 0.67, col = pal$texto_soft, xpd = NA)
  points(0.185, 12.45, pch = 21, cex = 1.18, bg = pal$terracota, col = pal$plomo, lwd = 0.8, xpd = NA)
  text(0.201, 12.45, "Resultado principal: provincia", adj = c(0, 0.5), cex = 0.67, col = pal$texto_soft, xpd = NA)

  rect(-0.23, 5.22, 0.56, 12.02, col = pal$blanco, border = pal$border_dark, lwd = 0.7)
  guides <- seq(0, 0.4, 0.1)
  for (guide in guides) {
    segments(guide, 5.63, guide, 11.48, col = pal$border, lty = "dotted", lwd = 0.8)
    text(guide, 5.42, format_kappa_es(guide, 1), cex = 0.62, col = pal$texto_muted)
  }
  text(-0.215, 11.70, "RASGOS GENERALES", adj = c(0, 0.5), cex = 0.58, font = 2, col = pal$oliva)
  text(0.455, 11.70, "OBS. / ESP. COND.", cex = 0.55, font = 2, col = pal$texto_muted)

  y_positions <- c(11.10, 10.15, 9.20, 8.25, 6.92, 5.97)
  segments(-0.215, 7.55, 0.545, 7.55, col = pal$border, lwd = 0.8)
  text(-0.215, 7.80, "SUBUNIVERSOS ESPECÍFICOS", adj = c(0, 0.5), cex = 0.58, font = 2, col = pal$terracota)

  for (index in seq_len(nrow(main))) {
    row <- main[index, ]
    y <- y_positions[[index]]
    text(-0.012, y, row$label, adj = c(1, 0.5), cex = 0.76, font = 2, col = pal$texto)
    range_values <- range(c(row$kappa_region, row$kappa_principal))
    segments(range_values[[1]], y, range_values[[2]], y, col = pal$azul, lwd = 3.6, lend = 1)
    points(row$kappa_nacional, y, pch = 21, cex = 1.22, lwd = 1.15, bg = pal$crema, col = pal$oliva)
    points(row$kappa_principal, y, pch = 21, cex = 1.32, lwd = 0.8, bg = pal$terracota, col = pal$plomo)
    text(row$kappa_principal, y + 0.25, paste0("κ ", format_kappa_es(row$kappa_principal)),
      cex = 0.62, font = 2, col = pal$terracota)
    right_label <- paste0(format_pct_es(row$observado_pct), " / ", format_pct_es(row$esperado_principal_pct))
    text(0.455, y, right_label, cex = 0.64, font = 2, col = pal$texto_soft)
    if (row$indicador_id %in% c("categoria_ocupacional", "campo_estudio")) {
      text(0.455, y - 0.27, paste0("cob. ", format_pct_es(row$cobertura_pct)), cex = 0.55, col = pal$texto_muted)
    }
  }
  text(0.20, 5.26, "κ = 0: ninguna coincidencia adicional al azar", cex = 0.59, col = pal$texto_muted)

  rect(-0.23, 1.45, 0.56, 4.78, col = pal$blanco, border = pal$border_dark, lwd = 0.7)
  text(-0.215, 4.48, "PRUEBA EN OTROS AÑOS Y OTRA FUENTE", adj = c(0, 0.5), cex = 0.59, font = 2, col = pal$azul)
  text(-0.215, 4.16,
    "Edad y educación armonizada; κ condicionado. ENHOGAR: márgenes ponderados e intervalo de 95%.",
    adj = c(0, 0.5), cex = 0.62, col = pal$texto_soft)

  validation_rows <- list(
    grupo_edad_10 = validation[validation$indicador_id == "grupo_edad_10", ],
    nivel_educativo_4 = validation[validation$indicador_id == "nivel_educativo_4", ]
  )
  validation_y <- c(grupo_edad_10 = 3.30, nivel_educativo_4 = 2.28)
  validation_labels <- c(grupo_edad_10 = "Grupo de edad", nivel_educativo_4 = "Nivel educativo (4 niveles)")
  source_colors <- c("Censo 2010" = pal$oliva, "Censo 2022" = pal$terracota,
    "Censo 2022 armonizado" = pal$terracota, "ENHOGAR 2024" = pal$azul)
  source_pch <- c("Censo 2010" = 21, "Censo 2022" = 21,
    "Censo 2022 armonizado" = 21, "ENHOGAR 2024" = 22)

  for (indicator in names(validation_rows)) {
    rows <- validation_rows[[indicator]]
    rows <- rows[order(rows$anio), ]
    y <- validation_y[[indicator]]
    text(-0.012, y, validation_labels[[indicator]], adj = c(1, 0.5), cex = 0.72, font = 2, col = pal$texto)
    segments(0, y, 0.43, y, col = pal$border, lwd = 0.7)
    for (index in seq_len(nrow(rows))) {
      row <- rows[index, ]
      color <- source_colors[[row$fuente]]
      if (row$fuente == "ENHOGAR 2024" && !is.na(row$ci_condicionado_bajo)) {
        segments(row$ci_condicionado_bajo, y, row$ci_condicionado_alto, y, col = color, lwd = 2.2)
      }
      points(row$kappa_condicionado, y, pch = source_pch[[row$fuente]], cex = 1.18,
        bg = color, col = pal$plomo, lwd = 0.7)
      label_shift <- if (row$anio == 2010L) -0.006 else if (row$anio == 2024L) 0.006 else 0
      label_adj <- if (row$anio == 2010L) 1 else if (row$anio == 2024L) 0 else 0.5
      label_y <- if (row$anio == 2022L) 0.47 else 0.27
      text(row$kappa_condicionado + label_shift, y + label_y, as.character(row$anio),
        adj = c(label_adj, 0.5), cex = 0.55, font = 2, col = color)
      text(row$kappa_condicionado + label_shift, y - label_y, format_kappa_es(row$kappa_condicionado),
        adj = c(label_adj, 0.5), cex = 0.54, col = pal$texto_muted)
    }
  }
  for (guide in guides) {
    segments(guide, 1.77, guide, 3.78, col = pal$border, lty = "dotted", lwd = 0.65)
    text(guide, 1.63, format_kappa_es(guide, 1), cex = 0.55, col = pal$texto_muted)
  }

  enhogar_info <- validation[validation$fuente == "ENHOGAR 2024", ][1, ]
  text(-0.225, 1.05,
    "Contrafactual principal: mismo territorio, composición por sexo y tipo de unión; además, edades de ambos salvo cuando edad es el resultado.",
    adj = c(0, 0.5), cex = 0.56, col = pal$texto_soft, xpd = NA)
  text(-0.225, 0.72,
    "* Campo de estudio: dato válido para ambos en 10,0% de las parejas. † Categoría ocupacional: 55,3%. Son asociaciones descriptivas, no efectos causales.",
    adj = c(0, 0.5), cex = 0.55, col = pal$texto_soft, xpd = NA)
  text(-0.225, 0.39,
    paste0(
      "ENHOGAR: ", format_num_es(enhogar_info$n), " parejas muestrales; ",
      format_num_es(enhogar_info$psus), " UPM; ", format_num_es(enhogar_info$bootstrap_replicas),
      " réplicas Rao–Wu. Fuente: ONE, Censos 2010/2022 y ENHOGAR 2024. Elaboración: Leonardo Mena."
    ),
    adj = c(0, 0.5), cex = 0.54, col = pal$texto_muted, xpd = NA)
}

png_path <- file.path(figure_dir, "12_homogamia_condicionada.png")
svg_path <- file.path(figure_dir, "12_homogamia_condicionada.svg")

render_chart(function() {
  png(
    filename = png_path, width = 4320, height = 3392, units = "px", res = 320,
    type = if (capabilities("cairo")) "cairo" else "windows", bg = pal$crema
  )
})

render_chart(function() {
  svg(
    filename = svg_path, width = 13.5, height = 10.6, pointsize = 12,
    family = "sans", bg = pal$crema, onefile = TRUE
  )
})

enhogar_validation <- validation[validation$fuente == "ENHOGAR 2024", ]
qa <- data.frame(
  chequeo = c(
    "R lee UTF-8",
    "Universo canónico 2022",
    "Seis resultados nacionales replicados",
    "Contrafactual geográfico sensible",
    "Estratos principales diagnosticados",
    "Educación armonizada 2010-2022",
    "ENHOGAR ponderada",
    "Incertidumbre ENHOGAR",
    "Limitación causal explícita"
  ),
  estado = "OK",
  detalle = c(
    "l10n_info UTF-8 = TRUE",
    paste0(format_num_es(unique(main$parejas_base)), " parejas con edades plausibles"),
    "Observados y esperados nacionales coinciden con la figura 11",
    "Se reportan κ por región y por provincia",
    paste0("máximo ", format_pct_es(max(main$pct_filas_estratos_menor_10_principal)), " de filas en estratos n<10"),
    "Cuatro niveles idénticos: preprimaria, primaria, secundaria y superior",
    paste0(format_num_es(enhogar_validation$n[[1]]), " parejas; factor de expansión final"),
    paste0(enhogar_validation$bootstrap_replicas[[1]], " réplicas Rao–Wu por UPM; estratos reducidos en educación"),
    "El gráfico describe asociación; no separa selección de convergencia"
  ),
  stringsAsFactors = FALSE
)
write.csv(qa, qa_path, row.names = FALSE, fileEncoding = "UTF-8", na = "")

message("Gráfico generado: ", png_path)
message("Gráfico generado: ", svg_path)
message("Datos principales: ", main_path)
message("Validación: ", validation_path)
message("QA: ", qa_path)
