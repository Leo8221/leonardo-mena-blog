#!/usr/bin/env Rscript

options(encoding = "UTF-8", scipen = 999)

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_argument)) stop("No se pudo localizar el script.", call. = FALSE)
module_dir <- dirname(normalizePath(sub("^--file=", "", script_argument[[1]]), winslash = "/"))
year_argument <- grep("^--year=", commandArgs(trailingOnly = TRUE), value = TRUE)
dataset_year <- if (length(year_argument)) sub("^--year=", "", year_argument[[1]]) else "2022"
assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
assert_true(dataset_year %in% c("2022", "2024"), "El año de ENHOGAR debe ser 2022 o 2024.")

raw_path <- if (dataset_year == "2022") {
  file.path(module_dir, "data", "raw", "enhogar_2022", "Personas_ENH22.csv")
} else {
  file.path(module_dir, "data", "raw", "enhogar_2024", "BD_ENH24_PERSONAS.csv")
}
processed_dir <- file.path(module_dir, "data", "procesados")
figure_dir <- file.path(module_dir, "figuras")
output_id <- if (dataset_year == "2022") "13" else "14"
output_path <- file.path(processed_dir, paste0(output_id, "_homogamia_enhogar_", dataset_year, ".csv"))
qa_path <- file.path(module_dir, paste0("qa-homogamia-enhogar-", dataset_year, ".csv"))

assert_true(file.exists(raw_path), paste("No existe la base descargada:", raw_path))
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

personas <- read.csv(
  raw_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA", "8", "9", "98", "99", "999", "9998"),
  fileEncoding = if (dataset_year == "2022") "Windows-1252" else "UTF-8"
)

region_name <- if (dataset_year == "2022") "Region" else "REGION"
required <- c("UPM", "HVIVIEN", "HHOGAR", "HLINEA", "P202", "P203", "P205", "P208", "P303", region_name, "ESTRATO")
assert_true(all(required %in% names(personas)), paste0("Faltan variables básicas de ENHOGAR ", dataset_year, "."))
weight_name <- names(personas)[grepl("^F(_?exp|expansion)", names(personas), ignore.case = TRUE)][1]
assert_true(!is.na(weight_name), paste0("No se encontró el factor de expansión de ENHOGAR ", dataset_year, "."))

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
for (name in c("P202", "P203", "P205", "P208", "P303", region_name, "ESTRATO", weight_name)) {
  personas[[name]] <- to_num(personas[[name]])
}

personas$hogar_key <- paste(personas$UPM, personas$HVIVIEN, personas$HHOGAR, sep = "-")
personas <- personas[personas$P205 %in% c(1, 2) & personas$P203 >= 16, , drop = FALSE]
conteos <- table(personas$hogar_key, personas$P205)
valid_keys <- rownames(conteos)[conteos[, "1"] == 1 & conteos[, "2"] == 1]
assert_true(length(valid_keys) > 1000, "Se obtuvieron muy pocos hogares con jefatura y pareja identificables.")

keep <- c("hogar_key", "UPM", "P202", "P203", "P208", "P303", region_name, "ESTRATO", weight_name)
jefaturas <- personas[personas$P205 == 1 & personas$hogar_key %in% valid_keys, keep, drop = FALSE]
parejas <- personas[personas$P205 == 2 & personas$hogar_key %in% valid_keys, keep, drop = FALSE]
names(jefaturas)[-1] <- paste0("j_", names(jefaturas)[-1])
names(parejas)[-1] <- paste0("c_", names(parejas)[-1])
pares <- merge(jefaturas, parejas, by = "hogar_key", sort = FALSE)
assert_true(nrow(pares) == length(valid_keys), "El enlace no produjo una fila por hogar válido.")

edu4 <- function(x) {
  x <- to_num(x)
  ifelse(x %in% 1:3, x, ifelse(x %in% 4:6, 4, NA_real_))
}

age_band <- function(x) {
  cut(
    to_num(x),
    breaks = c(15, 24, 34, 44, 54, 64, 74, Inf),
    labels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"),
    right = TRUE
  )
}

pares$j_edu4 <- edu4(pares$j_P303)
pares$c_edu4 <- edu4(pares$c_P303)
pares$j_age <- age_band(pares$j_P203)
pares$c_age <- age_band(pares$c_P203)
pares$composition_sex <- paste(pares$j_P202, pares$c_P202, sep = "-")
pares$union_par <- paste(pares$j_P208, pares$c_P208, sep = "-")
pares$region <- pares[[paste0("j_", region_name)]]
pares$weight <- to_num(pares[[paste0("j_", weight_name)]])
pares$same_education <- as.integer(pares$j_edu4 == pares$c_edu4)
pares$age_gap <- abs(pares$j_P203 - pares$c_P203)
pares$age_mean <- (pares$j_P203 + pares$c_P203) / 2

pares <- pares[
  !is.na(pares$j_edu4) & !is.na(pares$c_edu4) &
    !is.na(pares$j_age) & !is.na(pares$c_age) &
    !is.na(pares$weight) & pares$weight > 0,
  , drop = FALSE
]
assert_true(nrow(pares) > 2000, "La muestra educativa de parejas es demasiado pequeña.")

weighted_expected <- function(j, c, w) {
  pj <- tapply(w, j, sum) / sum(w)
  pc <- tapply(w, c, sum) / sum(w)
  categories <- intersect(names(pj), names(pc))
  sum(pj[categories] * pc[categories])
}

weighted_conditioned_expected <- function(data) {
  strata <- interaction(data$composition_sex, data$union_par, data$j_age, data$c_age, drop = TRUE, sep = "|")
  total_weight <- sum(data$weight)
  groups <- split(seq_len(nrow(data)), strata)
  sum(vapply(groups, function(index) {
    group_weight <- sum(data$weight[index])
    group_weight / total_weight * weighted_expected(
      data$j_edu4[index], data$c_edu4[index], data$weight[index]
    )
  }, numeric(1)))
}

observed <- with(pares, sum(weight * same_education) / sum(weight))
expected_national <- weighted_expected(pares$j_edu4, pares$c_edu4, pares$weight)
expected_conditioned <- weighted_conditioned_expected(pares)

pares$model_region <- factor(pares$region)
pares$model_composition <- factor(pares$composition_sex)
pares$model_union <- factor(pares$union_par)
model_complete <- complete.cases(pares[, c(
  "same_education", "model_region", "model_composition", "model_union",
  "age_gap", "age_mean", "weight"
)])
assert_true(sum(model_complete) > 2000, "La muestra completa para el modelo logit es demasiado pequeña.")
model_data <- pares[model_complete, , drop = FALSE]
model_data$model_weight <- model_data$weight / mean(model_data$weight)
model <- glm(
  same_education ~ model_region + model_composition + model_union + age_gap + age_mean,
  data = model_data,
  family = quasibinomial(),
  weights = model_weight
)
predicted <- predict(model, newdata = model_data, type = "response")
assert_true(all(is.finite(predicted)), "El modelo logit produjo predicciones no finitas.")
adjusted_logit <- sum(predicted * model_data$weight) / sum(model_data$weight)

metric <- data.frame(
  fuente = paste0("ENHOGAR ", dataset_year),
  n_muestra_parejas = nrow(pares),
  parejas_expandidas = sum(pares$weight),
  observado_pct = 100 * observed,
  esperado_nacional_pct = 100 * expected_national,
  esperado_condicionado_pct = 100 * expected_conditioned,
  kappa_nacional = (observed - expected_national) / (1 - expected_national),
  kappa_condicionado = (observed - expected_conditioned) / (1 - expected_conditioned),
  probabilidad_ajustada_logit_pct = 100 * adjusted_logit,
  n_modelo_logit = nrow(model_data),
  cobertura_modelo_logit_pct = 100 * nrow(model_data) / nrow(pares),
  cobertura_educativa_pct = 100 * nrow(pares) / length(valid_keys),
  modelo_logit_convergio = isTRUE(model$converged),
  fecha_tecnica = as.character(Sys.Date()),
  stringsAsFactors = FALSE
)
write.csv(metric, output_path, row.names = FALSE, fileEncoding = "UTF-8", na = "")

historical_path <- file.path(processed_dir, "12_homogamia_validacion.csv")
assert_true(file.exists(historical_path), "Falta la validación histórica de Censos 2010 y 2022.")
historical <- read.csv(historical_path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
historical <- historical[
  historical$indicador_id == "nivel_educativo_4" &
    historical$fuente %in% c("Censo 2010", "Censo 2022 armonizado"),
  ,
  drop = FALSE
]
comparison <- rbind(
  data.frame(fuente = historical$fuente, kappa_condicionado = historical$kappa_condicionado),
  data.frame(fuente = metric$fuente, kappa_condicionado = metric$kappa_condicionado)
)
comparison$fuente <- factor(comparison$fuente, levels = c("Censo 2010", "Censo 2022 armonizado", metric$fuente))

png_path <- file.path(figure_dir, paste0(output_id, "_homogamia_educativa_enhogar_", dataset_year, ".png"))
svg_path <- file.path(figure_dir, paste0(output_id, "_homogamia_educativa_enhogar_", dataset_year, ".svg"))
render_chart <- function(open_device) {
  open_device()
  on.exit(dev.off(), add = TRUE)
  par(mar = c(5.3, 4.8, 4.5, 1.2), family = "sans", bg = "#faf8f3")
  x <- seq_len(nrow(comparison))
  plot(
    x, comparison$kappa_condicionado, type = "n", xaxt = "n",
    xlab = "", ylab = "κ educativo condicionado",
    ylim = c(0.20, 0.36), xlim = c(0.65, 3.35),
    main = "Homogamia educativa: censos y encuesta",
    col.axis = "#73695f", col.lab = "#4a443e", col.main = "#1a1512", col.sub = "#4a443e"
  )
  mtext("Asociación ajustada por composición, unión y edades de ambos miembros", side = 3, line = 0.6, cex = .9, col = "#4a443e")
  abline(h = seq(0.20, 0.35, 0.05), col = "#e6dfd5", lty = "dotted")
  points(x, comparison$kappa_condicionado, pch = 21, cex = 2,
    bg = c("#6b7554", "#c86448", "#45748c"), col = "#343a40")
  text(x, comparison$kappa_condicionado + 0.012,
    formatC(comparison$kappa_condicionado, digits = 2, format = "f", decimal.mark = ","),
    font = 2, col = "#1a1512")
  axis(1, at = x, labels = c("Censo 2010", "Censo 2022", paste0("ENHOGAR ", dataset_year)), col.axis = "#4a443e", las = 1)
  mtext(paste0("ENHOGAR ", dataset_year, ": validación ponderada; Censos: universos censales."), side = 1, line = 2.8, adj = 0, cex = .75, col = "#73695f")
  box(col = "#d4c9ba")
}
render_chart(function() png(png_path, width = 2400, height = 1550, units = "px", res = 240, bg = "#faf8f3"))
render_chart(function() svg(svg_path, width = 10, height = 6.5, pointsize = 12, bg = "#faf8f3"))

qa <- data.frame(
  chequeo = c("Parejas únicas por hogar", "Educación válida en ambos miembros", "Factor de expansión presente", "Modelo logit converge", "No se filtra por ocupación"),
  estado = c("OK", "OK", "OK", ifelse(metric$modelo_logit_convergio, "OK", "ERROR"), "OK"),
  detalle = c(
    paste0(nrow(pares), " parejas analíticas"),
    paste0(format(round(metric$cobertura_educativa_pct, 1), decimal.mark = ","), "% de hogares con pareja válida"),
    weight_name,
    "glm quasibinomial ponderado por factor de expansión",
    "La ocupación no forma parte del filtro principal ni del ajuste base"
  ),
  stringsAsFactors = FALSE
)
write.csv(qa, qa_path, row.names = FALSE, fileEncoding = "UTF-8", na = "")

message("Salida: ", output_path)
message("Figura: ", svg_path)
