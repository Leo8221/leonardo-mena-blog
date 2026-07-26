#!/usr/bin/env Rscript

if (!isTRUE(l10n_info()[["UTF-8"]])) stop("R no está leyendo UTF-8; no se generan gráficos para evitar texto roto.")
root <- normalizePath(getwd(), mustWork = TRUE)
if (!dir.exists(file.path(root, "research"))) stop("Ejecute desde la raíz del repositorio.")

petroleum_dir <- file.path(root, "posts", "republica-habla-de", "2026-05-04-efectos-segunda-ronda-petroleo", "assets", "graphics")
homogamy_dir <- file.path(root, "posts", "republica-en-un-grafico", "2026-07-25-la-pareja-como-mercado", "assets", "graphics")
demography_dir <- file.path(root, "posts", "republica-en-un-grafico", "2026-07-26-demografia-dominicana", "assets", "graphics")
dir.create(petroleum_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(homogamy_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(demography_dir, recursive = TRUE, showWarnings = FALSE)

pal <- list(cream = "#FAF8F3", ink = "#1A1512", soft = "#5D554E", terracotta = "#C86448",
            olive = "#6B7554", blue = "#3E7186", slate = "#243B53", grid = "#DDD5C9")
csv <- function(path) read.csv(path, check.names = FALSE, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
fmt <- function(x, digits = 1) format(round(x, digits), nsmall = digits, big.mark = ".", decimal.mark = ",", trim = TRUE)
source_note <- function(text) mtext(text, side = 1, line = 5.5, adj = 0, cex = 0.75, col = pal$soft)
draw_grid <- function() abline(h = pretty(par("usr")[3:4]), col = "#DDD5C9", lty = 3)
save_plot <- function(stem, dir, draw, width = 1800, height = 1100) {
  for (kind in c("png", "svg")) {
    path <- file.path(dir, paste0(stem, ".", kind))
    if (kind == "png") png(path, width = width, height = height, res = 180, bg = pal$cream)
    else svg(path, width = width / 180, height = height / 180, bg = pal$cream)
    par(bg = pal$cream, fg = pal$ink, col.axis = pal$soft, col.lab = pal$soft,
        family = "sans", mar = c(6.4, 5.4, 4.3, 1.4), oma = c(1.2, 0, 0, 0))
    draw()
    dev.off()
  }
}

# 3. Petróleo, subsidios y segunda ronda --------------------------------------
pet_dir <- file.path(root, "posts", "republica-habla-de", "2026-05-04-efectos-segunda-ronda-petroleo")
pet <- csv(file.path(pet_dir, "data", "base_mensual_extendida.csv")); pet$date <- as.Date(pet$date)
models <- csv(file.path(pet_dir, "data", "resumen_modelos.csv"))
subsidy <- csv(file.path(pet_dir, "data", "resumen_subsidio_implicito.csv"))

save_plot("01-petroleo-combustibles-transporte", petroleum_dir, function() {
  d <- pet[pet$date >= as.Date("2011-01-01"), ]
  ylim <- range(c(d$oil_dop_yoy, d$fuel_yoy, d$ipc_transporte_yoy), na.rm = TRUE)
  plot(d$date, d$oil_dop_yoy, type = "n", ylim = ylim, xlab = "", ylab = "Variación interanual (%)",
       main = "Petróleo, combustibles y transporte")
  draw_grid(); lines(d$date, d$oil_dop_yoy, col = pal$slate, lwd = 2)
  lines(d$date, d$fuel_yoy, col = pal$terracotta, lwd = 2); lines(d$date, d$ipc_transporte_yoy, col = pal$olive, lwd = 2)
  abline(h = 0, col = pal$grid)
  legend("topleft", c("Petróleo en pesos", "Combustibles locales", "IPC transporte"),
         col = c(pal$slate, pal$terracotta, pal$olive), lwd = 2, bty = "n", horiz = TRUE, cex = 0.84)
  source_note("Fuente: ONE/BCRD y elaboración propia · variación interanual")
})

save_plot("02-efecto-acumulado-shock-petrolero", petroleum_dir, function() {
  d <- models[models$modelo == "petroleo_usd_rezagos_0_12", ]; d$variable <- factor(d$variable, levels = d$variable[order(d$efecto_10pct_pp)])
  par(mar = c(6.4, 11, 4.3, 1.4))
  y <- barplot(d$efecto_10pct_pp, names.arg = d$variable, horiz = TRUE, las = 1,
               col = ifelse(abs(d$t_acumulado) >= 2, pal$terracotta, pal$grid), border = NA,
               xlab = "Puntos porcentuales acumulados", main = "Un shock petrolero no termina en la bomba")
  abline(v = 0, col = pal$soft); text(d$efecto_10pct_pp, y, paste0(fmt(d$efecto_10pct_pp, 2), " pp"),
       pos = ifelse(d$efecto_10pct_pp >= 0, 4, 2), cex = 0.82, font = 2)
  source_note("Fuente: modelos extendidos del artículo · asociación descriptiva, no efecto causal")
})

save_plot("03-precio-observado-contrafactual", petroleum_dir, function() {
  d <- pet[pet$date >= as.Date("2021-01-01"), ]; old <- par(mar = c(2.4, 5.4, 3.5, 1.4), mfrow = c(2, 1))
  one <- function(observed, counterfactual, label) {
    ylim <- range(c(observed, counterfactual), na.rm = TRUE)
    plot(d$date, observed, type = "n", ylim = ylim, xlab = "", ylab = "RD$ por galón", main = label)
    draw_grid(); lines(d$date, counterfactual, col = pal$terracotta, lwd = 2, lty = 2); lines(d$date, observed, col = pal$slate, lwd = 2.2)
    legend("topleft", c("Observado", "Estimado sin amortiguación"), col = c(pal$slate, pal$terracotta),
           lwd = c(2.2, 2), lty = c(1, 2), bty = "n", cex = 0.78)
  }
  one(d$precio_gasolina_regular, d$gasolina_regular_precio_pred_sin_amortiguacion, "Gasolina regular")
  one(d$precio_gasoil_regular, d$gasoil_regular_precio_pred_sin_amortiguacion, "Gasoil regular")
  par(old); mtext("Precio observado vs. contrafactual sin amortiguación", side = 3, outer = TRUE, line = -1, cex = 1.15, font = 2)
  source_note("Fuente: relación precio local–petróleo en pesos, entrenada 2010–2019 · 2021–2025 observado")
})

save_plot("04-amortiguacion-implicita-por-producto", petroleum_dir, function() {
  labels <- c(gasolina_regular = "Gasolina regular", gasolina_premium = "Gasolina premium", gasoil_regular = "Gasoil regular", gasoil_optimo = "Gasoil óptimo", glp = "GLP")
  subsidy$label <- labels[subsidy$producto]; subsidy <- subsidy[order(subsidy$subsidio_implicito_prom_2022_2025_rd_gal), ]
  par(mar = c(6.4, 10, 4.3, 1.4))
  y <- barplot(subsidy$subsidio_implicito_prom_2022_2025_rd_gal, names.arg = subsidy$label, horiz = TRUE, las = 1,
               col = pal$terracotta, border = NA, xlab = "RD$ por galón", main = "La amortiguación cambia según el combustible",
               xlim = c(0, max(subsidy$subsidio_implicito_prom_2022_2025_rd_gal) * 1.18))
  text(subsidy$subsidio_implicito_prom_2022_2025_rd_gal, y, paste0("RD$", fmt(subsidy$subsidio_implicito_prom_2022_2025_rd_gal)), pos = 4, cex = 0.82, font = 2)
  source_note("Fuente: modelo de precio implícito · no equivale por sí solo a gasto fiscal ejecutado")
})

# 4. Homogamia educativa -------------------------------------------------------
homo_source <- file.path(root, "research", "mercado-laboral-dominicano", "figuras")
homo_files <- c("10_similitud_parejas", "11_homogamia_ajustada", "12_homogamia_condicionada",
                "13_homogamia_educativa_enhogar_2022", "14_homogamia_educativa_enhogar_2024")
for (stem in homo_files) {
  for (ext in c("png", "svg")) {
    src <- file.path(homo_source, paste0(stem, ".", ext))
    if (file.exists(src)) file.copy(src, file.path(homogamy_dir, paste0("homogamia-", sub("^[0-9]+_", "", stem), ".", ext)), overwrite = TRUE)
  }
}

# 5. Demografía dominicana ----------------------------------------------------
demography_source <- file.path(root, "research", "demografia-dominicana", "data")
projection <- csv(file.path(demography_source, "demografia_proyecciones_2000_2030.csv"))
age <- csv(file.path(demography_source, "demografia_edad_enhogar_2022_2024.csv"))

save_plot("01-proyeccion-poblacion-por-region", demography_dir, function() {
  d <- projection[projection$level == "region", ]; locations <- unique(d$location); yr <- sort(unique(d$year))
  cols <- c(pal$terracotta, pal$olive, pal$blue, pal$slate, "#9B7653", "#7B6D8D")
  plot(yr, d$total[d$location == locations[1]], type = "n", ylim = range(d$total), xlab = "Año", ylab = "Población",
       main = "La población cambia de centro de gravedad")
  draw_grid(); for (i in seq_along(locations)) lines(yr, d$total[d$location == locations[i]], col = cols[i], lwd = 2)
  legend("topleft", sub("^Región ", "", locations), col = cols[seq_along(locations)], lwd = 2, bty = "n", cex = 0.78, ncol = 2)
  source_note("Fuente: ONE, estimaciones y proyecciones de población total 2000–2030")
})

save_plot("02-participacion-region-metropolitana", demography_dir, function() {
  country <- projection[projection$location == "Total país", ]; metro <- projection[projection$location == "Región Metropolitana", ]
  d <- merge(country[, c("year", "total")], metro[, c("year", "total")], by = "year", suffixes = c("_pais", "_metro")); share <- 100 * d$total_metro / d$total_pais
  plot(d$year, share, type = "l", col = pal$terracotta, lwd = 2.5, xlab = "Año", ylab = "% de la población", ylim = range(share) + c(-1, 1),
       main = "El peso demográfico de la región metropolitana")
  draw_grid(); ix <- c(1, nrow(d)); points(d$year[ix], share[ix], pch = 21, bg = pal$cream, col = pal$terracotta, cex = 1.4)
  text(d$year[ix], share[ix], paste0(fmt(share[ix]), "%"), pos = 3, cex = 0.85, font = 2)
  source_note("Fuente: ONE, estimaciones y proyecciones de población total 2000–2030")
})

save_plot("03-poblacion-por-sexo-proyeccion", demography_dir, function() {
  d <- projection[projection$location == "Total país" & projection$year %in% c(2000, 2030), ]; mat <- rbind(Hombres = d$male, Mujeres = d$female) / 1e6
  barplot(mat, beside = TRUE, names.arg = c("2000", "2030"), col = c(pal$slate, pal$terracotta), border = NA,
          ylim = c(0, max(mat) * 1.18), ylab = "Millones de personas", main = "La población proyectada por sexo")
  legend("topleft", rownames(mat), fill = c(pal$slate, pal$terracotta), bty = "n", horiz = TRUE)
  source_note("Fuente: ONE, estimaciones y proyecciones de población total 2000–2030")
})

save_plot("04-estructura-edades-enhogar", demography_dir, function() {
  age$age_band <- factor(age$age_band, levels = unique(age$age_band)); age$share <- ave(age$weighted_persons, age$year, FUN = function(x) 100 * x / sum(x))
  yr <- sort(unique(age$year)); d1 <- age[age$year == yr[1], ]; d2 <- age[age$year == yr[2], ]; y <- seq_along(levels(age$age_band))
  plot(d1$share, y, type = "o", pch = 21, bg = pal$cream, col = pal$olive, lwd = 2, yaxt = "n", xlab = "% de la población ponderada", ylab = "",
       main = "La estructura de edades observada")
  axis(2, at = y, labels = levels(age$age_band), las = 1, cex.axis = 0.72); lines(d2$share, y, type = "o", pch = 21, bg = pal$cream, col = pal$blue, lwd = 2)
  legend("bottomright", as.character(yr), col = c(pal$olive, pal$blue), lwd = 2, pch = 21, pt.bg = pal$cream, bty = "n")
  source_note("Fuente: ONE, ENHOGAR 2022 y 2024 · factores de ponderación oficiales · 90–97 agrupado como tramo superior")
})

save_plot("05-crecimiento-proyectado-por-provincia", demography_dir, function() {
  d <- projection[projection$level == "provincia" & projection$year %in% c(2000, 2030), c("location", "year", "total")]
  wide <- reshape(d, idvar = "location", timevar = "year", direction = "wide"); wide$growth <- 100 * (wide$total.2030 / wide$total.2000 - 1); wide <- wide[order(wide$growth), ]
  keep <- c(head(seq_len(nrow(wide)), 5), tail(seq_len(nrow(wide)), 5)); d <- wide[keep, ]
  par(mar = c(6.4, 10, 4.3, 1.4))
  y <- barplot(d$growth, names.arg = d$location, horiz = TRUE, las = 1, col = ifelse(d$growth >= 0, pal$terracotta, pal$olive), border = NA,
               xlab = "Cambio proyectado (%)", main = "El crecimiento no se reparte igual",
               xlim = c(min(d$growth) * 2.5, max(d$growth) * 1.18))
  abline(v = 0, col = pal$soft); text(d$growth, y, paste0(fmt(d$growth), "%"), pos = ifelse(d$growth >= 0, 4, 2), cex = 0.78, font = 2)
  source_note("Fuente: ONE, estimaciones y proyecciones de población total por provincia 2000–2030")
})

manifest <- function(ids, files, source, dir) write.csv(data.frame(id = ids, file = files, source = source), file.path(dir, "visual-manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")
manifest(paste0("petroleo-", sprintf("%02d", 1:4)), paste0(sprintf("%02d", 1:4), c("-petroleo-combustibles-transporte", "-efecto-acumulado-shock-petrolero", "-precio-observado-contrafactual", "-amortiguacion-implicita-por-producto"), ".png"), "ONE/BCRD y modelos locales", petroleum_dir)
manifest(homo_files, paste0("homogamia-", sub("^[0-9]+_", "", homo_files), ".png"), "ONE, Censos 2010/2022 y ENHOGAR", homogamy_dir)
manifest(paste0("demografia-", sprintf("%02d", 1:5)), paste0(sprintf("%02d", 1:5), c("-proyeccion-poblacion-por-region", "-participacion-region-metropolitana", "-poblacion-por-sexo-proyeccion", "-estructura-edades-enhogar", "-crecimiento-proyectado-por-provincia"), ".png"), "ONE, proyecciones 2000–2030 y ENHOGAR 2022/2024", demography_dir)
cat("POST_GRAPHICS_OK\n")
