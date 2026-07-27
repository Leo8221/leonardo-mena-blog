#!/usr/bin/env Rscript

options(encoding = "UTF-8", scipen = 999)
if (!isTRUE(l10n_info()[["UTF-8"]])) stop("R no está leyendo UTF-8.", call. = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(tidyr)
  library(svglite)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]
repo_root <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), ".."), winslash = "/")
setwd(repo_root)

pal <- list(
  crema = "#F7F1E7", tinta = "#252525", terracota = "#B65C43",
  azul = "#4F789F", oliva = "#76825A", ocre = "#C79B53",
  gris = "#9B9B94", gris_claro = "#DED9D0"
)

theme_editorial <- function(grid = "y") {
  theme_minimal(base_size = 12, base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = pal$crema, colour = NA),
      panel.background = element_rect(fill = pal$crema, colour = NA),
      panel.grid.major = if (grid == "y") element_line(colour = pal$gris_claro, linewidth = .35) else element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(colour = pal$tinta),
      axis.text = element_text(colour = pal$tinta),
      plot.title = element_text(face = "bold", size = 17, colour = pal$tinta),
      plot.subtitle = element_text(colour = "#5A5A55", size = 11),
      plot.caption = element_text(colour = "#65655F", size = 8.5, hjust = 0),
      legend.position = "top", legend.title = element_blank(),
      plot.margin = margin(14, 24, 14, 14)
    )
}

export_plot <- function(plot, dir, slug, width = 10, height = 6.5) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(dir, paste0(slug, ".svg")), plot, device = svglite::svglite,
         width = width, height = height, bg = pal$crema)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ggsave(file.path(dir, paste0(slug, ".png")), plot, device = ragg::agg_png,
           width = width, height = height, dpi = 320, bg = pal$crema)
  } else {
    ggsave(file.path(dir, paste0(slug, ".png")), plot,
           width = width, height = height, dpi = 320, bg = pal$crema)
  }
}

write_map <- function(dir, rows) write_csv(bind_rows(rows), file.path(dir, "chart-map.csv"))

# 1. Nearshoring: la expansión exportadora no equivale automáticamente a más empleo.
near_dir <- file.path(repo_root, "research", "nearshoring-limites")
near_data <- file.path(near_dir, "data", "procesados")
near_fig <- file.path(near_dir, "figuras")
dir.create(near_data, recursive = TRUE, showWarnings = FALSE)
dir.create(near_fig, recursive = TRUE, showWarnings = FALSE)

cnzfe_trade <- tibble(
  anio = 2015:2024,
  exportaciones = c(5423.6, 5503.9, 5709.6, 6035.2, 6249.5, 5894.5, 7179.6, 7827.3, 7959.4, 8425.9),
  importaciones = c(3498.0, 3534.1, 3746.7, 3837.6, 3951.6, 3620.2, 4612.5, 5275.7, 4873.6, 4853.5)
)
cnzfe_jobs <- tibble(
  anio = 2015:2024,
  empleos = c(158713, 160594, 163096, 168123, 172711, 164421, 179455, 187869, 193344, 193398),
  empresas = c(630, 645, 665, 673, 695, 692, 734, 774, 820, 843)
)
write_csv(cnzfe_trade, file.path(near_data, "01_cnzfe_exportaciones_importaciones_2015_2024.csv"))
write_csv(cnzfe_jobs, file.path(near_data, "02_cnzfe_empresas_empleos_2015_2024.csv"))

trade_long <- cnzfe_trade |>
  pivot_longer(-anio, names_to = "serie", values_to = "valor") |>
  mutate(serie = recode(serie, exportaciones = "Exportaciones", importaciones = "Importaciones"))
jobs_long <- cnzfe_jobs |>
  pivot_longer(-anio, names_to = "serie", values_to = "valor") |>
  mutate(serie = recode(serie, empleos = "Empleos directos", empresas = "Empresas en operación"),
         unidad = if_else(serie == "Empleos directos", "Personas", "Empresas"))

p_near_trade <- ggplot(trade_long, aes(anio, valor, colour = serie)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.4) +
  scale_colour_manual(values = c("Exportaciones" = pal$terracota, "Importaciones" = pal$azul)) +
  scale_x_continuous(breaks = seq(2015, 2024, 2)) +
  scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = ".")) +
  labs(title = "Las zonas francas exportan más, pero importan menos que antes",
       subtitle = "Exportaciones e importaciones de zonas francas · millones de US$ · 2015–2024",
       x = NULL, y = "Millones de US$",
       caption = "Fuente: CNZFE, Informe Estadístico 2024; exportaciones e importaciones reportadas por el BCRD.") +
  theme_editorial()
export_plot(p_near_trade, near_fig, "01_exportaciones_importaciones_zonas_francas", 10.5, 6.4)

p_near_jobs <- ggplot(jobs_long, aes(anio, valor, colour = serie)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.4) +
  scale_colour_manual(values = c("Empleos directos" = pal$terracota, "Empresas en operación" = pal$azul)) +
  facet_wrap(~unidad, scales = "free_y", ncol = 1, labeller = labeller(unidad = c(Personas = "Empleos directos", Empresas = "Empresas en operación"))) +
  scale_x_continuous(breaks = seq(2015, 2024, 2)) +
  scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = ".")) +
  labs(title = "La recuperación reciente elevó el número de empresas más rápido que el empleo",
       subtitle = "Zonas francas en operación y empleos directos · 2015–2024",
       x = NULL, y = NULL,
       caption = "Fuente: CNZFE, Informe Estadístico 2024; empleos directos sin incluir operadoras.") +
  theme_editorial()
export_plot(p_near_jobs, near_fig, "02_empresas_empleos_zonas_francas", 10.5, 7.2)

sector_2024 <- tribble(
  ~actividad, ~empleos, ~exportaciones,
  "Productos médicos y farmacéuticos", 33437, 2762.6,
  "Tabaco y derivados", 39005, 1326.2,
  "Productos eléctricos y electrónicos", 10315, 1151.8,
  "Confecciones y textiles", 36194, 829.0,
  "Joyería", 2159, 720.0,
  "Productos agroindustriales", 5444, NA_real_
)
write_csv(sector_2024, file.path(near_data, "03_cnzfe_actividad_empleos_exportaciones_2024.csv"))
sector_long <- sector_2024 |>
  pivot_longer(c(empleos, exportaciones), names_to = "metrica", values_to = "valor") |>
  mutate(metrica = recode(metrica, empleos = "Empleos directos", exportaciones = "Exportaciones (millones US$)")) |>
  filter(!is.na(valor))
sector_order <- sector_2024 |> arrange(desc(empleos)) |> pull(actividad)
sector_long$actividad <- factor(sector_long$actividad, levels = sector_order)
p_near_sector <- ggplot(sector_long, aes(valor, actividad, fill = metrica)) +
  geom_col(show.legend = FALSE, width = .7) +
  geom_text(aes(label = if_else(metrica == "Empleos directos", comma(valor), dollar(valor, prefix = "US$ ", accuracy = .1))),
            hjust = -.08, size = 3.1, fontface = "bold") +
  facet_wrap(~metrica, scales = "free_x", ncol = 1) +
  scale_fill_manual(values = c("Empleos directos" = pal$terracota, "Exportaciones (millones US$)" = pal$azul)) +
  scale_x_continuous(expand = expansion(mult = c(0, .18))) +
  labs(title = "La especialización exportadora no genera la misma cantidad de empleo",
       subtitle = "Principales actividades de zonas francas · 2024",
       x = NULL, y = NULL,
       caption = "Fuente: CNZFE, Informe Estadístico 2024. Las exportaciones están expresadas en millones de US$.") +
  theme_editorial("x") + theme(axis.text.y = element_text(size = 9))
export_plot(p_near_sector, near_fig, "03_actividad_empleo_exportaciones_zonas_francas", 10.5, 8.4)

region_invest <- tibble(
  region = factor(c("Norte", "Distrito Nacional y Santo Domingo", "Sur", "Este"),
                  levels = c("Norte", "Distrito Nacional y Santo Domingo", "Sur", "Este")),
  inversion = c(2523231406.6, 2319394149.6, 1704631461.9, 1188462272.4),
  participacion = c(32.6, 30.0, 22.0, 15.4)
)
write_csv(region_invest, file.path(near_data, "04_cnzfe_inversion_region_2024.csv"))
p_near_region <- ggplot(region_invest, aes(inversion / 1e9, region)) +
  geom_col(fill = pal$oliva, width = .65) +
  geom_text(aes(label = sprintf("%.1f%%", participacion)), hjust = -.12, size = 3.4, fontface = "bold") +
  scale_x_continuous(labels = label_number(suffix = " mil M US$", accuracy = .1), expand = expansion(mult = c(0, .18))) +
  labs(title = "La inversión acumulada de zonas francas se concentra en dos polos",
       subtitle = "Inversión acumulada por región · 2024",
       x = NULL, y = NULL,
       caption = "Fuente: CNZFE, Informe Estadístico 2024. Las etiquetas muestran la participación regional.") +
  theme_editorial("x")
export_plot(p_near_region, near_fig, "04_inversion_zonas_francas_region", 10.5, 5.8)

write_map(near_dir, list(
  tibble(id = "01_exportaciones_importaciones_zonas_francas", pregunta = "¿Cómo cambia el saldo externo de zonas francas?", familia = "Líneas comparables", fuente = "CNZFE/BCRD", advertencia = "2024 preliminar"),
  tibble(id = "02_empresas_empleos_zonas_francas", pregunta = "¿La expansión empresarial se traduce en empleo al mismo ritmo?", familia = "Tendencias en panel", fuente = "CNZFE", advertencia = "Empleos directos sin operadoras"),
  tibble(id = "03_actividad_empleo_exportaciones_zonas_francas", pregunta = "¿Qué actividades concentran empleo y exportaciones?", familia = "Barras facetadas", fuente = "CNZFE", advertencia = "Exportaciones en millones de US$"),
  tibble(id = "04_inversion_zonas_francas_region", pregunta = "¿Dónde se concentra la inversión?", familia = "Barras ordenadas", fuente = "CNZFE", advertencia = "Inversión acumulada, no flujo anual")
))

# 2. Atletas: el gasto visible se concentra en pocas disciplinas y programas.
ath_dir <- file.path(repo_root, "research", "atletas-spillover")
ath_data <- file.path(ath_dir, "data", "procesados")
ath_fig <- file.path(ath_dir, "figuras")
dir.create(ath_data, recursive = TRUE, showWarnings = FALSE)
dir.create(ath_fig, recursive = TRUE, showWarnings = FALSE)

athletes <- tribble(
  ~disciplina, ~atletas, ~estipendio_mensual,
  "Voleibol", 40, 570000,
  "Taekwondo", 36, 370000,
  "Baloncesto", 35, 415000,
  "Balonmano", 31, 365000,
  "Softbol", 30, 205000,
  "Lucha", 23, 180000,
  "Atletismo", 22, 290000,
  "Judo", 22, 285000,
  "Boxeo", 20, 185000,
  "Ciclismo", 20, 100000,
  "Levantamiento de pesas", 14, 255000,
  "Natación", 14, 80000
)
write_csv(athletes, file.path(ath_data, "01_atletas_estipendios_disciplina_2025.csv"))
ath_long <- athletes |>
  pivot_longer(c(atletas, estipendio_mensual), names_to = "metrica", values_to = "valor") |>
  mutate(metrica = recode(metrica, atletas = "Atletas con estipendio", estipendio_mensual = "Estipendio mensual (RD$)"))
ath_long$disciplina <- factor(ath_long$disciplina, levels = athletes |> arrange(atletas) |> pull(disciplina))
p_ath <- ggplot(ath_long, aes(valor, disciplina, fill = metrica)) +
  geom_col(show.legend = FALSE, width = .67) +
  geom_text(aes(label = if_else(metrica == "Atletas con estipendio", comma(valor), dollar(valor, prefix = "RD$ ", accuracy = 1000))),
            hjust = -.1, size = 3.0, fontface = "bold") +
  facet_wrap(~metrica, scales = "free_x", ncol = 1) +
  scale_fill_manual(values = c("Atletas con estipendio" = pal$terracota, "Estipendio mensual (RD$)" = pal$azul)) +
  scale_x_continuous(expand = expansion(mult = c(0, .18))) +
  labs(title = "El apoyo al alto rendimiento se concentra en pocas disciplinas",
       subtitle = "Atletas incluidos en la nómina de estipendios fijos · disciplinas con mayor cantidad de beneficiarios",
       x = NULL, y = NULL,
       caption = "Fuente: MIDEREC, Memoria institucional 2025. La tabla completa reporta 527 atletas y RD$5.091 millones mensuales.") +
  theme_editorial("x") + theme(axis.text.y = element_text(size = 9))
export_plot(p_ath, ath_fig, "01_atletas_estipendios_disciplina", 10.5, 8.6)

ath_sex <- tibble(sexo = c("Masculino", "Femenino"), atletas = c(300, 227), participacion = c(57, 43))
write_csv(ath_sex, file.path(ath_data, "02_atletas_sexo_2025.csv"))
p_ath_sex <- ggplot(ath_sex, aes(1, atletas, fill = sexo)) +
  geom_col(width = .7, colour = pal$crema) +
  geom_text(aes(label = paste0(atletas, " · ", participacion, "%")), position = position_stack(vjust = .5), colour = pal$crema, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Masculino" = pal$azul, "Femenino" = pal$terracota)) +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(labels = comma, expand = c(0, 0)) +
  labs(title = "La nómina de alto rendimiento es mayoritariamente masculina",
       subtitle = "Atletas con estipendio fijo mensual · 2025",
       x = NULL, y = "Atletas", caption = "Fuente: MIDEREC, Memoria institucional 2025.") +
  theme_editorial() + theme(axis.text.x = element_blank(), legend.position = "top")
export_plot(p_ath_sex, ath_fig, "02_atletas_sexo_estipendios", 8.5, 5.5)

community <- tribble(
  ~programa, ~personas, ~costo,
  "Juegos fronterizos", 17400, 4475389.64,
  "Apoyo a clubes y ligas", 142924, 77472993.97,
  "Remodelación de instalaciones", 83435, 489468620.71
) |>
  mutate(costo_por_persona = costo / personas)
write_csv(community, file.path(ath_data, "03_programas_comunitarios_costo_personas_2025.csv"))
p_community <- ggplot(community, aes(costo_por_persona, reorder(programa, costo_por_persona))) +
  geom_col(fill = pal$oliva, width = .65) +
  geom_text(aes(label = dollar(costo_por_persona, prefix = "RD$ ", accuracy = 1)), hjust = -.12, size = 3.2, fontface = "bold") +
  scale_x_continuous(labels = dollar_format(prefix = "RD$ ", accuracy = 1), expand = expansion(mult = c(0, .2))) +
  labs(title = "El costo por persona cambia radicalmente según el programa",
       subtitle = "Costo reportado dividido entre personas impactadas · 2025",
       x = NULL, y = NULL,
       caption = "Fuente: MIDEREC, Memoria institucional 2025. Razón descriptiva; no mide impacto causal.") +
  theme_editorial("x")
export_plot(p_community, ath_fig, "03_costo_por_persona_programas_deportivos", 10.5, 5.8)

write_map(ath_dir, list(
  tibble(id = "01_atletas_estipendios_disciplina", pregunta = "¿Dónde se concentra el apoyo al alto rendimiento?", familia = "Barras facetadas", fuente = "MIDEREC", advertencia = "Muestra las disciplinas con más beneficiarios"),
  tibble(id = "02_atletas_sexo_estipendios", pregunta = "¿Cómo se distribuye la nómina por sexo?", familia = "Barra apilada", fuente = "MIDEREC", advertencia = "527 atletas con estipendio fijo"),
  tibble(id = "03_costo_por_persona_programas_deportivos", pregunta = "¿El gasto por persona es uniforme entre programas?", familia = "Barras comparables", fuente = "MIDEREC", advertencia = "Cociente descriptivo; no es spillover causal")
))

message("Visuales cuantitativos generados: nearshoring y atletas. El paquete histórico de ingenios se genera por separado.")
