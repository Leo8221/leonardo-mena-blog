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
      axis.title = element_text(colour = pal$tinta), axis.text = element_text(colour = pal$tinta),
      plot.title = element_text(face = "bold", size = 17, colour = pal$tinta),
      plot.subtitle = element_text(colour = "#5A5A55", size = 11),
      plot.caption = element_text(colour = "#65655F", size = 8.5, hjust = 0),
      legend.position = "top", legend.title = element_blank(),
      plot.margin = margin(14, 24, 14, 14)
    )
}

export_plot <- function(plot, dir, slug, width = 10.5, height = 6.5) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(dir, paste0(slug, ".svg")), plot, device = svglite::svglite,
         width = width, height = height, bg = pal$crema)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ggsave(file.path(dir, paste0(slug, ".png")), plot, device = ragg::agg_png,
           width = width, height = height, dpi = 320, bg = pal$crema)
  }
}

sugar_dir <- file.path(repo_root, "research", "ingenios-azucar")
sugar_data <- file.path(sugar_dir, "data", "procesados")
sugar_fig <- file.path(sugar_dir, "figuras")
dir.create(sugar_data, recursive = TRUE, showWarnings = FALSE)
dir.create(sugar_fig, recursive = TRUE, showWarnings = FALSE)

# Series históricas de producción y exportaciones. Las fuentes se documentan por tramo:
# Hall (1940–1960), USDA (1961–1969), Banco Mundial (1970–1984) y BCRD/FMI (1985–1990).
historical <- tribble(
  ~year, ~production, ~exports, ~source,
  1940, 430000, 414000, "Hall (2000), tabla 1.6",
  1941, 431705, 386385, "Hall (2000), tabla 1.6",
  1942, 484328, 189919, "Hall (2000), tabla 1.6",
  1943, 480216, 406101, "Hall (2000), tabla 1.6",
  1944, 512089, 692649, "Hall (2000), tabla 1.6",
  1945, 370063, 437869, "Hall (2000), tabla 1.6",
  1946, 458919, 388950, "Hall (2000), tabla 1.6",
  1947, 465428, 474509, "Hall (2000), tabla 1.6",
  1948, 421633, 381200, "Hall (2000), tabla 1.8",
  1949, 476484, 436800, "Hall (2000), tabla 1.8",
  1950, 453000, 424632, "Hall (2000), tabla 1.8",
  1951, 481584, 474474, "Hall (2000), tabla 1.8",
  1952, 547523, 530881, "Hall (2000), tabla 1.8",
  1953, 553479, 536199, "Hall (2000), tabla 1.8",
  1954, 532372, 604032, "Hall (2000), tabla 1.8",
  1955, 629850, 579442, "Hall (2000), tabla 1.8",
  1956, 755114, 693146, "Hall (2000), tabla 1.8",
  1957, 794843, 723400, "Hall (2000), tabla 1.8",
  1958, 690792, 651500, "Hall (2000), tabla 4.1",
  1959, 694177, 626000, "Hall (2000), tabla 4.1",
  1960, 1099129, 1060000, "Hall (2000), tabla 4.1",
  1961, 873000, 793000, "USDA (1972), tabla 12",
  1962, 902000, 846000, "USDA (1972), tabla 12",
  1963, 806000, 671000, "USDA (1972), tabla 12",
  1964, 828000, 662000, "USDA (1972), tabla 12",
  1965, 583000, 522000, "USDA (1972), tabla 12",
  1966, 671000, 572000, "USDA (1972), tabla 12",
  1967, 812000, 647000, "USDA (1972), tabla 12",
  1968, 666000, 605000, "USDA (1972), tabla 12",
  1969, 885000, 636000, "USDA (1972), tabla 12",
  1970, 984000, 800000, "Banco Mundial (1984), tabla 56",
  1971, 1097000, 962000, "Banco Mundial (1984), tabla 56",
  1972, 1139000, 1000000, "Banco Mundial (1984), tabla 56",
  1973, 1142000, 981000, "Banco Mundial (1984), tabla 56",
  1974, 1194000, 1030000, "Banco Mundial (1984), tabla 56",
  1975, 1135000, 952000, "Banco Mundial (1984), tabla 56",
  1976, 1249000, 1025000, "Banco Mundial (1984), tabla 56",
  1977, 1222000, 1179000, "Banco Mundial (1984), tabla 56",
  1978, 1164000, 1089000, "Banco Mundial (1984), tabla 56",
  1979, 1203000, 911000, "Banco Mundial (1984), tabla 56",
  1980, 1013000, 811000, "Banco Mundial (1984), tabla 56",
  1981, 1137000, 864000, "Banco Mundial (1984), tabla 56",
  1982, 1219000, 816000, "Banco Mundial (1984), tabla 56",
  1983, 1200000, 954000, "Banco Mundial (1984), tabla 56",
  1984, 1200000, 1000000, "Banco Mundial (1984), tabla 56",
  1985, 921000, 655000, "BCRD/FMI (1996), tabla 24",
  1986, 876000, 449000, "BCRD/FMI (1996), tabla 24",
  1987, 901000, 550000, "BCRD/FMI (1996), tabla 24",
  1988, 858000, 514000, "BCRD/FMI (1996), tabla 24",
  1989, 841000, 491000, "BCRD/FMI (1996), tabla 24",
  1990, 511000, 355000, "BCRD/FMI (1996), tabla 24"
) |>
  arrange(year) |>
  mutate(export_share = exports / production)
write_csv(historical, file.path(sugar_data, "01_produccion_exportaciones_azucar_1940_1990.csv"))

hist_long <- historical |>
  select(year, production, exports) |>
  pivot_longer(-year, names_to = "serie", values_to = "tons") |>
  mutate(serie = recode(serie, production = "Producción", exports = "Exportaciones"),
         tons_m = tons / 1000)

p_hist <- ggplot(hist_long, aes(year, tons_m, colour = serie)) +
  geom_line(linewidth = 1.05) + geom_point(data = filter(hist_long, !is.na(tons_m)), size = 1.8) +
  scale_colour_manual(values = c("Producción" = pal$terracota, "Exportaciones" = pal$azul)) +
  scale_x_continuous(breaks = seq(1940, 1990, 10), limits = c(1940, 1990)) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(title = "El azúcar tuvo un auge, pero no una trayectoria lineal",
       subtitle = "Producción y exportaciones de azúcar crudo · miles de toneladas métricas",
       x = NULL, y = NULL,
       caption = "Fuentes: Hall (2000), USDA/Instituto del Azúcar, Banco Mundial (anexo 1984) y BCRD/FMI. Las definiciones se documentan por tramo en la base procesada.") +
  theme_editorial()
export_plot(p_hist, sugar_fig, "01_produccion_exportaciones_azucar_1940_1990", 10.5, 6.4)

cea <- tribble(
  ~year, ~cea_production,
  1970, 660000, 1971, 705000, 1972, 710000, 1973, 768000, 1974, 790000,
  1975, 825000, 1976, 840000, 1977, 641000, 1978, 730000, 1979, 619000,
  1980, 603000, 1981, 694000, 1982, 701000, 1983, 731000, 1984, 635000,
  1985, 439000, 1986, 432000, 1987, 361000, 1988, 297000, 1989, 277000,
  1990, 266000
)
write_csv(cea, file.path(sugar_data, "02_produccion_cea_1970_1990.csv"))
p_cea <- ggplot(cea, aes(year, cea_production / 1000)) +
  geom_area(fill = pal$ocre, alpha = .75) + geom_line(colour = pal$terracota, linewidth = 1.1) +
  geom_point(colour = pal$terracota, size = 1.7) +
  scale_x_continuous(breaks = seq(1970, 1990, 5), limits = c(1970, 1990)) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(title = "El CEA creció hasta mediados de los setenta y luego retrocedió",
       subtitle = "Producción de azúcar crudo del Consejo Estatal del Azúcar · 1970–1990",
       x = NULL, y = NULL,
       caption = "Fuente: Instituto Dominicano del Azúcar, reproducido en IMF Staff Country Report 1996, tabla 25. Esta serie cubre al CEA, no toda la producción nacional.") +
  theme_editorial()
export_plot(p_cea, sugar_fig, "02_produccion_cea_1970_1990", 10.5, 6.2)

control <- tribble(
  ~year, ~share,
  1951, 5.4, 1952, 9.5, 1953, 16.9, 1954, 21.0, 1955, 26.3, 1956, 33.4, 1957, 71.3
)
write_csv(control, file.path(sugar_data, "03_control_trujillo_industria_1951_1957.csv"))
p_control <- ggplot(control, aes(year, share)) +
  geom_col(fill = pal$terracota, width = .72) +
  geom_text(aes(label = paste0(number(share, accuracy = .1), "%")), vjust = -.35, fontface = "bold", size = 3.4) +
  scale_x_continuous(breaks = 1951:1957) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 82), breaks = seq(0, 80, 20)) +
  labs(title = "La expansión azucarera también concentró propiedad",
       subtitle = "Porcentaje de la industria azucarera controlado por Trujillo · 1951–1957",
       x = NULL, y = NULL,
       caption = "Fuente: ONE, Estadística Industrial de la República Dominicana (1958), reproducida en Hall (2000), tabla 1.7.") +
  theme_editorial("x")
export_plot(p_control, sugar_fig, "03_control_trujillo_industria_1951_1957", 9.5, 5.8)

write_csv(tibble(
  id = c("01_produccion_exportaciones_azucar_1940_1990", "02_produccion_cea_1970_1990", "03_control_trujillo_industria_1951_1957"),
  pregunta = c("¿El auge azucarero fue una trayectoria continua?", "¿Qué pasó con la producción estatal después del auge?", "¿La expansión también concentró propiedad?"),
  familia = c("Serie histórica con corte de fuentes", "Serie histórica estatal", "Barras históricas"),
  fuente = c("Hall/USDA/BCRD-FMI", "INAZUCAR/IMF", "ONE/Hall"),
  advertencia = c("Serie nacional ensamblada por tramos documentados; no se interpolan años", "No representa la producción nacional total", "Control de industria, no participación de empleo")
), file.path(sugar_dir, "chart-map.csv"))

message("Visuales históricos del artículo de ingenios azucareros generados.")
