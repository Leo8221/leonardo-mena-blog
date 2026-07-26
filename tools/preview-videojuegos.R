args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("Uso: Rscript preview-videojuegos.R <directorio_salida>")
out_dir <- normalizePath(args[[1]], winslash = "/", mustWork = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
library(sf)
library(ggplot2)
library(grid)
library(png)
repo_root <- normalizePath(getwd(), winslash = "/")
rd <- sf::st_read(file.path(repo_root, "mapa_rd", "provincia", "PROVCenso2010.shp"), quiet = TRUE, stringsAsFactors = FALSE) |>
  sf::st_make_valid() |>
  sf::st_transform(32619)
activity <- read.csv(file.path(repo_root, "research", "mercado-laboral-dominicano", "data", "processed", "actividad-provincia-censo-2022.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
activity$provincia <- sprintf("%02d", as.integer(activity$provincia))
if (nrow(activity) != 32L || anyDuplicated(activity$provincia)) stop("La tabla provincial no tiene las 32 provincias esperadas.")
rd$PROV <- sprintf("%02d", as.integer(rd$PROV))
rd <- merge(rd, activity, by.x = "PROV", by.y = "provincia", sort = FALSE)
if (nrow(rd) != 32L || anyNA(rd$sector)) stop("Hay provincias sin actividad dominante enlazada.")
rd_union <- sf::st_union(rd)
grid_geom <- sf::st_make_grid(rd_union, cellsize = 30000, square = FALSE)
hex <- sf::st_sf(tile_id = seq_along(grid_geom), geometry = grid_geom) |>
  sf::st_intersection(sf::st_sf(geometry = rd_union))
hex_ll <- sf::st_transform(hex, 4326)
hex_xy <- sf::st_coordinates(sf::st_point_on_surface(hex_ll))
coast <- lengths(sf::st_intersects(hex_ll, sf::st_boundary(sf::st_union(hex_ll)))) > 0
hex$terrain <- dplyr::case_when(
  coast ~ "Costa",
  hex_xy[, 2] > 19.0 & hex_xy[, 1] < -70.1 ~ "Montana",
  hex_xy[, 1] < -70.45 & hex_xy[, 2] > 18.65 ~ "Bosque",
  hex_xy[, 2] > 18.5 & hex_xy[, 2] < 19.35 ~ "Cultivo",
  TRUE ~ "Llanura"
)
terrain_colors <- c(Costa = "#2f6074", Montana = "#b59872", Bosque = "#4e6b4e", Cultivo = "#a9a956", Llanura = "#8d985d")
centers <- sf::st_point_on_surface(rd)
center_xy <- sf::st_coordinates(centers)
centers_df <- data.frame(x = center_xy[, 1], y = center_xy[, 2], label = rd$PROV, sector = rd$sector)
map_plot <- ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#1e3a46") +
  geom_sf(data = hex, aes(fill = terrain), color = "#26352d", linewidth = .38) +
  geom_sf(data = rd, fill = NA, color = "#17261e", linewidth = .8) +
  scale_fill_manual(values = terrain_colors, name = "Terreno") + coord_sf(expand = FALSE) +
  labs(title = "Republica Dominicana · tablero provincial", subtitle = "Cada ficha representa la actividad CNAE dominante por empleo observado en la provincia", caption = "Censo 2022. AGRO = agricultura/pesca · COM = comercio · PUB = administracion · TUR = turismo · IND = manufactura. No es PIB provincial.") +
  theme_void() + theme(legend.position = "bottom", legend.title = element_text(color = "#f7efe5", face = "bold"), legend.text = element_text(color = "#f7efe5"), plot.title = element_text(color = "#f7efe5", face = "bold", size = 16), plot.subtitle = element_text(color = "#d5d0c2", size = 10), plot.caption = element_text(color = "#d5d0c2", hjust = 0, size = 8.5), plot.background = element_rect(fill = "#1e3a46", color = NA), panel.background = element_rect(fill = "#1e3a46", color = NA))
sector_short <- c("Agricultura y pesca" = "AGRO", "Comercio" = "COM", "Administracion publica" = "PUB", "Turismo y comidas" = "TUR", "Manufactura" = "IND")
centers_df$short <- unname(sector_short[centers_df$sector])
centers_df$tag_fill <- unname(c("Agricultura y pesca" = "#e0bd62", "Comercio" = "#9fc2cc", "Administracion publica" = "#c5c0b8", "Turismo y comidas" = "#df8862", "Manufactura" = "#c39a70")[centers_df$sector])
sprite_dir <- file.path(repo_root, "posts", "fundamentos", "2026-07-25-videojuegos-y-economia", "assets", "mapa")
sprite_files <- c(
  "Agricultura y pesca" = file.path(sprite_dir, "sprite-agriculture.png"),
  "Comercio" = file.path(sprite_dir, "sprite-commerce.png"),
  "Administracion publica" = file.path(sprite_dir, "sprite-public.png"),
  "Turismo y comidas" = file.path(sprite_dir, "sprite-tourism.png"),
  "Manufactura" = file.path(sprite_dir, "sprite-manufacturing.png")
)
if (!all(file.exists(sprite_files))) stop("Faltan sprites del mapa.")
for (i in seq_len(nrow(centers_df))) {
  map_plot <- map_plot + annotation_raster(
    png::readPNG(sprite_files[[centers_df$sector[[i]]]]),
    xmin = centers_df$x[[i]] - 15000,
    xmax = centers_df$x[[i]] + 15000,
    ymin = centers_df$y[[i]] - 15000,
    ymax = centers_df$y[[i]] + 15000,
    interpolate = TRUE
  )
}
map_plot <- map_plot +
  geom_label(data = centers_df, aes(x = x, y = y - 19000, label = short), fill = centers_df$tag_fill, color = "#16252b", size = 1.75, fontface = "bold", linewidth = .25, label.padding = grid::unit(.13, "lines"), inherit.aes = FALSE) +
  geom_text(data = centers_df, aes(x = x, y = y - 33000, label = label), color = "#f7efe5", size = 1.75, fontface = "bold", inherit.aes = FALSE)
ggsave(file.path(out_dir, "01-mapa-civilization-rd.png"), map_plot, width = 10, height = 7, dpi = 160, bg = "#1e3a46")

if (FALSE) {
png(file.path(out_dir, "02-constelacion-decisiones.png"), width = 1800, height = 975, res = 150, bg = "#0b1020")
grid.newpage()
grid.rect(gp = gpar(fill = "#101a35", col = NA))
grid.rect(gp = gpar(fill = "#172949", col = NA, alpha = .20))
grid.lines(x = c(.04, .96), y = c(.39, .39), gp = gpar(col = "#8fa4c7", alpha = .08))
grid.lines(x = c(.07, .93), y = c(.34, .34), gp = gpar(col = "#8fa4c7", alpha = .06))
set.seed(15)
stars <- data.frame(x = runif(38, .03, .97), y = runif(38, .06, .95), r = runif(38, .003, .010))
for (i in seq_len(nrow(stars))) grid.circle(stars$x[[i]], stars$y[[i]], r = stars$r[[i]], gp = gpar(fill = "#eef1dc", col = NA, alpha = .7))
grid.text("MENU DE HABILIDADES", x = .04, y = .985, just = "left", gp = gpar(col = "#e8d49b", fontsize = 9, fontface = "bold"))
grid.text("DECISIONES REALES · RECURSOS ESCASOS", x = .04, y = .958, just = "left", gp = gpar(col = "#a7b5ca", fontsize = 8))
grid.text("NIVEL 01", x = .96, y = .985, just = "right", gp = gpar(col = "#a7b5ca", fontsize = 8))

curve_points <- function(x0, y0, x1, y1, bend = 0) {
  t <- seq(0, 1, length.out = 40)
  cx <- (x0 + x1) / 2 + bend
  cy <- (y0 + y1) / 2
  data.frame(x = (1 - t)^2 * x0 + 2 * (1 - t) * t * cx + t^2 * x1, y = (1 - t)^2 * y0 + 2 * (1 - t) * t * cy + t^2 * y1)
}
connect <- function(x0, y0, x1, y1, color, bend = 0) {
  p <- curve_points(x0, y0, x1, y1, bend)
  grid.lines(p$x, p$y, gp = gpar(col = color, alpha = .20, lwd = 10, lineend = "round"))
  grid.lines(p$x, p$y, gp = gpar(col = color, lwd = 2.5, lineend = "round"))
}
connect(.5, .18, .25, .56, "#dfbd62", -.04); connect(.25, .56, .17, .72, "#dfbd62", -.02); connect(.17, .72, .12, .86, "#dfbd62", 0)
connect(.5, .18, .5, .55, "#8fb8c9"); connect(.5, .55, .5, .74, "#8fb8c9"); connect(.5, .74, .5, .87, "#8fb8c9")
connect(.5, .18, .75, .56, "#d58b67", .04); connect(.75, .56, .83, .72, "#d58b67", .02); connect(.83, .72, .88, .86, "#d58b67", 0)

draw_skill <- function(x, y, label, sublabel, color, kind, radius = .028, label_y = y + .055) {
  grid.circle(x, y, r = radius + .008, gp = gpar(fill = "#10192e", col = color, lwd = 2.8))
  grid.circle(x, y, r = radius - .008, gp = gpar(fill = "#10192e", col = "#6b7182", lwd = .5))
  if (kind == "book") grid.lines(c(x - .012, x, x + .012, x + .010, x), c(y - .009, y - .009, y - .009, y + .012, y + .012), gp = gpar(col = "#e7d49c", lwd = 1.5))
  if (kind == "briefcase") { grid.rect(x, y, width = .025, height = .017, gp = gpar(fill = NA, col = "#e7d49c", lwd = 1.4)); grid.lines(c(x - .007, x + .007), c(y + .008, y + .008), gp = gpar(col = "#e7d49c", lwd = 1.3)) }
  if (kind == "store") { grid.polygon(c(x - .018, x, x + .018), c(y + .001, y + .016, y + .001), gp = gpar(fill = NA, col = "#e7d49c", lwd = 1.3)); grid.rect(x, y - .010, width = .026, height = .020, gp = gpar(fill = NA, col = "#e7d49c", lwd = 1.3)) }
  if (kind == "coin") { grid.circle(x, y, r = .010, gp = gpar(fill = NA, col = "#e7d49c", lwd = 1.4)); grid.lines(c(x, x), c(y - .012, y + .012), gp = gpar(col = "#e7d49c", lwd = 1.2)) }
  grid.text(label, x, label_y, gp = gpar(col = "#f3e7c7", fontsize = 9, fontface = "bold"))
  grid.text(sublabel, x, label_y - .026, gp = gpar(col = "#aebed0", fontsize = 7.5))
}
draw_skill(.5, .18, "RECURSOS ESCASOS", "tiempo · dinero · atención", "#e2bd63", "coin", .035, .115)
draw_skill(.25, .56, "ESTUDIAR", "capital humano", "#dfbd62", "book", .030, .615)
draw_skill(.17, .72, "TÍTULO", "más opciones laborales", "#dfbd62", "book", .025, .775)
draw_skill(.12, .86, "ESPECIALIZAR", "salario potencial", "#dfbd62", "book", .023, .915)
draw_skill(.5, .55, "TRABAJAR", "ingreso hoy", "#8fb8c9", "briefcase", .030, .605)
draw_skill(.5, .74, "EXPERIENCIA", "red laboral", "#8fb8c9", "briefcase", .025, .795)
draw_skill(.5, .87, "ASCENSO", "estabilidad futura", "#8fb8c9", "briefcase", .023, .925)
draw_skill(.75, .56, "EMPRENDER", "autonomía", "#d58b67", "store", .030, .615)
draw_skill(.83, .72, "INVERTIR", "control del proyecto", "#d58b67", "coin", .025, .775)
draw_skill(.88, .86, "ESCALAR", "riesgo y retorno", "#d58b67", "coin", .023, .915)
legend_data <- data.frame(x = c(.25, .5, .75), color = c("#dfbd62", "#8fb8c9", "#d58b67"), benefit = c("capital humano", "ingreso y experiencia", "autonomía y control"), cost = c("renuncia: ingreso presente", "renuncia: tiempo de formación", "renuncia: estabilidad"), stringsAsFactors = FALSE)
for (i in seq_len(nrow(legend_data))) {
  grid.rect(legend_data$x[[i]], .405, width = .17, height = .035, gp = gpar(fill = "#17233b", col = legend_data$color[[i]], lwd = 1))
  grid.text(legend_data$benefit[[i]], legend_data$x[[i]], .412, gp = gpar(col = "#d7e0e8", fontsize = 7.2, fontface = "bold"))
  grid.text(legend_data$cost[[i]], legend_data$x[[i]], .355, gp = gpar(col = legend_data$color[[i]], fontsize = 7.2))
}
grid.text("cada ventaja implica renunciar a otra", x = .96, y = .035, just = "right", gp = gpar(col = "#9aaac0", fontsize = 8))
dev.off()
}

# Segunda pasada deliberada: tres rutas verticales, con jerarquía de menú y sin cruces.
png(file.path(out_dir, "02-constelacion-decisiones.png"), width = 1800, height = 975, res = 150, bg = "#0b1020")
grid.newpage()
grid.rect(gp = gpar(fill = "#101a35", col = NA))
grid.rect(gp = gpar(fill = "#263d66", col = NA, alpha = .16))
set.seed(15)
stars <- data.frame(x = runif(45, .03, .97), y = runif(45, .06, .95), r = runif(45, .002, .009))
for (i in seq_len(nrow(stars))) grid.circle(stars$x[[i]], stars$y[[i]], r = stars$r[[i]], gp = gpar(fill = "#eef1dc", col = NA, alpha = .7))
grid.text("SELECCIONA TU RUTA", x = .04, y = .965, just = "left", gp = gpar(col = "#e8d49b", fontsize = 12, fontface = "bold"))
grid.text("Una ventaja desbloqueada · una alternativa que dejas atrás", x = .04, y = .935, just = "left", gp = gpar(col = "#a7b5ca", fontsize = 8))
grid.text("NIVEL 01", x = .96, y = .965, just = "right", gp = gpar(col = "#a7b5ca", fontsize = 8))

draw_icon_clean <- function(x, y, kind, color = "#e7d49c") {
  if (kind == "book") { grid.polygon(c(x - .022, x - .002, x - .002, x - .021), c(y - .014, y - .010, y + .018, y + .012), gp = gpar(fill = "#e0bd62", col = "#111b2e", lwd = 1)); grid.polygon(c(x + .002, x + .022, x + .021, x + .002), c(y - .010, y - .014, y + .012, y + .018), gp = gpar(fill = "#f0d47b", col = "#111b2e", lwd = 1)); grid.lines(c(x, x), c(y - .012, y + .018), gp = gpar(col = "#7b5b2e", lwd = 1)) }
  if (kind == "briefcase") { grid.rect(x, y - .002, width = .044, height = .030, gp = gpar(fill = "#6c9fb0", col = "#111b2e", lwd = 1)); grid.lines(c(x - .012, x - .006, x + .006, x + .012), c(y + .013, y + .021, y + .021, y + .013), gp = gpar(col = "#e7d49c", lwd = 1.5)); grid.rect(x, y - .003, width = .007, height = .007, gp = gpar(fill = "#d5b15d", col = "#111b2e", lwd = .7)); grid.lines(c(x - .022, x + .022), c(y + .002, y + .002), gp = gpar(col = "#d5b15d", lwd = 1)) }
  if (kind == "store") { grid.rect(x, y - .010, width = .044, height = .030, gp = gpar(fill = "#e6d7b2", col = "#111b2e", lwd = 1)); grid.rect(x, y + .010, width = .052, height = .012, gp = gpar(fill = "#d58b67", col = "#111b2e", lwd = 1)); grid.lines(c(x - .014, x - .004, x + .006, x + .016), c(y + .010, y + .010, y + .010, y + .010), gp = gpar(col = "#f0d47b", lwd = 2)); grid.rect(x - .011, y - .010, width = .009, height = .012, gp = gpar(fill = "#6c9fb0", col = "#111b2e", lwd = .7)); grid.rect(x + .011, y - .010, width = .009, height = .012, gp = gpar(fill = "#6c9fb0", col = "#111b2e", lwd = .7)) }
  if (kind == "coin") { grid.circle(x - .009, y + .005, r = .012, gp = gpar(fill = "#e0bd62", col = "#111b2e", lwd = 1)); grid.circle(x + .009, y - .005, r = .012, gp = gpar(fill = "#c58a4d", col = "#111b2e", lwd = 1)); grid.lines(c(x - .009, x + .009), c(y + .005, y - .005), gp = gpar(col = "#f3e7c7", lwd = 1)) }
}
draw_node_clean <- function(x, y, label, sublabel, color, kind, radius = .026) {
  grid.circle(x, y, r = radius + .010, gp = gpar(fill = "#10192e", col = color, lwd = 2.5))
  grid.circle(x, y, r = radius - .004, gp = gpar(fill = "#17233b", col = "#6b7182", lwd = .5))
  draw_icon_clean(x, y, kind)
}
columns_clean <- data.frame(x = c(.22, .50, .78), color = c("#dfbd62", "#8fb8c9", "#d58b67"), title = c("ESTUDIAR", "TRABAJAR", "EMPRENDER"), benefit = c("capital humano", "ingreso y experiencia", "autonomía y control"), cost = c("renuncia: ingreso presente", "renuncia: tiempo de formación", "renuncia: estabilidad"), stringsAsFactors = FALSE)
for (i in seq_len(nrow(columns_clean))) {
  x <- columns_clean$x[[i]]; color <- columns_clean$color[[i]]
  grid.rect(x, .45, width = .255, height = .56, gp = gpar(fill = "#0d162b", col = color, lwd = 1.2))
  icon_x <- x - .073
  text_x <- x - .030
  grid.lines(c(.5, icon_x), c(.83, .66), gp = gpar(col = color, alpha = .55, lwd = 2.2))
  grid.lines(c(icon_x, icon_x), c(.66, .49), gp = gpar(col = color, alpha = .85, lwd = 2.2))
  grid.lines(c(icon_x, icon_x), c(.49, .35), gp = gpar(col = color, alpha = .85, lwd = 2.2))
  draw_node_clean(icon_x, .66, columns_clean$title[[i]], columns_clean$benefit[[i]], color, c("book", "briefcase", "store")[[i]], .030)
  draw_node_clean(icon_x, .49, c("TÍTULO", "EXPERIENCIA", "INVERTIR")[[i]], c("más opciones laborales", "red laboral", "control del proyecto")[[i]], color, c("book", "briefcase", "coin")[[i]], .025)
  draw_node_clean(icon_x, .35, c("ESPECIALIZAR", "ASCENSO", "ESCALAR")[[i]], c("salario potencial", "estabilidad futura", "riesgo y retorno")[[i]], color, c("book", "briefcase", "coin")[[i]], .025)
  grid.text(columns_clean$title[[i]], text_x, .682, just = "left", gp = gpar(col = "#f3e7c7", fontsize = 10, fontface = "bold"))
  grid.text(columns_clean$benefit[[i]], text_x, .642, just = "left", gp = gpar(col = "#aebed0", fontsize = 7.3))
  grid.text(c("TÍTULO", "EXPERIENCIA", "INVERTIR")[[i]], text_x, .512, just = "left", gp = gpar(col = "#f3e7c7", fontsize = 9, fontface = "bold"))
  grid.text(c("más opciones laborales", "red laboral", "control del proyecto")[[i]], text_x, .472, just = "left", gp = gpar(col = "#aebed0", fontsize = 7.1))
  grid.text(c("ESPECIALIZAR", "ASCENSO", "ESCALAR")[[i]], text_x, .372, just = "left", gp = gpar(col = "#f3e7c7", fontsize = 9, fontface = "bold"))
  grid.text(c("salario potencial", "estabilidad futura", "riesgo y retorno")[[i]], text_x, .332, just = "left", gp = gpar(col = "#aebed0", fontsize = 7.1))
  grid.rect(x, .20, width = .205, height = .045, gp = gpar(fill = "#17233b", col = color, lwd = 1))
  grid.text(paste("BENEFICIO ·", columns_clean$benefit[[i]]), x, .207, gp = gpar(col = "#d7e0e8", fontsize = 7.4, fontface = "bold"))
  grid.text(columns_clean$cost[[i]], x, .145, gp = gpar(col = color, fontsize = 7.2))
}
grid.circle(.5, .83, r = .037, gp = gpar(fill = "#10192e", col = "#e2bd63", lwd = 3))
draw_icon_clean(.5, .83, "coin")
grid.text("RECURSOS ESCASOS", .5, .925, gp = gpar(col = "#f3e7c7", fontsize = 10, fontface = "bold"))
grid.text("tiempo · dinero · atención", .5, .900, gp = gpar(col = "#aebed0", fontsize = 8))
grid.text("Cada ruta ofrece una ventaja; ninguna elimina el coste de elegir.", x = .5, y = .055, gp = gpar(col = "#d7e0e8", fontsize = 9))
dev.off()
constellation_asset <- file.path(repo_root, "posts", "fundamentos", "2026-07-25-videojuegos-y-economia", "assets", "constellation-decisions.png")
if (!file.exists(constellation_asset)) stop("Falta la composición Python de la constelación: ", constellation_asset)
file.copy(constellation_asset, file.path(out_dir, "02-constelacion-decisiones.png"), overwrite = TRUE)

png(file.path(out_dir, "03-ventana-comerciante-inflacion.png"), width = 1800, height = 900, res = 150, bg = "#111a24")
grid.newpage()
grid.rect(gp = gpar(fill = "#111a24", col = NA))
grid.text("NEW EDEN ECONOMIC NETWORK", x = .08, y = .95, just = "left", gp = gpar(col = "#91b9c9", fontsize = 8, fontface = "bold"))
grid.text("QUARTERLY ECONOMIC NEWSLETTER · Q4 2010", x = .08, y = .91, just = "left", gp = gpar(col = "#e2bd6a", fontsize = 13, fontface = "bold"))
grid.text("CCP · PRICE LEVELS", x = .92, y = .93, just = "right", gp = gpar(col = "#d5e7ed", fontsize = 9, fontface = "bold"))
grid.lines(x = c(.08, .92), y = c(.875, .875), gp = gpar(col = "#3f5661"))
events <- list(c("OCT", "PLEX +6%", "demanda sostenida", "#718e9b"), c("NOV", "combustible y rigs ↑", "insumos mas escasos", "#718e9b"), c("14 DIC", "regresan veteranos", "mas demanda de bienes", "#c66b47"))
for (i in seq_along(events)) {
  x <- c(.23, .50, .77)[[i]]
  grid.rect(x = x, y = .79, width = .24, height = .10, gp = gpar(fill = "#192631", col = events[[i]][[4]], lwd = 2))
  grid.text(events[[i]][[1]], x = x-.10, y = .82, just = "left", gp = gpar(col = "#9eb7c1", fontsize = 7, fontface = "bold"))
  grid.text(events[[i]][[2]], x = x-.10, y = .795, just = "left", gp = gpar(col = "#f0d08a", fontsize = 8, fontface = "bold"))
  grid.text(events[[i]][[3]], x = x-.10, y = .765, just = "left", gp = gpar(col = "#b9c5c8", fontsize = 7))
}
nodes <- list(c(.19, "ISK FAUCET", "PLEX +18%", "creacion diaria nov → dic", "#c4a65a"), c(.50, "DEMANDA", "mas ISK gastado", "jugadores que vuelven", "#c66b47"), c(.81, "INDICE DE PRECIOS", "CPI +4,7%", "inflacion general · trimestre", "#8eb8c8"))
for (i in seq_along(nodes)) {
  x <- as.numeric(nodes[[i]][[1]])
  grid.rect(x = x, y = .50, width = .22, height = .30, gp = gpar(fill = "#17232d", col = nodes[[i]][[5]], lwd = 2))
  grid.text(nodes[[i]][[2]], x = x-.095, y = .62, just = "left", gp = gpar(col = "#91b9c9", fontsize = 7, fontface = "bold"))
  grid.text(nodes[[i]][[3]], x = x-.095, y = .55, just = "left", gp = gpar(col = "#f3e7c7", fontsize = 12, fontface = "bold"))
  grid.text(nodes[[i]][[4]], x = x-.095, y = .47, just = "left", gp = gpar(col = "#b9c5c8", fontsize = 8))
  if (i < 3) grid.text("→", x = c(.345,.655)[[i]], y = .50, gp = gpar(col = "#e2bd6a", fontsize = 24))
}
results <- list(c("PLEX −3,4%", "mas oferta de PLEX", "#c4a65a"), c("TECH II +4,6%", "demanda de naves y modulos", "#c66b47"), c("no todos los precios suben", "la oferta tambien importa", "#718e9b"))
for (i in seq_along(results)) {
  x <- c(.23,.50,.77)[[i]]
  grid.lines(x = c(x-.10,x-.10), y = c(.30,.39), gp = gpar(col = results[[i]][[3]], lwd = 3))
  grid.text(results[[i]][[1]], x = x-.085, y = .32, just = "left", gp = gpar(col = "#f0d08a", fontsize = 8, fontface = "bold"))
  grid.text(results[[i]][[2]], x = x-.085, y = .285, just = "left", gp = gpar(col = "#aebbc0", fontsize = 7))
}
grid.lines(x = c(.08, .92), y = c(.20, .20), gp = gpar(col = "#3f5661"))
grid.text("Fuente: CCP · Quarterly Economic Newsletter, Q4 2010 · PLEX = Pilot License Extension", x = .08, y = .15, just = "left", gp = gpar(col = "#8fa2aa", fontsize = 7))
dev.off()
file.copy(constellation_asset, file.path(out_dir, "02-constelacion-decisiones.png"), overwrite = TRUE)
inflacion_asset <- file.path(repo_root, "posts", "fundamentos", "2026-07-25-videojuegos-y-economia", "assets", "inflacion", "eve-inflacion.png")
if (!file.exists(inflacion_asset)) stop("Falta la composición Python de inflación: ", inflacion_asset)
file.copy(inflacion_asset, file.path(out_dir, "03-ventana-comerciante-inflacion.png"), overwrite = TRUE)
