#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(readxl)
  library(janitor)
  library(stringr)
  library(stringi)
  library(jsonlite)
  library(rnaturalearth)
})

sf_use_s2(FALSE)

root <- getwd()
out_dir <- file.path(root, "atlas", "data")

normalize_key <- function(x) {
  x |>
    str_to_upper() |>
    str_trim() |>
    stri_trans_general("Latin-ASCII") |>
    str_replace_all("SEYBO", "SEIBO")
}

clean_label <- function(x) {
  x |>
    str_to_title() |>
    str_replace_all(" De ", " de ") |>
    str_replace_all(" Del ", " del ")
}

write_geojson <- function(sf_obj, path) {
  if (file.exists(path)) unlink(path)
  st_write(sf_obj, path, driver = "GeoJSON", quiet = TRUE)
}

# MiPyMES regional map from the published article.
mipyme_dir <- file.path(root, "posts", "republica-en-un-grafico", "2026-02-14-mipymes-rd")
mipyme_path <- file.path(mipyme_dir, "Encuesta-Nacional-a-las-MIPYMES-2023-Base-de-datos.xlsx")

regions_shape <- st_read(file.path(root, "mapa_rd", "region", "REGCenso2010.shp"), quiet = TRUE) |>
  st_transform(32619) |>
  st_make_valid() |>
  st_simplify(dTolerance = 1800, preserveTopology = TRUE)

region_map <- tibble::tribble(
  ~region_shapefile, ~region,
  "REGION CIBAO NORTE", "Norte",
  "REGION CIBAO SUR", "Norte",
  "REGION CIBAO NORDESTE", "Norte",
  "REGION CIBAO NOROESTE", "Norte",
  "REGION VALDESIA", "Sur",
  "REGION ENRIQUILLO", "Sur",
  "REGION EL VALLE", "Sur",
  "REGION YUMA", "Este",
  "REGION HIGUAMO", "Este",
  "REGION OZAMA O METROPOLITANA", "Metropolitana"
)

mipyme_survey <- read_excel(mipyme_path) |>
  clean_names()

mipyme_region_stats <- mipyme_survey |>
  group_by(region, clasificacion_mipymes) |>
  summarise(empresas = sum(factor, na.rm = TRUE), .groups = "drop") |>
  group_by(region) |>
  mutate(pct = empresas / sum(empresas) * 100) |>
  filter(clasificacion_mipymes == "MICRO") |>
  transmute(region, pct_micro = pct) |>
  left_join(
    mipyme_survey |>
      mutate(informal = ifelse(formalidad == "Informal", 1, 0)) |>
      group_by(region) |>
      summarise(pct_informal = weighted.mean(informal, factor, na.rm = TRUE) * 100, .groups = "drop"),
    by = "region"
  ) |>
  mutate(
    label = paste0(region, ": ", round(pct_micro, 1), "% micro; ", round(pct_informal, 1), "% informal")
  )

regions_mipymes <- regions_shape |>
  mutate(region_key = normalize_key(TOPONIMIA)) |>
  left_join(region_map, by = c("region_key" = "region_shapefile")) |>
  group_by(region) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  left_join(mipyme_region_stats, by = "region") |>
  st_transform(4326)

write_geojson(regions_mipymes, file.path(out_dir, "rd-regions-mipymes.geojson"))

# Tourism world map and treemap from the tourism article.
tourism_dir <- file.path(root, "posts", "republica-habla-de", "2026-01-20-Turismo_expansion")
tourism_country <- read_csv(file.path(tourism_dir, "razones_turismo.csv"), show_col_types = FALSE) |>
  rename(country_survey = 1, beach_pct = 2) |>
  mutate(
    country_en = case_when(
      country_survey == "Canadá" ~ "Canada",
      country_survey == "Estados Unidos" ~ "United States of America",
      country_survey == "México" ~ "Mexico",
      country_survey == "Haití" ~ "Haiti",
      country_survey == "República Dominicana" ~ "Dominican Republic",
      country_survey == "Curazao" ~ "Curaçao",
      str_detect(country_survey, "Caicos y Turcas") ~ "Turks and Caicos Islands",
      str_detect(country_survey, "Vírgenes Americanas") ~ "United States Virgin Islands",
      country_survey == "San Martin" ~ "Saint Martin",
      country_survey == "Brasil" ~ "Brazil",
      country_survey == "Perú" ~ "Peru",
      country_survey == "Alemania" ~ "Germany",
      country_survey == "Bélgica" ~ "Belgium",
      country_survey == "Dinamarca" ~ "Denmark",
      country_survey == "España" ~ "Spain",
      country_survey == "Finlandia" ~ "Finland",
      country_survey == "Francia" ~ "France",
      country_survey == "Grecia" ~ "Greece",
      country_survey == "Holanda" ~ "Netherlands",
      country_survey == "Italia" ~ "Italy",
      country_survey == "Japón" ~ "Japan",
      country_survey == "Republica Checa" ~ "Czechia",
      country_survey == "Rusia" ~ "Russia",
      country_survey == "Suecia" ~ "Sweden",
      country_survey == "Suiza" ~ "Switzerland",
      country_survey %in% c("Reino Unido", "Escocia") ~ "United Kingdom",
      country_survey == "Corea del Sur" ~ "South Korea",
      TRUE ~ country_survey
    )
  )

world_tourism <- ne_countries(scale = "small", returnclass = "sf") |>
  filter(admin != "Antarctica") |>
  select(country_en = admin, geometry) |>
  st_make_valid() |>
  st_simplify(dTolerance = 0.12, preserveTopology = TRUE) |>
  left_join(tourism_country, by = "country_en") |>
  mutate(
    country = coalesce(country_survey, country_en),
    label = ifelse(is.na(beach_pct), country_en, paste0(country, ": ", round(beach_pct, 1), "% playa"))
  ) |>
  select(country, country_en, beach_pct, label, geometry)

write_geojson(world_tourism, file.path(out_dir, "world-tourism.geojson"))

tourism_treemap <- tibble::tribble(
  ~motivo, ~porcentaje,
  "Playas", 50.5,
  "Clima", 15.3,
  "Hospitalidad", 11.9,
  "Amigos/Familia", 9.9,
  "Precios", 5.3,
  "Trabajo", 1.5,
  "Tranquilidad", 1.2,
  "Historia", 1.0,
  "Naturaleza", 0.9,
  "Vida nocturna", 0.6,
  "Ecoturismo", 0.6,
  "Golf", 0.1,
  "Otros", 1.2
) |>
  mutate(
    categoria = case_when(
      motivo %in% c("Playas", "Clima", "Precios", "Vida nocturna", "Tranquilidad") ~ "Masivo",
      motivo %in% c("Hospitalidad", "Amigos/Familia", "Trabajo", "Otros") ~ "Vinculado",
      TRUE ~ "Nicho"
    )
  )

# Transport article: relationship between rent and formal employment concentration.
transport_dir <- file.path(root, "posts", "republica-habla-de", "2026-03-04-transporte-masivo")
employment <- read_csv(file.path(transport_dir, "tss_trabajadores_provincia_2021.csv"), show_col_types = FALSE) |>
  mutate(
    province_key = normalize_key(provincia),
    province = case_when(
      province_key == "SANTIAGO DE LOS CABALLEROS" ~ "Santiago",
      province_key == "MONSENOR NOUEL" ~ "Monseñor Nouel",
      province_key == "MARIA TRINIDAD SANCHEZ" ~ "M. Trinidad Sánchez",
      province_key == "SAN JUAN DE LA MAGUANA" ~ "San Juan",
      TRUE ~ clean_label(stri_trans_general(provincia, "Latin-ASCII"))
    )
  )

rent <- read_csv(file.path(transport_dir, "mipymes_alquiler_prov_expandido.csv"), show_col_types = FALSE) |>
  filter(clasificacion == "Microempresa") |>
  mutate(
    province_key = normalize_key(provincia),
    rent_thousand = mediana_alquiler_rd / 1000
  ) |>
  group_by(province_key) |>
  summarise(median_rent_rd = weighted.mean(mediana_alquiler_rd, n_expandido, na.rm = TRUE), .groups = "drop")

transport_space <- employment |>
  left_join(rent, by = "province_key") |>
  filter(!is.na(median_rent_rd)) |>
  mutate(
    median_rent_thousand = median_rent_rd / 1000,
    category = case_when(
      province == "Distrito Nacional" ~ "DN",
      province %in% c("Santo Domingo", "San Cristobal") ~ "Periurbana GSD",
      TRUE ~ "Resto"
    )
  ) |>
  arrange(desc(pct_empleo_nacional)) |>
  transmute(
    province,
    category,
    workers = trabajadores,
    jobs = empleos,
    employment_share = pct_empleo_nacional,
    median_rent_rd,
    median_rent_thousand
  )

# Debt article: compact fiscal burden series.
debt <- read_csv(file.path(root, "posts", "republica-habla-de", "2025-12-19_deuda_publica", "deuda_rd.csv"), show_col_types = FALSE) |>
  group_by(anio) |>
  summarise(
    principal = sum(principal, na.rm = TRUE),
    interest = sum(intereses, na.rm = TRUE),
    commissions = sum(comisiones, na.rm = TRUE),
    service = principal + interest + commissions,
    interest_share = interest / service * 100,
    .groups = "drop"
  )

payload <- list(
  updated = as.character(Sys.Date()),
  sources = list(
    list(title = "Densidad empresarial por provincia", article = "2026-01-06-Perpectiva_del_desarrollo", href = "../posts/republica-en-un-grafico/2026-01-06-Perpectiva_del_desarrollo/index.html", files = c("PROVCenso2010.shp", "empresas registradas 2015-2024", "poblacion ONE")),
    list(title = "MiPyMES por region", article = "2026-02-14-mipymes-rd", href = "../posts/republica-en-un-grafico/2026-02-14-mipymes-rd/index.html", files = c("REGCenso2010.shp", basename(mipyme_path))),
    list(title = "Turismo por pais y motivo", article = "2026-01-20-Turismo_expansion", href = "../posts/republica-habla-de/2026-01-20-Turismo_expansion/index.html", files = c("razones_turismo.csv", "rnaturalearth")),
    list(title = "Transporte masivo y empleo formal", article = "2026-03-04-transporte-masivo", href = "../posts/republica-habla-de/2026-03-04-transporte-masivo/index.html", files = c("tss_trabajadores_provincia_2021.csv", "mipymes_alquiler_prov_expandido.csv")),
    list(title = "Deuda publica", article = "2025-12-19_deuda_publica", href = "../posts/republica-habla-de/2025-12-19_deuda_publica/index.html", files = c("deuda_rd.csv"))
  ),
  tourism = list(
    treemap = tourism_treemap,
    countryPreference = tourism_country |>
      arrange(desc(beach_pct)) |>
      transmute(country = country_survey, beach_pct)
  ),
  transport = list(
    rentEmployment = transport_space,
    employmentTop = employment |>
      arrange(desc(pct_empleo_nacional)) |>
      slice_head(n = 12) |>
      transmute(province, employment_share = pct_empleo_nacional, jobs = empleos, workers = trabajadores)
  ),
  debt = list(
    service = debt
  )
)

write_json(payload, file.path(out_dir, "article-visuals.json"), pretty = TRUE, auto_unbox = TRUE, na = "null")
message("Article visual assets built in ", out_dir)
