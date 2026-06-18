#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(readxl)
  library(janitor)
  library(stringr)
  library(stringi)
  library(scales)
})

sf_use_s2(FALSE)

root <- getwd()
post_dir <- file.path(root, "posts", "republica-en-un-grafico", "2026-01-06-Perpectiva_del_desarrollo")
shape_path <- file.path(root, "mapa_rd", "provincia", "PROVCenso2010.shp")
out_path <- file.path(root, "atlas", "data", "rd-provinces.geojson")

population_path <- list.files(post_dir, pattern = "poblaci", full.names = TRUE)[1]
business_path <- list.files(post_dir, pattern = "empresas", full.names = TRUE)[1]

normalize_key <- function(x) {
  x |>
    str_to_upper() |>
    str_trim() |>
    stri_trans_general("Latin-ASCII") |>
    str_replace_all("SEYBO", "SEIBO")
}

provinces <- st_read(shape_path, quiet = TRUE) |>
  st_transform(32619) |>
  st_make_valid() |>
  st_simplify(dTolerance = 1800, preserveTopology = TRUE) |>
  st_transform(4326) |>
  mutate(
    province_key = normalize_key(TOPONIMIA),
    province = str_to_title(stri_trans_general(TOPONIMIA, "Latin-ASCII")),
    region_code = REG
  )

population <- read_excel(population_path, skip = 5) |>
  clean_names() |>
  tidyr::drop_na(total_77) |>
  select(region_provincia, total_77) |>
  filter(!str_detect(region_provincia, "Region|Región")) |>
  mutate(province_key = normalize_key(region_provincia))

businesses <- read_csv(business_path, show_col_types = FALSE) |>
  clean_names() |>
  filter(anio == 2024) |>
  mutate(province_key = normalize_key(provincia_desc)) |>
  select(province_key, businesses = cuenta)

map_ready <- provinces |>
  left_join(businesses, by = "province_key") |>
  left_join(population, by = "province_key") |>
  mutate(
    population = as.numeric(total_77),
    business_density = businesses / population * 1000,
    opportunity = rescale(business_density, to = c(0, 100), from = range(business_density, na.rm = TRUE)),
    label = paste0(province, ": ", number(business_density, accuracy = 0.1), " empresas por 1,000 hab.")
  ) |>
  select(PROV, region_code, province, province_key, businesses, population, business_density, opportunity, label, geometry)

if (file.exists(out_path)) unlink(out_path)
st_write(map_ready, out_path, driver = "GeoJSON", quiet = TRUE)
message("Map asset built: ", out_path)
