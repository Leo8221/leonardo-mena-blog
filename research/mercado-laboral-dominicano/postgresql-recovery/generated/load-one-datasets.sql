\set ON_ERROR_STOP on
CREATE SCHEMA IF NOT EXISTS raw;
CREATE SCHEMA IF NOT EXISTS meta;

DROP TABLE IF EXISTS raw.atmosfera_clima_1991_2025 CASCADE;
CREATE TABLE raw.atmosfera_clima_1991_2025 (
  "provincia" text,
  "idprovincia" text,
  "estacion" text,
  "ano" text,
  "mes" text,
  "variable" text,
  "valor" text,
  "unidad_de_medida" text,
  "col" text
);
\copy raw.atmosfera_clima_1991_2025 ("provincia", "idprovincia", "estacion", "ano", "mes", "variable", "valor", "unidad_de_medida", "col") FROM 'C:/Users/leona/leonardo-mena-blog/research/mercado-laboral-dominicano/postgresql-recovery/../data/raw/one_csv/atmosfera_clima_1991_2025.csv' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8', NULL '');
ANALYZE raw.atmosfera_clima_1991_2025;
DROP TABLE IF EXISTS public.atmosfera_clima_1991_2025 CASCADE;
CREATE TABLE public.atmosfera_clima_1991_2025 AS SELECT
  NULLIF(BTRIM("provincia"), '') AS "provincia",
  CASE WHEN BTRIM("idprovincia") ~ '^-?[0-9]+$' THEN BTRIM("idprovincia")::integer END AS "idprovincia",
  NULLIF(BTRIM("estacion"), '') AS "estacion",
  CASE WHEN BTRIM("ano") ~ '^-?[0-9]+$' THEN BTRIM("ano")::integer END AS "ano",
  NULLIF(BTRIM("mes"), '') AS "mes",
  NULLIF(BTRIM("variable"), '') AS "variable",
  CASE WHEN BTRIM("valor") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("valor")::numeric END AS "valor",
  NULLIF(BTRIM("unidad_de_medida"), '') AS "unidad_de_medida",
  NULLIF(BTRIM("col"), '') AS "col"
FROM raw.atmosfera_clima_1991_2025;
ANALYZE public.atmosfera_clima_1991_2025;

DROP TABLE IF EXISTS raw.atmosfera_clima_ca_2017_2023 CASCADE;
CREATE TABLE raw.atmosfera_clima_ca_2017_2023 (
  "estacion" text,
  "provincia" text,
  "ano" text,
  "mes" text,
  "precipitacion" text,
  "temperatura_maxima" text,
  "temperatura_minima" text,
  "velocidad_del_viento" text,
  "humedad_relativa" text,
  "presion_atmosferica" text
);
\copy raw.atmosfera_clima_ca_2017_2023 ("estacion", "provincia", "ano", "mes", "precipitacion", "temperatura_maxima", "temperatura_minima", "velocidad_del_viento", "humedad_relativa", "presion_atmosferica") FROM 'C:/Users/leona/leonardo-mena-blog/research/mercado-laboral-dominicano/postgresql-recovery/../data/raw/one_csv/atmosfera_clima_ca_2017_2023.csv' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8', NULL '');
ANALYZE raw.atmosfera_clima_ca_2017_2023;
DROP TABLE IF EXISTS public.atmosfera_clima_ca_2017_2023 CASCADE;
CREATE TABLE public.atmosfera_clima_ca_2017_2023 AS SELECT
  NULLIF(BTRIM("estacion"), '') AS "estacion",
  NULLIF(BTRIM("provincia"), '') AS "provincia",
  CASE WHEN BTRIM("ano") ~ '^-?[0-9]+$' THEN BTRIM("ano")::integer END AS "ano",
  NULLIF(BTRIM("mes"), '') AS "mes",
  CASE WHEN BTRIM("precipitacion") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("precipitacion")::numeric END AS "precipitacion",
  CASE WHEN BTRIM("temperatura_maxima") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("temperatura_maxima")::numeric END AS "temperatura_maxima",
  CASE WHEN BTRIM("temperatura_minima") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("temperatura_minima")::numeric END AS "temperatura_minima",
  CASE WHEN BTRIM("velocidad_del_viento") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("velocidad_del_viento")::numeric END AS "velocidad_del_viento",
  CASE WHEN BTRIM("humedad_relativa") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("humedad_relativa")::numeric END AS "humedad_relativa",
  CASE WHEN BTRIM("presion_atmosferica") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("presion_atmosferica")::numeric END AS "presion_atmosferica"
FROM raw.atmosfera_clima_ca_2017_2023;
ANALYZE public.atmosfera_clima_ca_2017_2023;

DROP TABLE IF EXISTS raw.atmosfera_clima_ca_old CASCADE;
CREATE TABLE raw.atmosfera_clima_ca_old (
  "estacion" text,
  "provincia" text,
  "ano" text,
  "mes" text,
  "precipitacion" text,
  "temperatura_maxima" text,
  "temperatura_minima" text,
  "presion_atmosferica" text,
  "velocidad_del_viento" text,
  "nubosidad" text,
  "humedad_relativa" text
);
\copy raw.atmosfera_clima_ca_old ("estacion", "provincia", "ano", "mes", "precipitacion", "temperatura_maxima", "temperatura_minima", "presion_atmosferica", "velocidad_del_viento", "nubosidad", "humedad_relativa") FROM 'C:/Users/leona/leonardo-mena-blog/research/mercado-laboral-dominicano/postgresql-recovery/../data/raw/one_csv/atmosfera_clima_ca_old.csv' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8', NULL '');
ANALYZE raw.atmosfera_clima_ca_old;
DROP TABLE IF EXISTS public.atmosfera_clima_ca_old CASCADE;
CREATE TABLE public.atmosfera_clima_ca_old AS SELECT
  NULLIF(BTRIM("estacion"), '') AS "estacion",
  NULLIF(BTRIM("provincia"), '') AS "provincia",
  CASE WHEN BTRIM("ano") ~ '^-?[0-9]+$' THEN BTRIM("ano")::integer END AS "ano",
  NULLIF(BTRIM("mes"), '') AS "mes",
  CASE WHEN BTRIM("precipitacion") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("precipitacion")::numeric END AS "precipitacion",
  CASE WHEN BTRIM("temperatura_maxima") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("temperatura_maxima")::numeric END AS "temperatura_maxima",
  CASE WHEN BTRIM("temperatura_minima") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("temperatura_minima")::numeric END AS "temperatura_minima",
  CASE WHEN BTRIM("presion_atmosferica") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("presion_atmosferica")::numeric END AS "presion_atmosferica",
  CASE WHEN BTRIM("velocidad_del_viento") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("velocidad_del_viento")::numeric END AS "velocidad_del_viento",
  CASE WHEN BTRIM("nubosidad") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("nubosidad")::numeric END AS "nubosidad",
  CASE WHEN BTRIM("humedad_relativa") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("humedad_relativa")::numeric END AS "humedad_relativa"
FROM raw.atmosfera_clima_ca_old;
ANALYZE public.atmosfera_clima_ca_old;

DROP TABLE IF EXISTS raw.eventos_fenomenos_naturales CASCADE;
CREATE TABLE raw.eventos_fenomenos_naturales (
  "sid" text,
  "basin" text,
  "subbasin" text,
  "name" text,
  "nature" text,
  "year" text,
  "month" text,
  "day" text,
  "usa_wind" text,
  "usa_pres" text,
  "categoria" text,
  "zona_afectada" text
);
\copy raw.eventos_fenomenos_naturales ("sid", "basin", "subbasin", "name", "nature", "year", "month", "day", "usa_wind", "usa_pres", "categoria", "zona_afectada") FROM 'C:/Users/leona/leonardo-mena-blog/research/mercado-laboral-dominicano/postgresql-recovery/../data/raw/one_csv/eventos_fenomenos_naturales.csv' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8', NULL '');
ANALYZE raw.eventos_fenomenos_naturales;
DROP TABLE IF EXISTS public.eventos_fenomenos_naturales CASCADE;
CREATE TABLE public.eventos_fenomenos_naturales AS SELECT
  NULLIF(BTRIM("sid"), '') AS "sid",
  NULLIF(BTRIM("basin"), '') AS "basin",
  NULLIF(BTRIM("subbasin"), '') AS "subbasin",
  NULLIF(BTRIM("name"), '') AS "name",
  NULLIF(BTRIM("nature"), '') AS "nature",
  CASE WHEN BTRIM("year") ~ '^-?[0-9]+$' THEN BTRIM("year")::integer END AS "year",
  CASE WHEN BTRIM("month") ~ '^-?[0-9]+$' THEN BTRIM("month")::integer END AS "month",
  CASE WHEN BTRIM("day") ~ '^-?[0-9]+$' THEN BTRIM("day")::integer END AS "day",
  CASE WHEN BTRIM("usa_wind") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("usa_wind")::numeric END AS "usa_wind",
  CASE WHEN BTRIM("usa_pres") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("usa_pres")::numeric END AS "usa_pres",
  NULLIF(BTRIM("categoria"), '') AS "categoria",
  NULLIF(BTRIM("zona_afectada"), '') AS "zona_afectada"
FROM raw.eventos_fenomenos_naturales;
ANALYZE public.eventos_fenomenos_naturales;

DROP TABLE IF EXISTS raw.gastos_gobiernos_locales_2022 CASCADE;
CREATE TABLE raw.gastos_gobiernos_locales_2022 (
  "periodo" text,
  "cod_mes" text,
  "mes" text,
  "cod_region" text,
  "region" text,
  "cod_provincia" text,
  "provincia" text,
  "cod_municipio" text,
  "municipio" text,
  "cod_capitulo" text,
  "capitulo" text,
  "cod_eco_titulo" text,
  "eco_titulo" text,
  "cod_eco_sub_titulo" text,
  "eco_sub_titulo" text,
  "cod_concepto" text,
  "concepto" text,
  "cod_cuenta" text,
  "cuenta" text,
  "devengado" text
);
\copy raw.gastos_gobiernos_locales_2022 ("periodo", "cod_mes", "mes", "cod_region", "region", "cod_provincia", "provincia", "cod_municipio", "municipio", "cod_capitulo", "capitulo", "cod_eco_titulo", "eco_titulo", "cod_eco_sub_titulo", "eco_sub_titulo", "cod_concepto", "concepto", "cod_cuenta", "cuenta", "devengado") FROM 'C:/Users/leona/leonardo-mena-blog/research/mercado-laboral-dominicano/postgresql-recovery/../data/raw/one_csv/gastos_gobiernos_locales_2022.csv' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8', NULL '');
ANALYZE raw.gastos_gobiernos_locales_2022;
DROP TABLE IF EXISTS public.gastos_gobiernos_locales_2022 CASCADE;
CREATE TABLE public.gastos_gobiernos_locales_2022 AS SELECT
  CASE WHEN BTRIM("periodo") ~ '^-?[0-9]+$' THEN BTRIM("periodo")::integer END AS "periodo",
  NULLIF(BTRIM("cod_mes"), '') AS "cod_mes",
  NULLIF(BTRIM("mes"), '') AS "mes",
  NULLIF(BTRIM("cod_region"), '') AS "cod_region",
  NULLIF(BTRIM("region"), '') AS "region",
  NULLIF(BTRIM("cod_provincia"), '') AS "cod_provincia",
  NULLIF(BTRIM("provincia"), '') AS "provincia",
  NULLIF(BTRIM("cod_municipio"), '') AS "cod_municipio",
  NULLIF(BTRIM("municipio"), '') AS "municipio",
  NULLIF(BTRIM("cod_capitulo"), '') AS "cod_capitulo",
  NULLIF(BTRIM("capitulo"), '') AS "capitulo",
  NULLIF(BTRIM("cod_eco_titulo"), '') AS "cod_eco_titulo",
  NULLIF(BTRIM("eco_titulo"), '') AS "eco_titulo",
  NULLIF(BTRIM("cod_eco_sub_titulo"), '') AS "cod_eco_sub_titulo",
  NULLIF(BTRIM("eco_sub_titulo"), '') AS "eco_sub_titulo",
  NULLIF(BTRIM("cod_concepto"), '') AS "cod_concepto",
  NULLIF(BTRIM("concepto"), '') AS "concepto",
  NULLIF(BTRIM("cod_cuenta"), '') AS "cod_cuenta",
  NULLIF(BTRIM("cuenta"), '') AS "cuenta",
  CASE WHEN BTRIM("devengado") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("devengado")::numeric END AS "devengado"
FROM raw.gastos_gobiernos_locales_2022;
ANALYZE public.gastos_gobiernos_locales_2022;

DROP TABLE IF EXISTS raw.gastos_gobiernos_locales_2023 CASCADE;
CREATE TABLE raw.gastos_gobiernos_locales_2023 (
  "periodo" text,
  "cod_mes" text,
  "mes" text,
  "cod_region" text,
  "region" text,
  "cod_provincia" text,
  "provincia" text,
  "cod_municipio" text,
  "municipio" text,
  "cod_capitulo" text,
  "capitulo" text,
  "cod_eco_titulo" text,
  "eco_titulo" text,
  "cod_eco_sub_titulo" text,
  "eco_sub_titulo" text,
  "cod_concepto" text,
  "concepto" text,
  "cod_cuenta" text,
  "cuenta" text,
  "devengado" text
);
\copy raw.gastos_gobiernos_locales_2023 ("periodo", "cod_mes", "mes", "cod_region", "region", "cod_provincia", "provincia", "cod_municipio", "municipio", "cod_capitulo", "capitulo", "cod_eco_titulo", "eco_titulo", "cod_eco_sub_titulo", "eco_sub_titulo", "cod_concepto", "concepto", "cod_cuenta", "cuenta", "devengado") FROM 'C:/Users/leona/leonardo-mena-blog/research/mercado-laboral-dominicano/postgresql-recovery/../data/raw/one_csv/gastos_gobiernos_locales_2023.csv' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8', NULL '');
ANALYZE raw.gastos_gobiernos_locales_2023;
DROP TABLE IF EXISTS public.gastos_gobiernos_locales_2023 CASCADE;
CREATE TABLE public.gastos_gobiernos_locales_2023 AS SELECT
  CASE WHEN BTRIM("periodo") ~ '^-?[0-9]+$' THEN BTRIM("periodo")::integer END AS "periodo",
  NULLIF(BTRIM("cod_mes"), '') AS "cod_mes",
  NULLIF(BTRIM("mes"), '') AS "mes",
  NULLIF(BTRIM("cod_region"), '') AS "cod_region",
  NULLIF(BTRIM("region"), '') AS "region",
  NULLIF(BTRIM("cod_provincia"), '') AS "cod_provincia",
  NULLIF(BTRIM("provincia"), '') AS "provincia",
  NULLIF(BTRIM("cod_municipio"), '') AS "cod_municipio",
  NULLIF(BTRIM("municipio"), '') AS "municipio",
  NULLIF(BTRIM("cod_capitulo"), '') AS "cod_capitulo",
  NULLIF(BTRIM("capitulo"), '') AS "capitulo",
  NULLIF(BTRIM("cod_eco_titulo"), '') AS "cod_eco_titulo",
  NULLIF(BTRIM("eco_titulo"), '') AS "eco_titulo",
  NULLIF(BTRIM("cod_eco_sub_titulo"), '') AS "cod_eco_sub_titulo",
  NULLIF(BTRIM("eco_sub_titulo"), '') AS "eco_sub_titulo",
  NULLIF(BTRIM("cod_concepto"), '') AS "cod_concepto",
  NULLIF(BTRIM("concepto"), '') AS "concepto",
  NULLIF(BTRIM("cod_cuenta"), '') AS "cod_cuenta",
  NULLIF(BTRIM("cuenta"), '') AS "cuenta",
  CASE WHEN BTRIM("devengado") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("devengado")::numeric END AS "devengado"
FROM raw.gastos_gobiernos_locales_2023;
ANALYZE public.gastos_gobiernos_locales_2023;

DROP TABLE IF EXISTS raw.gastos_gobiernos_locales_2024 CASCADE;
CREATE TABLE raw.gastos_gobiernos_locales_2024 (
  "periodo" text,
  "cod_mes" text,
  "mes" text,
  "cod_region" text,
  "region" text,
  "cod_provincia" text,
  "provincia" text,
  "cod_municipio" text,
  "municipio" text,
  "cod_capitulo" text,
  "capitulo" text,
  "cod_eco_titulo" text,
  "eco_titulo" text,
  "cod_eco_sub_titulo" text,
  "eco_sub_titulo" text,
  "cod_concepto" text,
  "concepto" text,
  "cod_cuenta" text,
  "cuenta" text,
  "devengado" text
);
\copy raw.gastos_gobiernos_locales_2024 ("periodo", "cod_mes", "mes", "cod_region", "region", "cod_provincia", "provincia", "cod_municipio", "municipio", "cod_capitulo", "capitulo", "cod_eco_titulo", "eco_titulo", "cod_eco_sub_titulo", "eco_sub_titulo", "cod_concepto", "concepto", "cod_cuenta", "cuenta", "devengado") FROM 'C:/Users/leona/leonardo-mena-blog/research/mercado-laboral-dominicano/postgresql-recovery/../data/raw/one_csv/gastos_gobiernos_locales_2024.csv' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8', NULL '');
ANALYZE raw.gastos_gobiernos_locales_2024;
DROP TABLE IF EXISTS public.gastos_gobiernos_locales_2024 CASCADE;
CREATE TABLE public.gastos_gobiernos_locales_2024 AS SELECT
  CASE WHEN BTRIM("periodo") ~ '^-?[0-9]+$' THEN BTRIM("periodo")::integer END AS "periodo",
  NULLIF(BTRIM("cod_mes"), '') AS "cod_mes",
  NULLIF(BTRIM("mes"), '') AS "mes",
  NULLIF(BTRIM("cod_region"), '') AS "cod_region",
  NULLIF(BTRIM("region"), '') AS "region",
  NULLIF(BTRIM("cod_provincia"), '') AS "cod_provincia",
  NULLIF(BTRIM("provincia"), '') AS "provincia",
  NULLIF(BTRIM("cod_municipio"), '') AS "cod_municipio",
  NULLIF(BTRIM("municipio"), '') AS "municipio",
  NULLIF(BTRIM("cod_capitulo"), '') AS "cod_capitulo",
  NULLIF(BTRIM("capitulo"), '') AS "capitulo",
  NULLIF(BTRIM("cod_eco_titulo"), '') AS "cod_eco_titulo",
  NULLIF(BTRIM("eco_titulo"), '') AS "eco_titulo",
  NULLIF(BTRIM("cod_eco_sub_titulo"), '') AS "cod_eco_sub_titulo",
  NULLIF(BTRIM("eco_sub_titulo"), '') AS "eco_sub_titulo",
  NULLIF(BTRIM("cod_concepto"), '') AS "cod_concepto",
  NULLIF(BTRIM("concepto"), '') AS "concepto",
  NULLIF(BTRIM("cod_cuenta"), '') AS "cod_cuenta",
  NULLIF(BTRIM("cuenta"), '') AS "cuenta",
  CASE WHEN BTRIM("devengado") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("devengado")::numeric END AS "devengado"
FROM raw.gastos_gobiernos_locales_2024;
ANALYZE public.gastos_gobiernos_locales_2024;

DROP TABLE IF EXISTS raw.ingresos_gobiernos_locales_2022 CASCADE;
CREATE TABLE raw.ingresos_gobiernos_locales_2022 (
  "periodo" text,
  "cod_mes" text,
  "mes" text,
  "cod_region" text,
  "region" text,
  "cod_provincia" text,
  "provincia" text,
  "cod_municipio" text,
  "municipio" text,
  "cod_capitulo" text,
  "unidad_ejecutora" text,
  "cod_eco_titulo" text,
  "eco_titulo" text,
  "cod_eco_sub_titulo" text,
  "eco_sub_titulo" text,
  "cod_concepto" text,
  "concepto" text,
  "cod_cuenta" text,
  "cuenta" text,
  "percibido" text
);
\copy raw.ingresos_gobiernos_locales_2022 ("periodo", "cod_mes", "mes", "cod_region", "region", "cod_provincia", "provincia", "cod_municipio", "municipio", "cod_capitulo", "unidad_ejecutora", "cod_eco_titulo", "eco_titulo", "cod_eco_sub_titulo", "eco_sub_titulo", "cod_concepto", "concepto", "cod_cuenta", "cuenta", "percibido") FROM 'C:/Users/leona/leonardo-mena-blog/research/mercado-laboral-dominicano/postgresql-recovery/../data/raw/one_csv/ingresos_gobiernos_locales_2022.csv' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8', NULL '');
ANALYZE raw.ingresos_gobiernos_locales_2022;
DROP TABLE IF EXISTS public.ingresos_gobiernos_locales_2022 CASCADE;
CREATE TABLE public.ingresos_gobiernos_locales_2022 AS SELECT
  CASE WHEN BTRIM("periodo") ~ '^-?[0-9]+$' THEN BTRIM("periodo")::integer END AS "periodo",
  NULLIF(BTRIM("cod_mes"), '') AS "cod_mes",
  NULLIF(BTRIM("mes"), '') AS "mes",
  NULLIF(BTRIM("cod_region"), '') AS "cod_region",
  NULLIF(BTRIM("region"), '') AS "region",
  NULLIF(BTRIM("cod_provincia"), '') AS "cod_provincia",
  NULLIF(BTRIM("provincia"), '') AS "provincia",
  NULLIF(BTRIM("cod_municipio"), '') AS "cod_municipio",
  NULLIF(BTRIM("municipio"), '') AS "municipio",
  NULLIF(BTRIM("cod_capitulo"), '') AS "cod_capitulo",
  NULLIF(BTRIM("unidad_ejecutora"), '') AS "unidad_ejecutora",
  NULLIF(BTRIM("cod_eco_titulo"), '') AS "cod_eco_titulo",
  NULLIF(BTRIM("eco_titulo"), '') AS "eco_titulo",
  NULLIF(BTRIM("cod_eco_sub_titulo"), '') AS "cod_eco_sub_titulo",
  NULLIF(BTRIM("eco_sub_titulo"), '') AS "eco_sub_titulo",
  NULLIF(BTRIM("cod_concepto"), '') AS "cod_concepto",
  NULLIF(BTRIM("concepto"), '') AS "concepto",
  NULLIF(BTRIM("cod_cuenta"), '') AS "cod_cuenta",
  NULLIF(BTRIM("cuenta"), '') AS "cuenta",
  CASE WHEN BTRIM("percibido") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("percibido")::numeric END AS "percibido"
FROM raw.ingresos_gobiernos_locales_2022;
ANALYZE public.ingresos_gobiernos_locales_2022;

DROP TABLE IF EXISTS raw.ingresos_gobiernos_locales_2023 CASCADE;
CREATE TABLE raw.ingresos_gobiernos_locales_2023 (
  "periodo" text,
  "cod_mes" text,
  "mes" text,
  "cod_region" text,
  "region" text,
  "cod_provincia" text,
  "provincia" text,
  "cod_municipio" text,
  "municipio" text,
  "cod_capitulo" text,
  "unidad_ejecutora" text,
  "cod_eco_titulo" text,
  "eco_titulo" text,
  "cod_eco_sub_titulo" text,
  "eco_sub_titulo" text,
  "cod_concepto" text,
  "concepto" text,
  "cod_cuenta" text,
  "cuenta" text,
  "percibido" text
);
\copy raw.ingresos_gobiernos_locales_2023 ("periodo", "cod_mes", "mes", "cod_region", "region", "cod_provincia", "provincia", "cod_municipio", "municipio", "cod_capitulo", "unidad_ejecutora", "cod_eco_titulo", "eco_titulo", "cod_eco_sub_titulo", "eco_sub_titulo", "cod_concepto", "concepto", "cod_cuenta", "cuenta", "percibido") FROM 'C:/Users/leona/leonardo-mena-blog/research/mercado-laboral-dominicano/postgresql-recovery/../data/raw/one_csv/ingresos_gobiernos_locales_2023.csv' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8', NULL '');
ANALYZE raw.ingresos_gobiernos_locales_2023;
DROP TABLE IF EXISTS public.ingresos_gobiernos_locales_2023 CASCADE;
CREATE TABLE public.ingresos_gobiernos_locales_2023 AS SELECT
  CASE WHEN BTRIM("periodo") ~ '^-?[0-9]+$' THEN BTRIM("periodo")::integer END AS "periodo",
  NULLIF(BTRIM("cod_mes"), '') AS "cod_mes",
  NULLIF(BTRIM("mes"), '') AS "mes",
  NULLIF(BTRIM("cod_region"), '') AS "cod_region",
  NULLIF(BTRIM("region"), '') AS "region",
  NULLIF(BTRIM("cod_provincia"), '') AS "cod_provincia",
  NULLIF(BTRIM("provincia"), '') AS "provincia",
  NULLIF(BTRIM("cod_municipio"), '') AS "cod_municipio",
  NULLIF(BTRIM("municipio"), '') AS "municipio",
  NULLIF(BTRIM("cod_capitulo"), '') AS "cod_capitulo",
  NULLIF(BTRIM("unidad_ejecutora"), '') AS "unidad_ejecutora",
  NULLIF(BTRIM("cod_eco_titulo"), '') AS "cod_eco_titulo",
  NULLIF(BTRIM("eco_titulo"), '') AS "eco_titulo",
  NULLIF(BTRIM("cod_eco_sub_titulo"), '') AS "cod_eco_sub_titulo",
  NULLIF(BTRIM("eco_sub_titulo"), '') AS "eco_sub_titulo",
  NULLIF(BTRIM("cod_concepto"), '') AS "cod_concepto",
  NULLIF(BTRIM("concepto"), '') AS "concepto",
  NULLIF(BTRIM("cod_cuenta"), '') AS "cod_cuenta",
  NULLIF(BTRIM("cuenta"), '') AS "cuenta",
  CASE WHEN BTRIM("percibido") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("percibido")::numeric END AS "percibido"
FROM raw.ingresos_gobiernos_locales_2023;
ANALYZE public.ingresos_gobiernos_locales_2023;

DROP TABLE IF EXISTS raw.ingresos_gobiernos_locales_2024 CASCADE;
CREATE TABLE raw.ingresos_gobiernos_locales_2024 (
  "periodo" text,
  "cod_mes" text,
  "mes" text,
  "cod_region" text,
  "region" text,
  "cod_provincia" text,
  "provincia" text,
  "cod_municipio" text,
  "municipio" text,
  "cod_capitulo" text,
  "unidad_ejecutora" text,
  "cod_eco_titulo" text,
  "eco_titulo" text,
  "cod_eco_sub_titulo" text,
  "eco_sub_titulo" text,
  "cod_concepto" text,
  "concepto" text,
  "cod_cuenta" text,
  "cuenta" text,
  "percibido" text
);
\copy raw.ingresos_gobiernos_locales_2024 ("periodo", "cod_mes", "mes", "cod_region", "region", "cod_provincia", "provincia", "cod_municipio", "municipio", "cod_capitulo", "unidad_ejecutora", "cod_eco_titulo", "eco_titulo", "cod_eco_sub_titulo", "eco_sub_titulo", "cod_concepto", "concepto", "cod_cuenta", "cuenta", "percibido") FROM 'C:/Users/leona/leonardo-mena-blog/research/mercado-laboral-dominicano/postgresql-recovery/../data/raw/one_csv/ingresos_gobiernos_locales_2024.csv' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8', NULL '');
ANALYZE raw.ingresos_gobiernos_locales_2024;
DROP TABLE IF EXISTS public.ingresos_gobiernos_locales_2024 CASCADE;
CREATE TABLE public.ingresos_gobiernos_locales_2024 AS SELECT
  CASE WHEN BTRIM("periodo") ~ '^-?[0-9]+$' THEN BTRIM("periodo")::integer END AS "periodo",
  NULLIF(BTRIM("cod_mes"), '') AS "cod_mes",
  NULLIF(BTRIM("mes"), '') AS "mes",
  NULLIF(BTRIM("cod_region"), '') AS "cod_region",
  NULLIF(BTRIM("region"), '') AS "region",
  NULLIF(BTRIM("cod_provincia"), '') AS "cod_provincia",
  NULLIF(BTRIM("provincia"), '') AS "provincia",
  NULLIF(BTRIM("cod_municipio"), '') AS "cod_municipio",
  NULLIF(BTRIM("municipio"), '') AS "municipio",
  NULLIF(BTRIM("cod_capitulo"), '') AS "cod_capitulo",
  NULLIF(BTRIM("unidad_ejecutora"), '') AS "unidad_ejecutora",
  NULLIF(BTRIM("cod_eco_titulo"), '') AS "cod_eco_titulo",
  NULLIF(BTRIM("eco_titulo"), '') AS "eco_titulo",
  NULLIF(BTRIM("cod_eco_sub_titulo"), '') AS "cod_eco_sub_titulo",
  NULLIF(BTRIM("eco_sub_titulo"), '') AS "eco_sub_titulo",
  NULLIF(BTRIM("cod_concepto"), '') AS "cod_concepto",
  NULLIF(BTRIM("concepto"), '') AS "concepto",
  NULLIF(BTRIM("cod_cuenta"), '') AS "cod_cuenta",
  NULLIF(BTRIM("cuenta"), '') AS "cuenta",
  CASE WHEN BTRIM("percibido") ~ '^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$' THEN BTRIM("percibido")::numeric END AS "percibido"
FROM raw.ingresos_gobiernos_locales_2024;
ANALYZE public.ingresos_gobiernos_locales_2024;

DROP VIEW IF EXISTS public.gastos_gobiernos_locales CASCADE;
CREATE VIEW public.gastos_gobiernos_locales AS
SELECT 2022 AS fuente_anio, t.* FROM public.gastos_gobiernos_locales_2022 t
UNION ALL SELECT 2023, t.* FROM public.gastos_gobiernos_locales_2023 t
UNION ALL SELECT 2024, t.* FROM public.gastos_gobiernos_locales_2024 t;
DROP VIEW IF EXISTS public.ingresos_gobiernos_locales CASCADE;
CREATE VIEW public.ingresos_gobiernos_locales AS
SELECT 2022 AS fuente_anio, t.* FROM public.ingresos_gobiernos_locales_2022 t
UNION ALL SELECT 2023, t.* FROM public.ingresos_gobiernos_locales_2023 t
UNION ALL SELECT 2024, t.* FROM public.ingresos_gobiernos_locales_2024 t;
CREATE INDEX IF NOT EXISTS gastos_locales_2022_territorio_idx ON public.gastos_gobiernos_locales_2022 (cod_region, cod_provincia, cod_municipio);
CREATE INDEX IF NOT EXISTS gastos_locales_2023_territorio_idx ON public.gastos_gobiernos_locales_2023 (cod_region, cod_provincia, cod_municipio);
CREATE INDEX IF NOT EXISTS gastos_locales_2024_territorio_idx ON public.gastos_gobiernos_locales_2024 (cod_region, cod_provincia, cod_municipio);
CREATE INDEX IF NOT EXISTS ingresos_locales_2022_territorio_idx ON public.ingresos_gobiernos_locales_2022 (cod_region, cod_provincia, cod_municipio);
CREATE INDEX IF NOT EXISTS ingresos_locales_2023_territorio_idx ON public.ingresos_gobiernos_locales_2023 (cod_region, cod_provincia, cod_municipio);
CREATE INDEX IF NOT EXISTS ingresos_locales_2024_territorio_idx ON public.ingresos_gobiernos_locales_2024 (cod_region, cod_provincia, cod_municipio);
CREATE INDEX IF NOT EXISTS clima_largo_territorio_idx ON public.atmosfera_clima_1991_2025 (idprovincia, ano, mes);
CREATE INDEX IF NOT EXISTS eventos_naturales_fecha_idx ON public.eventos_fenomenos_naturales (year, month, day);
DROP TABLE IF EXISTS meta.fuentes;
CREATE TABLE meta.fuentes (tabla text PRIMARY KEY, dataset text NOT NULL, archivo text NOT NULL, hoja text NOT NULL, filas bigint NOT NULL, csv_sha256 text NOT NULL, url text NOT NULL, cargado_en timestamptz NOT NULL DEFAULT now());
INSERT INTO meta.fuentes (tabla,dataset,archivo,hoja,filas,csv_sha256,url) VALUES ('atmosfera_clima_1991_2025','atmosfera_clima_1991_2025','atmosfera_clima_1991_2025.xlsx','1991-2025',18654,'643C3FED360961FF88AA0D6E3088777C7D4B4A756AF9CA94FA0D12A2823B5CDB','https://www.one.gob.do/media/0aybenwv/base-de-datos-atm%C3%B3sfera-y-clima-1991-2025.xlsx');
INSERT INTO meta.fuentes (tabla,dataset,archivo,hoja,filas,csv_sha256,url) VALUES ('atmosfera_clima_ca_2017_2023','atmosfera_clima_ca_2017_2023','atmosfera_clima_1991_2025.xlsx','CA_2017_2023',720,'1CE5896B45C435D5D4580CF3DCD6D673DC358A9065563498AF9AD88D622F1C08','https://www.one.gob.do/media/0aybenwv/base-de-datos-atm%C3%B3sfera-y-clima-1991-2025.xlsx');
INSERT INTO meta.fuentes (tabla,dataset,archivo,hoja,filas,csv_sha256,url) VALUES ('atmosfera_clima_ca_old','atmosfera_clima_ca_old','atmosfera_clima_1991_2025.xlsx','CA_old',870,'099C3975E8FA299E6C0ADBAED30DE3B1633FA260D902CBE961F112B2813DB10C','https://www.one.gob.do/media/0aybenwv/base-de-datos-atm%C3%B3sfera-y-clima-1991-2025.xlsx');
INSERT INTO meta.fuentes (tabla,dataset,archivo,hoja,filas,csv_sha256,url) VALUES ('eventos_fenomenos_naturales','eventos_fenomenos_naturales','eventos_fenomenos_naturales.xlsx','Base',1387,'785A7D88330C8B06A9CD5369082210B592D86F329D01C50BEBE3A1B96DE62833','https://www.one.gob.do/media/wsfa2mqb/base-de-datos-de-eventos-y-fenomenos-naturales.xlsx');
INSERT INTO meta.fuentes (tabla,dataset,archivo,hoja,filas,csv_sha256,url) VALUES ('gastos_gobiernos_locales_2022','gastos_gobiernos_locales_2022','gastos_gobiernos_locales_2022.xlsx','Gastos 2022',149296,'8105F30E5B4ECAA4E990DA0D8FFFC27989EEEE2F5586C25487F78D026A276E64','https://www.one.gob.do/media/3jwlzsfg/base-de-datos-de-gastos-de-los-gobienos-locales-2022.xlsx');
INSERT INTO meta.fuentes (tabla,dataset,archivo,hoja,filas,csv_sha256,url) VALUES ('gastos_gobiernos_locales_2023','gastos_gobiernos_locales_2023','gastos_gobiernos_locales_2023.xlsx','Gastos 2023',147525,'E4F3733C648BF77872351023A47DF2D46EE6815DAE90A8B41E442ECDC04CD43B','https://www.one.gob.do/media/of4a3wbc/base-de-datos-de-los-gobiernos-locales-2023.xlsx');
INSERT INTO meta.fuentes (tabla,dataset,archivo,hoja,filas,csv_sha256,url) VALUES ('gastos_gobiernos_locales_2024','gastos_gobiernos_locales_2024','gastos_gobiernos_locales_2024.xlsx','Gastos 2024',126309,'5B050C1866ED7FF99FFDB1A5D5121A80B47D5D62FE1E63D10F1E272F2EDE580F','https://www.one.gob.do/media/2iiomesv/base-de-datos-de-los-gobiernos-locales-2024.xlsx');
INSERT INTO meta.fuentes (tabla,dataset,archivo,hoja,filas,csv_sha256,url) VALUES ('ingresos_gobiernos_locales_2022','ingresos_gobiernos_locales_2022','ingresos_gobiernos_locales_2022.xlsx','Ingresos 2022',18785,'542FDAF34A8422ED6397B7C4528DE9611D68F20AC6099CB38F3E0AEAFFAB6D8E','https://www.one.gob.do/media/2fphze2s/base-de-datos-de-ingresos-de-los-gobiernos-locales-2022.xlsx');
INSERT INTO meta.fuentes (tabla,dataset,archivo,hoja,filas,csv_sha256,url) VALUES ('ingresos_gobiernos_locales_2023','ingresos_gobiernos_locales_2023','ingresos_gobiernos_locales_2023.xlsx','Ingresos 2023',19097,'43A7F4CDA9EAA0592A33EFF88B8C427DBC159EA80276B0D4B23E04C548906C4A','https://www.one.gob.do/media/ghypspwn/base-de-datos-ingresos-de-los-gobiernos-locales-2023.xlsx');
INSERT INTO meta.fuentes (tabla,dataset,archivo,hoja,filas,csv_sha256,url) VALUES ('ingresos_gobiernos_locales_2024','ingresos_gobiernos_locales_2024','ingresos_gobiernos_locales_2024.xlsx','Ingresos 2024',19241,'5C57A0A9356E427BEBA39785A8F6EDA668DE286EDBD479A7B85C304AFDD128D5','https://www.one.gob.do/media/h0wf25ut/base-de-datos-ingresos-de-los-gobiernos-locales-2024.xlsx');
DROP TABLE IF EXISTS meta.calidad;
CREATE TABLE meta.calidad (tabla text PRIMARY KEY, filas_fuente bigint NOT NULL, filas_cargadas bigint NOT NULL, estado text NOT NULL, medido_en timestamptz NOT NULL DEFAULT now());
INSERT INTO meta.calidad (tabla,filas_fuente,filas_cargadas,estado) VALUES ('atmosfera_clima_1991_2025',18654,(SELECT COUNT(*) FROM public.atmosfera_clima_1991_2025),CASE WHEN (SELECT COUNT(*) FROM public.atmosfera_clima_1991_2025)=18654 THEN 'OK' ELSE 'ERROR' END);
INSERT INTO meta.calidad (tabla,filas_fuente,filas_cargadas,estado) VALUES ('atmosfera_clima_ca_2017_2023',720,(SELECT COUNT(*) FROM public.atmosfera_clima_ca_2017_2023),CASE WHEN (SELECT COUNT(*) FROM public.atmosfera_clima_ca_2017_2023)=720 THEN 'OK' ELSE 'ERROR' END);
INSERT INTO meta.calidad (tabla,filas_fuente,filas_cargadas,estado) VALUES ('atmosfera_clima_ca_old',870,(SELECT COUNT(*) FROM public.atmosfera_clima_ca_old),CASE WHEN (SELECT COUNT(*) FROM public.atmosfera_clima_ca_old)=870 THEN 'OK' ELSE 'ERROR' END);
INSERT INTO meta.calidad (tabla,filas_fuente,filas_cargadas,estado) VALUES ('eventos_fenomenos_naturales',1387,(SELECT COUNT(*) FROM public.eventos_fenomenos_naturales),CASE WHEN (SELECT COUNT(*) FROM public.eventos_fenomenos_naturales)=1387 THEN 'OK' ELSE 'ERROR' END);
INSERT INTO meta.calidad (tabla,filas_fuente,filas_cargadas,estado) VALUES ('gastos_gobiernos_locales_2022',149296,(SELECT COUNT(*) FROM public.gastos_gobiernos_locales_2022),CASE WHEN (SELECT COUNT(*) FROM public.gastos_gobiernos_locales_2022)=149296 THEN 'OK' ELSE 'ERROR' END);
INSERT INTO meta.calidad (tabla,filas_fuente,filas_cargadas,estado) VALUES ('gastos_gobiernos_locales_2023',147525,(SELECT COUNT(*) FROM public.gastos_gobiernos_locales_2023),CASE WHEN (SELECT COUNT(*) FROM public.gastos_gobiernos_locales_2023)=147525 THEN 'OK' ELSE 'ERROR' END);
INSERT INTO meta.calidad (tabla,filas_fuente,filas_cargadas,estado) VALUES ('gastos_gobiernos_locales_2024',126309,(SELECT COUNT(*) FROM public.gastos_gobiernos_locales_2024),CASE WHEN (SELECT COUNT(*) FROM public.gastos_gobiernos_locales_2024)=126309 THEN 'OK' ELSE 'ERROR' END);
INSERT INTO meta.calidad (tabla,filas_fuente,filas_cargadas,estado) VALUES ('ingresos_gobiernos_locales_2022',18785,(SELECT COUNT(*) FROM public.ingresos_gobiernos_locales_2022),CASE WHEN (SELECT COUNT(*) FROM public.ingresos_gobiernos_locales_2022)=18785 THEN 'OK' ELSE 'ERROR' END);
INSERT INTO meta.calidad (tabla,filas_fuente,filas_cargadas,estado) VALUES ('ingresos_gobiernos_locales_2023',19097,(SELECT COUNT(*) FROM public.ingresos_gobiernos_locales_2023),CASE WHEN (SELECT COUNT(*) FROM public.ingresos_gobiernos_locales_2023)=19097 THEN 'OK' ELSE 'ERROR' END);
INSERT INTO meta.calidad (tabla,filas_fuente,filas_cargadas,estado) VALUES ('ingresos_gobiernos_locales_2024',19241,(SELECT COUNT(*) FROM public.ingresos_gobiernos_locales_2024),CASE WHEN (SELECT COUNT(*) FROM public.ingresos_gobiernos_locales_2024)=19241 THEN 'OK' ELSE 'ERROR' END);
DO $$ BEGIN IF EXISTS (SELECT 1 FROM meta.calidad WHERE estado <> 'OK') THEN RAISE EXCEPTION 'Conteo inesperado en una tabla ONE'; END IF; END $$;
GRANT CONNECT ON DATABASE one_datos TO analitica_lectura;
GRANT USAGE ON SCHEMA public, meta TO analitica_lectura;
GRANT SELECT ON ALL TABLES IN SCHEMA public, meta TO analitica_lectura;
GRANT SELECT ON ALL SEQUENCES IN SCHEMA public, meta TO analitica_lectura;