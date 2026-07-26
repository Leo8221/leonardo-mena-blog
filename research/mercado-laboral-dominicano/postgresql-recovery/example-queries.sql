-- Base central:
--   censos_linea_tiempo
--
-- Vistas principales:
--   armonizado.personas
--   armonizado.hogares
--   armonizado.viviendas
--   armonizado.metadatos_variables
--
-- Nota de rendimiento:
-- Para agregaciones pesadas, filtra por anio o consulta directamente
-- censo_2002 / censo_2010 / censo_2022. La base central usa postgres_fdw.

-- 1) Poblacion por sexo y anio
SELECT
  anio,
  sexo_lbl,
  COUNT(*) AS personas
FROM armonizado.personas
GROUP BY anio, sexo_lbl
ORDER BY anio, sexo_lbl;

-- 2) Envejecimiento: poblacion 65+ por anio
SELECT
  anio,
  COUNT(*) AS personas,
  COUNT(*) FILTER (WHERE edad >= 65) AS personas_65_mas,
  ROUND(100.0 * COUNT(*) FILTER (WHERE edad >= 65) / COUNT(*), 2) AS pct_65_mas
FROM armonizado.personas
GROUP BY anio
ORDER BY anio;

-- 3) Educacion superior por sexo y anio
SELECT
  anio,
  sexo_lbl,
  COUNT(*) FILTER (WHERE educ_superior_bool IS NOT NULL) AS personas_validas,
  ROUND(
    100.0 * COUNT(*) FILTER (WHERE educ_superior_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE educ_superior_bool IS NOT NULL), 0),
    2
  ) AS pct_educ_superior
FROM armonizado.personas
GROUP BY anio, sexo_lbl
ORDER BY anio, sexo_lbl;

-- 4) Alfabetismo por anio
SELECT
  anio,
  COUNT(*) FILTER (WHERE sabe_leer_bool IS NOT NULL) AS personas_validas,
  ROUND(
    100.0 * COUNT(*) FILTER (WHERE sabe_leer_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE sabe_leer_bool IS NOT NULL), 0),
    2
  ) AS pct_sabe_leer
FROM armonizado.personas
GROUP BY anio
ORDER BY anio;

-- 5) Hogares con internet, computadora, celular y automovil
SELECT
  anio,
  COUNT(*) AS hogares,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_internet IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_internet IS NOT NULL), 0), 2) AS pct_internet,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_computadora IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_computadora IS NOT NULL), 0), 2) AS pct_computadora,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_celular IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_celular IS NOT NULL), 0), 2) AS pct_celular,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_automovil IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_automovil IS NOT NULL), 0), 2) AS pct_automovil
FROM armonizado.hogares
GROUP BY anio
ORDER BY anio;

-- 6) Materiales de vivienda por provincia y anio
SELECT
  anio,
  provincia_code,
  paredes_code,
  COUNT(*) AS viviendas
FROM armonizado.viviendas
WHERE paredes_code IS NOT NULL
GROUP BY anio, provincia_code, paredes_code
ORDER BY anio, provincia_code, viviendas DESC;

-- 7) Revisar comparabilidad documentada
SELECT *
FROM armonizado.metadatos_variables
ORDER BY anio, tabla, variable;

