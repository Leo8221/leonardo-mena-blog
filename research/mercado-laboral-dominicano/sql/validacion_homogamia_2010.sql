\set ON_ERROR_STOP on
\set QUIET 1

SET statement_timeout = 0;
SET work_mem = '512MB';

-- Replica historica con categorias comparables entre 2010 y 2022.
-- Educacion se armoniza a cuatro niveles: preprimaria, primaria, secundaria
-- y superior (universitaria, maestria o doctorado).

CREATE TEMP TABLE tmp_base AS
SELECT
  provincia_code,
  jefatura_sexo_code::text || '-' || pareja_sexo_code::text AS composicion_sexo,
  COALESCE(jefatura_estado_conyugal_code::text, 'NA') || '-' ||
    COALESCE(pareja_estado_conyugal_code::text, 'NA') AS union_par,
  CASE
    WHEN jefatura_edad BETWEEN 16 AND 24 THEN '1'
    WHEN jefatura_edad BETWEEN 25 AND 34 THEN '2'
    WHEN jefatura_edad BETWEEN 35 AND 44 THEN '3'
    WHEN jefatura_edad BETWEEN 45 AND 54 THEN '4'
    WHEN jefatura_edad BETWEEN 55 AND 64 THEN '5'
    WHEN jefatura_edad BETWEEN 65 AND 74 THEN '6'
    ELSE '7'
  END AS j_edad,
  CASE
    WHEN pareja_edad BETWEEN 16 AND 24 THEN '1'
    WHEN pareja_edad BETWEEN 25 AND 34 THEN '2'
    WHEN pareja_edad BETWEEN 35 AND 44 THEN '3'
    WHEN pareja_edad BETWEEN 45 AND 54 THEN '4'
    WHEN pareja_edad BETWEEN 55 AND 64 THEN '5'
    WHEN pareja_edad BETWEEN 65 AND 74 THEN '6'
    ELSE '7'
  END AS c_edad,
  CASE WHEN jefatura_nivel_educativo_code BETWEEN 1 AND 4
    THEN jefatura_nivel_educativo_code::text END AS j_educacion,
  CASE WHEN pareja_nivel_educativo_code BETWEEN 1 AND 4
    THEN pareja_nivel_educativo_code::text END AS c_educacion
FROM analitica.parejas_jefatura_historica
WHERE edades_plausibles;

ANALYZE tmp_base;

CREATE TEMP TABLE tmp_validas (
  categoria_j text NOT NULL,
  categoria_c text NOT NULL,
  estrato text NOT NULL
);

CREATE TEMP TABLE tmp_resultados (
  indicador_id text,
  n bigint,
  observado_n bigint,
  esperado_nacional_n numeric,
  esperado_condicionado_n numeric,
  n_estratos integer,
  mediana_n_estrato numeric,
  pct_filas_estratos_menor_10 numeric
);

CREATE FUNCTION pg_temp.registrar_validacion(p_indicador_id text)
RETURNS void LANGUAGE sql AS $func$
  INSERT INTO tmp_resultados
  WITH total AS MATERIALIZED (
    SELECT COUNT(*)::numeric AS n,
      COUNT(*) FILTER (WHERE categoria_j = categoria_c)::numeric AS observado_n
    FROM tmp_validas
  ),
  mj_n AS MATERIALIZED (
    SELECT categoria_j AS categoria, COUNT(*)::numeric AS n_j
    FROM tmp_validas GROUP BY categoria_j
  ),
  mc_n AS MATERIALIZED (
    SELECT categoria_c AS categoria, COUNT(*)::numeric AS n_c
    FROM tmp_validas GROUP BY categoria_c
  ),
  esp_n AS MATERIALIZED (
    SELECT SUM(j.n_j * c.n_c / t.n)::numeric AS esperado_n
    FROM mj_n j JOIN mc_n c USING (categoria) CROSS JOIN total t
  ),
  estratos AS MATERIALIZED (
    SELECT estrato, COUNT(*)::numeric AS n
    FROM tmp_validas GROUP BY estrato
  ),
  mj AS MATERIALIZED (
    SELECT estrato, categoria_j AS categoria, COUNT(*)::numeric AS n_j
    FROM tmp_validas GROUP BY estrato, categoria_j
  ),
  mc AS MATERIALIZED (
    SELECT estrato, categoria_c AS categoria, COUNT(*)::numeric AS n_c
    FROM tmp_validas GROUP BY estrato, categoria_c
  ),
  esp_c AS MATERIALIZED (
    SELECT SUM(j.n_j * c.n_c / e.n)::numeric AS esperado_n
    FROM mj j JOIN mc c USING (estrato, categoria) JOIN estratos e USING (estrato)
  ),
  diag AS MATERIALIZED (
    SELECT COUNT(*)::integer AS n_estratos,
      PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY n)::numeric AS mediana_n,
      COALESCE(100.0 * SUM(n) FILTER (WHERE n < 10) / SUM(n), 0) AS pct_pequenos
    FROM estratos
  )
  SELECT p_indicador_id, t.n::bigint, t.observado_n::bigint,
    en.esperado_n, ec.esperado_n, d.n_estratos, d.mediana_n, d.pct_pequenos
  FROM total t CROSS JOIN esp_n en CROSS JOIN esp_c ec CROSS JOIN diag d;
$func$;

INSERT INTO tmp_validas
SELECT j_edad, c_edad,
  CONCAT_WS('|', provincia_code, composicion_sexo, union_par)
FROM tmp_base;
DO $do$ BEGIN PERFORM pg_temp.registrar_validacion('grupo_edad_10'); END $do$;

TRUNCATE tmp_validas;
INSERT INTO tmp_validas
SELECT j_educacion, c_educacion,
  CONCAT_WS('|', provincia_code, composicion_sexo, union_par, j_edad, c_edad)
FROM tmp_base
WHERE j_educacion IS NOT NULL AND c_educacion IS NOT NULL;
DO $do$ BEGIN PERFORM pg_temp.registrar_validacion('nivel_educativo_4'); END $do$;

\set QUIET 0

SELECT
  2010 AS anio,
  'Censo 2010'::text AS fuente,
  indicador_id,
  n,
  ROUND(100.0 * observado_n::numeric / n, 4) AS observado_pct,
  ROUND(100.0 * esperado_nacional_n / n, 4) AS esperado_nacional_pct,
  ROUND(100.0 * esperado_condicionado_n / n, 4) AS esperado_condicionado_pct,
  ROUND(
    (observado_n::numeric / n - esperado_nacional_n / n)
      / NULLIF(1 - esperado_nacional_n / n, 0), 4
  ) AS kappa_nacional,
  ROUND(
    (observado_n::numeric / n - esperado_condicionado_n / n)
      / NULLIF(1 - esperado_condicionado_n / n, 0), 4
  ) AS kappa_condicionado,
  ROUND(100.0 * n / b.parejas_base, 4) AS cobertura_pct,
  n_estratos,
  ROUND(mediana_n_estrato, 2) AS mediana_n_estrato,
  ROUND(pct_filas_estratos_menor_10, 4) AS pct_filas_estratos_menor_10,
  b.parejas_base
FROM tmp_resultados
CROSS JOIN (SELECT COUNT(*)::bigint AS parejas_base FROM tmp_base) b
ORDER BY indicador_id;
