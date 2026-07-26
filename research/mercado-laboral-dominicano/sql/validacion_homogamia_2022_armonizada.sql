\set ON_ERROR_STOP on

SET statement_timeout = 0;
SET work_mem = '512MB';

-- Nivel educativo de 2022 armonizado a las cuatro categorias disponibles en
-- el Censo 2010. Se conserva el mismo contrafactual condicionado principal.

WITH base AS MATERIALIZED (
  SELECT
    provincia,
    composicion_sexo,
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
    CASE
      WHEN jefatura_nivel_educativo_code BETWEEN 1 AND 3
        THEN jefatura_nivel_educativo_code::text
      WHEN jefatura_nivel_educativo_code BETWEEN 4 AND 6 THEN '4'
    END AS categoria_j,
    CASE
      WHEN pareja_nivel_educativo_code BETWEEN 1 AND 3
        THEN pareja_nivel_educativo_code::text
      WHEN pareja_nivel_educativo_code BETWEEN 4 AND 6 THEN '4'
    END AS categoria_c
  FROM analitica.parejas_jefatura_2022
  WHERE edades_plausibles
),
validas AS MATERIALIZED (
  SELECT *, CONCAT_WS('|', provincia, composicion_sexo, union_par, j_edad, c_edad) AS estrato
  FROM base WHERE categoria_j IS NOT NULL AND categoria_c IS NOT NULL
),
total AS MATERIALIZED (
  SELECT COUNT(*)::numeric AS n,
    COUNT(*) FILTER (WHERE categoria_j = categoria_c)::numeric AS observado_n
  FROM validas
),
mj_n AS MATERIALIZED (
  SELECT categoria_j AS categoria, COUNT(*)::numeric AS n_j
  FROM validas GROUP BY categoria_j
),
mc_n AS MATERIALIZED (
  SELECT categoria_c AS categoria, COUNT(*)::numeric AS n_c
  FROM validas GROUP BY categoria_c
),
esp_n AS MATERIALIZED (
  SELECT SUM(j.n_j * c.n_c / t.n)::numeric AS esperado_n
  FROM mj_n j JOIN mc_n c USING (categoria) CROSS JOIN total t
),
estratos AS MATERIALIZED (
  SELECT estrato, COUNT(*)::numeric AS n FROM validas GROUP BY estrato
),
mj AS MATERIALIZED (
  SELECT estrato, categoria_j AS categoria, COUNT(*)::numeric AS n_j
  FROM validas GROUP BY estrato, categoria_j
),
mc AS MATERIALIZED (
  SELECT estrato, categoria_c AS categoria, COUNT(*)::numeric AS n_c
  FROM validas GROUP BY estrato, categoria_c
),
esp_c AS MATERIALIZED (
  SELECT SUM(j.n_j * c.n_c / e.n)::numeric AS esperado_n
  FROM mj j JOIN mc c USING (estrato, categoria) JOIN estratos e USING (estrato)
),
diag AS MATERIALIZED (
  SELECT COUNT(*)::integer AS n_estratos,
    PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY n)::numeric AS mediana_n,
    100.0 * SUM(n) FILTER (WHERE n < 10) / SUM(n) AS pct_pequenos
  FROM estratos
)
SELECT
  2022 AS anio,
  'Censo 2022 armonizado'::text AS fuente,
  'nivel_educativo_4'::text AS indicador_id,
  t.n::bigint AS n,
  ROUND(100.0 * t.observado_n / t.n, 4) AS observado_pct,
  ROUND(100.0 * en.esperado_n / t.n, 4) AS esperado_nacional_pct,
  ROUND(100.0 * ec.esperado_n / t.n, 4) AS esperado_condicionado_pct,
  ROUND((t.observado_n / t.n - en.esperado_n / t.n)
    / NULLIF(1 - en.esperado_n / t.n, 0), 4) AS kappa_nacional,
  ROUND((t.observado_n / t.n - ec.esperado_n / t.n)
    / NULLIF(1 - ec.esperado_n / t.n, 0), 4) AS kappa_condicionado,
  ROUND(100.0 * t.n / b.parejas_base, 4) AS cobertura_pct,
  d.n_estratos,
  ROUND(d.mediana_n, 2) AS mediana_n_estrato,
  ROUND(d.pct_pequenos, 4) AS pct_filas_estratos_menor_10,
  b.parejas_base
FROM total t CROSS JOIN esp_n en CROSS JOIN esp_c ec CROSS JOIN diag d
CROSS JOIN (SELECT COUNT(*)::numeric AS parejas_base FROM base) b;
