\set ON_ERROR_STOP on
\set QUIET 1

SET statement_timeout = 0;
SET work_mem = '512MB';
SET temp_buffers = '256MB';
SET synchronous_commit = off;

-- Homogamia observada frente a dos contrafactuales de emparejamiento.
-- Universo: parejas jefatura-conyuge/companero con edades plausibles.
--
-- Nacional: independencia usando solamente los margenes nacionales.
-- Principal: independencia dentro de provincia, composicion por sexo y tipo
-- de union. Salvo cuando la edad es el resultado, tambien preserva las bandas
-- de edad de ambos miembros. Procesar cada indicador por separado evita una
-- tabla intermedia de mas de diez millones de filas.

CREATE TEMP TABLE tmp_hom_base AS
SELECT
  p.region,
  p.provincia,
  p.composicion_sexo,
  COALESCE(p.jefatura_estado_conyugal_code::text, 'NA') || '-' ||
    COALESCE(p.pareja_estado_conyugal_code::text, 'NA') AS union_par,
  CASE
    WHEN p.jefatura_edad BETWEEN 16 AND 24 THEN '16-24'
    WHEN p.jefatura_edad BETWEEN 25 AND 34 THEN '25-34'
    WHEN p.jefatura_edad BETWEEN 35 AND 44 THEN '35-44'
    WHEN p.jefatura_edad BETWEEN 45 AND 54 THEN '45-54'
    WHEN p.jefatura_edad BETWEEN 55 AND 64 THEN '55-64'
    WHEN p.jefatura_edad BETWEEN 65 AND 74 THEN '65-74'
    ELSE '75+'
  END AS j_edad_10,
  CASE
    WHEN p.pareja_edad BETWEEN 16 AND 24 THEN '16-24'
    WHEN p.pareja_edad BETWEEN 25 AND 34 THEN '25-34'
    WHEN p.pareja_edad BETWEEN 35 AND 44 THEN '35-44'
    WHEN p.pareja_edad BETWEEN 45 AND 54 THEN '45-54'
    WHEN p.pareja_edad BETWEEN 55 AND 64 THEN '55-64'
    WHEN p.pareja_edad BETWEEN 65 AND 74 THEN '65-74'
    ELSE '75+'
  END AS c_edad_10,
  CASE WHEN p.jefatura_nivel_educativo_code BETWEEN 1 AND 6
    THEN p.jefatura_nivel_educativo_code::text END AS j_educacion,
  CASE WHEN p.pareja_nivel_educativo_code BETWEEN 1 AND 6
    THEN p.pareja_nivel_educativo_code::text END AS c_educacion,
  p.jefatura_campo_estudio_amplio::text AS j_campo,
  p.pareja_campo_estudio_amplio::text AS c_campo,
  CASE WHEN p.jefatura_categoria_ocupacional_code BETWEEN 1 AND 5
    THEN p.jefatura_categoria_ocupacional_code::text END AS j_categoria_ocupacional,
  CASE WHEN p.pareja_categoria_ocupacional_code BETWEEN 1 AND 5
    THEN p.pareja_categoria_ocupacional_code::text END AS c_categoria_ocupacional,
  CASE WHEN p.jefatura_autoidentificacion_code BETWEEN 1 AND 8
    THEN p.jefatura_autoidentificacion_code::text END AS j_autoidentificacion,
  CASE WHEN p.pareja_autoidentificacion_code BETWEEN 1 AND 8
    THEN p.pareja_autoidentificacion_code::text END AS c_autoidentificacion,
  CASE
    WHEN j.p40_1 IN (2, 3, 4) OR j.p40_2 IN (2, 3, 4)
      OR j.p40_3 IN (2, 3, 4) OR j.p40_4 IN (2, 3, 4)
      OR j.p40_5 IN (2, 3, 4) OR j.p40_6 IN (2, 3, 4) THEN '1'
    WHEN j.p40_1 = 1 AND j.p40_2 = 1 AND j.p40_3 = 1
      AND j.p40_4 = 1 AND j.p40_5 = 1 AND j.p40_6 = 1 THEN '0'
  END AS j_dificultad,
  CASE
    WHEN c.p40_1 IN (2, 3, 4) OR c.p40_2 IN (2, 3, 4)
      OR c.p40_3 IN (2, 3, 4) OR c.p40_4 IN (2, 3, 4)
      OR c.p40_5 IN (2, 3, 4) OR c.p40_6 IN (2, 3, 4) THEN '1'
    WHEN c.p40_1 = 1 AND c.p40_2 = 1 AND c.p40_3 = 1
      AND c.p40_4 = 1 AND c.p40_5 = 1 AND c.p40_6 = 1 THEN '0'
  END AS c_dificultad
FROM analitica.parejas_jefatura_2022 p
JOIN analitica.xcnpv_unificada j
  ON j.hogar_id = p.hogar_id AND j.p28_parent = 1
JOIN analitica.xcnpv_unificada c
  ON c.hogar_id = p.hogar_id AND c.p28_parent = 2
WHERE p.edades_plausibles;

ANALYZE tmp_hom_base;

CREATE TEMP TABLE tmp_validas (
  categoria_j text NOT NULL,
  categoria_c text NOT NULL,
  estrato_grueso text NOT NULL,
  estrato text NOT NULL
);

CREATE TEMP TABLE tmp_resultados (
  orden integer,
  indicador_id text,
  indicador text,
  familia text,
  n bigint,
  observado_n bigint,
  esperado_nacional_n numeric,
  esperado_grueso_n numeric,
  esperado_principal_n numeric,
  categorias_jefatura integer,
  categorias_pareja integer,
  n_estratos_principal integer,
  mediana_n_estrato_principal numeric,
  pct_filas_estratos_menor_10_principal numeric
);

CREATE FUNCTION pg_temp.registrar_homogamia(
  p_orden integer,
  p_indicador_id text,
  p_indicador text,
  p_familia text
) RETURNS void
LANGUAGE sql
AS $func$
  INSERT INTO tmp_resultados
  WITH total AS MATERIALIZED (
    SELECT
      COUNT(*)::numeric AS n,
      COUNT(*) FILTER (WHERE categoria_j = categoria_c)::numeric AS observado_n
    FROM tmp_validas
  ),
  mj_nacional AS MATERIALIZED (
    SELECT categoria_j AS categoria, COUNT(*)::numeric AS n_j
    FROM tmp_validas GROUP BY categoria_j
  ),
  mc_nacional AS MATERIALIZED (
    SELECT categoria_c AS categoria, COUNT(*)::numeric AS n_c
    FROM tmp_validas GROUP BY categoria_c
  ),
  esperado_nacional AS MATERIALIZED (
    SELECT SUM(j.n_j * c.n_c / t.n)::numeric AS esperado_n
    FROM mj_nacional j
    JOIN mc_nacional c USING (categoria)
    CROSS JOIN total t
  ),
  estratos_gruesos AS MATERIALIZED (
    SELECT estrato_grueso, COUNT(*)::numeric AS n
    FROM tmp_validas
    GROUP BY estrato_grueso
  ),
  mj_grueso AS MATERIALIZED (
    SELECT estrato_grueso, categoria_j AS categoria, COUNT(*)::numeric AS n_j
    FROM tmp_validas
    GROUP BY estrato_grueso, categoria_j
  ),
  mc_grueso AS MATERIALIZED (
    SELECT estrato_grueso, categoria_c AS categoria, COUNT(*)::numeric AS n_c
    FROM tmp_validas
    GROUP BY estrato_grueso, categoria_c
  ),
  esperado_grueso AS MATERIALIZED (
    SELECT SUM(j.n_j * c.n_c / e.n)::numeric AS esperado_n
    FROM mj_grueso j
    JOIN mc_grueso c USING (estrato_grueso, categoria)
    JOIN estratos_gruesos e USING (estrato_grueso)
  ),
  estratos AS MATERIALIZED (
    SELECT
      estrato,
      COUNT(*)::numeric AS n,
      COUNT(*) FILTER (WHERE categoria_j = categoria_c)::numeric AS observado_n
    FROM tmp_validas
    GROUP BY estrato
  ),
  mj AS MATERIALIZED (
    SELECT estrato, categoria_j AS categoria, COUNT(*)::numeric AS n_j
    FROM tmp_validas
    GROUP BY estrato, categoria_j
  ),
  mc AS MATERIALIZED (
    SELECT estrato, categoria_c AS categoria, COUNT(*)::numeric AS n_c
    FROM tmp_validas
    GROUP BY estrato, categoria_c
  ),
  esperado_principal AS MATERIALIZED (
    SELECT SUM(j.n_j * c.n_c / e.n)::numeric AS esperado_n
    FROM mj j
    JOIN mc c USING (estrato, categoria)
    JOIN estratos e USING (estrato)
  ),
  diagnostico AS MATERIALIZED (
    SELECT
      COUNT(*)::integer AS n_estratos,
      PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY n)::numeric AS mediana_n,
      100.0 * SUM(n) FILTER (WHERE n < 10) / SUM(n) AS pct_en_pequenos
    FROM estratos
  ),
  categorias AS MATERIALIZED (
    SELECT
      COUNT(DISTINCT categoria_j)::integer AS n_j,
      COUNT(DISTINCT categoria_c)::integer AS n_c
    FROM tmp_validas
  )
  SELECT
    p_orden,
    p_indicador_id,
    p_indicador,
    p_familia,
    t.n::bigint,
    t.observado_n::bigint,
    en.esperado_n,
    eg.esperado_n,
    ep.esperado_n,
    cat.n_j,
    cat.n_c,
    d.n_estratos,
    d.mediana_n,
    d.pct_en_pequenos
  FROM total t
  CROSS JOIN esperado_nacional en
  CROSS JOIN esperado_grueso eg
  CROSS JOIN esperado_principal ep
  CROSS JOIN diagnostico d
  CROSS JOIN categorias cat;
$func$;

-- La edad es el resultado: no se permite que la edad defina sus propios estratos.
TRUNCATE tmp_validas;
INSERT INTO tmp_validas
SELECT
  j_edad_10,
  c_edad_10,
  CONCAT_WS('|', region, composicion_sexo, union_par),
  CONCAT_WS('|', provincia, composicion_sexo, union_par)
FROM tmp_hom_base;
DO $do$ BEGIN
  PERFORM pg_temp.registrar_homogamia(1, 'grupo_edad_10', 'Grupo de edad', 'Demografia');
END $do$;

TRUNCATE tmp_validas;
INSERT INTO tmp_validas
SELECT
  j_educacion,
  c_educacion,
  CONCAT_WS('|', region, composicion_sexo, union_par, j_edad_10, c_edad_10),
  CONCAT_WS('|', provincia, composicion_sexo, union_par, j_edad_10, c_edad_10)
FROM tmp_hom_base
WHERE j_educacion IS NOT NULL AND c_educacion IS NOT NULL;
DO $do$ BEGIN
  PERFORM pg_temp.registrar_homogamia(4, 'nivel_educativo', 'Nivel educativo', 'Educacion');
END $do$;

TRUNCATE tmp_validas;
INSERT INTO tmp_validas
SELECT
  j_campo,
  c_campo,
  CONCAT_WS('|', region, composicion_sexo, union_par, j_edad_10, c_edad_10),
  CONCAT_WS('|', provincia, composicion_sexo, union_par, j_edad_10, c_edad_10)
FROM tmp_hom_base
WHERE j_campo IS NOT NULL AND c_campo IS NOT NULL;
DO $do$ BEGIN
  PERFORM pg_temp.registrar_homogamia(6, 'campo_estudio', 'Campo amplio de estudio', 'Educacion');
END $do$;

TRUNCATE tmp_validas;
INSERT INTO tmp_validas
SELECT
  j_dificultad,
  c_dificultad,
  CONCAT_WS('|', region, composicion_sexo, union_par, j_edad_10, c_edad_10),
  CONCAT_WS('|', provincia, composicion_sexo, union_par, j_edad_10, c_edad_10)
FROM tmp_hom_base
WHERE j_dificultad IS NOT NULL AND c_dificultad IS NOT NULL;
DO $do$ BEGIN
  PERFORM pg_temp.registrar_homogamia(9, 'dificultad_funcional', 'Alguna dificultad funcional', 'Salud');
END $do$;

TRUNCATE tmp_validas;
INSERT INTO tmp_validas
SELECT
  j_categoria_ocupacional,
  c_categoria_ocupacional,
  CONCAT_WS('|', region, composicion_sexo, union_par, j_edad_10, c_edad_10),
  CONCAT_WS('|', provincia, composicion_sexo, union_par, j_edad_10, c_edad_10)
FROM tmp_hom_base
WHERE j_categoria_ocupacional IS NOT NULL AND c_categoria_ocupacional IS NOT NULL;
DO $do$ BEGIN
  PERFORM pg_temp.registrar_homogamia(21, 'categoria_ocupacional', 'Categoria ocupacional', 'Trabajo');
END $do$;

TRUNCATE tmp_validas;
INSERT INTO tmp_validas
SELECT
  j_autoidentificacion,
  c_autoidentificacion,
  CONCAT_WS('|', region, composicion_sexo, union_par, j_edad_10, c_edad_10),
  CONCAT_WS('|', provincia, composicion_sexo, union_par, j_edad_10, c_edad_10)
FROM tmp_hom_base
WHERE j_autoidentificacion IS NOT NULL AND c_autoidentificacion IS NOT NULL;
DO $do$ BEGIN
  PERFORM pg_temp.registrar_homogamia(22, 'autoidentificacion', 'Autoidentificacion etnorracial', 'Identidad');
END $do$;

\set QUIET 0

SELECT
  r.orden,
  r.indicador_id,
  r.indicador,
  r.familia,
  r.n,
  r.observado_n,
  ROUND(100.0 * r.observado_n / r.n, 4) AS observado_pct,
  ROUND(100.0 * r.esperado_nacional_n / r.n, 4) AS esperado_nacional_pct,
  ROUND(100.0 * r.esperado_grueso_n / r.n, 4) AS esperado_region_pct,
  ROUND(100.0 * r.esperado_principal_n / r.n, 4) AS esperado_principal_pct,
  ROUND(
    (r.observado_n::numeric / r.n - r.esperado_nacional_n / r.n)
      / NULLIF(1 - r.esperado_nacional_n / r.n, 0), 4
  ) AS kappa_nacional,
  ROUND(
    (r.observado_n::numeric / r.n - r.esperado_grueso_n / r.n)
      / NULLIF(1 - r.esperado_grueso_n / r.n, 0), 4
  ) AS kappa_region,
  ROUND(
    (r.observado_n::numeric / r.n - r.esperado_principal_n / r.n)
      / NULLIF(1 - r.esperado_principal_n / r.n, 0), 4
  ) AS kappa_principal,
  ROUND(100.0 * r.n / b.parejas_base, 4) AS cobertura_pct,
  r.categorias_jefatura,
  r.categorias_pareja,
  r.n_estratos_principal,
  ROUND(r.mediana_n_estrato_principal, 2) AS mediana_n_estrato_principal,
  ROUND(r.pct_filas_estratos_menor_10_principal, 4)
    AS pct_filas_estratos_menor_10_principal,
  b.parejas_base,
  q.errores_qa,
  q.advertencias_qa
FROM tmp_resultados r
CROSS JOIN (SELECT COUNT(*)::bigint AS parejas_base FROM tmp_hom_base) b
CROSS JOIN (
  SELECT
    COUNT(*) FILTER (WHERE estado = 'error') AS errores_qa,
    COUNT(*) FILTER (WHERE estado = 'advertencia') AS advertencias_qa
  FROM meta.controles_calidad_analitica
) q
ORDER BY kappa_principal DESC, cobertura_pct DESC, r.orden;
