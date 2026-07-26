\set ON_ERROR_STOP on

WITH base AS (
  SELECT *
  FROM analitica.parejas_jefatura_2022
  WHERE edades_plausibles
),
qa AS (
  SELECT COUNT(*) FILTER (WHERE estado = 'error')::integer AS errores_qa
  FROM meta.controles_calidad_analitica
),
metricas AS (
  SELECT
    1::integer AS orden,
    'situacion_ocupacional'::text AS indicador_id,
    COUNT(*) FILTER (WHERE jefatura_ocupado = pareja_ocupado)::bigint AS iguales,
    COUNT(*) FILTER (
      WHERE jefatura_ocupado IS NOT NULL
        AND pareja_ocupado IS NOT NULL
    )::bigint AS denominador
  FROM base

  UNION ALL

  SELECT
    2,
    'nivel_educativo',
    COUNT(*) FILTER (WHERE mismo_nivel_educativo IS TRUE),
    COUNT(*) FILTER (WHERE mismo_nivel_educativo IS NOT NULL)
  FROM base

  UNION ALL

  SELECT
    3,
    'campo_estudio',
    COUNT(*) FILTER (WHERE mismo_campo_estudio_amplio IS TRUE),
    COUNT(*) FILTER (WHERE mismo_campo_estudio_amplio IS NOT NULL)
  FROM base

  UNION ALL

  SELECT
    4,
    'grupo_ocupacional',
    COUNT(*) FILTER (WHERE mismo_gran_grupo_ocupacional IS TRUE),
    COUNT(*) FILTER (WHERE mismo_gran_grupo_ocupacional IS NOT NULL)
  FROM base
)
SELECT
  m.orden,
  m.indicador_id,
  m.iguales,
  m.denominador,
  ROUND(100.0 * m.iguales / NULLIF(m.denominador, 0), 2) AS porcentaje,
  ROUND(100.0 * m.denominador / NULLIF((SELECT COUNT(*) FROM base), 0), 2) AS cobertura,
  (SELECT COUNT(*) FROM base)::bigint AS parejas_base,
  (SELECT errores_qa FROM qa) AS errores_qa
FROM metricas m
ORDER BY m.orden;
