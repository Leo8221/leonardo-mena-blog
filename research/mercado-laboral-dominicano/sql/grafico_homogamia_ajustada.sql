\set ON_ERROR_STOP on

SET statement_timeout = 0;
SET work_mem = '256MB';

-- Homogamia observada y esperada bajo independencia.
-- Universo: parejas jefatura-conyuge/companero con edades plausibles.
-- Se excluyen atributos compartidos por hogar y estado conyugal.

WITH base_parejas AS MATERIALIZED (
  SELECT
    hogar_id,
    jefatura_edad,
    pareja_edad,
    jefatura_nivel_educativo,
    pareja_nivel_educativo,
    jefatura_campo_estudio_amplio,
    pareja_campo_estudio_amplio,
    jefatura_ocupado,
    pareja_ocupado,
    jefatura_ocupacion_gran_grupo,
    pareja_ocupacion_gran_grupo,
    jefatura_categoria_ocupacional_code,
    pareja_categoria_ocupacional_code,
    jefatura_autoidentificacion_code,
    pareja_autoidentificacion_code
  FROM analitica.parejas_jefatura_2022
  WHERE edades_plausibles
),
atributos_raw AS MATERIALIZED (
  SELECT
    p.hogar_id,
    j.p41 AS j_p41,
    c.p41 AS c_p41,
    j.p42 AS j_p42,
    c.p42 AS c_p42,
    j.p40_1 AS j_p40_1,
    j.p40_2 AS j_p40_2,
    j.p40_3 AS j_p40_3,
    j.p40_4 AS j_p40_4,
    j.p40_5 AS j_p40_5,
    j.p40_6 AS j_p40_6,
    c.p40_1 AS c_p40_1,
    c.p40_2 AS c_p40_2,
    c.p40_3 AS c_p40_3,
    c.p40_4 AS c_p40_4,
    c.p40_5 AS c_p40_5,
    c.p40_6 AS c_p40_6,
    j.p48d AS j_p48d,
    c.p48d AS c_p48d,
    j.p49 AS j_p49,
    c.p49 AS c_p49
  FROM base_parejas p
  JOIN analitica.xcnpv_unificada j
    ON j.hogar_id = p.hogar_id
   AND j.p28_parent = 1
  JOIN analitica.xcnpv_unificada c
    ON c.hogar_id = p.hogar_id
   AND c.p28_parent = 2
),
categorias AS (
  SELECT
    v.orden,
    v.indicador_id,
    v.indicador,
    v.familia,
    v.categoria_jefatura,
    v.categoria_pareja
  FROM base_parejas b
  CROSS JOIN LATERAL (
    VALUES
      (
        1,
        'grupo_edad_10',
        'Grupo de edad',
        'Demografia',
        CASE
          WHEN b.jefatura_edad BETWEEN 16 AND 24 THEN '16-24'
          WHEN b.jefatura_edad BETWEEN 25 AND 34 THEN '25-34'
          WHEN b.jefatura_edad BETWEEN 35 AND 44 THEN '35-44'
          WHEN b.jefatura_edad BETWEEN 45 AND 54 THEN '45-54'
          WHEN b.jefatura_edad BETWEEN 55 AND 64 THEN '55-64'
          WHEN b.jefatura_edad BETWEEN 65 AND 74 THEN '65-74'
          WHEN b.jefatura_edad >= 75 THEN '75+'
        END,
        CASE
          WHEN b.pareja_edad BETWEEN 16 AND 24 THEN '16-24'
          WHEN b.pareja_edad BETWEEN 25 AND 34 THEN '25-34'
          WHEN b.pareja_edad BETWEEN 35 AND 44 THEN '35-44'
          WHEN b.pareja_edad BETWEEN 45 AND 54 THEN '45-54'
          WHEN b.pareja_edad BETWEEN 55 AND 64 THEN '55-64'
          WHEN b.pareja_edad BETWEEN 65 AND 74 THEN '65-74'
          WHEN b.pareja_edad >= 75 THEN '75+'
        END
      ),
      (
        4, 'nivel_educativo', 'Nivel educativo', 'Educacion',
        b.jefatura_nivel_educativo::text,
        b.pareja_nivel_educativo::text
      ),
      (
        6, 'campo_estudio', 'Campo amplio de estudio', 'Educacion',
        b.jefatura_campo_estudio_amplio::text,
        b.pareja_campo_estudio_amplio::text
      ),
      (
        15, 'situacion_ocupacional', 'Condicion de ocupacion', 'Trabajo',
        CASE WHEN b.jefatura_ocupado IS TRUE THEN '1' WHEN b.jefatura_ocupado IS FALSE THEN '0' END,
        CASE WHEN b.pareja_ocupado IS TRUE THEN '1' WHEN b.pareja_ocupado IS FALSE THEN '0' END
      ),
      (
        20, 'grupo_ocupacional', 'Gran grupo ocupacional', 'Trabajo',
        b.jefatura_ocupacion_gran_grupo::text,
        b.pareja_ocupacion_gran_grupo::text
      ),
      (
        21, 'categoria_ocupacional', 'Categoria ocupacional', 'Trabajo',
        CASE WHEN b.jefatura_categoria_ocupacional_code BETWEEN 1 AND 5
          THEN b.jefatura_categoria_ocupacional_code::text END,
        CASE WHEN b.pareja_categoria_ocupacional_code BETWEEN 1 AND 5
          THEN b.pareja_categoria_ocupacional_code::text END
      ),
      (
        22, 'autoidentificacion', 'Autoidentificacion etnorracial', 'Identidad',
        CASE WHEN b.jefatura_autoidentificacion_code BETWEEN 1 AND 8
          THEN b.jefatura_autoidentificacion_code::text END,
        CASE WHEN b.pareja_autoidentificacion_code BETWEEN 1 AND 8
          THEN b.pareja_autoidentificacion_code::text END
      )
  ) AS v(
    orden,
    indicador_id,
    indicador,
    familia,
    categoria_jefatura,
    categoria_pareja
  )

  UNION ALL

  SELECT
    v.orden,
    v.indicador_id,
    v.indicador,
    v.familia,
    v.categoria_jefatura,
    v.categoria_pareja
  FROM atributos_raw b
  CROSS JOIN LATERAL (
    VALUES
      (
        2, 'alfabetismo', 'Sabe leer y escribir', 'Educacion',
        CASE WHEN b.j_p41 IN (1, 2) THEN b.j_p41::text END,
        CASE WHEN b.c_p41 IN (1, 2) THEN b.c_p41::text END
      ),
      (
        3, 'trayectoria_escolar', 'Asistencia o trayectoria escolar', 'Educacion',
        CASE WHEN b.j_p42 IN (1, 2, 3) THEN b.j_p42::text END,
        CASE WHEN b.c_p42 IN (1, 2, 3) THEN b.c_p42::text END
      ),
      (
        9, 'dificultad_funcional', 'Alguna dificultad funcional', 'Salud',
        CASE
          WHEN b.j_p40_1 IN (2, 3, 4) OR b.j_p40_2 IN (2, 3, 4)
            OR b.j_p40_3 IN (2, 3, 4) OR b.j_p40_4 IN (2, 3, 4)
            OR b.j_p40_5 IN (2, 3, 4) OR b.j_p40_6 IN (2, 3, 4)
          THEN '1'
          WHEN b.j_p40_1 = 1 AND b.j_p40_2 = 1 AND b.j_p40_3 = 1
            AND b.j_p40_4 = 1 AND b.j_p40_5 = 1 AND b.j_p40_6 = 1
          THEN '0'
        END,
        CASE
          WHEN b.c_p40_1 IN (2, 3, 4) OR b.c_p40_2 IN (2, 3, 4)
            OR b.c_p40_3 IN (2, 3, 4) OR b.c_p40_4 IN (2, 3, 4)
            OR b.c_p40_5 IN (2, 3, 4) OR b.c_p40_6 IN (2, 3, 4)
          THEN '1'
          WHEN b.c_p40_1 = 1 AND b.c_p40_2 = 1 AND b.c_p40_3 = 1
            AND b.c_p40_4 = 1 AND b.c_p40_5 = 1 AND b.c_p40_6 = 1
          THEN '0'
        END
      ),
      (
        13, 'uso_smartphone', 'Uso de smartphone', 'Uso digital',
        CASE WHEN b.j_p48d IN (1, 2) THEN b.j_p48d::text END,
        CASE WHEN b.c_p48d IN (1, 2) THEN b.c_p48d::text END
      ),
      (
        14, 'uso_internet', 'Uso de internet', 'Uso digital',
        CASE WHEN b.j_p49 IN (1, 2) THEN b.j_p49::text END,
        CASE WHEN b.c_p49 IN (1, 2) THEN b.c_p49::text END
      )
  ) AS v(
    orden,
    indicador_id,
    indicador,
    familia,
    categoria_jefatura,
    categoria_pareja
  )
),
contingencia AS MATERIALIZED (
  SELECT
    orden,
    indicador_id,
    indicador,
    familia,
    categoria_jefatura,
    categoria_pareja,
    COUNT(*)::bigint AS n_celda
  FROM categorias
  WHERE categoria_jefatura IS NOT NULL
    AND categoria_pareja IS NOT NULL
  GROUP BY
    orden,
    indicador_id,
    indicador,
    familia,
    categoria_jefatura,
    categoria_pareja
),
totales AS (
  SELECT
    orden,
    indicador_id,
    indicador,
    familia,
    SUM(n_celda)::bigint AS n,
    SUM(n_celda) FILTER (WHERE categoria_jefatura = categoria_pareja)::bigint AS observado_n,
    COUNT(DISTINCT categoria_jefatura)::integer AS categorias_jefatura,
    COUNT(DISTINCT categoria_pareja)::integer AS categorias_pareja
  FROM contingencia
  GROUP BY orden, indicador_id, indicador, familia
),
marginal_jefatura AS (
  SELECT indicador_id, categoria_jefatura AS categoria, SUM(n_celda)::numeric AS n_jefatura
  FROM contingencia
  GROUP BY indicador_id, categoria_jefatura
),
marginal_pareja AS (
  SELECT indicador_id, categoria_pareja AS categoria, SUM(n_celda)::numeric AS n_pareja
  FROM contingencia
  GROUP BY indicador_id, categoria_pareja
),
marginales AS (
  SELECT
    COALESCE(j.indicador_id, c.indicador_id) AS indicador_id,
    COALESCE(j.categoria, c.categoria) AS categoria,
    COALESCE(j.n_jefatura, 0) AS n_jefatura,
    COALESCE(c.n_pareja, 0) AS n_pareja
  FROM marginal_jefatura j
  FULL JOIN marginal_pareja c
    ON c.indicador_id = j.indicador_id
   AND c.categoria = j.categoria
),
esperados AS (
  SELECT
    m.indicador_id,
    SUM((m.n_jefatura / t.n) * (m.n_pareja / t.n)) AS esperado_p,
    0.5 * SUM(ABS(m.n_jefatura / t.n - m.n_pareja / t.n)) AS duncan
  FROM marginales m
  JOIN totales t USING (indicador_id)
  GROUP BY m.indicador_id
),
calculos AS (
  SELECT
    t.*,
    e.esperado_p,
    e.duncan,
    t.observado_n::numeric / t.n AS observado_p,
    (SELECT COUNT(*) FROM base_parejas)::bigint AS parejas_base
  FROM totales t
  JOIN esperados e USING (indicador_id)
)
SELECT
  orden,
  indicador_id,
  indicador,
  familia,
  n,
  observado_n,
  ROUND(esperado_p * n, 2) AS esperado_n,
  ROUND(100.0 * observado_p, 4) AS observado_pct,
  ROUND(100.0 * esperado_p, 4) AS esperado_pct,
  ROUND(100.0 * (observado_p - esperado_p), 4) AS exceso_pp,
  ROUND(observado_p / NULLIF(esperado_p, 0), 4) AS razon_observado_esperado,
  ROUND((observado_p - esperado_p) / NULLIF(1 - esperado_p, 0), 4) AS kappa,
  ROUND(
    (observado_p / NULLIF(1 - observado_p, 0))
      / NULLIF(esperado_p / NULLIF(1 - esperado_p, 0), 0),
    4
  ) AS odds_ratio_global,
  ROUND(100.0 * duncan, 4) AS duncan_pct,
  ROUND(100.0 * n / parejas_base, 4) AS cobertura_pct,
  categorias_jefatura,
  categorias_pareja,
  parejas_base,
  (SELECT COUNT(*) FILTER (WHERE estado = 'error')
   FROM meta.controles_calidad_analitica) AS errores_qa
FROM calculos
ORDER BY kappa DESC, cobertura_pct DESC, orden;
