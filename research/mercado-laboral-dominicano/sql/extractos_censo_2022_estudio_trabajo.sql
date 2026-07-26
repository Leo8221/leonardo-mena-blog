/* =========================================================
   Consultas para articulo: lo que se estudia vs lo que se trabaja
   Requiere:
   - diccionarios.p45_campo_estudio
   - public.vw_estudio_trabajo_censo
   ========================================================= */

-- 1) Top campos de estudio entre ocupados 25-34 con educacion superior
SELECT
    campo_estudio_detallado,
    COUNT(*) AS personas
FROM public.vw_estudio_trabajo_censo
WHERE p27_edad BETWEEN 25 AND 34
  AND educ_superior = 1
  AND ocupado = 1
  AND campo_estudio_estado = 'valido_isced_f_2013'
GROUP BY campo_estudio_detallado
ORDER BY personas DESC
LIMIT 25;

-- 2) Top ocupaciones por campo de estudio
WITH ranked AS (
    SELECT
        campo_estudio_detallado,
        ocupacion_desc,
        grupo_calificacion_ocupacion,
        COUNT(*) AS personas,
        ROW_NUMBER() OVER (
            PARTITION BY campo_estudio_detallado
            ORDER BY COUNT(*) DESC
        ) AS ranking
    FROM public.vw_estudio_trabajo_censo
    WHERE p27_edad BETWEEN 25 AND 34
      AND educ_superior = 1
      AND ocupado = 1
      AND campo_estudio_estado = 'valido_isced_f_2013'
      AND p60_code IS NOT NULL
      AND p60_code NOT IN (9998,9999)
      AND ocupacion_desc IS NOT NULL
    GROUP BY campo_estudio_detallado, ocupacion_desc, grupo_calificacion_ocupacion
)
SELECT *
FROM ranked
WHERE ranking <= 5
ORDER BY campo_estudio_detallado, ranking;

-- 3) Calificacion ocupacional por campo de estudio
SELECT
    campo_estudio_detallado,
    grupo_calificacion_ocupacion,
    COUNT(*) AS personas,
    ROUND(
        100.0 * COUNT(*)
        / NULLIF(SUM(COUNT(*)) OVER (PARTITION BY campo_estudio_detallado), 0),
        2
    ) AS pct_dentro_campo
FROM public.vw_estudio_trabajo_censo
WHERE p27_edad BETWEEN 25 AND 34
  AND educ_superior = 1
  AND ocupado = 1
  AND campo_estudio_estado = 'valido_isced_f_2013'
  AND p60_code IS NOT NULL
  AND p60_code NOT IN (9998,9999)
GROUP BY campo_estudio_detallado, grupo_calificacion_ocupacion
ORDER BY campo_estudio_detallado, personas DESC;

-- 4) Campos con mayor proporcion de ocupaciones de alta calificacion
SELECT
    campo_estudio_detallado,
    COUNT(*) AS ocupados_con_ocupacion,
    COUNT(*) FILTER (WHERE grupo_calificacion_ocupacion = 'Alta calificacion') AS alta_calificacion,
    ROUND(
        100.0 * COUNT(*) FILTER (WHERE grupo_calificacion_ocupacion = 'Alta calificacion')
        / NULLIF(COUNT(*), 0),
        2
    ) AS pct_alta_calificacion
FROM public.vw_estudio_trabajo_censo
WHERE p27_edad BETWEEN 25 AND 34
  AND educ_superior = 1
  AND ocupado = 1
  AND campo_estudio_estado = 'valido_isced_f_2013'
  AND p60_code IS NOT NULL
  AND p60_code NOT IN (9998,9999)
GROUP BY campo_estudio_detallado
HAVING COUNT(*) >= 500
ORDER BY pct_alta_calificacion DESC, ocupados_con_ocupacion DESC;

-- 5) Campos con mayor desajuste hacia ocupaciones medias o elementales
SELECT
    campo_estudio_detallado,
    COUNT(*) AS ocupados_con_ocupacion,
    COUNT(*) FILTER (WHERE grupo_calificacion_ocupacion <> 'Alta calificacion') AS no_alta_calificacion,
    ROUND(
        100.0 * COUNT(*) FILTER (WHERE grupo_calificacion_ocupacion <> 'Alta calificacion')
        / NULLIF(COUNT(*), 0),
        2
    ) AS pct_no_alta_calificacion
FROM public.vw_estudio_trabajo_censo
WHERE p27_edad BETWEEN 25 AND 34
  AND educ_superior = 1
  AND ocupado = 1
  AND campo_estudio_estado = 'valido_isced_f_2013'
  AND p60_code IS NOT NULL
  AND p60_code NOT IN (9998,9999)
GROUP BY campo_estudio_detallado
HAVING COUNT(*) >= 500
ORDER BY pct_no_alta_calificacion DESC, ocupados_con_ocupacion DESC;
