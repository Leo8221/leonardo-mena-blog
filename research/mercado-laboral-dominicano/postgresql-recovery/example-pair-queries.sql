-- Consultas de inicio para censo_2022.
-- Suprimen combinaciones pequenas en salidas editoriales para evitar ruido.

-- 1) Cobertura general y controles.
SELECT * FROM analitica.resumen_parejas_2022;
SELECT * FROM meta.controles_calidad_analitica ORDER BY control;

-- 2) Caracteristicas demograficas de las parejas por territorio.
SELECT
  provincia,
  composicion_sexo,
  COUNT(*) AS parejas,
  ROUND(AVG(jefatura_edad), 1) AS edad_media_jefatura,
  ROUND(AVG(pareja_edad), 1) AS edad_media_pareja,
  ROUND(AVG(diferencia_edad_absoluta), 1) AS brecha_edad_media
FROM analitica.parejas_jefatura_2022
WHERE edades_plausibles
GROUP BY provincia, composicion_sexo
HAVING COUNT(*) >= 50
ORDER BY provincia, parejas DESC;

-- 3) Con que campos de estudio se empareja cada campo.
SELECT
  sexo_persona,
  campo_estudio_persona,
  campo_estudio_pareja,
  parejas,
  pct_dentro_campo_persona
FROM analitica.matriz_campos_estudio_parejas_2022
WHERE parejas >= 50
ORDER BY campo_estudio_persona, sexo_persona, parejas DESC;

-- 4) Con que grandes grupos ocupacionales se empareja cada ocupacion.
SELECT
  sexo_persona,
  ocupacion_persona_grupo,
  ocupacion_pareja_grupo,
  parejas,
  pct_dentro_ocupacion_persona
FROM analitica.matriz_ocupaciones_parejas_2022
WHERE parejas >= 50
ORDER BY ocupacion_persona_grupo, sexo_persona, parejas DESC;

-- 5) Combinaciones detalladas de ocupaciones. P60 puede ser la ocupacion
-- actual o la ultima ocupacion, segun la trayectoria laboral de la persona.
SELECT
  ocupacion_persona_code,
  ocupacion_persona_desc,
  ocupacion_pareja_code,
  ocupacion_pareja_desc,
  COUNT(*) AS parejas
FROM analitica.parejas_profesiones_2022
WHERE ocupacion_persona_code NOT IN (9998, 9999)
  AND ocupacion_pareja_code NOT IN (9998, 9999)
GROUP BY ocupacion_persona_code, ocupacion_persona_desc,
         ocupacion_pareja_code, ocupacion_pareja_desc
HAVING COUNT(*) >= 50
ORDER BY parejas DESC;

-- 6) Homogamia educativa por zona.
SELECT
  zona_lbl,
  COUNT(*) FILTER (WHERE mismo_nivel_educativo IS NOT NULL) AS parejas_con_datos,
  ROUND(100.0 * COUNT(*) FILTER (WHERE mismo_nivel_educativo IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE mismo_nivel_educativo IS NOT NULL), 0), 2) AS pct_mismo_nivel
FROM analitica.parejas_jefatura_2022
GROUP BY zona_lbl
ORDER BY zona_lbl;

-- 7) En censos_linea_tiempo: cambio historico de brecha de edad.
SELECT
  anio,
  COUNT(*) AS parejas,
  ROUND(AVG(diferencia_edad_absoluta), 2) AS brecha_edad_media,
  ROUND(100.0 * COUNT(*) FILTER (WHERE mismo_nivel_educativo IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE mismo_nivel_educativo IS NOT NULL), 0), 2) AS pct_mismo_nivel
FROM analitica.parejas_historicas
WHERE edades_plausibles
GROUP BY anio
ORDER BY anio;
