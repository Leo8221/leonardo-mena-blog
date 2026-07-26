\set ON_ERROR_STOP on

SET statement_timeout = 0;
SET work_mem = '256MB';
SET maintenance_work_mem = '1GB';
SET max_parallel_maintenance_workers = 4;

-- Capa derivada para estudios de hogar, pareja, educacion y ocupacion.
-- No modifica ni reemplaza los microdatos de origen.

CREATE SCHEMA IF NOT EXISTS analitica;
CREATE SCHEMA IF NOT EXISTS meta;

CREATE UNIQUE INDEX IF NOT EXISTS cno_2019_code_uidx
  ON diccionarios.cno_2019 (code);
CREATE UNIQUE INDEX IF NOT EXISTS cnae_2019_code_uidx
  ON diccionarios.cnae_2019 (code);
ANALYZE diccionarios.cno_2019;
ANALYZE diccionarios.cnae_2019;

CREATE OR REPLACE FUNCTION analitica.nivel_educativo(p_code integer)
RETURNS text
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT CASE p_code
    WHEN 1 THEN 'Preprimaria'
    WHEN 2 THEN 'Primaria o basica'
    WHEN 3 THEN 'Secundaria o media'
    WHEN 4 THEN 'Universitaria'
    WHEN 5 THEN 'Maestria'
    WHEN 6 THEN 'Doctorado'
    ELSE NULL
  END;
$$;

CREATE OR REPLACE FUNCTION analitica.campo_estudio_amplio(p_code integer)
RETURNS text
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT CASE LEFT(LPAD(p_code::text, 4, '0'), 2)
    WHEN '00' THEN 'Programas y certificaciones genericos'
    WHEN '01' THEN 'Educacion'
    WHEN '02' THEN 'Artes y humanidades'
    WHEN '03' THEN 'Ciencias sociales, periodismo e informacion'
    WHEN '04' THEN 'Negocios, administracion y derecho'
    WHEN '05' THEN 'Ciencias naturales, matematicas y estadistica'
    WHEN '06' THEN 'Tecnologias de la informacion y la comunicacion'
    WHEN '07' THEN 'Ingenieria, industria y construccion'
    WHEN '08' THEN 'Agricultura, silvicultura, pesca y veterinaria'
    WHEN '09' THEN 'Salud y bienestar'
    WHEN '10' THEN 'Servicios'
    ELSE NULL
  END;
$$;

CREATE OR REPLACE FUNCTION analitica.ocupacion_gran_grupo(p_code integer)
RETURNS text
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT CASE LEFT(p_code::text, 1)
    WHEN '0' THEN 'Ocupaciones militares'
    WHEN '1' THEN 'Directores y gerentes'
    WHEN '2' THEN 'Profesionales cientificos e intelectuales'
    WHEN '3' THEN 'Tecnicos y profesionales de nivel medio'
    WHEN '4' THEN 'Personal de apoyo administrativo'
    WHEN '5' THEN 'Servicios y vendedores'
    WHEN '6' THEN 'Agricultura, forestales y pesca'
    WHEN '7' THEN 'Oficiales, operarios y artesanos'
    WHEN '8' THEN 'Operadores de instalaciones y maquinas'
    WHEN '9' THEN 'Ocupaciones elementales'
    ELSE NULL
  END;
$$;

CREATE OR REPLACE FUNCTION analitica.ocupacion_gran_grupo(p_code text)
RETURNS text
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT CASE LEFT(p_code, 1)
    WHEN '0' THEN 'Ocupaciones militares'
    WHEN '1' THEN 'Directores y gerentes'
    WHEN '2' THEN 'Profesionales cientificos e intelectuales'
    WHEN '3' THEN 'Tecnicos y profesionales de nivel medio'
    WHEN '4' THEN 'Personal de apoyo administrativo'
    WHEN '5' THEN 'Servicios y vendedores'
    WHEN '6' THEN 'Agricultura, forestales y pesca'
    WHEN '7' THEN 'Oficiales, operarios y artesanos'
    WHEN '8' THEN 'Operadores de instalaciones y maquinas'
    WHEN '9' THEN 'Ocupaciones elementales'
    ELSE NULL
  END;
$$;

CREATE OR REPLACE VIEW analitica.personas_relaciones_2022 AS
SELECT
  u.hogar_id,
  '2022-' || u.hogar_id::text AS id_hogar,
  '2022-' || u.fila_origen::text AS id_persona,
  (u.p28_parent <> 14) AS es_hogar_particular,
  CASE WHEN u.p28_parent = 14 THEN 'Vivienda colectiva' ELSE 'Hogar particular' END AS tipo_unidad,
  u.p25_orden AS orden_persona,
  u.region,
  u.provincia,
  u.municipio,
  u.dmunicipal,
  u.zona,
  CASE u.zona WHEN 1 THEN 'Urbana' WHEN 2 THEN 'Rural' ELSE NULL END AS zona_lbl,
  u.p26_sexo AS sexo_code,
  CASE u.p26_sexo WHEN 1 THEN 'Hombre' WHEN 2 THEN 'Mujer' ELSE NULL END AS sexo_lbl,
  NULLIF(u.p27_edad, 999) AS edad,
  u.p28_parent AS parentesco_code,
  CASE u.p28_parent
    WHEN 1 THEN 'Jefatura del hogar'
    WHEN 2 THEN 'Esposa, esposo, companera o companero'
    WHEN 3 THEN 'Hija o hijo'
    WHEN 4 THEN 'Hija o hijo de crianza'
    WHEN 5 THEN 'Madre o padre'
    WHEN 6 THEN 'Nieta o nieto'
    WHEN 7 THEN 'Suegra o suegro'
    WHEN 8 THEN 'Abuela o abuelo'
    WHEN 9 THEN 'Hermana o hermano'
    WHEN 10 THEN 'Nuera o yerno'
    WHEN 11 THEN 'Otro pariente'
    WHEN 12 THEN 'No pariente'
    WHEN 13 THEN 'Empleo domestico'
    WHEN 14 THEN 'Persona en vivienda colectiva'
    ELSE NULL
  END AS parentesco_lbl,
  u.p43 AS nivel_educativo_code,
  analitica.nivel_educativo(u.p43) AS nivel_educativo_lbl,
  u.p44 AS ultimo_curso_aprobado,
  u.p45_code AS campo_estudio_code,
  analitica.campo_estudio_amplio(u.p45_code) AS campo_estudio_amplio,
  u.p46 AS graduado_code,
  (u.p46 = 1) AS graduado_bool,
  u.p47 AS anos_estudios_superiores,
  u.p60_code AS ocupacion_code,
  analitica.ocupacion_gran_grupo(COALESCE(cno_exact.code, cno_padded.code)) AS ocupacion_gran_grupo,
  COALESCE(cno_exact."desc", cno_padded."desc") AS ocupacion_desc,
  u.p61 AS categoria_ocupacional_code,
  u.p62_code AS rama_actividad_code,
  COALESCE(cnae_exact."desc", cnae_padded."desc") AS rama_actividad_desc,
  u.p63 AS estado_conyugal_code,
  CASE u.p63
    WHEN 1 THEN 'Separado de matrimonio'
    WHEN 2 THEN 'Divorciado'
    WHEN 3 THEN 'Viudo'
    WHEN 4 THEN 'Separado de union libre'
    WHEN 5 THEN 'Casado'
    WHEN 6 THEN 'Unido'
    WHEN 7 THEN 'Nunca casado ni unido'
    ELSE NULL
  END AS estado_conyugal_lbl,
  CASE
    WHEN u.p53 = 1 OR u.p54 = 1 OR u.p55 = 1 THEN true
    WHEN u.p53 = 2 AND u.p54 = 2 AND u.p55 = 2 THEN false
    ELSE NULL
  END AS ocupado_bool,
  u.p64 AS autoidentificacion_etnorracial_code,
  u.p65 AS hijos_nacidos_vivos,
  u.p66 AS hijos_sobrevivientes,
  u.p01 AS tipo_vivienda_code,
  u.p03 AS paredes_code,
  u.p04 AS techo_code,
  u.p05 AS piso_code,
  u.p07 AS cuartos_vivienda,
  u.p10 AS agua_code,
  u.p11 AS sanitario_code,
  u.p14 AS basura_code,
  u.p16 AS tenencia_code,
  u.p17 AS dormitorios,
  u.p18 AS combustible_code,
  u.p19 AS alumbrado_code,
  (u.p15m = 1) AS tiene_internet,
  (u.p15h = 1 OR u.p15i = 1 OR u.p15j = 1) AS tiene_computadora,
  (u.p15r = 1) AS tiene_automovil,
  (u.p15q = 1) AS tiene_motor,
  u.p29_padre AS padre_vive_hogar_code,
  u.p30_madre AS madre_vive_hogar_code,
  CASE
    WHEN u.p40_1 IN (2,3,4) OR u.p40_2 IN (2,3,4) OR u.p40_3 IN (2,3,4)
      OR u.p40_4 IN (2,3,4) OR u.p40_5 IN (2,3,4) OR u.p40_6 IN (2,3,4)
    THEN true
    WHEN u.p40_1 = 1 AND u.p40_2 = 1 AND u.p40_3 = 1
      AND u.p40_4 = 1 AND u.p40_5 = 1 AND u.p40_6 = 1
    THEN false
    ELSE NULL
  END AS discapacidad_bool,
  (u.p48a = 1) AS uso_computadora_escritorio,
  (u.p48b = 1) AS uso_portatil,
  (u.p48c = 1) AS uso_tableta,
  (u.p48d = 1) AS uso_smartphone,
  CASE u.p49 WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS uso_internet_bool,
  u.fila_origen
FROM analitica.xcnpv_unificada u
LEFT JOIN diccionarios.cno_2019 cno_exact
  ON cno_exact.code = u.p60_code::text
LEFT JOIN diccionarios.cno_2019 cno_padded
  ON cno_exact.code IS NULL
 AND cno_padded.code = LPAD(u.p60_code::text, 4, '0')
LEFT JOIN diccionarios.cnae_2019 cnae_exact
  ON cnae_exact.code = u.p62_code::text
LEFT JOIN diccionarios.cnae_2019 cnae_padded
  ON cnae_exact.code IS NULL
 AND cnae_padded.code = LPAD(u.p62_code::text, 4, '0');

DROP MATERIALIZED VIEW IF EXISTS analitica.parejas_jefatura_2022 CASCADE;
DROP MATERIALIZED VIEW IF EXISTS analitica.hogares_pareja_validos_2022 CASCADE;
CREATE MATERIALIZED VIEW analitica.hogares_pareja_validos_2022 AS
SELECT hogar_id
FROM analitica.xcnpv_unificada
WHERE p28_parent IN (1, 2)
GROUP BY hogar_id
HAVING COUNT(*) FILTER (WHERE p28_parent = 1) = 1
   AND COUNT(*) FILTER (WHERE p28_parent = 2) = 1;

CREATE UNIQUE INDEX hogares_pareja_validos_2022_hogar_idx
  ON analitica.hogares_pareja_validos_2022 (hogar_id);
ANALYZE analitica.hogares_pareja_validos_2022;

CREATE MATERIALIZED VIEW analitica.parejas_jefatura_2022 AS
WITH j AS (
  SELECT p.*
  FROM analitica.personas_relaciones_2022 p
  JOIN analitica.hogares_pareja_validos_2022 v USING (hogar_id)
  WHERE p.parentesco_code = 1
),
c AS (
  SELECT p.*
  FROM analitica.personas_relaciones_2022 p
  JOIN analitica.hogares_pareja_validos_2022 v USING (hogar_id)
  WHERE p.parentesco_code = 2
)
SELECT
  j.hogar_id,
  j.id_hogar,
  j.region,
  j.provincia,
  j.municipio,
  j.dmunicipal,
  j.zona,
  j.zona_lbl,
  j.tipo_vivienda_code,
  j.paredes_code,
  j.techo_code,
  j.piso_code,
  j.cuartos_vivienda,
  j.dormitorios,
  j.agua_code,
  j.sanitario_code,
  j.basura_code,
  j.tenencia_code,
  j.combustible_code,
  j.alumbrado_code,
  j.tiene_internet,
  j.tiene_computadora,
  j.tiene_automovil,
  j.tiene_motor,
  j.id_persona AS jefatura_id_persona,
  j.sexo_code AS jefatura_sexo_code,
  j.sexo_lbl AS jefatura_sexo,
  j.edad AS jefatura_edad,
  j.nivel_educativo_code AS jefatura_nivel_educativo_code,
  j.nivel_educativo_lbl AS jefatura_nivel_educativo,
  j.campo_estudio_code AS jefatura_campo_estudio_code,
  j.campo_estudio_amplio AS jefatura_campo_estudio_amplio,
  j.graduado_bool AS jefatura_graduado,
  j.ocupacion_code AS jefatura_ocupacion_code,
  j.ocupacion_gran_grupo AS jefatura_ocupacion_gran_grupo,
  j.ocupacion_desc AS jefatura_ocupacion_desc,
  j.categoria_ocupacional_code AS jefatura_categoria_ocupacional_code,
  j.rama_actividad_code AS jefatura_rama_actividad_code,
  j.rama_actividad_desc AS jefatura_rama_actividad_desc,
  j.ocupado_bool AS jefatura_ocupado,
  j.estado_conyugal_code AS jefatura_estado_conyugal_code,
  j.estado_conyugal_lbl AS jefatura_estado_conyugal,
  j.autoidentificacion_etnorracial_code AS jefatura_autoidentificacion_code,
  j.hijos_nacidos_vivos AS jefatura_hijos_nacidos_vivos,
  c.id_persona AS pareja_id_persona,
  c.sexo_code AS pareja_sexo_code,
  c.sexo_lbl AS pareja_sexo,
  c.edad AS pareja_edad,
  c.nivel_educativo_code AS pareja_nivel_educativo_code,
  c.nivel_educativo_lbl AS pareja_nivel_educativo,
  c.campo_estudio_code AS pareja_campo_estudio_code,
  c.campo_estudio_amplio AS pareja_campo_estudio_amplio,
  c.graduado_bool AS pareja_graduado,
  c.ocupacion_code AS pareja_ocupacion_code,
  c.ocupacion_gran_grupo AS pareja_ocupacion_gran_grupo,
  c.ocupacion_desc AS pareja_ocupacion_desc,
  c.categoria_ocupacional_code AS pareja_categoria_ocupacional_code,
  c.rama_actividad_code AS pareja_rama_actividad_code,
  c.rama_actividad_desc AS pareja_rama_actividad_desc,
  c.ocupado_bool AS pareja_ocupado,
  c.estado_conyugal_code AS pareja_estado_conyugal_code,
  c.estado_conyugal_lbl AS pareja_estado_conyugal,
  c.autoidentificacion_etnorracial_code AS pareja_autoidentificacion_code,
  c.hijos_nacidos_vivos AS pareja_hijos_nacidos_vivos,
  (j.edad - c.edad) AS diferencia_edad_jefatura_menos_pareja,
  ABS(j.edad - c.edad) AS diferencia_edad_absoluta,
  CASE
    WHEN j.sexo_code = 1 AND c.sexo_code = 2 THEN 'Hombre-Mujer'
    WHEN j.sexo_code = 2 AND c.sexo_code = 1 THEN 'Mujer-Hombre'
    WHEN j.sexo_code = c.sexo_code THEN 'Mismo sexo declarado'
    ELSE 'No determinado'
  END AS composicion_sexo,
  CASE WHEN j.nivel_educativo_code IS NULL OR c.nivel_educativo_code IS NULL THEN NULL
       ELSE j.nivel_educativo_code = c.nivel_educativo_code END AS mismo_nivel_educativo,
  CASE WHEN j.campo_estudio_amplio IS NULL OR c.campo_estudio_amplio IS NULL THEN NULL
       ELSE j.campo_estudio_amplio = c.campo_estudio_amplio END AS mismo_campo_estudio_amplio,
  CASE WHEN j.ocupacion_gran_grupo IS NULL OR c.ocupacion_gran_grupo IS NULL THEN NULL
       ELSE j.ocupacion_gran_grupo = c.ocupacion_gran_grupo END AS mismo_gran_grupo_ocupacional,
  (j.edad >= 16 AND c.edad >= 16 AND ABS(j.edad - c.edad) <= 45) AS edades_plausibles,
  'alta: una jefatura y una pareja declarada en el hogar'::text AS confianza_enlace
FROM j
JOIN c USING (hogar_id);

CREATE UNIQUE INDEX parejas_jefatura_2022_hogar_idx
  ON analitica.parejas_jefatura_2022 (hogar_id);
CREATE INDEX parejas_jefatura_2022_territorio_idx
  ON analitica.parejas_jefatura_2022 (provincia, municipio, zona);
CREATE INDEX parejas_jefatura_2022_campos_idx
  ON analitica.parejas_jefatura_2022 (jefatura_campo_estudio_code, pareja_campo_estudio_code);
CREATE INDEX parejas_jefatura_2022_ocupaciones_idx
  ON analitica.parejas_jefatura_2022 (jefatura_ocupacion_code, pareja_ocupacion_code);
ANALYZE analitica.parejas_jefatura_2022;

CREATE OR REPLACE VIEW analitica.parejas_profesiones_2022 AS
SELECT
  hogar_id,
  provincia,
  municipio,
  zona,
  'Jefatura'::text AS rol_persona,
  jefatura_sexo AS sexo_persona,
  jefatura_edad AS edad_persona,
  jefatura_campo_estudio_code AS campo_estudio_persona_code,
  jefatura_campo_estudio_amplio AS campo_estudio_persona,
  jefatura_ocupacion_code AS ocupacion_persona_code,
  jefatura_ocupacion_gran_grupo AS ocupacion_persona_grupo,
  jefatura_ocupacion_desc AS ocupacion_persona_desc,
  pareja_sexo AS sexo_pareja,
  pareja_edad AS edad_pareja,
  pareja_campo_estudio_code AS campo_estudio_pareja_code,
  pareja_campo_estudio_amplio AS campo_estudio_pareja,
  pareja_ocupacion_code AS ocupacion_pareja_code,
  pareja_ocupacion_gran_grupo AS ocupacion_pareja_grupo,
  pareja_ocupacion_desc AS ocupacion_pareja_desc
FROM analitica.parejas_jefatura_2022
UNION ALL
SELECT
  hogar_id,
  provincia,
  municipio,
  zona,
  'Pareja'::text AS rol_persona,
  pareja_sexo,
  pareja_edad,
  pareja_campo_estudio_code,
  pareja_campo_estudio_amplio,
  pareja_ocupacion_code,
  pareja_ocupacion_gran_grupo,
  pareja_ocupacion_desc,
  jefatura_sexo,
  jefatura_edad,
  jefatura_campo_estudio_code,
  jefatura_campo_estudio_amplio,
  jefatura_ocupacion_code,
  jefatura_ocupacion_gran_grupo,
  jefatura_ocupacion_desc
FROM analitica.parejas_jefatura_2022;

DROP MATERIALIZED VIEW IF EXISTS analitica.matriz_campos_estudio_parejas_2022;
CREATE MATERIALIZED VIEW analitica.matriz_campos_estudio_parejas_2022 AS
WITH conteos AS (
  SELECT
    sexo_persona,
    campo_estudio_persona,
    campo_estudio_pareja,
    COUNT(*)::bigint AS parejas
  FROM analitica.parejas_profesiones_2022
  WHERE campo_estudio_persona IS NOT NULL
    AND campo_estudio_pareja IS NOT NULL
  GROUP BY sexo_persona, campo_estudio_persona, campo_estudio_pareja
)
SELECT
  *,
  ROUND(100.0 * parejas /
    NULLIF(SUM(parejas) OVER (PARTITION BY sexo_persona, campo_estudio_persona), 0), 2) AS pct_dentro_campo_persona
FROM conteos;

CREATE UNIQUE INDEX matriz_campos_parejas_2022_idx
  ON analitica.matriz_campos_estudio_parejas_2022
  (sexo_persona, campo_estudio_persona, campo_estudio_pareja);

DROP MATERIALIZED VIEW IF EXISTS analitica.matriz_ocupaciones_parejas_2022;
CREATE MATERIALIZED VIEW analitica.matriz_ocupaciones_parejas_2022 AS
WITH conteos AS (
  SELECT
    sexo_persona,
    ocupacion_persona_grupo,
    ocupacion_pareja_grupo,
    COUNT(*)::bigint AS parejas
  FROM analitica.parejas_profesiones_2022
  WHERE ocupacion_persona_grupo IS NOT NULL
    AND ocupacion_pareja_grupo IS NOT NULL
    AND ocupacion_persona_code NOT IN (9998, 9999)
    AND ocupacion_pareja_code NOT IN (9998, 9999)
  GROUP BY sexo_persona, ocupacion_persona_grupo, ocupacion_pareja_grupo
)
SELECT
  *,
  ROUND(100.0 * parejas /
    NULLIF(SUM(parejas) OVER (PARTITION BY sexo_persona, ocupacion_persona_grupo), 0), 2) AS pct_dentro_ocupacion_persona
FROM conteos;

CREATE UNIQUE INDEX matriz_ocupaciones_parejas_2022_idx
  ON analitica.matriz_ocupaciones_parejas_2022
  (sexo_persona, ocupacion_persona_grupo, ocupacion_pareja_grupo);

CREATE OR REPLACE VIEW analitica.resumen_parejas_2022 AS
SELECT
  COUNT(*)::bigint AS parejas_identificadas,
  COUNT(*) FILTER (WHERE edades_plausibles)::bigint AS parejas_edades_plausibles,
  ROUND(AVG(diferencia_edad_absoluta), 2) AS diferencia_edad_absoluta_promedio,
  ROUND(100.0 * COUNT(*) FILTER (WHERE mismo_nivel_educativo IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE mismo_nivel_educativo IS NOT NULL), 0), 2) AS pct_mismo_nivel_educativo,
  ROUND(100.0 * COUNT(*) FILTER (WHERE mismo_campo_estudio_amplio IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE mismo_campo_estudio_amplio IS NOT NULL), 0), 2) AS pct_mismo_campo_estudio,
  ROUND(100.0 * COUNT(*) FILTER (WHERE mismo_gran_grupo_ocupacional IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE mismo_gran_grupo_ocupacional IS NOT NULL), 0), 2) AS pct_mismo_gran_grupo_ocupacional
FROM analitica.parejas_jefatura_2022;

CREATE TABLE IF NOT EXISTS meta.controles_calidad_analitica (
  control text PRIMARY KEY,
  resultado bigint,
  esperado bigint,
  estado text NOT NULL CHECK (estado IN ('ok', 'advertencia', 'error')),
  detalle text,
  validado_en timestamptz NOT NULL DEFAULT now()
);

INSERT INTO meta.controles_calidad_analitica
  (control, resultado, esperado, estado, detalle, validado_en)
SELECT 'filas_unificada', COUNT(*), 10773983,
       CASE WHEN COUNT(*) = 10773983 THEN 'ok' ELSE 'error' END,
       'Debe coincidir con la publicacion de personas XCNPV 2022.', now()
FROM analitica.xcnpv_unificada
ON CONFLICT (control) DO UPDATE SET
  resultado = EXCLUDED.resultado,
  esperado = EXCLUDED.esperado,
  estado = EXCLUDED.estado,
  detalle = EXCLUDED.detalle,
  validado_en = EXCLUDED.validado_en;

INSERT INTO meta.controles_calidad_analitica
  (control, resultado, esperado, estado, detalle, validado_en)
SELECT 'bloques_convivencia', MAX(hogar_id), 3736217,
       CASE WHEN MAX(hogar_id) = 3736217 THEN 'ok' ELSE 'error' END,
       'Incluye 3,726,936 hogares particulares y 9,281 bloques colectivos.', now()
FROM analitica.xcnpv_unificada
ON CONFLICT (control) DO UPDATE SET
  resultado = EXCLUDED.resultado,
  esperado = EXCLUDED.esperado,
  estado = EXCLUDED.estado,
  detalle = EXCLUDED.detalle,
  validado_en = EXCLUDED.validado_en;

INSERT INTO meta.controles_calidad_analitica
  (control, resultado, esperado, estado, detalle, validado_en)
SELECT 'hogares_particulares', COUNT(*) FILTER (WHERE p28_parent = 1), 3726936,
       CASE WHEN COUNT(*) FILTER (WHERE p28_parent = 1) = 3726936 THEN 'ok' ELSE 'error' END,
       'Hogares particulares identificados por una jefatura declarada.', now()
FROM analitica.xcnpv_unificada
ON CONFLICT (control) DO UPDATE SET
  resultado = EXCLUDED.resultado,
  esperado = EXCLUDED.esperado,
  estado = EXCLUDED.estado,
  detalle = EXCLUDED.detalle,
  validado_en = EXCLUDED.validado_en;

INSERT INTO meta.controles_calidad_analitica
  (control, resultado, esperado, estado, detalle, validado_en)
SELECT 'parejas_jefatura_unicas', COUNT(*), NULL, 'ok',
       'Una jefatura y exactamente una esposa/esposo/companero por hogar.', now()
FROM analitica.parejas_jefatura_2022
ON CONFLICT (control) DO UPDATE SET
  resultado = EXCLUDED.resultado,
  esperado = EXCLUDED.esperado,
  estado = EXCLUDED.estado,
  detalle = EXCLUDED.detalle,
  validado_en = EXCLUDED.validado_en;

INSERT INTO meta.controles_calidad_analitica
  (control, resultado, esperado, estado, detalle, validado_en)
VALUES (
  'anomalia_p25_orden_fuente', 1, 1, 'advertencia',
  'La fila oficial 8,141,679 repite P25_ORDEN=2 en el bloque 2,843,047. Se conserva el valor y la identidad usa fila_origen.',
  now()
)
ON CONFLICT (control) DO UPDATE SET
  resultado = EXCLUDED.resultado,
  esperado = EXCLUDED.esperado,
  estado = EXCLUDED.estado,
  detalle = EXCLUDED.detalle,
  validado_en = EXCLUDED.validado_en;

COMMENT ON MATERIALIZED VIEW analitica.parejas_jefatura_2022 IS
  'Una fila por pareja conviviente identificable con alta confianza: jefatura y una unica pareja declarada. No representa parejas no convivientes ni enlaza parejas secundarias del hogar.';
COMMENT ON VIEW analitica.parejas_profesiones_2022 IS
  'Dos filas por pareja para responder, desde cada persona, que campo de estudio u ocupacion tiene su pareja.';

CREATE OR REPLACE VIEW analitica.parejas_jefatura_historica AS
SELECT
  2022::smallint AS anio,
  'XCNPV2022'::text AS fuente,
  id_hogar,
  region AS region_code,
  provincia AS provincia_code,
  municipio AS municipio_code,
  dmunicipal AS distrito_municipal_code,
  zona AS zona_code,
  jefatura_id_persona,
  jefatura_sexo_code,
  jefatura_sexo,
  jefatura_edad,
  jefatura_nivel_educativo_code,
  jefatura_nivel_educativo,
  jefatura_campo_estudio_code::text,
  jefatura_ocupacion_code::text,
  jefatura_estado_conyugal_code,
  pareja_id_persona,
  pareja_sexo_code,
  pareja_sexo,
  pareja_edad,
  pareja_nivel_educativo_code,
  pareja_nivel_educativo,
  pareja_campo_estudio_code::text,
  pareja_ocupacion_code::text,
  pareja_estado_conyugal_code,
  diferencia_edad_jefatura_menos_pareja,
  diferencia_edad_absoluta,
  mismo_nivel_educativo,
  CASE WHEN jefatura_campo_estudio_code IS NULL OR pareja_campo_estudio_code IS NULL THEN NULL
       ELSE jefatura_campo_estudio_code = pareja_campo_estudio_code END AS mismo_campo_estudio_codigo,
  CASE WHEN jefatura_ocupacion_code IS NULL OR pareja_ocupacion_code IS NULL THEN NULL
       ELSE jefatura_ocupacion_code = pareja_ocupacion_code END AS misma_ocupacion_codigo,
  edades_plausibles,
  confianza_enlace
FROM analitica.parejas_jefatura_2022;
