CREATE SCHEMA IF NOT EXISTS armonizado;

DROP VIEW IF EXISTS armonizado.personas CASCADE;
CREATE VIEW armonizado.personas AS
WITH b AS (
  SELECT
    concat('2022-', hogar_id::text) AS id_hogar,
    NULL::text AS id_vivienda,
    concat_ws('-', '2022', hogar_id::text, p25_orden::text) AS id_persona,
    region AS region_code,
    provincia AS provincia_code,
    municipio AS municipio_code,
    dmunicipal AS distrito_municipal_code,
    zona AS zona_code,
    p26_sexo AS sexo_code,
    p27_edad AS edad,
    p28_parent AS parentesco_code,
    p41 AS sabe_leer_code,
    p42 AS asiste_code,
    p43 AS nivel_educativo_code,
    p44 AS ultimo_anio_aprobado,
    p45_code::text AS campo_estudio_code,
    p46 AS graduado_code,
    p53 AS tuvo_empleo_code,
    p54 AS trabajo_pago_code,
    p55 AS ayudo_sin_pago_code,
    p56 AS busco_trabajo_code,
    p58 AS trabajo_antes_code,
    p60_code::text AS ocupacion_code,
    p62_code::text AS rama_actividad_code,
    p63 AS estado_civil_code,
    p34_ano AS anio_nacimiento,
    p65 AS hijos_nacidos_vivos,
    p66 AS hijos_sobrevivientes
  FROM analitica.xcnpv_unificada
)
SELECT
  2022::smallint AS anio,
  'XCNPV2022'::text AS fuente,
  id_hogar,
  id_vivienda,
  id_persona,
  region_code,
  provincia_code,
  municipio_code,
  distrito_municipal_code,
  NULL::integer AS seccion_code,
  NULL::integer AS barrio_code,
  zona_code,
  CASE zona_code WHEN 1 THEN 'Urbano' WHEN 2 THEN 'Rural' ELSE NULL END AS zona_lbl,
  sexo_code,
  CASE sexo_code WHEN 1 THEN 'Hombre' WHEN 2 THEN 'Mujer' ELSE NULL END AS sexo_lbl,
  edad,
  CASE
    WHEN edad IS NULL THEN NULL
    WHEN edad >= 100 THEN '100+'
    ELSE concat((edad / 5) * 5, '-', ((edad / 5) * 5) + 4)
  END AS grupo_edad_5,
  CASE
    WHEN edad IS NULL THEN NULL
    WHEN edad < 15 THEN '0-14'
    WHEN edad BETWEEN 15 AND 24 THEN '15-24'
    WHEN edad BETWEEN 25 AND 34 THEN '25-34'
    WHEN edad BETWEEN 35 AND 44 THEN '35-44'
    WHEN edad BETWEEN 45 AND 54 THEN '45-54'
    WHEN edad BETWEEN 55 AND 64 THEN '55-64'
    ELSE '65+'
  END AS grupo_edad_amplio,
  parentesco_code,
  sabe_leer_code,
  CASE sabe_leer_code WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS sabe_leer_bool,
  asiste_code,
  CASE asiste_code WHEN 1 THEN true WHEN 2 THEN false WHEN 3 THEN false ELSE NULL END AS asiste_actual_bool,
  nivel_educativo_code,
  CASE nivel_educativo_code
    WHEN 1 THEN 'Preprimaria'
    WHEN 2 THEN 'Primaria o basica'
    WHEN 3 THEN 'Secundaria o media'
    WHEN 4 THEN 'Universitaria'
    WHEN 5 THEN 'Maestria'
    WHEN 6 THEN 'Doctorado'
    ELSE NULL
  END AS nivel_educativo_lbl,
  CASE
    WHEN nivel_educativo_code = 1 THEN 'Preprimaria'
    WHEN nivel_educativo_code = 2 THEN 'Primaria'
    WHEN nivel_educativo_code = 3 THEN 'Secundaria'
    WHEN nivel_educativo_code = 4 THEN 'Superior'
    WHEN nivel_educativo_code IN (5,6) THEN 'Postgrado'
    ELSE NULL
  END AS nivel_educativo_grupo,
  CASE WHEN nivel_educativo_code IN (4,5,6) THEN true WHEN nivel_educativo_code IN (1,2,3) THEN false ELSE NULL END AS educ_superior_bool,
  CASE WHEN nivel_educativo_code IN (3,4,5,6) THEN true WHEN nivel_educativo_code IN (1,2) THEN false ELSE NULL END AS secundaria_o_mas_bool,
  ultimo_anio_aprobado,
  campo_estudio_code,
  graduado_code,
  CASE WHEN graduado_code = 1 THEN true WHEN graduado_code = 2 THEN false ELSE NULL END AS graduado_bool,
  tuvo_empleo_code,
  trabajo_pago_code,
  ayudo_sin_pago_code,
  busco_trabajo_code,
  trabajo_antes_code,
  NULL::integer AS condicion_actividad_code,
  NULL::text AS condicion_actividad_lbl,
  CASE
    WHEN tuvo_empleo_code = 1 OR trabajo_pago_code = 1 OR ayudo_sin_pago_code = 1 THEN true
    WHEN tuvo_empleo_code = 2 AND trabajo_pago_code = 2 AND ayudo_sin_pago_code = 2 THEN false
    ELSE NULL
  END AS ocupado_bool,
  CASE
    WHEN (tuvo_empleo_code = 2 OR tuvo_empleo_code IS NULL)
      AND (trabajo_pago_code = 2 OR trabajo_pago_code IS NULL)
      AND (ayudo_sin_pago_code = 2 OR ayudo_sin_pago_code IS NULL)
      AND busco_trabajo_code = 1 THEN true
    WHEN busco_trabajo_code = 2 THEN false
    ELSE NULL
  END AS desocupado_bool,
  CASE
    WHEN (tuvo_empleo_code = 2 OR tuvo_empleo_code IS NULL)
      AND (trabajo_pago_code = 2 OR trabajo_pago_code IS NULL)
      AND (ayudo_sin_pago_code = 2 OR ayudo_sin_pago_code IS NULL)
      AND busco_trabajo_code = 2 THEN true
    ELSE NULL
  END AS inactivo_bool,
  ocupacion_code,
  rama_actividad_code,
  estado_civil_code,
  anio_nacimiento,
  NULL::text AS provincia_nacimiento_code,
  NULL::text AS pais_nacimiento_code,
  hijos_nacidos_vivos,
  hijos_sobrevivientes,
  'hogar_id se reconstruye del orden oficial de la base unificada; estado conyugal=P63; condicion de actividad se deriva de P53-P56'::text AS nota_comparabilidad
FROM b;

DROP VIEW IF EXISTS armonizado.hogares CASCADE;
CREATE VIEW armonizado.hogares AS
SELECT
  2022::smallint AS anio,
  'XCNPV2022'::text AS fuente,
  concat('2022-', hogar_id::text) AS id_hogar,
  NULL::text AS id_vivienda,
  region AS region_code,
  provincia AS provincia_code,
  municipio AS municipio_code,
  dmunicipal AS distrito_municipal_code,
  NULL::integer AS seccion_code,
  NULL::integer AS barrio_code,
  zona AS zona_code,
  CASE zona WHEN 1 THEN 'Urbano' WHEN 2 THEN 'Rural' ELSE NULL END AS zona_lbl,
  NULL::integer AS hombres_total,
  NULL::integer AS mujeres_total,
  NULL::integer AS miembros_total,
  NULL::integer AS tipo_hogar_code,
  p16 AS tenencia_code,
  p17 AS dormitorios,
  p07 AS cuartos_vivienda,
  NULL::integer AS hacinamiento_code,
  NULL::integer AS saneamiento_code,
  NULL::integer AS jefe_sexo_code,
  NULL::integer AS jefe_edad,
  NULL::integer AS jefe_anios_estudio,
  CASE p15a WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS tiene_estufa,
  CASE p15b WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS tiene_nevera,
  CASE p15c WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS tiene_lavadora,
  CASE p15d WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS tiene_televisor,
  CASE p15e WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS tiene_radio,
  CASE WHEN p15h = 1 OR p15i = 1 OR p15j = 1 THEN true WHEN p15h = 2 AND p15i = 2 AND p15j = 2 THEN false ELSE NULL END AS tiene_computadora,
  CASE p15m WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS tiene_internet,
  CASE p15r WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS tiene_automovil,
  CASE p15q WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS tiene_motor,
  CASE p15k WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS tiene_celular,
  CASE p15l WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS tiene_telefono_fijo,
  CASE p15p WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS tiene_aire,
  CASE WHEN p15f = 1 OR p15g = 1 THEN true WHEN p15f = 2 AND p15g = 2 THEN false ELSE NULL END AS tiene_cisterna,
  CASE p15n WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS tiene_inversor,
  CASE p15o WHEN 1 THEN true WHEN 2 THEN false ELSE NULL END AS tiene_planta,
  p11 AS sanitario_code,
  p10 AS agua_code,
  p14 AS basura_code,
  p18 AS combustible_code,
  p19 AS alumbrado_code,
  'tipo_hogar, hacinamiento y jefe no estan precalculados en tabla limpia 2022 usada aqui'::text AS nota_comparabilidad
FROM analitica.xcnpv_unificada
WHERE p25_orden = 1;

DROP VIEW IF EXISTS armonizado.viviendas CASCADE;
CREATE VIEW armonizado.viviendas AS
SELECT
  2022::smallint AS anio,
  'XCNPV2022'::text AS fuente,
  NULL::text AS id_vivienda,
  region AS region_code,
  provincia AS provincia_code,
  municipio AS municipio_code,
  dmunicipal AS distrito_municipal_code,
  NULL::integer AS seccion_code,
  NULL::integer AS barrio_code,
  zona AS zona_code,
  CASE zona WHEN 1 THEN 'Urbano' WHEN 2 THEN 'Rural' ELSE NULL END AS zona_lbl,
  p01 AS tipo_vivienda_code,
  p02 AS condicion_ocupacion_code,
  p03 AS paredes_code,
  p04 AS techo_code,
  p05 AS piso_code,
  p07 AS cuartos_vivienda,
  p06 AS cuarto_cocina_code,
  p08 AS cantidad_hogares,
  miembros_total AS personas_total,
  NULL::integer AS calidad_vivienda_code,
  NULL::boolean AS entorno_aguas_estancadas,
  NULL::boolean AS entorno_basura,
  NULL::boolean AS entorno_canada,
  NULL::boolean AS entorno_ruido_vehiculo,
  NULL::boolean AS entorno_pocilga_granja,
  NULL::boolean AS entorno_humo_gases,
  NULL::boolean AS entorno_desechos,
  NULL::boolean AS entorno_ruido_planta,
  NULL::boolean AS entorno_envasadora,
  NULL::boolean AS entorno_ninguna,
  'el archivo publico separado no incluye una llave nacional de vivienda; id_vivienda queda nulo y esta vista solo debe agregarse'::text AS nota_comparabilidad
FROM public.vivienda_hogar_limpia;

DROP VIEW IF EXISTS armonizado.metadatos_variables CASCADE;
CREATE VIEW armonizado.metadatos_variables AS
SELECT * FROM (VALUES
  ('personas','id_hogar','secuencia estable reconstruida desde P25_ORDEN=1 en la base unificada oficial','alta'),
  ('personas','educ_superior_bool','P43 in 4,5,6','alta'),
  ('personas','ocupado_bool','derivado de P53=1 OR P54=1 OR P55=1','media'),
  ('personas','condicion_actividad_code','no disponible como recodificacion equivalente 2002/2010','baja'),
  ('personas','estado_civil_code','P63; P66 corresponde a hijos sobrevivientes','alta'),
  ('hogares','activos_hogar','P15A-P15R, codigos 1=si 2=no','alta'),
  ('viviendas','id_vivienda','no publicado en CSV separado; no usar para joins','baja'),
  ('viviendas','materiales','P03 paredes, P04 techo, P05 piso','alta')
) AS t(tabla, variable, regla, comparabilidad);
