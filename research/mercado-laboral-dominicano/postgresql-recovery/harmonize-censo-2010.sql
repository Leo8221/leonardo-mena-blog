CREATE SCHEMA IF NOT EXISTS armonizado;

CREATE OR REPLACE FUNCTION armonizado.to_int(v text)
RETURNS integer
LANGUAGE sql
IMMUTABLE
AS $$
  SELECT CASE WHEN btrim(v) ~ '^-?[0-9]+$' THEN btrim(v)::integer ELSE NULL END;
$$;

CREATE OR REPLACE FUNCTION armonizado.yes1(v text)
RETURNS boolean
LANGUAGE sql
IMMUTABLE
AS $$
  SELECT CASE
    WHEN btrim(v) = '1' THEN true
    WHEN btrim(v) IN ('2','8','9','99') THEN false
    ELSE NULL
  END;
$$;

DROP VIEW IF EXISTS armonizado.personas CASCADE;
CREATE VIEW armonizado.personas AS
WITH b AS (
  SELECT
    concat_ws('-', NULLIF(btrim(carpeta),''), NULLIF(btrim(num_vivienda),''), NULLIF(btrim(num_hogar),'')) AS id_hogar,
    concat_ws('-', NULLIF(btrim(carpeta),''), NULLIF(btrim(num_vivienda),'')) AS id_vivienda,
    concat_ws('-', NULLIF(btrim(carpeta),''), NULLIF(btrim(num_vivienda),''), NULLIF(btrim(num_hogar),''), NULLIF(btrim(num_persona),'')) AS id_persona,
    armonizado.to_int(region) AS region_code,
    armonizado.to_int(provincia) AS provincia_code,
    armonizado.to_int(municipio) AS municipio_code,
    armonizado.to_int(distrito) AS distrito_municipal_code,
    armonizado.to_int(p27_sexo) AS sexo_code,
    armonizado.to_int(p29_edad_anos_cumplidos) AS edad,
    armonizado.to_int(p26_parentesco) AS parentesco_code,
    armonizado.to_int(p35_sabe_leer) AS sabe_leer_code,
    armonizado.to_int(p36_asiste) AS asiste_code,
    armonizado.to_int(p37_nivel) AS nivel_educativo_code,
    armonizado.to_int(p38_ultimo_ano_aprobado) AS ultimo_anio_aprobado,
    NULLIF(btrim(p39_carrera_universitaria_codigo),'') AS campo_estudio_code,
    armonizado.to_int(p40_segraduo) AS graduado_code,
    armonizado.to_int(p45_tuvo_empleo) AS tuvo_empleo_code,
    armonizado.to_int(p46_actividad_porpaga) AS trabajo_pago_code,
    armonizado.to_int(p47_ayudo_sinpaga) AS ayudo_sin_pago_code,
    armonizado.to_int(p48_busco_trabajo) AS busco_trabajo_code,
    armonizado.to_int(p51_trabajo_antes) AS trabajo_antes_code,
    armonizado.to_int(p45r1_condicion_actividad) AS condicion_actividad_code,
    NULLIF(btrim(p52_ocupacion_codigo),'') AS ocupacion_code,
    NULLIF(btrim(p54_rama_actividad_codigo),'') AS rama_actividad_code,
    armonizado.to_int(p55_estado_civil) AS estado_civil_code,
    armonizado.to_int(p28c_ano_nace) AS anio_nacimiento,
    NULLIF(btrim(p31_municipio_nacio_codigo),'') AS municipio_nacimiento_code,
    NULLIF(btrim(p32_pais_nacio_codigo),'') AS pais_nacimiento_code,
    armonizado.to_int(p56_total) AS hijos_nacidos_vivos,
    armonizado.to_int(p57_total) AS hijos_sobrevivientes
  FROM raw.personas
)
SELECT
  2010::smallint AS anio,
  'CNPV2010'::text AS fuente,
  id_hogar,
  id_vivienda,
  id_persona,
  region_code,
  provincia_code,
  municipio_code,
  distrito_municipal_code,
  NULL::integer AS seccion_code,
  NULL::integer AS barrio_code,
  NULL::integer AS zona_code,
  NULL::text AS zona_lbl,
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
    WHEN 4 THEN 'Universitaria o superior'
    ELSE NULL
  END AS nivel_educativo_lbl,
  CASE nivel_educativo_code
    WHEN 1 THEN 'Preprimaria'
    WHEN 2 THEN 'Primaria'
    WHEN 3 THEN 'Secundaria'
    WHEN 4 THEN 'Superior'
    ELSE NULL
  END AS nivel_educativo_grupo,
  CASE WHEN nivel_educativo_code = 4 THEN true WHEN nivel_educativo_code IN (1,2,3) THEN false ELSE NULL END AS educ_superior_bool,
  CASE WHEN nivel_educativo_code IN (3,4) THEN true WHEN nivel_educativo_code IN (1,2) THEN false ELSE NULL END AS secundaria_o_mas_bool,
  ultimo_anio_aprobado,
  campo_estudio_code,
  graduado_code,
  CASE WHEN graduado_code = 1 THEN true WHEN graduado_code = 2 THEN false ELSE NULL END AS graduado_bool,
  tuvo_empleo_code,
  trabajo_pago_code,
  ayudo_sin_pago_code,
  busco_trabajo_code,
  trabajo_antes_code,
  condicion_actividad_code,
  CASE condicion_actividad_code
    WHEN 1 THEN 'Ocupado'
    WHEN 2 THEN 'Cesante'
    WHEN 3 THEN 'Busca trabajo primera vez'
    WHEN 4 THEN 'Desalentado'
    WHEN 5 THEN 'Quehaceres domesticos'
    WHEN 6 THEN 'Estudiante'
    WHEN 7 THEN 'Rentista'
    WHEN 8 THEN 'Jubilado/pensionado'
    WHEN 9 THEN 'Discapacitado'
    WHEN 10 THEN 'Anciano'
    WHEN 11 THEN 'Otra actividad'
    WHEN 12 THEN 'Ninguna actividad'
    WHEN 99 THEN 'No declarada'
    ELSE NULL
  END AS condicion_actividad_lbl,
  CASE WHEN condicion_actividad_code = 1 THEN true WHEN condicion_actividad_code IS NOT NULL THEN false ELSE NULL END AS ocupado_bool,
  CASE WHEN condicion_actividad_code IN (2,3) THEN true WHEN condicion_actividad_code IS NOT NULL THEN false ELSE NULL END AS desocupado_bool,
  CASE WHEN condicion_actividad_code IN (5,6,7,8,9,10,11,12) THEN true WHEN condicion_actividad_code IS NOT NULL THEN false ELSE NULL END AS inactivo_bool,
  ocupacion_code,
  rama_actividad_code,
  estado_civil_code,
  anio_nacimiento,
  municipio_nacimiento_code AS provincia_nacimiento_code,
  pais_nacimiento_code,
  hijos_nacidos_vivos,
  hijos_sobrevivientes,
  'zona no incluida en CSV publico 2010 descargado'::text AS nota_comparabilidad
FROM b;

DROP VIEW IF EXISTS armonizado.hogares CASCADE;
CREATE VIEW armonizado.hogares AS
SELECT
  2010::smallint AS anio,
  'CNPV2010'::text AS fuente,
  concat_ws('-', NULLIF(btrim(carpeta),''), NULLIF(btrim(num_vivienda),''), NULLIF(btrim(num_hogar),'')) AS id_hogar,
  concat_ws('-', NULLIF(btrim(carpeta),''), NULLIF(btrim(num_vivienda),'')) AS id_vivienda,
  armonizado.to_int(region) AS region_code,
  armonizado.to_int(provincia) AS provincia_code,
  armonizado.to_int(municipio) AS municipio_code,
  armonizado.to_int(distrito) AS distrito_municipal_code,
  NULL::integer AS seccion_code,
  NULL::integer AS barrio_code,
  NULL::integer AS zona_code,
  NULL::text AS zona_lbl,
  armonizado.to_int(h25a_hombres) AS hombres_total,
  armonizado.to_int(h25b_mujeres) AS mujeres_total,
  armonizado.to_int(h25c_total) AS miembros_total,
  armonizado.to_int(h30_tipo_hogar) AS tipo_hogar_code,
  armonizado.to_int(h10_tenencia) AS tenencia_code,
  armonizado.to_int(h11_dormitorios) AS dormitorios,
  NULL::integer AS cuartos_vivienda,
  armonizado.to_int(h31_hacinamiento) AS hacinamiento_code,
  armonizado.to_int(h32_gradsan) AS saneamiento_code,
  armonizado.to_int(p27_sexo_jefe) AS jefe_sexo_code,
  armonizado.to_int(p29_edad_jefe) AS jefe_edad,
  armonizado.to_int(p38_anoest_jefe) AS jefe_anios_estudio,
  armonizado.yes1(h09a_estufa) AS tiene_estufa,
  armonizado.yes1(h09b_nevera) AS tiene_nevera,
  armonizado.yes1(h09c_lavadora) AS tiene_lavadora,
  armonizado.yes1(h09d_televisor) AS tiene_televisor,
  armonizado.yes1(h09e_radio) AS tiene_radio,
  armonizado.yes1(h09h_computadora) AS tiene_computadora,
  armonizado.yes1(h09i_internet) AS tiene_internet,
  armonizado.yes1(h09o_automovil) AS tiene_automovil,
  armonizado.yes1(h09nn_motor) AS tiene_motor,
  armonizado.yes1(h09m_celular) AS tiene_celular,
  armonizado.yes1(h09l_telefono_fijo) AS tiene_telefono_fijo,
  armonizado.yes1(h09n_aire) AS tiene_aire,
  armonizado.yes1(h09f_cisterna) AS tiene_cisterna,
  armonizado.yes1(h09j_inversor) AS tiene_inversor,
  armonizado.yes1(h09k_planta) AS tiene_planta,
  armonizado.to_int(h12_sanitario) AS sanitario_code,
  armonizado.to_int(h15_procedencia_agua) AS agua_code,
  armonizado.to_int(h14_basura) AS basura_code,
  armonizado.to_int(h16_combustible) AS combustible_code,
  armonizado.to_int(h17_alumbrado) AS alumbrado_code,
  'zona no incluida en CSV publico 2010 descargado'::text AS nota_comparabilidad
FROM raw.hogares;

DROP VIEW IF EXISTS armonizado.viviendas CASCADE;
CREATE VIEW armonizado.viviendas AS
SELECT
  2010::smallint AS anio,
  'CNPV2010'::text AS fuente,
  concat_ws('-', NULLIF(btrim(carpeta),''), NULLIF(btrim(num_vivienda),'')) AS id_vivienda,
  armonizado.to_int(region) AS region_code,
  armonizado.to_int(provincia) AS provincia_code,
  armonizado.to_int(municipio) AS municipio_code,
  armonizado.to_int(distrito) AS distrito_municipal_code,
  NULL::integer AS seccion_code,
  NULL::integer AS barrio_code,
  NULL::integer AS zona_code,
  NULL::text AS zona_lbl,
  armonizado.to_int(v01_tipo_vivienda) AS tipo_vivienda_code,
  armonizado.to_int(v02_cond_ocupacion) AS condicion_ocupacion_code,
  armonizado.to_int(v03_paredes) AS paredes_code,
  armonizado.to_int(v04_techo) AS techo_code,
  armonizado.to_int(v05_piso) AS piso_code,
  armonizado.to_int(v07_cuartos_vivienda) AS cuartos_vivienda,
  armonizado.to_int(v06_cuarto_cocina) AS cuarto_cocina_code,
  armonizado.to_int(v08_num_hogares) AS cantidad_hogares,
  NULL::integer AS personas_total,
  NULL::integer AS calidad_vivienda_code,
  armonizado.yes1(ac07a_aguas_estancadas) AS entorno_aguas_estancadas,
  armonizado.yes1(ac07b_basura) AS entorno_basura,
  armonizado.yes1(ac07c_canada) AS entorno_canada,
  armonizado.yes1(ac07j_ruido_vehiculo) AS entorno_ruido_vehiculo,
  armonizado.yes1(ac07d_pocilga) AS entorno_pocilga_granja,
  armonizado.yes1(ac07e_humo) AS entorno_humo_gases,
  armonizado.yes1(ac07f_desechos) AS entorno_desechos,
  armonizado.yes1(ac07l_ruido_planta) AS entorno_ruido_planta,
  armonizado.yes1(ac07g_embasadora) AS entorno_envasadora,
  NULL::boolean AS entorno_ninguna,
  'zona no incluida en CSV publico 2010 descargado'::text AS nota_comparabilidad
FROM raw.viviendas;

DROP VIEW IF EXISTS armonizado.metadatos_variables CASCADE;
CREATE VIEW armonizado.metadatos_variables AS
SELECT * FROM (VALUES
  ('personas','id_hogar','carpeta + num_vivienda + num_hogar','alta'),
  ('personas','educ_superior_bool','P37_NIVEL = 4','alta'),
  ('personas','ocupado_bool','P45R1_CONDICION_ACTIVIDAD = 1','alta'),
  ('hogares','zona_code','no disponible en CSV publico 2010 descargado','baja'),
  ('hogares','activos_hogar','H09A-H09O, codigos 1=si 2=no','alta'),
  ('viviendas','entorno','AC07A-AC07N, codigos 1=si 2=no','media')
) AS t(tabla, variable, regla, comparabilidad);
