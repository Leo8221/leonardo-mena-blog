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
    armonizado.to_int(region) AS region_code,
    armonizado.to_int(provincia) AS provincia_code,
    armonizado.to_int(municipio) AS municipio_code,
    armonizado.to_int(zona) AS zona_code,
    armonizado.to_int(seccion) AS seccion_code,
    armonizado.to_int(barrio) AS barrio_code,
    armonizado.to_int(p28_sexo) AS sexo_code,
    armonizado.to_int(p30_edad) AS edad,
    armonizado.to_int(p27_parentesco) AS parentesco_code,
    armonizado.to_int(p37_sabe_leer) AS sabe_leer_code,
    armonizado.to_int(p38_asiste) AS asiste_code,
    armonizado.to_int(p40a_nivel) AS nivel_educativo_code,
    armonizado.to_int(p40b_ano_estudio) AS ultimo_anio_aprobado,
    NULLIF(btrim(p42a_carrera),'') AS campo_estudio_code,
    armonizado.to_int(p41_finalizo) AS graduado_code,
    armonizado.to_int(p46_tuvo_empleo) AS tuvo_empleo_code,
    armonizado.to_int(p47_trabajo) AS trabajo_pago_code,
    armonizado.to_int(p48_ayudo) AS ayudo_sin_pago_code,
    armonizado.to_int(p55_busco_trab) AS busco_trabajo_code,
    armonizado.to_int(p49_trab_antes) AS trabajo_antes_code,
    armonizado.to_int(p46_condicion_actividad) AS condicion_actividad_code,
    NULLIF(btrim(p50_ocupacion),'') AS ocupacion_code,
    NULLIF(btrim(p54_rama_actividad),'') AS rama_actividad_code,
    armonizado.to_int(p60_estado_civil) AS estado_civil_code,
    armonizado.to_int(p29c_ano_nace) AS anio_nacimiento,
    NULLIF(btrim(p31b_provincia),'') AS provincia_nacimiento_code,
    NULLIF(btrim(p32b_pais_nace),'') AS pais_nacimiento_code,
    armonizado.to_int(p64_total) AS hijos_nacidos_vivos,
    armonizado.to_int(p65_total) AS hijos_sobrevivientes
  FROM raw.personas
)
SELECT
  2002::smallint AS anio,
  'CNPV2002'::text AS fuente,
  NULL::text AS id_hogar,
  NULL::text AS id_vivienda,
  NULL::text AS id_persona,
  region_code,
  provincia_code,
  municipio_code,
  NULL::integer AS distrito_municipal_code,
  seccion_code,
  barrio_code,
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
  CASE asiste_code WHEN 1 THEN true WHEN 2 THEN false WHEN 3 THEN false WHEN 4 THEN false ELSE NULL END AS asiste_actual_bool,
  nivel_educativo_code,
  CASE nivel_educativo_code
    WHEN 1 THEN 'Ninguno'
    WHEN 2 THEN 'Preescolar'
    WHEN 3 THEN 'Primario'
    WHEN 4 THEN 'Secundario'
    WHEN 5 THEN 'Universitario'
    WHEN 6 THEN 'Especialidad'
    WHEN 7 THEN 'Maestria'
    WHEN 8 THEN 'Doctorado'
    WHEN 9 THEN 'No sabe'
    ELSE NULL
  END AS nivel_educativo_lbl,
  CASE
    WHEN nivel_educativo_code = 1 THEN 'Ninguno'
    WHEN nivel_educativo_code = 2 THEN 'Preprimaria'
    WHEN nivel_educativo_code = 3 THEN 'Primaria'
    WHEN nivel_educativo_code = 4 THEN 'Secundaria'
    WHEN nivel_educativo_code = 5 THEN 'Superior'
    WHEN nivel_educativo_code IN (6,7,8) THEN 'Postgrado'
    WHEN nivel_educativo_code = 9 THEN 'No sabe'
    ELSE NULL
  END AS nivel_educativo_grupo,
  CASE WHEN nivel_educativo_code IN (5,6,7,8) THEN true WHEN nivel_educativo_code IN (1,2,3,4,9) THEN false ELSE NULL END AS educ_superior_bool,
  CASE WHEN nivel_educativo_code IN (4,5,6,7,8) THEN true WHEN nivel_educativo_code IN (1,2,3,9) THEN false ELSE NULL END AS secundaria_o_mas_bool,
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
    WHEN 2 THEN 'Desempleado'
    WHEN 3 THEN 'Busca trabajo primera vez'
    WHEN 4 THEN 'Desalentado'
    WHEN 5 THEN 'Quehaceres domesticos'
    WHEN 6 THEN 'Estudiante'
    WHEN 7 THEN 'Rentista'
    WHEN 8 THEN 'Jubilado/pensionado'
    WHEN 9 THEN 'Discapacitado/anciano'
    WHEN 10 THEN 'Ninguna actividad'
    WHEN 11 THEN 'Otra actividad'
    WHEN 99 THEN 'No declarada'
    ELSE NULL
  END AS condicion_actividad_lbl,
  CASE WHEN condicion_actividad_code = 1 THEN true WHEN condicion_actividad_code IS NOT NULL THEN false ELSE NULL END AS ocupado_bool,
  CASE WHEN condicion_actividad_code IN (2,3) THEN true WHEN condicion_actividad_code IS NOT NULL THEN false ELSE NULL END AS desocupado_bool,
  CASE WHEN condicion_actividad_code IN (5,6,7,8,9,10,11) THEN true WHEN condicion_actividad_code IS NOT NULL THEN false ELSE NULL END AS inactivo_bool,
  ocupacion_code,
  rama_actividad_code,
  estado_civil_code,
  anio_nacimiento,
  provincia_nacimiento_code,
  pais_nacimiento_code,
  hijos_nacidos_vivos,
  hijos_sobrevivientes,
  'sin llave hogar-persona en archivo publico 2002'::text AS nota_comparabilidad
FROM b;

DROP VIEW IF EXISTS armonizado.hogares CASCADE;
CREATE VIEW armonizado.hogares AS
SELECT
  2002::smallint AS anio,
  'CNPV2002'::text AS fuente,
  NULL::text AS id_hogar,
  NULL::text AS id_vivienda,
  armonizado.to_int(region) AS region_code,
  armonizado.to_int(provincia) AS provincia_code,
  armonizado.to_int(municipio) AS municipio_code,
  NULL::integer AS distrito_municipal_code,
  armonizado.to_int(seccion) AS seccion_code,
  armonizado.to_int(barrio) AS barrio_code,
  armonizado.to_int(zona) AS zona_code,
  CASE armonizado.to_int(zona) WHEN 1 THEN 'Urbano' WHEN 2 THEN 'Rural' ELSE NULL END AS zona_lbl,
  armonizado.to_int(h22a_varones) AS hombres_total,
  armonizado.to_int(h22b_hembras) AS mujeres_total,
  armonizado.to_int(h22c_total) AS miembros_total,
  armonizado.to_int(h00_tipo_hogar) AS tipo_hogar_code,
  armonizado.to_int(h11_tenencia) AS tenencia_code,
  armonizado.to_int(h12_cuartos) AS dormitorios,
  NULL::integer AS cuartos_vivienda,
  armonizado.to_int(hacinamiento) AS hacinamiento_code,
  armonizado.to_int(gradsan) AS saneamiento_code,
  NULL::integer AS jefe_sexo_code,
  NULL::integer AS jefe_edad,
  armonizado.to_int(p40b_anoestjefe) AS jefe_anios_estudio,
  armonizado.yes1(h19b_estufa) AS tiene_estufa,
  armonizado.yes1(h19a_nevera) AS tiene_nevera,
  armonizado.yes1(h19c_lavadora) AS tiene_lavadora,
  armonizado.yes1(h19d_televisor) AS tiene_televisor,
  armonizado.yes1(h19f_radio) AS tiene_radio,
  armonizado.yes1(h19i_computadora) AS tiene_computadora,
  armonizado.yes1(h19m_internet) AS tiene_internet,
  armonizado.yes1(h19g_automovil) AS tiene_automovil,
  NULL::boolean AS tiene_motor,
  NULL::boolean AS tiene_celular,
  armonizado.yes1(h19l_telefono) AS tiene_telefono_fijo,
  armonizado.yes1(h19e_aire) AS tiene_aire,
  armonizado.yes1(h19h_cisterna) AS tiene_cisterna,
  armonizado.yes1(h19j_inversor) AS tiene_inversor,
  armonizado.yes1(h19k_planta) AS tiene_planta,
  armonizado.to_int(h16_sanitario) AS sanitario_code,
  armonizado.to_int(h15_abastecimiento_agua) AS agua_code,
  armonizado.to_int(h18_basura) AS basura_code,
  armonizado.to_int(h13_combustible) AS combustible_code,
  armonizado.to_int(h14_alumbrado) AS alumbrado_code,
  'sin llave hogar-persona en archivo publico 2002'::text AS nota_comparabilidad
FROM raw.hogares;

DROP VIEW IF EXISTS armonizado.viviendas CASCADE;
CREATE VIEW armonizado.viviendas AS
SELECT
  2002::smallint AS anio,
  'CNPV2002'::text AS fuente,
  NULL::text AS id_vivienda,
  armonizado.to_int(region) AS region_code,
  armonizado.to_int(provincia) AS provincia_code,
  armonizado.to_int(municipio) AS municipio_code,
  NULL::integer AS distrito_municipal_code,
  armonizado.to_int(seccion) AS seccion_code,
  armonizado.to_int(barrio) AS barrio_code,
  armonizado.to_int(zona) AS zona_code,
  CASE armonizado.to_int(zona) WHEN 1 THEN 'Urbano' WHEN 2 THEN 'Rural' ELSE NULL END AS zona_lbl,
  armonizado.to_int(v01_tipo_vivienda) AS tipo_vivienda_code,
  armonizado.to_int(v03_condicion_ocupacion) AS condicion_ocupacion_code,
  armonizado.to_int(v04_paredes) AS paredes_code,
  armonizado.to_int(v05_techo) AS techo_code,
  armonizado.to_int(v06_piso) AS piso_code,
  armonizado.to_int(v07_cuartos_vivienda) AS cuartos_vivienda,
  armonizado.to_int(v08_cuarto_cocina) AS cuarto_cocina_code,
  armonizado.to_int(v10_cantidad_hogares) AS cantidad_hogares,
  armonizado.to_int(v22c_total) AS personas_total,
  armonizado.to_int(calcasa) AS calidad_vivienda_code,
  armonizado.yes1(v09a_aguas_estancadas) AS entorno_aguas_estancadas,
  armonizado.yes1(v09b_acumulacion_basura) AS entorno_basura,
  armonizado.yes1(v09c_canada_basura) AS entorno_canada,
  armonizado.yes1(v09d_ruido_vehiculo) AS entorno_ruido_vehiculo,
  armonizado.yes1(v09e_pocilga_granja) AS entorno_pocilga_granja,
  armonizado.yes1(v09f_humo_gases) AS entorno_humo_gases,
  armonizado.yes1(v09h_desechos) AS entorno_desechos,
  armonizado.yes1(v09i_ruidos_planta) AS entorno_ruido_planta,
  armonizado.yes1(v09j_envasadora) AS entorno_envasadora,
  armonizado.yes1(v09m_ninguna) AS entorno_ninguna,
  'sin llave vivienda-persona en archivo publico 2002'::text AS nota_comparabilidad
FROM raw.viviendas;

DROP VIEW IF EXISTS armonizado.metadatos_variables CASCADE;
CREATE VIEW armonizado.metadatos_variables AS
SELECT * FROM (VALUES
  ('personas','id_hogar','no disponible en microdato publico 2002','baja'),
  ('personas','educ_superior_bool','P40A_NIVEL in 5,6,7,8','alta'),
  ('personas','ocupado_bool','P46_CONDICION_ACTIVIDAD = 1','alta'),
  ('hogares','activos_hogar','H19A-H19M, codigos 1=si 2=no','alta'),
  ('viviendas','entorno','V09A-V09M, codigos 1=si 2=no','media')
) AS t(tabla, variable, regla, comparabilidad);
