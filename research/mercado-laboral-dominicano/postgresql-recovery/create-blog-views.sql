-- Base destino: censos_linea_tiempo
-- Proposito: vistas listas para articulos del blog sobre CNPV 2002, 2010 y 2022.
-- No modifica raw ni armonizado. Solo crea el esquema blog.

CREATE SCHEMA IF NOT EXISTS blog;

-- ---------------------------------------------------------------------------
-- 1) Vistas base por anio
-- ---------------------------------------------------------------------------

CREATE OR REPLACE VIEW blog.personas_2002 AS SELECT * FROM fdw_2002.personas;
CREATE OR REPLACE VIEW blog.personas_2010 AS SELECT * FROM fdw_2010.personas;
CREATE OR REPLACE VIEW blog.personas_2022 AS SELECT * FROM fdw_2022.personas;

CREATE OR REPLACE VIEW blog.hogares_2002 AS SELECT * FROM fdw_2002.hogares;
CREATE OR REPLACE VIEW blog.hogares_2010 AS SELECT * FROM fdw_2010.hogares;
CREATE OR REPLACE VIEW blog.hogares_2022 AS SELECT * FROM fdw_2022.hogares;

CREATE OR REPLACE VIEW blog.viviendas_2002 AS SELECT * FROM fdw_2002.viviendas;
CREATE OR REPLACE VIEW blog.viviendas_2010 AS SELECT * FROM fdw_2010.viviendas;
CREATE OR REPLACE VIEW blog.viviendas_2022 AS SELECT * FROM fdw_2022.viviendas;

-- ---------------------------------------------------------------------------
-- 2) Vistas base unificadas, con nombres y flags de uso editorial
-- ---------------------------------------------------------------------------

CREATE OR REPLACE VIEW blog.personas_todos AS
SELECT
  anio,
  fuente,
  id_hogar,
  id_vivienda,
  id_persona,
  region_code,
  provincia_code,
  municipio_code,
  distrito_municipal_code,
  seccion_code,
  barrio_code,
  zona_code,
  zona_lbl,
  sexo_code,
  sexo_lbl,
  edad,
  grupo_edad_5,
  grupo_edad_amplio,
  CASE WHEN edad BETWEEN 0 AND 14 THEN true ELSE false END AS es_nino_0_14,
  CASE WHEN edad BETWEEN 15 AND 24 THEN true ELSE false END AS es_joven_15_24,
  CASE WHEN edad BETWEEN 25 AND 64 THEN true ELSE false END AS es_adulto_25_64,
  CASE WHEN edad >= 65 THEN true ELSE false END AS es_mayor_65,
  parentesco_code,
  sabe_leer_bool,
  asiste_actual_bool,
  nivel_educativo_code,
  nivel_educativo_lbl,
  nivel_educativo_grupo,
  secundaria_o_mas_bool,
  educ_superior_bool,
  ultimo_anio_aprobado,
  campo_estudio_code,
  graduado_bool,
  ocupado_bool,
  desocupado_bool,
  inactivo_bool,
  condicion_actividad_code,
  condicion_actividad_lbl,
  ocupacion_code,
  rama_actividad_code,
  estado_civil_code,
  anio_nacimiento,
  hijos_nacidos_vivos,
  hijos_sobrevivientes,
  nota_comparabilidad
FROM armonizado.personas;

CREATE OR REPLACE VIEW blog.hogares_todos AS
SELECT
  anio,
  fuente,
  id_hogar,
  id_vivienda,
  region_code,
  provincia_code,
  municipio_code,
  distrito_municipal_code,
  seccion_code,
  barrio_code,
  zona_code,
  zona_lbl,
  hombres_total,
  mujeres_total,
  miembros_total,
  tipo_hogar_code,
  tenencia_code,
  dormitorios,
  cuartos_vivienda,
  hacinamiento_code,
  saneamiento_code,
  jefe_sexo_code,
  jefe_edad,
  jefe_anios_estudio,
  tiene_estufa,
  tiene_nevera,
  tiene_lavadora,
  tiene_televisor,
  tiene_radio,
  tiene_computadora,
  tiene_internet,
  tiene_automovil,
  tiene_motor,
  tiene_celular,
  tiene_telefono_fijo,
  tiene_aire,
  tiene_cisterna,
  tiene_inversor,
  tiene_planta,
  sanitario_code,
  agua_code,
  basura_code,
  combustible_code,
  alumbrado_code,
  nota_comparabilidad
FROM armonizado.hogares;

CREATE OR REPLACE VIEW blog.viviendas_todos AS
SELECT
  anio,
  fuente,
  id_vivienda,
  region_code,
  provincia_code,
  municipio_code,
  distrito_municipal_code,
  seccion_code,
  barrio_code,
  zona_code,
  zona_lbl,
  tipo_vivienda_code,
  condicion_ocupacion_code,
  paredes_code,
  techo_code,
  piso_code,
  cuartos_vivienda,
  cuarto_cocina_code,
  cantidad_hogares,
  personas_total,
  calidad_vivienda_code,
  entorno_aguas_estancadas,
  entorno_basura,
  entorno_canada,
  entorno_ruido_vehiculo,
  entorno_pocilga_granja,
  entorno_humo_gases,
  entorno_desechos,
  entorno_ruido_planta,
  entorno_envasadora,
  entorno_ninguna,
  nota_comparabilidad
FROM armonizado.viviendas;

-- ---------------------------------------------------------------------------
-- 3) Vistas de indicadores para articulos
-- ---------------------------------------------------------------------------

CREATE OR REPLACE VIEW blog.resumen_conteos AS
SELECT tabla, anio, filas, validado_en
FROM armonizado.validacion_conteos;

CREATE OR REPLACE VIEW blog.indicadores_nacionales AS
SELECT
  anio,
  COUNT(*) AS poblacion,
  COUNT(*) FILTER (WHERE sexo_code = 1) AS hombres,
  COUNT(*) FILTER (WHERE sexo_code = 2) AS mujeres,
  ROUND(100.0 * COUNT(*) FILTER (WHERE sexo_code = 1) / NULLIF(COUNT(*), 0), 2) AS pct_hombres,
  ROUND(100.0 * COUNT(*) FILTER (WHERE sexo_code = 2) / NULLIF(COUNT(*), 0), 2) AS pct_mujeres,
  COUNT(*) FILTER (WHERE edad BETWEEN 0 AND 14) AS poblacion_0_14,
  COUNT(*) FILTER (WHERE edad BETWEEN 15 AND 64) AS poblacion_15_64,
  COUNT(*) FILTER (WHERE edad >= 65) AS poblacion_65_mas,
  ROUND(100.0 * COUNT(*) FILTER (WHERE edad >= 65) / NULLIF(COUNT(*), 0), 2) AS pct_65_mas,
  ROUND(100.0 * COUNT(*) FILTER (WHERE sabe_leer_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE sabe_leer_bool IS NOT NULL), 0), 2) AS pct_sabe_leer_validos,
  ROUND(100.0 * COUNT(*) FILTER (WHERE secundaria_o_mas_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE secundaria_o_mas_bool IS NOT NULL), 0), 2) AS pct_secundaria_o_mas_validos,
  ROUND(100.0 * COUNT(*) FILTER (WHERE educ_superior_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE educ_superior_bool IS NOT NULL), 0), 2) AS pct_educ_superior_validos,
  ROUND(100.0 * COUNT(*) FILTER (WHERE ocupado_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE ocupado_bool IS NOT NULL), 0), 2) AS pct_ocupados_validos
FROM blog.personas_todos
GROUP BY anio;

CREATE OR REPLACE VIEW blog.indicadores_provincia AS
SELECT
  anio,
  provincia_code,
  COUNT(*) AS poblacion,
  ROUND(100.0 * COUNT(*) FILTER (WHERE sexo_code = 2) / NULLIF(COUNT(*), 0), 2) AS pct_mujeres,
  ROUND(100.0 * COUNT(*) FILTER (WHERE edad BETWEEN 0 AND 14) / NULLIF(COUNT(*), 0), 2) AS pct_0_14,
  ROUND(100.0 * COUNT(*) FILTER (WHERE edad >= 65) / NULLIF(COUNT(*), 0), 2) AS pct_65_mas,
  ROUND(100.0 * COUNT(*) FILTER (WHERE sabe_leer_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE sabe_leer_bool IS NOT NULL), 0), 2) AS pct_sabe_leer_validos,
  ROUND(100.0 * COUNT(*) FILTER (WHERE educ_superior_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE educ_superior_bool IS NOT NULL), 0), 2) AS pct_educ_superior_validos,
  ROUND(100.0 * COUNT(*) FILTER (WHERE ocupado_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE ocupado_bool IS NOT NULL), 0), 2) AS pct_ocupados_validos
FROM blog.personas_todos
GROUP BY anio, provincia_code;

CREATE OR REPLACE VIEW blog.educacion_por_sexo_edad AS
SELECT
  anio,
  sexo_lbl,
  grupo_edad_amplio,
  COUNT(*) AS personas,
  ROUND(100.0 * COUNT(*) FILTER (WHERE sabe_leer_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE sabe_leer_bool IS NOT NULL), 0), 2) AS pct_sabe_leer_validos,
  ROUND(100.0 * COUNT(*) FILTER (WHERE secundaria_o_mas_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE secundaria_o_mas_bool IS NOT NULL), 0), 2) AS pct_secundaria_o_mas_validos,
  ROUND(100.0 * COUNT(*) FILTER (WHERE educ_superior_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE educ_superior_bool IS NOT NULL), 0), 2) AS pct_educ_superior_validos
FROM blog.personas_todos
GROUP BY anio, sexo_lbl, grupo_edad_amplio;

CREATE OR REPLACE VIEW blog.trabajo_por_sexo_edad AS
SELECT
  anio,
  sexo_lbl,
  grupo_edad_amplio,
  COUNT(*) AS personas,
  COUNT(*) FILTER (WHERE ocupado_bool IS NOT NULL OR desocupado_bool IS NOT NULL OR inactivo_bool IS NOT NULL) AS personas_validas_trabajo,
  ROUND(100.0 * COUNT(*) FILTER (WHERE ocupado_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE ocupado_bool IS NOT NULL), 0), 2) AS pct_ocupados_validos,
  ROUND(100.0 * COUNT(*) FILTER (WHERE desocupado_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE desocupado_bool IS NOT NULL), 0), 2) AS pct_desocupados_validos,
  ROUND(100.0 * COUNT(*) FILTER (WHERE inactivo_bool IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE inactivo_bool IS NOT NULL), 0), 2) AS pct_inactivos_validos
FROM blog.personas_todos
GROUP BY anio, sexo_lbl, grupo_edad_amplio;

CREATE OR REPLACE VIEW blog.activos_hogar_nacional AS
SELECT
  anio,
  COUNT(*) AS hogares,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_internet IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_internet IS NOT NULL), 0), 2) AS pct_internet,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_computadora IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_computadora IS NOT NULL), 0), 2) AS pct_computadora,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_celular IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_celular IS NOT NULL), 0), 2) AS pct_celular,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_automovil IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_automovil IS NOT NULL), 0), 2) AS pct_automovil,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_motor IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_motor IS NOT NULL), 0), 2) AS pct_motor,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_nevera IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_nevera IS NOT NULL), 0), 2) AS pct_nevera,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_lavadora IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_lavadora IS NOT NULL), 0), 2) AS pct_lavadora
FROM blog.hogares_todos
GROUP BY anio;

CREATE OR REPLACE VIEW blog.activos_hogar_provincia AS
SELECT
  anio,
  provincia_code,
  COUNT(*) AS hogares,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_internet IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_internet IS NOT NULL), 0), 2) AS pct_internet,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_computadora IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_computadora IS NOT NULL), 0), 2) AS pct_computadora,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_celular IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_celular IS NOT NULL), 0), 2) AS pct_celular,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_automovil IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_automovil IS NOT NULL), 0), 2) AS pct_automovil,
  ROUND(100.0 * COUNT(*) FILTER (WHERE tiene_motor IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE tiene_motor IS NOT NULL), 0), 2) AS pct_motor
FROM blog.hogares_todos
GROUP BY anio, provincia_code;

CREATE OR REPLACE VIEW blog.vivienda_materiales_provincia AS
SELECT
  anio,
  provincia_code,
  paredes_code,
  techo_code,
  piso_code,
  COUNT(*) AS viviendas
FROM blog.viviendas_todos
GROUP BY anio, provincia_code, paredes_code, techo_code, piso_code;

CREATE OR REPLACE VIEW blog.vivienda_entorno_provincia AS
SELECT
  anio,
  provincia_code,
  COUNT(*) AS viviendas,
  ROUND(100.0 * COUNT(*) FILTER (WHERE entorno_aguas_estancadas IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE entorno_aguas_estancadas IS NOT NULL), 0), 2) AS pct_aguas_estancadas,
  ROUND(100.0 * COUNT(*) FILTER (WHERE entorno_basura IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE entorno_basura IS NOT NULL), 0), 2) AS pct_basura,
  ROUND(100.0 * COUNT(*) FILTER (WHERE entorno_canada IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE entorno_canada IS NOT NULL), 0), 2) AS pct_canada,
  ROUND(100.0 * COUNT(*) FILTER (WHERE entorno_ruido_vehiculo IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE entorno_ruido_vehiculo IS NOT NULL), 0), 2) AS pct_ruido_vehiculo,
  ROUND(100.0 * COUNT(*) FILTER (WHERE entorno_humo_gases IS TRUE)
    / NULLIF(COUNT(*) FILTER (WHERE entorno_humo_gases IS NOT NULL), 0), 2) AS pct_humo_gases
FROM blog.viviendas_todos
GROUP BY anio, provincia_code;

CREATE OR REPLACE VIEW blog.comparabilidad AS
SELECT *
FROM armonizado.metadatos_variables;

-- ---------------------------------------------------------------------------
-- 4) Catalogo de vistas para uso editorial
-- ---------------------------------------------------------------------------

CREATE OR REPLACE VIEW blog.catalogo_vistas AS
SELECT * FROM (VALUES
  ('personas_todos', 'Microdato armonizado de personas para 2002, 2010 y 2022. Usar filtros por anio para consultas grandes.'),
  ('hogares_todos', 'Microdato armonizado de hogares: servicios, activos, miembros y condiciones basicas.'),
  ('viviendas_todos', 'Microdato armonizado de viviendas: materiales, condicion de ocupacion y entorno cuando existe.'),
  ('indicadores_nacionales', 'Resumen nacional por anio: sexo, edad, alfabetismo, secundaria, superior y ocupacion.'),
  ('indicadores_provincia', 'Resumen por provincia y anio para mapas y rankings territoriales.'),
  ('educacion_por_sexo_edad', 'Educacion por sexo y grupo de edad: base para articulos de cambio generacional y brechas.'),
  ('trabajo_por_sexo_edad', 'Ocupacion, desocupacion e inactividad por sexo y grupo de edad.'),
  ('activos_hogar_nacional', 'Brecha material/digital de hogares por anio.'),
  ('activos_hogar_provincia', 'Brecha material/digital de hogares por provincia y anio.'),
  ('vivienda_materiales_provincia', 'Materiales de pared, techo y piso por provincia y anio.'),
  ('vivienda_entorno_provincia', 'Problemas de entorno de vivienda por provincia y anio, disponible sobre todo 2002/2010.'),
  ('comparabilidad', 'Notas de comparabilidad y reglas de variables armonizadas.')
) AS t(vista, uso_editorial);

