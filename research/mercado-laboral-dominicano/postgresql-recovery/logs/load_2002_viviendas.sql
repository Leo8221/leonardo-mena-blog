\set ON_ERROR_STOP on
DROP TABLE IF EXISTS raw.viviendas;
CREATE TABLE raw.viviendas (
  "region" text,
  "provincia" text,
  "municipio" text,
  "zona" text,
  "seccion" text,
  "barrio" text,
  "v01_tipo_vivienda" text,
  "v02_acceso_vivienda" text,
  "v03_condicion_ocupacion" text,
  "v04_paredes" text,
  "v05_techo" text,
  "v06_piso" text,
  "v07_cuartos_vivienda" text,
  "v08_cuarto_cocina" text,
  "v09a_aguas_estancadas" text,
  "v09b_acumulacion_basura" text,
  "v09c_canada_basura" text,
  "v09d_ruido_vehiculo" text,
  "v09e_pocilga_granja" text,
  "v09f_humo_gases" text,
  "v09g_ruidos" text,
  "v09h_desechos" text,
  "v09i_ruidos_planta" text,
  "v09j_envasadora" text,
  "v09k_bomba" text,
  "v09l_musica" text,
  "v09m_ninguna" text,
  "v10_cantidad_hogares" text,
  "v22a_varones" text,
  "v22b_hembras" text,
  "v22c_total" text,
  "calcasa" text
);
\copy raw.viviendas ("region", "provincia", "municipio", "zona", "seccion", "barrio", "v01_tipo_vivienda", "v02_acceso_vivienda", "v03_condicion_ocupacion", "v04_paredes", "v05_techo", "v06_piso", "v07_cuartos_vivienda", "v08_cuarto_cocina", "v09a_aguas_estancadas", "v09b_acumulacion_basura", "v09c_canada_basura", "v09d_ruido_vehiculo", "v09e_pocilga_granja", "v09f_humo_gases", "v09g_ruidos", "v09h_desechos", "v09i_ruidos_planta", "v09j_envasadora", "v09k_bomba", "v09l_musica", "v09m_ninguna", "v10_cantidad_hogares", "v22a_varones", "v22b_hembras", "v22c_total", "calcasa") FROM 'D:/datos_one_censos/2002/CENSO2002RD-VIVIENDAS.csv' WITH (FORMAT csv, HEADER true, NULL '', QUOTE '"', ESCAPE '"');
ANALYZE raw.viviendas;
