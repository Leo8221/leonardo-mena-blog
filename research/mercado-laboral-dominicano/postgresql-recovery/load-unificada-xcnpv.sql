\set ON_ERROR_STOP on

SET statement_timeout = 0;
SET work_mem = '256MB';
SET maintenance_work_mem = '1GB';
SET max_parallel_maintenance_workers = 4;

-- Ejecutar en censo_2022 con:
-- psql -v unificada_csv="D:/datos_one_censos/2022/BD_FINAL_VIVIENDA_HOGAR_PERSONA_XCNPV_PUB.csv" -f load-unificada-xcnpv.sql
--
-- La publicacion separada de personas elimina las llaves operativas de vivienda.
-- La base unificada oficial conserva cada hogar como un bloque contiguo y ordena
-- sus miembros por P25_ORDEN. Se genera hogar_id como el acumulado de P25_ORDEN=1.
-- Las validaciones al final impiden publicar la capa si esa propiedad no se cumple.

CREATE SCHEMA IF NOT EXISTS staging;
CREATE SCHEMA IF NOT EXISTS analitica;

DROP TABLE IF EXISTS staging.unificada_import;
CREATE UNLOGGED TABLE staging.unificada_import (
  fila_origen bigint GENERATED ALWAYS AS IDENTITY,
  phogar text, region text, provincia text, municipio text, dmunicipal text,
  zona text, dia_empad text, mes_empad text, ano_empad text, tiporeg text,
  p01 text, p02 text, p03 text, p04 text, p05 text, p06 text, p07 text,
  p08 text, p09 text, p10 text, p11 text, p12 text, p13 text, p14 text,
  p15a text, p15b text, p15c text, p15d text, p15e text, p15f text,
  p15g text, p15h text, p15i text, p15j text, p15k text, p15l text,
  p15m text, p15n text, p15o text, p15p text, p15q text, p15r text,
  p16 text, p17 text, p18 text, p19 text, p20 text, p21 text, p24 text,
  p25_orden text, p26_sexo text, p27_edad text, p28_parent text,
  p29_padre text, p30_madre text, p34_dia text, p34_mes text, p34_ano text,
  p40_1 text, p40_2 text, p40_3 text, p40_4 text, p40_5 text, p40_6 text,
  p41 text, p42 text, p43 text, p44 text, p45_code text, p46 text, p47 text,
  p48a text, p48b text, p48c text, p48d text, p49 text, p53 text, p54 text,
  p55 text, p56 text, p57 text, p58 text, p59 text, p60_code text, p61 text,
  p62_code text, p63 text, p64 text, p65 text, p65a text, p65b text,
  p66 text, p66a text, p66b text, p67_mes text, p67_ano text
);

COPY staging.unificada_import (phogar, region, provincia, municipio, dmunicipal, zona, dia_empad, mes_empad, ano_empad, tiporeg, p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12, p13, p14, p15a, p15b, p15c, p15d, p15e, p15f, p15g, p15h, p15i, p15j, p15k, p15l, p15m, p15n, p15o, p15p, p15q, p15r, p16, p17, p18, p19, p20, p21, p24, p25_orden, p26_sexo, p27_edad, p28_parent, p29_padre, p30_madre, p34_dia, p34_mes, p34_ano, p40_1, p40_2, p40_3, p40_4, p40_5, p40_6, p41, p42, p43, p44, p45_code, p46, p47, p48a, p48b, p48c, p48d, p49, p53, p54, p55, p56, p57, p58, p59, p60_code, p61, p62_code, p63, p64, p65, p65a, p65b, p66, p66a, p66b, p67_mes, p67_ano) FROM :'unificada_csv' WITH (FORMAT csv, HEADER true, NULL '', QUOTE '"', ESCAPE '"');

DROP TABLE IF EXISTS analitica.xcnpv_unificada CASCADE;
CREATE TABLE analitica.xcnpv_unificada AS
SELECT
  fila_origen,
  SUM(CASE WHEN NULLIF(BTRIM(p25_orden), '')::integer = 1 THEN 1 ELSE 0 END)
    OVER (ORDER BY fila_origen ROWS UNBOUNDED PRECEDING)::bigint AS hogar_id,
  NULLIF(BTRIM(phogar), '')::integer AS phogar,
  NULLIF(BTRIM(region), '')::integer AS region,
  NULLIF(BTRIM(provincia), '')::integer AS provincia,
  NULLIF(BTRIM(municipio), '')::integer AS municipio,
  NULLIF(BTRIM(dmunicipal), '')::integer AS dmunicipal,
  NULLIF(BTRIM(zona), '')::integer AS zona,
  NULLIF(BTRIM(dia_empad), '')::integer AS dia_empad,
  NULLIF(BTRIM(mes_empad), '')::integer AS mes_empad,
  NULLIF(BTRIM(ano_empad), '')::integer AS ano_empad,
  NULLIF(BTRIM(tiporeg), '')::integer AS tiporeg,
  NULLIF(BTRIM(p01), '')::integer AS p01,
  NULLIF(BTRIM(p02), '')::integer AS p02,
  NULLIF(BTRIM(p03), '')::integer AS p03,
  NULLIF(BTRIM(p04), '')::integer AS p04,
  NULLIF(BTRIM(p05), '')::integer AS p05,
  NULLIF(BTRIM(p06), '')::integer AS p06,
  NULLIF(BTRIM(p07), '')::integer AS p07,
  NULLIF(BTRIM(p08), '')::integer AS p08,
  NULLIF(BTRIM(p09), '')::integer AS p09,
  NULLIF(BTRIM(p10), '')::integer AS p10,
  NULLIF(BTRIM(p11), '')::integer AS p11,
  NULLIF(BTRIM(p12), '')::integer AS p12,
  NULLIF(BTRIM(p13), '')::integer AS p13,
  NULLIF(BTRIM(p14), '')::integer AS p14,
  NULLIF(BTRIM(p15a), '')::integer AS p15a,
  NULLIF(BTRIM(p15b), '')::integer AS p15b,
  NULLIF(BTRIM(p15c), '')::integer AS p15c,
  NULLIF(BTRIM(p15d), '')::integer AS p15d,
  NULLIF(BTRIM(p15e), '')::integer AS p15e,
  NULLIF(BTRIM(p15f), '')::integer AS p15f,
  NULLIF(BTRIM(p15g), '')::integer AS p15g,
  NULLIF(BTRIM(p15h), '')::integer AS p15h,
  NULLIF(BTRIM(p15i), '')::integer AS p15i,
  NULLIF(BTRIM(p15j), '')::integer AS p15j,
  NULLIF(BTRIM(p15k), '')::integer AS p15k,
  NULLIF(BTRIM(p15l), '')::integer AS p15l,
  NULLIF(BTRIM(p15m), '')::integer AS p15m,
  NULLIF(BTRIM(p15n), '')::integer AS p15n,
  NULLIF(BTRIM(p15o), '')::integer AS p15o,
  NULLIF(BTRIM(p15p), '')::integer AS p15p,
  NULLIF(BTRIM(p15q), '')::integer AS p15q,
  NULLIF(BTRIM(p15r), '')::integer AS p15r,
  NULLIF(BTRIM(p16), '')::integer AS p16,
  NULLIF(BTRIM(p17), '')::integer AS p17,
  NULLIF(BTRIM(p18), '')::integer AS p18,
  NULLIF(BTRIM(p19), '')::integer AS p19,
  NULLIF(BTRIM(p20), '')::integer AS p20,
  NULLIF(BTRIM(p21), '')::integer AS p21,
  NULLIF(BTRIM(p24), '')::integer AS p24,
  NULLIF(BTRIM(p25_orden), '')::integer AS p25_orden,
  NULLIF(BTRIM(p26_sexo), '')::integer AS p26_sexo,
  NULLIF(BTRIM(p27_edad), '')::integer AS p27_edad,
  NULLIF(BTRIM(p28_parent), '')::integer AS p28_parent,
  NULLIF(BTRIM(p29_padre), '')::integer AS p29_padre,
  NULLIF(BTRIM(p30_madre), '')::integer AS p30_madre,
  NULLIF(BTRIM(p34_dia), '')::integer AS p34_dia,
  NULLIF(BTRIM(p34_mes), '')::integer AS p34_mes,
  NULLIF(BTRIM(p34_ano), '')::integer AS p34_ano,
  NULLIF(BTRIM(p40_1), '')::integer AS p40_1,
  NULLIF(BTRIM(p40_2), '')::integer AS p40_2,
  NULLIF(BTRIM(p40_3), '')::integer AS p40_3,
  NULLIF(BTRIM(p40_4), '')::integer AS p40_4,
  NULLIF(BTRIM(p40_5), '')::integer AS p40_5,
  NULLIF(BTRIM(p40_6), '')::integer AS p40_6,
  NULLIF(BTRIM(p41), '')::integer AS p41,
  NULLIF(BTRIM(p42), '')::integer AS p42,
  NULLIF(BTRIM(p43), '')::integer AS p43,
  NULLIF(BTRIM(p44), '')::integer AS p44,
  NULLIF(BTRIM(p45_code), '')::integer AS p45_code,
  NULLIF(BTRIM(p46), '')::integer AS p46,
  NULLIF(BTRIM(p47), '')::integer AS p47,
  NULLIF(BTRIM(p48a), '')::integer AS p48a,
  NULLIF(BTRIM(p48b), '')::integer AS p48b,
  NULLIF(BTRIM(p48c), '')::integer AS p48c,
  NULLIF(BTRIM(p48d), '')::integer AS p48d,
  NULLIF(BTRIM(p49), '')::integer AS p49,
  NULLIF(BTRIM(p53), '')::integer AS p53,
  NULLIF(BTRIM(p54), '')::integer AS p54,
  NULLIF(BTRIM(p55), '')::integer AS p55,
  NULLIF(BTRIM(p56), '')::integer AS p56,
  NULLIF(BTRIM(p57), '')::integer AS p57,
  NULLIF(BTRIM(p58), '')::integer AS p58,
  NULLIF(BTRIM(p59), '')::integer AS p59,
  NULLIF(BTRIM(p60_code), '')::integer AS p60_code,
  NULLIF(BTRIM(p61), '')::integer AS p61,
  NULLIF(BTRIM(p62_code), '')::integer AS p62_code,
  NULLIF(BTRIM(p63), '')::integer AS p63,
  NULLIF(BTRIM(p64), '')::integer AS p64,
  NULLIF(BTRIM(p65), '')::integer AS p65,
  NULLIF(BTRIM(p65a), '')::integer AS p65a,
  NULLIF(BTRIM(p65b), '')::integer AS p65b,
  NULLIF(BTRIM(p66), '')::integer AS p66,
  NULLIF(BTRIM(p66a), '')::integer AS p66a,
  NULLIF(BTRIM(p66b), '')::integer AS p66b,
  NULLIF(BTRIM(p67_mes), '')::integer AS p67_mes,
  NULLIF(BTRIM(p67_ano), '')::integer AS p67_ano
FROM staging.unificada_import;

\ir finalize-unificada-xcnpv.sql
