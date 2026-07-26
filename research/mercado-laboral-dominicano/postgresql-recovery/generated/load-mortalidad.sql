\set ON_ERROR_STOP on
DROP TABLE IF EXISTS raw.mortalidad CASCADE;
CREATE TABLE raw.mortalidad (
  "phogar" text,
  "region" text,
  "provincia" text,
  "municipio" text,
  "dmunicipal" text,
  "zona" text,
  "dia_empad" text,
  "mes_empad" text,
  "ano_empad" text,
  "resultado_final" text,
  "p24_orden" text,
  "p24a" text,
  "p24b_dia" text,
  "p24b_mes" text,
  "p24b_ano" text,
  "p24c" text,
  "p24d" text
);
\copy raw.mortalidad ("phogar", "region", "provincia", "municipio", "dmunicipal", "zona", "dia_empad", "mes_empad", "ano_empad", "resultado_final", "p24_orden", "p24a", "p24b_dia", "p24b_mes", "p24b_ano", "p24c", "p24d") FROM 'D:/datos_one_censos/2022/BD_Mortalidad_XCNPV.csv' WITH (FORMAT csv, HEADER true, NULL '', QUOTE '"', ESCAPE '"');
ANALYZE raw.mortalidad;
DROP TABLE IF EXISTS public.mortalidad CASCADE;
CREATE TABLE public.mortalidad AS
SELECT
  NULLIF(BTRIM("phogar"), '')::integer AS "phogar",
  NULLIF(BTRIM("region"), '')::integer AS "region",
  NULLIF(BTRIM("provincia"), '')::integer AS "provincia",
  NULLIF(BTRIM("municipio"), '')::integer AS "municipio",
  NULLIF(BTRIM("dmunicipal"), '')::integer AS "dmunicipal",
  NULLIF(BTRIM("zona"), '')::integer AS "zona",
  NULLIF(BTRIM("dia_empad"), '')::integer AS "dia_empad",
  NULLIF(BTRIM("mes_empad"), '')::integer AS "mes_empad",
  NULLIF(BTRIM("ano_empad"), '')::integer AS "ano_empad",
  NULLIF(BTRIM("resultado_final"), '')::integer AS "resultado_final",
  NULLIF(BTRIM("p24_orden"), '')::integer AS "p24_orden",
  NULLIF(BTRIM("p24a"), '')::integer AS "p24a",
  NULLIF(BTRIM("p24b_dia"), '')::integer AS "p24b_dia",
  NULLIF(BTRIM("p24b_mes"), '')::integer AS "p24b_mes",
  NULLIF(BTRIM("p24b_ano"), '')::integer AS "p24b_ano",
  NULLIF(BTRIM("p24c"), '')::integer AS "p24c",
  NULLIF(BTRIM("p24d"), '')::integer AS "p24d"
FROM raw.mortalidad;
ANALYZE public.mortalidad;