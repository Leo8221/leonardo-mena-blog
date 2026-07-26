\set ON_ERROR_STOP on

SET statement_timeout = 0;
SET work_mem = '256MB';
SET maintenance_work_mem = '1GB';
SET max_parallel_maintenance_workers = 4;

-- Ejecutar en censo_2002 y censo_2010. Las vistas armonizado.personas de
-- esos anos ya contienen llaves compuestas de hogar publicadas en sus CSV.

CREATE SCHEMA IF NOT EXISTS analitica;
CREATE SCHEMA IF NOT EXISTS meta;

DO $$
DECLARE
  sample_id text;
BEGIN
  SELECT id_hogar INTO sample_id
  FROM armonizado.personas
  LIMIT 1;
  IF sample_id IS NULL OR sample_id = '' THEN
    RAISE EXCEPTION 'La fuente no tiene llave de hogar; no se pueden construir parejas historicas validas.';
  END IF;
END $$;

DROP MATERIALIZED VIEW IF EXISTS analitica.parejas_jefatura_historica;
DROP MATERIALIZED VIEW IF EXISTS analitica.hogares_pareja_validos_historica;
CREATE MATERIALIZED VIEW analitica.hogares_pareja_validos_historica AS
SELECT id_hogar
FROM armonizado.personas
WHERE parentesco_code IN (1, 2)
GROUP BY id_hogar
HAVING COUNT(*) FILTER (WHERE parentesco_code = 1) = 1
   AND COUNT(*) FILTER (WHERE parentesco_code = 2) = 1;

CREATE UNIQUE INDEX hogares_pareja_validos_historica_hogar_idx
  ON analitica.hogares_pareja_validos_historica (id_hogar);
ANALYZE analitica.hogares_pareja_validos_historica;

CREATE MATERIALIZED VIEW analitica.parejas_jefatura_historica AS
WITH j AS (
  SELECT p.*
  FROM armonizado.personas p
  JOIN analitica.hogares_pareja_validos_historica v USING (id_hogar)
  WHERE p.parentesco_code = 1
),
c AS (
  SELECT p.*
  FROM armonizado.personas p
  JOIN analitica.hogares_pareja_validos_historica v USING (id_hogar)
  WHERE p.parentesco_code = 2
)
SELECT
  j.anio,
  j.fuente,
  j.id_hogar,
  j.region_code,
  j.provincia_code,
  j.municipio_code,
  j.distrito_municipal_code,
  j.zona_code,
  j.id_persona AS jefatura_id_persona,
  j.sexo_code AS jefatura_sexo_code,
  j.sexo_lbl AS jefatura_sexo,
  j.edad AS jefatura_edad,
  j.nivel_educativo_code AS jefatura_nivel_educativo_code,
  j.nivel_educativo_lbl AS jefatura_nivel_educativo,
  j.campo_estudio_code AS jefatura_campo_estudio_code,
  j.ocupacion_code AS jefatura_ocupacion_code,
  j.estado_civil_code AS jefatura_estado_conyugal_code,
  c.id_persona AS pareja_id_persona,
  c.sexo_code AS pareja_sexo_code,
  c.sexo_lbl AS pareja_sexo,
  c.edad AS pareja_edad,
  c.nivel_educativo_code AS pareja_nivel_educativo_code,
  c.nivel_educativo_lbl AS pareja_nivel_educativo,
  c.campo_estudio_code AS pareja_campo_estudio_code,
  c.ocupacion_code AS pareja_ocupacion_code,
  c.estado_civil_code AS pareja_estado_conyugal_code,
  (j.edad - c.edad) AS diferencia_edad_jefatura_menos_pareja,
  ABS(j.edad - c.edad) AS diferencia_edad_absoluta,
  CASE WHEN j.nivel_educativo_code IS NULL OR c.nivel_educativo_code IS NULL THEN NULL
       ELSE j.nivel_educativo_code = c.nivel_educativo_code END AS mismo_nivel_educativo,
  CASE WHEN j.campo_estudio_code IS NULL OR c.campo_estudio_code IS NULL THEN NULL
       ELSE j.campo_estudio_code = c.campo_estudio_code END AS mismo_campo_estudio_codigo,
  CASE WHEN j.ocupacion_code IS NULL OR c.ocupacion_code IS NULL THEN NULL
       ELSE j.ocupacion_code = c.ocupacion_code END AS misma_ocupacion_codigo,
  (j.edad >= 16 AND c.edad >= 16 AND ABS(j.edad - c.edad) <= 45) AS edades_plausibles,
  'alta: una jefatura y una pareja declarada en el hogar'::text AS confianza_enlace
FROM j
JOIN c USING (id_hogar);

CREATE UNIQUE INDEX parejas_jefatura_historica_hogar_idx
  ON analitica.parejas_jefatura_historica (id_hogar);
CREATE INDEX parejas_jefatura_historica_territorio_idx
  ON analitica.parejas_jefatura_historica (provincia_code, municipio_code);
CREATE INDEX parejas_jefatura_historica_estudio_idx
  ON analitica.parejas_jefatura_historica
  (jefatura_campo_estudio_code, pareja_campo_estudio_code);
CREATE INDEX parejas_jefatura_historica_ocupacion_idx
  ON analitica.parejas_jefatura_historica
  (jefatura_ocupacion_code, pareja_ocupacion_code);
ANALYZE analitica.parejas_jefatura_historica;

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
SELECT 'parejas_jefatura_unicas', COUNT(*), NULL,
       CASE WHEN COUNT(*) > 500000 THEN 'ok' ELSE 'advertencia' END,
       'Hogares con exactamente una jefatura y una pareja declarada.', now()
FROM analitica.parejas_jefatura_historica
ON CONFLICT (control) DO UPDATE SET
  resultado = EXCLUDED.resultado,
  esperado = EXCLUDED.esperado,
  estado = EXCLUDED.estado,
  detalle = EXCLUDED.detalle,
  validado_en = EXCLUDED.validado_en;

COMMENT ON MATERIALIZED VIEW analitica.parejas_jefatura_historica IS
  'Parejas convivientes jefatura-conyuge identificadas con una llave compuesta de hogar del CSV censal.';
