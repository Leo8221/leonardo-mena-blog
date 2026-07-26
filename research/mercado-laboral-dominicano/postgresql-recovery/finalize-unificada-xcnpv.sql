\set ON_ERROR_STOP on

SET statement_timeout = 0;
SET maintenance_work_mem = '1GB';
SET max_parallel_maintenance_workers = 4;

-- La base contiene 3,726,936 hogares particulares y 9,281 bloques de
-- viviendas colectivas. En estas ultimas, P25_ORDEN tambien reinicia en 1,
-- pero P28_PARENT=14 y no existe jefatura. Los dos tipos de bloque deben
-- conservarse separados; solo los hogares particulares entran a parejas.

DO $$
DECLARE
  n_rows bigint;
  n_starts bigint;
  n_heads bigint;
  n_collective_starts bigint;
  n_collective_people bigint;
  min_hogar bigint;
  max_hogar bigint;
  n_blocks_malformed bigint;
  n_blocks_bad_type bigint;
  n_blocks_bad_start bigint;
BEGIN
  SELECT COUNT(*),
         COUNT(*) FILTER (WHERE p25_orden = 1),
         COUNT(*) FILTER (WHERE p28_parent = 1),
         COUNT(*) FILTER (WHERE p25_orden = 1 AND p28_parent = 14),
         COUNT(*) FILTER (WHERE p28_parent = 14),
         MIN(hogar_id),
         MAX(hogar_id)
  INTO n_rows, n_starts, n_heads, n_collective_starts, n_collective_people,
       min_hogar, max_hogar
  FROM analitica.xcnpv_unificada;

  WITH bloques AS (
    SELECT
      hogar_id,
      COUNT(*) AS n,
      MIN(p25_orden) AS min_orden,
      MAX(p25_orden) AS max_orden,
      COUNT(DISTINCT p25_orden) AS n_ordenes,
      COUNT(*) FILTER (WHERE p25_orden = 1) AS n_inicios,
      COUNT(*) FILTER (WHERE p28_parent = 1) AS n_jefes,
      COUNT(*) FILTER (WHERE p28_parent = 14) AS n_colectivos,
      COUNT(*) FILTER (WHERE p28_parent <> 14) AS n_no_colectivos
    FROM analitica.xcnpv_unificada
    GROUP BY hogar_id
  )
  SELECT
    COUNT(*) FILTER (
      WHERE min_orden <> 1 OR max_orden <> n OR n_ordenes <> n
    ),
    COUNT(*) FILTER (
      WHERE NOT (
        (n_jefes = 1 AND n_colectivos = 0)
        OR (n_jefes = 0 AND n_no_colectivos = 0)
      )
    ),
    COUNT(*) FILTER (WHERE n_inicios <> 1)
  INTO n_blocks_malformed, n_blocks_bad_type, n_blocks_bad_start
  FROM bloques;

  IF n_rows <> 10773983 THEN
    RAISE EXCEPTION 'Conteo inesperado en base unificada: %', n_rows;
  END IF;
  IF n_starts <> 3736217 OR n_heads <> 3726936 THEN
    RAISE EXCEPTION 'La estructura de bloques no coincide: inicios=%, jefaturas=%', n_starts, n_heads;
  END IF;
  IF n_collective_starts <> 9281 OR n_collective_people <> 48090 THEN
    RAISE EXCEPTION 'Viviendas colectivas inesperadas: bloques=%, personas=%', n_collective_starts, n_collective_people;
  END IF;
  IF max_hogar <> n_starts THEN
    RAISE EXCEPTION 'Secuencia de bloques incompleta: max=%, inicios=%', max_hogar, n_starts;
  END IF;
  IF min_hogar <> 1 OR n_blocks_bad_start <> 0 OR n_blocks_bad_type <> 0 THEN
    RAISE EXCEPTION 'Bloques invalidos: min=%, inicio_mal=%, tipo_mal=%',
      min_hogar, n_blocks_bad_start, n_blocks_bad_type;
  END IF;
  IF n_blocks_malformed <> 1 OR NOT EXISTS (
    SELECT 1
    FROM analitica.xcnpv_unificada
    WHERE fila_origen = 8141679
      AND hogar_id = 2843047
      AND p25_orden = 2
      AND p28_parent = 3
  ) THEN
    RAISE EXCEPTION 'La anomalia conocida de P25_ORDEN cambio: bloques_mal=%', n_blocks_malformed;
  END IF;
END $$;

CREATE UNIQUE INDEX IF NOT EXISTS xcnpv_unificada_fila_idx
  ON analitica.xcnpv_unificada (fila_origen);
CREATE INDEX IF NOT EXISTS xcnpv_unificada_hogar_persona_idx
  ON analitica.xcnpv_unificada (hogar_id, p25_orden);
CREATE UNIQUE INDEX IF NOT EXISTS xcnpv_unificada_un_jefe_idx
  ON analitica.xcnpv_unificada (hogar_id)
  WHERE p28_parent = 1;
CREATE INDEX IF NOT EXISTS xcnpv_unificada_parejas_idx
  ON analitica.xcnpv_unificada (p28_parent, hogar_id)
  WHERE p28_parent IN (1, 2);
CREATE INDEX IF NOT EXISTS xcnpv_unificada_territorio_idx
  ON analitica.xcnpv_unificada (provincia, municipio, dmunicipal, zona);
CREATE INDEX IF NOT EXISTS xcnpv_unificada_estudio_idx
  ON analitica.xcnpv_unificada (p43, p45_code, p46);
CREATE INDEX IF NOT EXISTS xcnpv_unificada_ocupacion_idx
  ON analitica.xcnpv_unificada (p60_code, p62_code);

ANALYZE analitica.xcnpv_unificada;

COMMENT ON TABLE analitica.xcnpv_unificada IS
  'Base oficial unificada XCNPV 2022, tipada y segmentada en hogares particulares y bloques colectivos por el orden oficial. Fuente preservada de ONE, captura 2025-07-22.';
COMMENT ON COLUMN analitica.xcnpv_unificada.hogar_id IS
  'Identificador sintetico de bloque de convivencia: suma acumulada de P25_ORDEN=1. Incluye hogares particulares y bloques colectivos; no es un ID publicado por ONE.';
COMMENT ON COLUMN analitica.xcnpv_unificada.p25_orden IS
  'Orden publicado por ONE. Se preserva una anomalia de fuente: fila 8,141,679 repite el orden 2 dentro del bloque 2,843,047; use fila_origen para identidad unica.';

DROP TABLE IF EXISTS staging.unificada_import;
