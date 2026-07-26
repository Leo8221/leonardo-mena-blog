\set ON_ERROR_STOP on

DO $$
DECLARE
  n_personas bigint;
  n_bloques bigint;
  n_hogares bigint;
  n_bloques_colectivos bigint;
  n_parejas bigint;
  n_campos bigint;
  n_ocupaciones bigint;
  n_errores bigint;
BEGIN
  SELECT COUNT(*), MAX(hogar_id),
         COUNT(*) FILTER (WHERE p28_parent = 1),
         COUNT(*) FILTER (WHERE p25_orden = 1 AND p28_parent = 14)
  INTO n_personas, n_bloques, n_hogares, n_bloques_colectivos
  FROM analitica.xcnpv_unificada;

  SELECT COUNT(*) INTO n_parejas
  FROM analitica.parejas_jefatura_2022;

  SELECT COUNT(*) INTO n_campos
  FROM analitica.matriz_campos_estudio_parejas_2022;

  SELECT COUNT(*) INTO n_ocupaciones
  FROM analitica.matriz_ocupaciones_parejas_2022;

  SELECT COUNT(*) INTO n_errores
  FROM meta.controles_calidad_analitica
  WHERE estado = 'error';

  IF n_personas <> 10773983 THEN
    RAISE EXCEPTION 'Personas inesperadas: %', n_personas;
  END IF;
  IF n_bloques <> 3736217 OR n_hogares <> 3726936 OR n_bloques_colectivos <> 9281 THEN
    RAISE EXCEPTION 'Unidades inesperadas: bloques=%, hogares=%, colectivos=%',
      n_bloques, n_hogares, n_bloques_colectivos;
  END IF;
  IF n_parejas < 1000000 THEN
    RAISE EXCEPTION 'Parejas identificadas demasiado bajas: %', n_parejas;
  END IF;
  IF n_campos = 0 OR n_ocupaciones = 0 THEN
    RAISE EXCEPTION 'Matrices vacias: campos=%, ocupaciones=%', n_campos, n_ocupaciones;
  END IF;
  IF n_errores > 0 THEN
    RAISE EXCEPTION 'Controles de calidad en error: %', n_errores;
  END IF;
END $$;

SELECT * FROM analitica.resumen_parejas_2022;
SELECT * FROM meta.controles_calidad_analitica ORDER BY control;
SELECT
  pg_size_pretty(pg_total_relation_size('analitica.xcnpv_unificada')) AS unificada,
  pg_size_pretty(pg_total_relation_size('analitica.parejas_jefatura_2022')) AS parejas;
