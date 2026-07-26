\set ON_ERROR_STOP on

CREATE SCHEMA IF NOT EXISTS meta;

DROP MATERIALIZED VIEW IF EXISTS analitica.parejas_jefatura_historica CASCADE;
DROP MATERIALIZED VIEW IF EXISTS analitica.hogares_pareja_validos_historica CASCADE;

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
VALUES (
  'parejas_no_disponibles_sin_llave', NULL, NULL, 'advertencia',
  'El CSV publico de personas CNPV 2002 cargado no contiene llave de vivienda, hogar ni persona. No se fabrican enlaces de pareja.',
  now()
)
ON CONFLICT (control) DO UPDATE SET
  resultado = EXCLUDED.resultado,
  esperado = EXCLUDED.esperado,
  estado = EXCLUDED.estado,
  detalle = EXCLUDED.detalle,
  validado_en = EXCLUDED.validado_en;

DELETE FROM meta.controles_calidad_analitica
WHERE control = 'parejas_jefatura_unicas';
