\set ON_ERROR_STOP on

-- Microbase muestral para validacion externa y bootstrap por UPM.
-- P208 es estado conyugal actual; P209 no se usa porque solo pregunta si la
-- persona alguna vez estuvo casada o unida.

WITH hogares_pareja AS (
  SELECT upm, hvivien, hhogar
  FROM public.personas_analiticas
  GROUP BY upm, hvivien, hhogar
  HAVING COUNT(*) FILTER (WHERE p205 = 1) = 1
     AND COUNT(*) FILTER (WHERE p205 = 2) = 1
),
base AS (
  SELECT
    j.upm,
    j.hvivien,
    j.hhogar,
    j.estratoreg,
    j.region,
    j.p202 AS j_sexo,
    c.p202 AS c_sexo,
    j.p203 AS j_edad,
    c.p203 AS c_edad,
    j.p208 AS j_union,
    c.p208 AS c_union,
    CASE
      WHEN j.p303 BETWEEN 1 AND 3 THEN j.p303
      WHEN j.p303 BETWEEN 4 AND 6 THEN 4
    END AS j_educacion_4,
    CASE
      WHEN c.p303 BETWEEN 1 AND 3 THEN c.p303
      WHEN c.p303 BETWEEN 4 AND 6 THEN 4
    END AS c_educacion_4,
    h.fexpansion AS peso
  FROM hogares_pareja hp
  JOIN public.personas_analiticas j
    USING (upm, hvivien, hhogar)
  JOIN public.personas_analiticas c
    USING (upm, hvivien, hhogar)
  JOIN public.hogares_analiticos h
    USING (upm, hvivien, hhogar)
  WHERE j.p205 = 1
    AND c.p205 = 2
    AND j.p203 BETWEEN 16 AND 97
    AND c.p203 BETWEEN 16 AND 97
    AND ABS(j.p203 - c.p203) <= 45
    AND h.hresult = 1
    AND h.fexpansion > 0
)
SELECT *
FROM base
ORDER BY estratoreg, upm, hvivien, hhogar;
