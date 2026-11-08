-- Connect to economia_rd on localhost:5433.
-- In censos_linea_tiempo, replace analitica. with fdw_economia.

-- 1. Find a series before using it: labels, units, frequency and actual coverage.
SELECT * FROM analitica.catalogo_series
WHERE dataset IN ('ipc_nacional','imae_2018','canasta_quintiles')
ORDER BY dataset,serie;

-- 2. A monthly macro time series; NULL means unavailable, never an invented zero.
SELECT * FROM analitica.macro_mensual
WHERE periodo >= DATE '2010-01-01' ORDER BY periodo;

-- 3. Historical exchange rates, with buying/selling and average/end-month distinct.
SELECT periodo,operacion,valor_dop_por_usd FROM analitica.tipo_cambio
WHERE frecuencia='mensual' AND estadistica='promedio_periodo'
ORDER BY periodo,operacion;

-- 4. Enrollment by school year and level. National totals use only regional rows.
SELECT periodo_escolar,nivel,sum(estudiantes) AS estudiantes
FROM analitica.educacion_matricula_anual
GROUP BY 1,2 ORDER BY 1,2;

-- 5. Follow a school code where it is available; this is not a panel of pupils.
SELECT periodo_escolar,codigo_centro,centro,matricula
FROM analitica.educacion_centros_historicos
WHERE codigo_centro='02334' ORDER BY periodo_escolar;

-- 6. Keep examination call and modality fixed when comparing years.
-- Exact duplicate source rows are collapsed; ambiguous keys remain flagged below.
SELECT anio,convocatoria,nivel_modalidad,sum(participaciones) AS participaciones,
 sum(promovidos) AS promovidos,
 100.0*sum(promovidos)/nullif(sum(participaciones),0) AS porcentaje_promovidos,
 sum(registros_clave_ambigua) AS registros_clave_ambigua,
 sum(registros_componentes_discrepantes) AS registros_componentes_discrepantes
FROM analitica.educacion_pruebas_anuales
WHERE convocatoria='1'
GROUP BY 1,2,3 ORDER BY 3,1;

-- 7. Employment contributions are jobs, not necessarily distinct people.
SELECT * FROM analitica.empleo_formal ORDER BY periodo;

-- 8. Annual trade: show months available to avoid comparing a partial year to 12 months.
SELECT extract(year FROM periodo)::int AS anio,flujo,
 count(distinct periodo) AS meses_disponibles,sum(millones_usd) AS millones_usd
FROM analitica.comercio_capitulo
GROUP BY 1,2 ORDER BY 1,2;

-- 9. Fiscal: find one classifier and one concept first. Do not add all classifier rows.
SELECT fuente_id,archivo,codigo,concepto,desde,hasta
FROM analitica.catalogo_presupuesto WHERE concepto ILIKE '%educaci%'
ORDER BY fuente_id,codigo;

-- 10. Central government, functional classification, education: keep one code/version.
SELECT periodo,codigo,concepto,millones_dop
FROM analitica.presupuesto_mensual
WHERE fuente_id='5a16a9b6777c' AND concepto ILIKE '%educaci%'
ORDER BY codigo,periodo;

-- 11. Contract history by start month; contractual amounts are not public payments.
SELECT periodo,moneda,count(distinct entidad_id) AS entidades,
 sum(contratos) AS contratos,sum(monto_contratado) AS monto_contratado
FROM analitica.compras_mensuales
WHERE moneda='DOP' AND periodo>=DATE '2015-01-01'
GROUP BY 1,2 ORDER BY 1;

-- 12. Compare GDP per capita in constant 2015 US dollars, preserving the exact code.
SELECT pais_iso3,pais,anio,valor,indicador_nombre
FROM analitica.indicadores_internacionales
WHERE indicador='NY.GDP.PCAP.KD' AND pais_iso3 IN('DOM','CRI','PAN','LAC')
ORDER BY pais_iso3,anio;

-- 13. Locate education indicators by readable name; gross and net rates differ.
SELECT DISTINCT indicador,indicador_nombre,unidad
FROM analitica.educacion_internacional
WHERE pais_iso3='DOM' AND indicador_nombre ILIKE '%matr%'
ORDER BY indicador;

-- 14. Full annual education series, tertiary enrollment, by country.
SELECT pais_iso3,anio,valor,indicador_nombre
FROM analitica.educacion_internacional
WHERE indicador='SE.TER.ENRR' AND pais_iso3 IN('DOM','CRI','PAN','LAC')
ORDER BY pais_iso3,anio;

-- 15. Survey waves: do not pool expansion factors from different years.
SELECT anio_encuesta,count(*) AS muestra,sum(factor_exp) AS expansion
FROM analitica.enief GROUP BY 1 ORDER BY 1;

-- 16. MiPyMES: estimates use expansion factors, not the unweighted sample count.
SELECT region,clasificacion_mipymes,count(*) AS muestra,
 sum(factor_expansion) AS establecimientos_estimados
FROM analitica.mipymes GROUP BY 1,2 ORDER BY 1,2;

-- 17. Read a variable dictionary before using a coded survey response.
SELECT * FROM enhogar_2022.diccionario_personas WHERE variable IN('P401','P402','P403');
SELECT * FROM meta.columnas WHERE tabla='mipymes_2023.microdatos' ORDER BY posicion;

-- 18. Explicit gap detection: this checks calendar continuity, not comparability.
WITH serie AS (
 SELECT periodo,valor FROM analitica.series_economicas
 WHERE dataset='ipc_nacional' AND serie='Índice general'
), esperado AS (
 SELECT generate_series(min(periodo),max(periodo),'1 month')::date AS periodo FROM serie
)
SELECT e.periodo FROM esperado e LEFT JOIN serie s USING(periodo) WHERE s.valor IS NULL;
