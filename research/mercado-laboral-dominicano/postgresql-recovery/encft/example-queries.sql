-- In database encft: latest validated quarter, age 15+.
SELECT trimestre,muestra_personas_15_mas,ocupados,tasa_desocupacion_abierta,
       tasa_participacion,tasa_empleo_informal
FROM analitica.indicadores_trimestrales ORDER BY trimestre DESC LIMIT 8;

-- Weighted employment by sex. Weights describe a QUARTER, never a single month.
SELECT trimestre,sexo_desc,count(*) AS muestra,
       sum(factor_expansion) AS ocupados_estimados
FROM analitica.personas
WHERE trimestre=20261 AND universo_laboral_15_mas AND ocupado=1
GROUP BY trimestre,sexo_desc;

-- Safe household link (including 2023). Original ID_HOGAR is not the join key.
SELECT count(*) AS personas_con_hogar
FROM analitica.personas p JOIN analitica.hogares h USING(clave_hogar_periodo)
WHERE p.trimestre=20231;

-- Annual levels are quarterly averages only for complete years.
SELECT * FROM analitica.promedios_anuales ORDER BY anio;

-- Review source caveats before research.
SELECT * FROM meta.controles_calidad WHERE estado<>'ok';

-- In censos_linea_tiempo: same indicators without copying the microdata.
-- SELECT * FROM analitica.encft_indicadores_trimestrales ORDER BY trimestre DESC;
