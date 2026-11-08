-- Additive: source tables remain intact. Every statement can be run again.
CREATE UNIQUE INDEX IF NOT EXISTS viviendas_clave_idx ON microdatos.viviendas(periodo,upm,vivienda);
-- statement
CREATE UNIQUE INDEX IF NOT EXISTS hogares_clave_idx ON microdatos.hogares(periodo,upm,vivienda,hogar);
-- statement
CREATE UNIQUE INDEX IF NOT EXISTS miembros_clave_idx ON microdatos.miembros(periodo,upm,vivienda,hogar,miembro);
-- statement
CREATE UNIQUE INDEX IF NOT EXISTS miembros_persona_trimestre_idx ON microdatos.miembros(trimestre,id_persona);
-- statement
CREATE UNIQUE INDEX IF NOT EXISTS hogares_id_trimestre_idx ON microdatos.hogares(trimestre,id_hogar);
-- statement
CREATE UNIQUE INDEX IF NOT EXISTS tasas_monedas_clave_idx ON meta.tasas_monedas(periodo,moneda);
-- statement
CREATE INDEX IF NOT EXISTS miembros_trimestre_edad_idx ON microdatos.miembros(trimestre,edad);
-- statement
CREATE INDEX IF NOT EXISTS diccionario_variable_idx ON meta.diccionario(periodo,seccion,campo);
-- statement
CREATE OR REPLACE VIEW analitica.personas AS
SELECT fila_origen, anio_archivo AS anio, trimestre, trimestre % 10 AS trimestre_numero,
       periodo, make_date(periodo/100,periodo%100,1) AS mes_entrevista,
       concat_ws(':',periodo,upm,vivienda,hogar,miembro) AS clave_persona_periodo,
       concat_ws(':',periodo,upm,vivienda,hogar) AS clave_hogar_periodo,
       concat_ws(':',periodo,upm,vivienda) AS clave_vivienda_periodo,
       id_persona AS id_persona_fuente, id_hogar AS id_hogar_fuente,
       upm,estrato,des_estrato,vivienda,hogar,miembro,
       factor_expansion,orden_region,grupo_region,id_provincia,des_provincia,id_municipio,des_municipio,zona,
       sexo, CASE sexo WHEN 1 THEN 'Hombre' WHEN 2 THEN 'Mujer' END AS sexo_desc,
       edad,parentesco,estado_civil,grupo_educacion,grupo_edad,
       pet,pet10,ocupado,subocupado,pea,desocupado,desocupado_ini,cesante,nuevo,ampliado,inactivo,
       grupo_rama,grupo_ocupacion,grupo_categoria,grupo_sector,grupo_empleo,
       ocupacion_principal_cod,rama_principal_cod,horas_trabaja_semana_principal,horas_trabajo_efect_total,
       ingreso_asalariado,ingreso_independientes,ingreso_asalariado_secun,ingreso_independientes_secun,
       comisiones,propinas,horas_extra,otros_pagos,bono_vacaciones,bonificaciones,regalia_pascual,
       remesas_ext,remesas_nac_a,archivo_origen,
       edad>=15 AS universo_laboral_15_mas,
       'trimestral'::text AS frecuencia_factor,
       'Los identificadores longitudinales requieren validacion; no enlazar modulos solo por ID_HOGAR.'::text AS nota_identificadores
FROM microdatos.miembros;
-- statement
CREATE OR REPLACE VIEW analitica.hogares AS
SELECT h.*,anio_archivo AS anio,trimestre%10 AS trimestre_numero,
       concat_ws(':',periodo,upm,vivienda,hogar) AS clave_hogar_periodo,
       concat_ws(':',periodo,upm,vivienda) AS clave_vivienda_periodo,
       factor_expansion>0 AS factor_valido
FROM microdatos.hogares h;
-- statement
CREATE OR REPLACE VIEW analitica.viviendas AS
SELECT v.*,anio_archivo AS anio,trimestre%10 AS trimestre_numero,
       concat_ws(':',periodo,upm,vivienda) AS clave_vivienda_periodo,
       coalesce(factor_expansion>0,false) AS factor_valido
FROM microdatos.viviendas v;
-- statement
CREATE OR REPLACE VIEW analitica.indicadores_trimestrales AS
WITH totales AS (
 SELECT anio_archivo AS anio,trimestre,trimestre%10 AS trimestre_numero,
        count(*) AS muestra_personas_total,count(*) FILTER (WHERE edad>=15) AS muestra_personas_15_mas,
        count(DISTINCT periodo) AS meses_disponibles,
        sum(factor_expansion) AS poblacion_total,
        sum(factor_expansion) FILTER(WHERE edad>=15) AS poblacion_15_mas,
        sum(factor_expansion*ocupado) FILTER(WHERE edad>=15) AS ocupados,
        sum(factor_expansion*pea) FILTER(WHERE edad>=15) AS fuerza_trabajo,
        sum(factor_expansion*desocupado) FILTER(WHERE edad>=15) AS desocupados,
        sum(factor_expansion*inactivo) FILTER(WHERE edad>=15) AS fuera_fuerza_trabajo,
        sum(factor_expansion*subocupado) FILTER(WHERE edad>=15) AS subocupados,
        sum(factor_expansion) FILTER(WHERE edad>=15 AND ocupado=1 AND grupo_empleo='Empleo Informal') AS ocupados_informales,
        sum(factor_expansion) FILTER(WHERE edad>=15 AND ocupado=1 AND grupo_empleo='Empleo Formal') AS ocupados_formales
 FROM microdatos.miembros GROUP BY anio_archivo,trimestre
)
SELECT t.*,100.0*fuerza_trabajo/nullif(poblacion_15_mas,0) AS tasa_participacion,
       100.0*ocupados/nullif(poblacion_15_mas,0) AS tasa_ocupacion,
       100.0*desocupados/nullif(fuerza_trabajo,0) AS tasa_desocupacion_abierta,
       100.0*ocupados_informales/nullif(ocupados,0) AS tasa_empleo_informal,
       'Estimaciones propias con microdatos y factores trimestrales de la fuente; universo de 15 anos y mas.'::text AS nota
FROM totales t;
-- statement
CREATE OR REPLACE VIEW analitica.promedios_anuales AS
SELECT anio,count(*)::integer AS trimestres_disponibles,
       (count(*)=4 AND min(meses_disponibles)=3) AS anio_completo,
       CASE WHEN count(*)=4 AND min(meses_disponibles)=3 THEN avg(poblacion_total) END AS poblacion_total_promedio,
       CASE WHEN count(*)=4 AND min(meses_disponibles)=3 THEN avg(poblacion_15_mas) END AS poblacion_15_mas_promedio,
       CASE WHEN count(*)=4 AND min(meses_disponibles)=3 THEN avg(ocupados) END AS ocupados_promedio,
       CASE WHEN count(*)=4 AND min(meses_disponibles)=3 THEN avg(fuerza_trabajo) END AS fuerza_trabajo_promedio,
       CASE WHEN count(*)=4 AND min(meses_disponibles)=3 THEN avg(desocupados) END AS desocupados_promedio,
       'Promedio de niveles trimestrales; no es suma de personas unicas. Anos incompletos sin promedio anual.'::text AS nota
FROM analitica.indicadores_trimestrales GROUP BY anio;
-- statement
COMMENT ON VIEW analitica.personas IS 'Una observacion de persona por trimestre. Factores trimestrales. Para indicadores laborales usar edad>=15; no sumar factores entre trimestres para contar personas unicas. ID_HOGAR fuente no enlaza miembros y hogares en 2023: usar periodo,UPM,vivienda,hogar. No declarar panel longitudinal validado.';
-- statement
COMMENT ON VIEW analitica.viviendas IS 'Se preserva una vivienda con factor nulo y otros valores atipicos en fuente. Consultar meta.controles_calidad y factor_valido antes de estimar.';
-- statement
COMMENT ON TABLE meta.tasas_monedas IS '273 filas de Parquet (2016-2022) y 126 recuperadas de las hojas Tasa monedas de los Excel (2023-2026). Mes 6 es anterior a la entrevista. No usar la tasa del periodo sin respetar mes y moneda del ingreso.';
