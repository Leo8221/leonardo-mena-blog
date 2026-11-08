-- This schema is the recommended entry point in the new economia_rd database.
CREATE OR REPLACE VIEW analitica.compras_procesos AS
SELECT ocid,id AS version_id,tender_id AS codigo_proceso,
 coalesce(tender_tenderperiod_startdate,date) AS fecha_proceso,
 (tender_tenderperiod_startdate IS NULL) AS fecha_suplida_por_publicacion,
 date AS fecha_version,tender_procuringentity_id AS entidad_id,
 tender_procuringentity_name AS entidad,tender_title AS titulo,
 tender_procurementmethod AS metodo,tender_procurementmethoddetails AS modalidad,
 tender_mainprocurementcategory AS categoria,tender_status AS estado,
 tender_numberoftenderers AS oferentes,tender_value_amount AS monto_estimado,
 tender_value_currency AS moneda,anio_archivo
FROM dgcp.main;

CREATE OR REPLACE VIEW analitica.compras_contratos AS
SELECT c.main_ocid AS ocid,c.main_id AS version_id,c.id AS contrato_id,c.awardid AS adjudicacion_id,
 c.title AS titulo,c.status AS estado,c.period_startdate AS fecha_inicio,
 c.period_enddate AS fecha_fin,c.value_amount AS monto_contrato,c.value_currency AS moneda,
 p.entidad_id,p.entidad,p.modalidad,p.categoria,p.fecha_proceso,c.anio_archivo
FROM dgcp.contracts c JOIN analitica.compras_procesos p
 ON p.ocid=c.main_ocid AND p.version_id=c.main_id;

CREATE OR REPLACE VIEW analitica.compras_proveedores_adjudicados AS
SELECT main_ocid AS ocid,main_id AS version_id,awards_id AS adjudicacion_id,id AS proveedor_id,name AS proveedor
FROM dgcp.awards_suppliers;

CREATE OR REPLACE VIEW analitica.compras_mensuales AS
SELECT date_trunc('month',fecha_inicio)::date AS periodo,entidad_id,entidad,modalidad,moneda,
 count(*) AS contratos,sum(monto_contrato) AS monto_contratado
FROM analitica.compras_contratos WHERE fecha_inicio IS NOT NULL
GROUP BY 1,2,3,4,5;

COMMENT ON VIEW analitica.compras_contratos IS
'Una fila de contrato OCDS. El monto contractual no equivale a pago. Fecha_inicio no es fecha de firma. No sumar monedas ni multiplicar contratos al unir proveedores o artículos.';
COMMENT ON VIEW analitica.compras_mensuales IS
'Contratos agrupados por mes de inicio contractual, entidad, modalidad y moneda. No son desembolsos y pueden contener períodos contractuales inconsistentes en origen.';

CREATE OR REPLACE VIEW analitica.educacion_matricula_anual AS
SELECT periodo_escolar,anio_inicio,codigo_territorio AS codigo_regional,territorio AS regional,
 nivel,estudiantes::bigint AS estudiantes
FROM analitica.educacion_matricula_nivel WHERE nivel_geografico='regional'
UNION ALL
SELECT periodo_escolar,2024,codigo_regional,regional,
 CASE nivel WHEN '1-Inicial' THEN 'Inicial' WHEN '2-Primario' THEN 'Primario'
 WHEN '3-Secundario' THEN 'Secundario' WHEN 'Educ. de Adultos' THEN 'Adultos' ELSE nivel END,
 sum(matricula)::bigint
FROM analitica.educacion_oferta_2024_2025 GROUP BY 1,3,4,5;

COMMENT ON VIEW analitica.educacion_matricula_anual IS
'Serie por regional y nivel. 2015-2016 a 2023-2024: tabulado oficial; 2024-2025: agregación del directorio de oferta. Revisar comparabilidad de cobertura del último curso. El código fuente 202120222 se interpreta como 2021-2022.';

CREATE OR REPLACE VIEW analitica.educacion_centros_historicos AS
SELECT CASE WHEN ano='20222023' THEN '2022-2023' WHEN ano='20232024' THEN '2023-2024' ELSE ano END AS periodo_escolar,
 substring(centros from '^[0-9]+') AS codigo_centro,centros AS centro,
 substring(regional from '^[0-9]+') AS codigo_regional,regional,
 substring(distrito from '^[0-9]+') AS codigo_distrito,distrito,sector,
 matricula::bigint AS matricula,provincia,municipio
FROM minerd.centros_2022_2024
UNION ALL
SELECT periodo_escolar,codigo_centro,centro_educativo,codigo_regional,regional,codigo_distrito,distrito,sector,matricula,NULL::text,NULL::text
FROM analitica.educacion_centros_2024_2025;

CREATE OR REPLACE VIEW analitica.educacion_pruebas_anuales AS
SELECT anio,convocatoria,codigo_regional,nivel_modalidad,count(*) AS registros_centros,
 sum(estudiantes) AS participaciones,sum(promovidos) AS promovidos,
 100.0*sum(promovidos)/nullif(sum(estudiantes),0) AS porcentaje_promovidos,
 count(*) FILTER(WHERE clave_ambigua) AS registros_clave_ambigua,
 count(*) FILTER(WHERE diferencia_sexo OR diferencia_resultado) AS registros_componentes_discrepantes,
 sum(repeticiones_fuente-1) AS duplicados_exactos_excluidos
FROM analitica.educacion_pruebas GROUP BY 1,2,3,4;

COMMENT ON VIEW analitica.educacion_pruebas_anuales IS
'Participaciones por convocatoria, no alumnos únicos. Excluye duplicados exactamente idénticos; conserva y cuenta claves ambiguas. La fuente no contiene 2021. Revisar rupturas en 2020 y 2024 y los desacuerdos entre componentes antes de interpretar tendencias.';

CREATE OR REPLACE VIEW analitica.canasta_familiar AS
SELECT periodo,CASE dataset WHEN 'canasta_quintiles' THEN 'quintil' ELSE 'region' END AS dimension,
 serie AS grupo,valor AS costo_dop
FROM bcrd.series WHERE dataset IN ('canasta_quintiles','canasta_regiones');

CREATE OR REPLACE VIEW analitica.catalogo_presupuesto AS
SELECT fuente_id,archivo,codigo,concepto,min(periodo) AS desde,max(periodo) AS hasta,count(*) AS observaciones,
 count(*) FILTER(WHERE valor_dop IS NULL) AS unidades_pendientes
FROM digepres.ejecucion_mensual GROUP BY 1,2,3,4;
