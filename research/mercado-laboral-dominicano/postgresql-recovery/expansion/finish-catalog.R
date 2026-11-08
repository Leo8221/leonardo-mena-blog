source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
summary<-fromJSON(file.path(base,'qa/validation-summary.json'));stopifnot(summary$errors==0)
con<-connect_pg();quality<-fread(file.path(base,'qa/database-checks.csv'),encoding='UTF-8')
quality[,verificado_en:=summary$validated_at]
dbWithTransaction(con,{
 dbExecute(con,'CREATE TABLE IF NOT EXISTS meta.controles_calidad(control text PRIMARY KEY,observado double precision,estado text,nota text,verificado_en text)')
 # Replace only this loader's own quality snapshot, never source records.
 for(i in seq_len(nrow(quality)))dbExecute(con,'INSERT INTO meta.controles_calidad VALUES($1,$2,$3,$4,$5) ON CONFLICT(control) DO UPDATE SET observado=EXCLUDED.observado,estado=EXCLUDED.estado,nota=EXCLUDED.nota,verificado_en=EXCLUDED.verificado_en',params=unname(as.list(quality[i])))
 dbExecute(con,'CREATE OR REPLACE VIEW analitica.catalogo_fuentes AS SELECT tabla,fuente,titulo,periodo,grano,filas,columnas,url,notas,cargado_en FROM meta.tablas')
 dbExecute(con,'CREATE OR REPLACE VIEW analitica.diccionario_variables AS SELECT c.*,t.fuente,t.grano,t.periodo FROM meta.columnas c JOIN meta.tablas t USING(tabla)')
 dbExecute(con,'CREATE OR REPLACE VIEW analitica.calidad_datos AS SELECT * FROM meta.controles_calidad')
 dbExecute(con,"CREATE OR REPLACE VIEW analitica.catalogo_indicadores AS SELECT i.indicador,i.nombre,i.unidad,i.definicion,i.organismo,i.temas,count(o.valor) AS observaciones,count(distinct o.pais_iso3) AS paises_y_agregados,min(o.anio) AS desde,max(o.anio) AS hasta,count(o.valor) FILTER(WHERE o.pais_iso3='DOM') AS observaciones_rd,min(o.anio) FILTER(WHERE o.pais_iso3='DOM') AS desde_rd,max(o.anio) FILTER(WHERE o.pais_iso3='DOM') AS hasta_rd FROM banco_mundial.indicadores i LEFT JOIN banco_mundial.observaciones o USING(indicador) GROUP BY i.indicador,i.nombre,i.unidad,i.definicion,i.organismo,i.temas")
 dbExecute(con,"COMMENT ON VIEW analitica.calidad_datos IS 'Resultados de la última validación guardada. Advertencia describe anomalías conservadas o excluidas de vistas; no equivale a error de carga. Consultar README y fechas de corte.'")
})
views<-dbGetQuery(con,"SELECT c.relname AS vista,obj_description(c.oid,'pg_class') AS descripcion FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname='analitica' AND c.relkind='v' ORDER BY 1")
fwrite(views,file.path(base,'qa/view-inventory.csv'))
details<-list(views=nrow(views),wdi_rd=dbGetQuery(con,"SELECT count(*) AS observaciones,count(distinct indicador) AS indicadores FROM analitica.indicadores_rd"),wdi_education=dbGetQuery(con,"SELECT count(*) AS observaciones,count(distinct indicador) AS indicadores,count(*) FILTER(WHERE pais_iso3='DOM') AS observaciones_rd FROM analitica.educacion_internacional"),pruebas=dbGetQuery(con,'SELECT count(*) AS registros,count(*) FILTER(WHERE clave_ambigua) AS registros_ambiguos,sum(repeticiones_fuente-1) AS duplicados_exactos_excluidos FROM analitica.educacion_pruebas'),dgcp=dbGetQuery(con,"SELECT sum(filas) AS filas_relacionales FROM meta.tablas WHERE tabla LIKE 'dgcp.%'"))
write_json(details,file.path(base,'qa/analytical-summary.json'),auto_unbox=TRUE,pretty=TRUE)
print(details);dbDisconnect(con)
