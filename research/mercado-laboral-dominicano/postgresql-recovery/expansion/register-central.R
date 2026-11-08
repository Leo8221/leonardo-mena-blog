source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
validation<-fromJSON(file.path(base,'qa/validation-summary.json'))
stopifnot(validation$errors==0)
local<-connect_pg(readonly=TRUE)
views<-dbGetQuery(local,"SELECT table_name FROM information_schema.views WHERE table_schema='analitica' ORDER BY table_name")$table_name
con<-connect_pg('censos_linea_tiempo')
q<-function(x)as.character(dbQuoteString(con,x))
dbWithTransaction(con,{
 dbExecute(con,'CREATE EXTENSION IF NOT EXISTS postgres_fdw')
 options<-dbGetQuery(con,"SELECT o.option_name,o.option_value FROM pg_foreign_server s CROSS JOIN LATERAL pg_options_to_table(s.srvoptions) o WHERE srvname='economia_rd_server'")
 if(nrow(options)){v<-setNames(options$option_value,options$option_name);stopifnot(v[['dbname']]=='economia_rd',v[['host']]=='127.0.0.1',v[['port']]=='5433')}
 else dbExecute(con,"CREATE SERVER economia_rd_server FOREIGN DATA WRAPPER postgres_fdw OPTIONS(host '127.0.0.1',port '5433',dbname 'economia_rd')")
 if(!nrow(dbGetQuery(con,"SELECT 1 FROM pg_user_mappings WHERE srvname='economia_rd_server' AND usename=current_user")))dbExecute(con,paste0('CREATE USER MAPPING FOR CURRENT_USER SERVER economia_rd_server OPTIONS(user ',q(Sys.getenv('PGUSER','postgres')),', password ',q(Sys.getenv('PG_RECOVERY_PASSWORD')),')'))
 dbExecute(con,'CREATE SCHEMA IF NOT EXISTS fdw_economia')
 for(v in views){
  existing<-dbGetQuery(con,"SELECT c.relkind,s.srvname FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace LEFT JOIN pg_foreign_table f ON f.ftrelid=c.oid LEFT JOIN pg_foreign_server s ON s.oid=f.ftserver WHERE n.nspname='fdw_economia' AND c.relname=$1",params=list(v))
  if(nrow(existing))stopifnot(existing$relkind=='f',existing$srvname=='economia_rd_server')
  else dbExecute(con,paste('IMPORT FOREIGN SCHEMA analitica LIMIT TO (',dbQuoteIdentifier(con,v),') FROM SERVER economia_rd_server INTO fdw_economia'))
 }
 sources<-list(
 c('economia_bcrd','Series económicas históricas','BCRD','1984-2026','series_economicas','serie, unidad y período','false','https://www.bancentral.gov.do/estadisticas/'),
 c('economia_fx','Tipo de cambio del dólar','BCRD','1985-2026','tipo_cambio','frecuencia, fecha, operación y estadística','false','https://www.bancentral.gov.do/estadisticas/'),
 c('economia_minerd','Educación: matrícula y centros históricos','MINERD','2015-2016 a 2024-2025','educacion_matricula_anual','año escolar, regional y nivel','false','https://www.ministeriodeeducacion.gob.do/transparencia/datos-abiertos/listados'),
 c('economia_pruebas','Pruebas nacionales','MINERD','2016-2024; sin 2021','educacion_pruebas','registro de centro, año, convocatoria y modalidad','false','https://www.ministeriodeeducacion.gob.do/transparencia/datos-abiertos/listados'),
 c('economia_tss','Empleo y empleadores cotizantes','TSS','2003-2026','empleo_formal','mes','false','https://tss.gob.do/'),
 c('economia_dga','Comercio por capítulo arancelario','DGA','2017-2026 T1','comercio_capitulo','mes, flujo y capítulo','false','https://www.aduanas.gob.do/datos-abiertos/'),
 c('economia_digepres','Ejecución fiscal por clasificadores','DIGEPRES','2004-2026','presupuesto_mensual','fuente, fila de clasificador y mes','false','https://digepres.gob.do/gastos/'),
 c('economia_dgcp','Contrataciones públicas OCDS','DGCP / Open Contracting Partnership','2015-2026','compras_contratos','contrato dentro de proceso','false','https://data.open-contracting.org/es/publication/22'),
 c('economia_wdi','Indicadores de desarrollo comparados','Banco Mundial WDI','1960-2025 según indicador','indicadores_internacionales','país o agregado, indicador y año','false','https://data.worldbank.org/country/dominican-republic'),
 c('economia_mipymes','Encuesta Nacional a las MiPyMES','BCRD','2023; flujos económicos 2022','mipymes','establecimiento observado','true','https://www.bancentral.gov.do/a/d/5962-resultados-de-la-encuesta-nacional-de-las-micro-pequenas-y-medianas-empresas-mipymes'),
 c('economia_enief','Inclusión y educación financiera','BCRD','2019 y 2023','enief','persona observada en cada encuesta','true','https://www.bancentral.gov.do/'),
 c('economia_enae','Encuesta Nacional de Actividad Económica','ONE','edición 2024','enae','empresa observada','true','https://www.one.gob.do/datos-y-estadisticas/'),
 c('economia_enhogar2022','ENHOGAR 2022: personas','ONE','2022','enhogar_2022_personas','persona en hogar','true','https://www.one.gob.do/datos-y-estadisticas/')
 )
 for(s in sources)dbExecute(con,"INSERT INTO catalogo.fuentes(fuente_id,titulo,organismo,periodo,base_datos,tabla_canonica,unidad_analisis,es_microdato,url,fecha_corte,notas) VALUES($1,$2,$3,$4,'economia_rd',$5,$6,$7,$8,NULL,$9) ON CONFLICT(fuente_id) DO UPDATE SET titulo=EXCLUDED.titulo,periodo=EXCLUDED.periodo,tabla_canonica=EXCLUDED.tabla_canonica,notas=EXCLUDED.notas,actualizado_en=now()",params=list(s[1],s[2],s[3],s[4],paste0('analitica.',s[5]),s[6],s[7]=='true',s[8],'Corte de datos variable por serie; consultar catálogo y metadatos en economia_rd. No se enlazan personas o empresas de distintas encuestas. Vistas centrales en fdw_economia. Validaciones y limitaciones en postgresql-recovery/expansion/README.md.'))
})
samples<-list()
for(v in c('macro_mensual','educacion_matricula_anual','compras_contratos','indicadores_rd','tipo_cambio')){
 a<-dbGetQuery(con,paste('SELECT * FROM',dbQuoteIdentifier(con,Id(schema='fdw_economia',table=v)),'LIMIT 1'))
 stopifnot(nrow(a)==1L)
 samples[[v]]<-ncol(a)
}
write_json(list(database='censos_linea_tiempo',server='economia_rd_server',schema='fdw_economia',views=views,read_checks=samples,registered_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%S%z')),file.path(base,'qa/central-registration.json'),auto_unbox=TRUE,pretty=TRUE)
dbDisconnect(local)
dbDisconnect(con)
