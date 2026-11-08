source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
folder<-file.path(base,'raw/dgcp')
files<-list.files(folder,pattern='[.]csv$',recursive=TRUE,full.names=TRUE)
stopifnot(length(files)>=120)
manifest<-fromJSON(file.path(base,'qa/dgcp-download.json'))
con<-init_database()
dbExecute(con,'CREATE SCHEMA IF NOT EXISTS dgcp')
dbExecute(con,'CREATE TABLE IF NOT EXISTS meta.dgcp_cargas(tabla text,anio integer,sha256 text,filas bigint,archivo text,PRIMARY KEY(tabla,anio))')
excluded<-c('contactPoint_name','contactPoint_email','contactPoint_telephone','address_streetAddress')
numeric_field<-function(nm)grepl('(^quantity$|_amount$|_durationInDays$|_numberOfTenderers$)',nm)
date_field<-function(nm)grepl('(^date$|Date$|^publishedDate$)',nm)
for(t in unique(basename(files))){
 tf<-files[basename(files)==t]
 table<-sub('[.]csv$','',t)
 id<-Id(schema='dgcp',table=table)
 original<-unique(unlist(lapply(tf,function(f)names(fread(f,nrows=0,encoding='UTF-8',check.names=FALSE)))))
 original<-setdiff(original,excluded)
 nm<-clean_names(original)
 stopifnot(!anyDuplicated(nm))
 types<-setNames(ifelse(numeric_field(original),'double precision',ifelse(date_field(original),'date','text')),nm)
 types<-c(types,anio_archivo='integer',fila_csv='integer')
 if(!dbExistsTable(con,id)){
  dbExecute(con,paste('CREATE TABLE',dbQuoteIdentifier(con,id),'(',paste(paste(dbQuoteIdentifier(con,names(types)),types),collapse=','),', PRIMARY KEY(anio_archivo,fila_csv))'))
 }else stopifnot(identical(dbListFields(con,id),names(types)))
 for(f in tf){
  year<-as.integer(basename(dirname(f)))
  sha<-digest(file=f,algo='sha256')
  saved<-dbGetQuery(con,'SELECT * FROM meta.dgcp_cargas WHERE tabla=$1 AND anio=$2',params=list(table,year))
  if(nrow(saved)){stopifnot(saved$sha256==sha);next}
  d<-fread(f,colClasses='character',encoding='UTF-8',check.names=FALSE,na.strings=c(''))
  for(c in setdiff(original,names(d)))d[,(c):=NA_character_]
  d<-d[,original,with=FALSE]
  for(c in original){
   if(numeric_field(c)){z<-suppressWarnings(as.numeric(d[[c]]));stopifnot(!any(!is.na(d[[c]])&is.na(z)));set(d,j=c,value=z)}
   if(date_field(c)){z<-as.Date(substr(d[[c]],1,10));stopifnot(!any(!is.na(d[[c]])&is.na(z)));set(d,j=c,value=z)}
  }
  setnames(d,nm)
  d[,anio_archivo:=year]
  d[,fila_csv:=.I]
  dbWithTransaction(con,{
   if(nrow(d))for(start in seq.int(1L,nrow(d),by=20000L))dbAppendTable(con,id,as.data.frame(d[start:min(start+19999L,nrow(d))]))
   dbExecute(con,'INSERT INTO meta.dgcp_cargas VALUES($1,$2,$3,$4,$5)',params=list(table,year,sha,nrow(d),normalizePath(f,winslash='/')))
  })
  actual<-dbGetQuery(con,paste('SELECT count(*) n FROM',dbQuoteIdentifier(con,id),'WHERE anio_archivo=$1'),params=list(year))$n
  stopifnot(as.numeric(actual)==nrow(d))
  cat(table,year,nrow(d),'verified\n')
 }
 dbExecute(con,paste('ANALYZE',dbQuoteIdentifier(con,id)))
 joincol<-if(table=='main')'ocid'else'main_ocid'
 if(joincol %in% nm)dbExecute(con,paste('CREATE INDEX IF NOT EXISTS',dbQuoteIdentifier(con,paste0('dgcp_',table,'_ocid')),'ON',dbQuoteIdentifier(con,id),'(',joincol,')'))
 total<-dbGetQuery(con,'SELECT sum(filas) n FROM meta.dgcp_cargas WHERE tabla=$1',params=list(table))$n
 dbExecute(con,"INSERT INTO meta.tablas(tabla,fuente,titulo,archivo,hoja,url,sha256,filas,columnas,periodo,grano,notas) VALUES($1,'DGCP / OCP',$2,$3,'','https://data.open-contracting.org/es/publication/22',$4,$5,$6,'2015-2026',$7,$8) ON CONFLICT(tabla) DO UPDATE SET filas=EXCLUDED.filas",params=list(paste0('dgcp.',table),paste('Contrataciones públicas OCDS:',table),normalizePath(file.path(base,'qa/dgcp-download.json'),winslash='/'),digest(file=file.path(base,'qa/dgcp-download.json'),algo='sha256'),as.numeric(total),length(types),paste('OCDS',table,'; claves padre main_ocid, main_id y *_id; año de archivo no equivale siempre al año de firma'),'Datos publicados por DGCP y compilados por Open Contracting Partnership. Los importes contractuales no son pagos. Se omiten campos de contacto personal sin utilidad analítica. Fecha SQL conserva día; timestamps exactos permanecen en CSV fuente.'))
 map<-data.frame(tabla=paste0('dgcp.',table),posicion=seq_along(types),nombre_sql=names(types),nombre_original=c(original,'Año del archivo de distribución','Fila de datos CSV'),tipo_sql=unname(types),nulos=0L)
 # Null counts are computed from the SQL data in the final validation.
 dbExecute(con,'DELETE FROM meta.columnas WHERE tabla=$1',params=list(paste0('dgcp.',table)))
 dbAppendTable(con,Id(schema='meta',table='columnas'),map)
}
write_json(dbGetQuery(con,'SELECT tabla,sum(filas) AS filas,count(*) AS archivos FROM meta.dgcp_cargas GROUP BY tabla ORDER BY tabla'),file.path(base,'qa/dgcp-load.json'),auto_unbox=TRUE,pretty=TRUE)
dbDisconnect(con)
