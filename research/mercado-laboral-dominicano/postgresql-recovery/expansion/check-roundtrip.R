source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
con<-connect_pg(readonly=TRUE)
results<-list()
loads<-dbGetQuery(con,'SELECT * FROM meta.dgcp_cargas WHERE anio IN(2015,2026) ORDER BY tabla,anio')
for(i in seq_len(nrow(loads))){
 l<-loads[i,]
 d<-fread(l$archivo,colClasses='character',encoding='UTF-8',na.strings='',check.names=FALSE)
 names(d)<-clean_names(names(d))
 idx<-unique(as.integer(c(1,2,ceiling(nrow(d)/2),nrow(d))))
 idx<-idx[idx>0 & idx<=nrow(d)]
 if(!length(idx))next
 id<-Id(schema='dgcp',table=l$tabla)
 actual<-dbGetQuery(con,paste('SELECT * FROM',dbQuoteIdentifier(con,id),'WHERE anio_archivo=',l$anio,'AND fila_csv IN(',paste(idx,collapse=','),') ORDER BY fila_csv'))
 expect<-d[idx]
 cols<-intersect(names(expect),names(actual))
 stopifnot(nrow(actual)==length(idx))
 for(nm in cols){
  a<-actual[[nm]]
  e<-expect[[nm]]
  if(inherits(a,'Date'))e<-as.Date(substr(e,1,10))else if(is.numeric(a))e<-as.numeric(e)
  stopifnot(identical(is.na(a),is.na(e)))
  if(is.numeric(a))stopifnot(all(abs(a-e)<=1e-12*pmax(1,abs(e)),na.rm=TRUE))
  else stopifnot(identical(as.character(a),as.character(e)))
 }
 results[[length(results)+1L]]<-data.table(tabla=paste0('dgcp.',l$tabla),anio=l$anio,filas=length(idx),columnas=length(cols),estado='ok')
 cat('Source roundtrip',l$tabla,l$anio,'ok\n')
}
fwrite(rbindlist(results),file.path(base,'qa/roundtrip-dgcp.csv'))
dbDisconnect(con)
