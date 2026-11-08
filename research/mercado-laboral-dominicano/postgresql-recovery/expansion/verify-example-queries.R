source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
con<-connect_pg(readonly=TRUE)
lines<-readLines(file.path(base,'example-queries.sql'),encoding='UTF-8')
sql<-paste(lines[!grepl('^\\s*--',lines)],collapse='\n')
queries<-trimws(strsplit(sql,';',fixed=TRUE)[[1]])
queries<-queries[nzchar(queries)]
stopifnot(length(queries)==19L)
results<-list()
for(i in seq_along(queries)){
 start<-Sys.time()
 x<-dbGetQuery(con,queries[i])
 elapsed<-as.numeric(difftime(Sys.time(),start,units='secs'))
 # Only the last query (gap detection) should return no rows for this snapshot.
 if(i<length(queries))stopifnot(nrow(x)>0L)
 if(i==length(queries))stopifnot(nrow(x)==0L)
 results[[i]]<-data.table(consulta=i,filas=nrow(x),columnas=ncol(x),segundos=round(elapsed,3),estado='ok')
 cat('Example',i,':',nrow(x),'rows\n')
}
stopifnot(dbGetQuery(con,"SELECT count(*) AS n FROM bcrd.series WHERE dataset='ipc_nacional' AND serie='Índice general'")$n>0)
fwrite(rbindlist(results),file.path(base,'qa/example-queries.csv'))
dbDisconnect(con)
