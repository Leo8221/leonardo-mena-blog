source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
m<-fromJSON(file.path(base,'qa/download-manifest.json'));f<-m[m$id=='30f99ffda639',]
x<-as.data.frame(read_excel(f$path,col_types='text'));names(x)<-clean_names(names(x));x$fila_origen<-seq_len(nrow(x))+1L
x<-typed_text(x[x$periodo=='202120222',],keep_text='periodo');stopifnot(nrow(x)==140)
con<-connect_pg()
stopifnot(nrow(dbGetQuery(con,"SELECT 1 FROM meta.tablas WHERE tabla='minerd.matricula_nivel' AND sha256=$1",params=list(digest(file=f$path,algo='sha256'))))==1)
dbWithTransaction(con,{
 if(dbGetQuery(con,"SELECT count(*) AS n FROM minerd.matricula_nivel WHERE periodo='202120222'")$n==0){
  dbAppendTable(con,Id(schema='minerd',table='matricula_nivel'),x)
  dbExecute(con,"UPDATE meta.tablas SET filas=1226,notas=notas||' El código original 202120222 se interpreta como 2021-2022 únicamente en la vista; se conserva literal en la tabla.' WHERE tabla='minerd.matricula_nivel'")
 }
})
dbDisconnect(con)
