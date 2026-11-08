source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
con<-connect_pg()
# Repair only two newly created imports whose title row was mistaken for a header.
# Raw downloaded files remain unchanged; no pre-existing user table is touched.
dbWithTransaction(con,{
 for(t in c('centros_2024_2025','matricula_detalle_2023_2024')){
  full<-paste0('minerd.',t);id<-Id(schema='minerd',table=t)
  stopifnot(dbExistsTable(con,id))
  stopifnot(nrow(dbGetQuery(con,"SELECT 1 FROM meta.tablas WHERE tabla=$1 AND fuente='MINERD'",params=list(full)))==1)
  stopifnot(!'regional' %in% dbListFields(con,id))
  dbExecute(con,paste('DROP TABLE',dbQuoteIdentifier(con,id)))
  dbExecute(con,'DELETE FROM meta.columnas WHERE tabla=$1',params=list(full))
  dbExecute(con,'DELETE FROM meta.tablas WHERE tabla=$1',params=list(full))
 }
})
dbDisconnect(con)
