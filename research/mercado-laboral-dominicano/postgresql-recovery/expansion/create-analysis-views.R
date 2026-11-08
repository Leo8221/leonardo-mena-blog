source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
con<-connect_pg()
# Verify one process per OCID and a complete composite relationship before joins.
stopifnot(dbGetQuery(con,'SELECT count(*)-count(distinct ocid) n FROM dgcp.main')$n==0)
stopifnot(dbGetQuery(con,'SELECT count(*) n FROM dgcp.contracts c WHERE NOT EXISTS(SELECT 1 FROM dgcp.main m WHERE m.ocid=c.main_ocid AND m.id=c.main_id)')$n==0)
sql<-paste(readLines(file.path(base,'analysis-views.sql'),encoding='UTF-8',warn=FALSE),collapse='\n')
# Send the complete trusted SQL script; quoted semicolons remain inside comments.
dbExecute(con,sql,immediate=TRUE)
write_json(dbGetQuery(con,"SELECT table_name FROM information_schema.views WHERE table_schema='analitica' ORDER BY 1"),file.path(base,'qa/analysis-views.json'),auto_unbox=TRUE,pretty=TRUE)
dbDisconnect(con)
