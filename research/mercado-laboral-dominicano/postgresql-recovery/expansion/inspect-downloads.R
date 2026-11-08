source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
manifest<-fromJSON(file.path(base,'qa/download-manifest.json'))
inventory<-list(); previews<-list(); errors<-list()
for(i in seq_len(nrow(manifest))){
 f<-manifest[i,];if(f$status=='failed'||!grepl('[.]xlsx?$',f$path))next
 sheets<-tryCatch(excel_sheets(f$path),error=function(e){errors[[length(errors)+1L]]<<-list(id=f$id,path=f$path,error=conditionMessage(e));character()})
 for(s in sheets){
  x<-tryCatch(suppressMessages(read_excel(f$path,sheet=s,col_names=FALSE,col_types='text',.name_repair='minimal')),error=function(e){errors[[length(errors)+1L]]<<-list(id=f$id,sheet=s,error=conditionMessage(e));NULL})
  if(is.null(x))next
  key<-paste(f$id,s,sep=':')
  inventory[[length(inventory)+1L]]<-data.frame(id=f$id,group=f$group,file=basename(f$path),sheet=s,rows=nrow(x),columns=ncol(x))
  previews[[key]]<-list(id=f$id,group=f$group,file=basename(f$path),sheet=s,rows=nrow(x),columns=ncol(x),preview=lapply(seq_len(min(14,nrow(x))),function(r)as.list(unname(x[r,seq_len(min(22,ncol(x))),drop=TRUE]))))
 }
}
fwrite(rbindlist(inventory),file.path(base,'qa/download-workbooks.csv'))
write_json(previews,file.path(base,'qa/download-previews.json'),auto_unbox=TRUE,pretty=FALSE,na='null')
write_json(errors,file.path(base,'qa/download-workbook-errors.json'),auto_unbox=TRUE,pretty=TRUE)
cat(length(inventory),'sheets;',length(errors),'unreadable sheets\n')
