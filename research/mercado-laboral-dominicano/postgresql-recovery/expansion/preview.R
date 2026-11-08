source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
args<-commandArgs(TRUE);m<-fromJSON(file.path(base,'qa/download-manifest.json'))
m<-m[grepl(args[1],m$path,ignore.case=TRUE),]
for(i in seq_len(nrow(m))){
 ss<-excel_sheets(m$path[i]);if(length(args)>1)ss<-ss[grepl(args[2],ss,ignore.case=TRUE)]
 for(s in ss){
  x<-suppressMessages(read_excel(m$path[i],sheet=s,col_names=FALSE,col_types='text',.name_repair='minimal'))
  cat('\nFILE',basename(m$path[i]),'SHEET',s,'DIM',nrow(x),ncol(x),'\n')
  rr<-if(length(args)>2)as.integer(strsplit(args[3],',',fixed=TRUE)[[1]])else seq_len(min(10,nrow(x)))
  for(r in rr){if(r>nrow(x))next;z<-unlist(x[r,],use.names=FALSE);keep<-which(!is.na(z));cat(r,paste(paste0(keep,':',substr(z[keep],1,110)),collapse=' | '),'\n')}
 }
}
