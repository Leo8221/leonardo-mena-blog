source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
source(file.path(base,'time-helpers.R'))
m<-fromJSON(file.path(base,'qa/download-manifest.json'))
for(id in c('e1d15befa0ba','a8b898c70528')){
 f<-m$path[m$id==id]
 s<-1
 x<-grid_read(f,s)
 cat('\n',id,'\n')
 for(r in c(7,40,41,42,43,44,45,46,47,48))if(r<=nrow(x))cat(r,paste(x[r,seq_len(min(8,ncol(x)))],collapse=' | '),'\n')
}
