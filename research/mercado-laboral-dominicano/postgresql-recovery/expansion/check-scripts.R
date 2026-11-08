stopifnot(isTRUE(l10n_info()[['UTF-8']]))
base<-'research/mercado-laboral-dominicano/postgresql-recovery/expansion'
files<-list.files(base,pattern='[.]R$',full.names=TRUE)
for(f in files)parse(f,encoding='UTF-8')
jsonlite::write_json(list(r_files=length(files),utf8=TRUE,status='ok',checked_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%S%z')),file.path(base,'qa/r-script-syntax.json'),auto_unbox=TRUE,pretty=TRUE)
cat('Parsed',length(files),'R scripts under UTF-8.\n')
