stopifnot(isTRUE(l10n_info()[["UTF-8"]]))

suppressPackageStartupMessages({
    library(DBI)
    library(RPostgres)
    library(readxl)
    library(data.table)
    library(jsonlite)
    library(digest)
})

base <- "research/mercado-laboral-dominicano/postgresql-recovery/expansion"

dir.create(file.path(base, "qa"), recursive = TRUE, showWarnings = FALSE)

# Prepara nombres SQL únicos; los originales se conservan en el catálogo.
clean_names <- function(x) {
    y <- gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(iconv(x, to = "ASCII//TRANSLIT"))))
    y[is.na(y) | y == ""] <- paste0("columna_", which(is.na(y) | y == ""))
    y[grepl("^[0-9]", y)] <- paste0("v_", y[grepl("^[0-9]", y)])
    long <- nchar(y) > 60L
    y[long] <- paste0(substr(y[long], 1, 52), "_", sprintf("%04d", which(long)))
    make.unique(y, sep = "_")
}

# La contraseña viene de la sesión. readonly bloquea escrituras en esa conexión.
connect_pg <- function(db = "economia_rd", readonly = FALSE) {
    password <- Sys.getenv("PG_RECOVERY_PASSWORD")
    stopifnot(nzchar(password))
    dbConnect(Postgres(), host = Sys.getenv("PGHOST", "127.0.0.1"), port = as.integer(Sys.getenv("PGPORT",
        "5433")), user = Sys.getenv("PGUSER", "postgres"), password = password, dbname = db, options = paste("-c application_name=blog_expansion -c lock_timeout=5000 -c statement_timeout=600000",
        if (readonly)
            "-c default_transaction_read_only=on"
        else ""))
}

# Crea la base y su catálogo solo si faltan; rechaza una base ajena al cargador.
init_database <- function() {
    admin <- connect_pg("postgres")
    if (!nrow(dbGetQuery(admin, "SELECT 1 FROM pg_database WHERE datname='economia_rd'")))
        dbExecute(admin, "CREATE DATABASE economia_rd TEMPLATE template0 ENCODING 'UTF8'")
    dbDisconnect(admin)
    con <- connect_pg()
    existing <- dbGetQuery(con, "SELECT count(*) AS n FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname NOT LIKE 'pg_%' AND n.nspname<>'information_schema' AND c.relkind IN('r','v','m')")$n
    if (existing > 0 && !dbExistsTable(con, Id(schema = "meta", table = "tablas")))
        stop("Existing database is not owned by this additive loader")
    for (s in c("meta", "analitica")) dbExecute(con, paste("CREATE SCHEMA IF NOT EXISTS", s))
    dbExecute(con, "CREATE TABLE IF NOT EXISTS meta.tablas (tabla text PRIMARY KEY,fuente text NOT NULL,titulo text NOT NULL,archivo text NOT NULL,hoja text,url text,sha256 text NOT NULL,filas bigint NOT NULL,columnas integer NOT NULL,periodo text,grano text,notas text,cargado_en timestamptz NOT NULL DEFAULT now())")
    dbExecute(con, "CREATE TABLE IF NOT EXISTS meta.columnas (tabla text NOT NULL,posicion integer NOT NULL,nombre_sql text NOT NULL,nombre_original text NOT NULL,tipo_sql text NOT NULL,nulos bigint NOT NULL,PRIMARY KEY(tabla,posicion))")
    con
}

# Convierte columnas numéricas sin perder códigos con ceros iniciales.
# keep_text permite conservar como texto una columna aunque parezca numérica.
typed_text <- function(df, keep_text = character()) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    for (nm in names(df)) {
        z <- df[[nm]]
        present <- z[!is.na(z)]
        if (!is.character(z) || !length(present) || nm %in% keep_text || grepl("(^id$|^cod|codigo|rnc|formunlario|formulario|_no$)",
            nm))
            next
        numeric_format <- grepl("^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$", present)
        if (all(numeric_format) && !any(grepl("^0[0-9]+$", present))) {
            values <- as.numeric(z)
            stopifnot(all(is.finite(values) | is.na(values)))
            if (all(is.na(values) | (values == trunc(values) & abs(values) <= .Machine$integer.max)))
                df[[nm]] <- as.integer(values)
            else df[[nm]] <- values
        }
    }
    df
}

# Carga una tabla nueva y registra archivo, hash y columnas en la misma transacción.
# Si ya existe, solo la verifica. No sustituye datos distintos automáticamente.
load_table <- function(con, df, schema, table, source, title, path, sheet = "", url = "", period = "",
    grain = "", notes = "", original_names = names(df), fingerprint = NULL) {
    stopifnot(nrow(df) > 0, !anyDuplicated(names(df)), all(nchar(names(df), type = "bytes") <= 63))
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    target <- paste(schema, table, sep = ".")
    id <- Id(schema = schema, table = table)
    sha <- if (is.null(fingerprint))
        digest(file = path, algo = "sha256")
    else fingerprint
    if (dbExistsTable(con, id)) {
        saved <- dbGetQuery(con, "SELECT sha256,filas FROM meta.tablas WHERE tabla=$1", params = list(target))
        n <- dbGetQuery(con, paste("SELECT count(*) n FROM", dbQuoteIdentifier(con, id)))$n
        if (nrow(saved) != 1L || saved$sha256 != sha || as.numeric(n) != nrow(df))
            stop("Refusing to replace an existing, different table: ", target)
        cat("Verified existing", target, nrow(df), "\n")
        return(invisible(list(table = target, rows = nrow(df), columns = ncol(df))))
    }
    dbExecute(con, paste("CREATE SCHEMA IF NOT EXISTS", dbQuoteIdentifier(con, schema)))
    column_types <- vapply(df, function(z) if (inherits(z, "Date"))
        "date"
    else if (is.integer(z))
        "integer"
    else if (is.numeric(z))
        "double precision"
    else if (is.logical(z))
        "boolean"
    else "text", character(1))
    dbWithTransaction(con, {
        dbWriteTable(con, id, df[0, , drop = FALSE], field.types = column_types, row.names = FALSE)
        for (start in seq.int(1L, nrow(df), by = 10000L)) dbAppendTable(con, id, df[start:min(start +
            9999L, nrow(df)), , drop = FALSE])
        if ("fila_origen" %in% names(df))
            dbExecute(con, paste("ALTER TABLE", dbQuoteIdentifier(con, id), "ADD PRIMARY KEY (fila_origen)"))
        metadata <- data.frame(tabla = target, posicion = seq_along(df), nombre_sql = names(df), nombre_original = original_names,
            tipo_sql = column_types, nulos = vapply(df, function(z) sum(is.na(z)), integer(1)))
        dbAppendTable(con, Id(schema = "meta", table = "columnas"), metadata)
        dbExecute(con, "INSERT INTO meta.tablas(tabla,fuente,titulo,archivo,hoja,url,sha256,filas,columnas,periodo,grano,notas) VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12)",
            params = list(target, source, title, normalizePath(path, winslash = "/"), sheet, url, sha,
                nrow(df), ncol(df), period, grain, notes))
        dbExecute(con, paste("COMMENT ON TABLE", dbQuoteIdentifier(con, id), "IS", dbQuoteString(con,
            paste(title, grain, period, notes, sep = ". "))))
    })
    dbExecute(con, paste("ANALYZE", dbQuoteIdentifier(con, id)))
    idx <- unique(as.integer(c(1, 2, ceiling(nrow(df)/2), nrow(df))))
    if ("fila_origen" %in% names(df)) {
        sql <- paste("SELECT * FROM", dbQuoteIdentifier(con, id), "WHERE fila_origen IN (", paste(df$fila_origen[idx],
            collapse = ","), ") ORDER BY fila_origen")
        actual <- dbGetQuery(con, sql)
        expected <- df[idx, , drop = FALSE]
        stopifnot(nrow(actual) == length(idx), identical(names(actual), names(expected)))
        for (nm in names(df)) {
            a <- actual[[nm]]
            b <- expected[[nm]]
            stopifnot(identical(is.na(a), is.na(b)))
            if (is.numeric(b))
                stopifnot(all(abs(as.numeric(a) - b) <= 1e-10 * pmax(1, abs(b)), na.rm = TRUE))
            else stopifnot(identical(as.character(a), as.character(b)))
        }
    }
    cat("Loaded and checked", target, nrow(df), "rows", ncol(df), "columns\n")
    invisible(list(table = target, rows = nrow(df), columns = ncol(df)))
}
