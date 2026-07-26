param(
  [string]$CacheDir = "D:\datos_one_censos",
  [string]$PgHost = "localhost",
  [int]$PgPort = 5433,
  [string]$PgUser = "postgres",
  [string]$PgPassword = $env:PG_RECOVERY_PASSWORD,
  [string]$PsqlPath = "C:\Program Files\PostgreSQL\18\bin\psql.exe"
)

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

if (-not $PgPassword) {
  throw "PgPassword is required."
}

$datasets = @(
  @{
    year = 2010; db = "censo_2010"; table = "hogares";
    url = "https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/CNPV_2010_PUB/CENSO2010RD-HOGARES.csv";
    codebook = "https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/CNPV_2010_PUB/Libro%20de%20codigo%20BD_Hogares_Censo2010.htm";
    file = "2010/CENSO2010RD-HOGARES.csv"; codebook_file = "2010/Libro_codigo_hogares_2010.htm"
  },
  @{
    year = 2010; db = "censo_2010"; table = "personas";
    url = "https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/CNPV_2010_PUB/CENSO2010RD-PERSONAS.csv";
    codebook = "https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/CNPV_2010_PUB/Libro%20de%20codigo%20BD_Personas_Censo2010.htm";
    file = "2010/CENSO2010RD-PERSONAS.csv"; codebook_file = "2010/Libro_codigo_personas_2010.htm"
  },
  @{
    year = 2010; db = "censo_2010"; table = "viviendas";
    url = "https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/CNPV_2010_PUB/CENSO2010RD-VIVIENDA-COMUNIDAD.csv";
    codebook = "https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/CNPV_2010_PUB/Libro%20de%20codigo%20BD_Vivienda_Comunidad_Censo2010.htm";
    file = "2010/CENSO2010RD-VIVIENDA-COMUNIDAD.csv"; codebook_file = "2010/Libro_codigo_vivienda_comunidad_2010.htm"
  },
  @{
    year = 2002; db = "censo_2002"; table = "hogares";
    url = "https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/CNPV_2002_PUB/CENSO2002RD-HOGARES.csv";
    codebook = "https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/CNPV_2002_PUB/Libro_de_codigos_BD_Hogares_Censo2002.htm";
    file = "2002/CENSO2002RD-HOGARES.csv"; codebook_file = "2002/Libro_codigo_hogares_2002.htm"
  },
  @{
    year = 2002; db = "censo_2002"; table = "personas";
    url = "https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/CNPV_2002_PUB/CENSO2002RD-PERSONAS.csv";
    codebook = "https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/CNPV_2002_PUB/Libro_de_codigos_BD_Personas_Censo2002.htm";
    file = "2002/CENSO2002RD-PERSONAS.csv"; codebook_file = "2002/Libro_codigo_personas_2002.htm"
  },
  @{
    year = 2002; db = "censo_2002"; table = "viviendas";
    url = "https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/CNPV_2002_PUB/CENSO2002RD-VIVIENDAS.csv";
    codebook = "https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/CNPV_2002_PUB/Libro_de_codigos_BD_Viviendas_Censo2002.htm";
    file = "2002/CENSO2002RD-VIVIENDAS.csv"; codebook_file = "2002/Libro_codigo_viviendas_2002.htm"
  }
)

function Invoke-Psql {
  param([string]$Database, [string[]]$PsqlArgsExtra)
  $env:PGPASSWORD = $PgPassword
  try {
    $psqlArgs = @("-w", "-h", $PgHost, "-p", "$PgPort", "-U", $PgUser, "-d", $Database) + $PsqlArgsExtra
    & $PsqlPath @psqlArgs
    if ($LASTEXITCODE -ne 0) { throw "psql failed with exit code $LASTEXITCODE" }
  } finally {
    Remove-Item Env:PGPASSWORD -ErrorAction SilentlyContinue
  }
}

function Normalize-Identifier {
  param([string]$Name, [hashtable]$Seen)
  $id = $Name.Trim()
  $id = $id -replace '^\uFEFF', ''
  $id = $id.ToLowerInvariant()
  $id = $id -replace '[^a-z0-9_]+', '_'
  $id = $id.Trim('_')
  if (-not $id) { $id = "col" }
  if ($id -match '^[0-9]') { $id = "c_$id" }
  $base = $id
  $i = 2
  while ($Seen.ContainsKey($id)) {
    $id = "${base}_${i}"
    $i++
  }
  $Seen[$id] = $true
  return $id
}

function Get-CsvHeaderColumns {
  param([string]$Path)
  $reader = [System.IO.StreamReader]::new($Path, [System.Text.Encoding]::UTF8, $true)
  try {
    $line = $reader.ReadLine()
  } finally {
    $reader.Close()
  }
  if (-not $line) { throw "No header found in $Path" }
  $raw = $line.Split(",")
  $seen = @{}
  $cols = New-Object System.Collections.Generic.List[string]
  foreach ($c in $raw) {
    $cols.Add((Normalize-Identifier -Name $c -Seen $seen))
  }
  return $cols
}

function SqlQuoteIdent {
  param([string]$Identifier)
  return '"' + ($Identifier -replace '"','""') + '"'
}

New-Item -ItemType Directory -Force -Path $CacheDir | Out-Null
New-Item -ItemType Directory -Force -Path (Join-Path $PSScriptRoot "logs") | Out-Null

$manifestPath = Join-Path $PSScriptRoot "logs\manifest.csv"
"year,db,table,kind,url,local_path,bytes" | Set-Content -Path $manifestPath -Encoding UTF8

foreach ($d in $datasets) {
  foreach ($kind in @("csv", "codebook")) {
    $url = if ($kind -eq "csv") { $d.url } else { $d.codebook }
    $rel = if ($kind -eq "csv") { $d.file } else { $d.codebook_file }
    $path = Join-Path $CacheDir $rel
    New-Item -ItemType Directory -Force -Path (Split-Path $path -Parent) | Out-Null
    if (-not (Test-Path $path) -or (Get-Item $path).Length -eq 0) {
      Write-Host "Downloading $kind $($d.year).$($d.table) -> $path"
      & curl.exe -L --fail --retry 3 --retry-delay 5 -C - -o $path $url
      if ($LASTEXITCODE -ne 0) { throw "curl failed for $url" }
    } else {
      Write-Host "Using cached $kind $($d.year).$($d.table) -> $path"
    }
    $bytes = (Get-Item $path).Length
    "$($d.year),$($d.db),$($d.table),$kind,""$url"",""$path"",$bytes" | Add-Content -Path $manifestPath -Encoding UTF8
  }
}

foreach ($db in @("censo_2010", "censo_2002")) {
  $exists = (& {
    $env:PGPASSWORD = $PgPassword
    try {
      & $PsqlPath @("-w", "-h", $PgHost, "-p", "$PgPort", "-U", $PgUser, "-d", "postgres", "-q", "-t", "-A", "-c", "SELECT 1 FROM pg_database WHERE datname='$db';")
    } finally {
      Remove-Item Env:PGPASSWORD -ErrorAction SilentlyContinue
    }
  }) -join ""
  if (-not $exists.Trim()) {
    Write-Host "Creating database $db"
    Invoke-Psql -Database "postgres" -PsqlArgsExtra @("-q", "-c", "CREATE DATABASE $db;")
  } else {
    Write-Host "Database $db already exists"
  }
  Invoke-Psql -Database $db -PsqlArgsExtra @("-q", "-c", "CREATE SCHEMA IF NOT EXISTS raw; CREATE SCHEMA IF NOT EXISTS meta;")
}

$summaryPath = Join-Path $PSScriptRoot "logs\load_summary.csv"
"year,db,table,columns,rows,source_file" | Set-Content -Path $summaryPath -Encoding UTF8

foreach ($d in $datasets) {
  $path = Join-Path $CacheDir $d.file
  $cols = Get-CsvHeaderColumns -Path $path
  $colSql = ($cols | ForEach-Object { "  $(SqlQuoteIdent $_) text" }) -join ",`n"
  $copyCols = ($cols | ForEach-Object { SqlQuoteIdent $_ }) -join ", "
  $copyPath = ($path -replace "\\", "/") -replace "'", "''"
  $sqlPath = Join-Path $PSScriptRoot ("logs\load_{0}_{1}.sql" -f $d.year, $d.table)
  $sql = @(
    "\set ON_ERROR_STOP on",
    "DROP TABLE IF EXISTS raw.$($d.table);",
    "CREATE TABLE raw.$($d.table) (",
    $colSql,
    ");",
    "\copy raw.$($d.table) ($copyCols) FROM '$copyPath' WITH (FORMAT csv, HEADER true, NULL '', QUOTE '""', ESCAPE '""');",
    "ANALYZE raw.$($d.table);"
  ) -join "`n"
  Set-Content -Path $sqlPath -Value $sql -Encoding UTF8
  Write-Host "Loading $($d.db).raw.$($d.table) from $path"
  Invoke-Psql -Database $d.db -PsqlArgsExtra @("-f", $sqlPath)
  $rows = (& {
    $env:PGPASSWORD = $PgPassword
    try {
      & $PsqlPath @("-w", "-h", $PgHost, "-p", "$PgPort", "-U", $PgUser, "-d", $d.db, "-q", "-t", "-A", "-c", "SELECT COUNT(*) FROM raw.$($d.table);")
    } finally {
      Remove-Item Env:PGPASSWORD -ErrorAction SilentlyContinue
    }
  }) -join ""
  "$($d.year),$($d.db),$($d.table),$($cols.Count),$($rows.Trim()),""$path""" | Add-Content -Path $summaryPath -Encoding UTF8
}

foreach ($db in @("censo_2010", "censo_2002")) {
  Invoke-Psql -Database $db -PsqlArgsExtra @("-q", "-c", "CREATE TABLE IF NOT EXISTS meta.archivos (year integer, table_name text, kind text, url text, local_path text, bytes bigint, loaded_at timestamptz default now());")
}

Write-Host "Done. Manifest: $manifestPath"
Write-Host "Done. Summary: $summaryPath"
