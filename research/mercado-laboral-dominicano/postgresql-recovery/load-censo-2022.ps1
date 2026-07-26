param(
  [string]$CacheDir = 'D:\datos_one_censos\2022',
  [string]$PgHost = 'localhost',
  [int]$PgPort = 5433,
  [string]$PgUser = 'postgres',
  [string]$PgPassword = $env:PG_RECOVERY_PASSWORD,
  [string]$PsqlPath = 'C:\Program Files\PostgreSQL\18\bin\psql.exe',
  [string]$CnoCsv = 'D:\datos_one_censos\2022\cno_2019_diccionario_completo.csv',
  [string]$CnaeCsv = 'D:\datos_one_censos\2022\cnae_2019_diccionario_completo.csv',
  [string]$TerritorialCsv = 'D:\datos_one_censos\2022\catalogo_municipios_2022.csv',
  [switch]$Resume
)

$ErrorActionPreference = 'Stop'

if (-not $PgPassword) {
  throw 'PgPassword o PG_RECOVERY_PASSWORD es obligatorio.'
}
if (-not (Test-Path -LiteralPath $PsqlPath)) {
  throw "No existe psql: $PsqlPath"
}

$datasets = @(
  @{ table = 'personas'; file = 'BD_PERSONAS_XCNPV.csv'; rows = 10773983L },
  @{ table = 'vivienda_hogar'; file = 'BD_VIVIENDAS_XCNPV.csv'; rows = 4455060L },
  @{ table = 'mortalidad'; file = 'BD_Mortalidad_XCNPV.csv'; rows = 158243L }
)

function Invoke-Psql {
  param([string]$Database, [string[]]$Arguments)
  $env:PGPASSWORD = $PgPassword
  try {
    & $PsqlPath @('-X', '-w', '-h', $PgHost, '-p', "$PgPort", '-U', $PgUser, '-d', $Database) @Arguments
    if ($LASTEXITCODE -ne 0) {
      throw "psql fallo con codigo $LASTEXITCODE en $Database"
    }
  } finally {
    Remove-Item Env:PGPASSWORD -ErrorAction SilentlyContinue
  }
}

function Get-PsqlScalar {
  param([string]$Database, [string]$Sql)
  $env:PGPASSWORD = $PgPassword
  try {
    $result = (& $PsqlPath @('-X', '-w', '-h', $PgHost, '-p', "$PgPort", '-U', $PgUser, '-d', $Database, '-q', '-t', '-A', '-c', $Sql)) -join ''
    if ($LASTEXITCODE -ne 0) {
      throw "psql fallo con codigo $LASTEXITCODE en $Database"
    }
    return $result.Trim()
  } finally {
    Remove-Item Env:PGPASSWORD -ErrorAction SilentlyContinue
  }
}

function Normalize-Identifier {
  param([string]$Name, [hashtable]$Seen)
  $id = $Name.Trim() -replace '^\uFEFF', ''
  $id = $id.ToLowerInvariant() -replace '[^a-z0-9_]+', '_'
  $id = $id.Trim('_')
  if (-not $id) { $id = 'col' }
  if ($id -match '^[0-9]') { $id = "c_$id" }
  $base = $id
  $i = 2
  while ($Seen.ContainsKey($id)) {
    $id = "${base}_$i"
    $i++
  }
  $Seen[$id] = $true
  return $id
}

function Get-CsvColumns {
  param([string]$Path)
  $reader = [IO.StreamReader]::new($Path, [Text.Encoding]::UTF8, $true)
  try { $header = $reader.ReadLine() } finally { $reader.Dispose() }
  if (-not $header) { throw "CSV sin encabezado: $Path" }
  $seen = @{}
  return @($header.Split(',') | ForEach-Object { Normalize-Identifier -Name $_ -Seen $seen })
}

function Quote-Ident {
  param([string]$Identifier)
  return '"' + ($Identifier -replace '"', '""') + '"'
}

function Quote-CopyPath {
  param([string]$Path)
  return (($Path -replace '\\', '/') -replace "'", "''")
}

foreach ($dataset in $datasets) {
  $path = Join-Path $CacheDir $dataset.file
  if (-not (Test-Path -LiteralPath $path)) { throw "Falta: $path" }
  if ((Get-Item -LiteralPath $path).Length -eq 0) { throw "Archivo vacio: $path" }
}
foreach ($path in @($CnoCsv, $CnaeCsv, $TerritorialCsv)) {
  if (-not (Test-Path -LiteralPath $path)) { throw "Falta: $path" }
}

$exists = $null
$env:PGPASSWORD = $PgPassword
try {
  $exists = (& $PsqlPath @('-X','-w','-h',$PgHost,'-p',"$PgPort",'-U',$PgUser,'-d','postgres','-q','-t','-A','-c',"SELECT 1 FROM pg_database WHERE datname='censo_2022';")) -join ''
  if ($LASTEXITCODE -ne 0) { throw 'No se pudo consultar pg_database.' }
} finally {
  Remove-Item Env:PGPASSWORD -ErrorAction SilentlyContinue
}

if (-not $exists.Trim()) {
  Invoke-Psql -Database postgres -Arguments @('-v','ON_ERROR_STOP=1','-c','CREATE DATABASE censo_2022;')
}

Invoke-Psql -Database censo_2022 -Arguments @('-v','ON_ERROR_STOP=1','-c','CREATE SCHEMA IF NOT EXISTS raw; CREATE SCHEMA IF NOT EXISTS diccionarios; CREATE SCHEMA IF NOT EXISTS meta;')

$generatedDir = Join-Path $PSScriptRoot 'generated'
New-Item -ItemType Directory -Force -Path $generatedDir | Out-Null

foreach ($dataset in $datasets) {
  $path = Join-Path $CacheDir $dataset.file
  $columns = @(Get-CsvColumns -Path $path)
  $table = $dataset.table
  $expectedRows = [long]$dataset.rows
  $reuseRaw = $false
  if ($Resume) {
    $publicExists = Get-PsqlScalar -Database censo_2022 -Sql "SELECT to_regclass('public.$table') IS NOT NULL;"
    if ($publicExists -eq 't') {
      $publicRows = [long](Get-PsqlScalar -Database censo_2022 -Sql "SELECT COUNT(*) FROM public.$table;")
      if ($publicRows -eq $expectedRows) {
        Write-Host "Reutilizando public.$table validada ($publicRows filas)"
        continue
      }
    }
    $rawExists = Get-PsqlScalar -Database censo_2022 -Sql "SELECT to_regclass('raw.$table') IS NOT NULL;"
    if ($rawExists -eq 't') {
      $rawRows = [long](Get-PsqlScalar -Database censo_2022 -Sql "SELECT COUNT(*) FROM raw.$table;")
      $reuseRaw = $rawRows -eq $expectedRows
      if ($reuseRaw) {
        Write-Host "Reutilizando raw.$table validada ($rawRows filas)"
      } else {
        Write-Host "raw.$table tiene $rawRows filas; se recargara (esperadas: $expectedRows)"
      }
    }
  }
  $rawColumns = ($columns | ForEach-Object { "  $(Quote-Ident $_) text" }) -join ",`n"
  $copyColumns = ($columns | ForEach-Object { Quote-Ident $_ }) -join ', '
  $typedColumns = ($columns | ForEach-Object { "  NULLIF(BTRIM($(Quote-Ident $_)), '')::integer AS $(Quote-Ident $_)" }) -join ",`n"
  $sqlPath = Join-Path $generatedDir "load-$table.sql"
  $rawSql = if ($reuseRaw) {
    "ANALYZE raw.$table;"
  } else {
@"
DROP TABLE IF EXISTS raw.$table CASCADE;
CREATE TABLE raw.$table (
$rawColumns
);
\copy raw.$table ($copyColumns) FROM '$(Quote-CopyPath $path)' WITH (FORMAT csv, HEADER true, NULL '', QUOTE '"', ESCAPE '"');
ANALYZE raw.$table;
"@
  }
  $sql = @"
\set ON_ERROR_STOP on
$rawSql
DROP TABLE IF EXISTS public.$table CASCADE;
CREATE TABLE public.$table AS
SELECT
$typedColumns
FROM raw.$table;
ANALYZE public.$table;
"@
  [IO.File]::WriteAllText($sqlPath, $sql, [Text.UTF8Encoding]::new($false))
  Write-Host "Cargando $table desde $path"
  Invoke-Psql -Database censo_2022 -Arguments @('-f', $sqlPath)
}

$dictionarySql = Join-Path $generatedDir 'load-dictionaries.sql'
$dictionaryBody = @"
\set ON_ERROR_STOP on
DROP TABLE IF EXISTS diccionarios.cno_2019 CASCADE;
CREATE TABLE diccionarios.cno_2019 (code text, nivel_n integer, nivel text, "desc" text);
\copy diccionarios.cno_2019 FROM '$(Quote-CopyPath $CnoCsv)' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8');

DROP TABLE IF EXISTS diccionarios.cnae_2019 CASCADE;
CREATE TABLE diccionarios.cnae_2019 (code text, nivel_n integer, nivel text, parent_code text, "desc" text);
\copy diccionarios.cnae_2019 FROM '$(Quote-CopyPath $CnaeCsv)' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8');

DROP TABLE IF EXISTS public.catalogo_territorial CASCADE;
CREATE TABLE public.catalogo_territorial (
  provincia_code integer,
  municipio_code integer,
  nombre_provincia text,
  nombre_municipio text
);
\copy public.catalogo_territorial FROM '$(Quote-CopyPath $TerritorialCsv)' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8');
"@
[IO.File]::WriteAllText($dictionarySql, $dictionaryBody, [Text.UTF8Encoding]::new($false))
Invoke-Psql -Database censo_2022 -Arguments @('-f', $dictionarySql)

$enrichmentSql = Join-Path $PSScriptRoot 'enrich-censo-2022.sql'
Invoke-Psql -Database censo_2022 -Arguments @('-f', $enrichmentSql)

$laborViewSql = Join-Path (Split-Path $PSScriptRoot -Parent) 'sql\mercado_laboral_censo_postgres.sql'
if (Test-Path -LiteralPath $laborViewSql) {
  Invoke-Psql -Database censo_2022 -Arguments @('-f', $laborViewSql)
}

$verifySql = Join-Path $PSScriptRoot 'verify-recovery.sql'
Invoke-Psql -Database censo_2022 -Arguments @('-f', $verifySql)

Remove-Item Env:PG_RECOVERY_PASSWORD -ErrorAction SilentlyContinue
Write-Host 'censo_2022 reconstruida y validada.'
