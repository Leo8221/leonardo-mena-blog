param(
  [string]$CacheDir = 'D:\datos_one_enhogar\2024',
  [string]$PgHost = 'localhost',
  [int]$PgPort = 5433,
  [string]$PgUser = 'postgres',
  [string]$PgPassword = $env:PG_RECOVERY_PASSWORD,
  [string]$PsqlPath = 'C:\Program Files\PostgreSQL\18\bin\psql.exe'
)

$ErrorActionPreference = 'Stop'

if (-not $PgPassword) { throw 'PgPassword o PG_RECOVERY_PASSWORD es obligatorio.' }
if (-not (Test-Path -LiteralPath $PsqlPath)) { throw "No existe psql: $PsqlPath" }

$datasets = @(
  @{
    table = 'personas'; file = 'BD_ENH24_PERSONAS.csv'; rows = 31822L
    sha256 = '0F3A60246FA5A1659011A36627663E1B079CF7B911616A724B6192630137B2ED'
    archive = '20250722075124'
  },
  @{
    table = 'hogares'; file = 'BD_ENH24_HOGARES.csv'; rows = 12018L
    sha256 = 'C475E180341D98222CBB1A5E548BC5CB49E8CB5B591EF3E0C58532D6C07A9967'
    archive = '20250722075109'
  }
)

function Invoke-Psql {
  param([string]$Database, [string[]]$Arguments)
  $env:PGPASSWORD = $PgPassword
  try {
    & $PsqlPath @('-X', '-w', '-h', $PgHost, '-p', "$PgPort", '-U', $PgUser, '-d', $Database) @Arguments
    if ($LASTEXITCODE -ne 0) { throw "psql fallo con codigo $LASTEXITCODE en $Database" }
  } finally {
    Remove-Item Env:PGPASSWORD -ErrorAction SilentlyContinue
  }
}

function Get-PsqlScalar {
  param([string]$Database, [string]$Sql)
  $env:PGPASSWORD = $PgPassword
  try {
    $result = (& $PsqlPath @('-X', '-w', '-h', $PgHost, '-p', "$PgPort", '-U', $PgUser, '-d', $Database, '-q', '-t', '-A', '-c', $Sql)) -join ''
    if ($LASTEXITCODE -ne 0) { throw "psql fallo con codigo $LASTEXITCODE en $Database" }
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
  while ($Seen.ContainsKey($id)) { $id = "${base}_$i"; $i++ }
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
  $hash = (Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash
  if ($hash -ne $dataset.sha256) { throw "Hash inesperado para $($dataset.file): $hash" }
}

$exists = Get-PsqlScalar -Database postgres -Sql "SELECT 1 FROM pg_database WHERE datname='enhogar_2024';"
if (-not $exists) {
  Invoke-Psql -Database postgres -Arguments @('-v', 'ON_ERROR_STOP=1', '-c', 'CREATE DATABASE enhogar_2024;')
}
Invoke-Psql -Database enhogar_2024 -Arguments @('-v', 'ON_ERROR_STOP=1', '-c', 'CREATE SCHEMA IF NOT EXISTS raw; CREATE SCHEMA IF NOT EXISTS meta;')

$generatedDir = Join-Path $PSScriptRoot 'generated'
New-Item -ItemType Directory -Force -Path $generatedDir | Out-Null

foreach ($dataset in $datasets) {
  $path = Join-Path $CacheDir $dataset.file
  $columns = @(Get-CsvColumns -Path $path)
  $table = $dataset.table
  $rawColumns = ($columns | ForEach-Object { "  $(Quote-Ident $_) text" }) -join ",`n"
  $copyColumns = ($columns | ForEach-Object { Quote-Ident $_ }) -join ', '
  $typedColumns = ($columns | ForEach-Object {
    $targetType = if ($_ -in @('fexpansion', 'fponderacion')) { 'double precision' }
      elseif ($_ -in @('region', 'estratoreg')) { 'numeric' }
      else { 'integer' }
    "  NULLIF(BTRIM($(Quote-Ident $_)), '')::$targetType AS $(Quote-Ident $_)"
  }) -join ",`n"
  $sqlPath = Join-Path $generatedDir "load-enhogar-$table.sql"
  $sql = @"
\set ON_ERROR_STOP on
DROP TABLE IF EXISTS raw.$table CASCADE;
CREATE TABLE raw.$table (
$rawColumns
);
\copy raw.$table ($copyColumns) FROM '$(Quote-CopyPath $path)' WITH (FORMAT csv, HEADER true, NULL '', QUOTE '"', ESCAPE '"');
ANALYZE raw.$table;
DROP TABLE IF EXISTS public.$table CASCADE;
CREATE TABLE public.$table AS
SELECT
$typedColumns
FROM raw.$table;
ANALYZE public.$table;
"@
  [IO.File]::WriteAllText($sqlPath, $sql, [Text.UTF8Encoding]::new($false))
  Invoke-Psql -Database enhogar_2024 -Arguments @('-f', $sqlPath)
}

$metadataSql = @"
\set ON_ERROR_STOP on
CREATE INDEX IF NOT EXISTS enhogar_personas_hogar_idx ON public.personas (upm, hvivien, hhogar, hlinea);
CREATE INDEX IF NOT EXISTS enhogar_personas_demografia_idx ON public.personas (region, hzona, p202, p203);
CREATE INDEX IF NOT EXISTS enhogar_hogares_id_idx ON public.hogares (upm, hvivien, hhogar);
CREATE INDEX IF NOT EXISTS enhogar_hogares_territorio_idx ON public.hogares (region, hzona);

CREATE OR REPLACE VIEW public.hogares_analiticos AS
SELECT DISTINCT ON (upm, hvivien, hhogar) h.*
FROM public.hogares h
WHERE hresult = 1 AND fexpansion IS NOT NULL
ORDER BY upm, hvivien, hhogar, (ecode2 = 999) DESC, ecode2 DESC;

CREATE OR REPLACE VIEW public.personas_analiticas AS
SELECT *
FROM public.personas
WHERE hlinea IS NOT NULL AND fexpansion IS NOT NULL;

CREATE OR REPLACE VIEW public.personas_hogar AS
SELECT
  p.*,
  h.hresult,
  h.hmiembro,
  h.v101 AS tipo_vivienda_code,
  h.v105 AS tenencia_code,
  h.v116 AS alumbrado_code,
  h.v117 AS horas_electricidad,
  h.grup_sec AS grupo_socioeconomico_hogar
FROM public.personas_analiticas p
LEFT JOIN public.hogares_analiticos h USING (upm, hvivien, hhogar);

CREATE TABLE IF NOT EXISTS meta.fuentes (
  tabla text PRIMARY KEY,
  archivo text NOT NULL,
  filas bigint NOT NULL,
  sha256 text NOT NULL,
  captura_archive text NOT NULL,
  pagina_oficial text NOT NULL,
  cargado_en timestamptz NOT NULL DEFAULT now()
);
TRUNCATE meta.fuentes;
INSERT INTO meta.fuentes (tabla, archivo, filas, sha256, captura_archive, pagina_oficial) VALUES
  ('personas', 'BD_ENH24_PERSONAS.csv', 31822, '0F3A60246FA5A1659011A36627663E1B079CF7B911616A724B6192630137B2ED', '20250722075124', 'https://www.one.gob.do/datos-y-estadisticas/'),
  ('hogares', 'BD_ENH24_HOGARES.csv', 12018, 'C475E180341D98222CBB1A5E548BC5CB49E8CB5B591EF3E0C58532D6C07A9967', '20250722075109', 'https://www.one.gob.do/datos-y-estadisticas/');

CREATE TABLE IF NOT EXISTS meta.calidad (
  metrica text PRIMARY KEY,
  valor bigint NOT NULL,
  nota text NOT NULL,
  medido_en timestamptz NOT NULL DEFAULT now()
);
TRUNCATE meta.calidad;
INSERT INTO meta.calidad (metrica, valor, nota) VALUES
  ('personas_microdato', 31822, 'Todas las filas publicadas; incluye entrevistas incompletas.'),
  ('personas_analiticas', 30160, 'Filas con linea de persona y factor de expansion.'),
  ('personas_sin_factor', 1662, 'No deben entrar en estimaciones ponderadas.'),
  ('llaves_persona_duplicadas', 4, 'Llaves upm-vivienda-hogar-linea repetidas en el archivo oficial.'),
  ('hogares_microdato', 12018, 'Todas las filas publicadas; incluye resultados incompletos.'),
  ('hogares_completos', 10359, 'Filas con entrevista completa y factor de expansion.'),
  ('llaves_hogar_analiticas_unicas', 10356, 'Vista deduplicada usada como contexto de personas.'),
  ('llaves_hogar_duplicadas', 7, 'Llaves upm-vivienda-hogar repetidas en el archivo oficial.');

DO `$`$
BEGIN
  IF (SELECT COUNT(*) FROM public.personas) <> 31822 THEN RAISE EXCEPTION 'Conteo inesperado en personas'; END IF;
  IF (SELECT COUNT(*) FROM public.hogares) <> 12018 THEN RAISE EXCEPTION 'Conteo inesperado en hogares'; END IF;
  IF (SELECT COUNT(*) FROM public.personas_analiticas) <> 30160 THEN RAISE EXCEPTION 'Conteo inesperado en personas analiticas'; END IF;
  IF (SELECT COUNT(*) FROM public.hogares_analiticos) <> 10356 THEN RAISE EXCEPTION 'Conteo inesperado en hogares analiticos'; END IF;
  IF (SELECT COUNT(*) FROM public.personas_hogar) <> 30160 THEN RAISE EXCEPTION 'Join personas-hogar incompleto'; END IF;
END
`$`$;
"@

$metadataPath = Join-Path $generatedDir 'finish-enhogar-2024.sql'
[IO.File]::WriteAllText($metadataPath, $metadataSql, [Text.UTF8Encoding]::new($false))
Invoke-Psql -Database enhogar_2024 -Arguments @('-f', $metadataPath)
Invoke-Psql -Database enhogar_2024 -Arguments @('-P', 'pager=off', '-c', "SELECT current_database() AS database, pg_size_pretty(pg_database_size(current_database())) AS size, (SELECT COUNT(*) FROM public.personas) AS personas, (SELECT COUNT(*) FROM public.hogares) AS hogares;")

Remove-Item Env:PG_RECOVERY_PASSWORD -ErrorAction SilentlyContinue
Write-Host 'enhogar_2024 reconstruida y validada.'
