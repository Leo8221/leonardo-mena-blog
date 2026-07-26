param(
  [string]$CsvDir = (Join-Path $PSScriptRoot '..\data\raw\one_csv'),
  [string]$PgHost = 'localhost',
  [int]$PgPort = 5433,
  [string]$PgUser = 'postgres',
  [string]$PgPassword = $env:PG_RECOVERY_PASSWORD,
  [string]$PsqlPath = 'C:\Program Files\PostgreSQL\18\bin\psql.exe',
  [switch]$GenerateOnly
)

$ErrorActionPreference = 'Stop'
if (-not $GenerateOnly -and -not $PgPassword) { throw 'PgPassword o PG_RECOVERY_PASSWORD es obligatorio.' }
if (-not (Test-Path -LiteralPath $PsqlPath)) { throw "No existe psql: $PsqlPath" }
$manifestPath = Join-Path $CsvDir 'manifest.csv'
if (-not (Test-Path -LiteralPath $manifestPath)) { throw "Falta el manifest de CSV: $manifestPath" }

function Invoke-Psql {
  param([string]$Database, [string[]]$Arguments)
  $env:PGPASSWORD = $PgPassword
  try {
    & $PsqlPath -X -w -h $PgHost -p "$PgPort" -U $PgUser -d $Database @Arguments
    if ($LASTEXITCODE -ne 0) { throw "psql fallo con codigo $LASTEXITCODE en $Database" }
  } finally { Remove-Item Env:PGPASSWORD -ErrorAction SilentlyContinue }
}

function Get-PsqlScalar {
  param([string]$Database, [string]$Sql)
  $env:PGPASSWORD = $PgPassword
  try {
    $value = (& $PsqlPath -X -w -h $PgHost -p "$PgPort" -U $PgUser -d $Database -q -t -A -c $Sql) -join ''
    if ($LASTEXITCODE -ne 0) { throw "psql fallo con codigo $LASTEXITCODE en $Database" }
    return $value.Trim()
  } finally { Remove-Item Env:PGPASSWORD -ErrorAction SilentlyContinue }
}

function Normalize-Identifier {
  param([string]$Name, [hashtable]$Seen)
  $value = ($Name.Trim() -replace '^\uFEFF', '').Normalize([Text.NormalizationForm]::FormD)
  $value = [regex]::Replace($value, '\p{Mn}', '')
  $value = $value.ToLowerInvariant() -replace '[^a-z0-9_]+', '_'
  $value = $value.Trim('_')
  if (-not $value) { $value = 'col' }
  if ($value -match '^[0-9]') { $value = "c_$value" }
  $base = $value; $i = 2
  while ($Seen.ContainsKey($value)) { $value = "${base}_$i"; $i++ }
  $Seen[$value] = $true
  return $value
}

function Get-CsvColumns {
  param([string]$Path)
  $header = Get-Content -LiteralPath $Path -Encoding utf8 -TotalCount 1
  if (-not $header) { throw "CSV sin encabezado: $Path" }
  $seen = @{}
  return @($header.Split(',') | ForEach-Object { Normalize-Identifier -Name $_ -Seen $seen })
}

function Get-CanonicalColumns {
  param([string]$Table, [string[]]$Columns)
  if ($Table -like 'ingresos_*') {
    return @($Columns | ForEach-Object {
      if ($_ -eq 'des_cuenta') { 'cuenta' } else { $_ }
    })
  }
  return @($Columns)
}

function Quote-Ident { param([string]$Name); return '"' + ($Name -replace '"', '""') + '"' }
function Quote-Literal { param([string]$Value); return "'" + ($Value -replace "'", "''") + "'" }
function Copy-Path { param([string]$Path); return (($Path -replace '\\', '/') -replace "'", "''") }

function Numeric-Expression {
  param([string]$Column, [ValidateSet('integer','numeric')][string]$Type)
  $identifier = Quote-Ident $Column
  $pattern = if ($Type -eq 'integer') { "^-?[0-9]+$" } else { "^-?[0-9]+([.][0-9]+)?([Ee][+-]?[0-9]+)?$" }
  return "CASE WHEN BTRIM($identifier) ~ '$pattern' THEN BTRIM($identifier)::$Type END AS $identifier"
}

function Typed-Expression {
  param([string]$Table, [string]$Column)
  $numericColumns = @{
    atmosfera_clima_1991_2025 = @('idprovincia','ano','valor')
    atmosfera_clima_ca_2017_2023 = @('ano','precipitacion','temperatura_maxima','temperatura_minima','velocidad_del_viento','humedad_relativa','presion_atmosferica')
    atmosfera_clima_ca_old = @('ano','precipitacion','temperatura_maxima','temperatura_minima','presion_atmosferica','velocidad_del_viento','nubosidad','humedad_relativa')
    eventos_fenomenos_naturales = @('year','month','day','usa_wind','usa_pres')
    gastos_gobiernos_locales_2022 = @('periodo','devengado')
    gastos_gobiernos_locales_2023 = @('periodo','devengado')
    gastos_gobiernos_locales_2024 = @('periodo','devengado')
    ingresos_gobiernos_locales_2022 = @('periodo','percibido')
    ingresos_gobiernos_locales_2023 = @('periodo','percibido')
    ingresos_gobiernos_locales_2024 = @('periodo','percibido')
  }
  $integerColumns = @('idprovincia','ano','year','month','day','periodo')
  if ($numericColumns[$Table] -contains $Column) {
    $type = if ($integerColumns -contains $Column) { 'integer' } else { 'numeric' }
    return Numeric-Expression -Column $Column -Type $type
  }
  $identifier = Quote-Ident $Column
  return "NULLIF(BTRIM($identifier), '') AS $identifier"
}

$manifest = @(Import-Csv -LiteralPath $manifestPath)
if ($manifest.Count -ne 10) { throw "Se esperaban 10 tablas de ONE; el manifest contiene $($manifest.Count)." }
foreach ($row in $manifest) {
  $csvPath = Join-Path $CsvDir $row.csv
  if (-not (Test-Path -LiteralPath $csvPath)) { throw "Falta CSV: $csvPath" }
  if ((Get-FileHash -Algorithm SHA256 -LiteralPath $csvPath).Hash -ne $row.sha256) { throw "Hash inesperado: $csvPath" }
}

if (-not $GenerateOnly) {
  $exists = Get-PsqlScalar -Database postgres -Sql "SELECT 1 FROM pg_database WHERE datname='one_datos';"
  if (-not $exists) { Invoke-Psql -Database postgres -Arguments @('-v','ON_ERROR_STOP=1','-c','CREATE DATABASE one_datos;') }
}

$sqlLines = [Collections.Generic.List[string]]::new()
$sqlLines.Add('\set ON_ERROR_STOP on')
$sqlLines.Add('CREATE SCHEMA IF NOT EXISTS raw;')
$sqlLines.Add('CREATE SCHEMA IF NOT EXISTS meta;')
$sqlLines.Add('')

foreach ($row in $manifest) {
  $csvPath = Join-Path $CsvDir $row.csv
  $columns = @(Get-CanonicalColumns -Table $row.table -Columns (Get-CsvColumns $csvPath))
  $quotedColumns = ($columns | ForEach-Object { Quote-Ident $_ }) -join ', '
  $rawDefinitions = ($columns | ForEach-Object { "  $(Quote-Ident $_) text" }) -join ",`n"
  $typedExpressions = ($columns | ForEach-Object { "  $(Typed-Expression -Table $row.table -Column $_)" }) -join ",`n"
  $table = $row.table
  $copyPath = Copy-Path $csvPath
  $sqlLines.Add("DROP TABLE IF EXISTS raw.$table CASCADE;")
  $sqlLines.Add("CREATE TABLE raw.$table (`n$rawDefinitions`n);")
  $sqlLines.Add("\copy raw.$table ($quotedColumns) FROM '$( $copyPath )' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8', NULL '');")
  $sqlLines.Add('ANALYZE raw.' + $table + ';')
  $sqlLines.Add("DROP TABLE IF EXISTS public.$table CASCADE;")
  $sqlLines.Add("CREATE TABLE public.$table AS SELECT`n$typedExpressions`nFROM raw.$table;")
  $sqlLines.Add('ANALYZE public.' + $table + ';')
  $sqlLines.Add('')
}

$sqlLines.Add('DROP VIEW IF EXISTS public.gastos_gobiernos_locales CASCADE;')
$sqlLines.Add('CREATE VIEW public.gastos_gobiernos_locales AS')
$sqlLines.Add('SELECT 2022 AS fuente_anio, t.* FROM public.gastos_gobiernos_locales_2022 t')
$sqlLines.Add('UNION ALL SELECT 2023, t.* FROM public.gastos_gobiernos_locales_2023 t')
$sqlLines.Add('UNION ALL SELECT 2024, t.* FROM public.gastos_gobiernos_locales_2024 t;')
$sqlLines.Add('DROP VIEW IF EXISTS public.ingresos_gobiernos_locales CASCADE;')
$sqlLines.Add('CREATE VIEW public.ingresos_gobiernos_locales AS')
$sqlLines.Add('SELECT 2022 AS fuente_anio, t.* FROM public.ingresos_gobiernos_locales_2022 t')
$sqlLines.Add('UNION ALL SELECT 2023, t.* FROM public.ingresos_gobiernos_locales_2023 t')
$sqlLines.Add('UNION ALL SELECT 2024, t.* FROM public.ingresos_gobiernos_locales_2024 t;')

$sqlLines.Add('CREATE INDEX IF NOT EXISTS gastos_locales_2022_territorio_idx ON public.gastos_gobiernos_locales_2022 (cod_region, cod_provincia, cod_municipio);')
$sqlLines.Add('CREATE INDEX IF NOT EXISTS gastos_locales_2023_territorio_idx ON public.gastos_gobiernos_locales_2023 (cod_region, cod_provincia, cod_municipio);')
$sqlLines.Add('CREATE INDEX IF NOT EXISTS gastos_locales_2024_territorio_idx ON public.gastos_gobiernos_locales_2024 (cod_region, cod_provincia, cod_municipio);')
$sqlLines.Add('CREATE INDEX IF NOT EXISTS ingresos_locales_2022_territorio_idx ON public.ingresos_gobiernos_locales_2022 (cod_region, cod_provincia, cod_municipio);')
$sqlLines.Add('CREATE INDEX IF NOT EXISTS ingresos_locales_2023_territorio_idx ON public.ingresos_gobiernos_locales_2023 (cod_region, cod_provincia, cod_municipio);')
$sqlLines.Add('CREATE INDEX IF NOT EXISTS ingresos_locales_2024_territorio_idx ON public.ingresos_gobiernos_locales_2024 (cod_region, cod_provincia, cod_municipio);')
$sqlLines.Add('CREATE INDEX IF NOT EXISTS clima_largo_territorio_idx ON public.atmosfera_clima_1991_2025 (idprovincia, ano, mes);')
$sqlLines.Add('CREATE INDEX IF NOT EXISTS eventos_naturales_fecha_idx ON public.eventos_fenomenos_naturales (year, month, day);')

$sqlLines.Add('DROP TABLE IF EXISTS meta.fuentes;')
$sqlLines.Add('CREATE TABLE meta.fuentes (tabla text PRIMARY KEY, dataset text NOT NULL, archivo text NOT NULL, hoja text NOT NULL, filas bigint NOT NULL, csv_sha256 text NOT NULL, url text NOT NULL, cargado_en timestamptz NOT NULL DEFAULT now());')
foreach ($row in $manifest) {
  $sqlLines.Add("INSERT INTO meta.fuentes (tabla,dataset,archivo,hoja,filas,csv_sha256,url) VALUES ($(Quote-Literal $row.table),$(Quote-Literal $row.dataset),$(Quote-Literal $row.workbook),$(Quote-Literal $row.sheet),$($row.rows),$(Quote-Literal $row.sha256),$(Quote-Literal $row.source_url));")
}
$sqlLines.Add('DROP TABLE IF EXISTS meta.calidad;')
$sqlLines.Add('CREATE TABLE meta.calidad (tabla text PRIMARY KEY, filas_fuente bigint NOT NULL, filas_cargadas bigint NOT NULL, estado text NOT NULL, medido_en timestamptz NOT NULL DEFAULT now());')
foreach ($row in $manifest) {
  $sqlLines.Add("INSERT INTO meta.calidad (tabla,filas_fuente,filas_cargadas,estado) VALUES ($(Quote-Literal $row.table),$($row.rows),(SELECT COUNT(*) FROM public.$($row.table)),CASE WHEN (SELECT COUNT(*) FROM public.$($row.table))=$($row.rows) THEN 'OK' ELSE 'ERROR' END);")
}
$sqlLines.Add('DO $$ BEGIN IF EXISTS (SELECT 1 FROM meta.calidad WHERE estado <> ''OK'') THEN RAISE EXCEPTION ''Conteo inesperado en una tabla ONE''; END IF; END $$;')
$sqlLines.Add('GRANT CONNECT ON DATABASE one_datos TO analitica_lectura;')
$sqlLines.Add('GRANT USAGE ON SCHEMA public, meta TO analitica_lectura;')
$sqlLines.Add('GRANT SELECT ON ALL TABLES IN SCHEMA public, meta TO analitica_lectura;')
$sqlLines.Add('GRANT SELECT ON ALL SEQUENCES IN SCHEMA public, meta TO analitica_lectura;')

$generatedDir = Join-Path $PSScriptRoot 'generated'
New-Item -ItemType Directory -Force -Path $generatedDir | Out-Null
$sqlPath = Join-Path $generatedDir 'load-one-datasets.sql'
[IO.File]::WriteAllText($sqlPath, ($sqlLines -join "`n"), [Text.UTF8Encoding]::new($false))
if ($GenerateOnly) {
  Write-Host "SQL generado: $sqlPath"
  exit 0
}
Invoke-Psql -Database postgres -Arguments @('-v','ON_ERROR_STOP=1','-c',"DO `$`$ BEGIN IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname='analitica_lectura') THEN CREATE ROLE analitica_lectura NOLOGIN; END IF; END `$`$;")
Invoke-Psql -Database one_datos -Arguments @('-f', $sqlPath)
Invoke-Psql -Database one_datos -Arguments @('-P','pager=off','-c','SELECT current_database() AS database, pg_size_pretty(pg_database_size(current_database())) AS size, COUNT(*) AS tablas FROM information_schema.tables WHERE table_schema = ''public'';')
Write-Host 'one_datos cargada y validada.'
