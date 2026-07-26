param(
  [string]$PgHost = '127.0.0.1',
  [int]$PgPort = 5433,
  [string]$PgUser = 'postgres',
  [string]$Password = $env:PG_RECOVERY_PASSWORD,
  [string]$UnificadaCsv = 'D:\datos_one_censos\2022\BD_FINAL_VIVIENDA_HOGAR_PERSONA_XCNPV_PUB.csv',
  [switch]$ForceReload,
  [switch]$ForceDerived
)

$ErrorActionPreference = 'Stop'

if ([string]::IsNullOrWhiteSpace($Password)) {
  throw 'Defina PG_RECOVERY_PASSWORD o use -Password. La clave no se guarda en el repositorio.'
}

$pgBin = 'C:\Program Files\PostgreSQL\18\bin'
$psql = Join-Path $pgBin 'psql.exe'
$moduleDir = $PSScriptRoot
$expectedBytes = 2252276900L
$expectedSha256 = '7E848405B2743774FBC0445BB9412F51007C064F61DC1C22CA77913613D15D70'

if (-not (Test-Path -LiteralPath $psql)) {
  throw "No se encontro psql: $psql"
}
if (-not (Test-Path -LiteralPath $UnificadaCsv)) {
  throw "No se encontro la base unificada: $UnificadaCsv"
}
if ((Get-Item -LiteralPath $UnificadaCsv).Length -ne $expectedBytes) {
  throw "La base unificada no esta completa; se esperaban $expectedBytes bytes."
}
$actualSha256 = (Get-FileHash -LiteralPath $UnificadaCsv -Algorithm SHA256).Hash
if ($actualSha256 -ne $expectedSha256) {
  throw "La base unificada tiene un SHA256 inesperado: $actualSha256"
}

function Invoke-Psql {
  param(
    [Parameter(Mandatory)][string]$Database,
    [Parameter(Mandatory)][string[]]$Arguments
  )

  & $psql -X -w -h $PgHost -p "$PgPort" -U $PgUser -d $Database @Arguments
  if ($LASTEXITCODE -ne 0) {
    throw "psql fallo en $Database con codigo $LASTEXITCODE."
  }
}

function Get-PsqlScalar {
  param(
    [Parameter(Mandatory)][string]$Database,
    [Parameter(Mandatory)][string]$Sql
  )

  $value = & $psql -X -w -h $PgHost -p "$PgPort" -U $PgUser -d $Database -q -t -A -c $Sql
  if ($LASTEXITCODE -ne 0) {
    throw "No se pudo consultar $Database."
  }
  return (($value -join '')).Trim()
}

$env:PGPASSWORD = $Password
try {
  $tableExists = Get-PsqlScalar -Database 'censo_2022' -Sql "SELECT to_regclass('analitica.xcnpv_unificada') IS NOT NULL;"
  $layerReady = Get-PsqlScalar -Database 'censo_2022' -Sql "SELECT to_regclass('analitica.xcnpv_unificada_hogar_persona_idx') IS NOT NULL;"
  $existingRows = if ($tableExists -eq 't') {
    Get-PsqlScalar -Database 'censo_2022' -Sql 'SELECT COUNT(*) FROM analitica.xcnpv_unificada;'
  } else {
    '0'
  }

  if ($ForceReload -or $existingRows -ne '10773983') {
    Write-Host 'Cargando y tipando la base unificada XCNPV 2022...'
    $csvForPsql = $UnificadaCsv.Replace('\', '/')
    Invoke-Psql -Database 'censo_2022' -Arguments @(
      '-v', 'ON_ERROR_STOP=1',
      '-v', "unificada_csv=$csvForPsql",
      '-f', (Join-Path $moduleDir 'load-unificada-xcnpv.sql')
    )
  } elseif ($layerReady -ne 't') {
    Write-Host 'La tabla tipada existe; reanudando validaciones e indices...'
    Invoke-Psql -Database 'censo_2022' -Arguments @(
      '-v', 'ON_ERROR_STOP=1',
      '-f', (Join-Path $moduleDir 'finalize-unificada-xcnpv.sql')
    )
  } else {
    Write-Host 'La base unificada validada ya existe; se omite la recarga pesada.'
  }

  $derivedReady = Get-PsqlScalar -Database 'censo_2022' -Sql "SELECT to_regclass('analitica.parejas_jefatura_2022') IS NOT NULL AND to_regclass('analitica.matriz_campos_estudio_parejas_2022') IS NOT NULL AND to_regclass('analitica.matriz_ocupaciones_parejas_2022') IS NOT NULL;"
  if ($ForceReload -or $ForceDerived -or $derivedReady -ne 't') {
    Write-Host 'Creando la capa de relaciones y parejas...'
    Invoke-Psql -Database 'censo_2022' -Arguments @(
      '-v', 'ON_ERROR_STOP=1',
      '-f', (Join-Path $moduleDir 'create-relaciones-parejas-2022.sql')
    )
  } else {
    Write-Host 'La capa de parejas validada ya existe; use -ForceDerived para reconstruirla.'
  }

  Write-Host 'Corrigiendo la capa armonizada de 2022...'
  Invoke-Psql -Database 'censo_2022' -Arguments @(
    '-v', 'ON_ERROR_STOP=1',
    '-f', (Join-Path $moduleDir 'harmonize-censo-2022.sql')
  )

  Write-Host 'Registrando la limitacion de enlace de parejas en censo_2002...'
  Invoke-Psql -Database 'censo_2002' -Arguments @(
    '-v', 'ON_ERROR_STOP=1',
    '-f', (Join-Path $moduleDir 'mark-pairs-unavailable-2002.sql')
  )

  foreach ($historicalDb in @('censo_2010')) {
    $historicalReady = Get-PsqlScalar -Database $historicalDb -Sql "SELECT to_regclass('analitica.parejas_jefatura_historica') IS NOT NULL;"
    if ($ForceDerived -or $historicalReady -ne 't') {
      Write-Host "Creando parejas historicas en $historicalDb..."
      Invoke-Psql -Database $historicalDb -Arguments @(
        '-v', 'ON_ERROR_STOP=1',
        '-f', (Join-Path $moduleDir 'create-parejas-historicas.sql')
      )
    } else {
      Write-Host "La capa historica de $historicalDb ya existe; se conserva."
    }
  }

  Write-Host 'Cargando las bases adicionales de ONE en one_datos...'
  $previousRecoveryPassword = $env:PG_RECOVERY_PASSWORD
  $env:PG_RECOVERY_PASSWORD = $Password
  try {
    & (Join-Path $moduleDir 'load-one-datasets.ps1') `
      -CsvDir (Join-Path $moduleDir '..\data\raw\one_csv') `
      -PgHost $PgHost -PgPort $PgPort -PgUser $PgUser
    if ($LASTEXITCODE -and $LASTEXITCODE -ne 0) {
      throw "La carga de one_datos fallo con codigo $LASTEXITCODE."
    }
  } finally {
    if ($null -eq $previousRecoveryPassword) {
      Remove-Item Env:\PG_RECOVERY_PASSWORD -ErrorAction SilentlyContinue
    } else {
      $env:PG_RECOVERY_PASSWORD = $previousRecoveryPassword
    }
  }

  Write-Host 'Ampliando el catalogo central y conectando ENHOGAR 2024 y one_datos...'
  Invoke-Psql -Database 'censos_linea_tiempo' -Arguments @(
    '-v', 'ON_ERROR_STOP=1',
    '-v', "fdw_password=$Password",
    '-f', (Join-Path $moduleDir 'extend-central-analytics.sql')
  )

  Write-Host 'Validando la capa analitica...'
  Invoke-Psql -Database 'censo_2022' -Arguments @(
    '-v', 'ON_ERROR_STOP=1',
    '-P', 'pager=off',
    '-f', (Join-Path $moduleDir 'verify-analytical-layer.sql')
  )

  Write-Host 'Preparacion analitica completada.'
} finally {
  Remove-Item Env:\PGPASSWORD -ErrorAction SilentlyContinue
}
