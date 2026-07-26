param(
  [string]$PgHost = '127.0.0.1',
  [int]$PgPort = 5433,
  [string]$PgUser = 'postgres',
  [string]$Password = $env:PG_RECOVERY_PASSWORD,
  [string]$BackupRoot = 'C:\Users\leona\PostgreSQL_Backups',
  [int]$Jobs = 4
)

$ErrorActionPreference = 'Stop'

if ([string]::IsNullOrWhiteSpace($Password)) {
  throw 'Defina PG_RECOVERY_PASSWORD o use -Password. La clave no se guarda en el repositorio.'
}
if ($Jobs -lt 1 -or $Jobs -gt 8) {
  throw 'Jobs debe estar entre 1 y 8.'
}

$pgBin = 'C:\Program Files\PostgreSQL\18\bin'
$pgDump = Join-Path $pgBin 'pg_dump.exe'
$pgDumpAll = Join-Path $pgBin 'pg_dumpall.exe'
$pgRestore = Join-Path $pgBin 'pg_restore.exe'
$stamp = Get-Date -Format 'yyyyMMdd_HHmmss'
$backupDir = Join-Path $BackupRoot "recovered-analytics-$stamp"
$databases = @(
  'censo_2002',
  'censo_2010',
  'censo_2022',
  'censos_linea_tiempo',
  'enhogar_2024',
  'postgres'
)

New-Item -ItemType Directory -Force -Path $backupDir | Out-Null
$env:PGPASSWORD = $Password

try {
  $globalsPath = Join-Path $backupDir 'globals.sql'
  & $pgDumpAll -w -h $PgHost -p "$PgPort" -U $PgUser --globals-only --no-role-passwords --file $globalsPath
  if ($LASTEXITCODE -ne 0) {
    throw "pg_dumpall fallo con codigo $LASTEXITCODE."
  }

  $manifest = [Collections.Generic.List[object]]::new()
  foreach ($db in $databases) {
    $target = Join-Path $backupDir $db
    Write-Host "Respaldando $db en $target..."
    & $pgDump -w -h $PgHost -p "$PgPort" -U $PgUser -d $db `
      --format=directory --jobs=$Jobs --compress=zstd:6 --file $target
    if ($LASTEXITCODE -ne 0) {
      throw "pg_dump fallo para $db con codigo $LASTEXITCODE."
    }

    & $pgRestore --list $target | Out-Null
    if ($LASTEXITCODE -ne 0) {
      throw "pg_restore no pudo leer el respaldo de $db."
    }

    $bytes = (Get-ChildItem -LiteralPath $target -Recurse -File | Measure-Object Length -Sum).Sum
    $manifest.Add([pscustomobject]@{
      database = $db
      format = 'directory-zstd-6'
      bytes = [int64]$bytes
      path = $target
      verified_with_pg_restore_list = $true
      created_at = (Get-Date).ToString('o')
    })
  }

  $manifestPath = Join-Path $backupDir 'backup-manifest.csv'
  $manifest | Export-Csv -LiteralPath $manifestPath -NoTypeInformation -Encoding UTF8

  $readmePath = Join-Path $backupDir 'RESTORE.txt'
  @(
    'Respaldo logico PostgreSQL 18 en formato directory con Zstandard.',
    '1. Restaurar roles: psql -d postgres -f globals.sql',
    '2. Crear la base destino vacia.',
    '3. Restaurar: pg_restore -j 4 -d BASE RUTA_DEL_DIRECTORIO',
    'Los mapeos FDW pueden contener secretos; proteger este directorio como informacion sensible.'
  ) | Set-Content -LiteralPath $readmePath -Encoding UTF8

  Write-Host "Respaldo completo y legible: $backupDir"
} finally {
  Remove-Item Env:\PGPASSWORD -ErrorAction SilentlyContinue
}
