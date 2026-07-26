param(
  [string]$DataDir = 'D:\PostgreSQL_Recuperado\18\data',
  [int]$Port = 5433,
  [string]$PgUser = 'postgres',
  [string]$InitDbPath = 'C:\Program Files\PostgreSQL\18\bin\initdb.exe'
)

$ErrorActionPreference = 'Stop'

if (-not $env:PG_RECOVERY_PASSWORD) {
  throw 'Define PG_RECOVERY_PASSWORD solo en la sesion antes de ejecutar.'
}

if (-not (Test-Path -LiteralPath $InitDbPath)) {
  throw "No existe initdb: $InitDbPath"
}

if (Test-Path -LiteralPath $DataDir) {
  $existing = @(Get-ChildItem -LiteralPath $DataDir -Force -ErrorAction Stop)
  if ($existing.Count -gt 0) {
    throw "El directorio no esta vacio; no se sobrescribira: $DataDir"
  }
} else {
  New-Item -ItemType Directory -Path $DataDir -Force | Out-Null
}

$passwordFile = Join-Path ([IO.Path]::GetTempPath()) ("pg-recovery-{0}.pw" -f [guid]::NewGuid())
try {
  [IO.File]::WriteAllText($passwordFile, $env:PG_RECOVERY_PASSWORD, [Text.UTF8Encoding]::new($false))
  & $InitDbPath @(
    '-D', $DataDir,
    '-U', $PgUser,
    '--pwfile', $passwordFile,
    '--auth=scram-sha-256',
    '--encoding=UTF8',
    '--locale-provider=icu',
    '--icu-locale=es-DO',
    '--text-search-config=spanish',
    '--data-checksums',
    '-c', "port=$Port",
    '-c', "listen_addresses=localhost",
    '-c', 'password_encryption=scram-sha-256',
    '-c', 'shared_buffers=512MB',
    '-c', 'maintenance_work_mem=1GB',
    '-c', 'max_wal_size=8GB',
    '-c', 'checkpoint_timeout=30min',
    '-c', 'wal_compression=on',
    '-c', 'logging_collector=on',
    '-c', 'log_directory=log',
    '-c', 'log_filename=postgresql-%Y-%m-%d_%H%M%S.log'
  )
  if ($LASTEXITCODE -ne 0) {
    throw "initdb fallo con codigo $LASTEXITCODE"
  }
} finally {
  Remove-Item -LiteralPath $passwordFile -Force -ErrorAction SilentlyContinue
  Remove-Item Env:PG_RECOVERY_PASSWORD -ErrorAction SilentlyContinue
}

Write-Host "Cluster inicializado: $DataDir"
Write-Host "Puerto configurado: $Port"

