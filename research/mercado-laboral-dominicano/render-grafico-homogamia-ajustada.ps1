param(
  [string]$Password = $env:PG_RECOVERY_PASSWORD,
  [switch]$UseCache
)

$ErrorActionPreference = 'Stop'

if (-not $UseCache -and [string]::IsNullOrWhiteSpace($Password)) {
  throw 'Defina PG_RECOVERY_PASSWORD o use -Password. La clave no se guarda en el repositorio.'
}

Remove-Item Env:LC_ALL -ErrorAction SilentlyContinue
Remove-Item Env:LC_CTYPE -ErrorAction SilentlyContinue
Remove-Item Env:LANG -ErrorAction SilentlyContinue

if ($UseCache) {
  $env:HOMOGAMIA_USE_CACHE = '1'
} else {
  $env:HOMOGAMIA_USE_CACHE = '0'
  $env:PG_RECOVERY_PASSWORD = $Password
}
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$script = Join-Path $PSScriptRoot 'build-grafico-homogamia-ajustada.R'

Push-Location $repo
try {
  Rscript $script
  if ($LASTEXITCODE -ne 0) {
    throw "Rscript fallo con codigo $LASTEXITCODE."
  }
} finally {
  Pop-Location
  Remove-Item Env:PG_RECOVERY_PASSWORD -ErrorAction SilentlyContinue
  Remove-Item Env:HOMOGAMIA_USE_CACHE -ErrorAction SilentlyContinue
}
