param(
  [string]$Password = $env:PG_RECOVERY_PASSWORD,
  [switch]$UseCache
)

$ErrorActionPreference = 'Stop'

if (-not $UseCache -and [string]::IsNullOrWhiteSpace($Password)) {
  throw 'Defina PG_RECOVERY_PASSWORD o use -Password. La clave no se guarda en el repositorio.'
}

if ($UseCache) {
  $env:HOMOGAMIA_RIGOR_USE_CACHE = '1'
} else {
  $env:HOMOGAMIA_RIGOR_USE_CACHE = '0'
  $env:PG_RECOVERY_PASSWORD = $Password
}

$repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$script = Join-Path $PSScriptRoot 'build-grafico-homogamia-condicionada.R'

try {
  # El wrapper común fija R, comprueba UTF-8 y trabaja desde la raíz.
  & (Join-Path $repo 'tools\run-r.ps1') $script
} finally {
  Remove-Item Env:PG_RECOVERY_PASSWORD -ErrorAction SilentlyContinue
  Remove-Item Env:HOMOGAMIA_RIGOR_USE_CACHE -ErrorAction SilentlyContinue
}
