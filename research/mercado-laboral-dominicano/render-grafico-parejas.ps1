param(
  [string]$Password = $env:PG_RECOVERY_PASSWORD
)

$ErrorActionPreference = 'Stop'

if ([string]::IsNullOrWhiteSpace($Password)) {
  throw 'Defina PG_RECOVERY_PASSWORD o use -Password. La clave no se guarda en el repositorio.'
}

$env:PG_RECOVERY_PASSWORD = $Password
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$script = Join-Path $PSScriptRoot 'build-grafico-parejas.R'

try {
  # El wrapper común fija R, comprueba UTF-8 y trabaja desde la raíz.
  & (Join-Path $repo 'tools\run-r.ps1') $script
} finally {
  Remove-Item Env:PG_RECOVERY_PASSWORD -ErrorAction SilentlyContinue
}
