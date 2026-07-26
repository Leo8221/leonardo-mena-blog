param(
  [string]$Password = $env:PG_RECOVERY_PASSWORD
)

$ErrorActionPreference = 'Stop'

if ([string]::IsNullOrWhiteSpace($Password)) {
  throw 'Defina PG_RECOVERY_PASSWORD o use -Password. La clave no se guarda en el repositorio.'
}

Remove-Item Env:LC_ALL -ErrorAction SilentlyContinue
Remove-Item Env:LC_CTYPE -ErrorAction SilentlyContinue
Remove-Item Env:LANG -ErrorAction SilentlyContinue

$env:PG_RECOVERY_PASSWORD = $Password
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$script = Join-Path $PSScriptRoot 'build-grafico-parejas.R'

Push-Location $repo
try {
  Rscript $script
  if ($LASTEXITCODE -ne 0) {
    throw "Rscript fallo con codigo $LASTEXITCODE."
  }
} finally {
  Pop-Location
  Remove-Item Env:PG_RECOVERY_PASSWORD -ErrorAction SilentlyContinue
}
