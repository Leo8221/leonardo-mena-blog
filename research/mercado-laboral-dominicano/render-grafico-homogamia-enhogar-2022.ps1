param(
  [switch]$UseCache,
  [ValidateSet('2022', '2024')]
  [string]$Year = '2022'
)

$ErrorActionPreference = 'Stop'

Remove-Item Env:LC_ALL -ErrorAction SilentlyContinue
Remove-Item Env:LC_CTYPE -ErrorAction SilentlyContinue
Remove-Item Env:LANG -ErrorAction SilentlyContinue

$repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$script = Join-Path $PSScriptRoot 'build-grafico-homogamia-enhogar-2022.R'

Push-Location $repo
try {
  Rscript $script "--year=$Year"
  if ($LASTEXITCODE -ne 0) {
    throw "Rscript fallo con codigo $LASTEXITCODE."
  }
} finally {
  Pop-Location
}
