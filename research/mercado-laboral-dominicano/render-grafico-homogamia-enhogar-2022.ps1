param(
  [switch]$UseCache,
  [ValidateSet('2022', '2024')]
  [string]$Year = '2022'
)

$ErrorActionPreference = 'Stop'

$repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$script = Join-Path $PSScriptRoot 'build-grafico-homogamia-enhogar-2022.R'

# El wrapper común fija R, comprueba UTF-8 y trabaja desde la raíz.
& (Join-Path $repo 'tools\run-r.ps1') $script "--year=$Year"
