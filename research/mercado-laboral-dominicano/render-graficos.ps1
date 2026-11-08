$ErrorActionPreference = 'Stop'

$repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$script = Join-Path $PSScriptRoot 'build-graficos.R'

# El wrapper común fija R, comprueba UTF-8 y trabaja desde la raíz.
& (Join-Path $repo 'tools\run-r.ps1') $script
