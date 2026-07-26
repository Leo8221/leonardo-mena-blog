$ErrorActionPreference = 'Stop'

Remove-Item Env:LC_ALL -ErrorAction SilentlyContinue
Remove-Item Env:LC_CTYPE -ErrorAction SilentlyContinue
Remove-Item Env:LANG -ErrorAction SilentlyContinue

$repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$script = Join-Path $PSScriptRoot 'build-graficos.R'

Push-Location $repo
try {
    Rscript $script
} finally {
    Pop-Location
}
