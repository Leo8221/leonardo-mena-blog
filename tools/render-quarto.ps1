param([string[]]$QuartoArgs = @('render'))
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot 'runtime.ps1')
$runtime = Initialize-BlogRuntime -WithQuarto
Push-Location $runtime.Root
try {
  & $runtime.Quarto @QuartoArgs
  if ($LASTEXITCODE -ne 0) { throw "Quarto fallo con codigo $LASTEXITCODE." }
} finally { Pop-Location }
