param([string[]]$QuartoArgs = @('render'))
# Sin argumentos genera docs/; no hace commit ni push. Para revisar borradores, pasa preview y
# --profile editor; Quarto usa entonces la salida temporal de ese perfil.
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot 'runtime.ps1')
$runtime = Initialize-BlogRuntime -WithQuarto
Push-Location $runtime.Root
try {
  & $runtime.Quarto @QuartoArgs
  if ($LASTEXITCODE -ne 0) { throw "Quarto fallo con codigo $LASTEXITCODE." }
} finally { Pop-Location }
