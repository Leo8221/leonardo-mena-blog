param(
  [Parameter(Mandatory = $true, Position = 0)]
  [string]$Script,
  [Parameter(ValueFromRemainingArguments = $true, Position = 1)]
  [string[]]$ScriptArgs
)
$ErrorActionPreference = 'Stop'
# El punto importa la función del entorno sin abrir otra sesión de PowerShell.
. (Join-Path $PSScriptRoot 'runtime.ps1')
$runtime = Initialize-BlogRuntime
$scriptPath = if ([IO.Path]::IsPathRooted($Script)) { $Script } else { Join-Path $runtime.Root $Script }
if (-not (Test-Path -LiteralPath $scriptPath)) { throw "No existe el script R: $scriptPath" }
Push-Location $runtime.Root
# Ejecutar desde la raíz mantiene válidas las rutas a datos y al tema común.
try {
  # @ScriptArgs reenvía cada argumento por separado, conservando rutas con espacios.
  & $runtime.RScript $scriptPath @ScriptArgs
  if ($LASTEXITCODE -ne 0) { throw "Rscript fallo con codigo $LASTEXITCODE." }
} finally { Pop-Location }
