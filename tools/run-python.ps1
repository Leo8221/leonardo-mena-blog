param(
  [Parameter(Mandatory = $true, Position = 0)]
  [string]$Script,
  [Parameter(ValueFromRemainingArguments = $true, Position = 1)]
  [string[]]$ScriptArgs
)

$ErrorActionPreference = 'Stop'

$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$python = $null
$registryCandidates = @(
  # Preferir el Python instalado, no los que vienen dentro de otras aplicaciones.
  'HKCU:\Software\Python\PythonCore\3.12\InstallPath',
  'HKLM:\Software\Python\PythonCore\3.14\InstallPath'
)
foreach ($key in $registryCandidates) {
  if (Test-Path $key) {
    $candidate = (Get-ItemProperty -LiteralPath $key -ErrorAction SilentlyContinue).ExecutablePath
    if ($candidate -and (Test-Path -LiteralPath $candidate)) {
      $python = $candidate
      break
    }
  }
}
if (-not $python) {
  throw 'No se encontro un Python instalado registrado. No se usara un interprete embebido de otra aplicacion.'
}

$scriptPath = if ([IO.Path]::IsPathRooted($Script)) {
  $Script
} else {
  Join-Path $repoRoot $Script
}
if (-not (Test-Path -LiteralPath $scriptPath)) {
  throw "No existe el script Python: $scriptPath"
}

$pythonDir = Split-Path -Parent $python
$env:Path = "$pythonDir;$env:Path"
Push-Location $repoRoot
# Los scripts usan rutas relativas al repositorio. finally devuelve la terminal
# a la carpeta desde la que los llamaste, incluso si Python falla.
try {
  # UTF-8 también en consola y tuberías; no depende de la página de códigos de Windows.
  & $python -X utf8 $scriptPath @ScriptArgs
  if ($LASTEXITCODE -ne 0) {
    throw "Python fallo con codigo $LASTEXITCODE."
  }
} finally {
  Pop-Location
}
