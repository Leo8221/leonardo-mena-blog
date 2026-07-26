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
try {
  & $python $scriptPath @ScriptArgs
  if ($LASTEXITCODE -ne 0) {
    throw "Python fallo con codigo $LASTEXITCODE."
  }
} finally {
  Pop-Location
}
