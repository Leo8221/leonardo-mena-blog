param(
  [Parameter(Mandatory = $true, Position = 0)]
  [string]$Script,
  [Parameter(ValueFromRemainingArguments = $true, Position = 1)]
  [string[]]$ScriptArgs
)

$ErrorActionPreference = 'Stop'

$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$rHome = 'C:\Program Files\R\R-4.5.3'
$rScript = Join-Path $rHome 'bin\x64\Rscript.exe'
$rUserLibrary = Join-Path $repoRoot '.r-library\4.5'

foreach ($requiredPath in @($rScript, $rUserLibrary)) {
  if (-not (Test-Path -LiteralPath $requiredPath)) {
    if ($requiredPath -eq $rUserLibrary) {
      New-Item -ItemType Directory -Force -Path $requiredPath | Out-Null
    } else {
      throw "No se encontro el recurso requerido: $requiredPath"
    }
  }
}

$scriptPath = if ([IO.Path]::IsPathRooted($Script)) {
  $Script
} else {
  Join-Path $repoRoot $Script
}
if (-not (Test-Path -LiteralPath $scriptPath)) {
  throw "No existe el script R: $scriptPath"
}

$env:R_HOME = $rHome
$env:R_LIBS_USER = $rUserLibrary
$env:Path = "$(Join-Path $rHome 'bin\x64');$(Join-Path $rHome 'bin');$env:Path"
foreach ($localeVariable in @('LC_ALL', 'LC_CTYPE', 'LC_COLLATE', 'LC_MONETARY', 'LC_TIME', 'LANG')) {
  Remove-Item -Path "Env:$localeVariable" -ErrorAction SilentlyContinue
}

& $rScript $scriptPath @ScriptArgs
if ($LASTEXITCODE -ne 0) {
  throw "Rscript fallo con codigo $LASTEXITCODE."
}
