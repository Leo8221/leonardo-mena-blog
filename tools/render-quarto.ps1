param(
  [string[]]$QuartoArgs = @('render')
)

$ErrorActionPreference = 'Stop'

$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$quarto = 'C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe'
$rHome = 'C:\Program Files\R\R-4.5.3'
$rBin = Join-Path $rHome 'bin\x64'
$rUserLibrary = Join-Path $repoRoot '.r-library\4.5'

foreach ($requiredPath in @($quarto, (Join-Path $rBin 'Rscript.exe'), $rUserLibrary)) {
  if (-not (Test-Path -LiteralPath $requiredPath)) {
    if ($requiredPath -eq $rUserLibrary) {
      New-Item -ItemType Directory -Force -Path $requiredPath | Out-Null
    } else {
      throw "No se encontro el recurso requerido: $requiredPath"
    }
  }
}

# Entorno reproducible del proyecto: no modifica la configuracion global del usuario.
$env:R_HOME = $rHome
$env:R_LIBS_USER = $rUserLibrary
$env:Path = "$rBin;$rHome\bin;$env:Path"
foreach ($localeVariable in @('LC_ALL', 'LC_CTYPE', 'LC_COLLATE', 'LC_MONETARY', 'LC_TIME', 'LANG')) {
  Remove-Item -Path "Env:$localeVariable" -ErrorAction SilentlyContinue
}

# Mantiene la cache de Quarto dentro del proyecto cuando el perfil de Windows
# no permite escribir en AppData. Estas carpetas estan ignoradas por Git.
$localAppData = Join-Path $repoRoot '.quarto-localappdata'
$roamingAppData = Join-Path $repoRoot '.quarto-appdata'
New-Item -ItemType Directory -Force -Path $localAppData, $roamingAppData | Out-Null
$env:LOCALAPPDATA = $localAppData
$env:APPDATA = $roamingAppData

Push-Location $repoRoot
try {
  & $quarto @QuartoArgs
  if ($LASTEXITCODE -ne 0) {
    throw "Quarto fallo con codigo $LASTEXITCODE."
  }
} finally {
  Pop-Location
}
