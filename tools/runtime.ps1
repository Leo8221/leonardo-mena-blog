function Initialize-BlogRuntime {
  param([switch]$WithQuarto)
  $root = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
  $rHome = 'C:\Program Files\R\R-4.5.3'
  $rBin = Join-Path $rHome 'bin\x64'
  $rScript = Join-Path $rBin 'Rscript.exe'
  $quarto = 'C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe'
  $library = Join-Path $root '.r-library\4.5'
  $required = @($rScript)
  if ($WithQuarto) { $required += $quarto }
  foreach ($file in $required) {
    if (-not (Test-Path -LiteralPath $file)) { throw "No se encontro la herramienta: $file. Revisa tools/runtime.ps1." }
  }
  New-Item -ItemType Directory -Force -Path $library | Out-Null
  $env:R_HOME = $rHome
  $env:R_LIBS_USER = $library
  $env:Path = "$rBin;$rHome\bin;$env:Path"
  foreach ($name in @('LC_ALL', 'LC_CTYPE', 'LC_COLLATE', 'LC_MONETARY', 'LC_TIME', 'LANG')) {
    Remove-Item -Path "Env:$name" -ErrorAction SilentlyContinue
  }
  & $rScript -e "if (!isTRUE(l10n_info()[['UTF-8']])) stop('R no esta leyendo UTF-8. Render cancelado.')"
  if ($LASTEXITCODE -ne 0) { throw 'La comprobacion UTF-8 de R fallo.' }
  if ($WithQuarto) {
    $env:LOCALAPPDATA = Join-Path $root '.quarto-localappdata'
    $env:APPDATA = Join-Path $root '.quarto-appdata'
    New-Item -ItemType Directory -Force -Path $env:LOCALAPPDATA, $env:APPDATA | Out-Null
  }
  return [pscustomobject]@{ Root = $root; RScript = $rScript; Quarto = $quarto }
}
