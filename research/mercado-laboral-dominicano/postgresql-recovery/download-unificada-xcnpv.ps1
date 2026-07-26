param(
  [string]$Destination = 'D:\datos_one_censos\2022\BD_FINAL_VIVIENDA_HOGAR_PERSONA_XCNPV_PUB.csv',
  [int]$ChunkSizeMB = 64
)

$ErrorActionPreference = 'Stop'

$url = 'https://web.archive.org/web/20250722074939id_/https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/BD_XCNPV/BD_FINAL_VIVIENDA_HOGAR_PERSONA_XCNPV_PUB.csv'
$expectedBytes = 2252276900L
$expectedSha256 = '7E848405B2743774FBC0445BB9412F51007C064F61DC1C22CA77913613D15D70'
$allowedRoot = [IO.Path]::GetFullPath('D:\datos_one_censos\2022')
$destinationPath = [IO.Path]::GetFullPath($Destination)

if (-not $destinationPath.StartsWith($allowedRoot, [StringComparison]::OrdinalIgnoreCase)) {
  throw "El destino debe permanecer dentro de $allowedRoot"
}
if ($ChunkSizeMB -lt 8 -or $ChunkSizeMB -gt 256) {
  throw 'ChunkSizeMB debe estar entre 8 y 256.'
}

$destinationDir = Split-Path -Parent $destinationPath
New-Item -ItemType Directory -Force -Path $destinationDir | Out-Null

$currentBytes = if (Test-Path -LiteralPath $destinationPath) {
  (Get-Item -LiteralPath $destinationPath).Length
} else {
  0L
}

if ($currentBytes -gt $expectedBytes) {
  throw "El archivo existente es mayor que el original esperado: $currentBytes > $expectedBytes bytes."
}
if ($currentBytes -eq $expectedBytes) {
  Write-Host "La descarga ya esta completa: $destinationPath"
  $hash = Get-FileHash -LiteralPath $destinationPath -Algorithm SHA256
  if ($hash.Hash -ne $expectedSha256) {
    throw "SHA256 inesperado: $($hash.Hash)"
  }
  $hash
  exit 0
}

$chunkBytes = [int64]$ChunkSizeMB * 1MB
$chunkPath = "$destinationPath.chunk"

try {
  while ($currentBytes -lt $expectedBytes) {
    $endByte = [Math]::Min($currentBytes + $chunkBytes - 1L, $expectedBytes - 1L)
    $expectedChunkBytes = $endByte - $currentBytes + 1L
    $range = "$currentBytes-$endByte"
    $completed = $false

    for ($attempt = 1; $attempt -le 6 -and -not $completed; $attempt++) {
      if (Test-Path -LiteralPath $chunkPath) {
        Remove-Item -LiteralPath $chunkPath -Force
      }

      & curl.exe -L --fail --silent --show-error --range $range $url --output $chunkPath
      $curlExit = $LASTEXITCODE
      $actualChunkBytes = if (Test-Path -LiteralPath $chunkPath) {
        (Get-Item -LiteralPath $chunkPath).Length
      } else {
        0L
      }

      if ($curlExit -eq 0 -and $actualChunkBytes -eq $expectedChunkBytes) {
        $completed = $true
      } else {
        Write-Warning "Fallo en rango $range (intento $attempt/6; curl=$curlExit; bytes=$actualChunkBytes)."
        Start-Sleep -Seconds ([Math]::Min(2 * $attempt, 10))
      }
    }

    if (-not $completed) {
      throw "No se pudo descargar correctamente el rango $range despues de 6 intentos."
    }

    $target = [IO.File]::Open($destinationPath, [IO.FileMode]::Append, [IO.FileAccess]::Write, [IO.FileShare]::Read)
    try {
      $source = [IO.File]::OpenRead($chunkPath)
      try {
        $source.CopyTo($target)
      } finally {
        $source.Dispose()
      }
    } finally {
      $target.Dispose()
    }

    $currentBytes = (Get-Item -LiteralPath $destinationPath).Length
    $pct = [Math]::Round(100.0 * $currentBytes / $expectedBytes, 1)
    Write-Host "Descargados $currentBytes / $expectedBytes bytes ($pct%)."
  }
} finally {
  if (Test-Path -LiteralPath $chunkPath) {
    Remove-Item -LiteralPath $chunkPath -Force
  }
}

if ((Get-Item -LiteralPath $destinationPath).Length -ne $expectedBytes) {
  throw 'La descarga termino con un tamano distinto del publicado por el servidor.'
}

Write-Host "Descarga completa: $destinationPath"
$hash = Get-FileHash -LiteralPath $destinationPath -Algorithm SHA256
if ($hash.Hash -ne $expectedSha256) {
  throw "SHA256 inesperado: $($hash.Hash)"
}
$hash
