param(
    [string]$ThesisRoot = 'C:\Users\leona\Archivo_Local\Documentos\tesis_monetaria_rd'
)

$ErrorActionPreference = 'Stop'
$repoRoot = Split-Path -Parent $PSScriptRoot
$targetDir = Join-Path $repoRoot 'research\camus-mipyme\data\raw\tesis'
New-Item -ItemType Directory -Force -Path $targetDir | Out-Null

$files = @(
    'scr\enmipymes_r.csv',
    'datos\sb\csv\carteras__creditos__sectores-economicos.csv',
    'datos\sb\csv\carteras__creditos__localidad.csv'
)
$rows = foreach ($relative in $files) {
    $source = Join-Path $ThesisRoot $relative
    if (-not (Test-Path -LiteralPath $source -PathType Leaf)) {
        throw "No existe el archivo de tesis: $source"
    }
    $localName = Split-Path -Leaf $relative
    if ($localName -eq 'carteras__creditos__sectores-economicos.csv') { $localName = 'sb_carteras_creditos_sectores_economicos.csv' }
    if ($localName -eq 'carteras__creditos__localidad.csv') { $localName = 'sb_carteras_creditos_localidad.csv' }
    $target = Join-Path $targetDir $localName
    Copy-Item -LiteralPath $source -Destination $target -Force
    [pscustomobject]@{
        source_path = $source
        local_file = $localName
        bytes = (Get-Item -LiteralPath $target).Length
        sha256 = (Get-FileHash -LiteralPath $target -Algorithm SHA256).Hash
    }
}

$manifest = Join-Path $targetDir 'tesis-mipyme-manifest.csv'
$rows | Export-Csv -LiteralPath $manifest -NoTypeInformation -Encoding UTF8
Write-Output "Manifest: $manifest"
$rows | ForEach-Object { Write-Output "$($_.local_file)|$($_.bytes)|$($_.sha256)" }
