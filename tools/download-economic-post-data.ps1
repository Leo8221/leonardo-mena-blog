$ErrorActionPreference = 'Stop'

$root = Split-Path -Parent $PSScriptRoot
$encftDir = Join-Path $root 'atlas\data\raw\bcrd-encft'
$ipcDir = Join-Path $root 'atlas\data\raw\bcrd-precios'
$remesasDir = Join-Path $root 'atlas\data\raw\bcrd-sector-externo'
$electricDir = Join-Path $root 'research\sector-electrico-dominicano\data\raw\mem'
$laborLawDir = Join-Path $root 'research\trampa-empleo-informal\data\raw\legal'

New-Item -ItemType Directory -Force -Path $encftDir, $ipcDir, $remesasDir, $electricDir, $laborLawDir | Out-Null

$downloads = @(
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/01_PET.xlsx'; Path = (Join-Path $encftDir '01_PET.xlsx'); Source = 'BCRD ENCFT' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/1_2_Ocupados_Ocupacion.xlsx'; Path = (Join-Path $encftDir '1_2_Ocupados_Ocupacion.xlsx'); Source = 'BCRD ENCFT' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/1_3_Ocupados_Categoria.xlsx'; Path = (Join-Path $encftDir '1_3_Ocupados_Categoria.xlsx'); Source = 'BCRD ENCFT' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/1_4_Ocupados_Edad.xlsx'; Path = (Join-Path $encftDir '1_4_Ocupados_Edad.xlsx'); Source = 'BCRD ENCFT' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/1_5_Ocupados_Educacion.xlsx'; Path = (Join-Path $encftDir '1_5_Ocupados_Educacion.xlsx'); Source = 'BCRD ENCFT' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/2_2_Sectores_Ocupacion.xlsx'; Path = (Join-Path $encftDir '2_2_Sectores_Ocupacion.xlsx'); Source = 'BCRD ENCFT' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/2_3_Sectores_Categoria.xlsx'; Path = (Join-Path $encftDir '2_3_Sectores_Categoria.xlsx'); Source = 'BCRD ENCFT' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/2_4_Sectores_Edad.xlsx'; Path = (Join-Path $encftDir '2_4_Sectores_Edad.xlsx'); Source = 'BCRD ENCFT' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/2_5_Sectores_Educacion.xlsx'; Path = (Join-Path $encftDir '2_5_Sectores_Educacion.xlsx'); Source = 'BCRD ENCFT' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/3_1_Deciles_Rama.xlsx'; Path = (Join-Path $encftDir '3_1_Deciles_Rama.xlsx'); Source = 'BCRD ENCFT' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/3_2_Deciles_Ocupacion.xlsx'; Path = (Join-Path $encftDir '3_2_Deciles_Ocupacion.xlsx'); Source = 'BCRD ENCFT' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/3_3_Deciles_Categoria.xlsx'; Path = (Join-Path $encftDir '3_3_Deciles_Categoria.xlsx'); Source = 'BCRD ENCFT' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/3_4_Deciles_Edad.xlsx'; Path = (Join-Path $encftDir '3_4_Deciles_Edad.xlsx'); Source = 'BCRD ENCFT' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/3_5_Deciles_Educacion.xlsx'; Path = (Join-Path $encftDir '3_5_Deciles_Educacion.xlsx'); Source = 'BCRD ENCFT' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_articulos_base_2019-2020.xlsx'; Path = (Join-Path $ipcDir 'ipc_articulos_base_2019-2020.xlsx'); Source = 'BCRD IPC' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_tnt_base_2019-2020.xls'; Path = (Join-Path $ipcDir 'ipc_tnt_base_2019-2020.xls'); Source = 'BCRD IPC' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_base_2019-2020_serie_referencial.xlsx'; Path = (Join-Path $ipcDir 'ipc_base_2019-2020_serie_referencial.xlsx'); Source = 'BCRD IPC' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/Remesas_PR.xlsx'; Path = (Join-Path $remesasDir 'Remesas_PR.xlsx'); Source = 'BCRD remesas' },
  @{ Url = 'https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/Remesas_PE.xlsx'; Path = (Join-Path $remesasDir 'Remesas_PE.xlsx'); Source = 'BCRD remesas' },
  @{ Url = 'https://mem.gob.do/wp-content/uploads/2025/01/Boletin-Informativo-Generacion-y-Gestion-Energia-2024-V.2.pdf'; Path = (Join-Path $electricDir 'boletin-generacion-2024.pdf'); Source = 'MEM' },
  @{ Url = 'https://mem.gob.do/wp-content/uploads/2025/03/Boletin-Informativo-Distribucion-y-Comercializacion-de-Energia-de-las-EDE-diciembre-2024.pdf'; Path = (Join-Path $electricDir 'boletin-distribucion-diciembre-2024.pdf'); Source = 'MEM' },
  @{ Url = 'https://www.tss.gob.do/assets/guiausuario24b.pdf'; Path = (Join-Path $laborLawDir 'guia-usuario-tss-2024.pdf'); Source = 'TSS' },
  @{ Url = 'https://www.tss.gob.do/assets/reso01-2025.pdf'; Path = (Join-Path $laborLawDir 'resolucion-tss-01-2025.pdf'); Source = 'TSS' },
  @{ Url = 'https://www.infotep.gob.do/transparencia/index.php/plan-estrategico/category/410-memorias-institucionales?download=13812%3Amemoria-institucional-2023'; Path = (Join-Path $laborLawDir 'memoria-infotep-2023.pdf'); Source = 'INFOTEP' }
)

$manifest = foreach ($item in $downloads) {
  if (-not (Test-Path -LiteralPath $item.Path)) {
    Invoke-WebRequest -Uri $item.Url -OutFile $item.Path -UseBasicParsing
  }
  $file = Get-Item -LiteralPath $item.Path
  $hash = (Get-FileHash -LiteralPath $item.Path -Algorithm SHA256).Hash
  [pscustomobject]@{
    source = $item.Source
    url = $item.Url
    file = $item.Path.Substring($root.Length + 1).Replace('\', '/')
    bytes = $file.Length
    sha256 = $hash
    downloaded_at = $file.LastWriteTime.ToString('o')
  }
}

$manifest | ConvertTo-Json -Depth 4 | Set-Content -LiteralPath (Join-Path $root 'research\economic-post-data-manifest.json') -Encoding UTF8
Write-Output ("Downloaded or verified {0} files." -f $manifest.Count)
