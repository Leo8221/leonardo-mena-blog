param([switch]$CatalogOnly)
$ErrorActionPreference='Stop'
$base=Join-Path (Get-Location) 'research\mercado-laboral-dominicano\postgresql-recovery\expansion'
$raw=Join-Path $base 'raw'
New-Item -ItemType Directory -Path $raw -Force | Out-Null
$items=[Collections.Generic.List[object]]::new()
$issues=[Collections.Generic.List[object]]::new()
$headers=@{'User-Agent'='Mozilla/5.0 (compatible; public statistics research)'}
function Add-Source([string]$group,[string]$url,[string]$page,[string]$title='') {
  $url=[System.Net.WebUtility]::HtmlDecode($url)
  if($url -notmatch '^https://'){return}
  if($items | Where-Object url -eq $url){return}
  $uri=[Uri]$url
  $leaf=[Uri]::UnescapeDataString([IO.Path]::GetFileName($uri.AbsolutePath))
  if($url -match 'fileNombre=([^&]+)'){$leaf=[Uri]::UnescapeDataString($Matches[1])}
  $leaf=$leaf -replace '[^A-Za-z0-9._-]','_'
  $sha=[Security.Cryptography.SHA256]::Create()
  $id=[BitConverter]::ToString($sha.ComputeHash([Text.Encoding]::UTF8.GetBytes($url))).Replace('-','').Substring(0,12).ToLower()
  $sha.Dispose()
  if($leaf.Length -gt 135){$leaf=$leaf.Substring(0,120)+[IO.Path]::GetExtension($leaf)}
  $path=Join-Path (Join-Path $raw $group) ($id+'_'+$leaf)
  $items.Add([pscustomobject]@{id=$id;group=$group;title=$title;url=$url;catalog_url=$page;path=$path;status='pending';bytes=0;sha256='';downloaded_at='';error=''})
}
function Read-Page([string]$url){
  try {return Invoke-WebRequest -Uri $url -UseBasicParsing -Headers $headers -TimeoutSec 35}
  catch {$issues.Add([pscustomobject]@{url=$url;error=$_.Exception.Message});return $null}
}
# Official source URLs already recorded in the repository, plus current catalogs.
$atlas=Get-Content -Raw -Encoding UTF8 atlas/data/source-manifest.json | ConvertFrom-Json
foreach($source in $atlas.sources){
  if($source.mode -eq 'url' -and $source.id -match '^(tss|dga)-'){Add-Source ($source.id.Split('-')[0]) $source.url $source.url $source.label}
  if($source.mode -ne 'bcrd_custom_view'){continue}
  $page=Read-Page $source.url
  $links=@()
  if($page){$links=@($page.Links | ForEach-Object href | Where-Object {$_ -match '\.xlsx?(\?|$)'})}
  if(-not $links.Count){
    $cached=Join-Path ('atlas/data/raw/'+$source.id) 'links.json'
    if(Test-Path $cached){$links=@((Get-Content -Raw -Encoding UTF8 $cached | ConvertFrom-Json).discovered)}
  }
  foreach($link in ($links | Sort-Object -Unique)){
    $abs=([Uri]::new([Uri]$source.url,[string]$link)).AbsoluteUri
    # Keep distinct historical bases separate. Exclude obsolete bases to avoid automatic splicing.
    if($abs -match '(base_2010|base_1999|base_1991|1947_2019[.]|/ipc[.]|/ipc_anual[.]|/ipc_grupos[.]|/ipc_bienes[.]|/ipc_quintiles[.]|/ipc_dominios[.])'){continue}
    Add-Source $source.id $abs $source.url $source.label
  }
}
foreach($pageUrl in @('https://digepres.gob.do/gastos/','https://digepres.gob.do/ingresos/')){
  $page=Read-Page $pageUrl
  if($page){foreach($link in $page.Links){if($link.href -match '\.xlsx?(\?|$)'){
    $abs=([Uri]::new([Uri]$pageUrl,[string]$link.href)).AbsoluteUri
    if(([Uri]$abs).Host -eq 'digepres.gob.do' -and $abs -notmatch 'Tabla-Dinamica'){Add-Source 'digepres' $abs $pageUrl}
  }}}
}
$eduSeeds=@(
 'https://www.ministeriodeeducacion.gob.do/transparencia/conjunto-de-datos-abiertos/1-centros-educativos/2024/listados',
 'https://www.ministeriodeeducacion.gob.do/transparencia/conjunto-de-datos-abiertos/2-estadisticas-de-estudiantes-matriculados/2024/listados',
 'https://www.ministeriodeeducacion.gob.do/transparencia/conjunto-de-datos-abiertos/estadisticas-de-pruebas-nacionales/2024/listados')
foreach($seed in $eduSeeds){
 $page=Read-Page $seed
 if(-not $page){continue}
 $yearPages=@($seed)+@($page.Links | ForEach-Object href | Where-Object {$_ -match '/conjunto-de-datos-abiertos/[^/]+/20[0-9]{2}/listados'} | Sort-Object -Unique)
 foreach($pageUrl in ($yearPages | Sort-Object -Unique)){
   $current=if($pageUrl -eq $seed){$page}else{Read-Page $pageUrl}
   if($current){foreach($link in $current.Links){if($link.href -match 'fileExt=xlsx'){
     Add-Source 'minerd' ([Uri]::new([Uri]$pageUrl,[string]$link.href)).AbsoluteUri $pageUrl
   }}}
 }
}
$items | ConvertTo-Json -Depth 4 | Set-Content -Encoding UTF8 (Join-Path $base 'qa/download-manifest.json')
$issues | ConvertTo-Json -Depth 4 | Set-Content -Encoding UTF8 (Join-Path $base 'qa/catalog-issues.json')
Write-Output ('Catalogued {0} public files.' -f $items.Count)
if($CatalogOnly){exit 0}
foreach($item in $items){
  New-Item -ItemType Directory -Path (Split-Path $item.path) -Force | Out-Null
  try{
    if(-not(Test-Path -LiteralPath $item.path)){
      $temporary=$item.path+'.part'
      Invoke-WebRequest -Uri $item.url -OutFile $temporary -UseBasicParsing -Headers $headers -TimeoutSec 90
      Move-Item -LiteralPath $temporary -Destination $item.path
      $item.status='downloaded'
    }else{$item.status='existing'}
    $item.bytes=(Get-Item -LiteralPath $item.path).Length
    $item.sha256=(Get-FileHash -LiteralPath $item.path -Algorithm SHA256).Hash.ToLower()
    $item.downloaded_at=(Get-Item -LiteralPath $item.path).LastWriteTime.ToString('o')
    if($item.bytes -lt 100){throw 'File too small to contain expected data'}
  }catch{$item.status='failed';$item.error=$_.Exception.Message}
  $items | ConvertTo-Json -Depth 4 | Set-Content -Encoding UTF8 (Join-Path $base 'qa/download-manifest.json')
  Write-Output ('{0} {1} {2}' -f $item.status,$item.group,[IO.Path]::GetFileName($item.path))
}
Write-Output ('Completed: {0} available, {1} failed.' -f @($items|Where-Object status -ne 'failed').Count,@($items|Where-Object status -eq 'failed').Count)
