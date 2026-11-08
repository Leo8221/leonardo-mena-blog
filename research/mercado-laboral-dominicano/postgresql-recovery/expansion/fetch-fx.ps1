$ErrorActionPreference='Stop'
$base=Join-Path (Get-Location) 'research/mercado-laboral-dominicano/postgresql-recovery/expansion'
$dest=Join-Path $base 'raw/bcrd-mercado-cambiario/TASA_DOLAR_REFERENCIA_MC.xlsx'
$url='https://cdn.bancentral.gov.do/documents/estadisticas/mercado-cambiario/documents/TASA_DOLAR_REFERENCIA_MC.xlsx'
New-Item -ItemType Directory -Force -Path (Split-Path $dest) | Out-Null
if(-not(Test-Path -LiteralPath $dest)){
 Invoke-WebRequest -Uri $url -UseBasicParsing -OutFile ($dest+'.part') -TimeoutSec 120
 Move-Item -LiteralPath ($dest+'.part') -Destination $dest
}
$manifest=Join-Path $base 'qa/fx-download.json'
if(Test-Path -LiteralPath $manifest){
 $saved=Get-Content -Raw -Encoding UTF8 -LiteralPath $manifest | ConvertFrom-Json
 if($saved.sha256 -ne (Get-FileHash -LiteralPath $dest).Hash.ToLower()){throw 'FX file differs from saved manifest; review the new source version separately.'}
}else{
 [pscustomobject]@{url=$url;path=$dest;sha256=(Get-FileHash -LiteralPath $dest).Hash.ToLower();downloaded_at=(Get-Date).ToString('o')} | ConvertTo-Json | Set-Content -Encoding UTF8 -LiteralPath $manifest
}
