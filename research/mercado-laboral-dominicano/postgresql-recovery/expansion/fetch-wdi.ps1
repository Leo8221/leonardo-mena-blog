$ErrorActionPreference='Stop'
$base=Join-Path (Get-Location) 'research/mercado-laboral-dominicano/postgresql-recovery/expansion'
$folder=Join-Path $base 'raw/wdi'
New-Item -ItemType Directory -Path $folder -Force | Out-Null
$catalog=Join-Path $folder 'indicators.json'
$url='https://api.worldbank.org/v2/indicator?source=2&format=json&per_page=20000'
if(-not(Test-Path $catalog)){Invoke-WebRequest -Uri $url -UseBasicParsing -OutFile $catalog -TimeoutSec 90}
$info=Get-Content -Raw -Encoding UTF8 $catalog | ConvertFrom-Json
if($info[0].pages -ne 1){throw 'Incomplete indicator catalog'}
$spanishCatalog=Join-Path $folder 'indicators-es.json'
if(-not(Test-Path $spanishCatalog)){Invoke-WebRequest -Uri 'https://api.worldbank.org/v2/es/indicator?source=2&format=json&per_page=20000' -UseBasicParsing -OutFile $spanishCatalog -TimeoutSec 90}
$codes=@($info[1] | ForEach-Object id)
$countries='DOM;HTI;CRI;PAN;GTM;HND;SLV;NIC;CUB;JAM;MEX;COL;PER;BRA;CHL;URY;ARG;ECU;BOL;PRY;TTO;USA;ESP;LAC;WLD'
$records=[Collections.Generic.List[object]]::new()
for($start=0;$start -lt $codes.Count;$start+=10){
 $batch=$codes[$start..([Math]::Min($start+9,$codes.Count-1))] -join ';'
 $uri='https://api.worldbank.org/v2/country/'+$countries+'/indicator/'+$batch+'?source=2&format=json&per_page=20000&date=1960:2026'
 $dest=Join-Path $folder ('batch_{0:D4}.json' -f $start)
 try{
  if(-not(Test-Path $dest)){Invoke-WebRequest -Uri $uri -UseBasicParsing -OutFile ($dest+'.part') -TimeoutSec 120;Move-Item -LiteralPath ($dest+'.part') -Destination $dest}
  $body=Get-Content -Raw -Encoding UTF8 $dest | ConvertFrom-Json
  if($body[0].pages -ne 1){throw 'Unexpected pagination or API error'}
  $records.Add([pscustomobject]@{batch=$start;url=$uri;path=$dest;sha256=(Get-FileHash -LiteralPath $dest).Hash.ToLower();rows=$body[1].Count;updated=$body[0].lastupdated;status='downloaded'})
  Write-Output ('WDI '+$start+' / '+$codes.Count)
 }catch{Write-Output ('WDI error '+$start+': '+$_.Exception.Message);throw}
 $records | ConvertTo-Json -Depth 4 | Set-Content -Encoding UTF8 (Join-Path $base 'qa/wdi-download.json')
}
