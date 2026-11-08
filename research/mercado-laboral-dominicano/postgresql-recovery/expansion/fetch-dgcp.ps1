$ErrorActionPreference='Stop'
$base=Join-Path (Get-Location) 'research/mercado-laboral-dominicano/postgresql-recovery/expansion'
$folder=Join-Path $base 'raw/dgcp'
New-Item -ItemType Directory -Force -Path $folder | Out-Null
$page='https://data.open-contracting.org/es/publication/22'
$catalog=Invoke-WebRequest -Uri $page -UseBasicParsing -TimeoutSec 60
$links=@($catalog.Links | ForEach-Object href | Where-Object {$_ -match 'name=20[0-9]{2}\.csv\.tar\.gz'} | Sort-Object -Unique)
$records=[Collections.Generic.List[object]]::new()
foreach($link in $links){
 $url=([Uri]::new([Uri]$page,[System.Net.WebUtility]::HtmlDecode($link))).AbsoluteUri
 $year=[regex]::Match($url,'name=(20[0-9]{2})').Groups[1].Value
 $dest=Join-Path $folder ($year+'.csv.tar.gz')
 if(-not(Test-Path -LiteralPath $dest)){
  Invoke-WebRequest -Uri $url -UseBasicParsing -OutFile ($dest+'.part') -TimeoutSec 600
  Move-Item -LiteralPath ($dest+'.part') -Destination $dest
 }
 $records.Add([pscustomobject]@{year=[int]$year;url=$url;catalog_url=$page;path=$dest;sha256=(Get-FileHash -LiteralPath $dest -Algorithm SHA256).Hash.ToLower();bytes=(Get-Item -LiteralPath $dest).Length;downloaded_at=(Get-Date).ToString('o')})
 $records | ConvertTo-Json | Set-Content -Encoding UTF8 (Join-Path $base 'qa/dgcp-download.json')
 Write-Output ('Downloaded DGCP '+$year)
}
