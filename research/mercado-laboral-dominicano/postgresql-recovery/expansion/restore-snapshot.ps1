param([switch]$VerifyOnly)
$ErrorActionPreference='Stop'
$base=Join-Path (Get-Location) 'research/mercado-laboral-dominicano/postgresql-recovery/expansion'
$raw=Join-Path $base 'raw'
$items=[Collections.Generic.List[object]]::new()
$downloadCount=0
function Add-SnapshotFile([string]$url,[string]$relative,[string]$hash){
 if($relative -match '(^[\\/]|(^|[\\/])\.\.([\\/]|$)|:)'){throw 'Invalid relative snapshot path'}
 $resolvedTarget=[IO.Path]::GetFullPath((Join-Path $raw $relative))
 $resolvedRoot=[IO.Path]::GetFullPath($raw).TrimEnd('\')+'\'
 if(-not $resolvedTarget.StartsWith($resolvedRoot,[StringComparison]::OrdinalIgnoreCase)){throw 'Snapshot path escapes raw directory'}
 $items.Add([pscustomobject]@{url=$url;relative=$relative;sha256=$hash})
}
$national=Get-Content -Raw -Encoding UTF8 (Join-Path $base 'qa/download-manifest.json') | ConvertFrom-Json
foreach($entry in $national){Add-SnapshotFile $entry.url ($entry.group+'/'+[IO.Path]::GetFileName($entry.path)) $entry.sha256}
$dgcp=Get-Content -Raw -Encoding UTF8 (Join-Path $base 'qa/dgcp-download.json') | ConvertFrom-Json
foreach($entry in $dgcp){Add-SnapshotFile $entry.url ('dgcp/'+$entry.year+'.csv.tar.gz') $entry.sha256}
$wdi=Get-Content -Raw -Encoding UTF8 (Join-Path $base 'qa/wdi-download.json') | ConvertFrom-Json
foreach($entry in $wdi){Add-SnapshotFile $entry.url ('wdi/batch_{0:D4}.json' -f [int]$entry.batch) $entry.sha256}
$catalogs=Get-Content -Raw -Encoding UTF8 (Join-Path $base 'qa/wdi-catalogs.json') | ConvertFrom-Json
foreach($entry in $catalogs){Add-SnapshotFile $entry.url ('wdi/'+$entry.filename) $entry.sha256}
$fx=Get-Content -Raw -Encoding UTF8 (Join-Path $base 'qa/fx-download.json') | ConvertFrom-Json
Add-SnapshotFile $fx.url 'bcrd-mercado-cambiario/TASA_DOLAR_REFERENCIA_MC.xlsx' $fx.sha256
foreach($entry in $items){
 $target=Join-Path $raw $entry.relative
 if(-not(Test-Path -LiteralPath $target)){
  if($VerifyOnly){throw ('Missing snapshot file: '+$entry.relative)}
  New-Item -ItemType Directory -Force -Path (Split-Path $target) | Out-Null
  Invoke-WebRequest -Uri $entry.url -UseBasicParsing -OutFile ($target+'.part') -TimeoutSec 180
  $downloadCount++
  if((Get-FileHash -LiteralPath ($target+'.part')).Hash.ToLower() -ne $entry.sha256){throw ('Producer changed the source; review the new version: '+$entry.relative)}
  Move-Item -LiteralPath ($target+'.part') -Destination $target
 }
 if((Get-FileHash -LiteralPath $target).Hash.ToLower() -ne $entry.sha256){throw ('Snapshot SHA256 mismatch: '+$entry.relative)}
}
# OCDS archives contain a year directory and thirteen CSV members. Validate paths before extraction.
foreach($entry in $dgcp){
 $year=[string]$entry.year
 if($year -notmatch '^20[0-9]{2}$'){throw 'Invalid archive year'}
 $archive=Join-Path $raw ('dgcp/'+$year+'.csv.tar.gz')
 $csvFolder=Join-Path $raw ('dgcp/'+$year)
 $members=@(& tar -tzf $archive)
 if($LASTEXITCODE -ne 0){throw 'Cannot inspect DGCP archive'}
 foreach($member in $members){if($member -notmatch ('^'+$year+'/([a-z_]+\.csv)?$')){throw ('Unexpected archive member: '+$member)}}
 if(-not(Test-Path -LiteralPath $csvFolder)){
  if($VerifyOnly){throw ('Missing extracted year: '+$year)}
  & tar -xzf $archive -C (Join-Path $raw 'dgcp')
  if($LASTEXITCODE -ne 0){throw 'Cannot extract DGCP archive'}
 }
 foreach($member in ($members | Where-Object {$_ -match '\.csv$'})){
  if(-not(Test-Path -LiteralPath (Join-Path (Join-Path $raw 'dgcp') $member))){throw ('Incomplete extracted snapshot: '+$member)}
 }
}
# Exact CSV hashes were recorded during the original verified PostgreSQL load.
$csvFiles=Import-Csv -Encoding UTF8 -LiteralPath (Join-Path $base 'qa/dgcp-csv-manifest.csv')
foreach($entry in $csvFiles){
 $relative='dgcp/'+$entry.anio+'/'+$entry.tabla+'.csv'
 if((Get-FileHash -LiteralPath (Join-Path $raw $relative)).Hash.ToLower() -ne $entry.sha256){throw ('Extracted CSV mismatch: '+$relative)}
}
[pscustomobject]@{checked_at=(Get-Date).ToString('o');downloaded_files=$items.Count;extracted_csv_files=$csvFiles.Count;status='ok';network_used=($downloadCount -gt 0)} |
 ConvertTo-Json | Set-Content -Encoding UTF8 -LiteralPath (Join-Path $base 'qa/source-snapshot-check.json')
Write-Output ('Verified '+$items.Count+' downloaded files and '+$csvFiles.Count+' extracted CSV files against SHA256 manifests.')
