$ErrorActionPreference='Stop'
$manifest='research/mercado-laboral-dominicano/postgresql-recovery/expansion/qa/download-manifest.json'
$items=Get-Content -Raw -Encoding UTF8 $manifest | ConvertFrom-Json
foreach($item in $items | Where-Object {$_.group -eq 'minerd' -and $_.status -eq 'failed'}){
  $item.url=[System.Net.WebUtility]::HtmlDecode($item.url)
  try{
    Invoke-WebRequest -Uri $item.url -OutFile ($item.path+'.part') -UseBasicParsing -TimeoutSec 90
    Move-Item -LiteralPath ($item.path+'.part') -Destination $item.path
    $item.bytes=(Get-Item -LiteralPath $item.path).Length
    $item.sha256=(Get-FileHash -LiteralPath $item.path -Algorithm SHA256).Hash.ToLower()
    $item.downloaded_at=(Get-Date).ToString('o')
    $item.status='downloaded';$item.error=''
  }catch{$item.error=$_.Exception.Message}
  $items | ConvertTo-Json -Depth 4 | Set-Content -Encoding UTF8 $manifest
  Write-Output ('{0}: {1}' -f $item.status,[IO.Path]::GetFileName($item.path))
}
