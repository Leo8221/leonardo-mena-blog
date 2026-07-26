param(
    [int]$YearFrom = 2009,
    [int]$YearTo = 2026
)

$ErrorActionPreference = 'Stop'

$repoRoot = Split-Path -Parent $PSScriptRoot
$outDir = Join-Path $repoRoot 'research\sector-electrico-dominicano\data\raw\mem'
New-Item -ItemType Directory -Force -Path $outDir | Out-Null

$years = $YearFrom..$YearTo
$catalog = [System.Collections.Generic.List[object]]::new()
$headers = @{ 'User-Agent' = 'Mozilla/5.0 (compatible; research reproducibility)' }
$manifestPath = Join-Path $outDir 'mem-electricity-manifest.csv'

if (Test-Path -LiteralPath $manifestPath) {
    foreach ($oldRow in (Import-Csv -LiteralPath $manifestPath)) {
        [void]$catalog.Add($oldRow)
    }
}

function Save-Manifest {
    param([System.Collections.Generic.List[object]]$Rows)
    $deduped = @($Rows | Group-Object { "$($_.category)|$($_.year)|$($_.source_url)|$($_.local_file)" } | ForEach-Object { $_.Group[-1] })
    $deduped | Sort-Object category, year, local_file | Export-Csv -LiteralPath $manifestPath -NoTypeInformation -Encoding UTF8
}

foreach ($year in $years) {
    $pages = @()
    if ($year -ge 2022) {
        $pages += @{ category = 'generacion'; url = "https://mem.gob.do/category/sector-electrico/boletin-de-generacion-y-gestion-de-energia/$year-boletin-de-generacion/" }
        $pages += @{ category = 'distribucion'; url = "https://mem.gob.do/category/sector-electrico/boletin-de-distribucion-y-comercializacion-de-energia/$year-boletin-de-distribucion/" }
    }
    if ($year -ge 2009) {
        $performanceSlug = if ($year -le 2022) { "$year/" } else { "$year-informe-de-desempeno/" }
        $pages += @{ category = 'desempeno'; url = "https://mem.gob.do/category/sector-electrico/informe-de-desempeno/$performanceSlug" }
    }
    if ($year -ge 2015) {
        $pages += @{ category = 'gestion-comercial'; url = "https://mem.gob.do/category/sector-electrico/informe-de-gestion-comercial/$year/" }
    }

    foreach ($page in $pages) {
        try {
            $response = Invoke-WebRequest -Uri $page.url -Headers $headers -UseBasicParsing
            $html = [System.Net.WebUtility]::HtmlDecode($response.Content)
            $matches = [regex]::Matches($html, 'https?://mem\.gob\.do/wp-content/uploads/[^\"''<>\s]+?\.(?:pdf|xlsx)', [System.Text.RegularExpressions.RegexOptions]::IgnoreCase)
            $urls = $matches | ForEach-Object { $_.Value.Replace('&amp;', '&') } | Sort-Object -Unique

            foreach ($url in $urls) {
                $leaf = [IO.Path]::GetFileName(([Uri]$url).AbsolutePath)
                if ([string]::IsNullOrWhiteSpace($leaf)) { continue }
                $safeLeaf = $leaf -replace '[^A-Za-z0-9._-]', '_'
                $localName = "mem_$($page.category)_$year`_$safeLeaf"
                $localPath = Join-Path $outDir $localName
                $status = 'existing'
                if ((-not (Test-Path -LiteralPath $localPath)) -or ((Get-Item -LiteralPath $localPath).Length -lt 10000)) {
                    try {
                        Invoke-WebRequest -Uri $url -Headers $headers -OutFile $localPath -UseBasicParsing
                        $status = 'downloaded'
                    } catch {
                        $status = "download_failed: $($_.Exception.Message)"
                    }
                }
                $hash = ''
                if (Test-Path -LiteralPath $localPath) {
                    for ($attempt = 1; $attempt -le 6; $attempt++) {
                        try {
                            $hash = (Get-FileHash -LiteralPath $localPath -Algorithm SHA256).Hash
                            break
                        } catch {
                            if ($attempt -eq 6) { $status = "hash_failed: $($_.Exception.Message)" }
                            Start-Sleep -Milliseconds 500
                        }
                    }
                }
                [void]$catalog.Add([pscustomobject]@{
                    year = $year
                    category = $page.category
                    catalog_url = $page.url
                    source_url = $url
                    local_file = $localName
                    status = $status
                    sha256 = $hash
                })
            }
        } catch {
            [void]$catalog.Add([pscustomobject]@{
                year = $year
                category = $page.category
                catalog_url = $page.url
                source_url = ''
                local_file = ''
                status = "catalog_failed: $($_.Exception.Message)"
                sha256 = ''
            })
        }
        Save-Manifest $catalog
        Write-Output "Procesado $($page.category) ${year}: $($catalog.Count) registros"
    }
}

Save-Manifest $catalog
Write-Output "Manifest: $manifestPath"
Write-Output "Files catalogued: $(@($catalog).Count)"
Write-Output "Downloaded: $(@($catalog | Where-Object status -eq 'downloaded').Count)"
Write-Output "Existing: $(@($catalog | Where-Object status -eq 'existing').Count)"
Write-Output "Failures: $(@($catalog | Where-Object status -like '*failed*').Count)"
