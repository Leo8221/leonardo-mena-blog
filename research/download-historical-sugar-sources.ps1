$ErrorActionPreference = 'Stop'

$root = Split-Path -Parent $PSScriptRoot
$raw = Join-Path $root 'research\ingenios-azucar\data\raw'
New-Item -ItemType Directory -Force -Path $raw | Out-Null

$sources = @(
    @{ Name = 'bcrd_boletin_mayo_1955.pdf'; Url = 'https://repositoriocultural.bancentral.gov.do/bitstreams/a8e19cba-3c9c-4bb2-b8d2-4c0acdf5337f/download' },
    @{ Name = 'bcrd_memoria_anual_1963.pdf'; Url = 'https://repositoriocultural.bancentral.gov.do/bitstreams/2bf75a3c-3fbc-4e8e-af81-1a4d7048624d/download' },
    @{ Name = 'bcrd_azucar_politica_inversiones_1972.pdf'; Url = 'https://repositoriocultural.bancentral.gov.do/bitstreams/97c7bcb9-2d1c-4032-ba44-434d2c22b84a/download' },
    @{ Name = 'usda_agriculture_trade_dominican_republic_1972.pdf'; Url = 'https://esmis.nal.usda.gov/sites/default/release-files/jq085j963/gf06g588s/gm80hz857/ERSF-02-11-1972_Agriculture_and_Trade_of_the_Dominican_Republic.pdf' },
    @{ Name = 'world_bank_dominican_sugar_supply_distribution_1984.pdf'; Url = 'https://documents1.worldbank.org/curated/en/393581468031538129/pdf/multi-page.pdf' },
    @{ Name = 'hall_sugar_power_preview_2000.pdf'; Url = 'https://api.pageplace.de/preview/DT0400.9780313030574_A23627259/preview-9780313030574_A23627259.pdf' }
)

foreach ($source in $sources) {
    $target = Join-Path $raw $source.Name
    $target = [System.IO.Path]::GetFullPath($target)
    & curl.exe -L --fail --retry 2 --output $target $source.Url
    if ($LASTEXITCODE -ne 0) { throw "No se pudo descargar $($source.Url)" }
    $size = [math]::Round((Get-Item -LiteralPath $target).Length / 1MB, 2)
    Write-Output ("{0}: {1} MB" -f $source.Name, $size)
}
