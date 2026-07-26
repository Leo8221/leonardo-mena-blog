$ErrorActionPreference = 'Stop'

$root = (Get-Location).Path
$downloads = @(
    @{ Url = 'https://www.one.gob.do/media/uqnbtqtn/enae-2025-informe-general.pdf'; Rel = 'research/nearshoring-limites/data/raw/one_enae_2025_informe_general.pdf' }
    @{ Url = 'https://www.one.gob.do/media/lrle0f4n/bolet%C3%ADn-directorio-de-empresas-tercer-trimestre-2025.pdf'; Rel = 'research/nearshoring-limites/data/raw/one_directorio_empresas_2025_t3.pdf' }
    @{ Url = 'https://www.one.gob.do/media/mfkbehzf/bolet%C3%ADn-estad%C3%ADstico-agropecuario-enero-diciembre-2025.pdf'; Rel = 'research/ingenios-azucar/data/raw/one_boletin_agropecuario_2025.pdf' }
    @{ Url = 'https://miderec.gob.do/wp-admin/admin-ajax.php?juwpfisadmin=false&action=wpfd&task=file.download&wpfd_category_id=62&wpfd_file_id=31250&token='; Rel = 'research/atletas-spillover/data/raw/miderec_memoria_institucional_2024.pdf' }
    @{ Url = 'https://miderec.gob.do/wp-admin/admin-ajax.php?juwpfisadmin=false&action=wpfd&task=file.download&wpfd_category_id=62&wpfd_file_id=34080&token='; Rel = 'research/atletas-spillover/data/raw/miderec_memoria_institucional_2025.pdf' }
    @{ Url = 'https://www.one.gob.do/media/zcgnf4d1/tasa-de-instalaciones-deportivas-nacional-y-seg%C3%BAn-provincia-2018.xlsx'; Rel = 'research/atletas-spillover/data/raw/one_tasa_instalaciones_deportivas_provincia_2018.xlsx' }
    @{ Url = 'https://cdn.bancentral.gov.do/documents/otras-publicaciones/documents/Informe_de_Encuesta_Nacional_de_Inclusion.pdf'; Rel = 'research/epicteto-fondo-emergencia/data/raw/bcrd_enief_2024.pdf' }
)

foreach ($download in $downloads) {
    $output = Join-Path $root $download.Rel
    New-Item -ItemType Directory -Force -Path (Split-Path -Parent $output) | Out-Null
    curl.exe -L --fail --retry 2 --connect-timeout 20 --max-time 180 -o $output $download.Url
    $size = (Get-Item -LiteralPath $output).Length
    if ($size -lt 1024) {
        throw "Descarga incompleta: $output ($size bytes)"
    }
    Write-Output "$($download.Rel) $size bytes"
}
