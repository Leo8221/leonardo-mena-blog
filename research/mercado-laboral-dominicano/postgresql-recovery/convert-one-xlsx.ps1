param(
  [string]$InputDir = (Join-Path $PSScriptRoot '..\data\raw\one_xlsx'),
  [string]$OutputDir = (Join-Path $PSScriptRoot '..\data\raw\one_csv'),
  [string[]]$SelectedDataset
)

$ErrorActionPreference = 'Stop'
Add-Type -AssemblyName System.IO.Compression.FileSystem

function Read-ZipText {
  param($Archive, [string]$Name)
  $entry = $Archive.GetEntry($Name)
  if (-not $entry) { return $null }
  $reader = [IO.StreamReader]::new($entry.Open())
  try { return $reader.ReadToEnd() } finally { $reader.Dispose() }
}

function Get-ColumnIndex {
  param([string]$Reference)
  $letters = ([regex]::Match($Reference, '^[A-Z]+')).Value
  $index = 0
  foreach ($letter in $letters.ToCharArray()) {
    $index = $index * 26 + ([int][char]$letter - [int][char]'A' + 1)
  }
  return $index
}

function Read-SharedStrings {
  param($Archive)
  $entry = $Archive.GetEntry('xl/sharedStrings.xml')
  if (-not $entry) { return @() }
  $strings = [Collections.Generic.List[string]]::new()
  $reader = [Xml.XmlReader]::Create($entry.Open(), [Xml.XmlReaderSettings]@{ IgnoreWhitespace = $true })
  try {
    while ($reader.Read()) {
      if ($reader.NodeType -eq [Xml.XmlNodeType]::Element -and $reader.LocalName -eq 'si') {
        $subtree = $reader.ReadSubtree()
        $parts = [Collections.Generic.List[string]]::new()
        try {
          while ($subtree.Read()) {
            if ($subtree.NodeType -eq [Xml.XmlNodeType]::Element -and $subtree.LocalName -eq 't') {
              $parts.Add($subtree.ReadElementContentAsString())
            }
          }
        } finally { $subtree.Dispose() }
        $strings.Add(($parts -join ''))
      }
    }
  } finally { $reader.Dispose() }
  return $strings.ToArray()
}

function Escape-Csv {
  param([AllowNull()][string]$Value)
  if ($null -eq $Value) { $Value = '' }
  $needsQuote = $Value.Contains(',') -or $Value.Contains("`r") -or $Value.Contains("`n") -or $Value.Contains('"')
  if ($Value.Contains('"')) { $Value = $Value.Replace('"', '""') }
  if ($needsQuote) {
    return '"' + $Value + '"'
  }
  return $Value
}

function Get-SheetEntry {
  param($Archive, [xml]$Workbook, [xml]$Relationships, [string]$SheetName)
  $workbookNs = New-Object Xml.XmlNamespaceManager($Workbook.NameTable)
  $workbookNs.AddNamespace('x', 'http://schemas.openxmlformats.org/spreadsheetml/2006/main')
  $relationshipNs = New-Object Xml.XmlNamespaceManager($Relationships.NameTable)
  $relationshipNs.AddNamespace('p', 'http://schemas.openxmlformats.org/package/2006/relationships')
  $sheet = $Workbook.SelectSingleNode("//x:sheets/x:sheet[@name='$SheetName']", $workbookNs)
  if (-not $sheet) { throw "No existe la hoja '$SheetName'." }
  $relationshipId = $sheet.GetAttribute('id', 'http://schemas.openxmlformats.org/officeDocument/2006/relationships')
  $target = ($Relationships.SelectSingleNode("//p:Relationship[@Id='$relationshipId']", $relationshipNs)).Target
  $sheetPath = if ($target.StartsWith('/')) { $target.TrimStart('/') } else { 'xl/' + $target.TrimStart('/') }
  return $Archive.GetEntry($sheetPath)
}

function Convert-Sheet {
  param([string]$WorkbookPath, [string]$SheetName, [string]$OutputPath)
  $archive = [IO.Compression.ZipFile]::OpenRead((Resolve-Path $WorkbookPath).Path)
  try {
    $workbook = [xml](Read-ZipText $archive 'xl/workbook.xml')
    $relationships = [xml](Read-ZipText $archive 'xl/_rels/workbook.xml.rels')
    $sharedStrings = Read-SharedStrings $archive
    $sheetEntry = Get-SheetEntry $archive $workbook $relationships $SheetName
    $reader = [Xml.XmlReader]::Create($sheetEntry.Open(), [Xml.XmlReaderSettings]@{ IgnoreWhitespace = $true })
    $writer = [IO.StreamWriter]::new($OutputPath, $false, [Text.UTF8Encoding]::new($false))
    try {
      $rowNumber = 0
      $headerMax = 0
      $dataRows = 0
      $sheetNs = New-Object Xml.XmlNamespaceManager((New-Object Xml.NameTable))
      $sheetNs.AddNamespace('x', 'http://schemas.openxmlformats.org/spreadsheetml/2006/main')
      while ($reader.Read()) {
        if ($reader.NodeType -ne [Xml.XmlNodeType]::Element -or $reader.LocalName -ne 'row') { continue }
        $rowNumber++
        $rowXml = [xml]$reader.ReadOuterXml()
        $cells = @{}
        foreach ($cell in $rowXml.SelectNodes('//x:c', $sheetNs)) {
          $column = Get-ColumnIndex $cell.r
          $value = ''
          $valueNode = $cell.SelectSingleNode('./x:v', $sheetNs)
          if ($valueNode) { $value = [string]$valueNode.'#text' }
          else {
            $textNodes = $cell.SelectNodes('.//x:t', $sheetNs)
            if ($textNodes) { $value = (($textNodes | ForEach-Object { $_.'#text' }) -join '') }
          }
          if ($cell.t -eq 's' -and $value -ne '') { $value = $sharedStrings[[int]$value] }
          $cells[$column] = $value
        }
        if ($rowNumber -eq 1) {
          if (-not $cells.Count) { throw "La hoja '$SheetName' no tiene encabezado." }
          $headerMax = ($cells.Keys | Measure-Object -Maximum).Maximum
        }
        if ($headerMax -eq 0) { continue }
        $values = for ($column = 1; $column -le $headerMax; $column++) {
          if ($cells.ContainsKey($column)) { [string]$cells[$column] } else { '' }
        }
        if ($rowNumber -eq 1) {
          $writer.WriteLine((($values | ForEach-Object { Escape-Csv $_ }) -join ','))
          continue
        }
        if (-not (($values | Where-Object { $_ -ne '' }).Count)) { continue }
        $writer.WriteLine((($values | ForEach-Object { Escape-Csv $_ }) -join ','))
        $dataRows++
      }
      return [pscustomobject]@{ rows = $dataRows; columns = $headerMax }
    } finally { $writer.Dispose(); $reader.Dispose() }
  } finally { $archive.Dispose() }
}

$datasets = @(
  @{ dataset = 'atmosfera_clima_1991_2025'; file = 'atmosfera_clima_1991_2025.xlsx'; sheet = '1991-2025'; table = 'atmosfera_clima_1991_2025'; url = 'https://www.one.gob.do/media/0aybenwv/base-de-datos-atm%C3%B3sfera-y-clima-1991-2025.xlsx' },
  @{ dataset = 'atmosfera_clima_ca_2017_2023'; file = 'atmosfera_clima_1991_2025.xlsx'; sheet = 'CA_2017_2023'; table = 'atmosfera_clima_ca_2017_2023'; url = 'https://www.one.gob.do/media/0aybenwv/base-de-datos-atm%C3%B3sfera-y-clima-1991-2025.xlsx' },
  @{ dataset = 'atmosfera_clima_ca_old'; file = 'atmosfera_clima_1991_2025.xlsx'; sheet = 'CA_old'; table = 'atmosfera_clima_ca_old'; url = 'https://www.one.gob.do/media/0aybenwv/base-de-datos-atm%C3%B3sfera-y-clima-1991-2025.xlsx' },
  @{ dataset = 'eventos_fenomenos_naturales'; file = 'eventos_fenomenos_naturales.xlsx'; sheet = 'Base'; table = 'eventos_fenomenos_naturales'; url = 'https://www.one.gob.do/media/wsfa2mqb/base-de-datos-de-eventos-y-fenomenos-naturales.xlsx' },
  @{ dataset = 'gastos_gobiernos_locales_2022'; file = 'gastos_gobiernos_locales_2022.xlsx'; sheet = 'Gastos 2022'; table = 'gastos_gobiernos_locales_2022'; url = 'https://www.one.gob.do/media/3jwlzsfg/base-de-datos-de-gastos-de-los-gobienos-locales-2022.xlsx' },
  @{ dataset = 'gastos_gobiernos_locales_2023'; file = 'gastos_gobiernos_locales_2023.xlsx'; sheet = 'Gastos 2023'; table = 'gastos_gobiernos_locales_2023'; url = 'https://www.one.gob.do/media/of4a3wbc/base-de-datos-de-los-gobiernos-locales-2023.xlsx' },
  @{ dataset = 'gastos_gobiernos_locales_2024'; file = 'gastos_gobiernos_locales_2024.xlsx'; sheet = 'Gastos 2024'; table = 'gastos_gobiernos_locales_2024'; url = 'https://www.one.gob.do/media/2iiomesv/base-de-datos-de-los-gobiernos-locales-2024.xlsx' },
  @{ dataset = 'ingresos_gobiernos_locales_2022'; file = 'ingresos_gobiernos_locales_2022.xlsx'; sheet = 'Ingresos 2022'; table = 'ingresos_gobiernos_locales_2022'; url = 'https://www.one.gob.do/media/2fphze2s/base-de-datos-de-ingresos-de-los-gobiernos-locales-2022.xlsx' },
  @{ dataset = 'ingresos_gobiernos_locales_2023'; file = 'ingresos_gobiernos_locales_2023.xlsx'; sheet = 'Ingresos 2023'; table = 'ingresos_gobiernos_locales_2023'; url = 'https://www.one.gob.do/media/ghypspwn/base-de-datos-ingresos-de-los-gobiernos-locales-2023.xlsx' },
  @{ dataset = 'ingresos_gobiernos_locales_2024'; file = 'ingresos_gobiernos_locales_2024.xlsx'; sheet = 'Ingresos 2024'; table = 'ingresos_gobiernos_locales_2024'; url = 'https://www.one.gob.do/media/h0wf25ut/base-de-datos-ingresos-de-los-gobiernos-locales-2024.xlsx' }
)

if ($SelectedDataset) {
  $datasets = @($datasets | Where-Object { $_.dataset -in $SelectedDataset })
  if (-not $datasets.Count) { throw "No coincidieron datasets con -SelectedDataset: $($SelectedDataset -join ', ')" }
}

New-Item -ItemType Directory -Force -Path $OutputDir | Out-Null
$fastConverter = Join-Path $PSScriptRoot 'xlsx2csv.ps1'
$manifest = [Collections.Generic.List[object]]::new()
foreach ($dataset in $datasets) {
  $inputPath = Join-Path $InputDir $dataset.file
  if (-not (Test-Path -LiteralPath $inputPath)) { throw "Falta el libro: $inputPath" }
  $outputPath = Join-Path $OutputDir ($dataset.table + '.csv')
  Write-Host "Convirtiendo $($dataset.file) / $($dataset.sheet)..."
  $resultText = & powershell.exe -NoProfile -ExecutionPolicy Bypass -File $fastConverter -WorkbookPath $inputPath -SheetName $dataset.sheet -OutputPath $outputPath
  if ($LASTEXITCODE -ne 0) { throw "Falló la conversión de $($dataset.file) / $($dataset.sheet)." }
  $rows = [int64](($resultText | Select-String '^rows=' | Select-Object -Last 1).ToString().Substring(5))
  $header = Get-Content -LiteralPath $outputPath -TotalCount 1
  $result = [pscustomobject]@{ rows = $rows; columns = ($header -split ',').Count }
  $file = Get-Item -LiteralPath $outputPath
  $manifest.Add([pscustomobject]@{
    dataset = $dataset.dataset
    workbook = $dataset.file
    sheet = $dataset.sheet
    table = $dataset.table
    csv = $file.Name
    rows = $result.rows
    columns = $result.columns
    bytes = $file.Length
    sha256 = (Get-FileHash -Algorithm SHA256 -LiteralPath $file.FullName).Hash
    source_url = $dataset.url
    download_date = (Get-Date -Format 'yyyy-MM-dd')
  })
  Write-Host "  filas=$($result.rows), columnas=$($result.columns), bytes=$($file.Length)"
}
$manifest | Export-Csv -LiteralPath (Join-Path $OutputDir 'manifest.csv') -NoTypeInformation -Encoding utf8
