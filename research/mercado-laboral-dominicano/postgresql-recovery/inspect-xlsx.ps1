param(
  [Parameter(Mandatory = $true)]
  [string]$Path,
  [int]$MaxRows = 3
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

function Get-CellValue {
  param($Cell, [string[]]$SharedStrings)
  $type = $Cell.GetAttribute('t')
  $value = ''
  $subtree = $Cell.ReadSubtree()
  try {
    while ($subtree.Read()) {
      if ($subtree.NodeType -eq [Xml.XmlNodeType]::Element -and $subtree.LocalName -eq 'v') {
        $value = $subtree.ReadElementContentAsString()
        break
      }
      if ($subtree.NodeType -eq [Xml.XmlNodeType]::Element -and $subtree.LocalName -eq 't') {
        $value += $subtree.ReadElementContentAsString()
      }
    }
  } finally { $subtree.Dispose() }
  if ($type -eq 's' -and $value -ne '') { return $SharedStrings[[int]$value] }
  return $value
}

if (-not (Test-Path -LiteralPath $Path)) { throw "No existe: $Path" }
$archive = [IO.Compression.ZipFile]::OpenRead((Resolve-Path $Path).Path)
try {
  $workbook = [xml](Read-ZipText $archive 'xl/workbook.xml')
  $relationships = [xml](Read-ZipText $archive 'xl/_rels/workbook.xml.rels')
  $workbookNs = New-Object Xml.XmlNamespaceManager($workbook.NameTable)
  $workbookNs.AddNamespace('x', 'http://schemas.openxmlformats.org/spreadsheetml/2006/main')
  $workbookNs.AddNamespace('r', 'http://schemas.openxmlformats.org/officeDocument/2006/relationships')
  $relationshipNs = New-Object Xml.XmlNamespaceManager($relationships.NameTable)
  $relationshipNs.AddNamespace('p', 'http://schemas.openxmlformats.org/package/2006/relationships')
  $sharedStrings = Read-SharedStrings $archive

  Write-Output "FILE=$((Resolve-Path $Path).Path)"
  foreach ($sheet in $workbook.SelectNodes('//x:sheets/x:sheet', $workbookNs)) {
    $relationshipId = $sheet.GetAttribute('id', 'http://schemas.openxmlformats.org/officeDocument/2006/relationships')
    $target = ($relationships.SelectSingleNode("//p:Relationship[@Id='$relationshipId']", $relationshipNs)).Target
    $sheetPath = if ($target.StartsWith('/')) { $target.TrimStart('/') } else { 'xl/' + $target.TrimStart('/') }
    $sheetEntry = $archive.GetEntry($sheetPath)
    $sheetReader = [Xml.XmlReader]::Create($sheetEntry.Open(), [Xml.XmlReaderSettings]@{ IgnoreWhitespace = $true })
    try {
      $rowNumber = 0
      $maxColumn = 0
      Write-Output "SHEET=$($sheet.Name);PATH=$sheetPath"
      while ($sheetReader.Read()) {
        if ($sheetReader.NodeType -ne [Xml.XmlNodeType]::Element -or $sheetReader.LocalName -ne 'row') { continue }
        $rowNumber++
        if ($rowNumber -gt $MaxRows) { break }
        $subtree = $sheetReader.ReadSubtree()
        $cells = @{}
        try {
          while ($subtree.Read()) {
            if ($subtree.NodeType -eq [Xml.XmlNodeType]::Element -and $subtree.LocalName -eq 'c') {
              $reference = $subtree.GetAttribute('r')
              $column = Get-ColumnIndex $reference
              $cells[$column] = Get-CellValue $subtree $sharedStrings
              if ($column -gt $maxColumn) { $maxColumn = $column }
            }
          }
        } finally { $subtree.Dispose() }
        $values = for ($column = 1; $column -le $maxColumn; $column++) {
          if ($cells.ContainsKey($column)) { $cells[$column] } else { '' }
        }
        Write-Output ("ROW=" + ($values -join '|'))
      }
    } finally { $sheetReader.Dispose() }
  }
} finally { $archive.Dispose() }
