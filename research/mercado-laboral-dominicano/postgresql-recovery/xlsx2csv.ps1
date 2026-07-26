param(
  [Parameter(Mandatory = $true)][string]$WorkbookPath,
  [Parameter(Mandatory = $true)][string]$SheetName,
  [Parameter(Mandatory = $true)][string]$OutputPath
)

$ErrorActionPreference = 'Stop'

$source = @'
using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Text;
using System.Xml;

public static class OneXlsxCsv
{
    private static string ZipText(ZipArchive archive, string name)
    {
        ZipArchiveEntry entry = archive.GetEntry(name);
        if (entry == null) return null;
        using (Stream stream = entry.Open())
        using (StreamReader reader = new StreamReader(stream)) return reader.ReadToEnd();
    }

    private static int ColumnIndex(string reference)
    {
        int index = 0;
        for (int i = 0; i < reference.Length && reference[i] >= 'A' && reference[i] <= 'Z'; i++)
            index = index * 26 + reference[i] - 'A' + 1;
        return index;
    }

    private static List<string> SharedStrings(ZipArchive archive)
    {
        var output = new List<string>();
        ZipArchiveEntry entry = archive.GetEntry("xl/sharedStrings.xml");
        if (entry == null) return output;
        using (XmlReader reader = XmlReader.Create(entry.Open(), new XmlReaderSettings { IgnoreWhitespace = true }))
        {
            while (reader.Read())
            {
                if (reader.NodeType != XmlNodeType.Element || reader.LocalName != "si") continue;
                using (XmlReader subtree = reader.ReadSubtree())
                {
                    var parts = new StringBuilder();
                    while (subtree.Read())
                        if (subtree.NodeType == XmlNodeType.Element && subtree.LocalName == "t")
                            parts.Append(subtree.ReadElementContentAsString());
                    output.Add(parts.ToString());
                }
            }
        }
        return output;
    }

    private static string XmlValue(XmlNode cell, List<string> shared)
    {
        XmlAttribute type = cell.Attributes["t"];
        XmlNode valueNode = cell.SelectSingleNode("*[local-name()='v']");
        string value = valueNode == null ? "" : valueNode.InnerText;
        if (valueNode == null && type != null && type.Value == "inlineStr")
        {
            var parts = new StringBuilder();
            foreach (XmlNode text in cell.SelectNodes(".//*[local-name()='t']")) parts.Append(text.InnerText);
            value = parts.ToString();
        }
        if (type != null && type.Value == "s" && value.Length > 0) value = shared[Int32.Parse(value)];
        return value;
    }

    private static string Csv(string value)
    {
        if (value == null) value = "";
        bool quoted = value.IndexOfAny(new[] { ',', '"', '\r', '\n' }) >= 0;
        if (value.IndexOf('"') >= 0) value = value.Replace("\"", "\"\"");
        return quoted ? "\"" + value + "\"" : value;
    }

    private static string SheetPath(ZipArchive archive, string sheetName)
    {
        XmlDocument workbook = new XmlDocument();
        workbook.LoadXml(ZipText(archive, "xl/workbook.xml"));
        XmlDocument relationships = new XmlDocument();
        relationships.LoadXml(ZipText(archive, "xl/_rels/workbook.xml.rels"));
        XmlNode sheet = workbook.SelectSingleNode("//*[local-name()='sheet' and @name=" + XPathLiteral(sheetName) + "]");
        if (sheet == null) throw new Exception("No existe la hoja: " + sheetName);
        string rid = sheet.Attributes["r:id"].Value;
        XmlNode relation = relationships.SelectSingleNode("//*[local-name()='Relationship' and @Id=" + XPathLiteral(rid) + "]");
        string target = relation.Attributes["Target"].Value.Replace('\\', '/').TrimStart('/');
        return target.StartsWith("xl/") ? target : "xl/" + target;
    }

    private static string XPathLiteral(string value)
    {
        if (value.IndexOf('\'') < 0) return "'" + value + "'";
        return "\"" + value + "\"";
    }

    public static long Convert(string workbookPath, string sheetName, string outputPath)
    {
        using (ZipArchive archive = ZipFile.OpenRead(workbookPath))
        using (XmlReader reader = XmlReader.Create(archive.GetEntry(SheetPath(archive, sheetName)).Open(), new XmlReaderSettings { IgnoreWhitespace = true }))
        using (StreamWriter writer = new StreamWriter(outputPath, false, new UTF8Encoding(false)))
        {
            List<string> shared = SharedStrings(archive);
            long dataRows = 0;
            int rowNumber = 0;
            int headerMax = 0;
            while (reader.Read())
            {
                if (reader.NodeType != XmlNodeType.Element || reader.LocalName != "row") continue;
                rowNumber++;
                XmlDocument row = new XmlDocument();
                row.LoadXml(reader.ReadOuterXml());
                var cells = new Dictionary<int, string>();
                foreach (XmlNode cell in row.SelectNodes("//*[local-name()='c']"))
                    cells[ColumnIndex(cell.Attributes["r"].Value)] = XmlValue(cell, shared);
                if (rowNumber == 1)
                {
                    foreach (int column in cells.Keys) if (column > headerMax) headerMax = column;
                }
                if (headerMax == 0) continue;
                var values = new string[headerMax];
                bool nonempty = false;
                for (int i = 1; i <= headerMax; i++)
                {
                    values[i - 1] = cells.ContainsKey(i) ? cells[i] : "";
                    if (values[i - 1].Length > 0) nonempty = true;
                }
                if (rowNumber == 1 || nonempty)
                {
                    var escaped = new string[headerMax];
                    for (int i = 0; i < headerMax; i++) escaped[i] = Csv(values[i]);
                    writer.WriteLine(String.Join(",", escaped));
                    if (rowNumber > 1) dataRows++;
                }
            }
            return dataRows;
        }
    }
}
'@

Add-Type -TypeDefinition $source -Language CSharp -ReferencedAssemblies @(
  'System.dll',
  'System.Xml.dll',
  'System.IO.Compression.dll',
  'System.IO.Compression.FileSystem.dll'
)

if (-not (Test-Path -LiteralPath $WorkbookPath)) { throw "No existe: $WorkbookPath" }
$result = [OneXlsxCsv]::Convert((Resolve-Path $WorkbookPath).Path, $SheetName, $OutputPath)
Write-Output "rows=$result"
