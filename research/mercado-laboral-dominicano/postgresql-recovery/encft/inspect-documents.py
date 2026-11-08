"""Read-only source workbook metadata and methodological text extraction."""
from pathlib import Path
import json
import os
import re
import zipfile
import xml.etree.ElementTree as ET

ROOT = Path(os.environ.get("ENCFT_SOURCE_ROOT", r"C:\Users\leona\Desktop\encuesta_trabajo_rd"))
OUT = Path("research/mercado-laboral-dominicano/postgresql-recovery/encft/qa")
OUT.mkdir(parents=True, exist_ok=True)
ns = {"s": "http://schemas.openxmlformats.org/spreadsheetml/2006/main"}
inventory = []
for path in sorted(ROOT.glob("20*/*.xlsx")):
    with zipfile.ZipFile(path) as z:
        workbook = ET.fromstring(z.read("xl/workbook.xml"))
        rels = ET.fromstring(z.read("xl/_rels/workbook.xml.rels"))
        mapping = {r.attrib["Id"]: r.attrib["Target"] for r in rels}
        for sheet in workbook.findall("s:sheets/s:sheet", ns):
            rid = sheet.attrib["{http://schemas.openxmlformats.org/officeDocument/2006/relationships}id"]
            target = mapping[rid]
            member = target.lstrip("/") if target.startswith("/") else "xl/" + target
            with z.open(member) as f:
                header = f.read(65536).decode("utf-8")
            dimension = re.search(r'<(?:\w+:)?dimension\b[^>]*\bref="([^"]+)"', header)
            inventory.append({"file": str(path), "sheet": sheet.attrib["name"],
                              "dimension": dimension.group(1) if dimension else None})
(OUT / "xlsx-inventory.json").write_text(json.dumps(inventory, ensure_ascii=False, indent=2), encoding="utf-8")
print(json.dumps(inventory, ensure_ascii=True))
try:
    from pypdf import PdfReader
    for name in ["Diseno_muestral.pdf", "Variables de remesas ENCFT.pdf"]:
        pdf = PdfReader(ROOT / "2026" / name)
        content = "\n\n".join(f"PAGE {i+1}\n" + p.extract_text() for i, p in enumerate(pdf.pages))
        (OUT / (Path(name).stem + ".txt")).write_text(content, encoding="utf-8")
        print(f"PDF {name}: {len(pdf.pages)} pages, extracted {len(content)} characters")
except ImportError:
    print("PYPDF_NOT_AVAILABLE")
