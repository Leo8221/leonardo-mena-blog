"""Compare selected numeric XML cells to processed values without loading Excel."""
from pathlib import Path
import json, os, re, zipfile, xml.etree.ElementTree as ET

root = Path(os.environ.get("ENCFT_SOURCE_ROOT", r"C:\Users\leona\Desktop\encuesta_trabajo_rd"))
qa = Path("research/mercado-laboral-dominicano/postgresql-recovery/encft/qa")
requests = json.loads((qa / "source-spotcheck-cells.json").read_text(encoding="utf-8"))
ns = {"s": "http://schemas.openxmlformats.org/spreadsheetml/2006/main"}
found = {}
with zipfile.ZipFile(next((root / "2023").glob("*.xlsx"))) as z:
    wb = ET.fromstring(z.read("xl/workbook.xml"))
    rels = {r.attrib["Id"]: r.attrib["Target"] for r in ET.fromstring(z.read("xl/_rels/workbook.xml.rels"))}
    for sheet in wb.findall("s:sheets/s:sheet", ns):
        name = sheet.attrib["name"]
        wanted = {r["cell"] for r in requests if r["sheet"] == name}
        if not wanted:
            continue
        target = rels[sheet.attrib["{http://schemas.openxmlformats.org/officeDocument/2006/relationships}id"]]
        member = target.lstrip("/") if target.startswith("/") else "xl/" + target
        pattern = re.compile(rb'<c\b[^>]*\br="(' + b'|'.join(c.encode() for c in wanted) + rb')"[^>]*>.*?<v>([^<]+)</v>.*?</c>', re.S)
        with z.open(member) as f:
            tail = b""
            while wanted:
                chunk = f.read(4 * 1024 * 1024)
                if not chunk:
                    break
                buf = tail + chunk
                for match in pattern.finditer(buf):
                    cell = match.group(1).decode()
                    found[(name, cell)] = float(match.group(2))
                    wanted.discard(cell)
                tail = buf[-8192:]
        if wanted:
            raise ValueError(f"Missing numeric cells in {name}: {wanted}")
for req in requests:
    actual = found[(req["sheet"], req["cell"])]
    assert abs(actual - req["expected"]) <= 1e-10 * max(1, abs(req["expected"]))
summary = {"year": 2023, "cells_checked": len(requests), "sheets": ["Miembros", "Hogar"], "status": "all_match", "conclusion": "The selected keys and unequal expansion factors are present in the original Excel, including the largest observed difference."}
(qa / "source-excel-spotcheck.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
print(json.dumps(summary))
