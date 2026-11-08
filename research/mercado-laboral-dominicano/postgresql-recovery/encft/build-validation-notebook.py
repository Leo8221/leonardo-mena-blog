"""Build and execute a small offline companion to the live SQL/R validation."""
from pathlib import Path
import contextlib
import io
import json
import platform

BASE = Path("research/mercado-laboral-dominicano/postgresql-recovery/encft")
def markdown(source):
    return {"cell_type": "markdown", "metadata": {}, "source": source}
def code(source):
    return {"cell_type": "code", "metadata": {}, "source": source, "execution_count": None, "outputs": []}

cells = [
    markdown("# Verificación de la incorporación ENCFT\n\n## Resultado\nBase `encft`: 41 trimestres (2016-T1 a 2026-T1), 790.993 observaciones de miembros. Este cuaderno revisa la evidencia guardada por los scripts R/SQL; **no vuelve a consultar PostgreSQL** ni contiene contraseñas. La carga es una copia física por módulo y vistas derivadas."),
    markdown("## Contexto y método\nLa validación en vivo está en `verify-encft.R`; el perfil independiente de origen, en `profile-quality.R`. Se comparan pesos, claves, integridad entre módulos, series trimestrales y muestras con todas las columnas.\n\n### Supuestos\nLos factores son trimestrales; los indicadores laborales filtran edad >= 15. Los IDs originales no acreditan por sí solos un panel longitudinal. Las verificaciones de conteo Excel/Parquet no comparan cada celda del Excel. Ejecutar desde la raíz del repositorio o desde la carpeta del cuaderno."),
    code("from pathlib import Path\nimport csv, json\nbase = Path.cwd()\nif not (base / 'qa/validation-summary.json').exists():\n    base = base / 'research/mercado-laboral-dominicano/postgresql-recovery/encft'\nqa = base / 'qa'\nsummary = json.loads((qa / 'validation-summary.json').read_text(encoding='utf-8'))\ndef rows(name):\n    with (qa / name).open(encoding='utf-8-sig', newline='') as f:\n        return list(csv.DictReader(f))\nprint('Base:', summary['database'])\nprint('Validación en vivo:', summary['validated_at'])\nprint('Corte:', summary['data_end'])"),
    markdown("## Datos\nLos originales se mantienen en `C:/Users/leona/Desktop/encuesta_trabajo_rd`. El manifiesto SHA256 identifica exactamente los procesados usados y los Excel que completaron las tasas de monedas."),
    code("reconciliation = rows('source-reconciliation.csv')\nassert len(reconciliation) == 55\nassert all(r['status'] in ('ok', 'recovered_from_excel') for r in reconciliation)\nassert summary['quarters'] == 41\nassert summary['rows']['miembros'] == 790993\nprint('Pares módulo-año conciliados:', len(reconciliation))\nprint('Hojas de monedas recuperadas:', sum(r['status'] == 'recovered_from_excel' for r in reconciliation))\nprint(json.dumps(summary['rows'], ensure_ascii=False, indent=2))"),
    markdown("## Resultados\nConsultas centrales ejecutadas por el verificador:\n```sql\nSELECT count(*) FROM microdatos.miembros;\nSELECT count(*) FROM microdatos.miembros m\nLEFT JOIN microdatos.hogares h USING (periodo, upm, vivienda, hogar)\nWHERE h.fila_origen IS NULL;\nSELECT count(*) FROM microdatos.miembros\nWHERE edad >= 15 AND (pea <> ocupado + desocupado OR pet <> pea + inactivo);\n```\nLos controles completos y las tolerancias están en el script. A continuación se comprueba que el registro guardado no contiene errores."),
    code("checks = rows('database-quality.csv')\nassert checks and all(r['estado'] != 'error' for r in checks)\nprint('Controles SQL:', len(checks))\nfor r in checks:\n    if r['estado'] == 'advertencia':\n        print(r['control'], ':', r['observado'], '—', r['nota'])"),
    code("comparison = rows('sql-versus-r.csv')\nassert len(comparison) == 8\nassert all(r['estado'] == 'ok' and int(r['trimestres']) == 41 for r in comparison)\nroundtrip = rows('roundtrip-checks.csv')\nassert len(roundtrip) == 5 and all(r['estado'] == 'ok' for r in roundtrip)\nprint('SQL frente a R: 8 métricas por 41 trimestres verificadas.')\nfor r in roundtrip:\n    print(r['modulo'], ':', r['filas_comparadas'], 'filas y', r['columnas_comparadas'], 'columnas comparadas')"),
    markdown("## Límites y uso\nSe usa la clave estructural para evitar los 73.840 enlaces fallidos por ID_HOGAR en 2023. En 2023-T4, 2.627 observaciones de Miembros tienen pesos distintos de Hogar; una muestra se contrastó con el Excel original. Se conserva el factor de cada módulo. Otras siete observaciones difieren en tamaño del hogar. Se conserva una vivienda sin factor y se separa DESOCUPADO_INI de DESOCUPADO. No se valida aquí cada variable de ingresos, remesas o vivienda ni se certifica un panel longitudinal. El control externo de 2025-T4 procede del [BCRD, PERN 2026-I, p. 23](https://cdn.bancentral.gov.do/documents/publicaciones-economicas/panorama-economico-de-la-region-norte/documents/PERN_2026-I.pdf).\n\nPara repetir la verificación en vivo: ejecutar `verify-encft.R` mediante `tools/run-r.ps1`, con `PG_RECOVERY_PASSWORD` únicamente en el entorno, y reconstruir este cuaderno. Consulte `README.md` para el orden completo de reproducción."),
]
namespace = {"__name__": "__main__"}
counter = 0
for cell in cells:
    if cell["cell_type"] != "code":
        continue
    counter += 1
    output = io.StringIO()
    with contextlib.redirect_stdout(output):
        exec(compile(cell["source"], f"validation.ipynb:cell-{counter}", "exec"), namespace)
    cell["execution_count"] = counter
    cell["outputs"] = [{"output_type": "stream", "name": "stdout", "text": output.getvalue()}]
notebook = {"nbformat": 4, "nbformat_minor": 4, "metadata": {"kernelspec": {"display_name": "Python 3", "language": "python", "name": "python3"}, "language_info": {"name": "python", "version": platform.python_version()}}, "cells": cells}
try:
    import nbformat
    notebook = nbformat.from_dict(notebook)
    nbformat.validate(notebook)
except ImportError:
    assert notebook["nbformat"] == 4 and all("source" in c for c in cells)
path = BASE / "validation.ipynb"
path.write_text(json.dumps(notebook, ensure_ascii=False, indent=1), encoding="utf-8")
print(f"Executed {counter} Python cells top-to-bottom and wrote {path}.")
