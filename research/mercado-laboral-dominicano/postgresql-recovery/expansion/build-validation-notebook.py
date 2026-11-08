"""Execute an offline audit companion; the live SQL verification is separate."""
from pathlib import Path
import contextlib
import io
import json
import platform

BASE = Path('research/mercado-laboral-dominicano/postgresql-recovery/expansion')

def md(text):
    return {'cell_type': 'markdown', 'metadata': {}, 'source': text}

def code(text):
    return {'cell_type': 'code', 'metadata': {}, 'source': text,
            'execution_count': None, 'outputs': []}

cells = [
    md('# Ampliación histórica: evidencia de la carga\n\nLa base `economia_rd` añade series nacionales, educación, compras y encuestas complementarias. Este cuaderno **revisa archivos de evidencia ya generados**: no consulta PostgreSQL ni descarga datos. La verificación en vivo está en `verify-expansion.R`; la comparación CSV/SQL, en `check-roundtrip.R`. No contiene contraseñas.'),
    md('## Método y supuestos\nSe preservan valores originales y se tipan fechas, identificadores y medidas. Las vistas añaden unidades, etiquetas, períodos y banderas de calidad. Las filas de tablas relacionadas no son observaciones independientes. No se supone un panel de las mismas personas entre encuestas. Las ausencias y años incompletos no se rellenan. Ejecutar desde la raíz del repositorio o desde esta carpeta.'),
    code("from pathlib import Path\nimport csv, json\nbase = Path.cwd()\nif not (base / 'qa/validation-summary.json').exists():\n    base = base / 'research/mercado-laboral-dominicano/postgresql-recovery/expansion'\nqa = base / 'qa'\ndef read_json(name):\n    return json.loads((qa / name).read_text(encoding='utf-8-sig'))\ndef read_csv(name):\n    with (qa / name).open(encoding='utf-8-sig', newline='') as f:\n        return list(csv.DictReader(f))\nsummary = read_json('validation-summary.json')\nprint('Base:', summary['database'])\nprint('Validación técnica:', summary['validated_at'])\nprint('Estado:', summary['status'])\nprint('Tamaño GiB:', round(summary['database_bytes'] / 1024**3, 2))"),
    md('## Datos y procedencia\nLos manifiestos SHA256 identifican los archivos nacionales, doce paquetes anuales de DGCP y 150 lotes de WDI. El catálogo descargado es más amplio que las hojas interpretadas y cargadas en SQL. La comprobación de archivos también incluye los dos diccionarios WDI y el histórico de dólar.'),
    code("tables = read_csv('table-inventory.csv')\nassert len(tables) == summary['tables'] == 45\nassert len({r['tabla'] for r in tables}) == len(tables)\nassert sum(int(r['filas']) for r in tables) == summary['rows'] == 17186057\nsnapshot = read_json('source-snapshot-check.json')\nassert snapshot['status'] == 'ok'\nassert snapshot['extracted_csv_files'] == 156\nprint('Tablas inventariadas:', len(tables))\nprint('Filas físicas inventariadas:', summary['rows'])\nprint('Archivos descargados verificados:', snapshot['downloaded_files'])\nprint('CSV extraídos verificados:', snapshot['extracted_csv_files'])"),
    md('## Resultados de los controles\nLos ceros siguientes prueban las condiciones consultadas, no la exactitud de cada respuesta de la fuente. Se concilian meses/componentes, claves, enlaces y unidades; las advertencias se conservan explícitamente.'),
    code("checks = read_csv('database-checks.csv')\nassert len(checks) == summary['checks'] == 24\nassert summary['errors'] == 0\nassert all(r['estado'] != 'error' for r in checks)\nassert sum(r['estado'] == 'advertencia' for r in checks) == summary['warnings'] == 9\nfor r in checks:\n    if r['estado'] == 'advertencia':\n        print(r['control'], ':', r['observado'], '—', r['nota'])"),
    code("roundtrip = read_csv('roundtrip-dgcp.csv')\nassert len(roundtrip) == 26\nassert all(r['estado'] == 'ok' for r in roundtrip)\nassert {int(r['anio']) for r in roundtrip} == {2015, 2026}\nexamples = read_csv('example-queries.csv')\nassert len(examples) == 19 and all(r['estado'] == 'ok' for r in examples)\nassert all(int(r['filas']) > 0 for r in examples[:-1])\nassert int(examples[-1]['filas']) == 0\nprint('Comparaciones CSV/SQL de todas las columnas de filas muestreadas:', len(roundtrip))\nprint('Consultas SQL de ejemplo ejecutadas:', len(examples))\nprint('Última consulta: sin huecos mensuales en el índice general del IPC.')"),
    code("wdi = read_json('wdi.json')[0]\nassert wdi['observaciones'] == 1083106\nassert wdi['indicadores_rd'] == 1363\nassert wdi['paises_y_agregados'] == 25\nassert (wdi['desde'], wdi['hasta']) == (1960, 2025)\nprint('WDI:', wdi)\nfor r in summary['coverage']:\n    print(r['dataset'], r['desde'], 'a', r['hasta'], r['frecuencia'])"),
    code("details = read_json('analytical-summary.json')\npruebas = details['pruebas'][0]\nassert pruebas['registros'] == 54821\nassert pruebas['duplicados_exactos_excluidos'] == 2765\nassert pruebas['registros_ambiguos'] == 988\ncentral = read_json('central-registration.json')\nassert len(central['views']) == details['views'] == 35\nassert len(central['read_checks']) == 5\nprint('Vistas disponibles en', central['database'] + '.' + central['schema'], ':', len(central['views']))\nprint('Pruebas nacionales:', pruebas)\nprint('Educación internacional:', details['wdi_education'][0])"),
    md('## Límites y siguiente uso\nEn MINERD se colapsaron exclusivamente repeticiones exactas; las claves con contenidos distintos siguen marcadas. Los resultados de pruebas son participaciones, no alumnos únicos, y no forman una serie homogénea sin revisar convocatorias/cobertura. En presupuesto se excluyen unidades pendientes y filas no conciliadas de la vista predeterminada. Contrato no equivale a pago; TSS cuenta empleos cotizantes; cada encuesta conserva su factor y universo. El catálogo WDI incluye países y agregados y sus series tienen coberturas distintas.\n\nConsulte [README.md](README.md) y [example-queries.sql](example-queries.sql) antes de calcular resultados. Para renovar esta evidencia, ejecutar los verificadores en vivo, luego `finish-catalog.R`, `register-central.R`, `restore-snapshot.ps1 -VerifyOnly` y reconstruir este cuaderno. No se certifica aquí la comparabilidad sustantiva de los 1.497 indicadores ni cada celda de los originales.'),
]
namespace = {'__name__': '__main__'}
counter = 0
for cell in cells:
    if cell['cell_type'] != 'code':
        continue
    counter += 1
    capture = io.StringIO()
    with contextlib.redirect_stdout(capture):
        exec(compile(cell['source'], f'validation.ipynb:cell-{counter}', 'exec'), namespace)
    cell['execution_count'] = counter
    cell['outputs'] = [{'output_type': 'stream', 'name': 'stdout', 'text': capture.getvalue()}]
notebook = {'nbformat': 4, 'nbformat_minor': 4, 'metadata': {
    'kernelspec': {'display_name': 'Python 3', 'language': 'python', 'name': 'python3'},
    'language_info': {'name': 'python', 'version': platform.python_version()}}, 'cells': cells}
try:
    import nbformat
    nbformat.validate(nbformat.from_dict(notebook))
except ImportError:
    assert all('source' in c for c in cells)
(BASE / 'validation.ipynb').write_text(json.dumps(notebook, ensure_ascii=False, indent=1), encoding='utf-8')
print(f'Executed {counter} cells top-to-bottom and wrote {BASE / "validation.ipynb"}.')
