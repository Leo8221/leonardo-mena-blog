"""Prepara atlas-data.json, el archivo que lee la página del Atlas.

Recorrido: los scripts R leen las fuentes y calculan indicadores; este archivo
une esos resultados con el catálogo, los valida y escribe el JSON público.
El navegador solo necesita ese JSON para mostrar y filtrar las vistas.

Para seguir el proceso, empieza en main(), al final. build_atlas_data() hace
el mismo trabajo en memoria y permite probarlo sin sobrescribir los datos.
No descarga fuentes ni necesita paquetes externos de Python.
"""

import argparse
from copy import deepcopy
from datetime import datetime, timezone
from decimal import Decimal, ROUND_HALF_UP
import json
import math
from pathlib import Path
import re
import unicodedata


DATA_DIR = Path(__file__).resolve().parents[1] / "data"
CHART_TYPES = {
    "macro", "sectors", "external", "trade", "labor", "prices",
    "territory", "mipymes", "visualLab",
}

# Los textos describen las series que entrega build-bcrd-live-data.R.
# Solo sustituyen al catálogo base cuando existe esa carga de datos.
MODULE_UPDATES = {
    "pulso-macro": {
        "dataMode": "Automático"
    },
    "sectores": {
        "dataMode": "Modelo",
        "source": "BCRD",
        "sourceDetail": "PIB por actividad económica y drivers normalizados.",
        "insight": "Peso relativo y señal dominante por sector."
    },
    "contexto-externo": {
        "dataMode": "Modelo",
        "source": "BCRD",
        "title": "Servicios al exterior",
        "topic": "Comercio de servicios",
        "summary": "Evolución de las exportaciones de servicios y sus principales rubros.",
        "sourceDetail": "Créditos de la balanza de servicios del Banco Central.",
        "question": "¿Cómo evolucionan las exportaciones de servicios?",
        "insight": "El índice compara ingresos por servicios con su máximo en el periodo mostrado.",
        "methodology": [
            "La serie toma los créditos anuales de la balanza de servicios del BCRD.",
            "Cada valor se divide por el máximo del periodo y se multiplica por 100.",
            "Los rubros del último año se normalizan respecto al mayor rubro; no son participaciones del total.",
            "Este índice no mide presión externa ni condiciones financieras globales."
        ]
    },
    "comercio-exterior": {
        "dataMode": "Automático",
        "source": "BCRD, DGA",
        "sourceDetail": "BCRD para flujos agregados y DGA para capítulos de exportación.",
        "question": "¿Cómo se mueven las exportaciones e importaciones?",
        "insight": "Exportaciones, importaciones y canasta exportadora en una sola vista.",
        "methodology": [
            "Se descargan los Excel oficiales del CustomView de sector externo del BCRD.",
            "Los flujos comparan el mismo número de trimestres en cada año; el año en curso es acumulado.",
            "La canasta exportadora usa capítulos publicados por la DGA cuando están disponibles."
        ]
    },
    "mercado-laboral": {
        "dataMode": "Automático",
        "source": "BCRD ENCFT",
        "sourceDetail": "Excel oficiales de la Encuesta Nacional Continua de Fuerza de Trabajo.",
        "question": "¿Qué dicen los últimos indicadores laborales?",
        "insight": "Tasas laborales recientes y ocupación por rama de actividad.",
        "methodology": [
            "Se descargan los Excel oficiales de ENCFT desde el CustomView del BCRD.",
            "Los indicadores toman el último trimestre disponible.",
            "La distribución por rama usa ocupados por actividad económica."
        ]
    },
    "costo-vida": {
        "dataMode": "Automático",
        "source": "BCRD",
        "sourceDetail": "Excel oficiales de IPC general, subyacente e IPC por grupos.",
        "question": "¿Dónde se concentra la presión reciente de precios?",
        "insight": "Inflación general, subyacente y variación mensual por grupo.",
        "methodology": [
            "Se descargan los Excel oficiales de precios del BCRD.",
            "La inflación general y subyacente usa variación interanual mensual.",
            "Los grupos se ordenan por la variación mensual absoluta más reciente."
        ]
    },
    "territorio-infraestructura": {
        "dataMode": "Corte fijo"
    },
    "mipymes-productividad": {
        "dataMode": "Artículo"
    },
    "laboratorio-visual": {
        "dataMode": "Artículo"
    }
}


def normalize(value):
    """Compara estados como ' Actívo ' y 'activo' sin cambiar el texto mostrado."""
    text = unicodedata.normalize("NFD", str(value or ""))
    return "".join(char for char in text if not unicodedata.combining(char)).strip().lower()


def number_or_null(value):
    """Conserva hasta cuatro decimales; un dato ausente nunca se vuelve cero.

    Las unidades llegan definidas por R: por ejemplo, TPM=5.25 significa
    5.25 %, no 0.0525. Aquí no se convierten unidades ni se recalculan tasas.
    Decimal reproduce el redondeo anterior, incluso en empates binarios.
    """
    if value is None or isinstance(value, bool) or (isinstance(value, str) and not value.strip()):
        return None
    try:
        number = float(value)
    except (ValueError, TypeError):
        return None
    if not math.isfinite(number):
        return None
    if abs(number) >= 1e21:
        return number
    rounded = float(Decimal.from_float(number).quantize(Decimal("0.0001"), rounding=ROUND_HALF_UP))
    return int(rounded) if rounded.is_integer() else rounded


def project_rows(rows, text_fields, number_fields, empty_text=()):
    """Elige las columnas públicas y convierte solo las numéricas.

    Se conserva el orden de R. Los nombres son el contrato que utiliza la web;
    cambiar 'tpm' por otro nombre aquí rompería su selector y su exportación.
    """
    return [
        {
            **{field: row[field] for field in text_fields if field in row},
            **{field: number_or_null(row.get(field)) for field in number_fields},
            **{field: row.get(field) or "" for field in empty_text},
        }
        for row in rows
    ]


def apply_live_data(source, live):
    """Une catálogo y resultados de R sin modificar los objetos de entrada.

    Los mínimos de meses evitan sustituir una serie por una carga incompleta.
    La TPM mantiene su propio corte: esperar al IMAE ocultaría meses de la tasa.
    Los bloques vacíos de socios, salarios y traspaso no se rellenan con ejemplos.
    """
    result = deepcopy(source)
    if not live or not isinstance(live.get("series"), dict):
        return result
    incoming = live["series"]
    series = result.setdefault("series", {})

    # Cada fila declara: serie, mínimo de observaciones, texto, números y texto opcional.
    flat_series = [
        ("policyRate", 1, ("period",), ("tpm",), ()),
        ("macro", 6, ("period",), ("dolar", "inflacion", "imae", "tpm"), ()),
        ("external", 3, ("period",), ("pressure",), ()),
        ("drivers", 1, ("driver",), ("value",), ()),
        ("sectors", 1, ("sector",), ("pressure",), ("driver", "direction")),
    ]
    for name, minimum, text, numbers, optional in flat_series:
        rows = incoming.get(name)
        if isinstance(rows, list) and len(rows) >= minimum:
            series[name] = project_rows(rows, text, numbers, optional)

    # Estos bloques contienen varias tablas. Una lista vacía es válida: indica
    # que R entregó cero filas, y se conserva para no mostrar datos anteriores.
    tables = [
        ("prices", "timeline", ("period",), ("headline", "core"), ()),
        ("prices", "components", ("component",), ("contribution", "pressure"), ()),
        ("trade", "flows", ("period",), ("exports", "imports"), ()),
        ("trade", "products", ("name",), ("share", "complexity"), ("signal",)),
        ("labor", "indicators", ("group",), ("value",), ()),
        ("labor", "sectors", ("name",), ("jobs",), ()),
        ("labor", "trend", ("period",), ("employment", "employers"), ()),
    ]
    for group, name, text, numbers, optional in tables:
        rows = (incoming.get(group) or {}).get(name)
        if isinstance(rows, list):
            block = series.setdefault(group, {})
            block[name] = project_rows(rows, text, numbers, optional)
            clear_table = {
                ("prices", "components"): "passThrough",
                ("trade", "products"): "partners",
                ("labor", "indicators"): "outcomes",
            }.get((group, name))
            if clear_table:
                block[clear_table] = []

    metrics = live.get("metrics") or {}
    if isinstance(result.get("metrics"), list):
        result["metrics"] = [
            {**metric, **(metrics.get(metric["id"]) or {})}
            for metric in result["metrics"]
        ]
    result["liveDataCutoff"] = live.get("dataCutoff") or {}
    if isinstance(result.get("modules"), list):
        result["modules"] = [
            {**module, **MODULE_UPDATES.get(module.get("id"), {})}
            for module in result["modules"]
        ]
    return result


def valid_date(value):
    """Los cortes editoriales usan fechas ISO: AAAA-MM-DD, con hora opcional."""
    try:
        datetime.fromisoformat(str(value).strip().replace("Z", "+00:00"))
        return True
    except ValueError:
        return False


def broken_text(value, path="atlas-source.json"):
    """Devuelve dónde hay señales de UTF-8 mal leído; nunca repara el contenido."""
    issues = []
    if isinstance(value, str) and re.search("[\ufffd\u00c3\u00c2\u00e2]", value):
        issues.append(f"texto con codificación sospechosa en {path}: {value[:120]}")
    elif isinstance(value, dict):
        for key, child in value.items():
            issues.extend(broken_text(child, f"{path}.{key}"))
    elif isinstance(value, list):
        for index, child in enumerate(value):
            issues.extend(broken_text(child, f"{path}[{index}]"))
    return issues


def validate(source, source_run):
    """Detiene la publicación si faltan datos, procedencia o metadatos obligatorios.

    Devuelve los módulos publicables. No basta con tener datos: cada módulo
    debe estar marcado visible=true y activo. Un borrador nunca pasa ese filtro.
    """
    errors = []
    modules = source.get("modules")
    if not isinstance(modules, list):
        raise ValueError("modules debe ser una lista")
    if not valid_date(source.get("updated")):
        errors.append("source.updated debe ser una fecha válida")
    visible = [module for module in modules if module.get("visible") is True and normalize(module.get("status")) == "activo"]
    ids = set()
    for index, module in enumerate(modules):
        module_id = module.get("id")
        if not module_id:
            errors.append(f"módulo {index + 1}: falta id")
        elif module_id in ids:
            errors.append(f"{module_id}: id duplicado")
        ids.add(module_id)
        if module.get("updated") and not valid_date(module["updated"]):
            errors.append(f"{module_id}: updated no es una fecha válida")
    visible_ids = {module["id"] for module in visible if module.get("id")}
    metrics = source.get("metrics")
    for metric in metrics if isinstance(metrics, list) else []:
        if not all(metric.get(field) for field in ("id", "label", "value", "module")):
            errors.append(f"{metric.get('id')}: faltan campos base de la métrica")
        if metric.get("module") and metric["module"] not in visible_ids:
            errors.append(f"{metric.get('id')}: metric.module no apunta a un módulo visible activo")
    for module in visible:
        module_id = module.get("id")
        if not all(module.get(field) for field in ("id", "title", "chart", "source", "question", "insight")):
            errors.append(f"{module_id}: faltan campos base")
        chart = module.get("chart")
        if chart not in CHART_TYPES or chart not in source.get("series", {}):
            errors.append(f"{module_id}: no hay datos para chart={chart}")
        if not isinstance(module.get("methodology"), list) or not module["methodology"]:
            errors.append(f"{module_id}: falta metodología visible")
        if not all(module.get(field) for field in ("summary", "family", "topic", "type")):
            errors.append(f"{module_id}: faltan metadatos públicos")
    errors.extend(broken_text(source))
    for item in (source_run or {}).get("sources", []):
        if item.get("required") is True and item.get("status") in {"missing", "download_failed", "copy_failed"}:
            errors.append(f"fuente obligatoria fallida: {item['id']} ({item['status']})")
    if errors:
        raise ValueError("Atlas data validation failed:\n- " + "\n- ".join(errors))
    return visible


def public_source_run(run):
    """Publica la procedencia útil, dejando fuera rutas locales y detalles de descarga."""
    if not run:
        return {
            "generatedAt": "",
            "summary": dict.fromkeys(("total", "downloaded", "cached", "verified", "manual", "failed"), 0),
            "sources": [],
        }
    sources = []
    for source in run.get("sources", []):
        item = {key: source[key] for key in ("id", "label", "mode", "kind", "status") if key in source}
        item["required"] = source.get("required") is True
        for key in ("bytes", "discovered", "selected", "downloaded"):
            item[key] = source.get(key) or None
        for key in ("modifiedAt", "md5"):
            item[key] = source.get(key) or ""
        item["usedBy"] = source.get("usedBy") if isinstance(source.get("usedBy"), list) else []
        item["updatePolicy"] = source.get("updatePolicy") or ""
        sources.append(item)
    return {
        "generatedAt": run.get("generatedAt") or "",
        "manifestVersion": run.get("manifestVersion") or None,
        "summary": run.get("summary") or {},
        "sources": sources,
    }


def build_atlas_data(source, live=None, source_run=None, generated_at=None):
    """Devuelve el documento público sin escribir archivos.

    generated_at permite repetir una prueba con fecha fija. Describe cuándo se
    construyó el archivo; los cortes estadísticos se conservan por separado.
    """
    merged = apply_live_data(source, live)
    visible = validate(merged, source_run)
    cutoffs = merged.get("liveDataCutoff", {})
    macro_cutoff = cutoffs.get("macro", "")
    if cutoffs.get("policyRate"):
        macro_cutoff = f"Series macro: {macro_cutoff}; TPM: {cutoffs['policyRate']}"
    module_cutoffs = {
        "pulso-macro": macro_cutoff,
        "sectores": cutoffs.get("sectors"),
        "contexto-externo": cutoffs.get("external"),
        "comercio-exterior": cutoffs.get("trade"),
        "mercado-laboral": cutoffs.get("labor"),
        "costo-vida": cutoffs.get("prices"),
    }
    public_modules = []
    for module in visible:
        public = {key: value for key, value in module.items() if key not in {"sourceDetail", "methodology", "related"}}
        public["sourceInfo"] = {
            "label": module["source"],
            "detail": module.get("sourceDetail") or "",
            "dataMode": module.get("dataMode") or "",
            "updated": module.get("updated") or module_cutoffs.get(module["id"]) or "",
            "methodology": list(filter(None, module.get("methodology", []))),
            "related": list(filter(None, module["related"])) if isinstance(module.get("related"), list) else [],
        }
        public_modules.append(public)
    return {
        **{key: value for key, value in merged.items() if key not in {"system", "modules", "brand"}},
        "brand": {key: merged["brand"][key] for key in ("name", "shortName")},
        "generatedAt": generated_at or datetime.now(timezone.utc).isoformat(timespec="milliseconds").replace("+00:00", "Z"),
        "sourceRun": public_source_run(source_run),
        "liveData": {
            "generatedAt": (live or {}).get("generatedAt") or "",
            "dataCutoff": (live or {}).get("dataCutoff") or {},
            "active": live is not None,
        },
        "modules": public_modules,
        "moduleCount": len(public_modules),
    }


def read_json(path, optional=False):
    """La falta de una carga opcional se admite; un JSON roto siempre es un error."""
    if optional and not path.exists():
        return None
    return json.loads(path.read_text(encoding="utf-8"))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data-dir", type=Path, default=DATA_DIR, help="Carpeta de entradas y salida; útil para pruebas aisladas.")
    args = parser.parse_args()
    try:
        output = build_atlas_data(
            read_json(args.data_dir / "atlas-source.json"),
            read_json(args.data_dir / "bcrd-live-data.json", optional=True),
            read_json(args.data_dir / "source-run.json", optional=True),
        )
        # Se valida y serializa todo antes de tocar la salida anterior.
        text = json.dumps(output, ensure_ascii=False, allow_nan=False, indent=2) + "\n"
        path = args.data_dir / "atlas-data.json"
        path.write_text(text, encoding="utf-8", newline="\n")
    except (OSError, ValueError) as error:
        parser.exit(1, f"{error}\n")
    print(f"Atlas data built: {path} ({output['moduleCount']} modules)")


if __name__ == "__main__":
    main()
