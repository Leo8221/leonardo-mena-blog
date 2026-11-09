"""Pruebas del contrato público. Ejecutar con tools/run-python.ps1 en Windows."""

from copy import deepcopy
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

from build_atlas_data import (
    DATA_DIR, apply_live_data, build_atlas_data, number_or_null, public_source_run, read_json,
)


class AtlasDataTests(unittest.TestCase):
    def setUp(self):
        self.source = read_json(DATA_DIR / "atlas-source.json")
        self.live = read_json(DATA_DIR / "bcrd-live-data.json")
        self.run = read_json(DATA_DIR / "source-run.json", optional=True)

    def test_public_document_matches_checked_in_contract(self):
        expected = read_json(DATA_DIR / "atlas-data.json")
        actual = build_atlas_data(self.source, self.live, self.run, expected["generatedAt"])
        self.assertEqual(actual, expected)

    def test_inputs_are_not_modified(self):
        before = deepcopy((self.source, self.live, self.run))
        build_atlas_data(self.source, self.live, self.run)
        self.assertEqual((self.source, self.live, self.run), before)

    def test_missing_and_incomplete_loads_keep_the_previous_series(self):
        self.assertEqual(apply_live_data(self.source, None), self.source)
        partial = {"series": {"macro": self.live["series"]["macro"][:3]}}
        result = apply_live_data(self.source, partial)
        self.assertEqual(result["series"]["macro"], self.source["series"]["macro"])

    def test_explicitly_empty_table_clears_older_values(self):
        result = apply_live_data(self.source, {"series": {"prices": {"components": []}}})
        self.assertEqual(result["series"]["prices"]["components"], [])
        self.assertEqual(result["series"]["prices"]["passThrough"], [])

    def test_missing_values_do_not_become_zero(self):
        for value in (None, "", " ", "sin dato", float("nan"), float("inf"), True):
            with self.subTest(value=value):
                self.assertIsNone(number_or_null(value))
        for value, expected in ((0, 0), (5.25, 5.25), ("5.50", 5.5), (1.23456, 1.2346), (-1.23456, -1.2346)):
            self.assertEqual(number_or_null(value), expected)
        self.live["series"]["policyRate"][0]["tpm"] = None
        self.assertIsNone(apply_live_data(self.source, self.live)["series"]["policyRate"][0]["tpm"])

    def test_policy_rate_has_independent_cutoff_and_raw_precision(self):
        # Entrada fija: esta prueba no depende de cuál sea el primer mes descargado mañana.
        self.live["series"]["macro"][0]["dolar"] = 63.6423
        result = build_atlas_data(self.source, self.live)
        self.assertEqual(len(result["series"]["policyRate"]), 24)
        self.assertEqual(result["series"]["macro"][0]["dolar"], 63.6423)
        self.assertEqual(result["liveDataCutoff"]["policyRate"], result["series"]["policyRate"][-1]["period"])

    def test_hidden_and_draft_modules_are_not_published(self):
        hidden = deepcopy(self.source["modules"][0])
        hidden.update(id="prueba-oculta", visible=False)
        draft = {**hidden, "id": "prueba-borrador", "visible": True, "status": "Borrador"}
        self.source["modules"].extend([hidden, draft])
        ids = {module["id"] for module in build_atlas_data(self.source, self.live)["modules"]}
        self.assertNotIn(hidden["id"], ids)
        self.assertNotIn(draft["id"], ids)

    def test_required_source_failures_block_output(self):
        for status in ("missing", "download_failed", "copy_failed"):
            with self.subTest(status=status), self.assertRaisesRegex(ValueError, "fuente obligatoria fallida"):
                build_atlas_data(self.source, self.live, {"sources": [{"id": "fuente", "required": True, "status": status}]})

    def test_invalid_catalogs_fail_with_a_readable_error(self):
        cases = [
            (lambda source: source.update(modules=None), "modules debe ser una lista"),
            (lambda source: source.update(updated="2026-02-30"), "fecha válida"),
            (lambda source: source["modules"].append(deepcopy(source["modules"][0])), "id duplicado"),
            (lambda source: source["modules"][0].update(methodology=[]), "metodología"),
            (lambda source: source["modules"][0].update(chart="desconocido"), "no hay datos"),
            (lambda source: source["metrics"][0].update(module="desconocido"), "módulo visible"),
            (lambda source: source["brand"].update(name="Cr\u00c3\u00a9dito"), "codificación"),
        ]
        for change, message in cases:
            with self.subTest(message=message), self.assertRaisesRegex(ValueError, message):
                source = deepcopy(self.source)
                change(source)
                build_atlas_data(source, self.live)

    def test_private_source_paths_are_not_exposed(self):
        result = public_source_run({"sources": [{"id": "ipc", "path": "private/raw.xls", "url": "private", "required": True}]})
        self.assertNotIn("private", json.dumps(result))
        self.assertTrue(result["sources"][0]["required"])

    def test_cli_preserves_previous_output_on_validation_failure(self):
        with tempfile.TemporaryDirectory() as directory:
            folder = Path(directory)
            self.source["updated"] = "incorrecta"
            (folder / "atlas-source.json").write_text(json.dumps(self.source), encoding="utf-8")
            output = folder / "atlas-data.json"
            output.write_text("salida anterior", encoding="utf-8")
            process = subprocess.run(
                [sys.executable, "-X", "utf8", str(Path(__file__).with_name("build_atlas_data.py")), "--data-dir", directory],
                capture_output=True, text=True, encoding="utf-8",
            )
            self.assertNotEqual(process.returncode, 0)
            self.assertIn("fecha válida", process.stderr)
            self.assertEqual(output.read_text(encoding="utf-8"), "salida anterior")


if __name__ == "__main__":
    unittest.main()
