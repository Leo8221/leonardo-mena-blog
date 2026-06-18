import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const atlasRoot = path.resolve(here, "..");
const sourcePath = path.join(atlasRoot, "data", "atlas-source.json");
const outputPath = path.join(atlasRoot, "data", "atlas-data.json");

const source = JSON.parse(fs.readFileSync(sourcePath, "utf8"));

const hidden = new Set(source.system.hiddenStatuses.map((status) => normalize(status)));
const visibleModules = source.modules.filter((module) => {
  return module.visible === true && normalize(module.status) === "activo" && !hidden.has(normalize(module.status));
});

const requiredSeries = {
  macro: "macro",
  sectors: "sectors",
  external: "external",
  trade: "trade",
  labor: "labor",
  prices: "prices",
  territory: "territory",
  mipymes: "mipymes",
  visualLab: "visualLab"
};

const errors = [];

for (const module of visibleModules) {
  if (!module.id || !module.title || !module.chart || !module.source || !module.question) {
    errors.push(`${module.id || "(sin id)"}: faltan campos base`);
  }

  const seriesKey = requiredSeries[module.chart];
  if (!seriesKey || !source.series[seriesKey]) {
    errors.push(`${module.id}: no hay datos para chart=${module.chart}`);
  }

  if (!Array.isArray(module.methodology) || module.methodology.length === 0) {
    errors.push(`${module.id}: falta metodologia visible`);
  }
}

if (errors.length > 0) {
  console.error("Atlas data validation failed:");
  for (const error of errors) console.error(`- ${error}`);
  process.exit(1);
}

const output = {
  ...source,
  generatedAt: new Date().toISOString(),
  modules: visibleModules,
  moduleCount: visibleModules.length
};

fs.writeFileSync(outputPath, `${JSON.stringify(output, null, 2)}\n`, "utf8");
console.log(`Atlas data built: ${path.relative(process.cwd(), outputPath)} (${visibleModules.length} modules)`);

function normalize(value) {
  return String(value || "")
    .normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "")
    .trim()
    .toLowerCase();
}
