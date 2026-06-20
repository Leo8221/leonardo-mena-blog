import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const atlasRoot = path.resolve(here, "..");
const sourcePath = path.join(atlasRoot, "data", "atlas-source.json");
const outputPath = path.join(atlasRoot, "data", "atlas-data.json");

const source = JSON.parse(fs.readFileSync(sourcePath, "utf8"));

const hiddenStatusLabels = ["Borrador", "Backlog", "Pendiente", "Proximo", "Próximo"];
const hidden = new Set(hiddenStatusLabels.map((status) => normalize(status)));
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
const allModuleIds = new Set();
const visibleModuleIds = new Set(visibleModules.map((module) => module.id));

if (!source.updated || !isValidDate(source.updated)) {
  errors.push("source.updated debe ser una fecha valida");
}

if (!Array.isArray(source.modules)) {
  errors.push("modules debe ser una lista");
} else {
  for (const [index, module] of source.modules.entries()) {
    const label = module.id || `(modulo ${index + 1})`;
    if (!module.id) {
      errors.push(`${label}: falta id`);
      continue;
    }
    if (allModuleIds.has(module.id)) {
      errors.push(`${module.id}: id duplicado`);
    }
    allModuleIds.add(module.id);
    if (module.updated && !isValidDate(module.updated)) {
      errors.push(`${module.id}: updated no es una fecha valida`);
    }
  }
}

if (Array.isArray(source.metrics)) {
  for (const metric of source.metrics) {
    if (!metric.id || !metric.label || !metric.value || !metric.module) {
      errors.push(`${metric.id || "(metrica sin id)"}: faltan campos base`);
    }
    if (metric.module && !visibleModuleIds.has(metric.module)) {
      errors.push(`${metric.id}: metric.module no apunta a un modulo visible activo`);
    }
  }
}

const brokenText = findBrokenText(source);
for (const issue of brokenText) {
  errors.push(`texto con codificacion sospechosa en ${issue.path}: ${issue.value}`);
}

for (const module of visibleModules) {
  if (!module.id || !module.title || !module.chart || !module.source || !module.question || !module.insight) {
    errors.push(`${module.id || "(sin id)"}: faltan campos base`);
  }

  const seriesKey = requiredSeries[module.chart];
  if (!seriesKey || !source.series[seriesKey]) {
    errors.push(`${module.id}: no hay datos para chart=${module.chart}`);
  }

  if (!Array.isArray(module.methodology) || module.methodology.length === 0) {
    errors.push(`${module.id}: falta metodologia visible`);
  }

  if (!module.summary || !module.family || !module.topic || !module.type) {
    errors.push(`${module.id}: faltan metadatos publicos`);
  }
}

if (errors.length > 0) {
  console.error("Atlas data validation failed:");
  for (const error of errors) console.error(`- ${error}`);
  process.exit(1);
}

const { system, modules, brand, ...publicSource } = source;

const publicModules = visibleModules.map((module) => {
  const {
    sourceDetail,
    methodology,
    related,
    ...publicModule
  } = module;
  return {
    ...publicModule,
    sourceInfo: {
      label: module.source,
      detail: sourceDetail || "",
      updated: module.updated || source.updated || "",
      methodology: Array.isArray(methodology) ? methodology.filter(Boolean) : [],
      related: Array.isArray(related) ? related.filter(Boolean) : []
    }
  };
});

const output = {
  ...publicSource,
  brand: {
    name: brand.name,
    shortName: brand.shortName
  },
  generatedAt: new Date().toISOString(),
  modules: publicModules,
  moduleCount: publicModules.length
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

function isValidDate(value) {
  const text = String(value || "").trim();
  if (!text) return false;
  return !Number.isNaN(Date.parse(text));
}

function findBrokenText(value, pathName = "atlas-source.json") {
  const issues = [];
  const patterns = [
    /[\uFFFD]/,
    /\u00c3[\u0080-\u00BF]?/,
    /\u00c2[\u0080-\u00BF]?/,
    /\u00e2[\u0080-\u00BF\u20ac\u2122\u2019\u201c\u201d\u2013\u2014]?/
  ];

  function walk(node, currentPath) {
    if (typeof node === "string") {
      if (patterns.some((pattern) => pattern.test(node))) {
        issues.push({ path: currentPath, value: node.slice(0, 120) });
      }
      return;
    }
    if (Array.isArray(node)) {
      node.forEach((item, index) => walk(item, `${currentPath}[${index}]`));
      return;
    }
    if (node && typeof node === "object") {
      for (const [key, child] of Object.entries(node)) {
        walk(child, `${currentPath}.${key}`);
      }
    }
  }

  walk(value, pathName);
  return issues;
}
