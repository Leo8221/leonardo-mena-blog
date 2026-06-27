import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const atlasRoot = path.resolve(here, "..");
const sourcePath = path.join(atlasRoot, "data", "atlas-source.json");
const outputPath = path.join(atlasRoot, "data", "atlas-data.json");
const sourceRunPath = path.join(atlasRoot, "data", "source-run.json");
const bcrdLivePath = path.join(atlasRoot, "data", "bcrd-live-data.json");

const source = JSON.parse(fs.readFileSync(sourcePath, "utf8"));
const sourceRun = fs.existsSync(sourceRunPath)
  ? JSON.parse(fs.readFileSync(sourceRunPath, "utf8"))
  : null;
const bcrdLive = fs.existsSync(bcrdLivePath)
  ? JSON.parse(fs.readFileSync(bcrdLivePath, "utf8"))
  : null;
const hydratedSource = applyLiveBcrdData(source, bcrdLive);

const hiddenStatusLabels = ["Borrador", "Backlog", "Pendiente", "Proximo", "Próximo"];
const hidden = new Set(hiddenStatusLabels.map((status) => normalize(status)));
const visibleModules = hydratedSource.modules.filter((module) => {
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

if (!hydratedSource.updated || !isValidDate(hydratedSource.updated)) {
  errors.push("source.updated debe ser una fecha valida");
}

if (!Array.isArray(hydratedSource.modules)) {
  errors.push("modules debe ser una lista");
} else {
  for (const [index, module] of hydratedSource.modules.entries()) {
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

if (Array.isArray(hydratedSource.metrics)) {
  for (const metric of hydratedSource.metrics) {
    if (!metric.id || !metric.label || !metric.value || !metric.module) {
      errors.push(`${metric.id || "(metrica sin id)"}: faltan campos base`);
    }
    if (metric.module && !visibleModuleIds.has(metric.module)) {
      errors.push(`${metric.id}: metric.module no apunta a un modulo visible activo`);
    }
  }
}

const brokenText = findBrokenText(hydratedSource);
for (const issue of brokenText) {
  errors.push(`texto con codificacion sospechosa en ${issue.path}: ${issue.value}`);
}

if (sourceRun) {
  const failedSources = (sourceRun.sources || []).filter((item) => {
    return item.required === true && ["missing", "download_failed", "copy_failed"].includes(item.status);
  });
  for (const item of failedSources) {
    errors.push(`fuente obligatoria fallida: ${item.id} (${item.status})`);
  }
}

for (const module of visibleModules) {
  if (!module.id || !module.title || !module.chart || !module.source || !module.question || !module.insight) {
    errors.push(`${module.id || "(sin id)"}: faltan campos base`);
  }

  const seriesKey = requiredSeries[module.chart];
  if (!seriesKey || !hydratedSource.series[seriesKey]) {
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

const { system, modules, brand, ...publicSource } = hydratedSource;

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
      dataMode: module.dataMode || "",
      updated: module.updated || hydratedSource.updated || "",
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
  sourceRun: publicSourceRun(sourceRun),
  liveData: publicLiveData(bcrdLive),
  modules: publicModules,
  moduleCount: publicModules.length
};

fs.writeFileSync(outputPath, `${JSON.stringify(output, null, 2)}\n`, "utf8");
console.log(`Atlas data built: ${path.relative(process.cwd(), outputPath)} (${visibleModules.length} modules)`);

function applyLiveBcrdData(baseSource, live) {
  const copy = JSON.parse(JSON.stringify(baseSource));
  if (!live || !live.series) return copy;

  copy.updated = new Date().toISOString().slice(0, 10);
  copy.series = copy.series || {};

  if (Array.isArray(live.series.macro) && live.series.macro.length >= 6) {
    copy.series.macro = live.series.macro.map((row) => ({
      period: row.period,
      dolar: numberOrNull(row.dolar),
      inflacion: numberOrNull(row.inflacion),
      imae: numberOrNull(row.imae),
      tpm: numberOrNull(row.tpm)
    }));
  }

  if (live.series.prices?.timeline && Array.isArray(live.series.prices.timeline)) {
    copy.series.prices = copy.series.prices || {};
    copy.series.prices.timeline = live.series.prices.timeline.map((row) => ({
      period: row.period,
      headline: numberOrNull(row.headline),
      core: numberOrNull(row.core)
    }));
  }

  if (live.series.prices?.components && Array.isArray(live.series.prices.components)) {
    copy.series.prices = copy.series.prices || {};
    copy.series.prices.components = live.series.prices.components.map((row) => ({
      component: row.component,
      contribution: numberOrNull(row.contribution),
      pressure: numberOrNull(row.pressure)
    }));
    copy.series.prices.passThrough = [];
  }

  if (Array.isArray(live.series.external) && live.series.external.length >= 3) {
    copy.series.external = live.series.external.map((row) => ({
      period: row.period,
      pressure: numberOrNull(row.pressure)
    }));
  }

  if (Array.isArray(live.series.drivers) && live.series.drivers.length > 0) {
    copy.series.drivers = live.series.drivers.map((row) => ({
      driver: row.driver,
      value: numberOrNull(row.value)
    }));
  }

  if (Array.isArray(live.series.sectors) && live.series.sectors.length > 0) {
    copy.series.sectors = live.series.sectors.map((row) => ({
      sector: row.sector,
      pressure: numberOrNull(row.pressure),
      driver: row.driver || "",
      direction: row.direction || ""
    }));
  }

  if (live.series.trade?.flows && Array.isArray(live.series.trade.flows)) {
    copy.series.trade = copy.series.trade || {};
    copy.series.trade.flows = live.series.trade.flows.map((row) => ({
      period: row.period,
      exports: numberOrNull(row.exports),
      imports: numberOrNull(row.imports)
    }));
  }

  if (live.series.trade?.products && Array.isArray(live.series.trade.products)) {
    copy.series.trade = copy.series.trade || {};
    copy.series.trade.products = live.series.trade.products.map((row) => ({
      name: row.name,
      share: numberOrNull(row.share),
      complexity: numberOrNull(row.complexity),
      signal: row.signal || ""
    }));
    copy.series.trade.partners = [];
  }

  if (live.series.labor?.indicators && Array.isArray(live.series.labor.indicators)) {
    copy.series.labor = copy.series.labor || {};
    copy.series.labor.indicators = live.series.labor.indicators.map((row) => ({
      group: row.group,
      value: numberOrNull(row.value)
    }));
    copy.series.labor.outcomes = [];
  }

  if (live.series.labor?.sectors && Array.isArray(live.series.labor.sectors)) {
    copy.series.labor = copy.series.labor || {};
    copy.series.labor.sectors = live.series.labor.sectors.map((row) => ({
      name: row.name,
      jobs: numberOrNull(row.jobs)
    }));
  }

  if (live.series.labor?.trend && Array.isArray(live.series.labor.trend)) {
    copy.series.labor = copy.series.labor || {};
    copy.series.labor.trend = live.series.labor.trend.map((row) => ({
      period: row.period,
      employment: numberOrNull(row.employment),
      employers: numberOrNull(row.employers)
    }));
  }

  if (live.metrics && Array.isArray(copy.metrics)) {
    copy.metrics = copy.metrics.map((metric) => {
      const update = live.metrics[metric.id];
      return update ? { ...metric, ...update } : metric;
    });
  }

  copy.liveDataCutoff = live.dataCutoff || {};
  copy.modules = copy.modules.map((module) => {
    if (module.id === "pulso-macro") {
      return {
        ...module,
        dataMode: "Automático"
      };
    }
    if (module.id === "sectores") {
      return {
        ...module,
        dataMode: "Modelo",
        source: "BCRD",
        sourceDetail: "PIB por actividad económica y drivers normalizados.",
        insight: "Peso relativo y señal dominante por sector."
      };
    }
    if (module.id === "contexto-externo") {
      return {
        ...module,
        dataMode: "Modelo",
        source: "BCRD",
        sourceDetail: "Balanza de servicios y series externas del Banco Central.",
        insight: "Señales externas para leer servicios, turismo y financiamiento."
      };
    }
    if (module.id === "comercio-exterior") {
      return {
        ...module,
        dataMode: "Automático",
        source: "BCRD, DGA",
        sourceDetail: "BCRD para flujos agregados y DGA para capítulos de exportación.",
        question: "¿Cómo se mueven las exportaciones e importaciones?",
        insight: "Exportaciones, importaciones y canasta exportadora en una sola vista.",
        methodology: [
          "Se descargan los Excel oficiales del CustomView de sector externo del BCRD.",
          "Los flujos anuales suman los trimestres disponibles de exportaciones e importaciones.",
          "La canasta exportadora usa capítulos publicados por la DGA cuando están disponibles."
        ]
      };
    }
    if (module.id === "mercado-laboral") {
      return {
        ...module,
        dataMode: "Automático",
        source: "BCRD ENCFT",
        sourceDetail: "Excel oficiales de la Encuesta Nacional Continua de Fuerza de Trabajo.",
        question: "¿Qué dicen los últimos indicadores laborales?",
        insight: "Tasas laborales recientes y ocupación por rama de actividad.",
        methodology: [
          "Se descargan los Excel oficiales de ENCFT desde el CustomView del BCRD.",
          "Los indicadores toman el último trimestre disponible.",
          "La distribución por rama usa ocupados por actividad económica."
        ]
      };
    }
    if (module.id === "costo-vida") {
      return {
        ...module,
        dataMode: "Automático",
        source: "BCRD",
        sourceDetail: "Excel oficiales de IPC general, subyacente e IPC por grupos.",
        question: "¿Dónde se concentra la presión reciente de precios?",
        insight: "Inflación general, subyacente y variación mensual por grupo.",
        methodology: [
          "Se descargan los Excel oficiales de precios del BCRD.",
          "La inflación general y subyacente usa variación interanual mensual.",
          "Los grupos se ordenan por la variación mensual absoluta más reciente."
        ]
      };
    }
    if (module.id === "territorio-infraestructura") {
      return {
        ...module,
        dataMode: "Corte fijo"
      };
    }
    if (module.id === "mipymes-productividad") {
      return {
        ...module,
        dataMode: "Artículo"
      };
    }
    if (module.id === "laboratorio-visual") {
      return {
        ...module,
        dataMode: "Artículo"
      };
    }
    return module;
  });
  return copy;
}

function normalize(value) {
  return String(value || "")
    .normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "")
    .trim()
    .toLowerCase();
}

function numberOrNull(value) {
  const number = Number(value);
  return Number.isFinite(number) ? Number(number.toFixed(4)) : null;
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

function publicSourceRun(run) {
  if (!run) {
    return {
      generatedAt: "",
      summary: {
        total: 0,
        downloaded: 0,
        cached: 0,
        verified: 0,
        manual: 0,
        failed: 0
      },
      sources: []
    };
  }

  return {
    generatedAt: run.generatedAt || "",
    manifestVersion: run.manifestVersion || null,
    summary: run.summary || {},
    sources: (run.sources || []).map((source) => ({
      id: source.id,
      label: source.label,
      mode: source.mode,
      kind: source.kind,
      status: source.status,
      required: source.required === true,
      bytes: source.bytes || null,
      discovered: source.discovered || null,
      selected: source.selected || null,
      downloaded: source.downloaded || null,
      modifiedAt: source.modifiedAt || "",
      md5: source.md5 || "",
      usedBy: Array.isArray(source.usedBy) ? source.usedBy : [],
      updatePolicy: source.updatePolicy || ""
    }))
  };
}

function publicLiveData(live) {
  if (!live) {
    return {
      generatedAt: "",
      dataCutoff: {},
      active: false
    };
  }
  return {
    generatedAt: live.generatedAt || "",
    dataCutoff: live.dataCutoff || {},
    active: true
  };
}

