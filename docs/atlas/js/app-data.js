const ASSET_URLS = {
  geojson: "data/rd-provinces.geojson",
  regionGeojson: "data/rd-regions-mipymes.geojson",
  worldGeojson: "data/world-tourism.geojson",
  articleVisuals: "data/article-visuals.json"
};

function requiredAssets(module) {
  if (!module) return [];
  if (module.chart === "territory") return ["geojson"];
  if (module.chart === "visualLab") {
    const mapAsset = {
      business: "geojson",
      mipymes: "regionGeojson",
      tourism: "worldGeojson"
    }[state.visualMap] || "geojson";
    return ["articleVisuals", mapAsset];
  }
  return [];
}

function moduleAssetsReady(module) {
  // Un fallo también cierra la espera: la vista muestra "sin datos" en lugar
  // de pedir una y otra vez el mismo archivo.
  return requiredAssets(module).every((key) => Boolean(state[key]) || Boolean(state.assetFailures[key]));
}

function ensureModuleAssets(module) {
  const keys = requiredAssets(module);
  if (!keys.length) return Promise.resolve();
  const activeId = module.id;
  return Promise.all(keys.map(loadAtlasAsset)).finally(() => {
    if (state.active === activeId) renderStage();
  });
}

// Reutiliza la petición al cambiar de vista; un fallo permite mostrar el estado sin datos.
function loadAtlasAsset(key) {
  if (state[key]) return Promise.resolve(state[key]);
  if (state.assetPromises[key]) return state.assetPromises[key];
  state.assetPromises[key] = fetch(ASSET_URLS[key], { cache: "default" })
    .then((response) => (response && response.ok ? response.json() : null))
    .then((data) => {
      if (data) {
        state[key] = data;
      } else {
        state.assetFailures[key] = true;
      }
      return data;
    })
    .catch(() => {
      state.assetFailures[key] = true;
      return null;
    });
  return state.assetPromises[key];
}

function deviceType() {
  if (window.matchMedia("(max-width: 680px)").matches) return "mobile";
  if (window.matchMedia("(max-width: 1024px)").matches) return "tablet";
  return "desktop";
}

function trackAtlasEvent(name, params = {}) {
  const module = findModule(state.active);
  const payload = {
    module_id: params.module_id || state.active,
    chart_id: params.chart_id || "",
    metric: params.metric || activeMetricForModule(module),
    filter: params.filter || state.family,
    query_length: Number.isFinite(params.query_length) ? params.query_length : state.query.length,
    device_type: deviceType(),
    referrer_section: params.referrer_section || "atlas"
  };
  if (typeof window.gtag === "function") {
    window.gtag("event", name, payload);
  }
  window.dispatchEvent(new CustomEvent("atlas:analytics", { detail: { name, payload } }));
}

function trackSearchUsage() {
  const key = `${state.query}|${state.family}`;
  if (key === state.lastTrackedSearch) return;
  state.lastTrackedSearch = key;
  trackAtlasEvent("atlas_search", { query_length: state.query.length });
  if (state.query && filteredModules().length === 0) {
    trackAtlasEvent("atlas_search_zero_results", { query_length: state.query.length });
  }
}

function visibleModules() {
  return state.data.modules.filter((module) => module.visible !== false && statusKey(module.status) === "activo");
}

function filteredModules() {
  const query = normalizeText(state.query);
  return visibleModules().filter((module) => {
    const matchesFamily = state.family === "all" || statusKey(module.family) === statusKey(state.family);
    const source = module.sourceInfo || {};
    const text = [
      module.title,
      module.question,
      module.summary,
      module.insight,
      module.topic,
      module.type,
      module.family,
      module.source,
      source.label,
      source.detail
    ].filter(Boolean).join(" ");
    const matchesQuery = !query || normalizeText(text).includes(query);
    return matchesFamily && matchesQuery;
  });
}

function territoryRows() {
  const rows = state.data.series.territory.provinces;
  if (state.territoryRegion === "all") return rows;
  return rows.filter((item) => item.region === state.territoryRegion);
}

function territoryMapFeatures() {
  if (!state.geojson || !Array.isArray(state.geojson.features)) return [];
  return state.geojson.features;
}

function territoryMapRows(valueField = state.territoryMapMetric) {
  return territoryMapFeatures()
    .map((feature) => feature.properties)
    .filter((item) => Number.isFinite(Number(item[valueField])))
    .sort((a, b) => Number(b[valueField]) - Number(a[valueField]));
}

function territoryMapMeta() {
  return {
    business_density: {
      valueField: "business_density",
      label: "densidad empresarial",
      unit: "empresas por 1,000 hab.",
      colorStart: "#edf4f2",
      colorEnd: "#c86448",
      caption: "Empresas registradas por 1,000 habitantes."
    },
    opportunity: {
      valueField: "opportunity",
      label: "oportunidad territorial",
      unit: "/100",
      colorStart: "#eef1ed",
      colorEnd: "#466a8f",
      caption: "Índice territorial normalizado."
    }
  }[state.territoryMapMetric];
}

function findModule(id) {
  return visibleModules().find((module) => module.id === id);
}

function setActive(moduleId, options = {}) {
  const next = moduleId === "overview" || findModule(moduleId) ? moduleId : "overview";
  const previous = state.active;
  state.active = next;
  updateUrlState({ replace: Boolean(options.replace) || next === previous });
  closeSidebar({ restoreFocus: false });
  syncMetricState();
  syncSearchState();
  renderNavigation();
  renderMobileNavigation();
  renderStage();
  if (next !== previous || state.booted) {
    trackAtlasEvent("atlas_module_open", {
      module_id: next,
      referrer_section: options.source || "navigation"
    });
  }
}

function buildAtlasUrl() {
  // Reconstruir los parámetros evita arrastrar filtros de una vista anterior.
  const url = new URL(window.location.href);
  url.hash = "";
  const params = new URLSearchParams();
  if (state.active !== "overview") params.set("view", state.active);
  if (state.family !== "all") params.set("filter", state.family);
  if (state.query) params.set("q", state.query);
  const module = findModule(state.active);
  const metric = activeMetricForModule(module);
  if (metric) params.set("metric", metric);
  if (module?.chart === "territory" && state.territoryRegion !== "all") params.set("region", state.territoryRegion);
  if (module?.chart === "visualLab") params.set("map", state.visualMap);
  url.search = params.toString();
  return url;
}

function updateUrlState({ replace = false } = {}) {
  // Al escribir o cambiar una métrica se reemplaza el enlace actual.
  // Al abrir otro módulo se añade una entrada para que funcione Atrás.
  if (!state.data) return;
  const url = buildAtlasUrl();
  const method = replace ? "replaceState" : "pushState";
  history[method](null, "", url);
  updateDocumentTitle();
}

function applyStateFromUrl() {
  const url = new URL(window.location.href);
  const legacyHash = url.hash ? url.hash.replace(/^#/, "").split("?")[0] : "";
  // Empezar por los valores iniciales también borra filtros ausentes del enlace.
  Object.assign(state, URL_STATE_DEFAULTS);
  if (els.search) els.search.value = "";
  const view = url.searchParams.get("view") || legacyHash;
  if (view && (view === "overview" || findModule(view))) state.active = view;
  const filter = url.searchParams.get("filter");
  if (filter) state.family = filter;
  const query = url.searchParams.get("q");
  if (query !== null) {
    state.query = query.trim().toLowerCase();
    if (els.search) els.search.value = query;
  }
  const module = findModule(state.active);
  const metric = url.searchParams.get("metric");
  if (metric) setActiveMetricForModule(module, metric);
  const region = url.searchParams.get("region");
  if (state.data.series.territory.provinces.some((item) => item.region === region)) {
    state.territoryRegion = region;
  }
  const map = url.searchParams.get("map");
  if (["business", "mipymes", "tourism"].includes(map)) state.visualMap = map;
}

// Relaciona el tipo de gráfico con su opción guardada y los valores aceptados.
const MODULE_METRICS = {
  macro: { property: "macroMetric", values: ["dolar", "inflacion", "imae", "tpm"] },
  trade: { property: "tradeMetric", values: ["exports", "imports"] },
  labor: { property: "laborMetric", values: ["employment", "informality", "wageIndex"] },
  territory: { property: "territoryMapMetric", values: ["business_density", "opportunity"] },
  visualLab: { property: "visualMap", values: ["business", "mipymes", "tourism"] }
};

function activeMetricForModule(module) {
  const options = MODULE_METRICS[module?.chart];
  return options ? state[options.property] : "";
}

function setActiveMetricForModule(module, metric) {
  const options = MODULE_METRICS[module?.chart];
  if (options?.values.includes(metric)) state[options.property] = metric;
}

function updateDocumentTitle() {
  const module = findModule(state.active);
  const metric = activeMetricForModule(module);
  const metricLabel = metric ? metric.replaceAll("_", " ") : "";
  document.title = module
    ? `${metricLabel ? `${metricLabel} | ` : ""}${module.title} | Atlas`
    : "Atlas";
}

function announceModule(module) {
  if (!els.status) return;
  els.status.textContent = module ? `${module.title} cargado.` : "Atlas cargado.";
}

function resetAtlasFilters({ focusSearch = false } = {}) {
  state.query = "";
  state.family = "all";
  if (els.search) els.search.value = "";
  refreshAtlasFilters({ redrawStage: true });
  if (focusSearch && els.search) els.search.focus();
}

// Buscar cambia los menús y la portada. La vista que estás leyendo se mantiene;
// al restablecer todo sí se redibuja, con sus filtros ya limpios.
function refreshAtlasFilters({ redrawStage = false } = {}) {
  syncFilterState();
  syncSearchState();
  renderNavigation();
  renderMobileNavigation();
  if (redrawStage || state.active === "overview") renderStage();
  updateUrlState({ replace: true });
}

// Un solo listener sobre el contenedor: las vistas reemplazan su HTML al navegar.
async function handleAtlasClick(event) {
  const button = event.target.closest("[data-module], [data-action], .chart-toggle, .article-link");
  if (!button) return;

  if (button.dataset.module) {
    const source = button.matches(".metric-card") ? "metric-strip"
      : button.matches(".module-button") ? "sidebar"
      : button.matches(".mobile-module-card") ? "mobile-nav" : "overview";
    setActive(button.dataset.module, { source });
    return;
  }
  if (button.matches(".chart-toggle")) {
    const { scope, metric } = button.dataset;
    const property = CHART_CONTROLS[scope];
    if (!property) return;
    state[property] = metric;
    updateUrlState({ replace: true });
    trackAtlasEvent(["visual", "territoryMap"].includes(scope) ? "atlas_map_selection" : "atlas_chart_toggle", {
      chart_id: scope, metric
    });
    renderStage();
    return;
  }
  if (button.matches(".article-link")) {
    trackAtlasEvent("atlas_related_article_open", {
      chart_id: button.closest(".chart-card")?.querySelector("canvas")?.id || "module"
    });
    return;
  }

  switch (button.dataset.action) {
    case "reset-filters":
      resetAtlasFilters({ focusSearch: true });
      break;
    case "copy-link":
      flashButton(button, await copyText(currentViewUrl()) ? "Copiado" : "Copiar");
      break;
    case "expand-chart":
      openChartFullscreen(button.dataset.canvas);
      break;
    case "download-chart":
      downloadCanvasPng(button.dataset.canvas, button);
      break;
    case "export-csv":
    case "download-dataset": {
      event.preventDefault();
      const module = findModule(state.active);
      const datasetId = button.dataset.dataset;
      const datasets = module ? moduleDatasets(module).filter(item =>
        item.rows.length && (!datasetId || item.id === datasetId)) : [];
      if (!datasets.length) { flashButton(button, "Sin datos"); return; }
      downloadDatasetsCsv(module, datasets);
      trackAtlasEvent("atlas_csv_download", { chart_id: datasetId || "module", metric: activeMetricForModule(module) });
      flashButton(button, datasetId ? "OK" : "Descargado");
      break;
    }
  }
}

function ensureCanvasDownloadActions() {
  els.stage.querySelectorAll(".chart-card canvas[id]").forEach((canvas) => {
    const card = canvas.closest(".chart-card");
    if (!card || card.querySelector(`[data-action="download-chart"][data-canvas="${canvas.id}"]`)) return;
    const head = card.querySelector(".card-head");
    let actions = head ? head.querySelector(".chart-actions") : null;
    if (!actions) {
      actions = document.createElement("div");
      actions.className = head ? "chart-actions" : "chart-actions chart-actions-inline";
      if (head) {
        head.appendChild(actions);
      } else {
        canvas.before(actions);
      }
    }
    actions.insertAdjacentHTML("beforeend", chartDownloadButton(canvas.id));
  });
}

function downloadDatasetsCsv(module, datasets) {
  const csv = datasetsToCsv(module, datasets);
  const suffix = datasets.length === 1 ? datasets[0].id : "datos";
  const generated = String(state.data.generatedAt || state.data.updated || "").slice(0, 10) || "sin-fecha";
  downloadTextFile(csv, `atlas-${module.id}-${suffix}-${generated}.csv`, "text/csv;charset=utf-8");
}

// Une las columnas de las tablas e incluye fuente y corte en cada fila del CSV.
function datasetsToCsv(module, datasets) {
  const info = module.sourceInfo || {};
  const metaColumns = [
    "module_id",
    "module_title",
    "dataset",
    "source",
    "corte",
    "generated_at"
  ];
  const fields = new Set();
  datasets.forEach((dataset) => {
    datasetColumns(dataset).forEach((column) => fields.add(column.field));
  });
  const dataFields = [...fields];
  const rows = [metaColumns.concat(dataFields).map(csvEscape).join(",")];

  datasets.forEach((dataset) => {
    dataset.rows.forEach((row) => {
      const values = [
        module.id,
        module.title,
        dataset.title,
        info.label || module.source || "",
        info.updated || "",
        state.data.generatedAt || ""
      ].concat(dataFields.map((field) => row[field]));
      rows.push(values.map(csvEscape).join(","));
    });
  });

  return rows.join("\r\n");
}

function csvEscape(value) {
  // En CSV, una coma o un salto de línea obliga a entrecomillar el campo.
  // Las comillas del texto se representan dos veces: "".
  if (value === null || value === undefined) return "";
  const text = Array.isArray(value) ? value.join("; ") : String(value);
  return /[",\r\n]/.test(text) ? `"${text.replaceAll('"', '""')}"` : text;
}

function downloadTextFile(content, filename, type) {
  const blob = new Blob([content], { type });
  const url = URL.createObjectURL(blob);
  const link = document.createElement("a");
  link.href = url;
  link.download = filename;
  document.body.appendChild(link);
  link.click();
  link.remove();
  window.setTimeout(() => URL.revokeObjectURL(url), 0);
}
