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
    ].filter(Boolean).join(" ").toLowerCase();
    const matchesQuery = !state.query || normalizeText(text).includes(normalizeText(state.query));
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
  if (!state.data) return;
  const url = buildAtlasUrl();
  const method = replace ? "replaceState" : "pushState";
  history[method](null, "", url);
  updateDocumentTitle();
}

function applyStateFromUrl() {
  const url = new URL(window.location.href);
  const legacyHash = url.hash ? url.hash.replace(/^#/, "").split("?")[0] : "";
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
  if (region) state.territoryRegion = region;
  const map = url.searchParams.get("map");
  if (map) state.visualMap = map;
}

function activeMetricForModule(module) {
  if (!module) return "";
  if (module.chart === "macro") return state.macroMetric;
  if (module.chart === "trade") return state.tradeMetric;
  if (module.chart === "labor") return state.laborMetric;
  if (module.chart === "territory") return state.territoryMapMetric;
  if (module.chart === "visualLab") return state.visualMap;
  return "";
}

function setActiveMetricForModule(module, metric) {
  if (!module || !metric) return;
  if (module.chart === "macro") state.macroMetric = metric;
  if (module.chart === "trade") state.tradeMetric = metric;
  if (module.chart === "labor") state.laborMetric = metric;
  if (module.chart === "territory") state.territoryMapMetric = metric;
  if (module.chart === "visualLab") state.visualMap = metric;
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

function bindResetFilterButtons(root) {
  root.querySelectorAll('[data-action="reset-filters"]').forEach((button) => {
    button.addEventListener("click", () => resetAtlasFilters({ focusSearch: true }));
  });
}

function resetAtlasFilters({ focusSearch = false } = {}) {
  state.query = "";
  state.family = "all";
  if (els.search) els.search.value = "";
  syncFilterState();
  syncSearchState();
  renderNavigation();
  renderMobileNavigation();
  renderStage();
  if (focusSearch && els.search) els.search.focus();
}

function hydrateModuleActions() {
  ensureCanvasDownloadActions();
  bindResetFilterButtons(els.stage);

  els.stage.querySelectorAll('[data-action="copy-link"]').forEach((button) => {
    button.addEventListener("click", async () => {
      const copied = await copyText(currentViewUrl());
      flashButton(button, copied ? "Copiado" : "Copiar");
    });
  });

  els.stage.querySelectorAll('[data-action="export-csv"]').forEach((button) => {
    button.addEventListener("click", () => {
      const module = findModule(state.active);
      if (!module) {
        flashButton(button, "Sin datos");
        return;
      }
      const datasets = moduleDatasets(module).filter((dataset) => dataset.rows.length > 0);
      if (!datasets.length) {
        flashButton(button, "Sin datos");
        return;
      }
      downloadDatasetsCsv(module, datasets);
      trackAtlasEvent("atlas_csv_download", { chart_id: "module", metric: activeMetricForModule(module) });
      flashButton(button, "Descargado");
    });
  });

  els.stage.querySelectorAll('[data-action="download-dataset"]').forEach((button) => {
    button.addEventListener("click", (event) => {
      event.preventDefault();
      event.stopPropagation();
      const module = findModule(state.active);
      const datasetId = button.dataset.dataset;
      const dataset = module ? moduleDatasets(module).find((item) => item.id === datasetId) : null;
      if (!module || !dataset) {
        flashButton(button, "Sin datos");
        return;
      }
      downloadDatasetsCsv(module, [dataset]);
      trackAtlasEvent("atlas_csv_download", { chart_id: dataset.id, metric: activeMetricForModule(module) });
      flashButton(button, "OK");
    });
  });

  els.stage.querySelectorAll('[data-action="expand-chart"]').forEach((button) => {
    button.addEventListener("click", () => openChartFullscreen(button.dataset.canvas));
  });

  els.stage.querySelectorAll('[data-action="download-chart"]').forEach((button) => {
    button.addEventListener("click", () => downloadCanvasPng(button.dataset.canvas, button));
  });

  els.stage.querySelectorAll(".article-link").forEach((link) => {
    link.addEventListener("click", () => {
      trackAtlasEvent("atlas_related_article_open", {
        chart_id: link.closest(".chart-card")?.querySelector("canvas")?.id || "module"
      });
    });
  });
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
