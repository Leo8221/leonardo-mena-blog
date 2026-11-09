// Solo estas opciones viajan en el enlace. Los datos y mapas cargados se quedan
// en memoria y se reutilizan al pasar de una vista a otra.
const URL_STATE_DEFAULTS = Object.freeze({
  active: "overview",
  query: "",
  family: "all",
  macroMetric: "dolar",
  tradeMetric: "exports",
  laborMetric: "employment",
  territoryMapMetric: "business_density",
  territoryRegion: "all",
  visualMap: "business"
});

const state = {
  data: null,
  geojson: null,
  regionGeojson: null,
  worldGeojson: null,
  articleVisuals: null,
  assetPromises: {},
  assetFailures: {},
  tooltip: null,
  tooltipPinned: false,
  ...URL_STATE_DEFAULTS,
  mapPinned: {},
  booted: false,
  focusStage: false,
  focusControl: null,
  routeSearch: "",
  lastTrackedSearch: ""
};

const els = {
  app: document.querySelector(".atlas-app"),
  moduleSwitch: document.getElementById("module-switch"),
  viewNavigation: document.getElementById("view-navigation"),
  intro: document.getElementById("atlas-intro"),
  feature: document.getElementById("atlas-feature"),
  catalog: document.getElementById("atlas-catalog"),
  family: document.getElementById("atlas-family"),
  catalogReset: document.getElementById("catalog-reset"),
  snapshot: document.getElementById("atlas-snapshot"),
  search: document.getElementById("atlas-search"),
  searchClear: document.getElementById("atlas-search-clear"),
  resultCount: document.getElementById("atlas-result-count"),
  metricStrip: document.getElementById("metric-strip"),
  stage: document.getElementById("module-stage"),
  main: document.getElementById("atlas-main"),
  status: document.getElementById("atlas-status"),
  themeToggle: document.getElementById("atlas-theme-toggle")
};

const ATLAS_THEME_STORAGE_KEY = "quarto-color-scheme";
const atlasThemeMedia = window.matchMedia ? window.matchMedia("(prefers-color-scheme: dark)") : null;

function storedAtlasTheme() {
  // Quarto llama "alternate" al tema oscuro. Compartir su clave hace que el
  // blog y Atlas recuerden la misma elección.
  try {
    const stored = window.localStorage.getItem(ATLAS_THEME_STORAGE_KEY);
    if (stored === "alternate") return "dark";
    if (stored === "default") return "light";
  } catch (error) {
    return null;
  }
  return null;
}

function preferredAtlasTheme() {
  return storedAtlasTheme()
    || document.documentElement.dataset.theme
    || (atlasThemeMedia?.matches ? "dark" : "light");
}

function applyAtlasTheme(theme, { persist = false, rerender = false } = {}) {
  const dark = theme === "dark";
  document.documentElement.dataset.theme = dark ? "dark" : "light";
  document.body.classList.toggle("atlas-dark", dark);
  document.body.classList.toggle("atlas-light", !dark);

  if (els.themeToggle) {
    els.themeToggle.setAttribute("aria-pressed", String(dark));
    els.themeToggle.setAttribute("aria-label", dark ? "Cambiar a modo claro" : "Cambiar a modo oscuro");
    els.themeToggle.title = dark ? "Cambiar a modo claro" : "Cambiar a modo oscuro";
  }

  if (persist) {
    try {
      window.localStorage.setItem(ATLAS_THEME_STORAGE_KEY, dark ? "alternate" : "default");
    } catch (error) {
      // Si el navegador bloquea el almacenamiento, el tema cambia solo aquí.
    }
  }

  if (typeof syncChartSystemColors === "function") syncChartSystemColors();
  if (rerender && state.booted) renderStage();
}

function toggleAtlasTheme() {
  const nextTheme = document.body.classList.contains("atlas-dark") ? "light" : "dark";
  applyAtlasTheme(nextTheme, { persist: true, rerender: true });
  trackAtlasEvent("atlas_theme_toggle", { theme: nextTheme });
}

function initAtlasTheme() {
  applyAtlasTheme(preferredAtlasTheme());

  if (els.themeToggle) {
    els.themeToggle.addEventListener("click", toggleAtlasTheme);
  }

  window.addEventListener("storage", (event) => {
    if (event.key === ATLAS_THEME_STORAGE_KEY) {
      applyAtlasTheme(storedAtlasTheme() || "light", { rerender: true });
    }
  });

  if (atlasThemeMedia) {
    atlasThemeMedia.addEventListener("change", (event) => {
      if (storedAtlasTheme()) return;
      applyAtlasTheme(event.matches ? "dark" : "light", { rerender: true });
    });
  }
}

const OVERVIEW_GROUPS = [
  {
    title: "Coyuntura y precios",
    summary: "Coyuntura, precios y señales externas.",
    modules: ["pulso-macro", "contexto-externo", "costo-vida"]
  },
  {
    title: "Sectores y territorio",
    summary: "Comercio, capacidad productiva y mapas.",
    modules: ["comercio-exterior", "sectores", "territorio-infraestructura"]
  },
  {
    title: "Trabajo y empresas",
    summary: "Mercado laboral, MiPyMES y gráficos de artículos.",
    modules: ["mercado-laboral", "mipymes-productividad", "laboratorio-visual"]
  }
];

const MODULE_GUIDES = {
  "pulso-macro": {
    unit: "Series macro en %, RD$/US$ o indice.",
    high: "Depende del indicador activo.",
    low: "Puede ser alivio, desaceleracion o menor presion.",
    limit: "Sirve para ubicarse; no explica causas por si solo."
  },
  sectores: {
    unit: "Indice de presion, escala 0-100.",
    high: "Mas presion relativa en esta comparacion.",
    low: "Menor exposicion relativa dentro del grupo visible.",
    limit: "Es una lectura inicial, no una cuenta sectorial."
  },
  "contexto-externo": {
    unit: "Exportaciones de servicios, índice con máximo del periodo igual a 100.",
    high: "Mayores ingresos por exportaciones de servicios.",
    low: "Menores ingresos respecto al máximo del periodo.",
    limit: "No mide riesgo ni poder adquisitivo; usa montos nominales."
  },
  "comercio-exterior": {
    unit: "Participacion, balance e indice.",
    high: "Mayor peso relativo, segun metrica.",
    low: "Menor peso relativo dentro de los socios o rubros visibles.",
    limit: "No mide rentabilidad."
  },
  "mercado-laboral": {
    unit: "Tasas en %, informalidad e indice salarial relativo.",
    high: "Cambia de sentido segun la metrica.",
    low: "Puede indicar rezago o menor exposicion al problema medido.",
    limit: "Comparacion descriptiva."
  },
  "costo-vida": {
    unit: "Inflacion en % e indices de presion 0-100.",
    high: "Mayor presion en la serie visible.",
    low: "Menor contribucion relativa al episodio de precios.",
    limit: "No sustituye el IPC oficial."
  },
  "territorio-infraestructura": {
    unit: "Indices territoriales, densidad y puntajes relativos.",
    high: "Mayor valor en la metrica activa.",
    low: "Menor presencia relativa dentro del territorio comparado.",
    limit: "Ayuda a mirar; no decide por si solo."
  },
  "mipymes-productividad": {
    unit: "Indicadores relativos e indices 0-100.",
    high: "Mayor valor en la metrica activa.",
    low: "Menor avance o menor intensidad del problema medido.",
    limit: "Resume patrones por segmento."
  },
  "laboratorio-visual": {
    unit: "Mapas y graficos de articulos.",
    high: "Mayor intensidad del indicador.",
    low: "Menor presencia relativa en la capa visible.",
    limit: "La lectura completa esta en el articulo."
  }
};

async function boot() {
  // Primero el resumen ligero; los mapas se descargan al abrir su vista.
  try {
    const atlasResponse = await fetch("data/atlas-data.json", { cache: "default" });
    if (!atlasResponse.ok) throw new Error(`HTTP ${atlasResponse.status}`);
    state.data = await atlasResponse.json();
    state.tooltip = ensureTooltip();
  } catch (error) {
    renderLoadError(error);
    return;
  }

  // Canvas no vuelve a componer el texto cuando termina de cargar la tipografía.
  // Esperarla evita que el primer dibujo conserve letras de la fuente de reserva.
  if (document.fonts) await document.fonts.ready;
  bindEvents();
  applyStateFromUrl();
  syncFilterState();
  updateDocumentTitle();
  updateUrlState({ replace: true });
  state.booted = true;
  trackAtlasEvent("atlas_open", { referrer_section: document.referrer ? "external" : "direct" });
  render();
}

function bindEvents() {
  els.app.addEventListener("click", handleAtlasClick);
  els.search.addEventListener("input", (event) => {
    state.query = event.target.value.trim().toLowerCase();
    refreshAtlasFilters();
    trackSearchUsage();
  });

  els.searchClear.addEventListener("click", () => {
    state.query = "";
    els.search.value = "";
    refreshAtlasFilters();
    trackAtlasEvent("atlas_search", { query_length: 0 });
    els.search.focus();
  });

  els.family.addEventListener("change", () => {
    state.family = els.family.value;
    refreshAtlasFilters();
    trackAtlasEvent("atlas_filter_change", { filter: state.family });
  });
  els.moduleSwitch.addEventListener("change", () => {
    setActive(els.moduleSwitch.value, { source: "view-switcher" });
  });

  window.addEventListener("resize", debounce(() => {
    renderStage();
  }, 140));

  window.addEventListener("popstate", () => {
    // Ir a Datos o Fuentes solo cambia el ancla, no la vista ni sus controles.
    if (window.location.search === state.routeSearch) return;
    applyStateFromUrl();
    state.routeSearch = window.location.search;
    state.focusStage = true;
    render();
    updateDocumentTitle();
  });

  window.addEventListener("hashchange", () => {
    // Los enlaces internos (Datos, Fuentes) no son rutas antiguas de módulos.
    const id = window.location.hash.slice(1).split("?")[0];
    if (id !== "overview" && !findModule(id)) return;
    applyStateFromUrl();
    updateUrlState({ replace: true });
    render();
  });
}

function render() {
  syncFilterState();
  syncSearchState();
  renderMetrics();
  renderNavigation();
  renderStage();
}
