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
  active: "overview",
  query: "",
  family: "all",
  macroMetric: "dolar",
  tradeMetric: "exports",
  laborMetric: "employment",
  territoryMapMetric: "business_density",
  territoryRegion: "all",
  visualMap: "business",
  mapPinned: {},
  booted: false,
  lastTrackedSearch: ""
};

const els = {
  app: document.querySelector(".atlas-app"),
  nav: document.getElementById("module-nav"),
  search: document.getElementById("atlas-search"),
  searchClear: document.getElementById("atlas-search-clear"),
  resultCount: document.getElementById("atlas-result-count"),
  metricStrip: document.getElementById("metric-strip"),
  mobileNav: document.getElementById("mobile-module-nav"),
  stage: document.getElementById("module-stage"),
  menuToggle: document.getElementById("menu-toggle"),
  sidebar: document.getElementById("atlas-sidebar"),
  sidebarBackdrop: document.getElementById("sidebar-backdrop"),
  main: document.getElementById("atlas-main"),
  status: document.getElementById("atlas-status"),
  topbarShare: document.getElementById("atlas-share"),
  themeToggle: document.getElementById("atlas-theme-toggle")
};

let sidebarReturnFocus = null;
const ATLAS_THEME_STORAGE_KEY = "quarto-color-scheme";
const atlasThemeMedia = window.matchMedia ? window.matchMedia("(prefers-color-scheme: dark)") : null;

function storedAtlasTheme() {
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
      // Ignore storage failures; the visual state still changes for this page.
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
    title: "Ahora",
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
    unit: "Indice externo normalizado, escala 0-100.",
    high: "Mas presion externa en el indice.",
    low: "Menor presion externa agregada.",
    limit: "Resume senales; no pronostica choques."
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
  try {
    const atlasResponse = await fetch("data/atlas-data.json", { cache: "default" });
    if (!atlasResponse.ok) throw new Error(`HTTP ${atlasResponse.status}`);
    state.data = await atlasResponse.json();
    state.tooltip = ensureTooltip();
  } catch (error) {
    renderLoadError(error);
    return;
  }

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
  els.search.addEventListener("input", (event) => {
    state.query = event.target.value.trim().toLowerCase();
    syncSearchState();
    renderNavigation();
    renderMobileNavigation();
    renderOverviewIfActive();
    updateUrlState({ replace: true });
    trackSearchUsage();
  });

  els.searchClear.addEventListener("click", () => {
    state.query = "";
    els.search.value = "";
    syncSearchState();
    renderNavigation();
    renderMobileNavigation();
    renderOverviewIfActive();
    updateUrlState({ replace: true });
    trackAtlasEvent("atlas_search", { query_length: 0 });
    els.search.focus();
  });

  document.querySelectorAll(".filter-pill").forEach((button) => {
    button.addEventListener("click", () => {
      state.family = button.dataset.filter;
      syncFilterState();
      syncSearchState();
      renderNavigation();
      renderMobileNavigation();
      renderOverviewIfActive();
      updateUrlState({ replace: true });
      trackAtlasEvent("atlas_filter_change", { filter: state.family });
    });
  });

  els.menuToggle.addEventListener("click", () => {
    if (document.body.classList.contains("sidebar-open")) {
      closeSidebar({ restoreFocus: true });
    } else {
      openSidebar();
    }
  });

  document.addEventListener("keydown", (event) => {
    if (event.key === "Escape" && document.body.classList.contains("sidebar-open")) {
      closeSidebar({ restoreFocus: true });
    }
  });

  document.addEventListener("click", (event) => {
    if (!document.body.classList.contains("sidebar-open")) return;
    if (els.sidebar.contains(event.target) || els.menuToggle.contains(event.target)) return;
    closeSidebar({ restoreFocus: false });
  });

  if (els.sidebarBackdrop) {
    els.sidebarBackdrop.addEventListener("click", () => closeSidebar({ restoreFocus: true }));
  }

  if (els.topbarShare) {
    els.topbarShare.addEventListener("click", async () => {
      const copied = await copyText(currentViewUrl());
      flashButton(els.topbarShare, copied ? "Copiado" : "Copiar");
      trackAtlasEvent("atlas_copy_link", { referrer_section: "topbar" });
    });
  }

  window.addEventListener("resize", debounce(() => {
    renderStage();
  }, 140));

  window.addEventListener("popstate", () => {
    applyStateFromUrl();
    syncFilterState();
    syncSearchState();
    renderMetrics();
    renderNavigation();
    renderMobileNavigation();
    renderStage();
  });

  window.addEventListener("hashchange", () => {
    if (!window.location.hash) return;
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
  renderMobileNavigation();
  renderStage();
}
