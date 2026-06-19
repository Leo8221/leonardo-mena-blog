const state = {
  data: null,
  geojson: null,
  regionGeojson: null,
  worldGeojson: null,
  articleVisuals: null,
  tooltip: null,
  tooltipPinned: false,
  active: "pulso-macro",
  query: "",
  family: "all",
  macroMetric: "dolar",
  tradeMetric: "exports",
  laborMetric: "employment",
  territoryMapMetric: "business_density",
  territoryRegion: "all",
  visualMap: "business",
  mapPinned: {}
};

const els = {
  nav: document.getElementById("module-nav"),
  search: document.getElementById("atlas-search"),
  metricStrip: document.getElementById("metric-strip"),
  mobileNav: document.getElementById("mobile-module-nav"),
  stage: document.getElementById("module-stage"),
  menuToggle: document.getElementById("menu-toggle")
};

const ARTICLE_ROUTES = {
  "2026-01-06-Perpectiva_del_desarrollo": "../posts/republica-en-un-grafico/2026-01-06-Perpectiva_del_desarrollo/index.html",
  "2026-02-14-mipymes-rd": "../posts/republica-en-un-grafico/2026-02-14-mipymes-rd/index.html",
  "2026-01-20-Turismo_expansion": "../posts/republica-habla-de/2026-01-20-Turismo_expansion/index.html",
  "2026-03-04-transporte-masivo": "../posts/republica-habla-de/2026-03-04-transporte-masivo/index.html",
  "2025-12-19_deuda_publica": "../posts/republica-habla-de/2025-12-19_deuda_publica/index.html"
};

const VISUAL_ARTICLES = {
  business: "2026-01-06-Perpectiva_del_desarrollo",
  mipymes: "2026-02-14-mipymes-rd",
  tourism: "2026-01-20-Turismo_expansion",
  transport: "2026-03-04-transporte-masivo",
  debt: "2025-12-19_deuda_publica"
};

async function boot() {
  try {
    const [atlasResponse, mapResponse, regionMapResponse, worldMapResponse, articleResponse] = await Promise.all([
      fetch("data/atlas-data.json", { cache: "no-store" }),
      fetch("data/rd-provinces.geojson", { cache: "no-store" }).catch(() => null),
      fetch("data/rd-regions-mipymes.geojson", { cache: "no-store" }).catch(() => null),
      fetch("data/world-tourism.geojson", { cache: "no-store" }).catch(() => null),
      fetch("data/article-visuals.json", { cache: "no-store" }).catch(() => null)
    ]);
    if (!atlasResponse.ok) throw new Error(`HTTP ${atlasResponse.status}`);
    state.data = await atlasResponse.json();
    state.geojson = mapResponse && mapResponse.ok ? await mapResponse.json() : null;
    state.regionGeojson = regionMapResponse && regionMapResponse.ok ? await regionMapResponse.json() : null;
    state.worldGeojson = worldMapResponse && worldMapResponse.ok ? await worldMapResponse.json() : null;
    state.articleVisuals = articleResponse && articleResponse.ok ? await articleResponse.json() : null;
    state.tooltip = ensureTooltip();
  } catch (error) {
    renderLoadError(error);
    return;
  }

  const hashId = window.location.hash.replace("#", "");
  if (visibleModules().some((module) => module.id === hashId)) {
    state.active = hashId;
  }

  bindEvents();
  render();
}

function bindEvents() {
  els.search.addEventListener("input", (event) => {
    state.query = event.target.value.trim().toLowerCase();
    renderNavigation();
    renderOverviewIfActive();
  });

  document.querySelectorAll(".filter-pill").forEach((button) => {
    button.addEventListener("click", () => {
      state.family = button.dataset.filter;
      document.querySelectorAll(".filter-pill").forEach((item) => {
        item.classList.toggle("is-active", item.dataset.filter === state.family);
      });
      renderNavigation();
      renderMobileNavigation();
      renderOverviewIfActive();
    });
  });

  els.menuToggle.addEventListener("click", () => {
    const isOpen = document.body.classList.toggle("sidebar-open");
    els.menuToggle.setAttribute("aria-expanded", String(isOpen));
  });

  window.addEventListener("resize", debounce(() => {
    renderStage();
  }, 140));

  window.addEventListener("hashchange", () => {
    const route = window.location.hash.replace("#", "") || "overview";
    if (route !== state.active) {
      setActive(route);
    }
  });
}

function render() {
  renderMetrics();
  renderNavigation();
  renderMobileNavigation();
  renderStage();
}

function renderMetrics() {
  els.metricStrip.innerHTML = state.data.metrics.map((metric) => `
    <button class="metric-card" type="button" data-module="${metric.module}" data-tone="${metric.tone}">
      <small>${escapeHtml(metric.label)}</small>
      <strong>${escapeHtml(metric.value)}</strong>
      <span>${escapeHtml(metric.delta)}</span>
      <em>${escapeHtml(metric.meta || "")}</em>
    </button>
  `).join("");

  els.metricStrip.querySelectorAll(".metric-card").forEach((card) => {
    card.addEventListener("click", () => setActive(card.dataset.module));
  });
}

function renderNavigation() {
  const modules = filteredModules();
  const buttons = [
    `<button class="module-button ${state.active === "overview" ? "is-active" : ""}" type="button" data-module="overview">
      <small>Atlas</small>
      <strong>Portada</strong>
      <span>${visibleModules().length} vistas</span>
    </button>`,
    ...modules.map((module) => `
      <button class="module-button ${state.active === module.id ? "is-active" : ""}" type="button" data-module="${module.id}">
        <small>${escapeHtml(module.family)}</small>
        <strong>${escapeHtml(module.title)}</strong>
        <span>${escapeHtml(module.topic)}</span>
      </button>
    `)
  ];

  els.nav.innerHTML = buttons.join("");
  els.nav.querySelectorAll(".module-button").forEach((button) => {
    button.addEventListener("click", () => setActive(button.dataset.module));
  });
}

function renderMobileNavigation() {
  if (!els.mobileNav) return;
  const modules = filteredModules();
  const buttons = [
    `<button class="mobile-module-card ${state.active === "overview" ? "is-active" : ""}" type="button" data-module="overview">
      <small>Atlas</small>
      <strong>Portada</strong>
    </button>`,
    ...modules.map((module) => `
      <button class="mobile-module-card ${state.active === module.id ? "is-active" : ""}" type="button" data-module="${module.id}">
        <small>${escapeHtml(module.family)}</small>
        <strong>${escapeHtml(module.title)}</strong>
      </button>
    `)
  ];

  els.mobileNav.innerHTML = buttons.join("");
  els.mobileNav.querySelectorAll(".mobile-module-card").forEach((button) => {
    button.addEventListener("click", () => setActive(button.dataset.module));
  });
  const activeButton = els.mobileNav.querySelector(".mobile-module-card.is-active");
  if (activeButton && window.matchMedia("(max-width: 640px)").matches) {
    window.requestAnimationFrame(() => {
      activeButton.scrollIntoView({ block: "nearest", inline: "start" });
    });
  }
}

function renderOverviewIfActive() {
  if (state.active === "overview") {
    renderStage();
  }
}

function renderStage() {
  if (!state.data) return;
  hideTooltip(true);

  if (state.active === "overview") {
    renderOverview();
    return;
  }

  const module = findModule(state.active);
  if (!module) {
    state.active = "overview";
    renderOverview();
    return;
  }

  els.stage.innerHTML = `
    <div class="stage-header">
      <div>
        <p class="eyebrow">${escapeHtml(module.family)} / ${escapeHtml(module.topic)}</p>
        <h2>${escapeHtml(module.title)}</h2>
      </div>
      ${renderStageActions(true)}
    </div>
    ${renderModuleBody(module)}
  `;

  hydrateModuleActions();
  hydrateCharts(module);
}

function renderOverview() {
  const modules = filteredModules();
  els.stage.innerHTML = `
    <div class="stage-header">
      <div>
        <p class="eyebrow">${escapeHtml(state.data.brand.shortName)}</p>
        <h2>Atlas</h2>
      </div>
      ${renderStageActions(false)}
    </div>
    <div class="module-grid">
      ${modules.map((module) => `
        <button class="module-card" type="button" data-module="${module.id}">
          <h3>${escapeHtml(module.title)}</h3>
          <footer>
            <span>${escapeHtml(module.family)}</span>
            <span>${escapeHtml(module.type)}</span>
          </footer>
        </button>
      `).join("")}
    </div>
  `;

  els.stage.querySelectorAll(".module-card").forEach((button) => {
    button.addEventListener("click", () => setActive(button.dataset.module));
  });
  hydrateModuleActions();
}

function renderStageActions(canExport) {
  return `
    <div class="stage-actions">
      <button class="stage-action" type="button" data-action="copy-link" title="Copiar enlace de esta vista">Enlace</button>
      ${canExport ? `<button class="stage-action" type="button" data-action="export-chart" title="Descargar grafico visible">PNG</button>` : ""}
    </div>
  `;
}

function renderModuleBody(module) {
  const renderer = {
    macro: renderMacro,
    external: renderExternal,
    sectors: renderSectors,
    trade: renderTrade,
    labor: renderLabor,
    prices: renderPrices,
    territory: renderTerritory,
    mipymes: renderMipymes,
    visualLab: renderVisualLab
  }[module.chart];

  const body = renderer ? renderer(module) : renderUnsupported(module);
  return `
    <div class="chart-layout">
      ${body}
    </div>
  `;
}

function renderMacro() {
  return `
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Trayectoria macro reciente</h3>
        </div>
        <div class="chart-toolbar">
          ${chartToggle("macro", "dolar", "Dolar")}
          ${chartToggle("macro", "inflacion", "Inflacion")}
          ${chartToggle("macro", "imae", "IMAE")}
          ${chartToggle("macro", "tpm", "TPM")}
        </div>
      </div>
      <canvas id="macro-chart" height="320" aria-label="Grafico de linea macro"></canvas>
    </section>
  `;
}

function renderExternal() {
  return `
    <section class="chart-card chart-card-wide">
      <h3>Indice de presion externa</h3>
      <canvas id="external-chart" height="320" aria-label="Grafico de presion externa"></canvas>
    </section>
    <section class="chart-card">
      <h3>Drivers comparables</h3>
      <div class="driver-list">${renderBarRows(state.data.series.drivers, {
        labelField: "driver",
        valueField: "value",
        max: 100,
        suffix: "/100"
      })}</div>
    </section>
  `;
}

function renderSectors() {
  return `
    <section class="chart-card">
      <h3>Presion por sector</h3>
      <div class="sector-list">${renderBarRows(state.data.series.sectors, {
        labelField: "sector",
        valueField: "pressure",
        max: 100,
        suffix: "/100"
      })}</div>
    </section>
    <section class="chart-card">
      <h3>Driver principal</h3>
      <canvas id="sector-driver-chart" height="320" aria-label="Grafico de drivers sectoriales"></canvas>
    </section>
  `;
}

function renderTrade() {
  return `
    <section class="chart-card chart-card-wide">
      <h3>Espacio de oportunidad comercial</h3>
      <canvas id="trade-space-chart" height="420" aria-label="Espacio de oportunidad comercial"></canvas>
    </section>
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Socios comerciales</h3>
        </div>
        <div class="chart-toolbar">
          ${chartToggle("trade", "exports", "Exporta")}
          ${chartToggle("trade", "imports", "Importa")}
          ${chartToggle("trade", "opportunity", "Oportunidad")}
        </div>
      </div>
      <canvas id="trade-chart" height="340" aria-label="Grafico de socios comerciales"></canvas>
    </section>
    <section class="chart-card">
      <h3>Canasta exportadora</h3>
      <div class="table-list">
        ${state.data.series.trade.products.map((item) => `
          <div class="table-row">
            <strong>${escapeHtml(item.name)}</strong>
            <span>${formatNumber(item.share)}% share</span>
            <span>Complejidad ${formatNumber(item.complexity)}</span>
            <em>${escapeHtml(item.signal)}</em>
          </div>
        `).join("")}
      </div>
    </section>
  `;
}

function renderLabor() {
  return `
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Insercion por educacion</h3>
        </div>
        <div class="chart-toolbar">
          ${chartToggle("labor", "employment", "Empleo")}
          ${chartToggle("labor", "informality", "Informalidad")}
          ${chartToggle("labor", "wageIndex", "Salario")}
        </div>
      </div>
      <canvas id="labor-chart" height="340" aria-label="Grafico laboral por educacion"></canvas>
    </section>
    <section class="chart-card">
      <h3>Empleo por sector</h3>
      <div class="driver-list">${renderBarRows(state.data.series.labor.sectors, {
        labelField: "name",
        valueField: "jobs",
        max: 40,
        suffix: "%"
      })}</div>
    </section>
  `;
}

function renderPrices() {
  return `
    <section class="chart-card chart-card-wide">
      <h3>Inflacion general y subyacente</h3>
      <canvas id="prices-chart" height="320" aria-label="Grafico de inflacion general y subyacente"></canvas>
    </section>
    <section class="chart-card">
      <h3>Contribucion por rubro</h3>
      <div class="driver-list">${renderBarRows(state.data.series.prices.components, {
        labelField: "component",
        valueField: "pressure",
        max: 100,
        suffix: "/100"
      })}</div>
    </section>
    <section class="chart-card">
      <h3>Canales de segunda ronda</h3>
      <div class="table-list">
        ${state.data.series.prices.passThrough.map((item) => `
          <div class="table-row">
            <strong>${escapeHtml(item.channel)}</strong>
            <span>${formatNumber(item.value)}/100</span>
            <em>${escapeHtml(item.note)}</em>
          </div>
        `).join("")}
      </div>
    </section>
  `;
}

function renderTerritory() {
  const regions = state.data.series.territory.regions;
  return `
    <section class="chart-card chart-card-wide map-card">
      <div class="card-head">
        <div>
          <h3>Mapa territorial</h3>
        </div>
        <div class="chart-toolbar">
          ${chartToggle("territoryMap", "business_density", "Densidad")}
          ${chartToggle("territoryMap", "opportunity", "Oportunidad")}
        </div>
      </div>
      <div class="map-workbench">
        <div class="map-canvas-wrap">
          <canvas id="territory-map" height="620" aria-label="Mapa territorial"></canvas>
        </div>
        <aside class="map-inspector" id="territory-map-inspector" aria-label="Detalle del mapa"></aside>
      </div>
    </section>
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Oportunidad territorial</h3>
        </div>
        <div class="chart-toolbar">
          ${regions.map((region) => {
            const id = region === "Todas" ? "all" : region;
            return chartToggle("territory", id, region);
          }).join("")}
        </div>
      </div>
      <canvas id="territory-chart" height="340" aria-label="Grafico territorial"></canvas>
    </section>
  `;
}

function renderMipymes() {
  return `
    <section class="chart-card chart-card-wide">
      <h3>Acceso, formalidad y productividad</h3>
      <canvas id="mipyme-chart" height="340" aria-label="Grafico de MiPyMES"></canvas>
    </section>
    <section class="chart-card">
      <h3>Barreras principales</h3>
      <div class="driver-list">${renderBarRows(state.data.series.mipymes.barriers, {
        labelField: "barrier",
        valueField: "value",
        max: 100,
        suffix: "/100"
      })}</div>
    </section>
    <section class="chart-card">
      <h3>Escalera productiva</h3>
      <div class="ladder-list">
        ${state.data.series.mipymes.ladder.map((item) => `
          <div class="ladder-step">
            <small>${formatNumber(item.score)}/100</small>
            <strong>${escapeHtml(item.stage)}</strong>
            <span>${escapeHtml(item.focus)}</span>
          </div>
        `).join("")}
      </div>
    </section>
  `;
}

function renderVisualLab() {
  if (!state.articleVisuals) {
    return "";
  }

  const visualMapActions = `${articleLink(VISUAL_ARTICLES[state.visualMap])}${chartExpandButton("visual-map")}`;
  const visualMapToolbar = [
    chartToggle("visual", "business", "Empresas"),
    chartToggle("visual", "mipymes", "MiPyMES"),
    chartToggle("visual", "tourism", "Turismo")
  ].join("");

  return `
    <section class="chart-card chart-card-wide map-card">
      <div class="card-head">
        <div>
          <h3>Mapas desde articulos</h3>
        </div>
        ${chartControls(visualMapToolbar, visualMapActions)}
      </div>
      <div class="map-workbench">
        <div class="map-canvas-wrap">
          <canvas id="visual-map" height="620" aria-label="Mapa interactivo desde articulos"></canvas>
        </div>
        <aside class="map-inspector" id="visual-map-inspector" aria-label="Detalle del mapa"></aside>
      </div>
    </section>
    <section class="chart-card">
      <div class="card-head">
        <div>
          <h3>Demanda turistica por motivo</h3>
        </div>
        ${chartControls("", `${articleLink(VISUAL_ARTICLES.tourism)}${chartExpandButton("tourism-treemap")}`)}
      </div>
      <canvas id="tourism-treemap" height="340" aria-label="Treemap de motivos turisticos"></canvas>
    </section>
    <section class="chart-card">
      <div class="card-head">
        <div>
          <h3>Empleo formal y alquiler</h3>
        </div>
        ${chartControls("", `${articleLink(VISUAL_ARTICLES.transport)}${chartExpandButton("transport-space")}`)}
      </div>
      <canvas id="transport-space" height="340" aria-label="Scatter de empleo formal y alquiler"></canvas>
    </section>
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Deuda publica</h3>
        </div>
        ${chartControls("", `${articleLink(VISUAL_ARTICLES.debt)}${chartExpandButton("debt-burden")}`)}
      </div>
      <canvas id="debt-burden" height="360" aria-label="Rigidez fiscal e intereses"></canvas>
    </section>
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Servicio de deuda</h3>
        </div>
        ${chartControls("", `${articleLink(VISUAL_ARTICLES.debt)}${chartExpandButton("debt-service")}`)}
      </div>
      <canvas id="debt-service" height="360" aria-label="Servicio de deuda por componente"></canvas>
    </section>
  `;
}

function renderUnsupported(module) {
  return `
    <section class="chart-card">
      <h3>Modulo no disponible</h3>
      <p>No hay renderer configurado para ${escapeHtml(module.chart)}.</p>
    </section>
  `;
}

function chartToggle(scope, id, label) {
  const active = {
    macro: state.macroMetric,
    trade: state.tradeMetric,
    labor: state.laborMetric,
    territoryMap: state.territoryMapMetric,
    territory: state.territoryRegion,
    visual: state.visualMap
  }[scope];
  return `<button class="chart-toggle ${active === id ? "is-active" : ""}" type="button" data-scope="${scope}" data-metric="${id}">${escapeHtml(label)}</button>`;
}

function chartControls(toolbarHtml = "", actionsHtml = "") {
  if (!toolbarHtml && !actionsHtml) return "";
  return `
    <div class="chart-controls">
      ${toolbarHtml ? `<div class="chart-toolbar">${toolbarHtml}</div>` : ""}
      ${actionsHtml ? `<div class="chart-actions">${actionsHtml}</div>` : ""}
    </div>
  `;
}

function articleLink(articleId, label = "Leer analisis") {
  const href = ARTICLE_ROUTES[articleId];
  if (!href) return "";
  return `<a class="article-link" href="${escapeHtml(href)}">${escapeHtml(label)}</a>`;
}

function chartExpandButton(canvasId) {
  return `<button class="chart-expand" type="button" data-action="expand-chart" data-canvas="${escapeHtml(canvasId)}" title="Ver a pantalla completa">Ampliar</button>`;
}

function hydrateCharts(module) {
  els.stage.querySelectorAll(".chart-toggle").forEach((button) => {
    button.addEventListener("click", () => {
      const scope = button.dataset.scope;
      const metric = button.dataset.metric;
      if (scope === "macro") state.macroMetric = metric;
      if (scope === "trade") state.tradeMetric = metric;
      if (scope === "labor") state.laborMetric = metric;
      if (scope === "territoryMap") state.territoryMapMetric = metric;
      if (scope === "territory") state.territoryRegion = metric;
      if (scope === "visual") state.visualMap = metric;
      renderStage();
    });
  });

  if (module.chart === "macro") {
    const labels = state.data.series.macro.map((item) => item.period);
    const values = state.data.series.macro.map((item) => item[state.macroMetric]);
    const titles = {
      dolar: "RD$ por US$",
      inflacion: "Inflacion interanual (%)",
      imae: "IMAE interanual (%)",
      tpm: "TPM (%)"
    };
    drawLineChart(document.getElementById("macro-chart"), labels, values, titles[state.macroMetric], {
      stepped: state.macroMetric === "tpm"
    });
  }

  if (module.chart === "external") {
    drawLineChart(
      document.getElementById("external-chart"),
      state.data.series.external.map((item) => item.period),
      state.data.series.external.map((item) => item.pressure),
      "Presion externa"
    );
  }

  if (module.chart === "sectors") {
    drawCategoricalCountChart(
      document.getElementById("sector-driver-chart"),
      state.data.series.sectors,
      "driver",
      "Sectores por driver principal"
    );
  }

  if (module.chart === "trade") {
    drawComplexScatterChart(document.getElementById("trade-space-chart"), state.data.series.trade.partners, {
      title: "Dependencia importadora vs potencial exportador",
      xField: "imports",
      yField: "exports",
      sizeField: "opportunity",
      labelField: "name",
      categoryField: "balance",
      xLabel: "Dependencia importadora",
      yLabel: "Potencial exportador",
      xReference: 8,
      yReference: 10
    });
    drawHorizontalBarChart(document.getElementById("trade-chart"), state.data.series.trade.partners, {
      labelField: "name",
      valueField: state.tradeMetric,
      title: "Socios comerciales",
      max: state.tradeMetric === "opportunity" ? 100 : null,
      signed: state.tradeMetric === "balance"
    });
  }

  if (module.chart === "labor") {
    drawHorizontalBarChart(document.getElementById("labor-chart"), state.data.series.labor.outcomes, {
      labelField: "group",
      valueField: state.laborMetric,
      title: "Resultados laborales",
      max: state.laborMetric === "wageIndex" ? 170 : 100
    });
  }

  if (module.chart === "prices") {
    drawDualLineChart(
      document.getElementById("prices-chart"),
      state.data.series.prices.timeline.map((item) => item.period),
      [
        { label: "General", values: state.data.series.prices.timeline.map((item) => item.headline), color: "#c86448" },
        { label: "Subyacente", values: state.data.series.prices.timeline.map((item) => item.core), color: "#466a8f" }
      ],
      "Inflacion (%)"
    );
  }

  if (module.chart === "territory") {
    const mapMeta = territoryMapMeta();
    drawChoroplethMap(document.getElementById("territory-map"), territoryMapFeatures(), {
      ...mapMeta,
      title: `Republica Dominicana: ${mapMeta.label}`,
      labelField: "province",
      fallbackLabel: "Sin dato",
      inspectorId: "territory-map-inspector",
      mapId: "territory",
      tooltipRows: [
        { field: "businesses", label: "Empresas" },
        { field: "population", label: "Poblacion" }
      ]
    });
    drawScatterChart(document.getElementById("territory-chart"), territoryRows(), {
      title: "Infraestructura vs mercado",
      xField: "infrastructure",
      yField: "market",
      sizeField: "opportunity",
      labelField: "province"
    });
  }

  if (module.chart === "mipymes") {
    drawGroupedBarChart(document.getElementById("mipyme-chart"), state.data.series.mipymes.finance, {
      labelField: "segment",
      fields: [
        { field: "access", label: "Acceso", color: "#466a8f" },
        { field: "formalization", label: "Formalidad", color: "#2a9d8f" },
        { field: "productivity", label: "Productividad", color: "#c86448" }
      ],
      title: "Indicadores por segmento"
    });
  }

  if (module.chart === "visualLab" && state.articleVisuals) {
    hydrateVisualLab();
  }
}

function hydrateVisualLab() {
  const mapConfig = visualMapConfig();

  drawChoroplethMap(document.getElementById("visual-map"), mapConfig.features, mapConfig);

  drawTreemapChart(document.getElementById("tourism-treemap"), state.articleVisuals.tourism.treemap, {
    title: "Estructura de motivaciones turisticas",
    labelField: "motivo",
    valueField: "porcentaje",
    categoryField: "categoria"
  });

  drawComplexScatterChart(document.getElementById("transport-space"), state.articleVisuals.transport.rentEmployment, transportScatterOptions());

  drawDebtBurdenChart(document.getElementById("debt-burden"), state.articleVisuals.debt.service, {
    title: "Servicio total e intereses",
    labelField: "anio",
    serviceField: "service",
    shareField: "interest_share"
  });

  drawStackedBarChart(document.getElementById("debt-service"), state.articleVisuals.debt.service, {
    title: "Servicio de deuda por componente",
    labelField: "anio",
    fields: [
      { field: "principal", label: "Principal", color: "#466a8f" },
      { field: "interest", label: "Intereses", color: "#c86448" },
      { field: "commissions", label: "Comisiones", color: "#d4ac0d" }
    ],
    unit: "US$ MM"
  });
}

function visualMapConfig() {
  return {
    business: {
      features: territoryMapFeatures(),
      title: "Densidad empresarial por provincia",
      valueField: "business_density",
      labelField: "province",
      label: "Densidad empresarial",
      unit: "empresas por 1,000 hab.",
      colorStart: "#edf4f2",
      colorEnd: "#c86448",
      inspectorId: "visual-map-inspector",
      mapId: "visual-business",
      tooltipRows: [
        { field: "businesses", label: "Empresas" },
        { field: "population", label: "Poblacion" }
      ]
    },
    mipymes: {
      features: state.regionGeojson?.features || [],
      title: "Microempresas por region",
      valueField: "pct_micro",
      labelField: "region",
      label: "Microempresas",
      unit: "% microempresas",
      colorStart: "#eef1ed",
      colorEnd: "#6b7554",
      inspectorId: "visual-map-inspector",
      mapId: "visual-mipymes",
      tooltipRows: [
        { field: "pct_informal", label: "Informalidad", suffix: "%" }
      ]
    },
    tourism: {
      features: state.worldGeojson?.features || [],
      title: "Preferencia por sol y playa segun pais de origen",
      valueField: "beach_pct",
      labelField: "country",
      label: "Sol y playa",
      unit: "% motivo playa",
      colorStart: "#f5f1e8",
      colorEnd: "#7a2e21",
      inspectorId: "visual-map-inspector",
      mapId: "visual-tourism",
      showLabels: false
    }
  }[state.visualMap];
}

function transportScatterOptions() {
  return {
    title: "Prima de ubicacion vs empleo formal",
    xField: "median_rent_thousand",
    yField: "employment_share",
    sizeField: "jobs",
    labelField: "province",
    categoryField: "category",
    xLabel: "Alquiler mediano anual (RD$ miles)",
    yLabel: "% del empleo formal",
    sizeLabel: "Empleos",
    xTransform: "sqrt",
    yTransform: "sqrt",
    labelTopBy: "jobs",
    labelCount: 4,
    xReference: 80,
    yReference: 5,
    legend: [
      { label: "DN", color: "#c86448" },
      { label: "Periurbana GSD", color: "#6b7554" },
      { label: "Resto", color: "#466a8f" }
    ],
    colorMap: {
      DN: "#c86448",
      "Periurbana GSD": "#6b7554",
      Resto: "#466a8f"
    }
  };
}

function visibleModules() {
  return state.data.modules.filter((module) => module.visible !== false && statusKey(module.status) === "activo");
}

function filteredModules() {
  return visibleModules().filter((module) => {
    const matchesFamily = state.family === "all" || statusKey(module.family) === statusKey(state.family);
    const text = `${module.title} ${module.topic} ${module.type} ${module.summary || ""} ${module.source || ""}`.toLowerCase();
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
      caption: "Indice territorial normalizado."
    }
  }[state.territoryMapMetric];
}

function findModule(id) {
  return visibleModules().find((module) => module.id === id);
}

function setActive(moduleId) {
  const next = moduleId === "overview" || findModule(moduleId) ? moduleId : "overview";
  state.active = next;
  if (next === "overview") {
    history.replaceState(null, "", window.location.pathname);
  } else {
    history.replaceState(null, "", `#${next}`);
  }
  document.body.classList.remove("sidebar-open");
  els.menuToggle.setAttribute("aria-expanded", "false");
  renderNavigation();
  renderMobileNavigation();
  renderStage();
}

function hydrateModuleActions() {
  els.stage.querySelectorAll('[data-action="copy-link"]').forEach((button) => {
    button.addEventListener("click", async () => {
      const copied = await copyText(currentViewUrl());
      flashButton(button, copied ? "Copiado" : "Copiar");
    });
  });

  els.stage.querySelectorAll('[data-action="export-chart"]').forEach((button) => {
    button.addEventListener("click", () => {
      const canvas = els.stage.querySelector("canvas");
      if (!canvas) {
        flashButton(button, "Sin grafico");
        return;
      }
      const link = document.createElement("a");
      link.download = `atlas-${state.active}.png`;
      link.href = canvas.toDataURL("image/png");
      link.click();
      flashButton(button, "Descargado");
    });
  });

  els.stage.querySelectorAll('[data-action="expand-chart"]').forEach((button) => {
    button.addEventListener("click", () => openChartFullscreen(button.dataset.canvas));
  });
}

function currentViewUrl() {
  const url = new URL(window.location.href);
  url.hash = state.active === "overview" ? "" : state.active;
  return url.toString();
}

async function copyText(value) {
  if (navigator.clipboard && window.isSecureContext) {
    try {
      await navigator.clipboard.writeText(value);
      return true;
    } catch {
      // Fall through to the input-based copy path.
    }
  }
  const input = document.createElement("input");
  input.value = value;
  input.setAttribute("readonly", "");
  input.style.position = "fixed";
  input.style.opacity = "0";
  document.body.appendChild(input);
  input.select();
  const copied = document.execCommand("copy");
  input.remove();
  return copied;
}

function flashButton(button, label) {
  const original = button.textContent;
  button.textContent = label;
  window.setTimeout(() => {
    button.textContent = original;
  }, 1200);
}

function openChartFullscreen(canvasId) {
  const sourceCanvas = document.getElementById(canvasId);
  if (!sourceCanvas) return;
  hideTooltip(true);
  const card = sourceCanvas.closest(".chart-card");
  const title = card?.querySelector("h3")?.textContent || "Grafico";
  const article = card?.querySelector(".article-link");
  const modal = document.createElement("div");
  modal.className = "atlas-modal";
  modal.setAttribute("role", "dialog");
  modal.setAttribute("aria-modal", "true");
  modal.innerHTML = `
    <section class="atlas-modal-panel">
      <header class="atlas-modal-head">
        <h2>${escapeHtml(title)}</h2>
        <div class="atlas-modal-actions">
          ${article ? article.outerHTML : ""}
          <button class="atlas-modal-close" type="button">Cerrar</button>
        </div>
      </header>
      <div class="atlas-modal-body">
        <canvas id="expanded-${escapeHtml(canvasId)}" height="640" aria-label="${escapeHtml(title)}"></canvas>
      </div>
    </section>
  `;

  const close = () => {
    hideTooltip(true);
    document.body.classList.remove("modal-open");
    window.removeEventListener("keydown", onKeydown);
    modal.remove();
  };
  const onKeydown = (event) => {
    if (event.key === "Escape") close();
  };

  modal.addEventListener("click", (event) => {
    if (event.target === modal) close();
  });
  modal.querySelector(".atlas-modal-close").addEventListener("click", close);
  document.body.appendChild(modal);
  document.body.classList.add("modal-open");
  window.addEventListener("keydown", onKeydown);

  window.requestAnimationFrame(() => {
    const expandedCanvas = modal.querySelector("canvas");
    redrawExpandedChart(canvasId, expandedCanvas, sourceCanvas);
  });
}

function redrawExpandedChart(canvasId, canvas, sourceCanvas) {
  if (!canvas) return;
  if (canvasId === "visual-map") {
    const mapConfig = visualMapConfig();
    drawChoroplethMap(canvas, mapConfig.features, {
      ...mapConfig,
      inspectorId: null,
      mapId: `${mapConfig.mapId}-fullscreen`
    });
    return;
  }

  if (canvasId === "tourism-treemap") {
    drawTreemapChart(canvas, state.articleVisuals.tourism.treemap, {
      title: "Estructura de motivaciones turisticas",
      labelField: "motivo",
      valueField: "porcentaje",
      categoryField: "categoria"
    });
    return;
  }

  if (canvasId === "transport-space") {
    drawComplexScatterChart(canvas, state.articleVisuals.transport.rentEmployment, transportScatterOptions());
    return;
  }

  if (canvasId === "debt-burden") {
    drawDebtBurdenChart(canvas, state.articleVisuals.debt.service, {
      title: "Servicio total e intereses",
      labelField: "anio",
      serviceField: "service",
      shareField: "interest_share"
    });
    return;
  }

  if (canvasId === "debt-service") {
    drawStackedBarChart(canvas, state.articleVisuals.debt.service, {
      title: "Servicio de deuda por componente",
      labelField: "anio",
      fields: [
        { field: "principal", label: "Principal", color: "#466a8f" },
        { field: "interest", label: "Intereses", color: "#c86448" },
        { field: "commissions", label: "Comisiones", color: "#d4ac0d" }
      ],
      unit: "US$ MM"
    });
    return;
  }

  if (!sourceCanvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  clearCanvas(ctx, width, height);
  ctx.drawImage(sourceCanvas, 0, 0, width, height);
}

function renderBarRows(rows, options) {
  const max = options.max || Math.max(...rows.map((item) => Math.abs(item[options.valueField])), 1);
  return rows.map((item) => {
    const value = Number(item[options.valueField]);
    const width = Math.min(100, Math.abs(value) / max * 100);
    return `
      <div class="bar-row">
        <strong>${escapeHtml(item[options.labelField])}</strong>
        <span class="bar-track"><span class="bar-fill" style="width:${width}%"></span></span>
        <span>${formatNumber(value)}${options.suffix || ""}</span>
      </div>
    `;
  }).join("");
}

function drawLineChart(canvas, labels, values, title, options = {}) {
  drawDualLineChart(
    canvas,
    labels,
    [{ label: title, values, color: options.color || "#466a8f", stepped: options.stepped }],
    title,
    options
  );
}

function drawDualLineChart(canvas, labels, series, title, options = {}) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const padding = { top: 32, right: 26, bottom: 42, left: 58 };
  const allValues = series.flatMap((item) => item.values);
  const min = Math.min(...allValues);
  const max = Math.max(...allValues);
  const span = max - min || 1;
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;

  clearCanvas(ctx, width, height);
  drawGrid(ctx, width, height, padding, 4);
  drawCanvasTitle(ctx, title, padding.left, 18);

  const tooltipPoints = [];
  series.forEach((serie) => {
    const isStepped = Boolean(serie.stepped || options.stepped);
    const points = serie.values.map((value, index) => ({
      x: padding.left + (plotW * index) / Math.max(serie.values.length - 1, 1),
      y: padding.top + plotH - ((value - min) / span) * plotH,
      value,
      period: labels[index],
      label: serie.label
    }));

    ctx.beginPath();
    points.forEach((point, index) => {
      if (index === 0) ctx.moveTo(point.x, point.y);
      else if (isStepped) {
        const previous = points[index - 1];
        ctx.lineTo(point.x, previous.y);
        ctx.lineTo(point.x, point.y);
      } else {
        ctx.lineTo(point.x, point.y);
      }
    });
    ctx.strokeStyle = serie.color;
    ctx.lineWidth = 3;
    ctx.stroke();

    points.forEach((point) => {
      ctx.beginPath();
      ctx.arc(point.x, point.y, 4, 0, Math.PI * 2);
      ctx.fillStyle = serie.color;
      ctx.fill();
      tooltipPoints.push({ ...point, radius: 10 });
    });
  });

  ctx.fillStyle = "#6b7280";
  ctx.font = "11px Inter";
  labels.forEach((label, index) => {
    const x = padding.left + (plotW * index) / Math.max(labels.length - 1, 1);
    ctx.fillText(label, x - 10, height - 14);
  });

  for (let i = 0; i <= 4; i += 1) {
    const value = min + (span * i) / 4;
    const y = padding.top + plotH - (plotH * i) / 4;
    ctx.fillText(formatNumber(value), 8, y + 4);
  }

  drawLegend(ctx, series, padding.left, height - 4);
  bindPointTooltip(canvas, tooltipPoints, (point) => `
    <strong>${escapeHtml(point.label)}</strong>
    <span>${escapeHtml(point.period)}: ${formatNumber(point.value)}</span>
  `);
}

function drawHorizontalBarChart(canvas, rows, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const values = rows.map((item) => Number(item[options.valueField]));
  const max = options.max || Math.max(...values.map(Math.abs), 1);
  const padding = { top: 34, right: 40, bottom: 24, left: Math.min(190, width * 0.38) };
  const rowHeight = Math.min(34, (height - padding.top - padding.bottom) / rows.length);
  const gap = Math.max(8, rowHeight * 0.35);
  const barH = Math.max(13, rowHeight - gap);
  const boxes = [];

  clearCanvas(ctx, width, height);
  drawCanvasTitle(ctx, options.title, padding.left, 18);

  rows.forEach((item, index) => {
    const value = Number(item[options.valueField]);
    const y = padding.top + index * (barH + gap);
    const available = width - padding.left - padding.right;
    const barW = Math.abs(value) / max * available;
    ctx.fillStyle = "#3f4752";
    ctx.font = "12px Inter";
    ctx.fillText(String(item[options.labelField]), 8, y + barH - 2);
    ctx.fillStyle = value < 0 ? "#c74b4b" : index === 0 ? "#c86448" : "#466a8f";
    ctx.fillRect(padding.left, y, barW, barH);
    boxes.push({ x: padding.left, y, width: barW, height: barH, item, value });
    ctx.fillStyle = "#191b1f";
    ctx.font = "700 12px Inter";
    ctx.fillText(formatNumber(value), padding.left + barW + 8, y + barH - 2);
  });

  bindBoxTooltip(canvas, boxes, (box) => `
    <strong>${escapeHtml(box.item[options.labelField])}</strong>
    <span>${formatNumber(box.value)}${escapeHtml(options.suffix || "")}</span>
  `);
}

function drawCategoricalCountChart(canvas, rows, field, title) {
  const counts = rows.reduce((acc, item) => {
    acc[item[field]] = (acc[item[field]] || 0) + 1;
    return acc;
  }, {});
  const chartRows = Object.entries(counts)
    .sort((a, b) => b[1] - a[1])
    .map(([name, value]) => ({ name, value }));
  drawHorizontalBarChart(canvas, chartRows, {
    labelField: "name",
    valueField: "value",
    title,
    max: Math.max(...chartRows.map((item) => item.value), 1)
  });
}

function drawGroupedBarChart(canvas, rows, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const padding = { top: 40, right: 26, bottom: 70, left: 52 };
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  const groupW = plotW / rows.length;
  const barW = Math.min(18, groupW / (options.fields.length + 1));
  const max = Math.max(...rows.flatMap((row) => options.fields.map((field) => row[field.field])), 100);
  const boxes = [];

  clearCanvas(ctx, width, height);
  drawCanvasTitle(ctx, options.title, padding.left, 18);
  drawGrid(ctx, width, height, padding, 4);

  rows.forEach((row, rowIndex) => {
    options.fields.forEach((field, fieldIndex) => {
      const value = Number(row[field.field]);
      const x = padding.left + rowIndex * groupW + fieldIndex * (barW + 4) + groupW / 4;
      const barH = value / max * plotH;
      const y = padding.top + plotH - barH;
      ctx.fillStyle = field.color;
      ctx.fillRect(x, y, barW, barH);
      boxes.push({ x, y, width: barW, height: barH, item: row, field, value });
    });

    ctx.save();
    ctx.translate(padding.left + rowIndex * groupW + groupW / 2, height - 18);
    ctx.rotate(-Math.PI / 6);
    ctx.fillStyle = "#6b7280";
    ctx.font = "11px Inter";
    ctx.fillText(row[options.labelField], -36, 0);
    ctx.restore();
  });

  drawLegend(ctx, options.fields.map((field) => ({ label: field.label, color: field.color })), padding.left, 32);
  bindBoxTooltip(canvas, boxes, (box) => `
    <strong>${escapeHtml(box.item[options.labelField])}</strong>
    <span>${escapeHtml(box.field.label)}: ${formatNumber(box.value)}</span>
  `);
}

function drawTreemapChart(canvas, rows, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const padding = { top: 44, right: 18, bottom: 18, left: 18 };
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  const total = rows.reduce((sum, item) => sum + Number(item[options.valueField]), 0) || 1;
  const colors = {
    Masivo: "#eef1ed",
    Vinculado: "#466a8f",
    Nicho: "#c86448"
  };
  const boxes = [];

  clearCanvas(ctx, width, height);
  drawCanvasTitle(ctx, options.title, padding.left, 22);

  layoutTreemap(
    rows
    .slice()
      .sort((a, b) => Number(b[options.valueField]) - Number(a[options.valueField])),
    padding.left,
    padding.top,
    plotW,
    plotH,
    options.valueField,
    boxes
  );

  boxes.forEach((box) => {
    const category = box.item[options.categoryField];
    const value = Number(box.item[options.valueField]);
    ctx.fillStyle = colors[category] || "#6b7280";
    ctx.fillRect(box.x, box.y, box.width, box.height);
    ctx.strokeStyle = "#ffffff";
    ctx.lineWidth = 2;
    ctx.strokeRect(box.x, box.y, box.width, box.height);
    if (box.width > 72 && box.height > 42) {
      ctx.fillStyle = category === "Masivo" ? "#191b1f" : "#ffffff";
      ctx.font = value > 20 ? "800 18px Inter" : "700 12px Inter";
      ctx.fillText(String(box.item[options.labelField]).slice(0, 18), box.x + 8, box.y + 22);
      ctx.font = "700 12px Inter";
      ctx.fillText(`${formatNumber(value)}%`, box.x + 8, box.y + 40);
    }
  });

  bindBoxTooltip(canvas, boxes, (box) => `
    <strong>${escapeHtml(box.item[options.labelField])}</strong>
    <span>${formatNumber(box.item[options.valueField])}% de motivaciones</span>
    <span>${escapeHtml(box.item[options.categoryField])}</span>
  `);
}

function drawDebtBurdenChart(canvas, rows, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const padding = width < 420
    ? { top: 46, right: 18, bottom: 54, left: 48 }
    : { top: 46, right: 52, bottom: 58, left: 62 };
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  const serviceMax = Math.max(...rows.map((row) => Number(row[options.serviceField])), 1);
  const shareMax = 70;
  const gap = width < 420 ? 4 : 7;
  const barW = Math.max(8, (plotW - gap * (rows.length - 1)) / rows.length);
  const boxes = [];
  const points = [];
  const serviceColor = "#466a8f";
  const serviceHighlight = "#9f5f4b";
  const shareColor = "#6b7554";

  const xAt = (index) => padding.left + index * (barW + gap);
  const shareY = (value) => padding.top + plotH - (Number(value) / shareMax) * plotH;

  clearCanvas(ctx, width, height);
  drawCanvasTitle(ctx, options.title, padding.left, 22);
  drawGrid(ctx, width, height, padding, 4);

  rows.forEach((row, index) => {
    const service = Number(row[options.serviceField]);
    const x = xAt(index);
    const barH = (service / serviceMax) * plotH;
    const y = padding.top + plotH - barH;
    ctx.fillStyle = index === rows.length - 1 ? serviceHighlight : serviceColor;
    ctx.fillRect(x, y, barW, barH);
    boxes.push({ x, y, width: barW, height: barH, item: row, value: service });

    const point = {
      x: x + barW / 2,
      y: shareY(row[options.shareField]),
      radius: 10,
      item: row,
      value: Number(row[options.shareField])
    };
    points.push(point);

    ctx.fillStyle = "#6b7280";
    ctx.font = "10px Inter";
    if (index % 2 === 0 || width > 520) {
      ctx.fillText(String(row[options.labelField]).slice(-2), x, height - 18);
    }
  });

  ctx.beginPath();
  points.forEach((point, index) => {
    if (index === 0) ctx.moveTo(point.x, point.y);
    else ctx.lineTo(point.x, point.y);
  });
  ctx.strokeStyle = shareColor;
  ctx.lineWidth = 3;
  ctx.stroke();

  points.forEach((point) => {
    ctx.beginPath();
    ctx.arc(point.x, point.y, 4, 0, Math.PI * 2);
    ctx.fillStyle = shareColor;
    ctx.fill();
  });

  drawLegend(ctx, [
    { label: "Servicio total", color: serviceColor },
    { label: "Intereses / servicio", color: shareColor }
  ], padding.left, 40);

  bindCanvasTooltip(canvas, (event) => {
    const cursor = getCanvasPoint(canvas, event);
    const point = points.find((candidate) => {
      const dx = cursor.x - candidate.x;
      const dy = cursor.y - candidate.y;
      return Math.sqrt(dx * dx + dy * dy) <= candidate.radius + 2;
    });
    if (point) {
      return `
        <strong>${escapeHtml(point.item[options.labelField])}</strong>
        <span>Intereses: ${formatNumber(point.value)}%</span>
        <span>Servicio: ${formatNumber(point.item[options.serviceField])} US$ MM</span>
      `;
    }
    const box = boxes.find((candidate) => (
      cursor.x >= candidate.x &&
      cursor.x <= candidate.x + candidate.width &&
      cursor.y >= candidate.y &&
      cursor.y <= candidate.y + candidate.height
    ));
    if (!box) return null;
    return `
      <strong>${escapeHtml(box.item[options.labelField])}</strong>
      <span>Servicio: ${formatNumber(box.value)} US$ MM</span>
      <span>Intereses: ${formatNumber(box.item[options.shareField])}%</span>
    `;
  });
}

function drawStackedBarChart(canvas, rows, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const padding = { top: 44, right: 26, bottom: 54, left: 62 };
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  const totals = rows.map((row) => options.fields.reduce((sum, field) => sum + Number(row[field.field] || 0), 0));
  const max = Math.max(...totals, 1);
  const gap = 7;
  const barW = Math.max(10, (plotW - gap * (rows.length - 1)) / rows.length);
  const boxes = [];

  clearCanvas(ctx, width, height);
  drawCanvasTitle(ctx, options.title, padding.left, 22);
  drawGrid(ctx, width, height, padding, 4);

  rows.forEach((row, rowIndex) => {
    let y = padding.top + plotH;
    const x = padding.left + rowIndex * (barW + gap);
    options.fields.forEach((field) => {
      const value = Number(row[field.field] || 0);
      const barH = value / max * plotH;
      y -= barH;
      ctx.fillStyle = field.color;
      ctx.fillRect(x, y, barW, barH);
      boxes.push({ x, y, width: barW, height: barH, item: row, field, value });
    });
    ctx.fillStyle = "#6b7280";
    ctx.font = "10px Inter";
    ctx.fillText(String(row[options.labelField]).slice(-2), x - 1, height - 16);
  });

  drawLegend(ctx, options.fields, padding.left, 40);
  bindBoxTooltip(canvas, boxes, (box) => `
    <strong>${escapeHtml(box.item[options.labelField])}</strong>
    <span>${escapeHtml(box.field.label)}: ${formatNumber(box.value)} ${escapeHtml(options.unit || "")}</span>
  `);
}

function drawComplexScatterChart(canvas, rows, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const padding = { top: 54, right: 34, bottom: 62, left: 70 };
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  const xValues = rows.map((item) => Number(item[options.xField]));
  const yValues = rows.map((item) => Number(item[options.yField]));
  const sizeValues = rows.map((item) => Number(item[options.sizeField] || 1));
  const xMin = Math.min(0, Math.min(...xValues) - 3);
  const xMax = Math.max(...xValues) + 5;
  const yMin = Math.min(0, Math.min(...yValues) - 3);
  const yMax = Math.max(...yValues) + 7;
  const sizeMax = Math.max(...sizeValues, 1);
  const xRef = options.xReference ?? (xMin + xMax) / 2;
  const yRef = options.yReference ?? (yMin + yMax) / 2;
  const transformValue = (value, mode) => {
    const numeric = Number(value);
    if (mode === "sqrt") return Math.sign(numeric) * Math.sqrt(Math.abs(numeric));
    if (mode === "log") return Math.sign(numeric) * Math.log1p(Math.abs(numeric));
    return numeric;
  };
  const xDomainMin = transformValue(xMin, options.xTransform);
  const xDomainMax = transformValue(xMax, options.xTransform);
  const yDomainMin = transformValue(yMin, options.yTransform);
  const yDomainMax = transformValue(yMax, options.yTransform);

  const xScale = (value) => padding.left + ((transformValue(value, options.xTransform) - xDomainMin) / (xDomainMax - xDomainMin || 1)) * plotW;
  const yScale = (value) => padding.top + plotH - ((transformValue(value, options.yTransform) - yDomainMin) / (yDomainMax - yDomainMin || 1)) * plotH;
  const xRefPos = xScale(xRef);
  const yRefPos = yScale(yRef);
  const points = [];
  const labelRows = new Set(
    rows
      .slice()
      .sort((a, b) => Number(b[options.labelTopBy || options.sizeField]) - Number(a[options.labelTopBy || options.sizeField]))
      .slice(0, options.labelCount || 5)
      .map((item) => item[options.labelField])
  );

  clearCanvas(ctx, width, height);
  drawGrid(ctx, width, height, padding, 4);

  ctx.fillStyle = "rgba(42, 157, 143, 0.07)";
  ctx.fillRect(xRefPos, padding.top, padding.left + plotW - xRefPos, yRefPos - padding.top);
  ctx.fillStyle = "rgba(200, 100, 72, 0.07)";
  ctx.fillRect(padding.left, yRefPos, xRefPos - padding.left, padding.top + plotH - yRefPos);

  ctx.strokeStyle = "#7f8a95";
  ctx.lineWidth = 1;
  ctx.setLineDash([5, 5]);
  ctx.beginPath();
  ctx.moveTo(xRefPos, padding.top);
  ctx.lineTo(xRefPos, padding.top + plotH);
  ctx.moveTo(padding.left, yRefPos);
  ctx.lineTo(padding.left + plotW, yRefPos);
  ctx.stroke();
  ctx.setLineDash([]);

  drawCanvasTitle(ctx, options.title, padding.left, 22);
  drawAxisLabels(ctx, options.xLabel, options.yLabel, padding, width, height);

  rows
    .slice()
    .sort((a, b) => Number(a[options.sizeField]) - Number(b[options.sizeField]))
    .forEach((item, index) => {
      const x = xScale(Number(item[options.xField]));
      const y = yScale(Number(item[options.yField]));
      const radius = 7 + Math.sqrt(Number(item[options.sizeField]) / sizeMax) * 18;
      const net = Number(item[options.categoryField]);
      const category = item[options.categoryField];
      const color = options.colorMap?.[category] || (Number.isFinite(net) ? (net >= 0 ? "#2a9d8f" : "#c86448") : "#466a8f");
      ctx.beginPath();
      ctx.arc(x, y, radius, 0, Math.PI * 2);
      ctx.fillStyle = addAlpha(color, 0.68);
      ctx.fill();
      ctx.strokeStyle = "#ffffff";
      ctx.lineWidth = 2;
      ctx.stroke();
      points.push({ x, y, radius, item, color });

      if (labelRows.has(item[options.labelField])) {
        ctx.fillStyle = "#191b1f";
        ctx.font = "700 11px Inter";
        ctx.fillText(item[options.labelField], x + radius + 5, y + 4);
      }
    });

  drawLegend(ctx, options.legend || [
    { label: "Superavit relativo", color: "#2a9d8f" },
    { label: "Deficit relativo", color: "#c86448" }
  ], padding.left, 40);

  bindPointTooltip(canvas, points, (point) => `
    <strong>${escapeHtml(point.item[options.labelField])}</strong>
    <span>${escapeHtml(options.xLabel)}: ${formatNumber(point.item[options.xField])}</span>
    <span>${escapeHtml(options.yLabel)}: ${formatNumber(point.item[options.yField])}</span>
    <span>${escapeHtml(options.sizeLabel || "Tamano")}: ${formatNumber(point.item[options.sizeField])}</span>
  `);
}

function drawChoroplethMap(canvas, features, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const isCompact = width < 420;
  const padding = isCompact
    ? { top: 14, right: 8, bottom: 44, left: 8 }
    : { top: 46, right: 34, bottom: 58, left: 34 };
  const coordinates = collectGeoCoordinates(features);
  const values = features
    .map((feature) => Number(feature.properties[options.valueField]))
    .filter(Number.isFinite);

  if (!coordinates.length || !values.length) {
    clearCanvas(ctx, width, height);
    if (!isCompact) drawCanvasTitle(ctx, options.title, padding.left, 22);
    ctx.fillStyle = "#6b7280";
    ctx.font = "13px Inter";
    ctx.fillText("Sin datos cartograficos para esta vista.", padding.left, height / 2);
    updateMapInspector(options.inspectorId, null, options, [], false);
    return;
  }

  const lons = coordinates.map((coord) => coord[0]);
  const lats = coordinates.map((coord) => coord[1]);
  const bounds = {
    minLon: Math.min(...lons),
    maxLon: Math.max(...lons),
    minLat: Math.min(...lats),
    maxLat: Math.max(...lats)
  };
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  const scale = Math.min(
    plotW / (bounds.maxLon - bounds.minLon || 1),
    plotH / (bounds.maxLat - bounds.minLat || 1)
  );
  const mapW = (bounds.maxLon - bounds.minLon) * scale;
  const mapH = (bounds.maxLat - bounds.minLat) * scale;
  const offsetX = padding.left + (plotW - mapW) / 2;
  const offsetY = padding.top + (plotH - mapH) / 2;
  const min = Math.min(...values);
  const max = Math.max(...values);
  const project = ([lon, lat]) => [
    offsetX + (lon - bounds.minLon) * scale,
    offsetY + (bounds.maxLat - lat) * scale
  ];
  const entries = features.map((feature) => ({
    feature,
    value: Number(feature.properties[options.valueField]),
    path: buildFeaturePath(feature, project),
    centroid: project(geoCentroid(feature))
  }));
  const mapId = options.mapId || canvas.id;
  const currentPinnedEntry = () => entries.find((entry) => mapEntryKey(entry, options) === state.mapPinned[mapId]);
  const topEntry = entries
    .filter((entry) => Number.isFinite(entry.value))
    .sort((a, b) => b.value - a.value)[0];

  const paint = (highlightEntry = null) => {
    clearCanvas(ctx, width, height);
    if (!isCompact) drawCanvasTitle(ctx, options.title, padding.left, 22);
    entries.forEach((entry) => {
      ctx.fillStyle = Number.isFinite(entry.value)
        ? interpolateColor(options.colorStart || "#edf4f2", options.colorEnd || "#c86448", normalizeRatio(entry.value, min, max))
        : "#f1f1ee";
      ctx.fill(entry.path);
      ctx.strokeStyle = "#ffffff";
      ctx.lineWidth = 1;
      ctx.stroke(entry.path);
    });

    if (highlightEntry) {
      ctx.save();
      ctx.fillStyle = "rgba(255, 255, 255, 0.22)";
      ctx.fill(highlightEntry.path);
      ctx.restore();
      ctx.strokeStyle = "#191b1f";
      ctx.lineWidth = 2.8;
      ctx.stroke(highlightEntry.path);
    }

    if (options.showLabels !== false) drawMapLabels(ctx, entries, options, max);
    drawMapLegend(ctx, min, max, width, height, padding, options);
  };

  const syncInspector = (entry, isPinned = false) => {
    updateMapInspector(options.inspectorId, entry, options, entries, isPinned);
    bindMapInspectorButtons(options.inspectorId, (key) => {
      const selected = entries.find((candidate) => mapEntryKey(candidate, options) === key);
      if (!selected) return;
      state.mapPinned[mapId] = key;
      paint(selected);
      syncInspector(selected, true);
    });
  };

  paint(currentPinnedEntry() || topEntry);
  syncInspector(currentPinnedEntry() || topEntry, Boolean(currentPinnedEntry()));

  const focusMapEntry = (event, pin = false) => {
    const point = getCanvasPoint(canvas, event);
    const found = entries.find((entry) => ctx.isPointInPath(entry.path, point.x, point.y));
    if (!found) {
      canvas.style.cursor = "default";
      paint(currentPinnedEntry() || topEntry);
      syncInspector(currentPinnedEntry() || topEntry, Boolean(currentPinnedEntry()));
      hideTooltip(pin);
      return null;
    }
    canvas.style.cursor = "pointer";
    if (pin) state.mapPinned[mapId] = mapEntryKey(found, options);
    paint(found);
    syncInspector(found, mapEntryKey(found, options) === state.mapPinned[mapId]);
    const valueLabel = Number.isFinite(found.value)
      ? `${formatNumber(found.value)} ${escapeHtml(options.unit || "")}`.trim()
      : "Sin dato";
    const extraRows = (options.tooltipRows || [])
      .map((row) => {
        const raw = found.feature.properties[row.field];
        if (raw === null || raw === undefined || raw === "") return "";
        return `<span>${escapeHtml(row.label)}: ${formatNumber(raw)}${escapeHtml(row.suffix || "")}</span>`;
      })
      .join("");
    showTooltip(`
      <strong>${escapeHtml(found.feature.properties[options.labelField] || options.fallbackLabel)}</strong>
      <span>${valueLabel}</span>
      ${extraRows}
    `, event, { pinned: pin && event.pointerType !== "mouse" });
    return found;
  };

  canvas.onmousemove = null;
  canvas.onclick = null;
  canvas.onmouseleave = null;
  canvas.onpointermove = (event) => {
    if (event.pointerType === "touch") return;
    focusMapEntry(event, false);
  };

  canvas.onpointerdown = (event) => {
    const found = focusMapEntry(event, true);
    if (found && event.pointerType !== "mouse") event.preventDefault();
  };

  canvas.onpointerleave = () => {
    const selected = currentPinnedEntry() || topEntry;
    canvas.style.cursor = "default";
    paint(selected);
    syncInspector(selected, Boolean(state.mapPinned[mapId]));
    hideTooltip();
  };
}

function mapEntryKey(entry, options) {
  return String(entry.feature.properties[options.labelField] || options.fallbackLabel || "");
}

function updateMapInspector(inspectorId, entry, options, entries, isPinned) {
  if (!inspectorId) return;
  const inspector = document.getElementById(inspectorId);
  if (!inspector) return;
  if (!entry) {
    inspector.innerHTML = `<div class="map-empty">Sin datos</div>`;
    return;
  }

  const label = entry.feature.properties[options.labelField] || options.fallbackLabel || "Sin dato";
  const value = Number.isFinite(entry.value) ? `${formatNumber(entry.value)} ${options.unit || ""}`.trim() : "Sin dato";
  const topEntries = entries
    .filter((candidate) => Number.isFinite(candidate.value))
    .sort((a, b) => b.value - a.value)
    .slice(0, 6);
  const extraRows = (options.tooltipRows || [])
    .map((row) => {
      const raw = entry.feature.properties[row.field];
      if (raw === null || raw === undefined || raw === "") return "";
      return `
        <div>
          <span>${escapeHtml(row.label)}</span>
          <strong>${formatNumber(raw)}${escapeHtml(row.suffix || "")}</strong>
        </div>
      `;
    })
    .join("");

  inspector.innerHTML = `
    <div class="map-focus">
      <small>${isPinned ? "Fijado" : "Foco"}</small>
      <strong>${escapeHtml(label)}</strong>
      <span>${escapeHtml(value)}</span>
    </div>
    ${extraRows ? `<div class="map-stat-grid">${extraRows}</div>` : ""}
    <div class="map-rank-list">
      ${topEntries.map((candidate, index) => {
        const candidateLabel = candidate.feature.properties[options.labelField] || options.fallbackLabel || "";
        return `
          <button class="map-rank-row" type="button" data-map-key="${escapeHtml(mapEntryKey(candidate, options))}">
            <span>${index + 1}</span>
            <strong>${escapeHtml(candidateLabel)}</strong>
            <em>${formatNumber(candidate.value)}</em>
          </button>
        `;
      }).join("")}
    </div>
  `;
}

function bindMapInspectorButtons(inspectorId, onSelect) {
  if (!inspectorId) return;
  const inspector = document.getElementById(inspectorId);
  if (!inspector) return;
  inspector.querySelectorAll(".map-rank-row").forEach((button) => {
    button.addEventListener("click", () => onSelect(button.dataset.mapKey));
  });
}

function drawScatterChart(canvas, rows, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const padding = { top: 40, right: 30, bottom: 48, left: 54 };
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  const xValues = rows.map((item) => item[options.xField]);
  const yValues = rows.map((item) => item[options.yField]);
  const xMin = Math.min(...xValues) - 4;
  const xMax = Math.max(...xValues) + 4;
  const yMin = Math.min(...yValues) - 4;
  const yMax = Math.max(...yValues) + 4;
  const points = [];

  clearCanvas(ctx, width, height);
  drawCanvasTitle(ctx, options.title, padding.left, 18);
  drawGrid(ctx, width, height, padding, 4);

  rows.forEach((item, index) => {
    const x = padding.left + ((item[options.xField] - xMin) / (xMax - xMin || 1)) * plotW;
    const y = padding.top + plotH - ((item[options.yField] - yMin) / (yMax - yMin || 1)) * plotH;
    const radius = 5 + (item[options.sizeField] / 100) * 7;
    ctx.beginPath();
    ctx.arc(x, y, radius, 0, Math.PI * 2);
    ctx.fillStyle = index < 3 ? "rgba(200, 100, 72, 0.78)" : "rgba(70, 106, 143, 0.62)";
    ctx.fill();
    ctx.strokeStyle = "#ffffff";
    ctx.lineWidth = 2;
    ctx.stroke();
    points.push({ x, y, radius: radius + 4, item });
    if (index < 6) {
      ctx.fillStyle = "#191b1f";
      ctx.font = "11px Inter";
      ctx.fillText(item[options.labelField], x + radius + 4, y + 4);
    }
  });

  ctx.fillStyle = "#6b7280";
  ctx.font = "11px Inter";
  ctx.fillText("Infraestructura", padding.left, height - 12);
  ctx.save();
  ctx.translate(14, padding.top + plotH);
  ctx.rotate(-Math.PI / 2);
  ctx.fillText("Mercado", 0, 0);
  ctx.restore();

  bindPointTooltip(canvas, points, (point) => `
    <strong>${escapeHtml(point.item[options.labelField])}</strong>
    <span>${escapeHtml(options.xLabel || "Infraestructura")}: ${formatNumber(point.item[options.xField])}</span>
    <span>${escapeHtml(options.yLabel || "Mercado")}: ${formatNumber(point.item[options.yField])}</span>
    <span>${escapeHtml(options.sizeLabel || "Oportunidad")}: ${formatNumber(point.item[options.sizeField])}</span>
  `);
}

function drawAxisLabels(ctx, xLabel, yLabel, padding, width, height) {
  ctx.fillStyle = "#6b7280";
  ctx.font = "11px Inter";
  ctx.fillText(xLabel, padding.left, height - 16);
  ctx.save();
  ctx.translate(18, padding.top + (height - padding.top - padding.bottom) / 2 + padding.bottom);
  ctx.rotate(-Math.PI / 2);
  ctx.fillText(yLabel, 0, 0);
  ctx.restore();
}

function bindPointTooltip(canvas, points, content) {
  bindCanvasTooltip(canvas, (event) => {
    const cursor = getCanvasPoint(canvas, event);
    const found = points.find((point) => {
      const dx = cursor.x - point.x;
      const dy = cursor.y - point.y;
      return Math.sqrt(dx * dx + dy * dy) <= point.radius + 2;
    });

    return found ? content(found) : null;
  });
}

function bindBoxTooltip(canvas, boxes, content) {
  bindCanvasTooltip(canvas, (event) => {
    const cursor = getCanvasPoint(canvas, event);
    const found = boxes.find((box) => (
      cursor.x >= box.x &&
      cursor.x <= box.x + box.width &&
      cursor.y >= box.y &&
      cursor.y <= box.y + box.height
    ));

    return found ? content(found) : null;
  });
}

function bindCanvasTooltip(canvas, resolveContent) {
  canvas.onmousemove = null;
  canvas.onclick = null;
  canvas.onmouseleave = null;
  canvas.onpointermove = (event) => {
    if (event.pointerType === "touch") return;
    const html = resolveContent(event);
    canvas.style.cursor = html ? "pointer" : "default";
    if (!html) {
      hideTooltip();
      return;
    }
    showTooltip(html, event, { pinned: false });
  };

  canvas.onpointerdown = (event) => {
    const html = resolveContent(event);
    canvas.style.cursor = html ? "pointer" : "default";
    if (!html) {
      hideTooltip(true);
      return;
    }
    showTooltip(html, event, { pinned: event.pointerType !== "mouse" });
    if (event.pointerType !== "mouse") event.preventDefault();
  };

  canvas.onpointerleave = () => {
    canvas.style.cursor = "default";
    hideTooltip();
  };
}

function layoutTreemap(items, x, y, width, height, valueField, boxes) {
  if (!items.length || width <= 0 || height <= 0) return;
  if (items.length === 1) {
    boxes.push({ x, y, width, height, item: items[0] });
    return;
  }

  const total = items.reduce((sum, item) => sum + Number(item[valueField]), 0);
  let running = 0;
  let splitIndex = 0;
  for (let index = 0; index < items.length; index += 1) {
    running += Number(items[index][valueField]);
    splitIndex = index + 1;
    if (running >= total / 2) break;
  }

  const left = items.slice(0, splitIndex);
  const right = items.slice(splitIndex);
  const leftTotal = left.reduce((sum, item) => sum + Number(item[valueField]), 0);
  const ratio = leftTotal / (total || 1);

  if (width >= height) {
    const leftW = width * ratio;
    layoutTreemap(left, x, y, leftW, height, valueField, boxes);
    layoutTreemap(right, x + leftW, y, width - leftW, height, valueField, boxes);
  } else {
    const topH = height * ratio;
    layoutTreemap(left, x, y, width, topH, valueField, boxes);
    layoutTreemap(right, x, y + topH, width, height - topH, valueField, boxes);
  }
}

function collectGeoCoordinates(features) {
  const coordinates = [];
  features.forEach((feature) => {
    const geometry = feature.geometry;
    if (!geometry) return;
    const polygons = geometry.type === "Polygon" ? [geometry.coordinates] : geometry.coordinates;
    polygons.forEach((polygon) => {
      polygon.forEach((ring) => {
        ring.forEach((coord) => coordinates.push(coord));
      });
    });
  });
  return coordinates;
}

function buildFeaturePath(feature, project) {
  const path = new Path2D();
  const geometry = feature.geometry;
  const polygons = geometry.type === "Polygon" ? [geometry.coordinates] : geometry.coordinates;
  polygons.forEach((polygon) => {
    polygon.forEach((ring) => {
      ring.forEach((coord, index) => {
        const [x, y] = project(coord);
        if (index === 0) path.moveTo(x, y);
        else path.lineTo(x, y);
      });
      path.closePath();
    });
  });
  return path;
}

function drawMapLabels(ctx, entries, options, max) {
  const topEntries = entries
    .filter((entry) => Number.isFinite(entry.value))
    .sort((a, b) => b.value - a.value)
    .slice(0, 5);

  topEntries.forEach((entry) => {
    const label = entry.feature.properties[options.labelField];
    if (!entry.centroid) return;
    const ratio = normalizeRatio(entry.value, 0, max);
    ctx.fillStyle = ratio > 0.58 ? "#ffffff" : "#191b1f";
    ctx.font = "700 10px Inter";
    ctx.fillText(String(label).replace("Santo Domingo", "S. Domingo"), entry.centroid[0] - 22, entry.centroid[1]);
  });
}

function geoCentroid(feature) {
  const coords = collectGeoCoordinates([feature]);
  if (!coords.length) return [0, 0];
  const totals = coords.reduce((acc, coord) => {
    acc.lon += coord[0];
    acc.lat += coord[1];
    return acc;
  }, { lon: 0, lat: 0 });
  return [totals.lon / coords.length, totals.lat / coords.length];
}

function drawMapLegend(ctx, min, max, width, height, padding, options = {}) {
  ctx.font = "11px Inter";
  const unitLabel = options.unit || "";
  const unitW = unitLabel ? ctx.measureText(unitLabel).width : 0;
  const availableW = width - padding.left - padding.right;
  const reserveUnit = unitLabel && availableW > 320 ? unitW + 16 : 0;
  const legendW = Math.min(240, Math.max(120, availableW - reserveUnit));
  const legendH = 10;
  const x = padding.left;
  const y = height - 28;
  const gradient = ctx.createLinearGradient(x, y, x + legendW, y);
  gradient.addColorStop(0, options.colorStart || "#edf4f2");
  gradient.addColorStop(1, options.colorEnd || "#c86448");
  ctx.fillStyle = gradient;
  ctx.fillRect(x, y, legendW, legendH);
  ctx.strokeStyle = "#cbd5cf";
  ctx.strokeRect(x, y, legendW, legendH);
  ctx.fillStyle = "#6b7280";
  const minLabel = formatNumber(min);
  const maxLabel = formatNumber(max);
  ctx.fillText(minLabel, x, y + 26);
  ctx.fillText(maxLabel, x + legendW - ctx.measureText(maxLabel).width, y + 26);
  if (unitLabel && x + legendW + 12 + unitW <= width - padding.right) {
    ctx.fillText(unitLabel, x + legendW + 12, y + 9);
  } else if (unitLabel) {
    ctx.fillText(unitLabel, x, y - 6);
  }
}

function normalizeRatio(value, min, max) {
  return Math.max(0, Math.min(1, (value - min) / (max - min || 1)));
}

function interpolateColor(start, end, ratio) {
  const from = hexToRgb(start);
  const to = hexToRgb(end);
  const mix = from.map((channel, index) => Math.round(channel + (to[index] - channel) * ratio));
  return `rgb(${mix[0]}, ${mix[1]}, ${mix[2]})`;
}

function addAlpha(hex, alpha) {
  const [r, g, b] = hexToRgb(hex);
  return `rgba(${r}, ${g}, ${b}, ${alpha})`;
}

function hexToRgb(hex) {
  const clean = hex.replace("#", "");
  return [
    parseInt(clean.slice(0, 2), 16),
    parseInt(clean.slice(2, 4), 16),
    parseInt(clean.slice(4, 6), 16)
  ];
}

function getCanvasPoint(canvas, event) {
  const rect = canvas.getBoundingClientRect();
  const source = event.touches?.[0] || event.changedTouches?.[0] || event;
  return {
    x: source.clientX - rect.left,
    y: source.clientY - rect.top
  };
}

function ensureTooltip() {
  let tooltip = document.querySelector(".atlas-tooltip");
  if (!tooltip) {
    tooltip = document.createElement("div");
    tooltip.className = "atlas-tooltip";
    document.body.appendChild(tooltip);
  }
  return tooltip;
}

function showTooltip(html, event, options = {}) {
  if (!state.tooltip) state.tooltip = ensureTooltip();
  const source = event.touches?.[0] || event.changedTouches?.[0] || event;
  state.tooltipPinned = Boolean(options.pinned);
  state.tooltip.innerHTML = html;
  state.tooltip.classList.toggle("is-pinned", state.tooltipPinned);
  state.tooltip.style.opacity = "1";
  state.tooltip.style.transform = "translateY(0)";
  state.tooltip.style.left = `${Math.max(8, Math.min(window.innerWidth - 260, source.clientX + 14))}px`;
  state.tooltip.style.top = `${Math.max(8, Math.min(window.innerHeight - 120, source.clientY + 14))}px`;
}

function hideTooltip(force = false) {
  if (!state.tooltip) return;
  if (state.tooltipPinned && !force) return;
  state.tooltipPinned = false;
  state.tooltip.classList.remove("is-pinned");
  state.tooltip.style.opacity = "0";
  state.tooltip.style.transform = "translateY(4px)";
}

function setupCanvas(canvas) {
  const rect = canvas.getBoundingClientRect();
  const dpr = window.devicePixelRatio || 1;
  canvas.classList.add("is-interactive");
  canvas.style.touchAction = "manipulation";
  canvas.width = Math.max(1, Math.floor(rect.width * dpr));
  canvas.height = Math.max(1, Math.floor(rect.height * dpr));
  const ctx = canvas.getContext("2d");
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  return ctx;
}

function clearCanvas(ctx, width, height) {
  ctx.clearRect(0, 0, width, height);
  ctx.fillStyle = "#ffffff";
  ctx.fillRect(0, 0, width, height);
}

function drawCanvasTitle(ctx, title, x, y) {
  ctx.fillStyle = "#3f4752";
  ctx.font = "700 12px Inter";
  ctx.fillText(title, x, y);
}

function drawLegend(ctx, series, x, y) {
  let offset = 0;
  series.forEach((item) => {
    ctx.fillStyle = item.color;
    ctx.fillRect(x + offset, y - 10, 10, 10);
    ctx.fillStyle = "#3f4752";
    ctx.font = "11px Inter";
    ctx.fillText(item.label, x + offset + 14, y);
    offset += ctx.measureText(item.label).width + 42;
  });
}

function drawGrid(ctx, width, height, padding, steps) {
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  ctx.strokeStyle = "#dde3de";
  ctx.lineWidth = 1;
  ctx.setLineDash([2, 4]);
  for (let i = 0; i <= steps; i += 1) {
    const y = padding.top + (plotH * i) / steps;
    ctx.beginPath();
    ctx.moveTo(padding.left, y);
    ctx.lineTo(padding.left + plotW, y);
    ctx.stroke();
  }
  ctx.setLineDash([]);
}

function formatNumber(value) {
  return new Intl.NumberFormat("es-DO", { maximumFractionDigits: 1 }).format(value);
}

function normalizeText(value) {
  return String(value).normalize("NFD").replace(/[\u0300-\u036f]/g, "").toLowerCase();
}

function statusKey(value) {
  return normalizeText(value).trim();
}

function escapeHtml(value) {
  return String(value)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#039;");
}

function debounce(fn, wait) {
  let timer = null;
  return (...args) => {
    window.clearTimeout(timer);
    timer = window.setTimeout(() => fn(...args), wait);
  };
}

function renderLoadError(error) {
  els.stage.innerHTML = `
    <div class="empty-state">
      <strong>No se pudo cargar el Atlas.</strong>
      <p>Revisa que <code>atlas/data/atlas-data.json</code> exista y que la pagina se abra desde un servidor local o GitHub Pages.</p>
      <p>${escapeHtml(error.message)}</p>
    </div>
  `;
}

boot();
