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
      ${renderSourceCard(module)}
    </div>
  `;
}

function renderSourceCard(module) {
  const info = module.sourceInfo;
  if (!info) return "";

  const sourceLabel = info.label || module.source || "Fuente";
  const method = Array.isArray(info.methodology) ? info.methodology.find(Boolean) : "";
  const links = Array.isArray(info.related)
    ? info.related.map((href) => {
      const safeHref = sourceRelatedHref(href);
      return `<a href="${escapeHtml(safeHref)}">${escapeHtml(sourceRelatedLabel(href))}</a>`;
    }).join("")
    : "";

  return `
    <aside class="source-card" aria-label="Ficha fuente">
      <div class="source-card-main">
        <span>Ficha fuente</span>
        <strong>${escapeHtml(sourceLabel)}</strong>
        ${info.detail ? `<p>${escapeHtml(info.detail)}</p>` : ""}
      </div>
      <dl>
        ${info.updated ? `<div><dt>Corte</dt><dd>${escapeHtml(info.updated)}</dd></div>` : ""}
        ${method ? `<div><dt>Lectura</dt><dd>${escapeHtml(method)}</dd></div>` : ""}
      </dl>
      ${links ? `<nav class="source-links" aria-label="Enlaces relacionados">${links}</nav>` : ""}
    </aside>
  `;
}

function sourceRelatedHref(href) {
  const value = String(href || "");
  if (/^(https?:|\/|#|\.\.\/)/.test(value)) return value;
  return `../${value}`;
}

function sourceRelatedLabel(href) {
  const value = String(href || "");
  if (value.includes("republica-habla-de")) return "Serie";
  if (value.includes("republica-en-un-grafico")) return "Visuales";
  if (value.includes("archivo")) return "Archivo";
  return "Abrir";
}

function renderMacro() {
  return `
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Trayectoria macro reciente</h3>
        </div>
        <div class="chart-toolbar">
          ${chartToggle("macro", "dolar", "Dólar")}
          ${chartToggle("macro", "inflacion", "Inflación")}
          ${chartToggle("macro", "imae", "IMAE")}
          ${chartToggle("macro", "tpm", "TPM")}
        </div>
      </div>
      <canvas id="macro-chart" height="320" aria-label="Gráfico de línea macro"></canvas>
    </section>
  `;
}

function renderExternal() {
  return `
    <section class="chart-card chart-card-wide">
      <h3>Índice de presión externa</h3>
      <canvas id="external-chart" height="320" aria-label="Gráfico de presión externa"></canvas>
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
      <h3>Presión por sector</h3>
      <div class="sector-list">${renderBarRows(state.data.series.sectors, {
        labelField: "sector",
        valueField: "pressure",
        max: 100,
        suffix: "/100"
      })}</div>
    </section>
    <section class="chart-card">
      <h3>Driver principal</h3>
      <canvas id="sector-driver-chart" height="320" aria-label="Gráfico de drivers sectoriales"></canvas>
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
      <canvas id="trade-chart" height="340" aria-label="Gráfico de socios comerciales"></canvas>
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
      <canvas id="labor-chart" height="340" aria-label="Gráfico laboral por educación"></canvas>
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
      <h3>Inflación general y subyacente</h3>
      <canvas id="prices-chart" height="320" aria-label="Gráfico de inflación general y subyacente"></canvas>
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
      <canvas id="territory-chart" height="340" aria-label="Gráfico territorial"></canvas>
    </section>
  `;
}

function renderMipymes() {
  return `
    <section class="chart-card chart-card-wide">
      <h3>Acceso, formalidad y productividad</h3>
      <canvas id="mipyme-chart" height="340" aria-label="Gráfico de MiPyMES"></canvas>
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
          <h3>Mapas desde artículos</h3>
        </div>
        ${chartControls(visualMapToolbar, visualMapActions)}
      </div>
      <div class="map-workbench">
        <div class="map-canvas-wrap">
          <canvas id="visual-map" height="620" aria-label="Mapa interactivo desde artículos"></canvas>
        </div>
        <aside class="map-inspector" id="visual-map-inspector" aria-label="Detalle del mapa"></aside>
      </div>
    </section>
    <section class="chart-card">
      <div class="card-head">
        <div>
          <h3>Demanda turística por motivo</h3>
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
          <h3>Deuda pública</h3>
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
      <h3>Módulo no disponible</h3>
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

function articleLink(articleId, label = "Leer análisis") {
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
      inflacion: "Inflación interanual (%)",
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
      "Presión externa"
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
      "Inflación (%)"
    );
  }

  if (module.chart === "territory") {
    const mapMeta = territoryMapMeta();
    drawChoroplethMap(document.getElementById("territory-map"), territoryMapFeatures(), {
      ...mapMeta,
      title: `República Dominicana: ${mapMeta.label}`,
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
    title: "Estructura de motivaciones turísticas",
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
      title: "Microempresas por región",
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
      title: "Preferencia por sol y playa según país de origen",
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
      caption: "Índice territorial normalizado."
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

boot();
