const state = {
  data: null,
  geojson: null,
  regionGeojson: null,
  worldGeojson: null,
  articleVisuals: null,
  tooltip: null,
  active: "pulso-macro",
  query: "",
  family: "all",
  macroMetric: "dolar",
  tradeMetric: "exports",
  laborMetric: "employment",
  territoryRegion: "all",
  visualMap: "business"
};

const els = {
  nav: document.getElementById("module-nav"),
  search: document.getElementById("atlas-search"),
  metricStrip: document.getElementById("metric-strip"),
  stage: document.getElementById("module-stage"),
  rail: document.getElementById("insight-rail"),
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
        item.classList.toggle("is-active", item === button);
      });
      renderNavigation();
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
}

function render() {
  renderMetrics();
  renderNavigation();
  renderStage();
  renderRail();
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
      <small>Portada</small>
      <strong>Vista general</strong>
      <span>${visibleModules().length} modulos activos y listos para crecer.</span>
    </button>`,
    ...modules.map((module) => `
      <button class="module-button ${state.active === module.id ? "is-active" : ""}" type="button" data-module="${module.id}">
        <small>${escapeHtml(module.family)} · ${escapeHtml(module.type)}</small>
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

function renderOverviewIfActive() {
  if (state.active === "overview") {
    renderStage();
    renderRail();
  }
}

function renderStage() {
  if (!state.data) return;

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
        <p>${escapeHtml(module.summary)}</p>
      </div>
      <span class="status-badge" data-status="${module.status}">${escapeHtml(module.status)}</span>
    </div>
    ${renderModuleBody(module)}
  `;

  hydrateCharts(module);
}

function renderOverview() {
  const modules = filteredModules();
  els.stage.innerHTML = `
    <div class="stage-header">
      <div>
        <p class="eyebrow">${escapeHtml(state.data.brand.shortName)} · Sistema modular</p>
        <h2>Todos los analisis, una arquitectura.</h2>
        <p>${escapeHtml(state.data.brand.sourceNote)}</p>
      </div>
      <span class="status-badge" data-status="Activo">${modules.length} visibles</span>
    </div>
    <div class="system-strip">
      <div>
        <small>Contrato</small>
        <strong>${escapeHtml(state.data.system.visibilityPolicy)}</strong>
      </div>
      <div>
        <small>Actualizado</small>
        <strong>${escapeHtml(state.data.updated)}</strong>
      </div>
      <div>
        <small>Version</small>
        <strong>${escapeHtml(state.data.system.version)}</strong>
      </div>
    </div>
    <div class="module-grid">
      ${modules.map((module) => `
        <button class="module-card" type="button" data-module="${module.id}">
          <span class="status-badge" data-status="${module.status}">${escapeHtml(module.status)}</span>
          <h3>${escapeHtml(module.title)}</h3>
          <p>${escapeHtml(module.summary)}</p>
          <footer>
            <span>${escapeHtml(module.family)}</span>
            <span>${escapeHtml(module.source)}</span>
          </footer>
        </button>
      `).join("")}
    </div>
  `;

  els.stage.querySelectorAll(".module-card").forEach((button) => {
    button.addEventListener("click", () => setActive(button.dataset.module));
  });
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
      ${renderModuleNotes(module)}
    </div>
  `;
}

function renderMacro() {
  return `
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Trayectoria macro reciente</h3>
          <p>Selector de metrica para comparar el pulso de corto plazo.</p>
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
      <p>Escala 0-100; valores mas altos indican un entorno global mas exigente.</p>
      <canvas id="external-chart" height="320" aria-label="Grafico de presion externa"></canvas>
    </section>
    <section class="chart-card">
      <h3>Drivers comparables</h3>
      <p>Lectura normalizada de los componentes que alimentan la senal externa.</p>
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
      <p>Ranking 0-100; mayor valor implica mayor tension relativa de los drivers actuales.</p>
      <div class="sector-list">${renderBarRows(state.data.series.sectors, {
        labelField: "sector",
        valueField: "pressure",
        max: 100,
        suffix: "/100"
      })}</div>
    </section>
    <section class="chart-card">
      <h3>Driver principal</h3>
      <p>Frecuencia del driver dominante dentro del ranking sectorial.</p>
      <canvas id="sector-driver-chart" height="320" aria-label="Grafico de drivers sectoriales"></canvas>
    </section>
  `;
}

function renderTrade() {
  const metricLabels = { exports: "Exportaciones", imports: "Importaciones", opportunity: "Oportunidad" };
  return `
    <section class="chart-card chart-card-wide">
      <h3>Espacio de oportunidad comercial</h3>
      <p>Lectura de socios por dependencia importadora, potencial exportador y tamano relativo de oportunidad.</p>
      <canvas id="trade-space-chart" height="420" aria-label="Espacio de oportunidad comercial"></canvas>
      <p class="chart-caption">Burbujas: oportunidad relativa. Color: posicion neta del socio.</p>
    </section>
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Socios comerciales</h3>
          <p>Ranking por flujo u oportunidad relativa en el bloque visible.</p>
        </div>
        <div class="chart-toolbar">
          ${chartToggle("trade", "exports", "Exporta")}
          ${chartToggle("trade", "imports", "Importa")}
          ${chartToggle("trade", "opportunity", "Oportunidad")}
        </div>
      </div>
      <canvas id="trade-chart" height="340" aria-label="Grafico de socios comerciales"></canvas>
      <p class="chart-caption">${metricLabels[state.tradeMetric]} · valores relativos.</p>
    </section>
    <section class="chart-card">
      <h3>Productos y sofisticacion</h3>
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
  const labels = { employment: "Empleo", informality: "Informalidad", wageIndex: "Indice salarial" };
  return `
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Insercion por educacion</h3>
          <p>Comparacion de empleo, informalidad e indice salarial relativo.</p>
        </div>
        <div class="chart-toolbar">
          ${chartToggle("labor", "employment", "Empleo")}
          ${chartToggle("labor", "informality", "Informalidad")}
          ${chartToggle("labor", "wageIndex", "Salario")}
        </div>
      </div>
      <canvas id="labor-chart" height="340" aria-label="Grafico laboral por educacion"></canvas>
      <p class="chart-caption">${labels[state.laborMetric]} por grupo educativo.</p>
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
      <p>Linea de seguimiento para distinguir shock visible y presion persistente.</p>
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
  const mapRows = territoryMapRows();
  return `
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Mapa de densidad empresarial</h3>
          <p>Coropleta provincial construida desde shapefile y datos empresariales/poblacionales.</p>
        </div>
      </div>
      <canvas id="territory-map" height="500" aria-label="Mapa provincial de densidad empresarial"></canvas>
      <p class="chart-caption">Color: empresas por 1,000 habitantes. Pasa el cursor para leer provincia y valor.</p>
    </section>
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Oportunidad territorial</h3>
          <p>Relacion entre infraestructura y escala de mercado por provincia.</p>
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
    <section class="chart-card">
      <h3>${mapRows.length ? "Ranking por densidad empresarial" : "Ranking provincial"}</h3>
      <div class="table-list">
        ${(mapRows.length ? mapRows.slice(0, 8) : territoryRows()).map((item) => `
          <div class="table-row">
            <strong>${escapeHtml(item.province)}</strong>
            <span>${escapeHtml(item.region || item.region_code || "RD")}</span>
            <span>${formatNumber(item.business_density ?? item.opportunity)}${item.business_density === undefined ? "/100" : " emp./1,000 hab."}</span>
          </div>
        `).join("")}
      </div>
    </section>
  `;
}

function renderMipymes() {
  return `
    <section class="chart-card chart-card-wide">
      <h3>Acceso, formalidad y productividad</h3>
      <p>Comparacion por tamano y condicion empresarial.</p>
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
  const visuals = state.articleVisuals;
  if (!visuals) {
    return `
      <section class="chart-card chart-card-wide">
        <h3>Galeria visual pendiente</h3>
        <p>Los activos interactivos de articulos todavia no estan publicados en esta version del Atlas.</p>
      </section>
    `;
  }

  const captions = {
    business: "Densidad empresarial provincial desde el articulo de desarrollo territorial.",
    mipymes: "Microempresas e informalidad por region desde el articulo de MiPyMES.",
    tourism: "Preferencia por sol y playa por pais de origen desde el articulo de turismo."
  };

  return `
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Mapas interactivos desde articulos</h3>
          <p>${escapeHtml(captions[state.visualMap])}</p>
        </div>
        <div class="chart-toolbar">
          ${chartToggle("visual", "business", "Empresas")}
          ${chartToggle("visual", "mipymes", "MiPyMES")}
          ${chartToggle("visual", "tourism", "Turismo")}
        </div>
      </div>
      <canvas id="visual-map" height="500" aria-label="Mapa interactivo desde articulos"></canvas>
      <p class="chart-caption">Datos trazables a articulos publicados. Hover para leer cada territorio.</p>
    </section>
    <section class="chart-card">
      <h3>Demanda turistica por motivo</h3>
      <p>Treemap interactivo para ver cuanto ocupan playa, clima y nichos de alto valor.</p>
      <canvas id="tourism-treemap" height="340" aria-label="Treemap de motivos turisticos"></canvas>
    </section>
    <section class="chart-card">
      <h3>Empleo formal y alquiler</h3>
      <p>Relacion provincial entre prima de ubicacion y concentracion del empleo formal.</p>
      <canvas id="transport-space" height="340" aria-label="Scatter de empleo formal y alquiler"></canvas>
    </section>
    <section class="chart-card chart-card-wide">
      <h3>Servicio de deuda</h3>
      <p>Composicion anual de principal, intereses y comisiones del servicio de deuda.</p>
      <canvas id="debt-service" height="360" aria-label="Servicio de deuda por componente"></canvas>
    </section>
    <section class="chart-card">
      <h3>Activos usados</h3>
      <div class="table-list">
        ${visuals.sources.map((source) => {
          const sourceFiles = Array.isArray(source.files) ? source.files : [source.files].filter(Boolean);
          return `
          <div class="table-row">
            <strong>${escapeHtml(source.title)}</strong>
            <span>${escapeHtml(source.article)}</span>
            <em>${escapeHtml(sourceFiles.join(", "))}</em>
          </div>
        `;
        }).join("")}
      </div>
    </section>
  `;
}

function renderModuleNotes(module) {
  return `
    <section class="chart-card module-notes">
      <h3>Fuente y metodo</h3>
      <p>${escapeHtml(module.sourceDetail)}</p>
      <ul>
        ${module.methodology.map((item) => `<li>${escapeHtml(item)}</li>`).join("")}
      </ul>
      <div class="source-footer">
        <span>Fuente: ${escapeHtml(module.source)}</span>
        <span>Actualizado: ${escapeHtml(state.data.updated)}</span>
      </div>
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
    territory: state.territoryRegion,
    visual: state.visualMap
  }[scope];
  return `<button class="chart-toggle ${active === id ? "is-active" : ""}" type="button" data-scope="${scope}" data-metric="${id}">${escapeHtml(label)}</button>`;
}

function hydrateCharts(module) {
  els.stage.querySelectorAll(".chart-toggle").forEach((button) => {
    button.addEventListener("click", () => {
      const scope = button.dataset.scope;
      const metric = button.dataset.metric;
      if (scope === "macro") state.macroMetric = metric;
      if (scope === "trade") state.tradeMetric = metric;
      if (scope === "labor") state.laborMetric = metric;
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
    drawLineChart(document.getElementById("macro-chart"), labels, values, titles[state.macroMetric]);
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
    drawChoroplethMap(document.getElementById("territory-map"), territoryMapFeatures(), {
      title: "Republica Dominicana: densidad empresarial",
      valueField: "business_density",
      labelField: "province",
      fallbackLabel: "Sin dato"
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
  const mapConfig = {
    business: {
      features: territoryMapFeatures(),
      title: "Densidad empresarial por provincia",
      valueField: "business_density",
      labelField: "province",
      unit: "empresas por 1,000 hab.",
      colorStart: "#edf4f2",
      colorEnd: "#c86448",
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
      unit: "% microempresas",
      colorStart: "#eef1ed",
      colorEnd: "#6b7554",
      tooltipRows: [
        { field: "pct_informal", label: "Informalidad", suffix: "%" }
      ]
    },
    tourism: {
      features: state.worldGeojson?.features || [],
      title: "Preferencia por sol y playa segun pais de origen",
      valueField: "beach_pct",
      labelField: "country",
      unit: "% motivo playa",
      colorStart: "#f5f1e8",
      colorEnd: "#7a2e21",
      showLabels: false
    }
  }[state.visualMap];

  drawChoroplethMap(document.getElementById("visual-map"), mapConfig.features, mapConfig);

  drawTreemapChart(document.getElementById("tourism-treemap"), state.articleVisuals.tourism.treemap, {
    title: "Estructura de motivaciones turisticas",
    labelField: "motivo",
    valueField: "porcentaje",
    categoryField: "categoria"
  });

  drawComplexScatterChart(document.getElementById("transport-space"), state.articleVisuals.transport.rentEmployment, {
    title: "Prima de ubicacion vs empleo formal",
    xField: "median_rent_thousand",
    yField: "employment_share",
    sizeField: "jobs",
    labelField: "province",
    categoryField: "category",
    xLabel: "Alquiler mediano anual (RD$ miles)",
    yLabel: "% del empleo formal",
    sizeLabel: "Empleos",
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

function renderRail() {
  if (!state.data) return;
  const module = state.active === "overview" ? null : findModule(state.active);
  const title = module ? module.title : "Lectura rapida";
  const question = module ? module.question : "Que modulo quieres explorar?";
  const source = module ? module.source : "Catalogo modular";
  const insight = module ? module.insight : "Elige un modulo; el detalle aparece cuando el dato lo pide.";

  els.rail.innerHTML = `
    <p class="eyebrow">Criterio aplicado</p>
    <h2>${escapeHtml(title)}</h2>
    <p>${escapeHtml(question)}</p>
    <div class="reading-list">
      <div class="reading-item">
        <small>Lectura</small>
        <strong>${escapeHtml(insight)}</strong>
      </div>
      <div class="reading-item">
        <small>Fuente</small>
        <strong>${escapeHtml(source)}</strong>
      </div>
      <a class="reading-item" href="../productos.html">
        <small>Productos</small>
        <strong>Ver mapa de productos</strong>
      </a>
      <a class="reading-item" href="../republica-habla-de.html">
        <small>Analisis</small>
        <strong>Leer articulos relacionados</strong>
      </a>
    </div>
  `;
}

function visibleModules() {
  return state.data.modules.filter((module) => module.visible !== false && statusKey(module.status) === "activo");
}

function filteredModules() {
  return visibleModules().filter((module) => {
    const matchesFamily = state.family === "all" || statusKey(module.family) === statusKey(state.family);
    const text = `${module.title} ${module.topic} ${module.type} ${module.summary} ${module.source}`.toLowerCase();
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

function territoryMapRows() {
  return territoryMapFeatures()
    .map((feature) => feature.properties)
    .filter((item) => Number.isFinite(Number(item.business_density)))
    .sort((a, b) => Number(b.business_density) - Number(a.business_density));
}

function findModule(id) {
  return visibleModules().find((module) => module.id === id);
}

function setActive(moduleId) {
  state.active = moduleId;
  if (moduleId === "overview") {
    history.replaceState(null, "", window.location.pathname);
  } else {
    history.replaceState(null, "", `#${moduleId}`);
  }
  document.body.classList.remove("sidebar-open");
  els.menuToggle.setAttribute("aria-expanded", "false");
  renderNavigation();
  renderStage();
  renderRail();
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

function drawLineChart(canvas, labels, values, title) {
  drawDualLineChart(canvas, labels, [{ label: title, values, color: "#466a8f" }], title);
}

function drawDualLineChart(canvas, labels, series, title) {
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

  series.forEach((serie) => {
    const points = serie.values.map((value, index) => ({
      x: padding.left + (plotW * index) / Math.max(serie.values.length - 1, 1),
      y: padding.top + plotH - ((value - min) / span) * plotH,
      value
    }));

    ctx.beginPath();
    points.forEach((point, index) => {
      if (index === 0) ctx.moveTo(point.x, point.y);
      else ctx.lineTo(point.x, point.y);
    });
    ctx.strokeStyle = serie.color;
    ctx.lineWidth = 3;
    ctx.stroke();

    points.forEach((point) => {
      ctx.beginPath();
      ctx.arc(point.x, point.y, 4, 0, Math.PI * 2);
      ctx.fillStyle = serie.color;
      ctx.fill();
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
    ctx.fillStyle = "#191b1f";
    ctx.font = "700 12px Inter";
    ctx.fillText(formatNumber(value), padding.left + barW + 8, y + barH - 2);
  });
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

  const xScale = (value) => padding.left + ((value - xMin) / (xMax - xMin || 1)) * plotW;
  const yScale = (value) => padding.top + plotH - ((value - yMin) / (yMax - yMin || 1)) * plotH;
  const xRefPos = xScale(xRef);
  const yRefPos = yScale(yRef);
  const points = [];

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

      if (index < (options.labelCount || 5)) {
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
  const padding = { top: 44, right: 34, bottom: 54, left: 34 };
  const coordinates = collectGeoCoordinates(features);
  const values = features
    .map((feature) => Number(feature.properties[options.valueField]))
    .filter(Number.isFinite);

  if (!coordinates.length || !values.length) {
    clearCanvas(ctx, width, height);
    drawCanvasTitle(ctx, options.title, padding.left, 22);
    ctx.fillStyle = "#6b7280";
    ctx.font = "13px Inter";
    ctx.fillText("Mapa pendiente de datos geograficos publicables.", padding.left, height / 2);
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

  const paint = (highlightEntry = null) => {
    clearCanvas(ctx, width, height);
    drawCanvasTitle(ctx, options.title, padding.left, 22);
    entries.forEach((entry) => {
      ctx.fillStyle = Number.isFinite(entry.value)
        ? interpolateColor(options.colorStart || "#edf4f2", options.colorEnd || "#c86448", normalizeRatio(entry.value, min, max))
        : "#f1f1ee";
      ctx.fill(entry.path);
      ctx.strokeStyle = "#ffffff";
      ctx.lineWidth = 0.9;
      ctx.stroke(entry.path);
    });

    if (highlightEntry) {
      ctx.strokeStyle = "#191b1f";
      ctx.lineWidth = 2.4;
      ctx.stroke(highlightEntry.path);
    }

    if (options.showLabels !== false) drawMapLabels(ctx, entries, options, max);
    drawMapLegend(ctx, min, max, width, height, padding, options);
  };

  paint();

  canvas.onmousemove = (event) => {
    const point = getCanvasPoint(canvas, event);
    const found = entries.find((entry) => ctx.isPointInPath(entry.path, point.x, point.y));
    if (!found) {
      paint();
      hideTooltip();
      return;
    }
    paint(found);
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
    `, event);
  };

  canvas.onmouseleave = () => {
    paint();
    hideTooltip();
  };
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
  canvas.onmousemove = (event) => {
    const cursor = getCanvasPoint(canvas, event);
    const found = points.find((point) => {
      const dx = cursor.x - point.x;
      const dy = cursor.y - point.y;
      return Math.sqrt(dx * dx + dy * dy) <= point.radius + 2;
    });

    if (!found) {
      hideTooltip();
      return;
    }

    showTooltip(content(found), event);
  };

  canvas.onmouseleave = hideTooltip;
}

function bindBoxTooltip(canvas, boxes, content) {
  canvas.onmousemove = (event) => {
    const cursor = getCanvasPoint(canvas, event);
    const found = boxes.find((box) => (
      cursor.x >= box.x &&
      cursor.x <= box.x + box.width &&
      cursor.y >= box.y &&
      cursor.y <= box.y + box.height
    ));

    if (!found) {
      hideTooltip();
      return;
    }

    showTooltip(content(found), event);
  };

  canvas.onmouseleave = hideTooltip;
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
  const legendW = Math.min(240, width - padding.left - padding.right);
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
  ctx.font = "11px Inter";
  ctx.fillText(formatNumber(min), x, y + 26);
  ctx.fillText(formatNumber(max), x + legendW - 34, y + 26);
  if (options.unit) ctx.fillText(options.unit, x + legendW + 12, y + 9);
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
  return {
    x: event.clientX - rect.left,
    y: event.clientY - rect.top
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

function showTooltip(html, event) {
  if (!state.tooltip) state.tooltip = ensureTooltip();
  state.tooltip.innerHTML = html;
  state.tooltip.style.opacity = "1";
  state.tooltip.style.transform = "translateY(0)";
  state.tooltip.style.left = `${Math.min(window.innerWidth - 260, event.clientX + 14)}px`;
  state.tooltip.style.top = `${Math.min(window.innerHeight - 120, event.clientY + 14)}px`;
}

function hideTooltip() {
  if (!state.tooltip) return;
  state.tooltip.style.opacity = "0";
  state.tooltip.style.transform = "translateY(4px)";
}

function setupCanvas(canvas) {
  const rect = canvas.getBoundingClientRect();
  const dpr = window.devicePixelRatio || 1;
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
