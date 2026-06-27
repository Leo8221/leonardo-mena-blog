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
        <div class="chart-actions">${chartDownloadButton("macro-chart")}</div>
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
      <h3>Índice por sector</h3>
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
      <div class="card-head">
        <div>
          <h3>Flujos comerciales BCRD</h3>
        </div>
        <div class="chart-toolbar">
          ${chartToggle("trade", "exports", "Exporta")}
          ${chartToggle("trade", "imports", "Importa")}
        </div>
      </div>
      <canvas id="trade-chart" height="340" aria-label="Gráfico de flujos comerciales"></canvas>
    </section>
    <section class="chart-card">
      <h3>Canasta exportadora</h3>
      <div class="table-list">
        ${state.data.series.trade.products.map((item) => `
          <div class="table-row">
            <strong>${escapeHtml(item.name)}</strong>
            <span>${formatNumber(item.share)}% share</span>
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
      <h3>Indicadores ENCFT recientes</h3>
      <canvas id="labor-chart" height="340" aria-label="Indicadores laborales ENCFT"></canvas>
    </section>
    <section class="chart-card">
      <h3>Ocupados por rama</h3>
      <div class="driver-list">${renderBarRows(state.data.series.labor.sectors, {
        labelField: "name",
        valueField: "jobs",
        max: 30,
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
      <h3>Variación mensual por grupo</h3>
      <div class="contribution-list">${renderContributionRows(state.data.series.prices.components, {
        labelField: "component",
        valueField: "contribution",
        suffix: " p.p."
      })}</div>
    </section>
    ${state.data.series.prices.passThrough?.length ? `
      <section class="chart-card">
        <h3>Canales de segunda ronda</h3>
        <div class="table-list">
          ${state.data.series.prices.passThrough.map((item) => `
            <div class="table-row">
              <strong>${escapeHtml(item.channel)}</strong>
              <span>Índice ${formatNumber(item.value)}</span>
              <em>${escapeHtml(item.note)}</em>
            </div>
          `).join("")}
        </div>
      </section>
    ` : ""}
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

  const visualMapActions = `${articleLink(VISUAL_ARTICLES[state.visualMap])}${chartExpandButton("visual-map")}${chartDownloadButton("visual-map")}`;
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
        ${chartControls("", `${articleLink(VISUAL_ARTICLES.tourism)}${chartExpandButton("tourism-treemap")}${chartDownloadButton("tourism-treemap")}`)}
      </div>
      <canvas id="tourism-treemap" height="340" aria-label="Treemap de motivos turísticos"></canvas>
    </section>
    <section class="chart-card">
      <div class="card-head">
        <div>
          <h3>Empleo formal y alquiler</h3>
        </div>
        ${chartControls("", `${articleLink(VISUAL_ARTICLES.transport)}${chartExpandButton("transport-space")}${chartDownloadButton("transport-space")}`)}
      </div>
      <canvas id="transport-space" height="340" aria-label="Scatter de empleo formal y alquiler"></canvas>
    </section>
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Deuda pública</h3>
        </div>
        ${chartControls("", `${articleLink(VISUAL_ARTICLES.debt)}${chartExpandButton("debt-burden")}${chartDownloadButton("debt-burden")}`)}
      </div>
      <canvas id="debt-burden" height="360" aria-label="Rigidez fiscal e intereses"></canvas>
    </section>
    <section class="chart-card chart-card-wide">
      <div class="card-head">
        <div>
          <h3>Servicio de deuda</h3>
        </div>
        ${chartControls("", `${articleLink(VISUAL_ARTICLES.debt)}${chartExpandButton("debt-service")}${chartDownloadButton("debt-service")}`)}
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

function articleLink(articleId, label = "Artículo") {
  const href = ARTICLE_ROUTES[articleId];
  if (!href) return "";
  return `<a class="article-link" href="${escapeHtml(href)}">${escapeHtml(label)}</a>`;
}

function chartExpandButton(canvasId) {
  return `<button class="chart-expand" type="button" data-action="expand-chart" data-canvas="${escapeHtml(canvasId)}" title="Ver a pantalla completa">Ampliar</button>`;
}

function chartDownloadButton(canvasId) {
  return `<button class="chart-download" type="button" data-action="download-chart" data-canvas="${escapeHtml(canvasId)}" title="Descargar este grafico">Descargar PNG</button>`;
}

function chartEvents(scope, metric = null) {
  const events = state.data.series.events?.[scope] || [];
  return events.filter((event) => !Array.isArray(event.metrics) || !metric || event.metrics.includes(metric));
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
      updateUrlState({ replace: true });
      trackAtlasEvent(scope === "visual" || scope === "territoryMap" ? "atlas_map_selection" : "atlas_chart_toggle", {
        chart_id: scope,
        metric
      });
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
      stepped: state.macroMetric === "tpm",
      events: chartEvents("macro", state.macroMetric)
    });
  }

  if (module.chart === "external") {
    drawLineChart(
      document.getElementById("external-chart"),
      state.data.series.external.map((item) => item.period),
      state.data.series.external.map((item) => item.pressure),
      "Presión externa",
      { events: chartEvents("external") }
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
    const tradeMetric = ["exports", "imports"].includes(state.tradeMetric) ? state.tradeMetric : "exports";
    drawHorizontalBarChart(document.getElementById("trade-chart"), state.data.series.trade.flows, {
      labelField: "period",
      valueField: tradeMetric,
      title: "Comercio exterior total",
      max: null
    });
  }

  if (module.chart === "labor") {
    drawHorizontalBarChart(document.getElementById("labor-chart"), state.data.series.labor.indicators, {
      labelField: "group",
      valueField: "value",
      title: "Indicadores laborales (%)",
      max: 100
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
      "Inflación (%)",
      { events: chartEvents("prices") }
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
        { field: "population", label: "Población" }
      ]
    });
    drawScatterChart(document.getElementById("territory-chart"), territoryRows(), {
      title: "Infraestructura vs mercado",
      xField: "infrastructure",
      yField: "market",
      sizeField: "opportunity",
      labelField: "province",
      xLabel: "Infraestructura",
      yLabel: "Mercado"
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
      colorStart: "#f3eadb",
      colorEnd: "#c86448",
      inspectorId: "visual-map-inspector",
      mapId: "visual-business",
      tooltipRows: [
        { field: "businesses", label: "Empresas" },
        { field: "population", label: "Población" }
      ]
    },
    mipymes: {
      features: state.regionGeojson?.features || [],
      title: "Microempresas por región",
      valueField: "pct_micro",
      labelField: "region",
      label: "Microempresas",
      unit: "% microempresas",
      colorStart: "#d8c8a3",
      colorEnd: "#c86448",
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
      colorStart: "#d8c8a3",
      colorEnd: "#c86448",
      inspectorId: "visual-map-inspector",
      mapId: "visual-tourism",
      showLabels: false
    }
  }[state.visualMap];
}

function transportScatterOptions() {
  return {
    title: "Prima de ubicación vs empleo formal",
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
