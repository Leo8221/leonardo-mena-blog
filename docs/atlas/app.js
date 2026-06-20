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
  mapPinned: {}
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
  sidebar: document.getElementById("atlas-sidebar")
};

let sidebarReturnFocus = null;

const OVERVIEW_GROUPS = [
  {
    title: "¿Qué está pasando ahora?",
    summary: "Coyuntura, precios y señales externas para ubicar el momento económico.",
    modules: ["pulso-macro", "contexto-externo", "costo-vida"]
  },
  {
    title: "¿Dónde están las oportunidades y restricciones?",
    summary: "Comercio, sectores y territorio para leer capacidad productiva y cuellos de botella.",
    modules: ["comercio-exterior", "sectores", "territorio-infraestructura"]
  },
  {
    title: "¿Cómo se traduce en la vida económica?",
    summary: "Trabajo, MiPyMES y visuales de artículos para conectar datos con experiencias concretas.",
    modules: ["mercado-laboral", "mipymes-productividad", "laboratorio-visual"]
  }
];

async function boot() {
  try {
    const atlasResponse = await fetch("data/atlas-data.json", { cache: "no-store" });
    if (!atlasResponse.ok) throw new Error(`HTTP ${atlasResponse.status}`);
    state.data = await atlasResponse.json();
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
  syncFilterState();
  render();
}

function bindEvents() {
  els.search.addEventListener("input", (event) => {
    state.query = event.target.value.trim().toLowerCase();
    syncSearchState();
    renderNavigation();
    renderMobileNavigation();
    renderOverviewIfActive();
  });

  els.searchClear.addEventListener("click", () => {
    state.query = "";
    els.search.value = "";
    syncSearchState();
    renderNavigation();
    renderMobileNavigation();
    renderOverviewIfActive();
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
  syncFilterState();
  syncSearchState();
  renderMetrics();
  renderNavigation();
  renderMobileNavigation();
  renderStage();
}

function renderMetrics() {
  els.metricStrip.innerHTML = state.data.metrics.map((metric) => `
    <button class="metric-card ${state.active === metric.module ? "is-active" : ""}" type="button" data-module="${metric.module}" data-tone="${metric.tone}" aria-label="${escapeHtml(`${metric.label}: ${metric.value}. ${metric.delta}. ${metric.meta || ""}`)}"${state.active === metric.module ? ' aria-current="page"' : ""}>
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

function navigationModules() {
  const filtered = filteredModules();
  const activeModule = state.active !== "overview" ? findModule(state.active) : null;
  const openOutsideFilters = Boolean(activeModule && !filtered.some((module) => module.id === activeModule.id));
  return {
    filtered,
    openOutsideFilters,
    modules: openOutsideFilters ? [activeModule, ...filtered] : filtered
  };
}

function renderNavEmpty() {
  return `
    <div class="nav-empty">
      <strong>Sin coincidencias</strong>
      <span>Ajusta la búsqueda o vuelve al Atlas completo.</span>
      <button class="nav-reset" type="button" data-action="reset-filters">Ver todo</button>
    </div>
  `;
}

function renderNavigation() {
  const nav = navigationModules();
  const buttons = [
    `<button class="module-button ${state.active === "overview" ? "is-active" : ""}" type="button" data-module="overview"${state.active === "overview" ? ' aria-current="page"' : ""}>
      <small>Atlas</small>
      <strong>Portada</strong>
      <span>${visibleModules().length} vistas</span>
    </button>`,
    ...nav.modules.map((module) => {
      const outsideFilter = nav.openOutsideFilters && module.id === state.active;
      return `
      <button class="module-button ${state.active === module.id ? "is-active" : ""} ${outsideFilter ? "is-outside-filter" : ""}" type="button" data-module="${module.id}"${state.active === module.id ? ' aria-current="page"' : ""}>
        <small>${escapeHtml(module.family)}</small>
        <strong>${escapeHtml(module.title)}</strong>
        <span>${escapeHtml(outsideFilter ? "Vista abierta" : module.topic)}</span>
      </button>
    `;
    })
  ];
  const empty = nav.filtered.length === 0 ? renderNavEmpty() : "";

  els.nav.innerHTML = buttons.join("") + empty;
  els.nav.querySelectorAll(".module-button").forEach((button) => {
    button.addEventListener("click", () => setActive(button.dataset.module));
  });
  bindResetFilterButtons(els.nav);
}

function renderMobileNavigation() {
  if (!els.mobileNav) return;
  const nav = navigationModules();
  const buttons = [
    `<button class="mobile-module-card ${state.active === "overview" ? "is-active" : ""}" type="button" data-module="overview"${state.active === "overview" ? ' aria-current="page"' : ""}>
      <small>Atlas</small>
      <strong>Portada</strong>
    </button>`,
    ...nav.modules.map((module) => {
      const outsideFilter = nav.openOutsideFilters && module.id === state.active;
      return `
      <button class="mobile-module-card ${state.active === module.id ? "is-active" : ""} ${outsideFilter ? "is-outside-filter" : ""}" type="button" data-module="${module.id}"${state.active === module.id ? ' aria-current="page"' : ""}>
        <small>${escapeHtml(module.family)}</small>
        <strong>${escapeHtml(outsideFilter ? `${module.title} · abierta` : module.title)}</strong>
      </button>
    `;
    }),
    nav.filtered.length === 0 ? `
      <button class="mobile-module-card is-reset" type="button" data-action="reset-filters">
        <small>Filtro</small>
        <strong>Ver todo</strong>
      </button>
    ` : ""
  ];

  els.mobileNav.innerHTML = buttons.join("");
  els.mobileNav.querySelectorAll(".mobile-module-card").forEach((button) => {
    if (button.dataset.module) {
      button.addEventListener("click", () => setActive(button.dataset.module));
    }
  });
  bindResetFilterButtons(els.mobileNav);
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
  if (els.app) {
    els.app.dataset.view = state.active === "overview" ? "overview" : "module";
  }

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

  if (!moduleAssetsReady(module)) {
    renderAssetsLoading(module);
    ensureModuleAssets(module);
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
  const groupedIds = new Set(OVERVIEW_GROUPS.flatMap((group) => group.modules));
  const grouped = OVERVIEW_GROUPS.map((group) => {
    return {
      ...group,
      modules: modules.filter((module) => group.modules.includes(module.id))
    };
  }).filter((group) => group.modules.length > 0);
  const ungrouped = modules.filter((module) => !groupedIds.has(module.id));
  if (ungrouped.length > 0) {
    grouped.push({
      title: "Otras vistas",
      summary: "Módulos activos todavía no asignados a una pregunta principal.",
      modules: ungrouped
    });
  }

  els.stage.innerHTML = `
    <div class="stage-header">
      <div>
        <p class="eyebrow">${escapeHtml(state.data.brand.shortName)}</p>
        <h2>Atlas</h2>
        <p>Explora los datos desde preguntas, no desde una lista plana de gráficos.</p>
      </div>
      ${renderStageActions(false)}
    </div>
    ${modules.length ? `
      <div class="overview-shell">
        ${grouped.map(renderOverviewGroup).join("")}
      </div>
    ` : renderEmptyOverview()}
  `;

  els.stage.querySelectorAll(".module-card").forEach((button) => {
    button.addEventListener("click", () => setActive(button.dataset.module));
  });
  hydrateModuleActions();
}

function renderOverviewGroup(group) {
  return `
    <section class="overview-group" aria-labelledby="overview-${slugify(group.title)}">
      <div class="overview-group-head">
        <h3 id="overview-${slugify(group.title)}">${escapeHtml(group.title)}</h3>
        <p>${escapeHtml(group.summary)}</p>
      </div>
      <div class="module-grid">
        ${group.modules.map(renderOverviewCard).join("")}
      </div>
    </section>
  `;
}

function renderOverviewCard(module) {
  const source = module.sourceInfo || {};
  return `
    <button class="module-card" type="button" data-module="${module.id}">
      <h3>${escapeHtml(module.title)}</h3>
      ${module.question ? `<p class="module-question">${escapeHtml(module.question)}</p>` : ""}
      ${module.insight ? `
        <span class="module-reading">
          <strong>La lectura</strong>
          <span>${escapeHtml(module.insight)}</span>
        </span>
      ` : ""}
      <span class="module-card-footer">
        <span>${escapeHtml(source.label || module.source || "Fuente")}</span>
        <span>${source.updated ? `Corte ${escapeHtml(source.updated)}` : "Abrir"}</span>
      </span>
    </button>
  `;
}

function renderAssetsLoading(module) {
  els.stage.innerHTML = `
    <div class="stage-header">
      <div>
        <p class="eyebrow">${escapeHtml(module.family)} / ${escapeHtml(module.topic)}</p>
        <h2>${escapeHtml(module.title)}</h2>
      </div>
      ${renderStageActions(false)}
    </div>
    <div class="empty-state">
      <strong>Cargando datos de esta vista</strong>
      <span>El Atlas carga mapas y visuales pesados solo cuando se abre el módulo.</span>
    </div>
  `;
  hydrateModuleActions();
}

function renderEmptyOverview() {
  return `
    <div class="empty-state">
      <strong>No hay módulos con esos filtros.</strong>
      <span>Prueba otra búsqueda o vuelve a ver todo el Atlas.</span>
      <button class="stage-action" type="button" data-action="reset-filters">Restablecer filtros</button>
    </div>
  `;
}

function renderStageActions(canExport) {
  return `
    <div class="stage-actions">
      <button class="stage-action" type="button" data-action="copy-link" title="Copiar enlace de esta vista">Enlace</button>
      ${canExport ? `<button class="stage-action" type="button" data-action="export-chart" title="Descargar gráfico visible">PNG</button>` : ""}
      ${canExport ? `<button class="stage-action" type="button" data-action="export-csv" title="Descargar datos de esta vista">CSV</button>` : ""}
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
    ${renderModuleBrief(module)}
    <div class="chart-layout">
      ${body}
      ${renderSourceCard(module)}
    </div>
    ${renderDataAppendix(module)}
  `;
}

function renderModuleBrief(module) {
  return `
    <section class="module-brief" aria-label="Lectura del módulo">
      ${module.question ? `<p class="module-brief-question">${escapeHtml(module.question)}</p>` : ""}
      ${module.summary ? `<p>${escapeHtml(module.summary)}</p>` : ""}
      ${module.insight ? `
        <p class="module-brief-reading">
          <strong>La lectura</strong>
          <span>${escapeHtml(module.insight)}</span>
        </p>
      ` : ""}
    </section>
  `;
}

function renderSourceCard(module) {
  const info = module.sourceInfo;
  if (!info) return "";

  const sourceLabel = info.label || module.source || "Fuente";
  const methods = Array.isArray(info.methodology) ? info.methodology.filter(Boolean) : [];
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
        ${state.data.generatedAt ? `<div><dt>Generado</dt><dd>${escapeHtml(state.data.generatedAt.slice(0, 10))}</dd></div>` : ""}
      </dl>
      ${methods.length ? `
        <details class="source-method">
          <summary>Metodología</summary>
          <ul>
            ${methods.map((item) => `<li>${escapeHtml(item)}</li>`).join("")}
          </ul>
        </details>
      ` : ""}
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

function renderDataAppendix(module) {
  const datasets = moduleDatasets(module).filter((dataset) => dataset.rows.length > 0);
  if (!datasets.length) return "";

  return `
    <section class="data-appendix" aria-label="Datos tabulares del módulo">
      <div class="data-appendix-head">
        <div>
          <span>Datos</span>
          <h3>Vista tabular</h3>
        </div>
        <button class="stage-action" type="button" data-action="export-csv">CSV completo</button>
      </div>
      <div class="data-table-grid">
        ${datasets.map((dataset, index) => renderDatasetCard(module, dataset, index)).join("")}
      </div>
    </section>
  `;
}

function renderDatasetCard(module, dataset, index) {
  const rows = dataset.rows;
  const columns = datasetColumns(dataset);
  const visibleRows = rows.slice(0, dataset.visibleRows || 12);
  const remaining = rows.length - visibleRows.length;
  return `
    <details class="data-table-card" ${index === 0 ? "open" : ""}>
      <summary>
        <span>
          <strong>${escapeHtml(dataset.title)}</strong>
          ${dataset.note ? `<small>${escapeHtml(dataset.note)}</small>` : ""}
        </span>
      </summary>
      <div class="data-table-actions">
        <button class="data-download" type="button" data-action="download-dataset" data-dataset="${escapeHtml(dataset.id)}">CSV</button>
      </div>
      <div class="data-table-wrap">
        <table>
          <caption class="sr-only">${escapeHtml(module.title)}: ${escapeHtml(dataset.title)}</caption>
          <thead>
            <tr>${columns.map((column) => `<th scope="col">${escapeHtml(column.label)}</th>`).join("")}</tr>
          </thead>
          <tbody>
            ${visibleRows.map((row) => `
              <tr>
                ${columns.map((column) => `<td>${escapeHtml(formatTableCell(row[column.field]))}</td>`).join("")}
              </tr>
            `).join("")}
          </tbody>
        </table>
      </div>
      <p class="data-table-note">
        ${visibleRows.length} de ${rows.length} filas${remaining > 0 ? ". Descarga el CSV para ver el conjunto completo." : "."}
      </p>
    </details>
  `;
}

function moduleDatasets(module) {
  const series = state.data.series;
  const geoRows = (features, fields) => {
    return (features || []).map((feature) => pickFields(feature.properties || {}, fields));
  };

  const datasets = {
    macro: () => [
      dataset("macro", "Indicadores macro", series.macro, [
        col("period", "Periodo"),
        col("dolar", "Dólar"),
        col("inflacion", "Inflación"),
        col("imae", "IMAE"),
        col("tpm", "TPM")
      ], `Serie activa: ${macroMetricLabel()}`)
    ],
    external: () => [
      dataset("presion-externa", "Presión externa", series.external, [
        col("period", "Periodo"),
        col("pressure", "Índice")
      ]),
      dataset("drivers-externos", "Drivers externos", series.drivers, [
        col("driver", "Driver"),
        col("value", "Valor")
      ])
    ],
    sectors: () => [
      dataset("sectores", "Sensibilidad sectorial", series.sectors, [
        col("sector", "Sector"),
        col("pressure", "Presión"),
        col("driver", "Driver"),
        col("direction", "Señal")
      ])
    ],
    trade: () => [
      dataset("socios", "Socios comerciales", series.trade.partners, [
        col("name", "Socio"),
        col("exports", "Exporta"),
        col("imports", "Importa"),
        col("balance", "Balance"),
        col("opportunity", "Oportunidad")
      ], `Ranking activo: ${tradeMetricLabel()}`),
      dataset("canasta", "Canasta exportadora", series.trade.products, [
        col("name", "Producto"),
        col("share", "Share"),
        col("complexity", "Complejidad"),
        col("signal", "Señal")
      ]),
      dataset("flujos", "Flujos recientes", series.trade.flows, [
        col("period", "Periodo"),
        col("exports", "Exporta"),
        col("imports", "Importa")
      ])
    ],
    labor: () => [
      dataset("resultados", "Resultados laborales", series.labor.outcomes, [
        col("group", "Grupo"),
        col("employment", "Empleo"),
        col("informality", "Informalidad"),
        col("wageIndex", "Salario")
      ], `Métrica activa: ${laborMetricLabel()}`),
      dataset("sectores", "Empleo por sector", series.labor.sectors, [
        col("name", "Sector"),
        col("jobs", "Empleo"),
        col("wageIndex", "Salario")
      ]),
      dataset("tendencia", "Tendencia laboral", series.labor.trend, [
        col("period", "Periodo"),
        col("employment", "Empleo"),
        col("realWage", "Salario real")
      ])
    ],
    prices: () => [
      dataset("inflacion", "Inflación", series.prices.timeline, [
        col("period", "Periodo"),
        col("headline", "General"),
        col("core", "Subyacente")
      ]),
      dataset("rubros", "Rubros", series.prices.components, [
        col("component", "Rubro"),
        col("contribution", "Contribución"),
        col("pressure", "Presión")
      ]),
      dataset("traspaso", "Canales de traspaso", series.prices.passThrough, [
        col("channel", "Canal"),
        col("value", "Valor"),
        col("note", "Nota")
      ])
    ],
    territory: () => [
      dataset("mapa", "Mapa territorial", territoryMapRows().map((row) => pickFields(row, [
        "province", "business_density", "opportunity", "businesses", "population"
      ])), [
        col("province", "Provincia"),
        col("business_density", "Densidad"),
        col("opportunity", "Oportunidad"),
        col("businesses", "Empresas"),
        col("population", "Población")
      ], `Mapa activo: ${territoryMapMetricLabel()}`),
      dataset("provincias", "Provincias", territoryRows(), [
        col("province", "Provincia"),
        col("region", "Región"),
        col("opportunity", "Oportunidad"),
        col("infrastructure", "Infraestructura"),
        col("labor", "Trabajo"),
        col("market", "Mercado")
      ], `Región activa: ${state.territoryRegion === "all" ? "Todas" : state.territoryRegion}`)
    ],
    mipymes: () => [
      dataset("finanzas", "Acceso y productividad", series.mipymes.finance, [
        col("segment", "Segmento"),
        col("access", "Acceso"),
        col("formalization", "Formalidad"),
        col("productivity", "Productividad")
      ]),
      dataset("barreras", "Barreras principales", series.mipymes.barriers, [
        col("barrier", "Barrera"),
        col("value", "Valor")
      ]),
      dataset("escalera", "Escalera productiva", series.mipymes.ladder, [
        col("stage", "Etapa"),
        col("focus", "Foco"),
        col("score", "Puntaje")
      ])
    ],
    visualLab: () => {
      if (!state.articleVisuals) return [];
      const mapConfig = visualMapConfig();
      const mapRows = geoRows(mapConfig.features, visualMapFields());
      return [
        dataset("mapa-articulo", `Mapa: ${mapConfig.label}`, mapRows, visualMapColumns(), `Vista activa: ${visualMapLabel()}`),
        dataset("turismo-motivos", "Turismo por motivo", state.articleVisuals.tourism.treemap, [
          col("motivo", "Motivo"),
          col("porcentaje", "%"),
          col("categoria", "Categoría")
        ]),
        dataset("empleo-alquiler", "Empleo formal y alquiler", state.articleVisuals.transport.rentEmployment, [
          col("province", "Provincia"),
          col("category", "Categoría"),
          col("jobs", "Empleos"),
          col("employment_share", "Empleo formal"),
          col("median_rent_thousand", "Alquiler")
        ]),
        dataset("deuda", "Servicio de deuda", state.articleVisuals.debt.service, [
          col("anio", "Año"),
          col("principal", "Principal"),
          col("interest", "Intereses"),
          col("commissions", "Comisiones"),
          col("service", "Servicio"),
          col("interest_share", "Intereses / servicio")
        ])
      ];
    }
  }[module.chart];

  return datasets ? datasets() : [];
}

function dataset(id, title, rows, columns, note = "") {
  return {
    id,
    title,
    note,
    columns,
    rows: Array.isArray(rows) ? rows.filter(Boolean) : []
  };
}

function col(field, label) {
  return { field, label };
}

function datasetColumns(dataset) {
  if (Array.isArray(dataset.columns) && dataset.columns.length) return dataset.columns;
  const fields = new Set();
  dataset.rows.forEach((row) => Object.keys(row || {}).forEach((key) => fields.add(key)));
  return [...fields].map((field) => col(field, field));
}

function pickFields(row, fields) {
  return fields.reduce((result, field) => {
    result[field] = row?.[field];
    return result;
  }, {});
}

function formatTableCell(value) {
  if (value === null || value === undefined || value === "") return "—";
  if (typeof value === "number") return formatNumber(value);
  return String(value);
}

function macroMetricLabel() {
  return {
    dolar: "Dólar",
    inflacion: "Inflación",
    imae: "IMAE",
    tpm: "TPM"
  }[state.macroMetric] || state.macroMetric;
}

function tradeMetricLabel() {
  return {
    exports: "Exporta",
    imports: "Importa",
    opportunity: "Oportunidad",
    balance: "Balance"
  }[state.tradeMetric] || state.tradeMetric;
}

function laborMetricLabel() {
  return {
    employment: "Empleo",
    informality: "Informalidad",
    wageIndex: "Salario"
  }[state.laborMetric] || state.laborMetric;
}

function territoryMapMetricLabel() {
  return {
    business_density: "Densidad empresarial",
    opportunity: "Oportunidad"
  }[state.territoryMapMetric] || state.territoryMapMetric;
}

function visualMapLabel() {
  return {
    business: "Empresas",
    mipymes: "MiPyMES",
    tourism: "Turismo"
  }[state.visualMap] || state.visualMap;
}

function visualMapFields() {
  return {
    business: ["province", "business_density", "opportunity", "businesses", "population"],
    mipymes: ["region", "pct_micro", "pct_informal"],
    tourism: ["country", "beach_pct"]
  }[state.visualMap];
}

function visualMapColumns() {
  return {
    business: [
      col("province", "Provincia"),
      col("business_density", "Densidad"),
      col("opportunity", "Oportunidad"),
      col("businesses", "Empresas"),
      col("population", "Población")
    ],
    mipymes: [
      col("region", "Región"),
      col("pct_micro", "Microempresas"),
      col("pct_informal", "Informalidad")
    ],
    tourism: [
      col("country", "País"),
      col("beach_pct", "Sol y playa")
    ]
  }[state.visualMap];
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
          <h3>Inserción por educación</h3>
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
      <h3>Contribución por rubro</h3>
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
      <canvas id="tourism-treemap" height="340" aria-label="Treemap de motivos turísticos"></canvas>
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
        { field: "population", label: "Población" }
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

const ASSET_URLS = {
  geojson: "data/rd-provinces.geojson",
  regionGeojson: "data/rd-regions-mipymes.geojson",
  worldGeojson: "data/world-tourism.geojson",
  articleVisuals: "data/article-visuals.json"
};

function requiredAssets(module) {
  if (!module) return [];
  if (module.chart === "territory") return ["geojson"];
  if (module.chart === "visualLab") return ["articleVisuals", "geojson", "regionGeojson", "worldGeojson"];
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
  state.assetPromises[key] = fetch(ASSET_URLS[key], { cache: "no-store" })
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

function setActive(moduleId) {
  const next = moduleId === "overview" || findModule(moduleId) ? moduleId : "overview";
  state.active = next;
  if (next === "overview") {
    history.replaceState(null, "", window.location.pathname);
  } else {
    history.replaceState(null, "", `#${next}`);
  }
  closeSidebar({ restoreFocus: false });
  syncMetricState();
  syncSearchState();
  renderNavigation();
  renderMobileNavigation();
  renderStage();
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
  bindResetFilterButtons(els.stage);

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
        flashButton(button, "Sin gráfico");
        return;
      }
      const link = document.createElement("a");
      link.download = `atlas-${state.active}.png`;
      link.href = canvas.toDataURL("image/png");
      link.click();
      flashButton(button, "Descargado");
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
      flashButton(button, "OK");
    });
  });

  els.stage.querySelectorAll('[data-action="expand-chart"]').forEach((button) => {
    button.addEventListener("click", () => openChartFullscreen(button.dataset.canvas));
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

function syncFilterState() {
  document.querySelectorAll(".filter-pill").forEach((item) => {
    const active = item.dataset.filter === state.family;
    item.classList.toggle("is-active", active);
    item.setAttribute("aria-pressed", String(active));
  });
}

function syncSearchState() {
  if (!state.data || !els.resultCount) return;
  const total = visibleModules().length;
  const filtered = filteredModules();
  const activeModule = state.active !== "overview" ? findModule(state.active) : null;
  const activeOutsideFilters = Boolean(activeModule && !filtered.some((module) => module.id === activeModule.id));
  const hasQuery = Boolean(state.query);
  if (els.searchClear) els.searchClear.hidden = !hasQuery;
  const countText = !hasQuery && state.family === "all" ? `${total} vistas` : `${filtered.length} de ${total} vistas`;
  els.resultCount.textContent = activeOutsideFilters ? `${countText}. Abierta fuera del filtro.` : countText;
}

function syncMetricState() {
  if (!els.metricStrip) return;
  els.metricStrip.querySelectorAll(".metric-card").forEach((card) => {
    const active = card.dataset.module === state.active;
    card.classList.toggle("is-active", active);
    if (active) {
      card.setAttribute("aria-current", "page");
    } else {
      card.removeAttribute("aria-current");
    }
  });
}

function openSidebar() {
  sidebarReturnFocus = document.activeElement instanceof HTMLElement ? document.activeElement : null;
  document.body.classList.add("sidebar-open");
  els.menuToggle.setAttribute("aria-expanded", "true");
  if (window.matchMedia("(max-width: 920px)").matches) {
    window.requestAnimationFrame(() => {
      const target = els.search || els.sidebar.querySelector("button, a, input");
      if (target) target.focus();
    });
  }
}

function closeSidebar({ restoreFocus = false } = {}) {
  document.body.classList.remove("sidebar-open");
  els.menuToggle.setAttribute("aria-expanded", "false");
  if (restoreFocus && sidebarReturnFocus) {
    sidebarReturnFocus.focus();
  }
}

function slugify(value) {
  return normalizeText(value)
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-|-$/g, "");
}

boot();
