function renderStageActions(canExport) {
  return `
    <div class="stage-actions">
      <button class="stage-action" type="button" data-action="copy-link" title="Copiar enlace de esta vista">Copiar enlace</button>
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
      ${module.insight ? `
        <p class="module-brief-reading">
          <strong>Lectura</strong>
          <span>${escapeHtml(module.insight)}</span>
        </p>
      ` : ""}
      ${renderModuleGuide(module)}
    </section>
  `;
}

function renderModuleGuide(module) {
  const guide = module.readingGuide || MODULE_GUIDES[module.id];
  if (!guide) return "";
  const items = [
    ["Unidad", guide.unit],
    ["Alto", guide.high],
    ["Bajo", guide.low],
    ["Límite", guide.limit]
  ].filter(([, value]) => Boolean(value));

  return `
    <details class="module-guide-disclosure">
      <summary>Notas</summary>
      <dl class="module-guide" aria-label="Guía de lectura">
        ${items.map(([label, value]) => `
          <div>
            <dt>${escapeHtml(label)}</dt>
            <dd>${escapeHtml(value)}</dd>
          </div>
        `).join("")}
      </dl>
    </details>
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
    <aside class="source-card" aria-label="Fuente">
      <div class="source-card-main">
        <span>Fuente</span>
        <strong>${escapeHtml(sourceLabel)}</strong>
        ${info.detail ? `<p>${escapeHtml(info.detail)}</p>` : ""}
      </div>
      <dl>
        ${info.dataMode ? `<div><dt>Tipo</dt><dd>${escapeHtml(info.dataMode)}</dd></div>` : ""}
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
            <tr>${columns.map((column) => `<th scope="col"${isNumericColumn(visibleRows, column.field) ? ' data-type="number"' : ""}>${escapeHtml(column.label)}</th>`).join("")}</tr>
          </thead>
          <tbody>
            ${visibleRows.map((row) => `
              <tr>
                ${columns.map((column) => `<td${isNumericColumn(visibleRows, column.field) ? ' data-type="number"' : ""}>${escapeHtml(formatTableCell(row[column.field]))}</td>`).join("")}
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

function isNumericColumn(rows, field) {
  const sample = rows.map((row) => row[field]).filter((value) => value !== null && value !== undefined && value !== "");
  return sample.length > 0 && sample.every((value) => Number.isFinite(Number(value)));
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
        col("pressure", "Índice"),
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
        col("complexity", "Cobertura"),
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
        col("employment", "Empleos (MM)"),
        col("employers", "Empleadores (mil)")
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
