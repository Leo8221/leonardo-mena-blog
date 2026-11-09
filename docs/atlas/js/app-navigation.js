function renderMetrics() {
  const targets = { usd: "dolar", inflation: "inflacion", trade: "exports", labor: "informality" };
  els.metricStrip.innerHTML = state.data.metrics.filter(metric => findModule(metric.module)).map(metric => `
    <button class="metric-card" type="button" data-module="${metric.module}" data-metric="${targets[metric.id] || ""}">
      <small>${escapeHtml(metric.label)}</small><strong>${escapeHtml(metric.value)}</strong>
      <span>${escapeHtml(metric.delta)}</span><em>${escapeHtml(metric.meta || "")}</em>
    </button>`).join("");
}

function renderNavigation() {
  // Los filtros pertenecen al catálogo; el selector conserva todas las vistas.
  els.moduleSwitch.innerHTML = visibleModules().map(module => `<option value="${module.id}">${escapeHtml(module.title)}</option>`).join("");
  els.moduleSwitch.value = state.active;
  els.viewNavigation.querySelector(".back-to-catalog").textContent = state.query || state.family !== "all"
    ? "← Volver a los resultados" : "← Todas las vistas";
}

function renderStage() {
  // stage es el área central: portada, espera o módulo. El HTML se reemplaza;
  // por eso los eventos comunes se escuchan en el contenedor, no en cada tarjeta.
  if (!state.data) return;
  hideTooltip(true);
  const overview = state.active === "overview";
  els.app.dataset.view = overview ? "overview" : "module";
  els.intro.hidden = !overview;
  els.catalog.hidden = !overview;
  els.snapshot.hidden = !overview;
  els.viewNavigation.hidden = overview;
  els.stage.setAttribute("aria-busy", "false");
  if (overview) { renderOverview(); return; }
  const module = findModule(state.active);
  if (!module) { setActive("overview", { replace: true }); return; }
  if (!moduleAssetsReady(module)) {
    els.stage.setAttribute("aria-busy", "true");
    renderAssetsLoading(module);
    ensureModuleAssets(module);
    return;
  }
  els.stage.innerHTML = `
    ${renderModuleHeader(module, true)}
    <nav class="view-sections" aria-label="En esta vista">
      <a href="#view-charts">Gráficos</a><a href="#view-data">Datos</a><a href="#view-sources">Fuentes y método</a>
    </nav>
    ${renderModuleBody(module)}`;
  ensureCanvasDownloadActions();
  // Los canvas deben existir y tener tamaño antes de que hydrateCharts los dibuje.
  hydrateCharts(module);
  announceModule(module);
  updateDocumentTitle();
  focusStageHeading();
}

function renderModuleHeader(module, canExport) {
  const source = module.sourceInfo || {};
  return `<div class="stage-header"><div>
    <p class="eyebrow">${escapeHtml(module.family)} · ${escapeHtml(module.type)}</p>
    <h1 id="view-heading" tabindex="-1">${escapeHtml(module.title)}</h1>
    <p class="view-provenance">${escapeHtml(source.label || module.source || "")}${source.updated ? ` · Corte: ${escapeHtml(source.updated)}` : ""}</p>
    </div>${renderStageActions(canExport)}</div>`;
}

function renderOverview() {
  const modules = filteredModules();
  const groupedIds = new Set(OVERVIEW_GROUPS.flatMap(group => group.modules));
  const groups = OVERVIEW_GROUPS.map(group => ({
    ...group, modules: modules.filter(module => group.modules.includes(module.id))
  })).filter(group => group.modules.length);
  const other = modules.filter(module => !groupedIds.has(module.id));
  if (other.length) groups.push({ title: "Más datos", summary: "Otras vistas disponibles.", modules: other });
  els.stage.innerHTML = modules.length ? `<div class="overview-shell">${groups.map(group => `
    <section class="overview-group" aria-labelledby="overview-${slugify(group.title)}">
      <div class="overview-group-head"><h2 id="overview-${slugify(group.title)}">${escapeHtml(group.title)}</h2><p>${escapeHtml(group.summary)}</p></div>
      <div class="module-grid">${group.modules.map(renderOverviewCard).join("")}</div>
    </section>`).join("")}</div>` : renderEmptyOverview();
  renderFeaturedSeries();
  announceModule(null);
  updateDocumentTitle();
  focusStageHeading();
}

function renderOverviewCard(module) {
  // module.chart coincide con el nombre del SVG en atlas/icons. Esas imágenes
  // se publican como recursos de Quarto y toman el color desde la máscara CSS.
  const source = module.sourceInfo || {};
  return `<a class="module-card" href="?view=${module.id}" data-module="${module.id}">
    <span class="module-card-top"><span class="module-icon" style="mask-image:url('icons/${escapeHtml(module.chart)}.svg')" aria-hidden="true"></span><span>${escapeHtml(module.type)}</span><span aria-hidden="true">↗</span></span>
    <h3>${escapeHtml(module.title)}</h3><p>${escapeHtml(module.summary || module.question || "")}</p>
    <span class="module-card-footer"><span>${escapeHtml(source.label || module.source || "")}</span>${source.updated ? `<span>${escapeHtml(source.updated)}</span>` : ""}</span>
  </a>`;
}

function renderFeaturedSeries() {
  const module = findModule("pulso-macro");
  const rows = state.data.series.macro || [];
  if (!module || !rows.length) { els.feature.hidden = true; return; }
  els.feature.hidden = false;
  const last = rows.at(-1);
  els.feature.innerHTML = `
    <div class="feature-heading"><div><span>Tipo de cambio</span><strong>RD$ ${new Intl.NumberFormat("es-DO", { minimumFractionDigits: 2, maximumFractionDigits: 2 }).format(last.dolar)}</strong></div><span>${escapeHtml(last.period)}</span></div>
    <canvas id="overview-series" height="190" role="img" aria-label="Tipo de cambio: pesos dominicanos por dólar, de ${escapeHtml(rows[0].period)} a ${escapeHtml(last.period)}"></canvas>
    <div class="feature-footer"><span>Venta spot · BCRD</span><a href="?view=pulso-macro&metric=dolar" data-module="pulso-macro" data-metric="dolar">Explorar la serie →</a></div>`;
  drawLineChart(document.getElementById("overview-series"), rows.map(row => row.period), rows.map(row => row.dolar), "RD$ por US$", { maxLabels: 3 });
}

function renderAssetsLoading(module) {
  els.stage.innerHTML = `${renderModuleHeader(module, false)}<div class="empty-state" role="status"><strong>Cargando los datos de esta vista…</strong><span>Puedes cambiar de vista mientras se carga el mapa.</span></div>`;
}

function renderEmptyOverview() {
  return `<div class="empty-state"><h2>No encontramos una vista con esos filtros.</h2><p>Prueba otro término o vuelve a ver todos los temas.</p><button class="stage-action" type="button" data-action="reset-filters">Ver todas las vistas</button></div>`;
}

function focusStageHeading() {
  // También después de descargar un mapa: el botón anterior ya no está en el DOM.
  if (state.focusControl) {
    const { scope, metric } = state.focusControl;
    state.focusControl = null;
    els.stage.querySelector(`.chart-toggle[data-scope="${scope}"][data-metric="${metric}"]`)?.focus({ preventScroll: true });
  }
  if (!state.focusStage) return;
  state.focusStage = false;
  const target = state.active === "overview" ? els.search : document.getElementById("view-heading");
  target?.focus({ preventScroll: true });
  target?.scrollIntoView({ block: "start", behavior: "instant" });
}
