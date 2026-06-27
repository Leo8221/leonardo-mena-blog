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
    card.addEventListener("click", () => setActive(card.dataset.module, { source: "metric-strip" }));
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
      <span>Ajusta la búsqueda o limpia los filtros.</span>
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
    button.addEventListener("click", () => setActive(button.dataset.module, { source: "sidebar" }));
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
      button.addEventListener("click", () => setActive(button.dataset.module, { source: "mobile-nav" }));
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
  if (els.stage) els.stage.setAttribute("aria-busy", "false");
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
    if (els.stage) els.stage.setAttribute("aria-busy", "true");
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
  announceModule(module);
  updateDocumentTitle();
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
    button.addEventListener("click", () => setActive(button.dataset.module, { source: "overview" }));
  });
  hydrateModuleActions();
  announceModule(null);
  updateDocumentTitle();
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
          <strong>Lectura</strong>
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
      <span>Los mapas cargan cuando hacen falta.</span>
    </div>
  `;
  hydrateModuleActions();
}

function renderEmptyOverview() {
  return `
    <div class="empty-state">
      <strong>No hay módulos con esos filtros.</strong>
      <span>Prueba otra búsqueda o limpia los filtros.</span>
      <button class="stage-action" type="button" data-action="reset-filters">Restablecer filtros</button>
    </div>
  `;
}
