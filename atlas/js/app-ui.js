function syncFilterState() {
  const families = new Set(state.data ? visibleModules().map(module => statusKey(module.family)) : []);
  if (state.data) {
    const familyAvailable = state.family === "all" || families.has(statusKey(state.family));
    if (!familyAvailable) state.family = "all";
  }

  document.querySelectorAll(".filter-pill").forEach((item) => {
    if (state.data) {
      const available = item.dataset.filter === "all" || families.has(statusKey(item.dataset.filter));
      item.hidden = !available;
    }
    const active = item.dataset.filter === state.family;
    item.classList.toggle("is-active", active);
    item.setAttribute("aria-pressed", String(active));
  });
}

function syncSearchState() {
  if (!state.data || !els.resultCount) return;
  const total = visibleModules().length;
  const { filtered, openOutsideFilters } = navigationModules();
  const hasQuery = Boolean(state.query);
  if (els.searchClear) els.searchClear.hidden = !hasQuery;
  const countText = !hasQuery && state.family === "all" ? `${total} vistas` : `${filtered.length} de ${total} vistas`;
  els.resultCount.textContent = openOutsideFilters ? `${countText}. Abierta fuera del filtro.` : countText;
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
  // En móvil, el contenido queda inerte para que Tab no salte detrás del menú.
  sidebarReturnFocus = document.activeElement instanceof HTMLElement ? document.activeElement : null;
  document.body.classList.add("sidebar-open", "no-scroll");
  els.menuToggle.setAttribute("aria-expanded", "true");
  if (els.sidebarBackdrop) els.sidebarBackdrop.hidden = false;
  if (els.main) els.main.inert = true;
  if (window.matchMedia("(max-width: 920px)").matches) {
    window.requestAnimationFrame(() => {
      const target = els.search || els.sidebar.querySelector("button, a, input");
      if (target) target.focus();
    });
  }
}

function closeSidebar({ restoreFocus = false } = {}) {
  document.body.classList.remove("sidebar-open", "no-scroll");
  els.menuToggle.setAttribute("aria-expanded", "false");
  if (els.sidebarBackdrop) els.sidebarBackdrop.hidden = true;
  if (els.main) els.main.inert = false;
  if (restoreFocus && sidebarReturnFocus) {
    sidebarReturnFocus.focus();
  }
}

function slugify(value) {
  return normalizeText(value)
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-|-$/g, "");
}

initAtlasTheme();
boot();
