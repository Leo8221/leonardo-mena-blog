function syncFilterState() {
  const families = [...new Set(visibleModules().map(module => module.family))];
  const current = families.find(family => statusKey(family) === statusKey(state.family));
  state.family = current || "all";
  if (!els.family) return;
  els.family.innerHTML = `<option value="all">Todos los temas</option>` + families.map(family =>
    `<option value="${escapeHtml(family)}">${escapeHtml(family)}</option>`).join("");
  els.family.value = state.family;
}

function syncSearchState() {
  if (!state.data || !els.resultCount) return;
  const total = visibleModules().length;
  const filtered = Boolean(state.query) || state.family !== "all";
  els.searchClear.hidden = !state.query;
  els.catalogReset.hidden = !filtered;
  els.resultCount.textContent = filtered ? `${filteredModules().length} de ${total} vistas` : `${total} vistas para explorar`;
}

function slugify(value) {
  return normalizeText(value).replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
}

// Este es el último script de index.html: las funciones anteriores ya están
// disponibles. Aplicamos el tema guardado antes de cargar datos y dibujar canvas.
initAtlasTheme();
boot();
