function drawChoroplethMap(canvas, features, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const isCompact = width < 420;
  const padding = isCompact
    ? { top: 12, right: 8, bottom: 40, left: 8 }
    : { top: 34, right: 26, bottom: 50, left: 26 };
  const coordinates = collectGeoCoordinates(features);
  const values = features
    .map((feature) => Number(feature.properties[options.valueField]))
    .filter(Number.isFinite);

  if (!coordinates.length || !values.length) {
    clearCanvas(ctx, width, height);
    if (!isCompact) drawCanvasTitle(ctx, options.title, padding.left, 22);
    ctx.fillStyle = CHART_SYSTEM.colors.muted;
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
        ? interpolateColor(options.colorStart || CHART_SYSTEM.colors.olive, options.colorEnd || CHART_SYSTEM.colors.terracotta, normalizeRatio(entry.value, min, max))
        : CHART_SYSTEM.colors.panel;
      ctx.fill(entry.path);
      ctx.strokeStyle = addAlpha("#ffffff", 0.74);
      ctx.lineWidth = width < 520 ? 0.8 : 1;
      ctx.stroke(entry.path);
    });

    if (highlightEntry) {
      ctx.save();
      ctx.fillStyle = "rgba(255, 255, 255, 0.12)";
      ctx.fill(highlightEntry.path);
      ctx.restore();
      ctx.strokeStyle = addAlpha(CHART_SYSTEM.colors.ink, 0.62);
      ctx.lineWidth = 1.8;
      ctx.stroke(highlightEntry.path);
    }

    if (options.showLabels !== false) drawMapLabels(ctx, entries, options, min, max);
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
    let found = entries.find((entry) => ctx.isPointInPath(entry.path, point.x, point.y));
    if (!found && touchLikeEvent(event)) {
      const radius = Math.max(28, Math.min(54, width * 0.1));
      found = entries
        .filter((entry) => Number.isFinite(entry.value))
        .map((entry) => ({
          entry,
          distance: Math.hypot(entry.centroid[0] - point.x, entry.centroid[1] - point.y)
        }))
        .filter((item) => item.distance <= radius)
        .sort((a, b) => a.distance - b.distance)[0]?.entry || null;
    }
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
