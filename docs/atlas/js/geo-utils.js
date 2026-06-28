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

function drawMapLabels(ctx, entries, options, min, max) {
  const topEntries = entries
    .filter((entry) => Number.isFinite(entry.value))
    .sort((a, b) => b.value - a.value)
    .slice(0, 5);

  topEntries.forEach((entry) => {
    const label = entry.feature.properties[options.labelField];
    if (!entry.centroid) return;
    const ratio = normalizeRatio(entry.value, min, max);
    const isDarkFill = ratio > 0.58;
    ctx.fillStyle = isDarkFill ? "#ffffff" : "#252321";
    ctx.strokeStyle = isDarkFill ? "rgba(25, 27, 31, 0.35)" : "rgba(255, 255, 255, 0.72)";
    ctx.lineWidth = 3;
    ctx.lineJoin = "round";
    ctx.font = "700 10px Inter";
    const text = String(label).replace("Santo Domingo", "S. Domingo");
    ctx.strokeText(text, entry.centroid[0] - 22, entry.centroid[1]);
    ctx.fillText(text, entry.centroid[0] - 22, entry.centroid[1]);
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
  ctx.font = "11px Inter";
  const unitLabel = options.unit || "";
  const unitW = unitLabel ? ctx.measureText(unitLabel).width : 0;
  const availableW = width - padding.left - padding.right;
  const reserveUnit = unitLabel && availableW > 320 ? unitW + 16 : 0;
  const legendW = Math.min(240, Math.max(120, availableW - reserveUnit));
  const legendH = 10;
  const x = padding.left;
  const y = height - 28;
  const gradient = ctx.createLinearGradient(x, y, x + legendW, y);
  gradient.addColorStop(0, options.colorStart || CHART_SYSTEM.colors.olive);
  gradient.addColorStop(1, options.colorEnd || CHART_SYSTEM.colors.terracotta);
  ctx.fillStyle = gradient;
  ctx.fillRect(x, y, legendW, legendH);
  ctx.strokeStyle = CHART_SYSTEM.colors.border;
  ctx.strokeRect(x, y, legendW, legendH);
  ctx.fillStyle = CHART_SYSTEM.colors.muted;
  const minLabel = formatNumber(min);
  const maxLabel = formatNumber(max);
  ctx.fillText(minLabel, x, y + 26);
  ctx.fillText(maxLabel, x + legendW - ctx.measureText(maxLabel).width, y + 26);
  if (unitLabel && x + legendW + 12 + unitW <= width - padding.right) {
    ctx.fillText(unitLabel, x + legendW + 12, y + 9);
  } else if (unitLabel) {
    ctx.fillText(unitLabel, x, y - 6);
  }
}
