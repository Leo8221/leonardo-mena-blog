function lineChartScale(values, step) {
  const finite = values.filter(Number.isFinite);
  let min = Math.min(...finite);
  let max = Math.max(...finite);
  if (!finite.length) return { min: 0, max: 1, steps: 4 };
  // Una serie constante queda en el centro, no pegada al borde inferior.
  if (min === max) {
    const margin = step ? step * 2 : Math.max(Math.abs(min) * 0.05, 0.5);
    min -= margin;
    max += margin;
  }
  if (!step) {
    const rough = (max - min) / 4;
    const magnitude = 10 ** Math.floor(Math.log10(rough));
    step = [1, 2, 2.5, 5, 10].find(n => n * magnitude >= rough) * magnitude;
  }
  min = Math.floor(min / step) * step;
  max = Math.ceil(max / step) * step;
  return { min, max, steps: Math.round((max - min) / step) };
}

function drawLineChart(canvas, labels, values, title, options = {}) {
  drawDualLineChart(
    canvas,
    labels,
    [{ label: title, values, color: options.color || CHART_SYSTEM.colors.blue, stepped: options.stepped }],
    title,
    options
  );
}

function drawDualLineChart(canvas, labels, series, title, options = {}) {
  // Cada serie comparte las fechas de labels. El valor menor queda abajo:
  // en canvas la coordenada y crece hacia abajo, por eso se resta de plotH.
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const padding = { top: 32, right: 26, bottom: 42, left: 58 };
  const allValues = series.flatMap((item) => item.values);
  const { min, max, steps } = lineChartScale(allValues, options.tickStep);
  const span = max - min;
  const formatValue = value => formatNumber(value, options.decimals);
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;

  clearCanvas(ctx, width, height);
  drawGrid(ctx, width, height, padding, steps);
  drawCanvasTitle(ctx, title, padding.left, 18);

  const eventMarkers = [];
  drawEventMarkers(ctx, labels, options.events || [], padding, plotW, plotH, width, eventMarkers);

  const tooltipPoints = [];
  series.forEach((serie, serieIndex) => {
    const isStepped = Boolean(serie.stepped || options.stepped);
    const dashPatterns = [[], [6, 4], [2, 4]];
    const points = serie.values.map((value, index) => ({
      x: padding.left + (plotW * index) / Math.max(labels.length - 1, 1),
      y: padding.top + plotH - ((value - min) / span) * plotH,
      value,
      period: labels[index],
      label: serie.label
    }));

    ctx.beginPath();
    points.forEach((point, index) => {
      if (!Number.isFinite(point.value)) return;
      if (index === 0 || !Number.isFinite(points[index - 1].value)) ctx.moveTo(point.x, point.y);
      else if (isStepped) {
        const previous = points[index - 1];
        ctx.lineTo(point.x, previous.y);
        ctx.lineTo(point.x, point.y);
      } else {
        ctx.lineTo(point.x, point.y);
      }
    });
    ctx.setLineDash(dashPatterns[serieIndex % dashPatterns.length]);
    ctx.strokeStyle = serie.color;
    ctx.lineWidth = 3;
    ctx.stroke();
    ctx.setLineDash([]);

    points.forEach((point) => {
      if (!Number.isFinite(point.value)) return;
      ctx.beginPath();
      if (serieIndex % 3 === 1) {
        ctx.rect(point.x - 4, point.y - 4, 8, 8);
      } else if (serieIndex % 3 === 2) {
        ctx.moveTo(point.x, point.y - 5);
        ctx.lineTo(point.x + 5, point.y + 4);
        ctx.lineTo(point.x - 5, point.y + 4);
        ctx.closePath();
      } else {
        ctx.arc(point.x, point.y, 4, 0, Math.PI * 2);
      }
      ctx.fillStyle = serie.color;
      ctx.fill();
      tooltipPoints.push({ ...point, radius: 10 });
    });

    const last = points[points.length - 1];
    if (Number.isFinite(last?.value) && width > 520) {
      ctx.fillStyle = serie.color;
      ctx.font = "700 11px Inter";
      const label = `${serie.label} ${formatValue(last.value)}`;
      ctx.fillText(label, Math.max(padding.left, width - padding.right - ctx.measureText(label).width), last.y - 8);
    }
  });

  ctx.fillStyle = CHART_SYSTEM.colors.muted;
  ctx.font = "11px Inter";
  // Los rótulos se espacian según el ancho; no se descartan puntos ni tooltips.
  const labelSpacing = Math.max(...labels.map(label => ctx.measureText(label).width)) * 2 + 16;
  const availableLabels = Math.max(2, Math.floor(plotW / labelSpacing) + 1);
  const labelCount = Math.min(labels.length, options.maxLabels || availableLabels, availableLabels);
  const labelIndexes = new Set(Array.from({ length: labelCount }, (_, index) =>
    Math.round(index * (labels.length - 1) / Math.max(1, labelCount - 1))));
  labels.forEach((label, index) => {
    if (!labelIndexes.has(index)) return;
    const x = padding.left + (plotW * index) / Math.max(labels.length - 1, 1);
    const labelWidth = ctx.measureText(label).width;
    const labelX = Math.min(Math.max(padding.left, x - labelWidth / 2), width - padding.right - labelWidth);
    ctx.fillText(label, labelX, height - 14);
  });

  for (let i = 0; i <= steps; i += 1) {
    const value = min + (span * i) / steps;
    const y = padding.top + plotH - (plotH * i) / steps;
    ctx.fillText(formatValue(value), 8, y + 4);
  }

  drawLegend(ctx, series, padding.left, height - 4);
  bindCanvasTooltip(canvas, (event) => {
    const cursor = getCanvasPoint(canvas, event);
    const eventMarker = findBoxAt(eventMarkers, cursor);
    if (eventMarker) {
      return `
        <strong>${escapeHtml(eventMarker.item.label)}</strong>
        <span>${escapeHtml(eventMarker.item.period)}</span>
      `;
    }
    const hitPadding = touchLikeEvent(event) ? CHART_SYSTEM.hitPadding.touch : CHART_SYSTEM.hitPadding.mouse;
    const point = findPointAt(tooltipPoints, cursor, hitPadding);
    return point ? `
      <strong>${escapeHtml(point.label)}</strong>
      <span>${escapeHtml(point.period)}: ${formatValue(point.value)}${escapeHtml(options.suffix || "")}</span>
    ` : null;
  });
}

function drawEventMarkers(ctx, labels, events, padding, plotW, plotH, width, markers) {
  if (!Array.isArray(events) || events.length === 0) return;
  const visibleEvents = events
    .map((item) => ({ ...item, index: labels.indexOf(item.period) }))
    .filter((item) => item.index >= 0);
  if (visibleEvents.length === 0) return;

  ctx.save();
  ctx.font = "700 10px Inter";
  visibleEvents.forEach((item, eventIndex) => {
    const x = padding.left + (plotW * item.index) / Math.max(labels.length - 1, 1);
    ctx.beginPath();
    ctx.setLineDash([4, 4]);
    ctx.strokeStyle = addAlpha(CHART_SYSTEM.colors.terracotta, 0.62);
    ctx.lineWidth = 1;
    ctx.moveTo(x, padding.top);
    ctx.lineTo(x, padding.top + plotH);
    ctx.stroke();
    ctx.setLineDash([]);

    markers.push({
      x: x - 10,
      y: padding.top,
      width: 20,
      height: plotH,
      item
    });

    if (width > 520 || eventIndex === visibleEvents.length - 1) {
      const label = fitCanvasText(ctx, item.label, width < 520 ? 86 : 120);
      const textWidth = ctx.measureText(label).width;
      const labelX = Math.max(padding.left, Math.min(x + 5, width - padding.right - textWidth - 8));
      const labelY = padding.top + 12 + (eventIndex % 2) * 14;
      ctx.fillStyle = addAlpha(CHART_SYSTEM.colors.card, 0.94);
      ctx.fillRect(labelX - 4, labelY - 10, textWidth + 8, 14);
      ctx.fillStyle = CHART_SYSTEM.colors.terracotta;
      ctx.fillText(label, labelX, labelY);
    }
  });
  ctx.restore();
}
