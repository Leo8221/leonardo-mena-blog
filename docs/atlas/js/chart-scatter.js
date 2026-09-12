function drawComplexScatterChart(canvas, rows, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const isCompact = width < 520;
  const padding = isCompact
    ? { top: 72, right: 22, bottom: 62, left: 50 }
    : { top: 54, right: 34, bottom: 62, left: 70 };
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  const xValues = rows.map((item) => Number(item[options.xField]));
  const yValues = rows.map((item) => Number(item[options.yField]));
  const sizeValues = rows.map((item) => Number(item[options.sizeField] || 1));
  const xMin = Math.min(0, Math.min(...xValues) - 3);
  const xMax = Math.max(...xValues) + 5;
  const yMin = Math.min(0, Math.min(...yValues) - 3);
  const yMax = Math.max(...yValues) + 7;
  const sizeMax = Math.max(...sizeValues, 1);
  const xRef = options.xReference ?? (xMin + xMax) / 2;
  const yRef = options.yReference ?? (yMin + yMax) / 2;
  const transformValue = (value, mode) => transformChartValue(value, mode);
  const xDomainMin = transformValue(xMin, options.xTransform);
  const xDomainMax = transformValue(xMax, options.xTransform);
  const yDomainMin = transformValue(yMin, options.yTransform);
  const yDomainMax = transformValue(yMax, options.yTransform);

  const xScale = (value) => padding.left + ((transformValue(value, options.xTransform) - xDomainMin) / (xDomainMax - xDomainMin || 1)) * plotW;
  const yScale = (value) => padding.top + plotH - ((transformValue(value, options.yTransform) - yDomainMin) / (yDomainMax - yDomainMin || 1)) * plotH;
  const xRefPos = xScale(xRef);
  const yRefPos = yScale(yRef);
  const points = [];
  const visibleLabelCount = isCompact
    ? Math.min(options.mobileLabelCount ?? 3, options.labelCount || 5)
    : (options.labelCount || 5);
  const labelRows = new Set(
    rows
      .slice()
      .sort((a, b) => Number(b[options.labelTopBy || options.sizeField]) - Number(a[options.labelTopBy || options.sizeField]))
      .slice(0, visibleLabelCount)
      .map((item) => item[options.labelField])
  );

  clearCanvas(ctx, width, height);
  drawGrid(ctx, width, height, padding, 4);

  ctx.fillStyle = "rgba(42, 157, 143, 0.07)";
  ctx.fillRect(xRefPos, padding.top, padding.left + plotW - xRefPos, yRefPos - padding.top);
  ctx.fillStyle = "rgba(200, 100, 72, 0.07)";
  ctx.fillRect(padding.left, yRefPos, xRefPos - padding.left, padding.top + plotH - yRefPos);

  ctx.strokeStyle = CHART_SYSTEM.colors.muted;
  ctx.lineWidth = 1;
  ctx.setLineDash([5, 5]);
  ctx.beginPath();
  ctx.moveTo(xRefPos, padding.top);
  ctx.lineTo(xRefPos, padding.top + plotH);
  ctx.moveTo(padding.left, yRefPos);
  ctx.lineTo(padding.left + plotW, yRefPos);
  ctx.stroke();
  ctx.setLineDash([]);

  drawCanvasTitle(ctx, options.title, padding.left, 22);
  drawAxisLabels(ctx, options.xLabel, options.yLabel, padding, width, height);

  rows
    .slice()
    .sort((a, b) => Number(a[options.sizeField]) - Number(b[options.sizeField]))
    .forEach((item, index) => {
      const x = xScale(Number(item[options.xField]));
      const y = yScale(Number(item[options.yField]));
      const radius = 7 + Math.sqrt(Number(item[options.sizeField]) / sizeMax) * 18;
      const net = Number(item[options.categoryField]);
      const category = item[options.categoryField];
      const color = options.colorMap?.[category] || (Number.isFinite(net) ? (net >= 0 ? CHART_SYSTEM.colors.teal : CHART_SYSTEM.colors.terracotta) : CHART_SYSTEM.colors.blue);
      ctx.beginPath();
      ctx.arc(x, y, radius, 0, Math.PI * 2);
      ctx.fillStyle = addAlpha(color, 0.68);
      ctx.fill();
      ctx.strokeStyle = "#ffffff";
      ctx.lineWidth = 2;
      ctx.stroke();
      points.push({ x, y, radius, item, color });

      if (labelRows.has(item[options.labelField])) {
        ctx.fillStyle = CHART_SYSTEM.colors.ink;
        ctx.font = "700 11px Inter";
        drawPointLabel(ctx, item[options.labelField], x, y, radius, width, height);
      }
    });

  drawLegend(ctx, options.legend || [
    { label: "Superavit relativo", color: CHART_SYSTEM.colors.teal },
    { label: "Deficit relativo", color: CHART_SYSTEM.colors.terracotta }
  ], padding.left, 40);

  bindPointTooltip(canvas, points, (point) => `
    <strong>${escapeHtml(point.item[options.labelField])}</strong>
    <span>${escapeHtml(options.xLabel)}: ${formatNumber(point.item[options.xField])}</span>
    <span>${escapeHtml(options.yLabel)}: ${formatNumber(point.item[options.yField])}</span>
    <span>${escapeHtml(options.sizeLabel || "Tamano")}: ${formatNumber(point.item[options.sizeField])}</span>
  `);
}

function drawScatterChart(canvas, rows, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const isCompact = width < 520;
  const padding = isCompact
    ? { top: 58, right: 18, bottom: 48, left: 42 }
    : { top: 40, right: 30, bottom: 48, left: 54 };
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  const xValues = rows.map((item) => item[options.xField]);
  const yValues = rows.map((item) => item[options.yField]);
  const xMin = Math.min(...xValues) - 4;
  const xMax = Math.max(...xValues) + 4;
  const yMin = Math.min(...yValues) - 4;
  const yMax = Math.max(...yValues) + 4;
  const points = [];

  clearCanvas(ctx, width, height);
  drawCanvasTitle(ctx, options.title, padding.left, 18);
  drawGrid(ctx, width, height, padding, 4);

  rows.forEach((item, index) => {
    const x = padding.left + ((item[options.xField] - xMin) / (xMax - xMin || 1)) * plotW;
    const y = padding.top + plotH - ((item[options.yField] - yMin) / (yMax - yMin || 1)) * plotH;
    const radius = 5 + (item[options.sizeField] / 100) * 7;
    ctx.beginPath();
    ctx.arc(x, y, radius, 0, Math.PI * 2);
    ctx.fillStyle = index < 3 ? "rgba(200, 100, 72, 0.78)" : "rgba(70, 106, 143, 0.62)";
    ctx.fill();
    ctx.strokeStyle = "#ffffff";
    ctx.lineWidth = 2;
    ctx.stroke();
    points.push({ x, y, radius: radius + 4, item });
    if (index < 6) {
      ctx.fillStyle = CHART_SYSTEM.colors.ink;
      ctx.font = "11px Inter";
      ctx.fillText(item[options.labelField], x + radius + 4, y + 4);
    }
  });

  ctx.fillStyle = CHART_SYSTEM.colors.muted;
  ctx.font = "11px Inter";
  ctx.fillText(options.xLabel || "Infraestructura", padding.left, height - 12);
  if (isCompact) {
    ctx.fillText(options.yLabel || "Mercado", padding.left, padding.top - 14);
  } else {
    ctx.save();
    ctx.translate(14, padding.top + plotH);
    ctx.rotate(-Math.PI / 2);
    ctx.fillText(options.yLabel || "Mercado", 0, 0);
    ctx.restore();
  }

  bindPointTooltip(canvas, points, (point) => `
    <strong>${escapeHtml(point.item[options.labelField])}</strong>
    <span>${escapeHtml(options.xLabel || "Infraestructura")}: ${formatNumber(point.item[options.xField])}</span>
    <span>${escapeHtml(options.yLabel || "Mercado")}: ${formatNumber(point.item[options.yField])}</span>
    <span>${escapeHtml(options.sizeLabel || "Oportunidad")}: ${formatNumber(point.item[options.sizeField])}</span>
  `);
}

function drawAxisLabels(ctx, xLabel, yLabel, padding, width, height) {
  ctx.fillStyle = CHART_SYSTEM.colors.muted;
  ctx.font = "11px Inter";
  ctx.fillText(xLabel, padding.left, height - 16);
  if (width < 520) {
    ctx.fillText(yLabel, padding.left, padding.top - 10);
  } else {
    ctx.save();
    ctx.translate(18, padding.top + (height - padding.top - padding.bottom) / 2 + padding.bottom);
    ctx.rotate(-Math.PI / 2);
    ctx.fillText(yLabel, 0, 0);
    ctx.restore();
  }
}
