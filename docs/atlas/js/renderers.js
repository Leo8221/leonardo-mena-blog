// Atlas chart renderers. Keep these pure enough to reuse from fullscreen views.
function renderBarRows(rows, options) {
  const max = options.max || Math.max(...rows.map((item) => Math.abs(item[options.valueField])), 1);
  return rows.map((item) => {
    const value = Number(item[options.valueField]);
    const width = Math.min(100, Math.abs(value) / max * 100);
    return `
      <div class="bar-row">
        <strong>${escapeHtml(item[options.labelField])}</strong>
        <span class="bar-track"><span class="bar-fill" style="width:${width}%"></span></span>
        <span>${formatNumber(value)}${options.suffix || ""}</span>
      </div>
    `;
  }).join("");
}

function renderContributionRows(rows, options) {
  const sortedRows = rows
    .slice()
    .sort((a, b) => Math.abs(Number(b[options.valueField])) - Math.abs(Number(a[options.valueField])));
  const max = Math.max(...sortedRows.map((item) => Math.abs(Number(item[options.valueField]))), 1);
  return sortedRows.map((item, index) => {
    const value = Number(item[options.valueField]);
    const width = Math.min(100, Math.abs(value) / max * 100);
    const sign = value > 0 ? "+" : "";
    return `
      <div class="contribution-row" style="--share:${width}%">
        <span class="contribution-rank">${index + 1}</span>
        <strong>${escapeHtml(item[options.labelField])}</strong>
        <span>${sign}${formatNumber(value)}${options.suffix || ""}</span>
      </div>
    `;
  }).join("");
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
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const padding = { top: 32, right: 26, bottom: 42, left: 58 };
  const allValues = series.flatMap((item) => item.values);
  const min = Math.min(...allValues);
  const max = Math.max(...allValues);
  const span = max - min || 1;
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;

  clearCanvas(ctx, width, height);
  drawGrid(ctx, width, height, padding, 4);
  drawCanvasTitle(ctx, title, padding.left, 18);

  const eventMarkers = [];
  drawEventMarkers(ctx, labels, options.events || [], padding, plotW, plotH, width, eventMarkers);

  const tooltipPoints = [];
  series.forEach((serie, serieIndex) => {
    const isStepped = Boolean(serie.stepped || options.stepped);
    const dashPatterns = [[], [6, 4], [2, 4]];
    const points = serie.values.map((value, index) => ({
      x: padding.left + (plotW * index) / Math.max(serie.values.length - 1, 1),
      y: padding.top + plotH - ((value - min) / span) * plotH,
      value,
      period: labels[index],
      label: serie.label
    }));

    ctx.beginPath();
    points.forEach((point, index) => {
      if (index === 0) ctx.moveTo(point.x, point.y);
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
    if (last && width > 520) {
      ctx.fillStyle = serie.color;
      ctx.font = "700 11px Inter";
      ctx.fillText(`${serie.label} ${formatNumber(last.value)}`, Math.min(last.x + 8, width - padding.right - 96), last.y - 8);
    }
  });

  ctx.fillStyle = CHART_SYSTEM.colors.muted;
  ctx.font = "11px Inter";
  labels.forEach((label, index) => {
    const x = padding.left + (plotW * index) / Math.max(labels.length - 1, 1);
    ctx.fillText(label, x - 10, height - 14);
  });

  for (let i = 0; i <= 4; i += 1) {
    const value = min + (span * i) / 4;
    const y = padding.top + plotH - (plotH * i) / 4;
    ctx.fillText(formatNumber(value), 8, y + 4);
  }

  drawLegend(ctx, series, padding.left, height - 4);
  bindCanvasTooltip(canvas, (event) => {
    const cursor = getCanvasPoint(canvas, event);
    const eventMarker = eventMarkers.find((marker) => (
      cursor.x >= marker.x &&
      cursor.x <= marker.x + marker.width &&
      cursor.y >= marker.y &&
      cursor.y <= marker.y + marker.height
    ));
    if (eventMarker) {
      return `
        <strong>${escapeHtml(eventMarker.item.label)}</strong>
        <span>${escapeHtml(eventMarker.item.period)}</span>
      `;
    }
    const hitPadding = touchLikeEvent(event) ? CHART_SYSTEM.hitPadding.touch : CHART_SYSTEM.hitPadding.mouse;
    const point = tooltipPoints.find((candidate) => {
      const dx = cursor.x - candidate.x;
      const dy = cursor.y - candidate.y;
      return Math.sqrt(dx * dx + dy * dy) <= candidate.radius + hitPadding;
    });
    return point ? `
      <strong>${escapeHtml(point.label)}</strong>
      <span>${escapeHtml(point.period)}: ${formatNumber(point.value)}</span>
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

function drawHorizontalBarChart(canvas, rows, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const values = rows.map((item) => Number(item[options.valueField]));
  const max = options.max || Math.max(...values.map(Math.abs), 1);
  const padding = { top: 34, right: 40, bottom: 24, left: Math.min(190, width * 0.38) };
  const rowHeight = Math.min(34, (height - padding.top - padding.bottom) / rows.length);
  const gap = Math.max(8, rowHeight * 0.35);
  const barH = Math.max(13, rowHeight - gap);
  const boxes = [];

  clearCanvas(ctx, width, height);
  drawCanvasTitle(ctx, options.title, padding.left, 18);

  rows.forEach((item, index) => {
    const value = Number(item[options.valueField]);
    const y = padding.top + index * (barH + gap);
    const available = width - padding.left - padding.right;
    const barW = Math.abs(value) / max * available;
    ctx.fillStyle = CHART_SYSTEM.colors.soft;
    ctx.font = "12px Inter";
    ctx.fillText(String(item[options.labelField]), 8, y + barH - 2);
    ctx.fillStyle = value < 0 ? CHART_SYSTEM.colors.terracotta : CHART_SYSTEM.colors.blue;
    ctx.fillRect(padding.left, y, barW, barH);
    boxes.push({ x: padding.left, y, width: barW, height: barH, item, value });
    const valueLabel = formatNumber(value);
    ctx.font = "700 12px Inter";
    const labelWidth = ctx.measureText(valueLabel).width;
    const labelX = padding.left + barW + 8;
    const labelY = y + barH - 2;
    if (labelX + labelWidth > width - 8) {
      ctx.fillStyle = CHART_SYSTEM.colors.card;
      ctx.fillText(valueLabel, Math.max(padding.left + 4, padding.left + barW - labelWidth - 8), labelY);
    } else {
      ctx.fillStyle = CHART_SYSTEM.colors.ink;
      ctx.fillText(valueLabel, labelX, labelY);
    }
  });

  bindBoxTooltip(canvas, boxes, (box) => `
    <strong>${escapeHtml(box.item[options.labelField])}</strong>
    <span>${formatNumber(box.value)}${escapeHtml(options.suffix || "")}</span>
  `);
}

function drawCategoricalCountChart(canvas, rows, field, title) {
  const counts = rows.reduce((acc, item) => {
    acc[item[field]] = (acc[item[field]] || 0) + 1;
    return acc;
  }, {});
  const chartRows = Object.entries(counts)
    .sort((a, b) => b[1] - a[1])
    .map(([name, value]) => ({ name, value }));
  drawHorizontalBarChart(canvas, chartRows, {
    labelField: "name",
    valueField: "value",
    title,
    max: Math.max(...chartRows.map((item) => item.value), 1)
  });
}

function drawGroupedBarChart(canvas, rows, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const padding = { top: 40, right: 26, bottom: 70, left: 52 };
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  const groupW = plotW / rows.length;
  const barW = Math.min(18, groupW / (options.fields.length + 1));
  const max = Math.max(...rows.flatMap((row) => options.fields.map((field) => row[field.field])), 100);
  const boxes = [];

  clearCanvas(ctx, width, height);
  drawCanvasTitle(ctx, options.title, padding.left, 18);
  drawGrid(ctx, width, height, padding, 4);

  rows.forEach((row, rowIndex) => {
    options.fields.forEach((field, fieldIndex) => {
      const value = Number(row[field.field]);
      const x = padding.left + rowIndex * groupW + fieldIndex * (barW + 4) + groupW / 4;
      const barH = value / max * plotH;
      const y = padding.top + plotH - barH;
      ctx.fillStyle = field.color;
      ctx.fillRect(x, y, barW, barH);
      boxes.push({ x, y, width: barW, height: barH, item: row, field, value });
    });

    ctx.save();
    ctx.translate(padding.left + rowIndex * groupW + groupW / 2, height - 18);
    ctx.rotate(-Math.PI / 6);
    ctx.fillStyle = CHART_SYSTEM.colors.muted;
    ctx.font = "11px Inter";
    ctx.fillText(row[options.labelField], -36, 0);
    ctx.restore();
  });

  drawLegend(ctx, options.fields.map((field) => ({ label: field.label, color: field.color })), padding.left, 32);
  bindBoxTooltip(canvas, boxes, (box) => `
    <strong>${escapeHtml(box.item[options.labelField])}</strong>
    <span>${escapeHtml(box.field.label)}: ${formatNumber(box.value)}</span>
  `);
}

function drawTreemapChart(canvas, rows, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const padding = { top: 44, right: 18, bottom: 18, left: 18 };
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  const total = rows.reduce((sum, item) => sum + Number(item[options.valueField]), 0) || 1;
  const colors = {
    Masivo: CHART_SYSTEM.colors.terracotta,
    Vinculado: CHART_SYSTEM.colors.olive,
    Nicho: CHART_SYSTEM.colors.gold
  };
  const boxes = [];

  clearCanvas(ctx, width, height);
  drawCanvasTitle(ctx, options.title, padding.left, 22);

  layoutTreemap(
    rows
    .slice()
      .sort((a, b) => Number(b[options.valueField]) - Number(a[options.valueField])),
    padding.left,
    padding.top,
    plotW,
    plotH,
    options.valueField,
    boxes
  );

  boxes.forEach((box) => {
    const category = box.item[options.categoryField];
    const value = Number(box.item[options.valueField]);
    ctx.fillStyle = colors[category] || CHART_SYSTEM.colors.muted;
    ctx.fillRect(box.x, box.y, box.width, box.height);
    ctx.strokeStyle = "#ffffff";
    ctx.lineWidth = 2;
    ctx.strokeRect(box.x, box.y, box.width, box.height);
    if (box.width > 72 && box.height > 42) {
      ctx.fillStyle = category === "Nicho" ? CHART_SYSTEM.colors.ink : "#ffffff";
      ctx.font = value > 20 ? "800 18px Inter" : "700 12px Inter";
      ctx.fillText(String(box.item[options.labelField]).slice(0, 18), box.x + 8, box.y + 22);
      ctx.font = "700 12px Inter";
      ctx.fillText(`${formatNumber(value)}%`, box.x + 8, box.y + 40);
    }
  });

  bindBoxTooltip(canvas, boxes, (box) => `
    <strong>${escapeHtml(box.item[options.labelField])}</strong>
    <span>${formatNumber(box.item[options.valueField])}% de motivaciones</span>
    <span>${escapeHtml(box.item[options.categoryField])}</span>
  `);
}

function drawDebtBurdenChart(canvas, rows, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const padding = width < 420
    ? { top: 46, right: 18, bottom: 54, left: 48 }
    : { top: 46, right: 52, bottom: 58, left: 62 };
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  const serviceMax = Math.max(...rows.map((row) => Number(row[options.serviceField])), 1);
  const shareMax = 70;
  const gap = width < 420 ? 4 : 7;
  const barW = Math.max(8, (plotW - gap * (rows.length - 1)) / rows.length);
  const boxes = [];
  const points = [];
  const serviceColor = CHART_SYSTEM.colors.blue;
  const serviceHighlight = CHART_SYSTEM.colors.terracotta;
  const shareColor = CHART_SYSTEM.colors.olive;

  const xAt = (index) => padding.left + index * (barW + gap);
  const shareY = (value) => padding.top + plotH - (Number(value) / shareMax) * plotH;

  clearCanvas(ctx, width, height);
  drawCanvasTitle(ctx, options.title, padding.left, 22);
  drawGrid(ctx, width, height, padding, 4);

  rows.forEach((row, index) => {
    const service = Number(row[options.serviceField]);
    const x = xAt(index);
    const barH = (service / serviceMax) * plotH;
    const y = padding.top + plotH - barH;
    ctx.fillStyle = index === rows.length - 1 ? serviceHighlight : serviceColor;
    ctx.fillRect(x, y, barW, barH);
    boxes.push({ x, y, width: barW, height: barH, item: row, value: service });

    const point = {
      x: x + barW / 2,
      y: shareY(row[options.shareField]),
      radius: 10,
      item: row,
      value: Number(row[options.shareField])
    };
    points.push(point);

    ctx.fillStyle = CHART_SYSTEM.colors.muted;
    ctx.font = "10px Inter";
    if (index % 2 === 0 || width > 520) {
      ctx.fillText(String(row[options.labelField]).slice(-2), x, height - 18);
    }
  });

  ctx.beginPath();
  points.forEach((point, index) => {
    if (index === 0) ctx.moveTo(point.x, point.y);
    else ctx.lineTo(point.x, point.y);
  });
  ctx.strokeStyle = shareColor;
  ctx.lineWidth = 3;
  ctx.stroke();

  points.forEach((point) => {
    ctx.beginPath();
    ctx.arc(point.x, point.y, 4, 0, Math.PI * 2);
    ctx.fillStyle = shareColor;
    ctx.fill();
  });

  drawLegend(ctx, [
    { label: "Servicio total", color: serviceColor },
    { label: "Intereses / servicio", color: shareColor }
  ], padding.left, 40);

  bindCanvasTooltip(canvas, (event) => {
    const cursor = getCanvasPoint(canvas, event);
    const point = points.find((candidate) => {
      const dx = cursor.x - candidate.x;
      const dy = cursor.y - candidate.y;
      return Math.sqrt(dx * dx + dy * dy) <= candidate.radius + 2;
    });
    if (point) {
      return `
        <strong>${escapeHtml(point.item[options.labelField])}</strong>
        <span>Intereses: ${formatNumber(point.value)}%</span>
        <span>Servicio: ${formatNumber(point.item[options.serviceField])} US$ MM</span>
      `;
    }
    const box = boxes.find((candidate) => (
      cursor.x >= candidate.x &&
      cursor.x <= candidate.x + candidate.width &&
      cursor.y >= candidate.y &&
      cursor.y <= candidate.y + candidate.height
    ));
    if (!box) return null;
    return `
      <strong>${escapeHtml(box.item[options.labelField])}</strong>
      <span>Servicio: ${formatNumber(box.value)} US$ MM</span>
      <span>Intereses: ${formatNumber(box.item[options.shareField])}%</span>
    `;
  });
}

function drawStackedBarChart(canvas, rows, options) {
  if (!canvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  const padding = { top: 44, right: 26, bottom: 54, left: 62 };
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  const totals = rows.map((row) => options.fields.reduce((sum, field) => sum + Number(row[field.field] || 0), 0));
  const max = Math.max(...totals, 1);
  const gap = 7;
  const barW = Math.max(10, (plotW - gap * (rows.length - 1)) / rows.length);
  const boxes = [];

  clearCanvas(ctx, width, height);
  drawCanvasTitle(ctx, options.title, padding.left, 22);
  drawGrid(ctx, width, height, padding, 4);

  rows.forEach((row, rowIndex) => {
    let y = padding.top + plotH;
    const x = padding.left + rowIndex * (barW + gap);
    options.fields.forEach((field) => {
      const value = Number(row[field.field] || 0);
      const barH = value / max * plotH;
      y -= barH;
      ctx.fillStyle = field.color;
      ctx.fillRect(x, y, barW, barH);
      boxes.push({ x, y, width: barW, height: barH, item: row, field, value });
    });
    ctx.fillStyle = CHART_SYSTEM.colors.muted;
    ctx.font = "10px Inter";
    ctx.fillText(String(row[options.labelField]).slice(-2), x - 1, height - 16);
  });

  drawLegend(ctx, options.fields, padding.left, 40);
  bindBoxTooltip(canvas, boxes, (box) => `
    <strong>${escapeHtml(box.item[options.labelField])}</strong>
    <span>${escapeHtml(box.field.label)}: ${formatNumber(box.value)} ${escapeHtml(options.unit || "")}</span>
  `);
}

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

function drawPointLabel(ctx, label, x, y, radius, width, height) {
  const maxWidth = Math.max(60, Math.min(150, width - 24));
  const text = fitCanvasText(ctx, String(label), maxWidth);
  const textWidth = ctx.measureText(text).width;
  const rightX = x + radius + 5;
  const leftX = x - radius - textWidth - 5;
  const labelX = rightX + textWidth <= width - 8
    ? rightX
    : Math.max(8, leftX);
  const labelY = Math.max(14, Math.min(height - 10, y + 4));
  ctx.fillText(text, labelX, labelY);
}

function fitCanvasText(ctx, text, maxWidth) {
  if (ctx.measureText(text).width <= maxWidth) return text;
  let trimmed = text;
  while (trimmed.length > 4 && ctx.measureText(`${trimmed}...`).width > maxWidth) {
    trimmed = trimmed.slice(0, -1);
  }
  return `${trimmed.trim()}...`;
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

function bindPointTooltip(canvas, points, content) {
  bindCanvasTooltip(canvas, (event) => {
    const cursor = getCanvasPoint(canvas, event);
    const hitPadding = touchLikeEvent(event) ? CHART_SYSTEM.hitPadding.touch : CHART_SYSTEM.hitPadding.mouse;
    const found = points.find((point) => {
      const dx = cursor.x - point.x;
      const dy = cursor.y - point.y;
      return Math.sqrt(dx * dx + dy * dy) <= point.radius + hitPadding;
    });

    return found ? content(found) : null;
  });
}

function bindBoxTooltip(canvas, boxes, content) {
  bindCanvasTooltip(canvas, (event) => {
    const cursor = getCanvasPoint(canvas, event);
    const found = boxes.find((box) => (
      cursor.x >= box.x &&
      cursor.x <= box.x + box.width &&
      cursor.y >= box.y &&
      cursor.y <= box.y + box.height
    ));

    return found ? content(found) : null;
  });
}

function bindCanvasTooltip(canvas, resolveContent) {
  canvas.onmousemove = null;
  canvas.onclick = null;
  canvas.onmouseleave = null;
  canvas.onpointermove = (event) => {
    if (event.pointerType === "touch") return;
    const html = resolveContent(event);
    canvas.style.cursor = html ? "pointer" : "default";
    if (!html) {
      hideTooltip();
      return;
    }
    showTooltip(html, event, { pinned: false });
  };

  canvas.onpointerdown = (event) => {
    const html = resolveContent(event);
    canvas.style.cursor = html ? "pointer" : "default";
    if (!html) {
      hideTooltip(true);
      return;
    }
    showTooltip(html, event, { pinned: event.pointerType !== "mouse" });
    if (event.pointerType !== "mouse") event.preventDefault();
  };

  canvas.onpointerleave = () => {
    canvas.style.cursor = "default";
    hideTooltip();
  };

  canvas.onpointercancel = () => {
    canvas.style.cursor = "default";
    hideTooltip(true);
  };
}

function transformChartValue(value, mode) {
  const numeric = Number(value);
  if (mode === "sqrt") return Math.sign(numeric) * Math.sqrt(Math.abs(numeric));
  if (mode === "log") return Math.sign(numeric) * Math.log1p(Math.abs(numeric));
  return numeric;
}

function layoutTreemap(items, x, y, width, height, valueField, boxes) {
  if (!items.length || width <= 0 || height <= 0) return;
  if (items.length === 1) {
    boxes.push({ x, y, width, height, item: items[0] });
    return;
  }

  const total = items.reduce((sum, item) => sum + Number(item[valueField]), 0);
  let running = 0;
  let splitIndex = 0;
  for (let index = 0; index < items.length; index += 1) {
    running += Number(items[index][valueField]);
    splitIndex = index + 1;
    if (running >= total / 2) break;
  }

  const left = items.slice(0, splitIndex);
  const right = items.slice(splitIndex);
  const leftTotal = left.reduce((sum, item) => sum + Number(item[valueField]), 0);
  const ratio = leftTotal / (total || 1);

  if (width >= height) {
    const leftW = width * ratio;
    layoutTreemap(left, x, y, leftW, height, valueField, boxes);
    layoutTreemap(right, x + leftW, y, width - leftW, height, valueField, boxes);
  } else {
    const topH = height * ratio;
    layoutTreemap(left, x, y, width, topH, valueField, boxes);
    layoutTreemap(right, x, y + topH, width, height - topH, valueField, boxes);
  }
}

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

function normalizeRatio(value, min, max) {
  return Math.max(0, Math.min(1, (value - min) / (max - min || 1)));
}

function interpolateColor(start, end, ratio) {
  const from = hexToRgb(start);
  const to = hexToRgb(end);
  const mix = from.map((channel, index) => Math.round(channel + (to[index] - channel) * ratio));
  return `rgb(${mix[0]}, ${mix[1]}, ${mix[2]})`;
}

function addAlpha(hex, alpha) {
  const [r, g, b] = hexToRgb(hex);
  return `rgba(${r}, ${g}, ${b}, ${alpha})`;
}

function hexToRgb(hex) {
  const clean = hex.replace("#", "");
  return [
    parseInt(clean.slice(0, 2), 16),
    parseInt(clean.slice(2, 4), 16),
    parseInt(clean.slice(4, 6), 16)
  ];
}
