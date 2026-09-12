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
