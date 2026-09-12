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
