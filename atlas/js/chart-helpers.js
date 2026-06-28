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
