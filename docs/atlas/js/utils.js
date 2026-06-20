// Atlas utility helpers for formatting, canvas coordinates, and tooltips.
function getCanvasPoint(canvas, event) {
  const rect = canvas.getBoundingClientRect();
  const source = event.touches?.[0] || event.changedTouches?.[0] || event;
  return {
    x: source.clientX - rect.left,
    y: source.clientY - rect.top
  };
}

function touchLikeEvent(event) {
  return event.pointerType === "touch" || event.pointerType === "pen" || window.matchMedia(`(max-width: ${CHART_SYSTEM.touchBreakpoint}px), (pointer: coarse)`).matches;
}

function ensureTooltip() {
  let tooltip = document.querySelector(".atlas-tooltip");
  if (!tooltip) {
    tooltip = document.createElement("div");
    tooltip.className = "atlas-tooltip";
    document.body.appendChild(tooltip);
  }
  return tooltip;
}

function showTooltip(html, event, options = {}) {
  if (!state.tooltip) state.tooltip = ensureTooltip();
  const source = event.touches?.[0] || event.changedTouches?.[0] || event;
  const sheetTooltip = touchLikeEvent(event);
  state.tooltipPinned = Boolean(options.pinned);
  state.tooltip.innerHTML = html;
  state.tooltip.classList.toggle("is-pinned", state.tooltipPinned);
  state.tooltip.classList.toggle("is-touch-sheet", sheetTooltip);
  state.tooltip.style.opacity = "1";
  state.tooltip.style.transform = "translateY(0)";
  if (sheetTooltip) {
    state.tooltip.style.left = "";
    state.tooltip.style.top = "";
    return;
  }
  state.tooltip.style.left = `${Math.max(8, Math.min(window.innerWidth - 260, source.clientX + 14))}px`;
  state.tooltip.style.top = `${Math.max(8, Math.min(window.innerHeight - 120, source.clientY + 14))}px`;
}

function hideTooltip(force = false) {
  if (!state.tooltip) return;
  if (state.tooltipPinned && !force) return;
  state.tooltipPinned = false;
  state.tooltip.classList.remove("is-pinned");
  state.tooltip.classList.remove("is-touch-sheet");
  state.tooltip.style.opacity = "0";
  state.tooltip.style.transform = "translateY(4px)";
}

function setupCanvas(canvas) {
  const rect = canvas.getBoundingClientRect();
  const dpr = window.devicePixelRatio || 1;
  canvas.classList.add("is-interactive");
  canvas.style.touchAction = "manipulation";
  canvas.width = Math.max(1, Math.floor(rect.width * dpr));
  canvas.height = Math.max(1, Math.floor(rect.height * dpr));
  const ctx = canvas.getContext("2d");
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  return ctx;
}

function clearCanvas(ctx, width, height) {
  ctx.clearRect(0, 0, width, height);
  ctx.fillStyle = "#ffffff";
  ctx.fillRect(0, 0, width, height);
}

function drawCanvasTitle(ctx, title, x, y) {
  ctx.fillStyle = "#3f4752";
  ctx.font = "700 12px Inter";
  ctx.fillText(title, x, y);
}

function drawLegend(ctx, series, x, y) {
  let offset = 0;
  series.forEach((item) => {
    ctx.fillStyle = item.color;
    ctx.fillRect(x + offset, y - 10, 10, 10);
    ctx.fillStyle = "#3f4752";
    ctx.font = "11px Inter";
    ctx.fillText(item.label, x + offset + 14, y);
    offset += ctx.measureText(item.label).width + 42;
  });
}

function drawGrid(ctx, width, height, padding, steps) {
  const plotW = width - padding.left - padding.right;
  const plotH = height - padding.top - padding.bottom;
  ctx.strokeStyle = "#dde3de";
  ctx.lineWidth = 1;
  ctx.setLineDash([2, 4]);
  for (let i = 0; i <= steps; i += 1) {
    const y = padding.top + (plotH * i) / steps;
    ctx.beginPath();
    ctx.moveTo(padding.left, y);
    ctx.lineTo(padding.left + plotW, y);
    ctx.stroke();
  }
  ctx.setLineDash([]);
}

function formatNumber(value) {
  return new Intl.NumberFormat("es-DO", { maximumFractionDigits: 1 }).format(value);
}

function normalizeText(value) {
  return String(value).normalize("NFD").replace(/[\u0300-\u036f]/g, "").toLowerCase();
}

function statusKey(value) {
  return normalizeText(value).trim();
}

function escapeHtml(value) {
  return String(value)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#039;");
}

function debounce(fn, wait) {
  let timer = null;
  return (...args) => {
    window.clearTimeout(timer);
    timer = window.setTimeout(() => fn(...args), wait);
  };
}

function renderLoadError(error) {
  els.stage.innerHTML = `
    <div class="empty-state">
      <strong>No se pudo cargar el Atlas.</strong>
      <p>Revisa que <code>atlas/data/atlas-data.json</code> exista y que la pagina se abra desde un servidor local o GitHub Pages.</p>
      <p>${escapeHtml(error.message)}</p>
    </div>
  `;
}
