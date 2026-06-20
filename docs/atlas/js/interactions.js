// Atlas interaction helpers for sharing, fullscreen, and exports.
function currentViewUrl() {
  if (typeof buildAtlasUrl === "function") {
    return buildAtlasUrl().toString();
  }
  return new URL(window.location.href).toString();
}

async function copyText(value) {
  if (navigator.clipboard && window.isSecureContext) {
    try {
      await navigator.clipboard.writeText(value);
      return true;
    } catch {
      // Fall through to the input-based copy path.
    }
  }
  const input = document.createElement("input");
  input.value = value;
  input.setAttribute("readonly", "");
  input.style.position = "fixed";
  input.style.opacity = "0";
  document.body.appendChild(input);
  input.select();
  const copied = document.execCommand("copy");
  input.remove();
  return copied;
}

function flashButton(button, label) {
  const original = button.textContent;
  button.textContent = label;
  window.setTimeout(() => {
    button.textContent = original;
  }, 1200);
}

function openChartFullscreen(canvasId) {
  const sourceCanvas = document.getElementById(canvasId);
  if (!sourceCanvas) return;
  hideTooltip(true);
  if (typeof trackAtlasEvent === "function") {
    trackAtlasEvent("atlas_fullscreen_open", { chart_id: canvasId });
  }
  const card = sourceCanvas.closest(".chart-card");
  const title = card?.querySelector("h3")?.textContent || "Gráfico";
  const article = card?.querySelector(".article-link");
  const previousFocus = document.activeElement instanceof HTMLElement ? document.activeElement : null;
  const modal = document.createElement("div");
  modal.className = "atlas-modal";
  modal.setAttribute("role", "dialog");
  modal.setAttribute("aria-modal", "true");
  modal.setAttribute("aria-labelledby", `modal-title-${canvasId}`);
  modal.innerHTML = `
    <section class="atlas-modal-panel" tabindex="-1">
      <header class="atlas-modal-head">
        <h2 id="modal-title-${escapeHtml(canvasId)}">${escapeHtml(title)}</h2>
        <div class="atlas-modal-actions">
          ${article ? article.outerHTML : ""}
          <button class="atlas-modal-close" type="button">Cerrar</button>
        </div>
      </header>
      <div class="atlas-modal-body">
        <canvas id="expanded-${escapeHtml(canvasId)}" height="640" aria-label="${escapeHtml(title)}"></canvas>
      </div>
    </section>
  `;

  const close = () => {
    hideTooltip(true);
    document.body.classList.remove("modal-open");
    modal.removeEventListener("keydown", onKeydown);
    modal.remove();
    if (previousFocus && document.contains(previousFocus)) previousFocus.focus();
  };
  const onKeydown = (event) => {
    if (event.key === "Escape") {
      close();
      return;
    }
    if (event.key !== "Tab") return;
    const focusables = modal.querySelectorAll('a[href], button:not([disabled]), [tabindex]:not([tabindex="-1"])');
    if (!focusables.length) return;
    const first = focusables[0];
    const last = focusables[focusables.length - 1];
    if (event.shiftKey && document.activeElement === first) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && document.activeElement === last) {
      event.preventDefault();
      first.focus();
    }
  };

  modal.addEventListener("click", (event) => {
    if (event.target === modal) close();
  });
  document.body.appendChild(modal);
  const closeButton = modal.querySelector(".atlas-modal-close");
  closeButton.addEventListener("click", close);
  document.body.classList.add("modal-open");
  modal.addEventListener("keydown", onKeydown);
  closeButton.focus();

  window.requestAnimationFrame(() => {
    const expandedCanvas = modal.querySelector("canvas");
    redrawExpandedChart(canvasId, expandedCanvas, sourceCanvas);
  });
}

function downloadCanvasPng(canvasId, button = null) {
  const canvas = document.getElementById(canvasId);
  if (!canvas) {
    if (button) flashButton(button, "Sin grafico");
    return;
  }
  const module = findModule(state.active);
  const metric = activeMetricForModule(module);
  const generated = String(state.data?.generatedAt || state.data?.updated || "").slice(0, 10) || "sin-fecha";
  const parts = ["atlas", state.active, canvasId, metric, generated].filter(Boolean);
  const link = document.createElement("a");
  link.download = `${parts.map(slugify).join("-")}.png`;
  link.href = canvas.toDataURL("image/png");
  link.click();
  if (typeof trackAtlasEvent === "function") {
    trackAtlasEvent("atlas_png_download", { chart_id: canvasId, metric });
  }
  if (button) flashButton(button, "PNG");
}

function redrawExpandedChart(canvasId, canvas, sourceCanvas) {
  if (!canvas) return;
  if (canvasId === "visual-map") {
    const mapConfig = visualMapConfig();
    drawChoroplethMap(canvas, mapConfig.features, {
      ...mapConfig,
      inspectorId: null,
      mapId: `${mapConfig.mapId}-fullscreen`
    });
    return;
  }

  if (canvasId === "tourism-treemap") {
    drawTreemapChart(canvas, state.articleVisuals.tourism.treemap, {
      title: "Estructura de motivaciones turísticas",
      labelField: "motivo",
      valueField: "porcentaje",
      categoryField: "categoria"
    });
    return;
  }

  if (canvasId === "transport-space") {
    drawComplexScatterChart(canvas, state.articleVisuals.transport.rentEmployment, transportScatterOptions());
    return;
  }

  if (canvasId === "debt-burden") {
    drawDebtBurdenChart(canvas, state.articleVisuals.debt.service, {
      title: "Servicio total e intereses",
      labelField: "anio",
      serviceField: "service",
      shareField: "interest_share"
    });
    return;
  }

  if (canvasId === "debt-service") {
    drawStackedBarChart(canvas, state.articleVisuals.debt.service, {
      title: "Servicio de deuda por componente",
      labelField: "anio",
      fields: [
        { field: "principal", label: "Principal", color: "#466a8f" },
        { field: "interest", label: "Intereses", color: "#c86448" },
        { field: "commissions", label: "Comisiones", color: "#d4ac0d" }
      ],
      unit: "US$ MM"
    });
    return;
  }

  if (!sourceCanvas) return;
  const ctx = setupCanvas(canvas);
  const { width, height } = canvas.getBoundingClientRect();
  clearCanvas(ctx, width, height);
  ctx.drawImage(sourceCanvas, 0, 0, width, height);
}
