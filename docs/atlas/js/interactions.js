// Atlas interaction helpers for sharing, fullscreen, and exports.
function currentViewUrl() {
  const url = new URL(window.location.href);
  url.hash = state.active === "overview" ? "" : state.active;
  return url.toString();
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
  const card = sourceCanvas.closest(".chart-card");
  const title = card?.querySelector("h3")?.textContent || "Gráfico";
  const article = card?.querySelector(".article-link");
  const modal = document.createElement("div");
  modal.className = "atlas-modal";
  modal.setAttribute("role", "dialog");
  modal.setAttribute("aria-modal", "true");
  modal.innerHTML = `
    <section class="atlas-modal-panel">
      <header class="atlas-modal-head">
        <h2>${escapeHtml(title)}</h2>
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
    window.removeEventListener("keydown", onKeydown);
    modal.remove();
  };
  const onKeydown = (event) => {
    if (event.key === "Escape") close();
  };

  modal.addEventListener("click", (event) => {
    if (event.target === modal) close();
  });
  modal.querySelector(".atlas-modal-close").addEventListener("click", close);
  document.body.appendChild(modal);
  document.body.classList.add("modal-open");
  window.addEventListener("keydown", onKeydown);

  window.requestAnimationFrame(() => {
    const expandedCanvas = modal.querySelector("canvas");
    redrawExpandedChart(canvasId, expandedCanvas, sourceCanvas);
  });
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
