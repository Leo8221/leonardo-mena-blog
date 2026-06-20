(() => {
  const STORAGE_KEY = "lm_atlas_tour_seen_v1";
  const TOUR_ID = "atlas-guided-tour";
  const trigger = document.getElementById("atlas-tour-start");

  if (!trigger || document.getElementById(TOUR_ID)) return;

  const labels = {
    close: "Cerrar",
    prev: "Atras",
    next: "Siguiente",
    done: "Listo"
  };

  if (!localStorage.getItem(STORAGE_KEY)) {
    trigger.dataset.new = "true";
  }

  const candidates = [
    {
      selector: ".atlas-topbar",
      title: "Ubicacion",
      body: "Desde aqui puedes volver al blog, abrir el archivo o iniciar esta guia."
    },
    {
      selector: "#menu-toggle, #atlas-sidebar",
      title: "Modulos",
      body: "El menu organiza busqueda, familias y vistas del Atlas. En movil se abre como panel."
    },
    {
      selector: ".mobile-atlas-nav, .filter-group",
      title: "Filtros",
      body: "Filtra por familia cuando quieras reducir el ruido antes de abrir un modulo."
    },
    {
      selector: "#metric-strip",
      title: "Senales rapidas",
      body: "Estos indicadores sirven para ubicarse sin entrar todavia al detalle."
    },
    {
      selector: "#module-stage",
      title: "Lectura principal",
      body: "Cada modulo combina una pregunta, una lectura breve, grafico y datos fuente."
    },
    {
      selector: ".module-card, .mobile-module-card",
      title: "Abrir una vista",
      body: "Las tarjetas cambian el modulo y actualizan la URL para poder compartir la vista."
    },
    {
      selector: ".chart-expand, canvas",
      title: "Explorar",
      body: "Los graficos permiten ampliar, descargar o fijar informacion cuando el dato lo permite."
    }
  ];

  const isVisible = (element) => {
    if (!element) return false;
    const rect = element.getBoundingClientRect();
    const style = window.getComputedStyle(element);
    return rect.width > 8 && rect.height > 8 && style.display !== "none" && style.visibility !== "hidden";
  };

  function collectSteps() {
    return candidates
      .map((step) => ({ ...step, element: document.querySelector(step.selector) }))
      .filter((step) => isVisible(step.element));
  }

  const layer = document.createElement("div");
  layer.id = TOUR_ID;
  layer.className = "atlas-tour";
  layer.hidden = true;
  layer.innerHTML = `
    <div class="atlas-tour-shade atlas-tour-shade-top"></div>
    <div class="atlas-tour-shade atlas-tour-shade-right"></div>
    <div class="atlas-tour-shade atlas-tour-shade-bottom"></div>
    <div class="atlas-tour-shade atlas-tour-shade-left"></div>
    <div class="atlas-tour-ring" aria-hidden="true"></div>
    <section class="atlas-tour-popover" role="dialog" aria-modal="true" aria-labelledby="atlas-tour-title">
      <button class="atlas-tour-close" type="button">${labels.close}</button>
      <p class="atlas-tour-count"></p>
      <h2 id="atlas-tour-title"></h2>
      <p class="atlas-tour-body"></p>
      <div class="atlas-tour-actions">
        <button class="atlas-tour-prev" type="button">${labels.prev}</button>
        <button class="atlas-tour-next" type="button">${labels.next}</button>
      </div>
    </section>
  `;
  document.body.appendChild(layer);

  const parts = {
    top: layer.querySelector(".atlas-tour-shade-top"),
    right: layer.querySelector(".atlas-tour-shade-right"),
    bottom: layer.querySelector(".atlas-tour-shade-bottom"),
    left: layer.querySelector(".atlas-tour-shade-left"),
    ring: layer.querySelector(".atlas-tour-ring"),
    popover: layer.querySelector(".atlas-tour-popover"),
    close: layer.querySelector(".atlas-tour-close"),
    count: layer.querySelector(".atlas-tour-count"),
    title: layer.querySelector("#atlas-tour-title"),
    body: layer.querySelector(".atlas-tour-body"),
    prev: layer.querySelector(".atlas-tour-prev"),
    next: layer.querySelector(".atlas-tour-next")
  };

  let steps = [];
  let index = 0;
  let returnFocus = null;

  const clamp = (value, min, max) => Math.max(min, Math.min(value, max));

  function setShade(element, top, left, width, height) {
    Object.assign(element.style, {
      top: `${top}px`,
      left: `${left}px`,
      width: `${width}px`,
      height: `${height}px`
    });
  }

  function placeStep() {
    const step = steps[index];
    if (!step) return;
    const previousScrollBehavior = document.documentElement.style.scrollBehavior;
    document.documentElement.style.scrollBehavior = "auto";
    step.element.scrollIntoView({ block: "center", inline: "nearest", behavior: "instant" });
    document.documentElement.style.scrollBehavior = previousScrollBehavior;

    window.setTimeout(() => window.requestAnimationFrame(() => {
      const viewportWidth = window.innerWidth;
      const viewportHeight = window.innerHeight;
      const rect = step.element.getBoundingClientRect();
      const pad = 8;
      const top = clamp(rect.top - pad, 8, viewportHeight - 24);
      const left = clamp(rect.left - pad, 8, viewportWidth - 24);
      const right = clamp(rect.right + pad, 24, viewportWidth - 8);
      const bottom = clamp(rect.bottom + pad, 24, viewportHeight - 8);
      const width = Math.max(16, right - left);
      const height = Math.max(16, bottom - top);

      setShade(parts.top, 0, 0, viewportWidth, top);
      setShade(parts.left, top, 0, left, height);
      setShade(parts.right, top, right, viewportWidth - right, height);
      setShade(parts.bottom, bottom, 0, viewportWidth, viewportHeight - bottom);

      Object.assign(parts.ring.style, {
        top: `${top}px`,
        left: `${left}px`,
        width: `${width}px`,
        height: `${height}px`
      });

      parts.title.textContent = step.title;
      parts.body.textContent = step.body;
      parts.count.textContent = `${index + 1} de ${steps.length}`;
      parts.prev.disabled = index === 0;
      parts.next.textContent = index === steps.length - 1 ? labels.done : labels.next;

      const popoverWidth = Math.min(340, viewportWidth - 32);
      const popoverHeight = Math.min(parts.popover.offsetHeight || 210, viewportHeight - 32);
      const preferredTop = bottom + 18 + popoverHeight <= viewportHeight - 16
        ? bottom + 18
        : top - popoverHeight - 18;
      Object.assign(parts.popover.style, {
        width: `${popoverWidth}px`,
        left: `${clamp(left, 16, viewportWidth - popoverWidth - 16)}px`,
        top: `${clamp(preferredTop, 16, viewportHeight - popoverHeight - 16)}px`,
        bottom: "auto"
      });
    }), 80);
  }

  function openTour() {
    steps = collectSteps();
    if (!steps.length) return;
    index = 0;
    returnFocus = document.activeElement;
    localStorage.setItem(STORAGE_KEY, "1");
    delete trigger.dataset.new;
    layer.hidden = false;
    document.body.classList.add("tour-open");
    placeStep();
    parts.close.focus({ preventScroll: true });
  }

  function closeTour() {
    layer.hidden = true;
    document.body.classList.remove("tour-open");
    if (returnFocus && typeof returnFocus.focus === "function") {
      returnFocus.focus({ preventScroll: true });
    }
  }

  function move(delta) {
    if (index === steps.length - 1 && delta > 0) {
      closeTour();
      return;
    }
    index = clamp(index + delta, 0, steps.length - 1);
    placeStep();
    parts.close.focus({ preventScroll: true });
  }

  trigger.addEventListener("click", openTour);
  parts.close.addEventListener("click", closeTour);
  parts.prev.addEventListener("click", () => move(-1));
  parts.next.addEventListener("click", () => move(1));
  layer.querySelectorAll(".atlas-tour-shade").forEach((shade) => shade.addEventListener("click", closeTour));

  document.addEventListener("keydown", (event) => {
    if (layer.hidden) return;
    if (event.key === "Escape") closeTour();
    if (event.key === "ArrowRight") move(1);
    if (event.key === "ArrowLeft") move(-1);
    if (event.key === "Tab" && !parts.popover.contains(document.activeElement)) {
      event.preventDefault();
      parts.close.focus({ preventScroll: true });
    }
  });

  window.addEventListener("resize", () => {
    if (!layer.hidden) placeStep();
  });
})();
