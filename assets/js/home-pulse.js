const pulseRoot = document.querySelector("[data-home-pulse]");

const preferredMetricIds = ["usd", "inflation", "labor"];

const formatDate = (value) => {
  if (!value) return "";
  // La hora local evita que una fecha sin hora aparezca como el día anterior.
  const date = new Date(`${value}T00:00:00`);
  if (Number.isNaN(date.getTime())) return value;
  return new Intl.DateTimeFormat("es-DO", {
    day: "2-digit",
    month: "short",
    year: "numeric"
  }).format(date);
};

const appendText = (element, text) => {
  element.textContent = text || "";
  return element;
};

const buildMetricCard = (metric, modulesById) => {
  const module = modulesById.get(metric.module);
  // El corte pertenece al módulo, no a la fecha global del archivo de datos.
  const cutoff = module?.sourceInfo?.updated;
  const source = [metric.meta || module?.source || "Atlas", cutoff ? `Corte: ${formatDate(cutoff)}` : ""].filter(Boolean).join(" · ");
  const link = document.createElement("a");
  link.className = "home-pulse-card";
  link.href = `atlas/?view=${encodeURIComponent(metric.module || "overview")}`;
  link.dataset.tone = metric.tone || "neutral";
  link.setAttribute(
    "aria-label",
    `${metric.label}: ${metric.value}. ${metric.delta}. ${source}. Abrir en Atlas.`
  );

  link.append(
    appendText(document.createElement("span"), metric.label),
    appendText(document.createElement("strong"), metric.value),
    appendText(document.createElement("em"), metric.delta),
    appendText(document.createElement("small"), source)
  );

  return link;
};

const renderFallback = () => {
  if (!pulseRoot) return;
  const fallback = pulseRoot.querySelector(".home-pulse-fallback");
  const fallbackNote = fallback?.querySelector("em");
  if (fallbackNote) {
    fallbackNote.textContent = "No se pudo cargar el resumen. Abre el Atlas para ver datos, fuentes y corte.";
  }
};

const renderPulse = async () => {
  // La portada lee el mismo resumen que Atlas. Si falla, conserva el enlace
  // escrito en index.qmd para que la sección siga siendo útil.
  if (!pulseRoot) return;

  try {
    const response = await fetch("atlas/data/atlas-data.json", { cache: "default" });
    if (!response.ok) {
      renderFallback();
      return;
    }

    const atlas = await response.json();
    const modulesById = new Map((atlas.modules || []).map((module) => [module.id, module]));
    const selected = preferredMetricIds
      .map((id) => (atlas.metrics || []).find((metric) => metric.id === id))
      .filter(Boolean);

    if (!selected.length) {
      renderFallback();
      return;
    }

    pulseRoot.replaceChildren(...selected.map((metric) => buildMetricCard(metric, modulesById)));
  } catch (_error) {
    renderFallback();
  }
};

renderPulse();

document.querySelectorAll('a[href$="suscribete.html"], a[href$="suscribete.qmd"]').forEach((link) => {
  link.addEventListener("click", () => {
    if (typeof window.gtag === "function") {
      window.gtag("event", "newsletter_click", {
        referrer_section: "home",
        device_type: window.matchMedia("(max-width: 680px)").matches ? "mobile" : "desktop"
      });
    }
  });
});
