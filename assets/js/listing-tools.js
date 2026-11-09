document.addEventListener("DOMContentLoaded", () => {
  const destination = document.getElementById("listing-categories");
  const categories = document.querySelector(".quarto-listing-category");
  if (!destination || !categories) return;

  const series = document.getElementById("topic-series");
  const seriesIcons = {
    "La República habla de": "analysis",
    "La República en un gráfico": "chart",
    "Fundamentos": "book",
    "Mis predecesores": "ideas"
  };
  const topicIcons = {
    "Capital Humano": "people", "Censo 2022": "people",
    "Desarrollo": "chart", "Deuda": "economy", "Econometría": "chart",
    "Economía Dominicana": "economy", "Educación": "book",
    "Financiamiento": "economy", "Metodología": "articles",
    "MiPyMES": "business", "Transporte": "transport", "Turismo": "compass"
  };

  // Quarto sigue calculando los temas, cantidades y filtros. Solo presentamos
  // sus controles como botones y separamos las series del índice temático.
  destination.insertBefore(categories, series);
  document.querySelector(".quarto-listing-category-title")?.remove();
  categories.setAttribute("role", "group");
  categories.setAttribute("aria-label", "Filtrar publicaciones por tema");
  const seriesButtons = series?.appendChild(document.createElement("div"));
  if (seriesButtons) {
    seriesButtons.className = "topic-series-buttons";
    seriesButtons.setAttribute("role", "group");
    seriesButtons.setAttribute("aria-label", "Filtrar publicaciones por serie");
  }

  for (const category of document.querySelectorAll(".category, .listing-category")) {
    const button = document.createElement("button");
    button.type = "button";
    button.className = category.className;
    if (category.hasAttribute("data-category")) button.dataset.category = category.dataset.category;
    button.append(...category.childNodes);
    button.onclick = category.onclick;
    category.replaceWith(button);

    if (series && button.classList.contains("category")) {
      const name = decodeURIComponent(atob(button.dataset.category));
      const icon = document.createElement("span");
      icon.className = `topic-symbol icon-${seriesIcons[name] || topicIcons[name] || "topics"}`;
      icon.setAttribute("aria-hidden", "true");
      button.prepend(icon);
      if (seriesIcons[name]) seriesButtons.append(button);
    }
  }

  const listing = Object.values(window["quarto-listings"] || {})[0];
  const status = document.querySelector(".listing-status");
  // El listado de Quarto puede envolver el título en una sección con ese ID.
  const heading = document.querySelector("h2#listing-heading, #listing-heading > h2");
  const search = document.querySelector(".quarto-listing .search");
  if (search) {
    search.placeholder = "Buscar en los artículos";
    search.setAttribute("aria-label", "Buscar en los artículos");
  }

  const updateSelection = () => {
    const selected = new URLSearchParams(location.hash.slice(1)).get("category") || "";
    for (const button of destination.querySelectorAll(".category")) {
      const active = decodeURIComponent(atob(button.dataset.category)) === selected;
      button.setAttribute("aria-pressed", String(active));
      button.classList.toggle("active", active);
    }
    const summary = destination.querySelector("summary");
    if (summary) summary.textContent = selected ? `Tema: ${selected}` : "Filtrar por tema";
    if (heading && series) heading.textContent = selected || "Todos los artículos";
    if (status && listing) {
      const total = listing.matchingItems.length;
      const visible = listing.visibleItems.length;
      status.textContent = total === 0 ? "No hay artículos con esos filtros." :
        `${total} ${total === 1 ? "artículo" : "artículos"}${visible < total ? ` · Mostrando ${listing.i}–${listing.i + visible - 1}` : ""}`;
    }
  };
  destination.addEventListener("click", (event) => {
    updateSelection();
    if (series && heading && event.target.closest(".category")) {
      heading.tabIndex = -1;
      heading.focus({ preventScroll: true });
      heading.scrollIntoView({ behavior: matchMedia("(prefers-reduced-motion: reduce)").matches ? "instant" : "smooth" });
    }
  });
  // List.js avisa después de buscar, ordenar o paginar. Quarto restaura la URL.
  listing?.on("updated", updateSelection);
  window.addEventListener("hashchange", updateSelection);
  updateSelection();
});
