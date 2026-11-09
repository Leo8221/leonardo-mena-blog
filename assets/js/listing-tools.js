document.addEventListener("DOMContentLoaded", () => {
  const destination = document.getElementById("listing-categories");
  const categories = document.querySelector(".quarto-listing-category");
  if (!destination || !categories) return;

  // Quarto calcula los temas y sus cantidades. Conservamos su filtro y solo
  // lo acercamos a la búsqueda, con botones que también funcionan con teclado.
  destination.append(categories);
  document.querySelector(".quarto-listing-category-title")?.remove();
  categories.setAttribute("role", "group");
  categories.setAttribute("aria-label", "Filtrar publicaciones por tema");
  for (const category of categories.querySelectorAll(".category")) {
    const button = document.createElement("button");
    button.type = "button";
    button.className = category.className;
    button.dataset.category = category.dataset.category;
    button.append(...category.childNodes);
    button.onclick = category.onclick;
    category.replaceWith(button);
  }

  const updateSelection = () => {
    const selected = new URLSearchParams(location.hash.slice(1)).get("category") || "";
    for (const button of categories.children) {
      const active = decodeURIComponent(atob(button.dataset.category)) === selected;
      button.setAttribute("aria-pressed", String(active));
      button.classList.toggle("active", active);
    }
    destination.querySelector("summary").textContent = selected ? `Tema: ${selected}` : "Filtrar por tema";
  };
  categories.addEventListener("click", updateSelection);
  // Quarto ya restaura el filtro al navegar por el historial.
  window.addEventListener("hashchange", updateSelection);
  updateSelection();
});
