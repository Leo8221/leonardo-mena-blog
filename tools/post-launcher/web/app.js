const form = document.querySelector("#post-form");
const result = document.querySelector("#result");
const list = document.querySelector("#posts");
const search = document.querySelector("#search");
let posts = [];

function show(message, error = false) {
  result.textContent = message;
  result.classList.toggle("error", error);
}
async function api(url, payload) {
  const response = await fetch(url, payload === undefined ? {} : {
    method: "POST", headers: { "content-type": "application/json" }, body: JSON.stringify(payload)
  });
  const body = await response.json();
  if (!response.ok) throw new Error(body.error || "No se pudo completar la operación.");
  return body;
}
function renderPosts() {
  const query = search.value.toLocaleLowerCase("es");
  const matches = posts.filter(post => (post.title + " " + post.section).toLocaleLowerCase("es").includes(query));
  list.replaceChildren();
  for (const post of matches) {
    const article = document.createElement("article");
    const heading = document.createElement("h2");
    heading.textContent = post.title;
    const meta = document.createElement("p");
    meta.textContent = post.section + " · " + (post.draft ? "Borrador" : "Listo");
    const actions = document.createElement("div");
    actions.className = "actions";
    const buttons = [["open", "Editar"], ["preview", "Vista previa"], ["directory", "Abrir carpeta"]];
    for (const [action, label] of buttons) {
      const button = document.createElement("button");
      button.type = "button"; button.className = "secondary"; button.textContent = label;
      button.addEventListener("click", () => runAction(button, action, post));
      actions.append(button);
    }
    article.append(heading, meta, actions);
    list.append(article);
  }
  if (!matches.length) list.textContent = "No hay artículos con esa búsqueda.";
}
async function loadPosts() {
  const data = await api("/api/posts");
  posts = data.posts;
  const select = document.querySelector("#section");
  if (!select.options.length) {
    for (const section of data.sections) select.add(new Option(section.label, section.id));
  }
  if (!document.querySelector("#date").value) document.querySelector("#date").value = data.today;
  renderPosts();
}
async function runAction(button, action, post) {
  button.disabled = true;
  show(action === "preview" ? "Preparando vista previa…" : "Un momento…");
  try {
    const data = await api("/api/" + (action === "directory" ? "open" : action), { index: post.index, target: action === "directory" ? "directory" : "file" });
    show(data.message);
  } catch (error) { show(error.message, true); }
  finally { button.disabled = false; }
}
form.addEventListener("submit", async event => {
  event.preventDefault();
  const button = form.querySelector("button");
  button.disabled = true;
  try {
    const fields = new FormData(form);
    const payload = { ...Object.fromEntries(fields), withR: fields.has("withR") };
    const data = await api("/api/create", payload);
    show("Borrador creado. Busca «" + data.post.title + "» en la lista y pulsa Editar.");
    form.reset(); document.querySelector("#new-article").open = false;
    document.querySelector("#date").value = "";
    await loadPosts();
    search.value = data.post.title; renderPosts();
  } catch (error) { show(error.message, true); }
  finally { button.disabled = false; }
});
search.addEventListener("input", renderPosts);
loadPosts().catch(error => show(error.message, true));
