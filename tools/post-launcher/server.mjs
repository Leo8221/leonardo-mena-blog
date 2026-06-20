import { createServer } from "node:http";
import fs from "node:fs";
import path from "node:path";
import { spawn } from "node:child_process";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const ROOT = path.resolve(__dirname, "../..");
const HOST = "127.0.0.1";
const PORT = Number(process.env.POST_LAUNCHER_PORT || 4318);

const SECTIONS = [
  {
    id: "republica-habla-de",
    label: "La Rep\u00fablica habla de",
    folder: "posts/republica-habla-de",
    category: "La Rep\u00fablica habla de",
    kind: "An\u00e1lisis"
  },
  {
    id: "republica-en-un-grafico",
    label: "La Rep\u00fablica en un gr\u00e1fico",
    folder: "posts/republica-en-un-grafico",
    category: "La Rep\u00fablica en un gr\u00e1fico",
    kind: "Visual"
  },
  {
    id: "fundamentos",
    label: "Fundamentos",
    folder: "posts/fundamentos",
    category: "Fundamentos",
    kind: "Escuela"
  },
  {
    id: "predecesores",
    label: "Mis predecesores",
    folder: "posts/predecesores",
    category: "Mis predecesores",
    kind: "Escuela"
  },
  {
    id: "mitos-economicos",
    label: "Mitos econ\u00f3micos",
    folder: "posts/mitos-economicos",
    category: "Mitos econ\u00f3micos",
    kind: "Borrador"
  }
];

function todayIso() {
  return new Date().toISOString().slice(0, 10);
}

function slugify(value) {
  return String(value || "")
    .normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "")
    .replace(/[^a-zA-Z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "")
    .replace(/-{2,}/g, "-")
    .toLowerCase()
    .slice(0, 80);
}

function yamlQuote(value) {
  return `"${String(value || "").replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`;
}

function frontMatterList(items) {
  return items.map((item) => `  - ${yamlQuote(item)}`).join("\n");
}

function normalizeCategories(section, rawCategories) {
  const extra = String(rawCategories || "")
    .split(",")
    .map((item) => item.trim())
    .filter(Boolean);
  return [...new Set([section.category, ...extra])];
}

function relativeIncludePrefix() {
  return "../../../";
}

function buildTemplate({ postRelativePath, title, date, description, categories, draft }) {
  const prefix = relativeIncludePrefix();
  return `---
title: ${yamlQuote(title)}
date: ${yamlQuote(date)}
author: "Leonardo Mena"
description: ${yamlQuote(description)}
categories:
${frontMatterList(categories)}
draft: ${draft ? "true" : "false"}
format:
  html:
    toc: true
    toc-location: left
include-after-body:
  - ${prefix}reading-progress.html
  - ${prefix}share-buttons-auto.html
---

\`\`\`{r setup, include=FALSE}
library(tidyverse)
library(scales)
library(here)

source(here::here("tema_graficos.R"))

post_dir <- here::here("${postRelativePath}")
\`\`\`

## Punto de partida

Escribe aqui la pregunta del articulo.

## Que miro

Explica el dato, la fuente y el periodo.

## Grafico o evidencia

\`\`\`{r}
# df <- readr::read_csv(file.path(post_dir, "data", "datos.csv"), show_col_types = FALSE)
# df
\`\`\`

## Lectura

Cuenta que cambia y que no se puede concluir todavia.

## Cierre

Deja una idea final o una pregunta abierta.
`;
}

function assertInsideRoot(targetPath) {
  const resolved = path.resolve(targetPath);
  if (resolved !== ROOT && !resolved.startsWith(`${ROOT}${path.sep}`)) {
    throw new Error("Ruta fuera del repositorio.");
  }
  return resolved;
}

function createPost(payload) {
  const section = SECTIONS.find((item) => item.id === payload.section);
  if (!section) throw new Error("Seccion no valida.");

  const title = String(payload.title || "").trim();
  if (!title) throw new Error("Falta el titulo.");

  const date = String(payload.date || todayIso()).trim();
  if (!/^\d{4}-\d{2}-\d{2}$/.test(date)) throw new Error("La fecha debe usar YYYY-MM-DD.");

  const baseSlug = slugify(payload.slug || title);
  if (!baseSlug) throw new Error("No pude generar un slug valido.");

  const folderName = `${date}-${baseSlug}`;
  const postDir = assertInsideRoot(path.join(ROOT, section.folder, folderName));
  const indexPath = path.join(postDir, "index.qmd");
  if (fs.existsSync(indexPath)) {
    throw new Error(`Ya existe ${path.relative(ROOT, indexPath)}.`);
  }

  const categories = normalizeCategories(section, payload.categories);
  const draft = payload.draft !== false;
  const description = String(payload.description || "").trim() || "Borrador de trabajo.";
  const postRelativePath = `${section.folder}/${folderName}`.replaceAll("\\", "/");
  const content = buildTemplate({ postRelativePath, title, date, description, categories, draft });

  fs.mkdirSync(postDir, { recursive: true });
  fs.writeFileSync(indexPath, content, "utf8");

  for (const name of ["data", "rds", "figures"]) {
    if (payload[name] !== false) fs.mkdirSync(path.join(postDir, name), { recursive: true });
  }

  return {
    section: section.label,
    directory: path.relative(ROOT, postDir).replaceAll("\\", "/"),
    index: path.relative(ROOT, indexPath).replaceAll("\\", "/"),
    absoluteIndex: indexPath,
    absoluteDirectory: postDir,
    draft
  };
}

function sendJson(response, statusCode, body) {
  response.writeHead(statusCode, {
    "content-type": "application/json; charset=utf-8",
    "cache-control": "no-store"
  });
  response.end(JSON.stringify(body));
}

function readBody(request) {
  return new Promise((resolve, reject) => {
    let body = "";
    request.on("data", (chunk) => {
      body += chunk;
      if (body.length > 100_000) {
        request.destroy();
        reject(new Error("Solicitud demasiado grande."));
      }
    });
    request.on("end", () => resolve(body));
    request.on("error", reject);
  });
}

function openTarget(targetPath) {
  const resolved = assertInsideRoot(targetPath);
  spawn("cmd", ["/c", "start", "", resolved], {
    detached: true,
    stdio: "ignore",
    windowsHide: true
  }).unref();
}

async function handleApi(request, response) {
  try {
    if (request.method === "GET" && request.url === "/api/sections") {
      sendJson(response, 200, { sections: SECTIONS, today: todayIso() });
      return;
    }

    if (request.method === "POST" && request.url === "/api/create") {
      const body = JSON.parse(await readBody(request) || "{}");
      sendJson(response, 201, { post: createPost(body) });
      return;
    }

    if (request.method === "POST" && request.url === "/api/open") {
      const body = JSON.parse(await readBody(request) || "{}");
      const target = body.target === "directory" ? body.directory : body.index;
      if (!target) throw new Error("Falta ruta para abrir.");
      openTarget(path.join(ROOT, target));
      sendJson(response, 200, { ok: true });
      return;
    }

    sendJson(response, 404, { error: "No encontrado." });
  } catch (error) {
    sendJson(response, 400, { error: error.message });
  }
}

function pageHtml() {
  return `<!doctype html>
<html lang="es">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Nuevo art&iacute;culo</title>
  <style>
    :root {
      --bg: #f6f4ee;
      --card: #fffdfa;
      --ink: #191b1f;
      --soft: #4e5968;
      --line: #d9d6cd;
      --terra: #c86448;
      --terra-dark: #a44933;
    }
    * { box-sizing: border-box; }
    body {
      margin: 0;
      background: var(--bg);
      color: var(--ink);
      font-family: Inter, system-ui, -apple-system, Segoe UI, sans-serif;
      line-height: 1.45;
    }
    main {
      width: min(920px, calc(100% - 32px));
      margin: 0 auto;
      padding: 34px 0 42px;
    }
    header {
      display: flex;
      align-items: end;
      justify-content: space-between;
      gap: 18px;
      border-bottom: 1px solid var(--line);
      padding-bottom: 18px;
    }
    h1 {
      margin: 0;
      font-family: Georgia, "Times New Roman", serif;
      font-size: clamp(2rem, 6vw, 3.5rem);
      line-height: 0.95;
    }
    p { color: var(--soft); }
    form {
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 16px;
      margin-top: 22px;
    }
    label {
      display: grid;
      gap: 7px;
      color: var(--soft);
      font-size: 0.78rem;
      font-weight: 800;
      letter-spacing: 0.06em;
      text-transform: uppercase;
    }
    input, select, textarea {
      width: 100%;
      min-height: 44px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: var(--card);
      color: var(--ink);
      font: inherit;
      padding: 10px 12px;
    }
    textarea {
      min-height: 92px;
      resize: vertical;
    }
    .full { grid-column: 1 / -1; }
    .checks {
      display: flex;
      flex-wrap: wrap;
      gap: 10px;
      align-items: center;
    }
    .checks label {
      min-height: 42px;
      display: inline-flex;
      align-items: center;
      gap: 8px;
      border: 1px solid var(--line);
      border-radius: 999px;
      background: var(--card);
      color: var(--ink);
      cursor: pointer;
      letter-spacing: 0;
      padding: 0 13px;
      text-transform: none;
    }
    .checks input { width: auto; min-height: auto; }
    button {
      min-height: 46px;
      border: 1px solid var(--terra);
      border-radius: 8px;
      background: var(--terra);
      color: #fff;
      cursor: pointer;
      font-weight: 850;
      padding: 0 18px;
    }
    button.secondary {
      background: var(--card);
      color: var(--terra-dark);
    }
    .actions {
      display: flex;
      flex-wrap: wrap;
      gap: 10px;
      align-items: center;
    }
    .result {
      display: none;
      margin-top: 18px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: var(--card);
      padding: 16px;
    }
    .result.is-visible { display: grid; gap: 12px; }
    code {
      overflow-wrap: anywhere;
      color: var(--terra-dark);
      font-size: 0.9rem;
      font-weight: 750;
    }
    .error { color: #8f2d20; font-weight: 800; }
    @media (max-width: 720px) {
      main { width: min(100% - 22px, 920px); padding-top: 18px; }
      header { display: grid; }
      form { grid-template-columns: 1fr; }
    }
  </style>
</head>
<body>
  <main>
    <header>
      <div>
        <h1>Nuevo art&iacute;culo</h1>
        <p>Elige la serie, crea la carpeta y empieza a escribir en <code>index.qmd</code>.</p>
      </div>
    </header>

    <form id="post-form">
      <label>Serie
        <select name="section" id="section"></select>
      </label>
      <label>Fecha
        <input name="date" id="date" type="date" required>
      </label>
      <label class="full">T&iacute;tulo
        <input name="title" id="title" required placeholder="Ej. Por que sube el transporte">
      </label>
      <label class="full">Slug opcional
        <input name="slug" id="slug" placeholder="se genera autom&aacute;tico si lo dejas vac&iacute;o">
      </label>
      <label class="full">Descripci&oacute;n
        <textarea name="description" id="description" placeholder="Una frase corta para portada, archivo y redes."></textarea>
      </label>
      <label class="full">Categor&iacute;as extra
        <input name="categories" id="categories" placeholder="Inflaci&oacute;n, transporte, mercado laboral">
      </label>
      <div class="checks full">
        <label><input type="checkbox" name="draft" checked> draft: true</label>
        <label><input type="checkbox" name="data" checked> data/</label>
        <label><input type="checkbox" name="rds" checked> rds/</label>
        <label><input type="checkbox" name="figures" checked> figures/</label>
      </div>
      <div class="actions full">
        <button type="submit">Crear art&iacute;culo</button>
      </div>
    </form>

    <section class="result" id="result" aria-live="polite"></section>
  </main>

  <script>
    const form = document.querySelector("#post-form");
    const result = document.querySelector("#result");
    const sectionSelect = document.querySelector("#section");
    const dateInput = document.querySelector("#date");
    let lastPost = null;

    function escapeHtml(value) {
      return String(value).replace(/[&<>"']/g, char => ({
        "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#039;"
      }[char]));
    }

    async function loadSections() {
      const response = await fetch("/api/sections");
      const data = await response.json();
      dateInput.value = data.today;
      sectionSelect.innerHTML = data.sections.map(section =>
        \`<option value="\${section.id}">\${escapeHtml(section.label)} · \${escapeHtml(section.kind)}</option>\`
      ).join("");
    }

    async function openTarget(target) {
      if (!lastPost) return;
      await fetch("/api/open", {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({
          target,
          index: lastPost.index,
          directory: lastPost.directory
        })
      });
    }

    form.addEventListener("submit", async (event) => {
      event.preventDefault();
      result.className = "result";
      result.textContent = "";

      const data = new FormData(form);
      const payload = Object.fromEntries(data.entries());
      payload.draft = data.has("draft");
      payload.data = data.has("data");
      payload.rds = data.has("rds");
      payload.figures = data.has("figures");

      const response = await fetch("/api/create", {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify(payload)
      });
      const body = await response.json();

      if (!response.ok) {
        result.className = "result is-visible";
        result.innerHTML = \`<p class="error">\${escapeHtml(body.error || "No se pudo crear.")}</p>\`;
        return;
      }

      lastPost = body.post;
      result.className = "result is-visible";
      result.innerHTML = \`
        <strong>Creado como borrador.</strong>
        <code>\${escapeHtml(lastPost.index)}</code>
        <div class="actions">
          <button class="secondary" type="button" data-open="file">Abrir index.qmd</button>
          <button class="secondary" type="button" data-open="directory">Abrir carpeta</button>
        </div>
        <p>Cuando est&eacute; listo, cambia <code>draft: true</code> a <code>draft: false</code> y renderiza el sitio.</p>
      \`;
    });

    result.addEventListener("click", (event) => {
      const button = event.target.closest("[data-open]");
      if (button) openTarget(button.dataset.open);
    });

    loadSections().catch((error) => {
      result.className = "result is-visible";
      result.innerHTML = \`<p class="error">\${escapeHtml(error.message)}</p>\`;
    });
  </script>
</body>
</html>`;
}

const server = createServer(async (request, response) => {
  if (request.url?.startsWith("/api/")) {
    await handleApi(request, response);
    return;
  }

  response.writeHead(200, {
    "content-type": "text/html; charset=utf-8",
    "cache-control": "no-store"
  });
  response.end(pageHtml());
});

function openBrowser(url) {
  if (process.argv.includes("--no-open")) return;
  spawn("cmd", ["/c", "start", "", url], {
    detached: true,
    stdio: "ignore",
    windowsHide: true
  }).unref();
}

if (process.argv.includes("--self-test")) {
  const sample = createPost({
    section: "republica-habla-de",
    title: "Prueba temporal",
    date: "2099-01-01",
    slug: "codex-self-test",
    draft: true,
    data: false,
    rds: false,
    figures: false
  });
  fs.rmSync(path.join(ROOT, sample.directory), { recursive: true, force: true });
  console.log("post launcher ok");
} else {
  server.listen(PORT, HOST, () => {
    const url = `http://${HOST}:${PORT}/`;
    console.log(`Launcher de articulos: ${url}`);
    openBrowser(url);
  });
}
