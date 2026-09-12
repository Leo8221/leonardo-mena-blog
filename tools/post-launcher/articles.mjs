import fs from "node:fs";
import path from "node:path";
import { SECTIONS } from "./sections.mjs";

export function todayIso() {
  const now = new Date();
  return [now.getFullYear(), String(now.getMonth() + 1).padStart(2, "0"), String(now.getDate()).padStart(2, "0")].join("-");
}

export function slugify(value) {
  return String(value).normalize("NFD").replace(/[\u0300-\u036f]/g, "")
    .replace(/[^a-zA-Z0-9]+/g, "-").replace(/^-+|-+$/g, "").toLowerCase().slice(0, 80);
}

export function articlePath(root, relative) {
  const normalized = String(relative || "").replaceAll("\\", "/");
  if (!/^posts\/[^/]+\/[^/]+\/index\.qmd$/.test(normalized)) throw new Error("Selecciona un index.qmd de posts.");
  const full = path.resolve(root, normalized);
  const realRoot = fs.realpathSync(root);
  const real = fs.realpathSync(full);
  if (!real.startsWith(realRoot + path.sep)) throw new Error("El artículo está fuera del proyecto.");
  return full;
}

export function buildTemplate({ title, date, description, categories, relative, withR }) {
  const yaml = [
    "---", "title: " + JSON.stringify(title), "date: " + JSON.stringify(date),
    'author: "Leonardo Mena"', "description: " + JSON.stringify(description),
    "categories:", ...categories.map(item => "  - " + JSON.stringify(item)), "draft: true",
    "format:", "  html:", "    toc: true", "    toc-location: left",
    "include-after-body:", "  - ../../../reading-progress.html", "  - ../../../share-buttons-auto.html", "---", ""
  ];
  if (withR) yaml.push(
    "```{r}", "#| label: setup", "#| include: false",
    'source(here::here("tema_graficos.R"))',
    "post_dir <- here::here(" + JSON.stringify(relative) + ")",
    '# Lee tus datos desde file.path(post_dir, "data", "archivo.csv").', "```", ""
  );
  yaml.push("<!-- Escribe tu artículo aquí. Puedes usar el editor visual de Quarto. -->", "");
  return yaml.join("\n");
}

export function createPost(root, payload) {
  const section = SECTIONS.find(item => item.id === payload.section);
  if (!section) throw new Error("Elige una serie válida.");
  const title = String(payload.title || "").trim();
  if (!title || title.length > 300) throw new Error("Escribe un título de hasta 300 caracteres.");
  const date = String(payload.date || todayIso());
  if (!/^\d{4}-\d{2}-\d{2}$/.test(date) || Number.isNaN(Date.parse(date)) ||
      new Date(date).toISOString().slice(0, 10) !== date) throw new Error("La fecha no es válida.");
  const slug = slugify(payload.slug || title);
  if (!slug) throw new Error("El título necesita al menos una letra o un número.");
  const relative = "posts/" + section.id + "/" + date + "-" + slug;
  const parent = path.join(root, "posts", section.id);
  fs.mkdirSync(parent, { recursive: true });
  if (!fs.realpathSync(parent).startsWith(fs.realpathSync(root) + path.sep)) throw new Error("Serie fuera del proyecto.");
  const directory = path.join(root, relative);
  const categories = [...new Set([section.category, ...String(payload.categories || "").split(",").map(s => s.trim()).filter(Boolean)])];
  const content = buildTemplate({ title, date, description: String(payload.description || "").trim(),
    categories, relative, withR: payload.withR === true });
  fs.mkdirSync(directory); // Falla si ya existe; nunca sobrescribe un artículo.
  fs.writeFileSync(path.join(directory, "index.qmd"), content, { encoding: "utf8", flag: "wx" });
  if (payload.withR === true) for (const folder of ["data", "figures"]) fs.mkdirSync(path.join(directory, folder));
  return { title, index: relative + "/index.qmd", directory: relative, draft: true };
}

function summary(file, relative) {
  const content = fs.readFileSync(file, "utf8");
  const yaml = content.match(/^\uFEFF?---\r?\n([\s\S]*?)\r?\n---/);
  const titleLine = yaml?.[1].match(/^title:\s*(.+)$/m)?.[1]?.trim();
  let title = titleLine || path.basename(path.dirname(file));
  if (title.startsWith('"')) { try { title = JSON.parse(title); } catch {} }
  else if (title.startsWith("'") && title.endsWith("'")) title = title.slice(1, -1).replaceAll("''", "'");
  return { index: relative, title, draft: /^draft:\s*true\s*$/m.test(yaml?.[1] || "") };
}

export function listPosts(root) {
  const posts = [];
  for (const section of SECTIONS) {
    const folder = path.join(root, "posts", section.id);
    if (!fs.existsSync(folder)) continue;
    for (const entry of fs.readdirSync(folder, { withFileTypes: true })) {
      if (!entry.isDirectory()) continue;
      const relative = "posts/" + section.id + "/" + entry.name + "/index.qmd";
      const file = path.join(root, relative);
      if (fs.existsSync(file)) posts.push({ ...summary(file, relative), section: section.label });
    }
  }
  return posts.sort((a, b) => b.index.split("/")[2].localeCompare(a.index.split("/")[2]));
}
