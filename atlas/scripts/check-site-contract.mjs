import fs from "node:fs";
import path from "node:path";

const ROOT = process.cwd();
const includeDocs = process.argv.includes("--include-docs");
const failures = [];

function read(relativePath) {
  const fullPath = path.join(ROOT, relativePath);
  try {
    return fs.readFileSync(fullPath, "utf8");
  } catch (error) {
    failures.push(`${relativePath}: no se pudo leer (${error.message})`);
    return "";
  }
}

function requireText(relativePath, checks) {
  const content = read(relativePath);
  for (const [label, pattern] of checks) {
    const ok = pattern instanceof RegExp ? pattern.test(content) : content.includes(pattern);
    if (!ok) failures.push(`${relativePath}: falta ${label}`);
  }
}

function requireMatch(relativePath, checks) {
  const content = read(relativePath);
  for (const [label, pattern] of checks) {
    if (!pattern.test(content)) failures.push(`${relativePath}: falta ${label}`);
  }
}

function walk(relativePath, extensions = new Set([".js", ".html"])) {
  const fullPath = path.join(ROOT, relativePath);
  if (!fs.existsSync(fullPath)) return [];
  const entries = fs.readdirSync(fullPath, { withFileTypes: true });
  return entries.flatMap((entry) => {
    const child = path.join(relativePath, entry.name);
    if (entry.isDirectory()) return walk(child, extensions);
    return extensions.has(path.extname(entry.name)) ? [child] : [];
  });
}

function checkNoConsoleLogs() {
  const files = [
    "reading-progress.html",
    "share-buttons-auto.html",
    ...walk("atlas/js"),
    "atlas/app.js"
  ];
  for (const file of files) {
    const content = read(file);
    if (/\bconsole\.(log|debug|warn|error)\s*\(/.test(content)) {
      failures.push(`${file}: contiene console.*`);
    }
  }
}

requireText("_quarto.yml", [
  ["include-before-body: skip-link.html", "include-before-body: skip-link.html"],
  ["tokens como recurso", "assets/css/tokens.css"]
]);

requireText("skip-link.html", [
  ["enlace de salto al contenido", 'href="#quarto-content"'],
  ["clase skip-link", "skip-link"]
]);

requireText("styles.css", [
  ["estilos de skip link", ".skip-link"],
  ["focus-visible global", ":focus-visible"],
  ["prefers-reduced-motion", "prefers-reduced-motion"]
]);

requireText("atlas/index.html", [
  ["skip link propio", 'href="#atlas-main"'],
  ["main destino", 'id="atlas-main"']
]);

requireMatch("atlas/index.html", [
  ["cache busting de estilos", /styles\.css\?v=\d{8}/],
  ["cache busting de app", /app\.js\?v=\d{8}/]
]);

requireText("atlas/styles.css", [
  ["estilos de skip link", ".skip-link"],
  ["focus-visible global", ":focus-visible"],
  ["prefers-reduced-motion", "prefers-reduced-motion"]
]);

checkNoConsoleLogs();

if (includeDocs) {
  for (const file of ["docs/index.html", "docs/about.html", "docs/suscribete.html"]) {
    requireText(file, [
      ["skip link renderizado", "skip-link"],
      ["destino Quarto", "quarto-content"]
    ]);
  }
  requireText("docs/atlas/index.html", [
    ["skip link del Atlas publicado", 'href="#atlas-main"']
  ]);
  requireMatch("docs/atlas/index.html", [
    ["cache busting de estilos Atlas publicado", /styles\.css\?v=\d{8}/],
    ["cache busting de app Atlas publicado", /app\.js\?v=\d{8}/]
  ]);
}

if (failures.length) {
  console.error(`Site contract failed:\n- ${failures.join("\n- ")}`);
  process.exit(1);
}

console.log(`Site contract ok${includeDocs ? " (source + docs)" : " (source)"}`);
