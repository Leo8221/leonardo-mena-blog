import fs from "node:fs";
import path from "node:path";

// Quarto llama este paso tanto en preview como al publicar. El SVG lleva su
// CSS dentro porque una imagen no puede cargar la hoja de estilos de la página.
const output = path.resolve(process.env.QUARTO_PROJECT_OUTPUT_DIR || "docs");
const chartStyle = fs.readFileSync("assets/css/chart-svg.css", "utf8");
const style = `<style id="lm-chart-theme">${chartStyle}</style>`;

// Cada archivo pasa por una transformación local. Ningún cambio aquí debe
// recalcular una figura ni alterar el texto editorial que Quarto ya produjo.
for (const name of fs.readdirSync(output, { recursive: true })) {
  const relative = name.replaceAll("\\", "/");
  const file = path.join(output, name);
  if (relative.startsWith("posts/") && relative.includes("/figure-html/") && name.endsWith(".svg")) {
    const original = fs.readFileSync(file, "utf8");
    const updated = applyChartTheme(original);
    if (updated !== original) fs.writeFileSync(file, updated);
  }
  if (!name.endsWith(".html") || relative.startsWith("site_libs/") || relative.startsWith("atlas/")) continue;
  const original = fs.readFileSync(file, "utf8");
  const updated = improvePageHtml(original);
  if (updated !== original) fs.writeFileSync(file, updated);
}

function applyChartTheme(original) {
  if (!original.includes("class='svglite'")) return original;
  // Idempotente: actualizar el CSS nunca duplica estilos ni toca los datos.
  const svg = original.replace(/<style id="lm-chart-theme">[\s\S]*?<\/style>\n?/g, "");
  return svg.replace("</svg>", `${style}\n</svg>`);
}

function improvePageHtml(original) {
  // Corrige atributos de la plantilla de Quarto, también cuando no hay JS.
  let html = original
    .replace('<nav class="navbar ', '<nav aria-label="Navegación principal" class="navbar ')
    .replace('<main class="content" id="quarto-document-content">', '<main class="content" id="quarto-document-content" aria-label="Contenido principal">')
    .replace(/(<button class="navbar-toggler"[^>]*?) role="menu"/g, "$1")
    .replace('aria-label="Navegación de palanca"', 'aria-label="Abrir o cerrar el menú"')
    .replace('<i class="bi bi-graph-up" role="img">', '<i class="bi bi-graph-up" aria-hidden="true">');
  // Las páginas con page-layout: custom no reciben un <main> de Quarto.
  if (!/<main\b|role=["']main["']/.test(html)) {
    html = html.replace('<div id="quarto-content"', '<div role="main" aria-label="Contenido principal" id="quarto-content"');
  }
  return improveListingLinks(html);
}

function improveListingLinks(html) {
  // Quarto 1.9 genera estos patrones. Las expresiones se limitan a sus clases;
  // no son un analizador de HTML general ni deben abarcar contenido arbitrario.
  // Las tarjetas saltan a h5 aunque estén bajo un h2. Las miniaturas son enlaces:
  // usamos el título como nombre accesible y respetamos los alt que ya existen.
  html = html.replace(/<h5(\s+class="[^"]*\blisting-title\b[^"]*")>([\s\S]*?)<\/h5>/g, '<h3$1>$2</h3>');
  const titles = new Map([...html.matchAll(/<h3\s+class="[^"]*\blisting-title\b[^"]*">\s*<a href="([^"]+)"[^>]*>([\s\S]*?)<\/a>/g)]
    .map(([, href, title]) => [href, title.replace(/<[^>]+>/g, '').trim().replaceAll('"', '&quot;')]));
  html = html.replace(/(<div class="thumbnail"><a href="([^"]+)"[^>]*)(>)/g, (match, link, href, end) =>
    titles.has(href) && !link.includes('aria-label=') ? `${link} aria-label="${titles.get(href)}"${end}` : match);
  html = html.replace(/<img\b[^>]*class=["'][^"']*\bthumbnail-image\b[^"']*["'][^>]*>/g, (image) =>
    /\salt\s*=/.test(image) ? image : image.replace(/\s*\/?>(\s*)$/, ' alt="">$1'));
  return html;
}
