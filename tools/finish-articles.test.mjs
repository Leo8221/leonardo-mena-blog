import { test } from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { execFileSync } from "node:child_process";

test("el acabado es idempotente y conserva el contenido de los gráficos", () => {
  const output = fs.mkdtempSync(path.join(os.tmpdir(), "blog-charts-"));
  try {
    const figures = path.join(output, "posts", "example", "index_files", "figure-html");
    fs.mkdirSync(figures, { recursive: true });
    const chart = path.join(figures, "chart.svg");
    const svg = "<svg><g class='svglite'><text>Metropolitana: 91%, crédito</text><path d='M 1 2 L 3 4'/></g></svg>";
    fs.writeFileSync(chart, svg);
    const html = path.join(output, "index.html");
    fs.writeFileSync(html, '<nav class="navbar navbar-expand-lg"><button class="navbar-toggler" role="menu" aria-label="Navegación de palanca">Menú</button></nav><main class="content" id="quarto-document-content">Artículo</main>' +
      '<h5 class="no-anchor card-title listing-title">Tarjeta</h5>' +
      '<div class="thumbnail"><a href="post.html"><img class="thumbnail-image" src="cover.png"></a></div>' +
      '<h3 class="no-anchor listing-title"><a href="post.html">Una "idea" &amp; datos</a></h3>' +
      '<img class="thumbnail-image" src="chart.svg" alt="Crédito por región">');
    const run = () => execFileSync(process.execPath, ["tools/finish-articles.mjs"], {
      env: { ...process.env, QUARTO_PROJECT_OUTPUT_DIR: output }
    });
    run();
    const first = fs.readFileSync(chart, "utf8");
    const firstHtml = fs.readFileSync(html, "utf8");
    assert.match(first, /prefers-color-scheme: dark/);
    assert.equal(first.replace(/<style id="lm-chart-theme">[\s\S]*?<\/style>\n/, ""), svg);
    assert.match(firstHtml, /aria-label="Navegación principal"/);
    assert.doesNotMatch(firstHtml, /role="menu"/);
    assert.match(firstHtml, /aria-label="Contenido principal"/);
    assert.match(firstHtml, /<h3 class="no-anchor card-title listing-title">Tarjeta<\/h3>/);
    assert.match(firstHtml, /aria-label="Una &quot;idea&quot; &amp; datos"/);
    assert.match(firstHtml, /src="cover.png" alt=""/);
    assert.match(firstHtml, /alt="Crédito por región"/);
    run();
    assert.equal(fs.readFileSync(chart, "utf8"), first);
    assert.equal(fs.readFileSync(html, "utf8"), firstHtml);
  } finally {
    fs.rmSync(output, { recursive: true, force: true });
  }
});
