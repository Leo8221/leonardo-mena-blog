import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { TextDecoder } from "node:util";

const root = path.resolve(fileURLToPath(new URL("../..", import.meta.url)));
const includeDocs = process.argv.includes("--include-docs");
const decoder = new TextDecoder("utf-8", { fatal: true });
const textExtensions = new Set([
  ".css",
  ".html",
  ".js",
  ".json",
  ".md",
  ".mjs",
  ".qmd",
  ".r",
  ".txt",
  ".xml",
  ".yaml",
  ".yml"
]);

const ignoredDirs = new Set([
  ".git",
  ".quarto",
  ".vscode",
  ".agents",
  ".codex-remote-attachments",
  "_freeze",
  "_site",
  "observatorio_cache"
]);

const ignoredDocDirs = new Set(["site_libs"]);
const badPatterns = [
  { label: "replacement character", pattern: /\uFFFD/ },
  { label: "probable UTF-8 mojibake: C3", pattern: /\u00c3[\u0080-\u00BF]?/ },
  { label: "probable UTF-8 mojibake: C2", pattern: /\u00c2[\u0080-\u00BF]?/ },
  { label: "probable smart-quote mojibake", pattern: /\u00e2[\u0080-\u00BF\u20ac\u2122\u2019\u201c\u201d\u2013\u2014]?/ }
];

const issues = [];

walk(root);

if (issues.length > 0) {
  console.error("Text integrity check failed:");
  for (const issue of issues.slice(0, 80)) {
    console.error(`- ${issue.file}:${issue.line} ${issue.label}: ${issue.preview}`);
  }
  if (issues.length > 80) {
    console.error(`... ${issues.length - 80} more issue(s)`);
  }
  process.exit(1);
}

console.log(`Text integrity ok (${includeDocs ? "source + docs" : "source"})`);

function walk(dir) {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    if (entry.name === "goal.md" || entry.name === "goal.md.txt") continue;
    const fullPath = path.join(dir, entry.name);
    const relative = path.relative(root, fullPath).replace(/\\/g, "/");
    const parts = relative.split("/");

    if (entry.isDirectory()) {
      if (ignoredDirs.has(entry.name)) continue;
      if (!includeDocs && entry.name === "docs") continue;
      if (parts[0] === "docs" && ignoredDocDirs.has(entry.name)) continue;
      walk(fullPath);
      continue;
    }

    if (!entry.isFile()) continue;
    if (!textExtensions.has(path.extname(entry.name).toLowerCase())) continue;
    scanFile(fullPath, relative);
  }
}

function scanFile(fullPath, relative) {
  let text;
  try {
    text = decoder.decode(fs.readFileSync(fullPath));
  } catch {
    issues.push({
      file: relative,
      line: 1,
      label: "invalid utf-8",
      preview: "file cannot be decoded as UTF-8"
    });
    return;
  }

  const lines = text.split(/\r?\n/);
  for (const [index, line] of lines.entries()) {
    for (const { label, pattern } of badPatterns) {
      if (pattern.test(line)) {
        issues.push({
          file: relative,
          line: index + 1,
          label,
          preview: line.trim().slice(0, 160)
        });
      }
    }
  }
}
