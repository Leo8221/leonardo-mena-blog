import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { createPost, listPosts, articlePath } from "./articles.mjs";

function fixture(t) {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "blog-articles-"));
  t.after(() => fs.rmSync(root, { recursive: true, force: true }));
  return root;
}
const payload = { section: "fundamentos", title: 'Inflación: "precios" y empleo', date: "2026-09-12", description: "Primera línea\nSegunda línea" };

test("crear texto simple no requiere R y siempre empieza en borrador", t => {
  const root = fixture(t), post = createPost(root, { ...payload, draft: false });
  const text = fs.readFileSync(path.join(root, post.index), "utf8");
  assert.ok(text.includes("draft: true"));
  assert.ok(!text.includes("```{r"));
  assert.ok(text.includes('description: "Primera línea\\nSegunda línea"'));
  assert.equal(listPosts(root)[0].title, payload.title);
  assert.deepEqual(fs.readdirSync(path.dirname(path.join(root, post.index))), ["index.qmd"]);
});

test("plantilla R y recursos se crean solo al solicitarlos", t => {
  const root = fixture(t), post = createPost(root, { ...payload, withR: true });
  const directory = path.join(root, post.directory);
  assert.ok(fs.readFileSync(path.join(directory, "index.qmd"), "utf8").includes("tema_graficos.R"));
  assert.ok(fs.existsSync(path.join(directory, "data")));
  assert.ok(fs.existsSync(path.join(directory, "figures")));
});

test("un título repetido no sobrescribe texto existente", t => {
  const root = fixture(t), post = createPost(root, payload);
  fs.appendFileSync(path.join(root, post.index), "\nTexto ya escrito.");
  assert.throws(() => createPost(root, payload));
  assert.ok(fs.readFileSync(path.join(root, post.index), "utf8").endsWith("Texto ya escrito."));
});

test("se rechazan fechas imposibles y secciones ajenas", t => {
  const root = fixture(t);
  assert.throws(() => createPost(root, { ...payload, date: "2026-02-31" }), /fecha/);
  assert.throws(() => createPost(root, { ...payload, section: "../../" }), /serie/);
  assert.throws(() => articlePath(root, "../index.qmd"), /index.qmd/);
});

test("una carpeta enlazada fuera del proyecto no permite crear artículos", t => {
  const root = fixture(t), outside = fixture(t);
  fs.mkdirSync(path.join(root, "posts"));
  fs.symlinkSync(outside, path.join(root, "posts/fundamentos"), process.platform === "win32" ? "junction" : "dir");
  assert.throws(() => createPost(root, payload), /fuera/);
  assert.deepEqual(fs.readdirSync(outside), []);
});
