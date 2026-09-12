import { createServer } from "node:http";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { SECTIONS } from "./sections.mjs";
import { createPost, listPosts, todayIso } from "./articles.mjs";
import { openLocalFile, openBrowser, previewPost } from "./quarto.mjs";

const directory = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(directory, "../..");
const port = Number(process.env.POST_LAUNCHER_PORT || 4318);
const assets = { "/": ["index.html", "text/html"], "/app.js": ["app.js", "text/javascript"], "/styles.css": ["styles.css", "text/css"] };

function json(response, status, body) {
  response.writeHead(status, { "content-type": "application/json; charset=utf-8", "cache-control": "no-store" });
  response.end(JSON.stringify(body));
}
async function readBody(request) {
  let body = "";
  for await (const chunk of request) {
    body += chunk;
    if (Buffer.byteLength(body) > 100000) throw new Error("Solicitud demasiado grande.");
  }
  return JSON.parse(body || "{}");
}

const server = createServer(async (request, response) => {
  try {
    const host = request.headers.host;
    if (!["127.0.0.1:" + port, "localhost:" + port].includes(host)) return json(response, 403, { error: "Host no permitido." });
    if (request.headers.origin && request.headers.origin !== "http://" + host) return json(response, 403, { error: "Origen no permitido." });
    const url = new URL(request.url, "http://" + host);
    if (request.method === "GET") {
      if (url.pathname === "/api/posts") return json(response, 200, { posts: listPosts(root), sections: SECTIONS, today: todayIso() });
      const asset = assets[url.pathname];
      if (!asset) return json(response, 404, { error: "No encontrado." });
      response.writeHead(200, { "content-type": asset[1] + "; charset=utf-8", "cache-control": "no-store" });
      return response.end(fs.readFileSync(path.join(directory, "web", asset[0])));
    }
    if (request.method !== "POST") return json(response, 405, { error: "Método no permitido." });
    const payload = await readBody(request);
    if (url.pathname === "/api/create") return json(response, 201, { post: createPost(root, payload), message: "Borrador creado." });
    if (url.pathname === "/api/open") {
      openLocalFile(root, payload.index, payload.target === "directory");
      return json(response, 200, { message: "Abierto en tu equipo." });
    }
    if (url.pathname === "/api/preview") {
      previewPost(root, payload.index);
      return json(response, 200, { message: "Se abrirá quarto preview en su propia ventana. Guarda para actualizar; Ctrl+C para detener." });
    }
    return json(response, 404, { error: "No encontrado." });
  } catch (error) { json(response, 400, { error: error.message }); }
});
server.on("error", error => {
  console.error(error.code === "EADDRINUSE" ? "El editor ya está abierto: http://127.0.0.1:" + port : error.message);
  process.exitCode = 1;
});
server.listen(port, "127.0.0.1", () => {
  const url = "http://127.0.0.1:" + port;
  console.log("Artículos del blog: " + url);
  console.log("Deja esta ventana abierta. Ctrl+C para cerrar.");
  if (!process.argv.includes("--no-open")) openBrowser(url);
});
