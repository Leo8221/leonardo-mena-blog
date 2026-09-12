# Lanzador de artículos

Abre `crear-articulo.cmd` desde la raíz.
El flujo nativo de Quarto y GitHub Pages está en [GUIA-EDITORIAL.md](../../GUIA-EDITORIAL.md).

## Mantenimiento

- `server.mjs`: HTTP local y rutas de la interfaz.
- `articles.mjs`: creación de archivos y lista de artículos.
- `sections.mjs`: series disponibles.
- `quarto.mjs`: abrir archivos o iniciar el preview nativo.
- `web/`: HTML, CSS y JavaScript.
- `../preview-article.ps1`: acceso a `quarto preview --profile editor`.
- `../runtime.ps1`: entorno común de R/Quarto.

El servidor escucha en 127.0.0.1:4318. Usa `POST_LAUNCHER_PORT` para cambiar
el puerto y `--no-open` para iniciarlo sin abrir el navegador.

Pruebas en carpetas temporales:

```powershell
node --test tools/post-launcher/articles.test.mjs
```
