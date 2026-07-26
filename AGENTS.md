# AGENTS.md

Reglas persistentes para trabajar en este repositorio.

## Alcance

- Mantener Quarto como motor editorial.
- Mantener articulos en `.qmd`.
- Mantener R para graficos y procesamiento reproducible.
- Mantener Atlas con HTML, CSS y JavaScript nativo.
- Publicar por GitHub Pages desde `docs/`.
- No cambiar rutas publicas existentes sin una migracion explicita.

## Fuente vs generado

- Cambiar primero los archivos fuente: `.qmd`, `_quarto.yml`, `styles.css`, `atlas/`, `tema_graficos.R` y scripts.
- `docs/` es salida publicable. Actualizarlo solo como resultado de build o sincronizacion controlada de recursos estaticos.
- No editar exclusivamente `docs/` para corregir problemas permanentes.
- No publicar posts con `draft: true` ni contenido placeholder como investigacion real.

## Contenido y datos

- No inventar datos, fuentes, credenciales, cargos, premios ni conclusiones.
- Si una seccion no esta lista, ocultarla.
- Distinguir fecha de corte de datos y fecha tecnica de generacion.
- Los graficos del Atlas deben partir de datos reproducibles o archivos reales de articulos.

## Quarto, R y encoding

- No agregar `.Rprofile`, `.Renviron` ni cambios globales de locale/encoding al repo para "arreglar" renders locales. Si R arranca con `LC_CTYPE=C` o muestra avisos como `Setting LC_CTYPE=C.UTF-8 failed` en Windows, detenerse: ese entorno puede convertir acentos en literales `<U+00E9>` dentro de graficos SVG.
- En Windows, antes de renderizar desde una sesion automatizada, verificar que R lea UTF-8 correctamente con `l10n_info()[["UTF-8"]] == TRUE`. Si falla por variables `LC_ALL`, `LC_CTYPE` o `LANG` heredadas del shell, corregir la sesion de ejecucion, no el contenido generado ni los textos del articulo.
- Para renders y scripts R reproducibles en este repositorio, usar `powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\render-quarto.ps1 -QuartoArgs @('render')` y `powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-r.ps1`. Para Python, usar `powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-python.ps1`; el wrapper resuelve el Python instalado desde el registro y evita intérpretes embebidos de otras aplicaciones. Estos wrappers fijan las runtimes sin depender de cuál aparezca primero en el `PATH`.
- No corregir regresiones de encoding editando solo `docs/`, sustituyendo strings, cambiando SVG a PNG o restaurando outputs antiguos. La correccion permanente debe quedar en el archivo fuente o en el entorno real de render.

## Mapas y joins geograficos

- No hacer `left_join()` de mapas contra nombres con acentos, mayusculas o textos dependientes del encoding del shapefile. Crear claves estables y ASCII, por ejemplo con `janitor::make_clean_names()`, codigos oficiales o IDs reproducibles.
- Todo mapa que agregue regiones debe validar despues del join: si faltan regiones, si una geometria queda agrupada como `NA`, o si las metricas quedan incompletas, el chunk debe fallar con `stop()` antes de publicar.
- Caso de referencia: el mapa de MiPyMES se rompio cuando `TOPONIMIA` no coincidio con strings acentuados y se publico una sola geometria gris con `NA`. Esa clase de fallo debe detectarse en fuente y en revision visual, nunca resolverse restaurando `docs/`.

## Diseno

- Mantener identidad editorial: fondo crema, terracota, oliva, azul de datos, negro editorial y superficies blancas.
- Evitar apariencia de dashboard corporativo, portal gubernamental o plantilla tecnologica generica.
- No abusar de sombras, gradientes, bordes redondos ni animaciones.
- Priorizar movil desde 320 px.
- Usar estados `:focus-visible`, contraste suficiente y `prefers-reduced-motion`.

## Atlas

- Registrar modulos en `atlas/data/atlas-source.json`.
- Generar `atlas/data/atlas-data.json` con `node atlas/scripts/build-atlas-data.mjs`.
- Generar visuales de articulos con `Rscript atlas/scripts/build-article-visuals.R`.
- Mantener `atlas/js/` modular: configuracion, utilidades, renderers e interacciones.
- Si un modulo no esta listo: `visible: false` o `status` distinto de `Activo`.

## Validacion

- Antes de cerrar cambios relevantes, ejecutar lo que aplique:
  - `node atlas/scripts/build-atlas-data.mjs`
  - `Rscript atlas/scripts/build-article-visuals.R`
  - `node --check atlas/app.js`
  - `node --check atlas/js/config.js`
  - `node --check atlas/js/utils.js`
  - `node --check atlas/js/renderers.js`
  - `node --check atlas/js/interactions.js`
  - `node atlas/scripts/check-text-integrity.mjs`
  - `quarto render`
  - revision visual en desktop y movil cuando cambie UI.
- Despues de `quarto render`, si se regeneran SVGs con texto o mapas, revisar visualmente al menos las figuras afectadas. Para MiPyMES, confirmar especificamente:
  - el mapa muestra Norte, Sur, Este y Metropolitana con porcentajes reales, no `NA`;
  - las figuras SVG muestran acentos normales (`tamano` no debe aparecer como `<U+00F1>` ni texto equivalente roto);
  - `docs/posts/republica-en-un-grafico/2026-02-14-mipymes-rd/index_files/figure-html/fig-mapa-mipymes-regiones-1.svg` y `fig-acceso-credito-formal-tamano-1.svg` se ven correctos en navegador.
- Si una validacion no se ejecuta o solo pasa con advertencias, decirlo claramente.

## Git

- Revisar `git status` antes de editar y antes de cerrar.
- No revertir cambios de usuario.
- No hacer push sin validar y sin que el usuario lo pida o lo autorice.
- Si un build local genera churn de version en `docs/`, no mezclarlo con cambios fuente sin revisar.
