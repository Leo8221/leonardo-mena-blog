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
- Si una validacion no se ejecuta o solo pasa con advertencias, decirlo claramente.

## Git

- Revisar `git status` antes de editar y antes de cerrar.
- No revertir cambios de usuario.
- No hacer push sin validar y sin que el usuario lo pida o lo autorice.
- Si un build local genera churn de version en `docs/`, no mezclarlo con cambios fuente sin revisar.
