# Implementation Plan

Plan de trabajo para modernizar el blog y Atlas sin migrar fuera de Quarto ni romper rutas publicas.

## Diagnostico inicial

- Repo en `main`, sincronizado con `origin/main` al iniciar esta fase.
- `AGENTS.md` y este plan no existian.
- `goal.md` existe como especificacion local; `goal.md.txt` esta vacio.
- `quarto render` falla dentro del sandbox por permisos al ejecutar `dart-sass`, pero compila fuera del sandbox.
- La compilacion local usa Quarto `1.8.26`; el `docs/` publicado venia de Quarto `1.9.38`, por lo que el render local produce churn grande de HTML, hashes y librerias. Ese churn se guardo en stash como `baseline quarto 1.8 docs render`.
- Atlas ya esta modularizado en `atlas/js/`, con datos generados y guia basica, pero aun no cumple todas las fases del goal.
- El sistema visual del blog sigue concentrado en `styles.css`; Atlas usa tokens propios en `atlas/styles.css`.
- `_quarto.yml` todavia tiene CTA de suscripcion con HTML inline.
- `share-buttons-auto.html` ya no contiene logs de depuracion ni reintentos.
- `reading-progress.html` ya respeta `prefers-reduced-motion` y usa `requestAnimationFrame`.
- El workflow de publicacion ahora se dispara en `push` a `main` para fuentes del sitio y fija Quarto `1.9.38`, igual que la salida actual en `docs/`.

## Objetivos

- Mantener Quarto, rutas publicas, articulos `.qmd`, R y GitHub Pages desde `docs/`.
- Consolidar una identidad editorial compartida entre blog y Atlas.
- Mejorar portada, navegacion, articulos, paginas de archivo/series, Sobre mi y Suscripcion.
- Hacer Atlas mas guiado, accesible, exportable y mantenible.
- Asegurar build reproducible, validacion visual y documentacion suficiente para continuar sin depender de memoria.

## Fases

- [ ] Fase 1: sistema de diseno compartido y `DESIGN-SYSTEM.md`.
- [ ] Fase 2: cabecera y navegacion global sin CTA inline.
- [ ] Fase 3: portada del blog con hero, Pulso RD, destacado, caminos, ultimas publicaciones, Sobre Leonardo y boletin.
- [ ] Fase 4: portada movil fluida desde 320 px.
- [ ] Fase 5: secciones, archivo, categorias y paginas de series.
- [ ] Fase 6: Sobre mi honesto, editorial y sin credenciales inventadas.
- [ ] Fase 7: Suscripcion responsive, sin estilos inline innecesarios.
- [ ] Fase 8: articulos con mejor lectura, metadatos, figuras, captions y navegacion.
- [x] Fase 9: compartir y progreso de lectura accesibles.
- [ ] Fase 10: identidad del Atlas como parte del sitio.
- [ ] Fase 11: portada guiada del Atlas por preguntas.
- [ ] Fase 12: busqueda y filtros del Atlas con accesibilidad y estados vacios.
- [ ] Fase 13: metricas del Atlas visibles y correctas en movil.
- [ ] Fase 14: modulos del Atlas con lectura, fuente, metodologia expandible y relacionados.
- [ ] Fase 15: alternativas textuales/tabulares para graficos Canvas.
- [ ] Fase 16: exportacion PNG y CSV con nombres y metadatos correctos.
- [ ] Fase 17: modales con foco, escape, focus trap y retorno de foco.
- [ ] Fase 18: responsive del Atlas en 320, 360, 390, 430, 768, 920, 1024, 1180 y 1440 px.
- [ ] Fase 19: accesibilidad global, `:focus-visible` y `prefers-reduced-motion`.

## Avance actual

- [x] Revisado estado Git.
- [x] Inspeccionado arbol del proyecto.
- [x] Identificados fuente y salida generada.
- [x] Ejecutado baseline `quarto render` fuera del sandbox.
- [x] Registrado problema de version Quarto local vs salida publicada.
- [x] Creado `AGENTS.md`.
- [x] Creado `IMPLEMENTATION_PLAN.md`.
- [x] Limpieza de `share-buttons-auto.html`: enlaces reales, `rel`, copia de enlace y `navigator.share`.
- [x] Mejora de `reading-progress.html`: inicializacion unica, RAF y reduced motion.
- [x] Workflow actualizado para publicar en push con Quarto `1.9.38`.
- [x] Validacion browser movil de portada: sin overflow horizontal y header compacto.
- [x] Validacion browser movil/escritorio de articulo: compartir, progreso, consola limpia y fallback de copia manual.
- [ ] Ejecutar inspeccion visual completa de Atlas, Sobre mi y Suscribete.
- [ ] Consolidar documentacion duplicada de Atlas (`README.md`, `GUIDE.md`, `GUIA.md`).

## Archivos afectados previstos

- `_quarto.yml`
- `index.qmd`
- `secciones.qmd`
- `archivo.qmd`
- `categorias.qmd`
- `republica-habla-de.qmd`
- `republica-en-un-grafico.qmd`
- `about.qmd`
- `suscribete.qmd`
- `styles.css`
- `reading-progress.html`
- `share-buttons-auto.html`
- `tema_graficos.R`
- `atlas/index.html`
- `atlas/styles.css`
- `atlas/app.js`
- `atlas/js/*.js`
- `atlas/data/atlas-source.json`
- `atlas/scripts/build-atlas-data.mjs`
- `atlas/scripts/build-article-visuals.R`
- `.github/workflows/actualizar_observatorio.yml`
- `docs/` como salida generada/publicable.

## Riesgos

- Version local de Quarto distinta a la usada para generar `docs/`.
- Cambios en `docs/` pueden ser ruido de build si no se controlan.
- Figuras R pueden regenerarse con diferencias binarias.
- Cambiar estructura CSS de golpe puede romper articulos antiguos.
- Atlas usa Canvas; accesibilidad requiere alternativas HTML adicionales.
- GitHub Pages necesita recursos explicitamente incluidos cuando no salen del render normal.

## Pruebas y validaciones

- [x] `quarto render` fuera del sandbox: pasa con Quarto local `1.8.26`, pero genera churn no aceptado en `docs/`.
- [x] Definir version de Quarto para build reproducible en CI.
- [ ] `node atlas/scripts/build-atlas-data.mjs`
- [ ] `Rscript atlas/scripts/build-article-visuals.R`
- [ ] `node --check` para JS del Atlas.
- [ ] Validar que no haya mojibake en fuente ni salida.
- [ ] Probar portada y Atlas en 320, 360, 390, 430, 768, 1024 y 1440 px.
- [ ] Revisar consola JS en portada, articulo, Sobre mi, Suscribete y Atlas.
- [ ] Revisar navegacion por teclado y foco visible.
- [x] Revisar consola JS en articulo validado: sin errores ni warnings.

## Criterios de aceptacion

- Blog y Atlas comparten identidad visual sin perder su funcion.
- La portada comunica que es una publicacion personal de economia aplicada con Atlas como mesa de datos.
- Atlas tiene entrada clara, portada guiada, modulos legibles y controles accesibles.
- Mobile no tiene ancho artificial ni overflow accidental desde 320 px.
- Articulos siguen siendo Quarto y conservan contenido/rutas.
- Secciones incompletas y drafts no se publican.
- `docs/` queda publicable y generado con un proceso reproducible.
- No hay errores JS visibles ni logs temporales.
- Documentacion de mantenimiento permite agregar graficos/secciones sin rehacer el sistema.
