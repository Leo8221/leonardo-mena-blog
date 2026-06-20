# Implementation Plan

Plan de trabajo para modernizar el blog y Atlas sin migrar fuera de Quarto ni romper rutas publicas.

## Diagnostico inicial

- Repo en `main`, sincronizado con `origin/main` al iniciar esta fase.
- `AGENTS.md` y este plan no existian.
- `goal.md` existe como especificacion local; `goal.md.txt` esta vacio.
- `quarto render` falla dentro del sandbox por permisos al ejecutar `dart-sass`, pero compila fuera del sandbox.
- La compilacion local usa Quarto `1.8.26`; el `docs/` publicado venia de Quarto `1.9.38`, por lo que el render local produce churn grande de HTML, hashes y librerias. Ese churn se guardo en stash como `baseline quarto 1.8 docs render`.
- Atlas ya esta modularizado en `atlas/js/`, con datos generados y guia basica, pero aun no cumple todas las fases del goal.
- El sistema visual del blog sigue concentrado en `styles.css`, pero ya existe una fuente compartida de tokens en `assets/css/tokens.css`.
- Atlas ya consume los alias de marca desde `assets/css/tokens.css` en vez de declarar su propia paleta base.
- `_quarto.yml` ya no tiene CTA de suscripcion con HTML inline.
- El generador del Atlas conserva `question`, `insight` y metodologia completa en el JSON publico, y valida IDs, metricas, series, fechas y texto con codificacion sospechosa.
- La portada del Atlas ya esta agrupada por preguntas y no como cuadricula plana.
- `share-buttons-auto.html` ya no contiene logs de depuracion ni reintentos.
- `reading-progress.html` ya respeta `prefers-reduced-motion` y usa `requestAnimationFrame`.
- El workflow de publicacion ahora se dispara en `push` a `main` para fuentes del sitio, regenera datos del Atlas antes del render y fija Quarto `1.9.38`, igual que la salida actual en `docs/`.

## Objetivos

- Mantener Quarto, rutas publicas, articulos `.qmd`, R y GitHub Pages desde `docs/`.
- Consolidar una identidad editorial compartida entre blog y Atlas.
- Mejorar portada, navegacion, articulos, paginas de archivo/series, Sobre mi y Suscripcion.
- Hacer Atlas mas guiado, accesible, exportable y mantenible.
- Asegurar build reproducible, validacion visual y documentacion suficiente para continuar sin depender de memoria.

## Fases

- [x] Fase 1: sistema de diseno compartido y `DESIGN-SYSTEM.md`.
- [x] Fase 2: cabecera y navegacion global sin CTA inline.
- [x] Fase 3: portada del blog con hero, Pulso RD, destacado, caminos, ultimas publicaciones, enfoque y boletin.
- [x] Fase 4: portada movil fluida desde 320 px.
- [x] Fase 5: secciones, archivo, categorias y paginas de series.
- [x] Fase 6: Sobre mi honesto, editorial y sin credenciales inventadas.
- [x] Fase 7: Suscripcion responsive, sin estilos inline innecesarios.
- [ ] Fase 8: articulos con mejor lectura, metadatos, figuras, captions y navegacion.
- [x] Fase 9: compartir y progreso de lectura accesibles.
- [x] Fase 10: identidad del Atlas como parte del sitio.
- [x] Fase 11: portada guiada del Atlas por preguntas.
- [x] Fase 12: busqueda y filtros del Atlas con accesibilidad y estados vacios.
- [x] Fase 13: metricas del Atlas visibles y correctas en movil.
- [x] Fase 14: modulos del Atlas con lectura, fuente, metodologia expandible y relacionados.
- [x] Fase 15: alternativas textuales/tabulares para graficos Canvas.
- [x] Fase 16: exportacion PNG y CSV con nombres y metadatos correctos.
- [x] Fase 17: modales con foco, escape, focus trap y retorno de foco.
- [x] Fase 18: responsive del Atlas en 320, 360, 390, 430, 768, 920, 1024, 1180 y 1440 px.
- [x] Fase 19: accesibilidad global, `:focus-visible` y `prefers-reduced-motion`.

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
- [x] Creado `DESIGN-SYSTEM.md`.
- [x] Creado `assets/css/tokens.css` como fuente compartida para blog y Atlas.
- [x] Eliminado CTA inline del navbar; ahora se estiliza desde CSS.
- [x] Footer sin estilo inline y enlaces externos con `rel`.
- [x] Workflow actualizado para regenerar datos del Atlas antes de publicar y observar cambios en `assets/**`.
- [x] Consolidada documentacion del Atlas: `README.md` corto, `GUIDE.md` unica guia operativa y `GUIA.md` eliminado.
- [x] Agregado `atlas/scripts/check-text-integrity.mjs` para detectar mojibake, caracteres de reemplazo y UTF-8 invalido.
- [x] Workflow actualizado para validar texto publicado despues de `quarto render`.
- [x] Portada del Atlas organizada por preguntas con familia, tipo, pregunta, resumen, lectura, fuente, corte y CTA.
- [x] Modulos del Atlas muestran pregunta, resumen, lectura y ficha fuente con metodologia expandible.
- [x] Busqueda/filtros del Atlas con contador, limpiar busqueda, estados vacios, `aria-current`, `aria-pressed` y cierre movil con Escape/clic fuera.
- [x] Metric strip del Atlas visible en movil con scroll horizontal, contexto textual y estado activo.
- [x] Modulos del Atlas agregan vista tabular, CSV por dataset y CSV completo con metadatos de modulo, fuente, corte y generacion.
- [x] Modal de pantalla completa del Atlas devuelve foco, cierra con Escape y atrapa Tab dentro del dialogo.
- [x] Agregado skip link global para paginas Quarto y skip link propio del Atlas.
- [x] Agregado contrato `atlas/scripts/check-site-contract.mjs` y validacion en GitHub Actions.
- [x] Suscripcion convertida a pagina responsive sin `iframe` de ancho fijo ni estilos inline.
- [x] Controles moviles del Atlas subidos a objetivo tactil minimo compartido.
- [x] Ejecutar inspeccion visual completa de Atlas, Sobre mi y Suscribete.
- [x] Validada portada guiada del Atlas en escritorio y captura movil Edge headless a 500 px; Edge headless a 390 px recorta una viewport interna mayor, asi que no se uso como evidencia visual definitiva.
- [x] Secciones, series y archivo revisados para textos visibles con tildes, jerarquia editorial y sin estilos inline en fuente.
- [x] Sobre mi ampliado con principios editoriales, temas y rutas de exploracion sin inventar credenciales.
- [x] Suscripcion completada con frecuencia honesta, fallback a Substack y nota breve de privacidad.
- [x] Portada del blog actualizada con Pulso RD cargado desde `atlas/data/atlas-data.json`, caminos editoriales, entrada rapida al Atlas, bloque de enfoque y boletin.
- [x] Portada movil corregida sin ancho artificial de 360 px, sin ocultar imagen destacada y con cinta superior simplificada para evitar recortes.
- [x] `assets/js/home-pulse.js` agregado como recurso Quarto y cubierto por el contrato del sitio.

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
- `assets/css/tokens.css`
- `DESIGN-SYSTEM.md`
- `reading-progress.html`
- `share-buttons-auto.html`
- `tema_graficos.R`
- `.github/workflows/actualizar_observatorio.yml`
- `atlas/index.html`
- `atlas/styles.css`
- `atlas/app.js`
- `atlas/js/*.js`
- `atlas/data/atlas-source.json`
- `atlas/scripts/build-atlas-data.mjs`
- `atlas/scripts/check-text-integrity.mjs`
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
- [x] `node atlas/scripts/build-atlas-data.mjs`
- [x] `Rscript atlas/scripts/build-article-visuals.R` pasa con advertencias locales conocidas de locale y `st_simplify`.
- [x] `node --check` para JS del Atlas.
- [x] `node atlas/scripts/check-text-integrity.mjs`
- [x] `node atlas/scripts/check-text-integrity.mjs --include-docs`
- [x] Validar que no haya mojibake en fuente ni salida despues de `quarto render`.
- [x] `quarto render --no-cache`
- [x] QA local de Atlas: portada por defecto, grupos por pregunta, modulo por hash, ficha fuente, metodologia expandible y consola limpia.
- [x] Probar portada y Atlas en 320, 360, 390, 430, 768, 1024 y 1440 px.
- [x] Revisar consola JS en portada, articulo, Sobre mi, Suscribete y Atlas.
- [x] Revisar navegacion por teclado y foco visible.
- [x] Revisar consola JS en articulo validado: sin errores ni warnings.
- [x] QA local de Atlas: tablas por modulo, CSV disponible, modal fullscreen con foco/Escape/Tab y consola limpia.
- [x] `node atlas/scripts/check-site-contract.mjs`
- [x] `node atlas/scripts/check-site-contract.mjs --include-docs`
- [x] QA browser local en home, Sobre mi, Suscribete y Atlas: 320, 360, 390, 430, 768, 1024 y 1440 px sin overflow ni errores de consola.
- [x] QA browser local extra de Atlas: 920 y 1180 px sin overflow ni errores de consola.
- [x] QA browser local de modal Atlas en movil 390 px: abrir, canvas visible, foco en cerrar y cierre correcto.
- [x] QA browser local en Sobre mi, Archivo, Secciones, Suscribete, Republica habla de, Republica en un grafico y Series: 320, 390, 768 y 1440 px sin overflow ni errores de consola.
- [x] Verificacion puntual de iframe de Substack en 320 px: ancho computado 286 px dentro del contenedor.
- [x] `node --check assets/js/home-pulse.js`
- [x] QA Edge headless de portada servida por HTTP a 390 px: Pulso RD carga `Dolar spot`, `Inflacion interanual` y `Brecha laboral joven` desde JSON.
- [x] Captura Edge headless de portada movil a 390 px: header sin recorte visible, titulo de Pulso RD envuelve y CTA no se amontonan.

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
