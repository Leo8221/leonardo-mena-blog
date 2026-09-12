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

## Limpieza del sitio — 2026-09-12

- Base: pull fast-forward a 51bd9877, conservando el trabajo local previo. Se apartó un index.lock antiguo después de comprobar que no había procesos Git activos.
- Retirados los dos recorridos guiados y sus recursos; eliminados CSS y manejadores sin consumidores.
- Simplificados los textos de portada, Atlas, Sobre mí y suscripción; retirados el ejemplo de boletín y los encabezados duplicados. No se modificaron artículos ni definiciones de datos.
- Atlas restaura los valores por defecto cuando desaparecen parámetros de la URL, valida métricas/mapas/regiones y sincroniza la URL al limpiar filtros. Atrás también actualiza el título.
- Cinco pruebas de regresión: cuatro fallaban en la base y las cinco pasan después del cambio. Se ejecutan en CI junto con la comprobación de sintaxis JavaScript.
- Validaciones realizadas en una copia Git aislada: R UTF-8, build-atlas-data, build-article-visuals, sintaxis de todos los JS de Atlas y assets, contrato del sitio e integridad de texto en fuentes y docs, render completo de 44 documentos y renders de las dos páginas ajustadas después.
- build-article-visuals emite una advertencia previa de st_simplify sobre coordenadas geográficas. No se cambió su algoritmo como parte de esta limpieza.
- El render usa Quarto 1.9.37 y CI usa 1.9.38. La salida de validación y las capturas quedan en ../leonardo-mena-blog-cleanup-check/; no se mezcla el churn del build con docs/ del árbol principal, que ya tenía archivos eliminados y cambios previos.
- Revisión de navegador: portada, Sobre mí, suscripción y Atlas a 1440 y 320 px sin desbordamiento; nueve módulos cargados; CSV descargado; sin errores JavaScript ni recursos locales faltantes. Ampliación y Escape comprobados, modo oscuro revisado y SVG de MiPyMES inspeccionados: Norte, Sur, Este y Metropolitana con porcentajes, y tamaño/crédito con acentos correctos.
- No se hizo commit ni push. El build de validación usa los artículos de origin/main; no incorpora los cambios editoriales locales ajenos a esta tarea.

Verificación al incorporar: las cinco pruebas, la sintaxis JavaScript, el contrato y git diff --check pasan en el árbol principal. El control global de texto señala cinco coincidencias en dos librerías generadas previas y no versionadas de site_libs/ (cookie-consent.js y pdfmake.min.js). Esos archivos no se modificaron. En la copia limpia, el mismo control pasa para fuentes y docs.

## Refactorización y edición con Quarto — 2026-09-12

- Se conserva Quarto como motor editorial, sus borradores, perfiles y preview. La publicación sigue siendo el workflow existente y GitHub Pages desde docs/.
- styles.css pasa de 3802 líneas a un índice de imports; los estilos se separan por responsabilidad en assets/css/site/, conservando su orden y reglas. README.md documenta la distribución.
- Los renderers de Atlas se separan en series, barras, composición y dispersión. renderers.js conserva las filas compartidas; HTML y recursos de Quarto incluyen los nuevos módulos.
- El creador existente crear-articulo.cmd separa servidor, plantillas, integración con Quarto y recursos de interfaz. Crea siempre draft: true, evita sobrescribir artículos y solo prepara R cuando se solicita.
- Vista previa llama a quarto preview con el perfil editor; muestra borradores en .preview/ y no modifica docs/. El post-render respeta QUARTO_PROJECT_OUTPUT_DIR. No se añade un publicador ni otro ciclo de estados.
- R y Quarto comparten tools/runtime.ps1; se mantienen las rutas fijas y la comprobación UTF-8 por sesión. GUIA-EDITORIAL.md explica crear, revisar y publicar con las herramientas existentes.
- El control de texto excluye librerías y cachés generadas; ya no interpreta sus contenidos minificados como texto editorial.
- Pruebas: 10/10 (cinco de URL y cinco de creación de artículos); sintaxis JavaScript; contrato y texto de fuentes y docs; generación de datos y visuales del Atlas; render completo de 44 documentos.
- Runtime probado con Windows PowerShell 5 y PowerShell 7. Vista previa del borrador comprobada con el perfil nativo: contenido presente y snapshot de todos los archivos de docs/ intacto.
- Navegador: portada, Sobre mí, suscripción y Atlas a 320 y 1440 px; nueve módulos, CSV, ampliación/Escape, mapas y modo oscuro; sin errores JS ni recursos locales ausentes. El creador permite crear/buscar borradores en ambos tamaños, sin desbordamiento.
- Advertencias: build-article-visuals conserva el aviso previo de st_simplify en coordenadas geográficas. El render parcial de prueba del borrador avisa de imágenes de otros borradores que no se renderizaron en esa previsualización.
- Build y capturas de validación en ../leonardo-mena-blog-cleanup-check/. Se preservan los artículos, investigaciones y modificaciones locales de docs/ ajenos a esta limpieza; no se copia el churn de Quarto 1.9.37 al árbol principal. CI usa 1.9.38.
- El usuario autorizó commit, push y prueba en vivo; sus resultados se registran debajo.

### Publicación y prueba en vivo

- Push de fuentes: 0e4b2111. GitHub Actions 34674401520 completó el render sin caché, las diez pruebas y los controles de texto/contrato; generó docs/ en dcb59088. Pages 34674574871 finalizó correctamente.
- Chrome contra https://leo8221.github.io/leonardo-mena-blog/: portada, Sobre mí, suscripción y Atlas a 320/1440 px; nueve módulos; búsqueda nativa de Quarto; filtro del archivo (11 artículos, 2 al buscar MiPyMES); menús móviles; restablecer filtros; portapapeles; CSV y PNG; ampliación/Escape; tema y navegación Atrás. Sin errores JavaScript ni recursos propios ausentes.
- SVG de MiPyMES inspeccionados en vivo: cuatro regiones con porcentajes y textos tamaño/crédito con acentos correctos.
- Esta revisión detectó contraste insuficiente en enlaces del archivo y un pequeño desbordamiento a 320 px. Corrección en assets/css/site/listings.css y sincronización controlada del mismo recurso en docs/. Validación local en 320/1440 px, claro/oscuro: sin desbordamiento; contraste mínimo medido de 6.77:1 en títulos, descripciones, fechas y etiquetas. No se cambia el filtrado nativo.
- Capturas e informes: ../leonardo-mena-blog-cleanup-check/.quarto-live-qa/ y ../leonardo-mena-blog-publish-check/.quarto-contrast-qa/.
- Avisos del build remoto: st_simplify sobre coordenadas geográficas y deprecación de Node 20 en actions/checkout@v4; ambos no impidieron el render ni el despliegue. No se cambió el algoritmo de mapas ni se actualizaron dependencias como parte de esta corrección.

## Revisión de material publicado — 2026-09-12

- Retirados del seguimiento 175 archivos generados o locales: cachés de Quarto/R, historiales de R, desktop.ini y los intermedios del reporte de inclusión. Se añaden exclusiones para impedir su incorporación accidental. A petición del usuario, también se eliminan las copias físicas del árbol principal, las copias de validación y el respaldo temporal de esta revisión.
- Retirada la copia del informe CNZFE 2024: su página 2 incluye una nota restrictiva. Se enlaza el archivo oficial de informes; ningún script depende de ese PDF. También se elimina la copia local.
- El importador de tesis registra rutas relativas y resuelve el directorio personal mediante USERPROFILE. Los datos y sus hashes no cambian.
- Eliminados 46 encabezados decorativos de CSS, incluido «EDITORIAL PREMIUM». Las reglas coinciden antes y después al excluir comentarios y espacios; los recursos en docs/ se sincronizan desde fuente.
- Alcance documental: metadatos y primeras dos páginas de 352 PDF (cinco sin texto extraíble inicial), primeras 12 filas y hasta 80 celdas por hoja de 88 XLSX, y encabezados de 137 CSV. No es una lectura completa de cada documento o celda.
- No se encontraron claves privadas ni tokens con los patrones de alta confianza aplicados al texto actual y a 20 416 líneas añadidas del historial de archivos de código y configuración. Esto no certifica ausencia absoluta de información sensible. Los microdatos ENHOGAR tienen procedencia oficial documentada; no se eliminaron borradores ni datos de investigación por su mera presencia.
- Validación previa al push: diez pruebas de URL/creación de artículos, integridad de texto, contrato del sitio, sintaxis del importador y git diff --check correctos. Esta limpieza no cambia reglas CSS, gráficos ni ejecución editorial.
- Las retiradas afectan a la versión actual. No se reescribe el historial de Git: las copias de commits anteriores siguen disponibles. Los informes detallados de revisión quedan fuera del repositorio en ../leonardo-mena-blog-audit-artifacts/.
- Ampliación documental: búsqueda de avisos de uso interno en todas las páginas con texto de los 351 PDF conservados: 19 026 páginas procesadas, sin coincidencias adicionales ni errores de lectura; 1 781 páginas carecen de texto suficiente y no se sometieron a OCR. Se mantiene el límite de que esto no equivale a una revisión manual completa.
- Publicación: limpieza en b1ae4e1e y retirada local registrada en 44da6f5b. GitHub Actions 34676780379 completó generación de datos, render sin caché, pruebas y controles; Pages 34676872402 desplegó correctamente. El PDF retirado devuelve 404 en main.
- Prueba en vivo posterior: portada, Sobre mí, suscripción y Atlas a 320/1440 px; nueve módulos, búsqueda y archivo de Quarto, menús, filtros, enlace compartible, descargas y navegación Atrás. Sin errores JavaScript ni recursos propios ausentes. Las 197 modificaciones locales previas se verificaron sin cambios.

## Portada y estabilidad del tema — 2026-09-12

- Pull previo sin cambios pendientes del remoto. Trabajo realizado en la copia aislada para conservar las modificaciones editoriales locales.
- La navegación comparte padding y bordes entre Cosmo y Slate. Se elimina la altura mínima exclusiva del tema oscuro en móvil y el escalado del selector; las transiciones de enlaces quedan limitadas a colores. Se conserva el selector nativo de Quarto.
- Portada: presentación más compacta, nombre del autor visible, botones junto al texto y acceso principal al archivo. El artículo destacado precede a los indicadores en el documento, sin reordenación mediante CSS.
- Listados nativos con max-description-length (140/110 caracteres), fechas junto al texto y tarjetas sin altura sobrante. Las miniaturas de publicaciones recientes se mantienen en escritorio y se ocultan en móvil; la imagen del destacado permanece. Los tres indicadores se muestran completos en móvil, sin carrusel horizontal.
- Validación local: render de index.qmd con Quarto y comprobación previa UTF-8; diez pruebas existentes; contrato e integridad de texto en fuentes y docs; git diff --check. Capturas a 1440, 1024, 768, 390 y 320 px, claro/oscuro: sin desbordamiento y desplazamiento medido de 0 px al cambiar tema. También 0 px en Sobre mí, archivo y artículo MiPyMES.
- La portada pasa de 3131 a 2692 px en escritorio (1440 px) y de 4090 a 3181 px en móvil (390 px). El destacado es visible en la primera pantalla de escritorio. Las cifras son medidas de esta revisión, no objetivos fijos del diseño.
- Evidencia fuera del repositorio: ../leonardo-mena-blog-audit-artifacts/landing-change-local/. La salida local usa Quarto 1.9.37; el workflow genera y publica docs/ con 1.9.38. No se incluye el churn del render local en el commit de fuentes.
- Producción: fuentes publicadas en 3f35ca4e; workflow 34679454069 completado correctamente, con docs/ generado en c0e3a0fa. Pages 34679618760 finalizó correctamente.
- Prueba en vivo: mismo resultado de 0 px de movimiento durante 45 fotogramas del cambio de tema y al finalizarlo, en cinco anchuras de portada y en Sobre mí, archivo y MiPyMES. Búsqueda, filtro del archivo, menús, nueve módulos del Atlas, CSV/PNG, enlace compartible, ampliación/Escape y navegación Atrás correctos; sin errores JavaScript ni recursos propios ausentes. SVG de MiPyMES revisados visualmente.
- Main sincronizado con el build, conservando las 197 modificaciones previas, incluida la eliminación local de docs/search.json. Capturas e informes finales fuera del repositorio: ../leonardo-mena-blog-audit-artifacts/landing-change-live/ y landing-functions-live/.

## Selección automática de artículos en portada — 2026-09-12

- Eliminada la exclusión por nombre del destacado actual. Ambos listados de Quarto comparten contenidos y orden: fecha descendente y título ascendente como desempate.
- Quarto genera un destacado y cuatro candidatos para recientes; CSS oculta el primero de recientes porque ya se muestra destacado. Quedan tres tarjetas visibles sin repetición. Se conserva la disposición de la portada y no se añaden scripts, plantillas ni pasos editoriales.
- GUIA-EDITORIAL.md documenta que publicar un artículo no requiere editar exclusiones en index.qmd. Las dos exclusiones previas de contenido fuera del render del proyecto se conservan.
- Validación: render real de index.qmd; diez pruebas existentes; texto, contrato y diff correctos. Proyecto temporal fuera del repositorio probado en 1440/320 px: borrador oculto, nuevo artículo publicado que desplaza al anterior y desempate por título entre dos artículos de igual fecha. En todos los casos hay un destacado y tres recientes visibles con cuatro títulos únicos. La portada real conserva los cuatro artículos visibles anteriores.
- Evidencia: ../leonardo-mena-blog-audit-artifacts/home-auto-*.json. Los artículos ficticios solo existen en el proyecto temporal y no se publican.
- Publicación: fuentes en 806aae2f; workflow 34680465301 correcto, docs/ generado en 492edf51 y Pages 34680645611 correcto. Comprobación en vivo a 1440/320 px: cuatro candidatos en el listado nativo, un destacado y tres recientes visibles, con cuatro títulos únicos. Se retiró el proyecto temporal de pruebas; los informes quedaron fuera del repositorio. Los 197 cambios locales previos se conservaron al sincronizar.

## Escritura de artículos: encabezado y ejemplos

- Autor, índice izquierdo y funciones de compartir/progreso centralizados con la herencia nativa de posts/_metadata.yml. El creador solo escribe los datos editoriales; mantiene draft: true y R opcional.
- Guía ampliada con encabezado mínimo, enlaces externos e internos a .qmd, imágenes con pie y texto alternativo, fuentes, notas al pie y bibliografía opcional.
- Validación previa: diez pruebas existentes, integridad de texto, contrato del sitio y git diff --check correctos. Render Quarto real de un proyecto temporal externo: encabezado nuevo, encabezado antiguo y excepciones de autor/índice. Cada script compartido aparece exactamente una vez; enlace interno convertido a HTML, imagen y nota al pie correctos. No se añaden artículos de prueba al repositorio.
- Publicación completada: fuentes en 33a3e350 y 02c84576, integradas en 31eb4573; workflow 34681592355 correcto, docs/ generado en 4ac70dd6 y Pages 34681751871 correcto.
- Prueba de navegador del artículo mínimo: se corrigió body-class a la opción nativa body-classes. Los controles de compartir se crean solo si falta el bloque existente; las imágenes sencillas de Markdown también admiten ampliación. Chrome a 1440/320 px: autor e índice heredados, excepciones por artículo, una barra y un bloque de compartir, copia de enlace, progreso al desplazarse y ampliación/Escape correctos, sin errores JavaScript. Evidencia externa: editorial-inheritance-qa.json.
- Verificación final en vivo: Gráfico #0 y MiPyMES a 1440/320 px, un bloque de compartir y una barra de lectura por página, copia al portapapeles, ampliación/Escape y cambio de tema sin desplazamiento correctos. Portada, Sobre mí, suscripción, menús móviles, búsqueda, filtro del archivo y nueve módulos del Atlas correctos, sin errores JavaScript ni recursos propios ausentes en los recorridos probados. SVG de MiPyMES revisados visualmente: cuatro regiones con porcentajes reales y acentos normales. Evidencia externa: editorial-live/ y landing-functions-live/.
- Copia local sincronizada y 197 cambios previos conservados, verificados mediante SHA-256 o ausencia del archivo. La documentación y los controles de escritura están listos; no quedan tareas de esta mejora pendientes.

## Presentación personal

- about.qmd: presentación actualizada a «Soy Leonardo Mena, egresado de Economía de la UASD», según indicación del autor. Resto del texto conservado.
- Render de about.qmd e integridad de texto correctos. Pendiente comprobar la frase publicada tras el despliegue.
