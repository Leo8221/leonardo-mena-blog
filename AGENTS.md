# Reglas del repositorio

- Quarto genera el sitio en `docs/`; los artículos se escriben en `.qmd` y los
  gráficos reproducibles, en R. El núcleo analítico del Atlas usa R; Python
  ensambla y valida los datos públicos. HTML, CSS y JavaScript nativo presentan
  los resultados y resuelven la interacción en el navegador.
- Editar las fuentes. Actualizar `docs/` mediante render o sincronización
  controlada de recursos estáticos. No cambiar rutas públicas sin migración.
- Borradores y publicados comparten `posts/`; el encabezado `draft` determina
  su estado. No publicar borradores, placeholders ni datos o credenciales inventados.
- `README.md` explica cómo escribir, revisar y publicar. Documentar parámetros,
  contratos y decisiones no evidentes en las funciones: entradas, salidas,
  unidades, fórmulas y el recorrido entre archivos. Los comentarios deben ayudar
  al autor a entender y aprender siguiendo el código. No crear manuales que
  repitan el código ni diarios de tareas; registrar cambios y pruebas en el commit.
- Mantener los documentos de procedencia y metodología que acompañan datos reales.

## Datos y gráficos

- Distinguir el corte de los datos de la fecha de generación.
- Los módulos del Atlas se registran en `atlas/data/atlas-source.json`.
  Solo mostrar módulos activos con datos, fuente y fechas verificables.
- Para mapas, unir por códigos o claves ASCII estables. Fallar antes del render
  si faltan regiones, métricas o aparecen geometrías agrupadas como `NA`.
- Conservar la paleta crema, terracota, oliva y azul de `assets/css/tokens.css`.
  Mantener móvil desde 320 px, contraste, foco visible y movimiento reducido.
- Los activos destinados a LinkedIn deben exportarse sin metadatos C2PA.

## Ejecución y comprobaciones

- En Windows, usar `tools/render-quarto.ps1`, `tools/run-r.ps1` y
  `tools/run-python.ps1`. R debe devolver `l10n_info()[["UTF-8"]] == TRUE`.
  Corregir el entorno de la sesión si falla; no añadir `.Rprofile`/`.Renviron`,
  cambiar locales globales ni reparar textos o SVG generados para ocultarlo.
- Ejecutar las pruebas afectadas, `node --check` en JavaScript modificado y
  `node atlas/scripts/check-text-integrity.mjs`.
- Al cambiar datos del Atlas, ejecutar `tools/run-python.ps1 atlas/scripts/build_atlas_data.py`
  y `tools/run-python.ps1 atlas/scripts/test_atlas_data.py`
  y, si cambian sus visuales, `atlas/scripts/build-article-visuals.R` con el wrapper R.
- Comprobar el render con `tools/render-quarto.ps1` cuando cambien sus fuentes.
  Revisar el diff generado: no mezclar cambios de versión de Quarto sin examinarlos.
- Si cambia UI, comprobar escritorio y móvil. Para dashboards, PDF y Power BI,
  usar la skill visual correspondiente, abrir capturas y distinguir revisión
  visual de pruebas automáticas. Declarar avisos y comprobaciones no ejecutadas.
- Si se regeneran SVG, abrir las figuras afectadas. En MiPyMES, revisar
  `fig-mapa-mipymes-regiones-1.svg` y `fig-acceso-credito-formal-tamano-1.svg`:
  Norte, Sur, Este y Metropolitana deben tener porcentajes reales, sin `NA`,
  y los acentos deben verse bien, sin literales como `<U+00F1>`.

## Git y trabajo local

- Revisar el estado antes de editar y al cerrar. No revertir cambios del usuario.
  Crear un commit local limitado al trabajo propio verificado. No hacer push
  sin autorización. Conservar las rutas de proyectos y worktrees de Codex.
- Antes de retirar residuos, verificar que sean regenerables o copias comprobadas.
  No usar `git clean`, `reset`, borrados globales ni clones de respaldo rutinarios.
