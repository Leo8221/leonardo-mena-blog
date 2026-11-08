# Reorganización editorial — 7 de noviembre de 2026

## Solicitud y decisiones

Actualizar desde GitHub y simplificar carpetas y organización para retomar la
publicación regular. Se conserva Quarto, las rutas públicas, las carpetas de
investigación y los proyectos existentes.

Estado inicial limpio, rama `main`, HEAD `6bb98e31`. El pull con avance directo
detectó divergencia: dos commits locales y actualizaciones remotas hasta
`018fb0f7`. Se integraron ambos historiales en `d2abab08`.
El único conflicto fue `docs/atlas/js/app-modules.js`, eliminado localmente y
modificado en remoto: se resolvió mediante sincronización controlada desde
`atlas/js/app-modules.js`, comprobando que coincidía con la versión remota.
Las demás eliminaciones locales se conservaron. No se hizo push.

## Cambios

- `editorial/`: guía, ideas y PDF de estilo que antes estaban sueltos en la raíz.
- `mantenimiento/`: sistema de diseño y mapa técnico; planes y registros previos
  quedan en `historial/` sin reescribir sus hechos históricos.
- `README.md`: entrada única para elegir tema, escribir, revisar y publicar.
- `escribir.code-workspace`: vista opcional de VS Code con Artículos y Editorial,
  y tareas para invocar el lanzador y el preview existentes desde la raíz.
- Actualizadas la guía del lanzador y las reglas de organización de `AGENTS.md`.

Los seis archivos trasladados se verificaron con SHA-256 antes y después del
movimiento; todos coincidieron. Después se amplió la guía editorial.
No se cambiaron fuentes `.qmd`, datos, estilos, plantillas ni rutas del sitio.
La inspección inicial de Quarto detectó 51 entradas, entre ellas ocho informes
independientes de `trabajo-siuben/`: el patrón `*.qmd` también los alcanzaba.
Se añadieron exclusiones explícitas de `trabajo-siuben/`, `editorial/` y
`mantenimiento/` al render del blog. Los informes conservan sus propios proyectos.

## Verificaciones y pendientes

- Diez pruebas existentes del lanzador y del estado de URL del Atlas: correctas.
  El primer intento pasó 9/10; el sandbox bloqueó la unión temporal de Windows
  de una prueba. La ejecución fuera del sandbox pasó 10/10.
- Integridad de texto en fuentes y `docs/`: correcta. Contrato del sitio en
  fuentes: correcto. Sintaxis JavaScript de 25 archivos: correcta.
- R: comprobación UTF-8 y parseo de `build-bcrd-live-data.R` correctos con el
  wrapper del proyecto. No se ejecutaron sus consultas ni cálculos.
- Veinte enlaces locales de las guías, carpetas y rutas de las dos tareas del
  workspace: correctos. JSON del workspace válido. No se probó la interfaz de
  VS Code; se verificaron su configuración y los programas existentes que invoca.
- Los cinco documentos trasladados sin edición conservan su contenido en Git
  (normalizando CRLF/LF en texto); el PDF conserva los mismos bytes.
- Primera inspección Quarto: completada con 51 entradas; identificó los ocho
  informes independientes. Quarto local es 1.10.18 y CI fija 1.9.38, por lo que
  la validación de render usa `.preview/organizacion-build`, sin mezclar cambios
  de versión en `docs/`.

- Inspección Quarto final: 43 entradas, todas en la raíz o en `posts/`. La
  diferencia contra la inspección anterior contiene exactamente los ocho
  informes de `trabajo-siuben/` y ninguna entrada añadida.

- Render de estructura: `quarto render --no-execute --output-dir
  .preview/organizacion-build` completado, 43/43 entradas, sin avisos WARN/ERROR
  en el log. No generó una carpeta `trabajo-siuben` en la salida temporal.
  Se usó `tools/render-quarto.ps1` desde PowerShell sin perfil.
- Quarto eliminó siete figuras versionadas en `posts/**/index_files/figure-html`
  durante esta prueba sin ejecución. Se restablecieron exclusivamente esos
  siete archivos desde HEAD; se había comprobado que no tenían cambios antes
  del render. Después, `posts/`, `docs/`, datos, investigaciones y estilos
  coinciden con el estado posterior al merge.
- `git diff --check`: correcto; solo avisos de conversión LF a CRLF de Git.

## Alcance de la validación y cierre

Cierre el 8 de noviembre de 2026. No se recalcularon los análisis R ni se
reconstruyeron los datos del Atlas: el render comprueba la estructura y la
exclusión de proyectos, no los resultados estadísticos ni la calidad visual
de las figuras. No se hizo revisión visual, ya que esta reorganización no
cambia la interfaz ni regenera gráficos. No se probó un despliegue en GitHub.

Evidencia local regenerable e ignorada por Git: `.preview/organizacion-inspect.json`,
`.preview/organizacion-inspect-final.json` y `.preview/organizacion-build.log`.
El sitio publicable `docs/` no recibió cambios de esta reorganización.

## Traslado institucional solicitado durante la revisión

El usuario aclaró que los informes SIUBEN no deben estar en el blog y señaló
`proyecto_v13` como destino. Se localizaron sus instrucciones y se conservó
su trabajo previo sin confirmar. Los informes originales y sus validaciones
se trasladaron a:

`C:\Users\leona\Archivo_Local\Escritorio\SIUBEN\prueba\nueva\review_v16\proyecto_v13\estudios\antecedentes-rsuh-202608`

Se movieron `trabajo-siuben/` y `research/rsuh-validacion/`: 664 archivos,
135.033.051 bytes. Todos coincidieron por SHA-256 antes y después del traslado.
Se preservó la estructura interna para conservar las rutas de los scripts y
se añadieron wrappers propios en el destino. El render del informe usa allí
la biblioteca R instalada del usuario; ya no depende de la biblioteca del blog.
El compendio más reciente de proyecto_v13 no se sobrescribió ni modificó.

El manifiesto y el recibo se guardan en
`proyecto_v13/outputs/traslado-informes-blog-20261108/`.
Las carpetas antiguas quedan ignoradas en el blog para evitar reintroducirlas.
Su historia anterior se conserva en Git local; no se reescribió ni publicó.

Comprobaciones del traslado:

- 31 scripts R parseados; UTF-8 y cinco dependencias R comprobadas en la
  biblioteca instalada del usuario, sin instalar paquetes.
- 12 scripts PowerShell parseados sin errores.
- Ejecutado `05_qa_resultados_territoriales.R` desde el paquete nuevo: correcto,
  sin consultas SQL ni regeneración de figuras. Reconcilia los resultados
  agregados existentes, no certifica nuevos datos institucionales.
- Conservación final: 663 archivos idénticos y el único ajuste previsto en
  `render.ps1`. Los wrappers y la guía del paquete son nuevos.
- Texto y contrato del blog correctos después del traslado; no cambian sus
  artículos, Atlas, estilos ni `docs/` con respecto al merge.
- El diff de proyecto_v13 conserva avisos de espacios finales en documentos
  históricos trasladados; no se reescribieron para limpiar el formato.

- La regresión general offline core de proyecto_v13 alcanzó su límite de
  300 segundos (salida 124). No se declara aprobada. Recibo:
  `outputs/verificacion_codigo/20261108T040712_330383Z_core` en ese proyecto.
- El traslado quedó confirmado en proyecto_v13 mediante el commit `fa9bcb3`,
  limitado al paquete nuevo. El resto de su estado Git anterior se conservó;
  solo se normalizó la expansión visual de la carpeta no versionada `estudios/`
  al comparar estados. Se usó para ese comando la identidad Git ya configurada
  en el blog, sin modificar configuraciones globales ni del proyecto destino.

Antes de un futuro push del blog hay que separar el historial local antiguo
que contiene SIUBEN (`6bb98e31`); eliminar carpetas del estado actual no purga
los commits previos. Esta sesión no reescribió historia ni hizo push y dejó
esa restricción explícita en `AGENTS.md`.
