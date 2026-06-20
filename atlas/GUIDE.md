# Guía de trabajo del Atlas

Esta guía existe para que Atlas pueda crecer sin tocar toda la aplicación cada
vez. La regla base: el artículo explica; Atlas deja explorar datos reales. Si
algo no está listo, no se muestra.

## Qué tocar

- `atlas/data/atlas-source.json`: módulos principales, filtros, textos,
  fuentes, fechas de corte y series base.
- `atlas/scripts/build-atlas-data.mjs`: valida y publica el contrato final en
  `atlas/data/atlas-data.json`.
- `atlas/scripts/build-article-visuals.R`: transforma datos nacidos de artículos
  publicados.
- `atlas/app.js`: estructura de vistas, navegación, filtros e hidratación.
  También define `moduleDatasets()`, que alimenta tablas y CSV.
- `atlas/js/renderers.js`: motores de gráficos, mapas y canvas.
- `atlas/js/interactions.js`: enlace, PNG, modales y pantalla completa.
- `atlas/styles.css`: sistema visual propio del Atlas, usando tokens
  compartidos desde `assets/css/tokens.css`.
- `_quarto.yml`: solo si agregas un recurso nuevo que GitHub Pages debe copiar.

## Ruta rápida

```powershell
Rscript atlas/scripts/build-article-visuals.R
node atlas/scripts/build-atlas-data.mjs
quarto render
```

Antes de cerrar un cambio de JavaScript:

```powershell
node --check atlas/app.js
node --check atlas/js/config.js
node --check atlas/js/utils.js
node --check atlas/js/renderers.js
node --check atlas/js/interactions.js
```

Para revisar texto roto o problemas simples de acentos:

```powershell
node atlas/scripts/check-text-integrity.mjs
```

## Contrato de módulo

Cada módulo activo en `atlas/data/atlas-source.json` necesita:

- `id`: identificador estable, sin espacios.
- `title`: nombre visible.
- `visible: true`.
- `status: "Activo"`.
- `family`, `topic`, `type`.
- `summary`: resumen breve.
- `question`: pregunta que responde.
- `insight`: lectura principal.
- `source` y `sourceDetail`.
- `updated` si el corte difiere de `updated` global.
- `chart`: tipo que se renderiza en `atlas/app.js`.
- `methodology`: lista breve y verificable.
- `related`: rutas reales relacionadas.

El generador oculta borradores, valida IDs, valida campos esenciales, comprueba
que las métricas apunten a módulos visibles y conserva `question`, `summary`,
`insight`, `sourceInfo`, `methodology` y `related` en el JSON público.

## Agregar una sección

1. Crea o limpia los datos.
2. Si vienen de un artículo, agrega la transformación en
   `atlas/scripts/build-article-visuals.R`.
3. Exporta el archivo necesario a `atlas/data/`.
4. Registra el módulo en `atlas/data/atlas-source.json`.
5. Usa `visible: false` o `status: "Borrador"` hasta que esté listo.
6. Reutiliza un renderer existente antes de crear uno nuevo.
7. Agrega el dataset de respaldo en `moduleDatasets()` para que el gráfico
   tenga tabla y CSV.
8. Corre los scripts de generación y el render.
9. Revisa `/atlas/` en desktop y móvil.
10. Toca al menos un mapa, una burbuja, una barra, un CSV y un gráfico
    ampliado.

## Tipos disponibles

- `macro`: líneas para series cortas. La TPM puede ir escalonada.
- `external`: índice y drivers externos.
- `sectors`: barras de sensibilidad.
- `trade`: scatter de oportunidad y ranking.
- `labor`: barras por grupo.
- `prices`: inflación y canales de traspaso.
- `territory`: mapa provincial y vista territorial.
- `mipymes`: barras, barreras y escalera productiva.
- `visualLab`: mapas y gráficos nacidos de artículos.

Renderers reutilizables:

- `drawLineChart`
- `drawDualLineChart`
- `drawHorizontalBarChart`
- `drawGroupedBarChart`
- `drawComplexScatterChart`
- `drawChoroplethMap`
- `drawTreemapChart`
- `drawDebtBurdenChart`
- `drawStackedBarChart`

## Opciones de gráfico

### Línea escalonada

```js
drawLineChart(canvas, labels, values, "TPM (%)", {
  stepped: true
});
```

Úsala para tasas que cambian por decisión, como la TPM.

### Barras 0-100

```js
renderBarRows(rows, {
  labelField: "sector",
  valueField: "pressure",
  max: 100,
  suffix: "/100"
});
```

La estética de las barras sale de `atlas/styles.css`: relleno mate y track
neutro, sin gradientes ajenos a la marca.

### Burbujas con outliers

```js
drawComplexScatterChart(canvas, rows, {
  xField: "rent",
  yField: "formal_employment",
  sizeField: "jobs",
  labelField: "province",
  categoryField: "zone",
  xTransform: "sqrt",
  yTransform: "sqrt",
  labelTopBy: "jobs",
  labelCount: 4,
  mobileLabelCount: 2
});
```

`sqrt` o `log` comprimen la escala visual sin cambiar el dato real mostrado en
tooltip o tabla. Sirve cuando Distrito Nacional o Santo Domingo aplastan el
resto de puntos.

### Mapa

```js
drawChoroplethMap(canvas, features, {
  valueField: "business_density",
  labelField: "province",
  unit: "empresas por 1,000 hab.",
  inspectorId: "territory-map-inspector",
  mapId: "territory"
});
```

Cada feature del GeoJSON debe tener en `properties` el campo de valor y el campo
de etiqueta.

### Pantalla completa

```js
chartControls("", chartExpandButton("id-del-canvas"))
```

Después agrega el caso en `redrawExpandedChart()` para redibujar el gráfico en
el modal y conservar la interacción.

### Tabla y CSV

Cada gráfico Canvas debe tener una alternativa tabular. El lugar único para
declararla es `moduleDatasets()` en `atlas/app.js`:

```js
dataset("mi-dataset", "Nombre visible", rows, [
  col("periodo", "Periodo"),
  col("valor", "Valor")
])
```

El Atlas genera:

- una tabla compacta al final del módulo;
- CSV por dataset;
- CSV completo por módulo;
- columnas de metadatos: módulo, dataset, fuente, corte y fecha de generación.

Si el gráfico cambia por un filtro o toggle, el dataset debe reflejar la vista
activa sin perder el dato original.

## Datos desde artículos

Usa este flujo cuando un artículo publicado tiene datos que merecen una pieza
interactiva:

1. Mantén el artículo como fuente narrativa.
2. Agrega la transformación en `atlas/scripts/build-article-visuals.R`.
3. Exporta a `atlas/data/`.
4. Registra el artículo en `sources` con `title`, `article`, `href` y `files`.
5. Conecta la visual en `renderVisualLab()` o en un módulo propio.
6. Corre R, Node y Quarto.

Activos actuales desde artículos:

- `rd-provinces.geojson`: densidad empresarial por provincia.
- `rd-regions-mipymes.geojson`: microempresas e informalidad por región.
- `world-tourism.geojson`: preferencia por playa por país de origen.
- `article-visuals.json`: turismo, empleo-alquiler, deuda y registro de
  fuentes.

## Fichas fuente

Cada ficha sale del módulo:

- `source`: nombre corto.
- `sourceDetail`: origen claro de los datos.
- `updated`: fecha de corte del dato.
- `methodology`: criterios verificables.
- `related`: rutas reales a artículos o series.

No escribas texto público de relleno. Si la fuente, metodología o artículo no
están listos, el módulo debe quedar oculto.

## R mínimo

```r
library(dplyr)
library(readr)
library(jsonlite)

datos <- read_csv("mi_archivo.csv") |>
  transmute(
    anio = as.integer(anio),
    valor = as.numeric(valor),
    grupo = as.character(grupo)
  )

write_json(
  list(updated = as.character(Sys.Date()), mi_grafico = datos),
  "atlas/data/mi-grafico.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)
```

## Acentos y texto

Los datos públicos deben estar en UTF-8. Si trabajas en R desde Windows:

- Lee CSV con helpers que fuercen UTF-8 cuando sea necesario.
- Normaliza claves de unión, no textos visibles.
- No publiques mojibake ni caracteres de reemplazo.
- Ejecuta `node atlas/scripts/check-text-integrity.mjs` antes de cerrar.

## Checklist antes de publicar

1. `Rscript atlas/scripts/build-article-visuals.R`
2. `node atlas/scripts/build-atlas-data.mjs`
3. `node atlas/scripts/check-text-integrity.mjs`
4. `node --check` para los JS del Atlas.
5. `quarto render`
6. Revisar `/atlas/` en desktop.
7. Revisar `/atlas/` en móvil.
8. Descargar al menos un CSV.
9. Abrir un gráfico ampliado y probar Escape/Tab.
10. Confirmar que las secciones incompletas sigan ocultas.
