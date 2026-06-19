# Guia de trabajo del Atlas

Esta guia es para mantener el Atlas sin convertirlo en una pieza dificil de
editar. La regla base: el articulo explica; el Atlas deja explorar datos reales.
Si algo no esta listo, no se muestra.

## Ruta rapida

1. Trabaja los datos en R.
2. Exporta una tabla limpia o GeoJSON a `atlas/data/`.
3. Si es un modulo principal, registralo en `atlas/data/atlas-source.json`.
4. Si viene de un articulo, agregalo en `atlas/scripts/build-article-visuals.R`.
5. Reutiliza un renderer existente en `atlas/app.js`.
6. Ejecuta:

```powershell
Rscript atlas/scripts/build-article-visuals.R
node atlas/scripts/build-atlas-data.mjs
quarto render
```

## Que archivo tocar

- `atlas/data/atlas-source.json`: modulos principales, filtros, texto corto,
  metadata y series base.
- `atlas/scripts/build-atlas-data.mjs`: normaliza lo que sale de
  `atlas-source.json`.
- `atlas/scripts/build-article-visuals.R`: datos que nacen de articulos ya
  publicados.
- `atlas/app.js`: renderers e interaccion.
- `atlas/styles.css`: sistema visual, responsive, botones, modales y tooltips.
- `_quarto.yml`: solo si agregas un recurso nuevo que GitHub Pages debe copiar.

## Opciones listas

Usa estas opciones antes de crear un grafico desde cero.

### Linea simple

```js
drawLineChart(canvas, labels, values, "TPM (%)", {
  stepped: true
});
```

Usa `stepped: true` para tasas que cambian por decision, como TPM.

### Barras horizontales

```js
drawHorizontalBarChart(canvas, rows, {
  labelField: "sector",
  valueField: "presion",
  title: "Presion por sector",
  max: 100
});
```

Sirve para rankings 0-100, brechas o indices.

### Burbujas con outliers

```js
drawComplexScatterChart(canvas, rows, {
  title: "Empleo formal y alquiler",
  xField: "rent",
  yField: "formal_employment",
  sizeField: "jobs",
  labelField: "province",
  categoryField: "zone",
  xLabel: "Alquiler mediano anual",
  yLabel: "% de empleo formal",
  xTransform: "sqrt",
  yTransform: "sqrt",
  labelTopBy: "jobs",
  labelCount: 4,
  mobileLabelCount: 2
});
```

`sqrt` o `log` comprimen la escala visual sin cambiar el dato que aparece en el
tooltip. Esto es util cuando Santo Domingo o el Distrito aplastan el resto.

### Mapa

```js
drawChoroplethMap(canvas, features, {
  title: "Republica Dominicana",
  valueField: "business_density",
  labelField: "province",
  unit: "empresas por 10 mil hab.",
  inspectorId: "territory-map-inspector",
  mapId: "territory"
});
```

Cada feature del GeoJSON debe tener en `properties` el campo de valor y el campo
de etiqueta.

### Pantalla completa

En el HTML del modulo:

```js
chartControls("", chartExpandButton("id-del-canvas"))
```

Despues agrega el caso en `redrawExpandedChart()` para que el modal redibuje el
grafico y conserve la interaccion.

## R: exportar datos simples

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

## Checklist antes de publicar

1. `node --check atlas/app.js`
2. `node atlas/scripts/build-atlas-data.mjs`
3. `Rscript atlas/scripts/build-article-visuals.R`
4. `quarto render`
5. Revisar `/atlas/` en desktop.
6. Revisar `/atlas/` en movil.
7. Tocar un mapa, una burbuja, una barra y un grafico ampliado.
8. Confirmar que las secciones incompletas sigan ocultas.

## Cuando crear un renderer nuevo

Crea un renderer solo si no puedes expresar la idea con `drawLineChart`,
`drawHorizontalBarChart`, `drawGroupedBarChart`, `drawComplexScatterChart`,
`drawChoroplethMap`, `drawTreemapChart`, `drawDebtBurdenChart` o
`drawStackedBarChart`.

Un buen renderer nuevo debe recibir:

- `canvas`
- `rows` o `features`
- `options`

Y debe cerrar con una interaccion reusable:

- `bindPointTooltip()` para puntos.
- `bindBoxTooltip()` para barras, areas o rectangulos.
- logica propia solo si es un mapa.
