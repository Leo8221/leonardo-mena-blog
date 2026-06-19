# Atlas

Atlas es la superficie interactiva del sitio. El articulo sigue siendo la
narrativa; el Atlas convierte sus datos en piezas explorables.

## Flujo normal

1. Preparar datos en R.
2. Exportar una tabla limpia o un GeoJSON a `atlas/data/`.
3. Registrar el modulo en `atlas/data/atlas-source.json`.
4. Reutilizar o crear un renderer en `atlas/app.js`.
5. Ejecutar:

```powershell
node atlas/scripts/build-atlas-data.mjs
quarto render
```

La app publica lee `atlas/data/atlas-data.json` y los activos incluidos en
`project.resources` de `_quarto.yml`.

## Contrato de modulo

Cada modulo activo en `atlas/data/atlas-source.json` debe tener:

- `id`: identificador estable, sin espacios.
- `title`: nombre visible.
- `visible: true`.
- `status: "Activo"`.
- `family`, `topic`, `type`.
- `source` y `sourceDetail`.
- `question` e `insight`.
- `chart`: renderer que se usara en `atlas/app.js`.
- `methodology`: notas cortas, verificables.
- datos compatibles bajo `series`.

Si la seccion no esta lista, usar `visible: false` o un `status` distinto de
`Activo`. Lo incompleto no se publica.

## Tipos disponibles

- `macro`: lineas para series cortas.
- `external`: indice y barras de drivers.
- `sectors`: ranking 0-100 por sector.
- `trade`: burbujas y barras de comercio.
- `labor`: barras por grupo y resultado.
- `prices`: lineas y drivers de inflacion.
- `territory`: mapa provincial y scatter territorial.
- `mipymes`: barras agrupadas y barreras.
- `visualLab`: mapas, treemap, burbujas y deuda desde articulos.

Renderers reutilizables en `atlas/app.js`:

- `drawDualLineChart`
- `drawHorizontalBarChart`
- `drawGroupedBarChart`
- `drawComplexScatterChart`
- `drawChoroplethMap`
- `drawTreemapChart`
- `drawDebtBurdenChart`
- `drawStackedBarChart`

## Opciones simples de graficos

Estas opciones se declaran donde se llama el renderer. No cambian los datos;
solo cambian como se leen visualmente.

- Series escalonadas: usar `drawLineChart(..., { stepped: true })`. Sirve para
  tasas de politica como la TPM, donde el dato cambia por decision y no de forma
  continua.
- Scatter con outliers fuertes: usar `xTransform: "sqrt"` y/o
  `yTransform: "sqrt"` en `drawComplexScatterChart`. Mantiene el valor real en
  el tooltip, pero separa mejor los puntos cuando DN o Santo Domingo aplastan el
  resto.
- Etiquetas limpias en scatter: usar `labelTopBy: "jobs"` y `labelCount: 4`.
  El grafico muestra los nodos principales; los demas se leen tocando/clickeando.
- Barras 0-100: usar `renderBarRows()`. La estetica sale de `atlas/styles.css`
  con relleno mate y track neutro, para que no parezca un componente ajeno.
- Movil: los tooltips de canvas se fijan con tap y se apagan al tocar otro punto,
  cambiar de modulo o cerrar modal. No hay que programarlo por grafico.
- Pantalla completa: usar `chartExpandButton("id-del-canvas")`. El renderer se
  redibuja en el modal y conserva la interaccion.

Equivalentes utiles en R:

```r
# Linea escalonada para TPM
ggplot(tpm, aes(fecha, valor)) +
  geom_step(linewidth = 1)

# Scatter comprimido sin cambiar el dato del tooltip/tabla
ggplot(provincias, aes(alquiler, empleo, size = empleos)) +
  geom_point(alpha = 0.75) +
  scale_x_continuous(trans = "sqrt") +
  scale_y_continuous(trans = "sqrt")
```

## Pipeline desde articulos

Usar esto cuando un articulo publicado tiene datos que merecen una pieza
interactiva.

1. Mantener el articulo como fuente narrativa.
2. Agregar la transformacion en `atlas/scripts/build-article-visuals.R`.
3. Exportar a `atlas/data/`:
   - `article-visuals.json` para tablas listas para graficos.
   - GeoJSON para mapas.
4. Registrar el articulo en `sources` con `title`, `article`, `href` y `files`.
5. En `renderVisualLab()`, agregar una tarjeta o un toggle que use esos datos.
6. Correr `Rscript atlas/scripts/build-article-visuals.R`.
7. Correr `quarto render`.
8. Revisar `/atlas/` en desktop y movil.

## Ejemplo R minimo

```r
library(dplyr)
library(readr)
library(jsonlite)

deuda <- read_csv("posts/republica-habla-de/2025-12-19_deuda_publica/deuda_rd.csv") |>
  group_by(anio) |>
  summarise(
    principal = sum(principal, na.rm = TRUE),
    interest = sum(intereses, na.rm = TRUE),
    commissions = sum(comisiones, na.rm = TRUE),
    service = principal + interest + commissions,
    interest_share = interest / service * 100,
    .groups = "drop"
  )

payload <- list(
  updated = as.character(Sys.Date()),
  debt = list(service = deuda)
)

write_json(payload, "atlas/data/mi-visual.json", pretty = TRUE, auto_unbox = TRUE, na = "null")
```

## Activos actuales desde articulos

- `rd-provinces.geojson`: densidad empresarial por provincia.
- `rd-regions-mipymes.geojson`: microempresas e informalidad por region.
- `world-tourism.geojson`: preferencia por playa por pais de origen.
- `article-visuals.json`: turismo, empleo-alquiler, deuda y registro de fuentes.
