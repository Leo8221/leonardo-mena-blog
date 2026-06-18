# Atlas

Atlas organiza vistas con datos, fuente y renderer propio.

## Datos

- Editar `atlas/data/atlas-source.json`.
- Ejecutar `node atlas/scripts/build-atlas-data.mjs`.
- La app publica lee `atlas/data/atlas-data.json`.

Cada modulo activo debe tener:

- `visible: true`
- `status: "Activo"`
- `chart`
- `source`
- `question`
- notas metodologicas
- una entrada correspondiente en `series`

## Modulos actuales

- Pulso macro RD
- Sensibilidad sectorial
- Condiciones externas
- Atlas de comercio exterior
- Mercado laboral y salarios
- Costo de vida y precios
- Territorio e infraestructura
- MiPyMES y productividad
- Laboratorio visual

## Agregar una vista

1. Definir la pregunta y la fuente real.
2. Agregar el modulo en `atlas/data/atlas-source.json`.
3. Agregar la data bajo `series`.
4. Crear o reutilizar un renderer en `atlas/app.js`.
5. Correr `node atlas/scripts/build-atlas-data.mjs`.
6. Correr `quarto render`.
7. Validar visualmente `/atlas/` en desktop y movil.

## Pipeline desde articulos

Usar esto cuando un articulo publicado tiene datos que merecen una pieza interactiva.

1. Mantener el articulo como fuente narrativa.
2. Agregar extraccion en `atlas/scripts/build-article-visuals.R`.
3. Exportar activos a `atlas/data/`:
   - `article-visuals.json` para tablas listas para graficos.
   - GeoJSON para mapas.
4. Reutilizar renderers de `atlas/app.js`:
   - `drawChoroplethMap`
   - `drawComplexScatterChart`
   - `drawTreemapChart`
   - `drawStackedBarChart`

Activos actuales desde articulos:

- `rd-provinces.geojson`: densidad empresarial por provincia.
- `rd-regions-mipymes.geojson`: microempresas e informalidad por region.
- `world-tourism.geojson`: preferencia por playa por pais de origen.
- `article-visuals.json`: motivaciones turisticas, empleo-alquiler, servicio de deuda y registro de fuentes.
