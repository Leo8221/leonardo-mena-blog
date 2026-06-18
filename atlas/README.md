# Atlas LM

Atlas LM is the standalone interactive product surface for the blog.

## Data Contract

- Edit `atlas/data/atlas-source.json`.
- Run `node atlas/scripts/build-atlas-data.mjs`.
- The app reads only `atlas/data/atlas-data.json`.

Visible modules must have:

- `visible: true`
- `status: "Activo"`
- `chart`
- `source`
- `question`
- at least one methodology note
- a matching entry under `series`

Modules marked `Borrador`, `Backlog`, `Pendiente` or `Proximo` are not emitted to the public JSON.

## Current Modules

- Pulso macro RD
- Sensibilidad sectorial
- Condiciones externas
- Atlas de comercio exterior
- Mercado laboral y salarios
- Costo de vida y precios
- Territorio e infraestructura
- MiPyMES y productividad
- Laboratorio visual

## Extension Pattern

Add a module to `modules`, add its data under `series`, then create or reuse a renderer in `atlas/app.js`.

## Article Visual Pipeline

Use this when a published article already has data worth turning into an interactive Atlas piece.

1. Keep the article as the narrative source.
2. Add extraction logic to `atlas/scripts/build-article-visuals.R`.
3. Export browser-ready assets under `atlas/data/`:
   - `article-visuals.json` for chart-ready tables.
   - GeoJSON files for maps.
4. Reuse renderers in `atlas/app.js`:
   - `drawChoroplethMap`
   - `drawComplexScatterChart`
   - `drawTreemapChart`
   - `drawStackedBarChart`

Current article-backed assets:

- `rd-provinces.geojson`: business density by province.
- `rd-regions-mipymes.geojson`: microenterprise and informality rates by region.
- `world-tourism.geojson`: beach preference by country of origin.
- `article-visuals.json`: tourism motivations, transport rent-employment space, debt service composition, and source registry.
