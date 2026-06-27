# Atlas

Atlas es la superficie interactiva del sitio. El blog argumenta; Atlas permite
explorar los datos que sostienen o amplían esos análisis.

Usa [GUIDE.md](GUIDE.md) como guía única de mantenimiento.

## Flujo mínimo

1. Trabaja los datos en R o en una fuente reproducible.
2. Exporta una tabla limpia, JSON o GeoJSON a `atlas/data/`.
3. Registra el módulo en `atlas/data/atlas-source.json`.
4. Ejecuta:

```powershell
Rscript atlas/scripts/fetch-atlas-sources.R
Rscript atlas/scripts/build-bcrd-live-data.R
Rscript atlas/scripts/build-map-assets.R
Rscript atlas/scripts/build-article-visuals.R
node atlas/scripts/build-atlas-data.mjs
quarto render
```

El workflow de GitHub Actions repite esos pasos antes de publicar `docs/`.

`fetch-atlas-sources.R` lee `atlas/data/source-manifest.json`, descarga o valida
las fuentes declaradas, guarda una copia auditable en `atlas/data/raw/` cuando
aplica y escribe `atlas/data/source-run.json`. Para BCRD usa los `CustomView`
oficiales de cada sector y filtra los Excel por `includePatterns`, de modo que
agregar una serie nueva implica registrar el patron del archivo publicado, no
editar `docs/` ni pegar salidas manuales.

`build-bcrd-live-data.R` abre esos Excel descargados y genera
`atlas/data/bcrd-live-data.json`; luego `build-atlas-data.mjs` hidrata las
series publicas del Atlas con esos valores antes de escribir `atlas-data.json`.

## Regla editorial

Si un módulo no tiene datos, fuente, lectura y fecha de corte verificables, debe
quedar oculto con `visible: false` o con un `status` distinto de `Activo`.
