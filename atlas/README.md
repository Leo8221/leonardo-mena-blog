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
Rscript atlas/scripts/build-article-visuals.R
node atlas/scripts/build-atlas-data.mjs
quarto render
```

El workflow de GitHub Actions repite esos pasos antes de publicar `docs/`.

## Regla editorial

Si un módulo no tiene datos, fuente, lectura y fecha de corte verificables, debe
quedar oculto con `visible: false` o con un `status` distinto de `Activo`.
