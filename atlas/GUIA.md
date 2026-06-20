# Guia del Atlas

El Atlas esta separado en piezas pequenas para poder crecer sin tocar todo cada vez:

- `atlas/index.html`: contenedor, SEO y orden de scripts.
- `atlas/app.js`: estado, navegacion, filtros y estructura de cada modulo.
- `atlas/js/config.js`: colores, rutas de articulos y constantes compartidas.
- `atlas/js/utils.js`: formato, canvas, tooltips y utilidades comunes.
- `atlas/js/renderers.js`: motores de graficos y mapas.
- `atlas/js/interactions.js`: enlace, PNG y pantalla completa.
- `atlas/data/atlas-source.json`: registro editable de modulos, fuentes y series.
- `atlas/scripts/build-atlas-data.mjs`: valida y genera `atlas-data.json`.
- `atlas/scripts/build-article-visuals.R`: convierte datos reales de articulos en JSON/GeoJSON.

## Agregar una seccion

1. En `atlas/data/atlas-source.json`, crea un modulo en `modules`.
2. Usa `visible: true` y `status: "Activo"` solo cuando este listo para publicarse. Para trabajar sin mostrarlo, deja `visible: false` o `status: "Borrador"`.
3. Completa estos campos: `id`, `title`, `type`, `family`, `topic`, `summary`, `source`, `sourceDetail`, `chart`, `methodology` y `related`.
4. Agrega la serie correspondiente en `series`. El `chart` del modulo debe existir en `atlas/app.js`.
5. Si el grafico sale de un articulo, agrega el procesamiento en `atlas/scripts/build-article-visuals.R` y escribe el resultado en `article-visuals.json` o en un GeoJSON liviano.
6. Corre:

```powershell
node atlas/scripts/build-atlas-data.mjs
Rscript atlas/scripts/build-article-visuals.R
```

7. Sincroniza `atlas/` hacia `docs/atlas/` antes de publicar en GitHub Pages, o renderiza el sitio con Quarto.

## Fichas fuente

Cada ficha se genera automaticamente desde el modulo:

- `source`: nombre corto de la fuente.
- `sourceDetail`: una linea clara sobre el origen de los datos.
- `methodology`: deja primero el criterio mas importante; la ficha usa ese punto.
- `related`: paginas o series relacionadas. Si escribes `republica-habla-de.html`, el Atlas lo enlaza como `../republica-habla-de.html`.

No escribas texto publico de relleno. Si la fuente, metodologia o articulo no estan listos, el modulo debe quedar oculto.

## Opciones de grafico

Tipos ya conectados:

- `macro`: linea; la TPM puede ir escalonada.
- `external`: linea compacta.
- `sectors`: barras de sensibilidad.
- `trade`: scatter complejo y ranking.
- `labor`: barras por grupo.
- `prices`: contribuciones y canales.
- `territory`: mapa provincial, inspector y ranking territorial.
- `mipymes`: matriz, barreras y escalera.
- `visualLab`: mapas y graficos nacidos de articulos.

Para un tipo nuevo, agrega el bloque HTML en `atlas/app.js`, el dibujo en `atlas/js/renderers.js` y la hidratacion en `hydrateCharts()`.

## Acentos

El script de R debe seguir siendo ASCII para evitar errores de locale en Windows. Para datos:

- Lee CSV con `read_csv_utf8()`.
- Une paises y provincias con `normalize_key()`.
- Usa escapes como `\\u00f3` cuando un texto fijo necesite acento dentro del script.
- El build falla si detecta mojibake en los archivos publicados.
