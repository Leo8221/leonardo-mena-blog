# Blog de Leonardo Mena

Para volver a escribir, abre **[crear-articulo.cmd](crear-articulo.cmd)** con doble
clic: permite crear, buscar, editar y previsualizar artículos.

Si usas VS Code, abre **[escribir.code-workspace](escribir.code-workspace)**:
verás solo **Artículos** y **Editorial**. Para trabajar en el sitio completo,
abre la carpeta del repositorio como siempre.

## Lo que necesitas para publicar

1. **Elegir un tema:** consulta [las ideas](editorial/ideas-posts.md).
2. **Escribir:** crea o abre un artículo con el lanzador. Todo lo propio del
   artículo va en su carpeta: `index.qmd`, `data/` y `figures/` cuando hagan falta.
3. **Revisar y publicar:** usa la vista previa; cuando esté listo, cambia
   `draft: true` a `draft: false`, confirma sus fuentes en Git y envíalas a `main`.
   [GitHub Actions](https://github.com/Leo8221/leonardo-mena-blog/actions)
   genera `docs/` para GitHub Pages.

La [guía editorial](editorial/GUIA-EDITORIAL.md) explica imágenes, fuentes,
enlaces y publicación. Los artículos aparecen automáticamente en los listados.

## Dónde va cada cosa

| Lugar | Para qué sirve |
| --- | --- |
| `posts/` | Artículos, tanto borradores como publicados, agrupados por serie. |
| `editorial/` | Guía de escritura, ideas y referencia de estilo. |
| `research/` | Investigaciones y datos compartidos por varios artículos. |
| `mantenimiento/` | Mapa técnico, diseño e historial de trabajo. |
| `docs/` | Sitio generado. Se produce con Quarto; no se escribe aquí. |

Las páginas `.qmd` de la raíz son la portada y las secciones del sitio.
`atlas/`, `assets/`, `tools/` y `R/` mantienen los visuales y las herramientas.
Consulta el [mapa técnico](mantenimiento/README.md) solo cuando necesites
trabajar en esas partes.
