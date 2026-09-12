# Escribir con Quarto

El sitio sigue usando Quarto y GitHub Pages. No hay un publicador adicional.

## Crear un artículo

Abre **crear-articulo.cmd** con doble clic. Elige la serie, escribe el título
y crea el borrador. Después pulsa **Editar** para abrir su `index.qmd` en tu
editor habitual. También puedes seguir creando los `.qmd` manualmente.

El artículo nace con `draft: true`. La plantilla es texto simple; solo añade
un bloque de R y carpetas de datos y figuras si marcas esa opción.
No hace falta registrar el artículo en la portada ni en los listados.

## Ver cómo queda

En el lanzador, busca el artículo y pulsa **Vista previa**. Ese botón ejecuta
el comando nativo `quarto preview`; guarda el archivo para actualizar la página
y usa Ctrl+C en su ventana para detenerlo.

También puedes hacerlo desde PowerShell:

```powershell
.\tools\preview-article.ps1 -Article "posts/fundamentos/fecha-titulo/index.qmd"
```

La vista previa usa el perfil nativo `editor`, definido en `_quarto-editor.yml`.
Muestra los borradores y escribe en `.preview/`, que Git ignora, sin modificar
`docs/`. Puedes seguir usando la previsualización de RStudio o de la extensión
de Quarto; selecciona ese perfil si quieres la misma separación.

## Publicar

1. Revisa el texto, las fuentes y los gráficos, también en móvil.
2. Cambia `draft: true` a `draft: false` en el artículo.
3. Guarda en Git el `.qmd` y sus datos/imágenes y envía los cambios a `main`,
   usando tu cliente Git habitual.
4. Comprueba [GitHub Actions](https://github.com/Leo8221/leonardo-mena-blog/actions).
   El proceso existente renderiza el sitio y actualiza `docs/` para GitHub Pages.

No tienes que copiar HTML a mano ni ejecutar un publicador nuevo.

Para comprobar todo el sitio localmente, el wrapper existente ejecuta
`quarto render` con las instalaciones y el entorno UTF-8 del proyecto:

```powershell
.\tools\render-quarto.ps1
```

Ese render completo sí genera `docs/`; no lo confundas con la vista previa.

## Mantenimiento

Las rutas de R y Quarto se definen una sola vez en `tools/runtime.ps1`.
El wrapper verifica UTF-8 antes de ejecutar R para detectar problemas de acentos.

Referencias: [borradores](https://quarto.org/docs/websites/website-drafts.html),
[perfiles](https://quarto.org/docs/projects/profiles.html) y
[preview de Quarto](https://quarto.org/docs/websites/).
