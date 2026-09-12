# Escribir con Quarto

El sitio sigue usando Quarto y GitHub Pages. No hay un publicador adicional.

## Crear un artículo

Abre **crear-articulo.cmd** con doble clic. Elige la serie, escribe el título
y crea el borrador. Después pulsa **Editar** para abrir su `index.qmd` en tu
editor habitual. También puedes seguir creando los `.qmd` manualmente.

El artículo nace con `draft: true`. La plantilla es texto simple; solo añade
un bloque de R y carpetas de datos y figuras si marcas esa opción.
No hace falta registrar el artículo en la portada ni en los listados. El más
reciente por fecha aparece destacado y los tres siguientes en «Últimas
publicaciones». Si coinciden las fechas, se ordenan por título. No cambies
exclusiones en `index.qmd` al publicar un artículo.

## El encabezado del artículo

El creador deja únicamente los datos que cambian en cada publicación:

```yaml
---
title: "Título de tu artículo"
date: "2026-09-12"
description: "Una frase que explique qué encontrará el lector."
categories:
  - "Fundamentos"
draft: true
---
```

Escribe el texto debajo del segundo `---`. Cambia la fecha por la de tu
publicación y conserva la categoría de la serie que elegiste en el creador.
El autor, el índice y las funciones de lectura se heredan de
`posts/_metadata.yml`; no tienes que copiar rutas HTML ni configurar botones.
Puedes indicar otro `author` en un artículo si corresponde.

## Enlaces, imágenes y fuentes

Estos ejemplos se escriben en el cuerpo del artículo, debajo del encabezado.
Sustituye los textos y archivos de ejemplo por los de tu publicación.

### Enlazar una página externa

```markdown
Consulta los datos en el [Banco Central](https://www.bancentral.gov.do/).
```

El texto entre corchetes será el enlace; la dirección va entre paréntesis.

### Enlazar otro artículo del blog

Usa la ruta del archivo `.qmd` desde la raíz del proyecto:

```markdown
También puedes leer el [análisis de MiPyMES](/posts/republica-en-un-grafico/2026-02-14-mipymes-rd/index.qmd).
```

Quarto convierte esa ruta al enlace publicado, incluido el prefijo del sitio
`/leonardo-mena-blog/`. No necesitas escribir el dominio ni contar `../`.

### Insertar una imagen con pie

Guarda la imagen dentro de la carpeta del artículo, por ejemplo en
`figures/mi-grafico.png`. Si la carpeta no existe, créala.

```markdown
![Título del gráfico. Fuente: nombre de la fuente.](figures/mi-grafico.png){fig-alt="Descripción de lo que muestra el gráfico."}
```

El texto entre corchetes aparece como pie. `fig-alt` describe la imagen para
quienes usan lectores de pantalla. Incluye la imagen en el mismo commit que
el artículo; evita rutas de tu computadora como `C:\Users\...`.

### Citar una fuente o añadir una nota

Para una referencia sencilla, enlaza el informe o la tabla exacta que usaste:

```markdown
Fuente: [título del informe](https://example.org/informe.pdf), página 12.
```

La dirección y la página anteriores son ejemplos: reemplázalas por la fuente
real. Para una explicación que no interrumpa el texto, usa una nota al pie:

```markdown
La comparación utiliza precios constantes.[^precios]

[^precios]: Explica aquí el año base y enlaza la metodología utilizada.
```

Si el artículo requiere bibliografía académica, Quarto también admite un
archivo `referencias.bib` junto al artículo: añade
`bibliography: referencias.bib` al encabezado y cita una entrada con
`[@clave]`, usando la clave real de esa entrada. No es necesario para
publicaciones que solo necesitan enlaces a sus fuentes.

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

Ejemplos de escritura: [Markdown](https://quarto.org/docs/authoring/markdown-basics.html),
[enlaces entre páginas](https://quarto.org/docs/websites/website-navigation.html#linking),
[citas](https://quarto.org/docs/authoring/citations.html) y
[configuración compartida](https://quarto.org/docs/projects/quarto-projects.html#directory-metadata).
