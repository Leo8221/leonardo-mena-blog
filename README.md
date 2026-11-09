# Blog de Leonardo Mena

Crea una carpeta en `posts/` y escribe su `index.qmd`. No hay generador.
Si usas VS Code, [escribir.code-workspace](escribir.code-workspace) muestra
solo los artículos y las ideas.

## Escribir

Cada artículo vive en `posts/serie/fecha-tema/index.qmd`. Sus imágenes y datos
van en esa misma carpeta, en `figures/` y `data/` cuando hagan falta.
Los borradores se quedan allí; no hay que moverlos para publicar.

```yaml
---
title: "Título"
date: "2026-11-08"
description: "Resumen para los listados."
categories: ["Fundamentos"]
draft: true
---
```

El autor y las opciones de lectura vienen de `posts/_metadata.yml`.
Los listados se actualizan solos. Las [ideas](editorial/ideas-posts.md) están
separadas del texto; los análisis compartidos, en `research/`.

```markdown
![Título. Fuente: informe o tabla.](figures/grafico.png){fig-alt="Descripción del gráfico"}

[Otro artículo](/posts/republica-en-un-grafico/2026-02-14-mipymes-rd/index.qmd)
```

## Revisar y publicar

Desde una terminal PowerShell en la raíz:

```powershell
.\tools\render-quarto.ps1 -QuartoArgs @('preview', '--profile', 'editor')
```

La vista previa muestra borradores en `.preview/`. Cuando el artículo esté
revisado, cambia `draft` a `false` y confirma el texto y sus recursos en Git.
No renombres carpetas publicadas: forman parte de sus enlaces.
GitHub Actions renderiza `main` y actualiza `docs/` para GitHub Pages.

## Código y comprobaciones

`_quarto.yml` configura el sitio. `styles.css` carga los estilos de `assets/`.
Atlas vive en `atlas/`; sus datos se declaran en `atlas/data/atlas-source.json`.
R calcula indicadores y prepara mapas; `atlas/scripts/build_atlas_data.py`
ensambla y valida el JSON que consume la web. El recorrido y las unidades se
explican en esos archivos; JavaScript se ocupa de la interacción y el dibujo.
`tema_graficos.R` contiene el tema común de los gráficos.
Las decisiones y los parámetros se explican junto a las funciones que los usan.

```powershell
.\tools\run-python.ps1 atlas/scripts/build_atlas_data.py
.\tools\run-python.ps1 atlas/scripts/test_atlas_data.py
node --test atlas/scripts/*.test.mjs
node atlas/scripts/check-text-integrity.mjs
node atlas/scripts/check-site-contract.mjs
.\tools\render-quarto.ps1
```

El último comando genera `docs/`. Los wrappers de `tools/` fijan R, Python y
Quarto y comprueban UTF-8 en Windows. Sus rutas están en `tools/runtime.ps1`.
No edites el HTML generado para corregir el sitio.
