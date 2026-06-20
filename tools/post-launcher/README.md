# Lanzador de articulos

Uso rapido:

1. Ejecuta `crear-articulo.cmd` desde la raiz del repo.
2. Elige la serie.
3. Escribe titulo, fecha, descripcion y categorias extra.
4. Crea el articulo.
5. Abre `index.qmd` y escribe.

El articulo nuevo nace con `draft: true`. Quarto lo oculta en el render hasta
que cambies esa linea a `draft: false`.

Tambien puedes abrirlo desde terminal:

```powershell
node tools/post-launcher/server.mjs
```

Por defecto crea estas carpetas junto al `index.qmd`:

- `data/`
- `rds/`
- `figures/`

Si no las necesitas, desmarcalas en el formulario.
