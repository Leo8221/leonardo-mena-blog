# Mantenimiento del blog

La entrada para escribir es [el README principal](../README.md).
Las reglas del repositorio siguen en [AGENTS.md](../AGENTS.md).

## Mapa técnico

| Ruta | Responsabilidad |
| --- | --- |
| `_quarto.yml`, `_quarto-editor.yml` | Sitio público y vista previa de borradores. |
| `*.qmd` en la raíz | Páginas y listados; sus rutas públicas se conservan. |
| `posts/_metadata.yml` | Configuración compartida de los artículos. |
| `styles.css`, `assets/`, `image/`, `*.html` en la raíz | Estilos, marca e inclusiones HTML del sitio. |
| `tema_graficos.R`, `R/` | Tema y funciones compartidas de gráficos. |
| `atlas/` | Aplicación, datos y scripts del Atlas; ver su [guía](../atlas/GUIDE.md). |
| `datos/`, `figuras/`, `mapa_rd/` | Recursos históricos compartidos: revisar referencias antes de moverlos. |
| `research/` | Fuentes, análisis y datos reutilizables. |
| `tools/` | Wrappers de R, Python y Quarto y [lanzador de artículos](../tools/post-launcher/README.md). |
| `.github/workflows/` | Render automático y actualización de `docs/`. |
| `docs/` | Salida pública generada por Quarto. |
| `.preview/`, `_freeze/`, `.quarto*/`, `*_cache/`, `tmp/` | Vista previa, estado y temporales. No son fuentes editoriales. |

## Referencias

- [Sistema de diseño](DESIGN-SYSTEM.md).
- [Guía editorial y de publicación](../editorial/GUIA-EDITORIAL.md).
- [Referencia editorial en PDF](../editorial/Guia_Estilos_Editorial_LM.pdf).
- [Plan e historial anterior](historial/IMPLEMENTATION_PLAN.md). Sus rutas
  describen el estado de cada intervención; para la organización actual usa este mapa.
- [Registro de limpieza anterior](historial/REGISTRO_LIMPIEZA.txt).
- [Reorganización de noviembre de 2026](historial/2026-11-07-organizacion.md).

Los informes SIUBEN y su validación RSUH se trasladaron a
`proyecto_v13/estudios/antecedentes-rsuh-202608/`. No son parte del blog.

## Comprobaciones

Ejecutar desde la raíz del repositorio. Los wrappers fijan las instalaciones
de R/Quarto y comprueban UTF-8 en Windows.

```powershell
node --test tools/post-launcher/*.test.mjs
node atlas/scripts/check-text-integrity.mjs
node atlas/scripts/check-site-contract.mjs
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\render-quarto.ps1
```

El último comando genera `docs/`. Revisar el diff antes de confirmar; un cambio
de versión de Quarto puede producir diferencias ajenas al trabajo realizado.
Aplicar además las verificaciones de Atlas y visuales exigidas por `AGENTS.md`
cuando se cambien esas partes.
