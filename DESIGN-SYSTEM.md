# Sistema de diseno

Este sitio debe sentirse como una publicacion economica personal contemporanea con una mesa de datos propia. La identidad combina lectura editorial, graficos sobrios e interaccion util para economia aplicada de Republica Dominicana.

## Principios

- Editorial antes que corporativo: menos paneles decorativos, mas jerarquia, ritmo y lectura.
- Datos con contexto: todo grafico debe tener fuente, fecha de corte y una lectura breve.
- Sobriedad dominicana: crema, terracota, oliva, azul de datos y negro editorial.
- Mobile primero: nada esencial debe desaparecer para resolver un problema de layout.
- Progresive enhancement: el contenido principal debe seguir siendo legible si JavaScript falla.

## Fuente De Tokens

La fuente compartida es `assets/css/tokens.css`.

- `styles.css` la importa como `assets/css/tokens.css`.
- `atlas/styles.css` la importa como `../assets/css/tokens.css`.
- `_quarto.yml` la declara como recurso para que GitHub Pages copie `docs/assets/css/tokens.css`.

No dupliques colores o sombras en hojas de pagina si existe un token equivalente. Si falta uno, agregalo primero en `assets/css/tokens.css`.

## Paleta

- Fondo principal: `--lm-bg-body`.
- Superficie: `--lm-bg-card`.
- Superficie secundaria: `--lm-bg-sec`.
- Texto principal: `--lm-text-main`.
- Texto secundario: `--lm-text-soft`.
- Texto atenuado: `--lm-text-muted`.
- Marca: `--lm-terracota`.
- Marca para texto/hover: `--lm-terracota-dark`.
- Acento secundario: `--lm-olive`.
- Datos: `--lm-data-blue`.
- Estados: `--lm-success`, `--lm-warning`, `--lm-error`.

Atlas puede usar los alias cortos `--bg`, `--card`, `--panel`, `--ink`, `--soft`, `--muted`, `--terracotta`, `--olive` y `--blue`; todos salen de la misma fuente.

## Tipografia

- Titulares y lectura editorial: `--lm-font-serif`.
- Navegacion, controles, metadatos y UI: `--lm-font-sans`.
- Codigo: `--lm-font-mono`.

No escales texto con ancho de viewport salvo en titulares grandes usando `clamp()`. En tarjetas, sidebars y controles usa tamanos mas contenidos.

## Espaciado Y Contenedores

- Usa los tokens `--lm-space-*` para separacion recurrente.
- Ancho editorial: `--lm-content-max`.
- Ancho amplio de portada: `--lm-wide-max`.
- Ancho de lectura: `--lm-reading-max`.
- Control tactil minimo: `--lm-control-min`.

Evita anchos fijos en mobile. Las paginas deben funcionar desde 320 px sin overflow horizontal.

## Bordes, Sombras Y Radios

- Bordes: `--lm-border`, `--lm-border-dark`.
- Radios: `--lm-radius-xs`, `--lm-radius-sm`, `--lm-radius-md`, `--lm-radius-pill`.
- Sombras: `--shadow-xs`, `--shadow-sm`, `--shadow-soft`, `--shadow-md`.

Usa sombras solo para indicar capa o interaccion. No conviertas todas las secciones en tarjetas flotantes.

## Botones Y Enlaces

- CTA principal: terracota, borde terracota, texto blanco.
- CTA secundario: fondo transparente, borde editorial, texto principal.
- Enlaces editoriales: terracota con subrayado sobrio.
- Todo control interactivo necesita `:focus-visible`.
- Objetivo tactil recomendado: al menos `--lm-control-min`.

## Navegacion

La navegacion global sale de `_quarto.yml`.

- No uses HTML con estilos inline en el navbar.
- El CTA de suscripcion se estiliza por CSS, no por markup embebido.
- Atlas debe ser visible como entrada principal, pero sin competir con todos los enlaces.
- En mobile, el menu debe mantener Inicio, Secciones, Analisis, Atlas, Escuela, Sobre mi y Suscribete.

## Graficos

- Usa azul para series de datos principales cuando no haya una razon semantica para otro color.
- Usa terracota para enfasis editorial o dato seleccionado.
- Usa oliva para contexto, comparacion o estado secundario.
- No dependas solo del color: agrega etiquetas, leyendas, tablas o texto alternativo.
- Todo modulo del Atlas debe explicar fuente y fecha de corte.

## Modo Oscuro

El modo oscuro del blog vive en `body.quarto-dark` dentro de `assets/css/tokens.css`.

No declares colores oscuros en componentes salvo que el token no alcance. Si un componente se rompe en oscuro, corrige el token o usa una excepcion local pequena y documentada.

## Accesibilidad

- El blog carga `skip-link.html` desde `_quarto.yml`; el Atlas tiene su propio enlace de salto a `#atlas-main`.
- Usa `:focus-visible` para links, botones, toggles, inputs y controles del Atlas.
- Respeta `prefers-reduced-motion`.
- No ocultes contenido esencial en mobile.
- Los enlaces externos con `target="_blank"` deben usar `rel="noopener noreferrer"`.
- Las imagenes funcionales necesitan texto alternativo util o nombre accesible.
- Ejecuta `node atlas/scripts/check-site-contract.mjs` antes de publicar cambios de layout; con `--include-docs` valida tambien la salida renderizada.

## Como Extender

1. Agrega primero el token nuevo en `assets/css/tokens.css`.
2. Usa el token en `styles.css` o `atlas/styles.css`.
3. Evita repetir valores hexadecimales en componentes.
4. Si el cambio afecta Quarto y Atlas, prueba ambas superficies.
5. Actualiza esta guia si introduces un patron reusable.
