# Estilos del sitio

`styles.css` es la entrada pública y conserva el orden de importación.
La separación no cambia la cascada original.

| Archivo | Responsabilidad |
| --- | --- |
| base.css | Tipografía, colores base y enlace de salto |
| navigation.css | Cabecera, menús, tema y accesibilidad |
| home.css | Portada y listados generales |
| observatory.css | Compatibilidad del observatorio y tarjetas históricas |
| listings.css | Listados editoriales y sus ajustes móviles |
| article-layout.css | Cabecera, párrafos y destacados del artículo |
| article-figures.css | Figuras y ampliación de gráficos |
| article-content.css | Listas, notas, índice y navegación entre artículos |
| article-extras.css | Progreso, audio, relacionados y bibliografía |
| search.css | Buscador de Quarto |
| sharing.css | Enlaces y controles para compartir |
| sections.css | Páginas de secciones, archivo y categorías |
| responsive.css | Ajustes de portada y navegación móvil |
| pages.css | Ajustes finales del artículo, Sobre mí y suscripción |

Los tokens compartidos siguen en `assets/css/tokens.css`. Atlas mantiene sus
propios estilos en `atlas/css/`. No copies reglas al final de `styles.css`:
edita el módulo correspondiente y revisa claro/oscuro y móvil.
