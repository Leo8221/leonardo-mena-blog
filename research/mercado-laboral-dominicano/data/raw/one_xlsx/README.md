# Bases adicionales de la ONE

Esta carpeta conserva los ocho libros XLSX descargados de la ONE. La lista, tamaños, hashes y URLs están en `manifest.csv`.

El cargador selecciona únicamente las hojas de datos:

- clima: `1991-2025`, `CA_2017_2023` y `CA_old`;
- fenómenos naturales: `Base`;
- gastos municipales: una hoja por año, 2022–2024;
- ingresos municipales: una hoja por año, 2022–2024.

Las portadas y diccionarios permanecen dentro de los XLSX originales y no se cargan como tablas analíticas. Los datos se convierten a CSV UTF-8 reproducible en `../one_csv/` antes de usar `\copy` en PostgreSQL.
