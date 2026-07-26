# PostgreSQL analítico: censos y encuestas de República Dominicana

Este módulo reconstruye, valida y documenta las bases estadísticas en PostgreSQL 18 sin modificar los directorios dañados originales.

## Conexión

- Servidor: `localhost`
- Puerto: `5433`
- Servicio de Windows: `postgresql-x64-18-recovered`
- Directorio de datos: `D:\PostgreSQL_Recuperado\18\data`
- Codificación: UTF-8 con checksums de páginas

La contraseña se recibe mediante `PG_RECOVERY_PASSWORD`; no se guarda en el repositorio.

## Bases disponibles

| Base | Contenido principal | Uso recomendado |
| --- | --- | --- |
| `censo_2002` | Personas, hogares y viviendas del VIII Censo | Comparaciones históricas |
| `censo_2010` | Personas, hogares y viviendas del IX Censo | Comparaciones históricas |
| `censo_2022` | Personas, vivienda-hogar, mortalidad y base unificada | Análisis detallado de 2022 y relaciones intrahogar |
| `censos_linea_tiempo` | Vistas federadas y catálogo de métricas | Punto de entrada para análisis entre fuentes |
| `enhogar_2024` | Personas y hogares con factores de expansión | Estimaciones muestrales de hogares |
| `one_datos` | Clima, fenómenos naturales e ingresos/gastos de gobiernos locales | Análisis territorial, ambiental y fiscal; cada dominio conserva su grano |

## Bases adicionales de ONE

Las bases XLSX entregadas por ONE se preservan en `data/raw/one_xlsx/` con tamaño,
SHA256 y URL de origen. Sus hojas de datos se convierten reproduciblemente a CSV
en `data/raw/one_csv/` y se cargan con `load-one-datasets.ps1`.

La base `one_datos` contiene tablas `raw` de texto para auditoría, tablas `public`
tipadas y `meta.fuentes`/`meta.calidad` para procedencia y conteos. Los gastos e
ingresos anuales también tienen vistas acumuladas, pero no se mezclan con clima o
eventos porque sus unidades de análisis son distintas. En la capa central,
`censos_linea_tiempo.fdw_one_datos` expone las diez tablas anuales/originales por
medio de `postgres_fdw`.

Para regenerar solo el SQL sin conectarse al servidor:

```powershell
powershell.exe -NoProfile -ExecutionPolicy Bypass `
  -File .\load-one-datasets.ps1 -GenerateOnly
```

## Corrección de la llave de hogar de 2022

`PHOGAR` no es una llave nacional: solo representa el número de hogar dentro de una vivienda y toma principalmente el valor `1`. No debe usarse por sí solo para unir personas.

La fuente correcta para relaciones intrahogar es la base unificada oficial del XCNPV. El enlace actual de ONE está roto, pero se recuperó la captura oficial del 22 de julio de 2025 preservada por Internet Archive. `load-unificada-xcnpv.sql` conserva el orden publicado y genera `hogar_id` como la suma acumulada de filas con `P25_ORDEN = 1`. La carga falla si no se cumplen simultáneamente estos controles:

- 10,773,983 personas;
- 3,736,217 inicios de bloque de convivencia;
- 3,726,936 hogares particulares con jefatura;
- 9,281 bloques de viviendas colectivas, con 48,090 personas;
- un único comienzo (`P25_ORDEN=1`) en cada bloque;
- una sola jefatura en cada hogar particular y ninguna en los bloques colectivos.

Este `hogar_id` es reproducible para el archivo oficial recuperado, pero no es un identificador publicado por ONE. Las capas de parejas excluyen íntegramente las viviendas colectivas. La fuente contiene una sola anomalía de orden: la fila 8,141,679 repite `P25_ORDEN=2` dentro del bloque 2,843,047. Se conserva el dato original, se registra como advertencia y `id_persona` usa `fila_origen`, que sí es único.

## Capa de relaciones y parejas

Dentro de `censo_2022`:

- `analitica.xcnpv_unificada`: microdato tipado con `hogar_id` válido;
- `analitica.personas_relaciones_2022`: variables y etiquetas listas para consultas;
- `analitica.parejas_jefatura_2022`: una fila por hogar con una jefatura y exactamente una pareja declarada;
- `analitica.parejas_profesiones_2022`: dos filas por pareja para analizar la profesión de cada persona frente a la de su pareja;
- `analitica.matriz_campos_estudio_parejas_2022`: combinaciones de campos amplios ISCED-F 2013;
- `analitica.matriz_ocupaciones_parejas_2022`: combinaciones de grandes grupos ocupacionales CNO;
- `analitica.resumen_parejas_2022`: indicadores generales de edad y similitud educativa/ocupacional;
- `meta.controles_calidad_analitica`: controles ejecutados y estado.

En `censo_2010`, `analitica.parejas_jefatura_historica` contiene 1,443,521 parejas comparables en características demográficas y educativas. La base central une 2010 y 2022 en `censos_linea_tiempo.analitica.parejas_historicas`. El CSV público de personas de 2002 cargado no contiene llaves de vivienda, hogar ni persona; por eso se conserva para análisis individuales o agregados, pero no se fabrican enlaces de pareja. Las consultas iniciales están en `example-pair-queries.sql`.

La pareja se identifica únicamente cuando el hogar tiene una jefatura (`P28=1`) y una sola esposa, esposo, compañera o compañero (`P28=2`). Es una relación conviviente de alta confianza, pero no representa parejas que viven separadas ni permite enlazar todas las parejas secundarias de hogares extensos.

“Profesión” debe declararse de forma precisa:

- `campo_estudio_*`: carrera o campo de formación;
- `ocupacion_*`: ocupación actual o última ocupación;
- `rama_actividad_*`: actividad económica de la empresa o institución.

## Preparación reproducible

```powershell
$env:PG_RECOVERY_PASSWORD = '<clave>'
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\prepare-analytics.ps1
Remove-Item Env:\PG_RECOVERY_PASSWORD
```

El proceso omite la recarga pesada cuando `analitica.xcnpv_unificada` ya existe con el conteo validado y conserva las capas derivadas si ya están listas. Use `-ForceReload` solo para reconstruir el microdato desde el CSV oficial y `-ForceDerived` para regenerar parejas y matrices.

## Respaldo

```powershell
$env:PG_RECOVERY_PASSWORD = '<clave>'
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\backup-analytics.ps1
Remove-Item Env:\PG_RECOVERY_PASSWORD
```

El respaldo usa el formato directory de `pg_dump`, compresión Zstandard y validación con `pg_restore --list`. Por defecto queda fuera del repositorio en `C:\Users\leona\PostgreSQL_Backups`.

## Controles de salud

La comprobación completa con `pg_amcheck` verificó 1,973 relaciones y 1,832,686 páginas sin encontrar corrupción. Los mensajes `invalid record length ... got 0` observados al arrancar correspondían al final normal del WAL durante la recuperación automática posterior a un cierre incorrecto.

## Fuente oficial

- [Datos y estadísticas de la Oficina Nacional de Estadística](https://www.one.gob.do/datos-y-estadisticas/)
- [Libro de códigos de personas XCNPV 2022](https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/BD_XCNPV/Libro_de_c%C3%B3digos_de_base_de_datos_de_Persona_XCNPV.htm)
