# Estado de calidad de las bases analíticas

Fecha técnica de validación: 2026-07-17. Los años censales y de encuesta son períodos de referencia, no la fecha de generación.

## Resultado

La instancia PostgreSQL 18 recuperada está operativa y no presenta corrupción física detectada. La base unificada oficial del XCNPV 2022 quedó cargada, tipada, indexada y vinculada a capas reutilizables de personas, hogares y parejas. Las capas de parejas cubren 2010 y 2022; 2002 queda excluido de enlaces intrahogar porque el CSV público de personas disponible no contiene identificadores.

## Integridad física

- Servicio: `postgresql-x64-18-recovered`, puerto 5433.
- Checksums de páginas, `fsync`, `full_page_writes` y `synchronous_commit`: activos.
- `pg_amcheck`: 1,973 relaciones y 1,832,686 páginas verificadas sin corrupción antes de crear las nuevas capas.
- Revisión posterior focalizada: 25 relaciones y 779,612 páginas en `censo_2022.analitica`, y 11 relaciones y 71,194 páginas en `censo_2010.analitica`, sin errores.
- Índices inválidos o no listos en la revisión inicial: cero.
- Índices inválidos o no listos después de la preparación, en las seis bases: cero.

## Fuente XCNPV 2022

- Archivo preservado: `D:\datos_one_censos\2022\BD_FINAL_VIVIENDA_HOGAR_PERSONA_XCNPV_PUB.csv`.
- Tamaño: 2,252,276,900 bytes.
- SHA-256: `7E848405B2743774FBC0445BB9412F51007C064F61DC1C22CA77913613D15D70`.
- Personas: 10,773,983.
- Bloques de convivencia: 3,736,217.
- Hogares particulares con jefatura: 3,726,936.
- Viviendas colectivas: 9,281 bloques y 48,090 personas; se excluyen de parejas.

`PHOGAR` no es una llave nacional y no se usa para enlazar personas. `hogar_id` es una secuencia reproducible derivada de los reinicios de `P25_ORDEN=1` en el archivo exacto validado.

Existe una anomalía preservada de la fuente: la fila 8,141,679 repite `P25_ORDEN=2` dentro del bloque 2,843,047. No se corrige silenciosamente. `fila_origen` e `id_persona` son las llaves únicas de persona.

## Parejas

| Año | Parejas identificadas | Regla de enlace | Estado |
| --- | ---: | --- | --- |
| 2002 | No disponible | El archivo de personas no contiene llave de hogar | Advertencia documentada |
| 2010 | 1,443,521 | Una jefatura y exactamente una pareja declarada | Validado |
| 2022 | 1,735,238 | `P28=1` y exactamente un `P28=2` | Validado |

En 2022, 1,731,814 parejas cumplen el filtro conservador de edades plausibles. Entre las parejas con información observable, los indicadores descriptivos iniciales son 55.48 % con el mismo nivel educativo, 32.03 % con el mismo campo amplio de estudio y 21.34 % con el mismo gran grupo ocupacional. Estos porcentajes son controles de salida, no interpretaciones causales.

La unión CNO deja 29 roles de persona sin descripción entre los códigos no especiales observados en parejas, distribuidos en 13 códigos raros. No se imputan ni se reasignan; las matrices ocupacionales requieren un grupo CNO reconocido.

La capa representa parejas convivientes jefatura–cónyuge/compañero. No cubre parejas que viven separadas ni enlaza exhaustivamente parejas secundarias en hogares extensos.

## Correcciones semánticas aplicadas

- Estado conyugal 2022: `P63`; `P66` corresponde a hijos sobrevivientes.
- Campo o carrera: `P45` / ISCED-F.
- Ocupación actual o última: `P60` / CNO.
- Rama de actividad del empleador: `P62` / CNAE.
- Los códigos ocupacionales conservan su nivel publicado; no se rellenan indiscriminadamente a cuatro dígitos.
- ENHOGAR 2024 conserva factores de expansión; los conteos muestrales no se presentan como población.

## Tablas canónicas

- `censo_2022.analitica.xcnpv_unificada`
- `censo_2022.analitica.personas_relaciones_2022`
- `censo_2022.analitica.parejas_jefatura_2022`
- `censo_2022.analitica.parejas_profesiones_2022`
- `censo_2022.analitica.matriz_campos_estudio_parejas_2022`
- `censo_2022.analitica.matriz_ocupaciones_parejas_2022`
- `censos_linea_tiempo.analitica.parejas_historicas`
- `censos_linea_tiempo.catalogo.fuentes`
- `censos_linea_tiempo.catalogo.metricas`

## Respaldo lógico verificado

Las seis bases y los objetos globales quedaron respaldados en `C:\Users\leona\PostgreSQL_Backups\recovered-analytics-20260717_203605`. El conjunto ocupa 1.181 GB en 41 archivos. Cada directorio fue validado con `pg_restore --list`; el manifiesto registra `verified_with_pg_restore_list=true` para todas las bases. No se ejecutó una restauración completa en una segunda instancia, por lo que la validación confirma legibilidad estructural del archivo, no un ensayo integral de recuperación.

## Limitaciones pendientes

- Recuperar una publicación 2002 con llaves permitiría construir parejas históricas de ese año; con el archivo actual no es posible.
- Las clasificaciones educativas y ocupacionales cambian entre censos. Las tendencias deben usar grupos amplios y notas explícitas de comparabilidad.
- Una futura capa de ENCFT debe conservar trimestre, ponderadores y definiciones oficiales; los indicadores laborales censales no sustituyen las tasas oficiales trimestrales.
