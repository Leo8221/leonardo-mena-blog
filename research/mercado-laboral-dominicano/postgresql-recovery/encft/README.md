# ENCFT en PostgreSQL

Incorporación local y reproducible de la **Encuesta Nacional Continua de Fuerza de Trabajo (BCRD)**. No modifica los Excel/Parquet originales ni elimina tablas de los censos.

## Alcance

- Base: `encft`, en `127.0.0.1:5433`.
- Fuente local: `C:/Users/leona/Desktop/encuesta_trabajo_rd`.
- Corte de los datos: **31 de marzo de 2026**. Cobertura: **2016-T1 a 2026-T1**, 41 trimestres y 123 meses de entrevista.
- Las fechas de ejecución y de modificación de archivos se registran aparte; no equivalen al corte de datos.
- Granularidad: observación de persona/hogar/vivienda en un período de entrevista. Los 790.993 registros de miembros **no son 790.993 personas distintas**: hay reentrevistas.
- Incorporación verificada el **30 de agosto de 2026**: 3.109 MiB (aproximadamente 3,04 GiB), con el acceso desde `censos_linea_tiempo` comprobado en ambos extremos de la cobertura.

| Tabla física | Filas | Función |
|---|---:|---|
| `microdatos.miembros` | 790.993 | Los 616 campos del Parquet combinado, más `fila_origen` |
| `microdatos.hogares` | 258.764 | Los 81 campos originales, más `fila_origen` |
| `microdatos.viviendas` | 258.607 | Los 58 campos originales, más `fila_origen` |
| `meta.diccionario` | 115.178 | Códigos, textos, sección y período de vigencia |
| `meta.tasas_monedas` | 399 | Tipos de cambio asociados a período, moneda y mes relativo |

Se conserva **una copia física por módulo**. Las capas analíticas son vistas, no copias `raw`/`limpia` adicionales. Las variables de vivienda y hogar que ya trae el módulo Miembros se conservan como parte de esa fuente; no se afirma una normalización física completa.

## Consultas recomendadas

- `analitica.personas`: variables demográficas, laborales, de diseño muestral e ingresos seleccionados, con claves de enlace verificadas. Para otros campos usar `microdatos.miembros` y `meta.diccionario`.
- `analitica.hogares` y `analitica.viviendas`: módulos completos y sus claves.
- `analitica.indicadores_trimestrales`: empleo, participación, desocupación abierta e informalidad, con pesos trimestrales y universo de **15 años y más**.
- `analitica.promedios_anuales`: promedio de niveles de los cuatro trimestres; devuelve `NULL` en años incompletos. No calcula una suma de personas únicas.
- `meta.columnas`: correspondencia entre nombres/tipos originales y SQL.
- `meta.archivos`, `meta.ingestas` y `meta.controles_calidad`: procedencia, SHA256, conteos y advertencias.

En `censos_linea_tiempo`, `fdw_encft` expone estas vistas sin duplicar microdatos. También hay accesos `analitica.encft_indicadores_trimestrales` y `analitica.encft_promedios_anuales`, y la entrada `encft_miembros` en `catalogo.fuentes`. La encuesta **no se incorpora a la unión de personas censales**.

## Reglas de uso

1. `FACTOR_EXPANSION` está diseñado para **trimestres**. No estimar una población mensual sumando únicamente las entrevistas de un mes ni sumar factores entre trimestres para contar personas distintas.
2. Los indicadores laborales de la capa analítica filtran `edad >= 15`. Algunos indicadores precalculados de la fuente también contienen población de 10 a 14 años; usarlos sin filtrar cambia los totales.
3. La desocupación abierta usa `DESOCUPADO / PEA`. `DESOCUPADO_INI` se preserva por separado y no se sustituye automáticamente por `DESOCUPADO`.
4. `GRUPO_EMPLEO` identifica empleo formal/informal. `GRUPO_SECTOR` es otra clasificación; no intercambiarlas.
5. Los ingresos calculados desde `INGRESO_ASALARIADO` están mensualizados según las notas del productor. No sumar indiscriminadamente campos originales, imputados y calculados ni volver a anualizar/mensualizar componentes sin revisar su definición.
6. En tasas de monedas, el mes 6 corresponde al anterior a la entrevista. Las remesas y otros ingresos pueden usar distintos meses/tasas: conservar la temporalidad del flujo.
7. El diseño muestral documenta inferencia para total país, cuatro grandes regiones y zona urbana/rural. Tener código provincial o municipal no garantiza precisión para estimaciones a esos niveles. Para errores estándar se requiere respetar estratos, UPM, ponderación y diseño; aquí no se calculan intervalos de confianza.
8. Las estimaciones generadas se etiquetan como cálculo propio con microdatos de la fuente. La validación de dos indicadores publicados no certifica toda la serie como publicación oficial.

## Enlaces entre módulos y límites conocidos

- Clave de vivienda: `(periodo, upm, vivienda)`.
- Clave de hogar: `(periodo, upm, vivienda, hogar)`.
- Clave de miembro: `(periodo, upm, vivienda, hogar, miembro)`.
- Las vistas exponen estas combinaciones como `clave_*_periodo`. Los identificadores originales también se conservan.
- En **2023**, `ID_HOGAR` de Miembros no coincide con el del módulo Hogar: usarlo directamente deja **73.840 registros sin enlace**. La clave estructural enlaza el 100% de los miembros sin multiplicar filas y coincide en provincia/municipio. No se alteran los ID originales para forzar coincidencias; subsisten las diferencias de contenido siguientes.
- En **2023-T4**, 2.627 registros de Miembros tienen un factor distinto del de Hogar (924 observaciones de hogar). La diferencia máxima observada es 63,52, aproximadamente 12,38% del factor de Hogar en ese caso. Una muestra de claves y factores, incluida la mayor diferencia, se verificó contra el Excel original: no es una alteración introducida por PostgreSQL. Se conserva el factor de **cada módulo**; no reemplazar el de Miembros por el de Hogar al unir. No se ha determinado la causa de la discrepancia en el productor.
- En noviembre de 2023 hay **7 registros de Miembros** cuyo tamaño declarado de hogar difiere del módulo Hogar. Se preservan; requieren revisión antes de usar ese enlace para composición familiar. Los indicadores laborales se calculan directamente desde Miembros y no dependen de un reemplazo de pesos/atributos con Hogar.
- Las claves estructurales son para enlaces **en el mismo período**, no una prueba de identidad longitudinal. El nuevo cálculo (`qa/panel-boundary-audit.csv`) encuentra 12.363 IDs comunes entre 2020-T4 y 2021-T1, corrigiendo el cero del log antiguo. Entre **2023-T4 y 2024-T1 no hay coincidencias literales**. Esto no prueba una sustitución real de todas las personas: los identificadores deben validarse antes de estudiar transiciones. No se declara validado un panel.
- Hay **una vivienda con factor de expansión nulo**, conservada sin imputación. `analitica.viviendas.factor_valido` la señala. También hay valores atípicos en características físicas que deben revisarse para un análisis de vivienda.
- El Parquet de tasas solo incluía 2016–2022. Se recuperaron **126 filas** adicionales de las hojas `Tasa monedas` de 2023–2026; sus nombres difieren en mayúsculas/minúsculas de las hojas anteriores.
- Los errores históricos `std::bad_alloc` de Miembros 2016/2017 permanecen en el log original, pero los Parquet finales existen y sus conteos coinciden con las dimensiones de los Excel. Esta conciliación no supone haber comparado cada celda de cada Excel con su Parquet.
- Se mantienen los cambios de esquema y tipos de la fuente combinada, incluidos campos de texto que contienen montos/códigos y columnas completamente nulas. No se realizan conversiones silenciosas a números de esos textos.

## Reproducir

Ejecutar desde la raíz del repositorio, con las dependencias R `arrow`, `data.table`, `jsonlite`, `readxl`, `digest`, `DBI` y `RPostgres`. Para la lectura auxiliar de documentos se utiliza el Python instalado mediante el wrapper del repositorio (`pypdf` para PDF).

```powershell
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-r.ps1 research/mercado-laboral-dominicano/postgresql-recovery/encft/inspect-source.R
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-python.ps1 research/mercado-laboral-dominicano/postgresql-recovery/encft/inspect-documents.py
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-r.ps1 research/mercado-laboral-dominicano/postgresql-recovery/encft/profile-quality.R
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-r.ps1 research/mercado-laboral-dominicano/postgresql-recovery/encft/prepare-source.R
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-r.ps1 research/mercado-laboral-dominicano/postgresql-recovery/encft/investigate-weights.R
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-python.ps1 research/mercado-laboral-dominicano/postgresql-recovery/encft/check-source-cells.py

# Suministrar PG_RECOVERY_PASSWORD en la sesión; nunca guardarla en estos archivos.
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-r.ps1 research/mercado-laboral-dominicano/postgresql-recovery/encft/load-encft.R
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-r.ps1 research/mercado-laboral-dominicano/postgresql-recovery/encft/load-encft.R --load
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-r.ps1 research/mercado-laboral-dominicano/postgresql-recovery/encft/verify-encft.R
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-r.ps1 research/mercado-laboral-dominicano/postgresql-recovery/encft/register-central.R
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-python.ps1 research/mercado-laboral-dominicano/postgresql-recovery/encft/build-validation-notebook.py
```

Sin `--load`, el cargador solo comprueba los archivos y tipos. Con `--load`, crea la nueva base si no existe; cada módulo se carga en una transacción. Si se interrumpe, se puede repetir: verifica SHA256 y conteo de los módulos terminados y rechaza sustituir tablas distintas. No ejecuta `DROP` ni `TRUNCATE`.

Los scripts de inspección/preparación aceptan la ruta fuente indicada en su código/argumento; el cargador y verificador aceptan `ENCFT_SOURCE_ROOT`. Conexión configurable mediante `PGHOST`, `PGPORT`, `PGUSER` y `ENCFT_DATABASE`. El registro central utiliza una correspondencia de usuario de PostgreSQL FDW, como las fuentes existentes; la contraseña se recibe por entorno y no se escribe en el repositorio.

## Evidencia de validación

Los archivos de `qa/` conservan conteos, esquema, hashes, conciliación de los 55 pares módulo-año y comparación independiente SQL/R por trimestre. `verify-encft.R` valida claves, cobertura, pesos, enlaces, identidades laborales para 15+, los promedios anuales y una muestra determinista con **todas las columnas** tras el viaje Parquet → PostgreSQL. `validation.ipynb` es un cuaderno complementario ejecutado sobre esa evidencia: no reconsulta PostgreSQL ni necesita credenciales. Se reconstruye con `build-validation-notebook.py` mediante el wrapper Python del repositorio.

La ejecución final terminó sin errores: **29 controles SQL, con seis advertencias de fuente**, ocho métricas coincidentes entre SQL y R en los 41 trimestres y cuatro filas con todas las columnas comparadas en cada uno de los cinco módulos. La revisión específica del Excel comprobó 30 celdas de claves/pesos, incluida la mayor discrepancia entre módulos. Repetir el cargador también se verificó: reconoce lo ya cargado y no duplica las filas.

Referencias metodológicas locales: `2026/Notas aclaratorias base ENCFT.docx`, `2026/Diseno_muestral.pdf` y el diccionario por período. Como controles externos se usan los ocupados nacionales (5.168.878) y la desocupación abierta (5,0%) de **2025-T4**, publicados en la página 23 del [Panorama Económico de la Región Norte 2026-I del BCRD](https://cdn.bancentral.gov.do/documents/publicaciones-economicas/panorama-economico-de-la-region-norte/documents/PERN_2026-I.pdf), y los ocupados de **2016-T4 (4.310.845) y 2017-T4 (4.431.912)** publicados en la página 6 de [Crónica Central de mayo de 2018](https://cdn.bancentral.gov.do/documents/sala-de-prensa/documents/cronica2018-05.pdf).

No se realizó una auditoría estadística completa de ingresos, remesas, transiciones de panel ni de cada variable de vivienda. La incorporación conserva esas variables y documenta los límites para análisis posteriores.
