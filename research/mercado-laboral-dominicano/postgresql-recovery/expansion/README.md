# Series históricas y bases para análisis en PostgreSQL

La base **`economia_rd`** reúne 45 tablas inventariadas, **17.186.057 filas** y **35 vistas analíticas**, sin reemplazar los censos, la ENCFT ni las otras bases existentes. El volumen incluye tablas relacionadas de compras públicas, diccionarios y catálogos: no representa 17 millones de personas ni de observaciones independientes. Tamaño al validar: aproximadamente **5,41 GiB**.

Preparación técnica: **30 de agosto de 2026**. El corte estadístico depende de la serie; no debe confundirse con la fecha de descarga. Los datos originales, las unidades y los períodos disponibles se conservan sin rellenar ausencias.

## Empezar a consultar

Conectar a PostgreSQL en `127.0.0.1`, puerto `5433`, base `economia_rd`. Usar el esquema **`analitica`** para empezar. También están disponibles las mismas 35 vistas desde la base **`censos_linea_tiempo`**, esquema **`fdw_economia`**; allí se integran con las fuentes anteriores sin volver a copiar las tablas.

```sql
-- Qué hay, a qué nivel se observa y con qué limitaciones.
SELECT tabla, titulo, periodo, grano, filas, notas
FROM analitica.catalogo_fuentes ORDER BY fuente, tabla;

-- Series mensuales preparadas para explorar.
SELECT * FROM analitica.macro_mensual ORDER BY periodo;

-- Buscar indicadores internacionales y ver su cobertura específica en RD.
SELECT indicador, nombre, desde_rd, hasta_rd, observaciones_rd
FROM analitica.catalogo_indicadores
WHERE nombre ILIKE '%educaci%' AND observaciones_rd > 0;

-- Advertencias registradas: no quedan ocultas en los archivos del proyecto.
SELECT * FROM analitica.calidad_datos WHERE estado <> 'ok';
```

En la base central, sustituir `analitica.` por `fdw_economia.`. PostgreSQL no permite escribir `economia_rd.analitica.tabla` para cruzar bases: la conexión o la vista federada elige la base.

[example-queries.sql](example-queries.sql) contiene **19 consultas ejecutadas**, agrupadas en 18 ejemplos, para matrícula, pruebas, inflación, dólar, empleo formal, comercio, presupuesto, compras, indicadores internacionales y encuestas. Las dos consultas de diccionarios físicos del ejemplo 17 requieren conexión directa a `economia_rd`; desde la central se puede usar `fdw_economia.diccionario_variables`.

## Contenido y cobertura

| Fuente / tema | Histórico cargado | Punto de entrada y tamaño |
|---|---|---|
| BCRD: precios, canasta, actividad, remesas, reservas, tasas, crédito y PIB | Desde 1984, según serie; la mayoría de las series mensuales llega a julio de 2026 | `series_economicas`: 105.295 observaciones; `catalogo_series`, `macro_mensual`, `canasta_familiar` |
| BCRD: dólar | Promedios desde 1985; diario desde 1991 hasta 28-08-2026 | `tipo_cambio`: 20.520 observaciones de compra/venta, con frecuencia y estadística separadas |
| TSS: empleo y empleadores cotizantes | Junio de 2003–junio de 2026; 277 meses consecutivos | `empleo_formal`, incluye variación interanual del empleo |
| DGA: comercio por capítulo arancelario | Enero de 2017–marzo de 2026 | `comercio_capitulo`: 10.553 filas de importaciones y 9.966 de exportaciones; USD FOB y millones de USD explícitos |
| DIGEPRES: ejecución mensual por clasificadores | Gastos desde 2004, ingresos desde 2009; algunos ámbitos desde 2014; hasta 2026 según archivo | 318.595 celdas fuente; `presupuesto_mensual` excluye unidades pendientes y filas no conciliadas; `catalogo_presupuesto` para elegir una clasificación |
| DGCP / Open Contracting Partnership | Archivos anuales de 2015–2026; 2026 incompleto | 600.314 procesos, 678.198 contratos; 13 tablas relacionadas con artículos, adjudicaciones, oferentes y entidades; 15.326.217 filas relacionales en total |
| MINERD: matrícula | Cursos 2015-2016–2024-2025 | `educacion_matricula_anual`; nivel, sector, sexo, edad, nacionalidad y detalle en tablas/vistas específicas, con coberturas diferentes |
| MINERD: centros y oferta | Directorio 2021; cursos 2022-2023, 2023-2024 y 2024-2025 | `educacion_centros_historicos`; último curso: 24.070 registros de oferta y 10.648 códigos de centro, matrícula total 2.612.882 |
| MINERD: pruebas nacionales | 2016–2024, sin 2021 | 57.586 filas originales; `educacion_pruebas`: 54.821 contenidos distintos, con banderas de calidad; `educacion_pruebas_anuales` |
| Banco Mundial, WDI | 1960–2025, según indicador y territorio | 1.083.106 valores publicados; 1.497 indicadores con observaciones y catálogo de 1.498; RD: 47.009 valores y 1.363 indicadores |
| Educación internacional, WDI | Histórico anual variable | `educacion_internacional`: 156 indicadores y 79.000 valores, de los que 2.969 corresponden a RD |
| ENIEF | Rondas 2019 y 2023 | `enief`: 6.676 personas observadas, 897 columnas fuente, más año de encuesta en la vista |
| Encuesta Nacional a las MiPyMES | Levantamiento 2023; buena parte de los flujos refiere a 2022 | 13.130 establecimientos; `mipymes`, `mipymes_resumen`; 127 columnas incluyendo fila de origen en la tabla completa |
| ENAE | Edición 2024 | 1.225 empresas, 747 columnas incluyendo fila de origen y diccionario de 2.274 filas; `enae` |
| ENHOGAR | 2022, complementa la base 2024 ya existente | 105.611 registros en Personas y 35.678 en Elegidos, ambos con diccionarios; `enhogar_2022_personas` y tablas completas en `enhogar_2022` |

Las series WDI también cubren salud, población, pobreza, desigualdad, empleo, género, energía, medioambiente, agricultura, infraestructura, tecnología, comercio y finanzas. Se descargaron **23 países más los agregados de América Latina y el Caribe y el mundo**, no solamente RD: DOM, HTI, CRI, PAN, GTM, HND, SLV, NIC, CUB, JAM, MEX, COL, PER, BRA, CHL, URY, ARG, ECU, BOL, PRY, TTO, USA, ESP, LAC y WLD. Hay traducciones al español de nombres y definiciones cuando las publica la API.

La [ENCFT ya incorporada](../encft/README.md) sigue disponible en `encft` / `fdw_encft`, con 41 trimestres de 2016-T1 a 2026-T1. No se duplicó aquí.

## Cómo trabajar con el tiempo

- **Series agregadas:** BCRD, TSS, DGA, presupuesto y WDI tienen fechas y frecuencias explícitas. Las fechas mensuales y trimestrales identifican el inicio del período; una fecha `2026-04-01` trimestral representa 2026-T2.
- **Unidades administrativas repetidas:** se puede seguir un código de centro, entidad contratante, capítulo arancelario o provincia cuando la fuente lo permite. No se garantiza que todos estén presentes cada año ni que nunca cambien sus límites o clasificación.
- **Encuestas repetidas:** ENIEF 2019/2023, ENHOGAR y ENCFT permiten comparar poblaciones y grupos en distintos períodos. No se ha certificado un panel que enlace a la misma persona o empresa entre rondas. No unir encuestas por números de fila o IDs aparentemente iguales.
- **Años parciales:** mostrar los meses/trimestres disponibles antes de comparar totales anuales. No extrapolar automáticamente 2026 ni sustituir `NULL` por cero.
- **Stocks y flujos:** empleo cotizante, crédito, depósitos y reservas no deben sumarse a lo largo del tiempo como si fueran ventas. Promedios y cierres de período son estadísticas distintas.
- **Precios y bases:** no sumar índices; no mezclar monedas corrientes, constantes o PPP. El PIB conserva separados nivel nominal, índice de volumen, ponderaciones, crecimiento e incidencia. Las ponderaciones fuente expresadas como fracción se convierten explícitamente a porcentaje.

## Precauciones que afectan a los resultados

### Educación

La matrícula 2015-2016–2023-2024 se toma del tabulado oficial por regional; los totales por regional y por distrito coinciden en los nueve cursos y cuatro niveles. El último curso se obtiene del directorio de oferta: **revisar comparabilidad de cobertura antes de interpretar su cambio como deserción o crecimiento**. No sumar al mismo tiempo regionales y distritos. Un centro puede aparecer varias veces por tanda, nivel y modalidad; usar la vista agregada de centros para contarlos.

El código de período fuente `202120222`, en 140 filas, se interpreta explícitamente como `2021-2022` en las vistas y se conserva intacto en la tabla original. Los códigos de centros de pruebas y de matrícula tienen estructuras distintas: no hay una tabla de correspondencia validada para unirlos. El directorio 2021 no se fuerza a un curso escolar; las coordenadas antiguas requieren revisar signos y límites geográficos antes de hacer mapas.

En pruebas nacionales se detectaron **2.765 repeticiones exactamente idénticas**: 2.764 en 2019 y una en 2023. Solo se colapsan en las vistas analíticas. Después persisten **494 claves ambiguas, que afectan a 988 registros con contenidos distintos**; se conservan con `clave_ambigua = true`, sin escoger arbitrariamente una versión. Hay 185 registros cuyos componentes por sexo no suman el total y 638 con discrepancia entre promovidos/aplazados y total. `diferencia_sexo` y `diferencia_resultado` los señalan. Las vistas anuales incluyen contadores de estas advertencias.

Las pruebas miden **participaciones por convocatoria**, no estudiantes únicos. `N/A` de aplazados en 2020 se mantiene como ausencia, 2021 no se inventa y la cobertura de 2024 necesita revisión antes de comparar tendencias. Filtrar convocatoria y modalidad; no presentar el agregado sin estas advertencias como una serie homogénea de desempeño.

### Presupuesto, comercio y compras

En DIGEPRES, el texto del encabezado no basta para conocer la escala: algunos Excel muestran millones mediante formato de celda, aunque almacenan pesos. El cargador inspecciona esos formatos y concilia la suma mensual con el total publicado. **948 celdas de unidad pendiente y 45 filas no conciliadas se excluyen de `presupuesto_mensual`**; permanecen en las tablas fuente y en la auditoría. La corrección puntual del formato de enero de 2010 en la fila TOTAL GASTOS está documentada en el código. Las clasificaciones alternativas y sus subtotales no se suman entre sí; elegir `fuente_id`, código y concepto. Los montos aprobado/vigente y los totales anuales no se mezclan con los meses ejecutados.

La escala del CSV de DGA se contrastó con la [publicación oficial de comercio de 2018, pp. 6 y 20](https://www.aduanas.gob.do/media/meapdim1/revista-de-comercio-enero-diciembre-2018-v1.pdf): importaciones 2017/2018 y exportaciones 2017. El campo físico está en **USD**, aunque el catálogo mencione millones. Exportaciones de 2018 no coincide exactamente con aquella edición; no se sustituyó el valor publicado en el CSV ni se atribuyó la diferencia a una causa no comprobada.

DGCP conserva contratos separados de artículos, proveedores y ofertas. **Monto contratado no es pago**, ni fecha de inicio contractual es fecha de firma. Unir proveedores o artículos y luego sumar el monto de contrato puede multiplicarlo: usar `compras_contratos` o `compras_mensuales`. No sumar monedas. Hay 33 contratos con inicio posterior al fin en la fuente; no usarlos como duraciones válidas. Los CSV originales conservan los timestamps; las columnas analíticas de fecha se cargan como fecha calendario. Se omitieron de la tabla de partes teléfonos, correos, contactos y direcciones de calle, innecesarios para estos análisis.

### Encuestas y WDI

Usar los factores propios: MiPyMES `factor_expansion` en la vista (`factor` en origen), ENIEF `factor_exp` por `anio_encuesta`, ENAE `factor_expansion`, ENHOGAR Personas `f_expansion` y Elegidos `fexpansion10m`. El universo de Elegidos difiere del de Personas; existe un elegido sin correspondencia en el módulo de Personas. No trasladar factores entre módulos. Para errores estándar, usar además el diseño muestral que corresponda.

ENIEF contiene cambios de cuestionario entre rondas; los nulos estructurales no significan respuesta negativa. El año de edición de MiPyMES o ENAE no fija por sí solo el período de cada flujo financiero. Consultar `diccionario_variables` y los diccionarios oficiales antes de usar códigos de respuesta.

WDI solo guarda valores publicados, sin interpolación. Un indicador puede tener pocos años en RD aunque el catálogo global empiece en 1960. La unidad de la API a veces está vacía: la vista remite al nombre y la definición; no se inventa una. Las definiciones, fuentes, revisiones y cambios metodológicos deben revisarse para cada comparación. LAC y WLD son agregados, no países.

## Procedencia, calidad y reproducción

Se conservan 272 archivos del catálogo nacional descargado, además de los 12 paquetes anuales de DGCP, el histórico de dólar y los 150 lotes de WDI. **Descargar no significa cargar todas las hojas**: los cargadores seleccionan las tablas descritas arriba, con una interpretación explícita de estructura y unidades. El resto permanece disponible para ampliaciones posteriores, sin presentarlo como ya analizable en SQL.

- `raw/`: originales descargados y CSV de DGCP; excluido de Git.
- `prepared/`: archivos de trabajo; excluido de Git.
- `meta.tablas` / `meta.columnas`: fuente, ruta, hoja, SHA256, dimensiones, nombres originales, tipos, nulos y notas. Los manifiestos específicos conservan el detalle de los conjuntos formados por varios archivos.
- `meta.archivos_bcrd`, `meta.archivos_digepres`, `meta.dgcp_cargas` y `meta.wdi_cargas`: procedencia y control de cargas por archivo/lote.
- [qa/validation-summary.json](qa/validation-summary.json), [inventario](qa/table-inventory.csv) y [controles](qa/database-checks.csv): conteos exactos y **24 controles SQL, cero errores bloqueantes y nueve advertencias explícitas**.
- [qa/roundtrip-dgcp.csv](qa/roundtrip-dgcp.csv): 26 comparaciones CSV/SQL, todas las columnas cargadas de hasta cuatro filas por tabla en 2015 y 2026. Las otras cargas también comparan muestras de filas completas. Esto no equivale a comparar cada celda de todos los archivos.
- [qa/example-queries.csv](qa/example-queries.csv): ejecución de las 19 consultas de ejemplo.
- [qa/central-registration.json](qa/central-registration.json): registro central y lecturas de comprobación.
- [qa/source-snapshot-check.json](qa/source-snapshot-check.json): verificación completa de hashes de archivos descargados y CSV extraídos; `restore-snapshot.ps1 -VerifyOnly` no usa la red.
- [qa/final-smoke.json](qa/final-smoke.json): controles de valores presentes en la vista macro, conciliación de lotes y disponibilidad de las 35 vistas y 13 fuentes en la central.
- [validation.ipynb](validation.ipynb): cuaderno ejecutado que revisa la evidencia guardada; **no vuelve a consultar la base en vivo**.

Los originales de las encuestas reutilizan rutas existentes del repositorio, indicadas en `load-surveys.R` y `load-enhogar2022.R`. Son insumos necesarios para reproducir desde cero. No se guardan contraseñas. Ejecutar desde la raíz del repositorio con la clave presente temporalmente en `PG_RECOVERY_PASSWORD`.

```powershell
# Recuperar la versión registrada; conserva los manifiestos y valida SHA256.
# Agregar -VerifyOnly para comprobar los archivos locales sin descargar.
powershell.exe -NoProfile -ExecutionPolicy Bypass `
  -File .\research\mercado-laboral-dominicano\postgresql-recovery\expansion\restore-snapshot.ps1

# Una sesión R por script, usando la runtime y el UTF-8 del proyecto.
$scripts = @('load-surveys.R','load-education.R','load-time-series.R',
  'load-fx.R','load-trade-tss.R','load-fiscal.R','load-enhogar2022.R',
  'load-dgcp.R','load-wdi.R','create-analysis-views.R','verify-expansion.R',
  'finish-catalog.R','verify-example-queries.R','check-roundtrip.R','register-central.R',
  'final-smoke.R')
foreach ($script in $scripts) {
  powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-r.ps1 `
    ("research/mercado-laboral-dominicano/postgresql-recovery/expansion/" + $script)
  if ($LASTEXITCODE -ne 0) { throw "Falló $script" }
}

powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\tools\run-python.ps1 `
  research/mercado-laboral-dominicano/postgresql-recovery/expansion/build-validation-notebook.py
```

La recarga verifica archivos/conteos y omite tablas o lotes ya cargados. **No es un actualizador automático universal**: si el productor revisa un archivo, hay que revisar esa nueva versión y su estructura; los cargadores rechazan reemplazos no previstos. `fetch-public.ps1`, `fetch-wdi.ps1` y los demás scripts `fetch-*` documentan la exploración/descarga inicial, pero pueden descubrir nuevas versiones; no usarlos para sobrescribir el manifiesto de esta versión ya cargada. No usar los scripts de reparación puntual como parte de una carga normal. La federación agrega un servidor, un esquema y 13 entradas al catálogo, sin reconstruir las fuentes previas.

Esta incorporación no modifica artículos, Atlas ni `docs/`, no requiere renderizar Quarto y no publica el sitio.

## Fuentes públicas

- [BCRD: estadísticas](https://www.bancentral.gov.do/estadisticas/), archivos originales enlazados en los manifiestos.
- [MINERD: listados de datos abiertos](https://www.ministeriodeeducacion.gob.do/transparencia/datos-abiertos/listados).
- [TSS](https://tss.gob.do/) y [DGA: datos abiertos](https://www.aduanas.gob.do/datos-abiertos/).
- [DIGEPRES: gastos](https://digepres.gob.do/gastos/) e [ingresos](https://digepres.gob.do/ingresos/).
- [DGCP vía catálogo de Open Contracting Partnership](https://data.open-contracting.org/es/publication/22).
- [Banco Mundial: RD](https://data.worldbank.org/country/dominican-republic) y [documentación de la API](https://datahelpdesk.worldbank.org/knowledgebase/articles/898581-api-basic-call-structures).
- [ONE: datos y estadísticas](https://www.one.gob.do/datos-y-estadisticas/).
