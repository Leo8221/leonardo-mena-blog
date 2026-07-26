# Series del sector eléctrico dominicano

## Descarga

Las fuentes se descargan con `tools/download-mem-electricity-series.ps1` desde los catálogos oficiales del Ministerio de Energía y Minas (MEM). El manifiesto `data/raw/mem/mem-electricity-manifest.csv` conserva la URL de catálogo, la URL de origen, el archivo local, el estado y el SHA-256.

El inventario cubre:

- boletines de generación y gestión de energía disponibles para 2022–2025;
- boletines de distribución y comercialización disponibles para 2022–2025;
- informes de desempeño disponibles entre 2009 y 2025, incluidos anexos XLSX cuando el MEM los publica;
- informes de gestión comercial disponibles entre 2015 y 2023.

El portal del MEM muestra explícitamente que no existen boletines de generación o distribución para varios años anteriores. Esos vacíos se conservan como vacíos de fuente; no se interpolan.

## Figuras actuales

Las figuras del borrador se generan desde `research/build-five-post-graphics.R`. La comparación del mapa de calor de gestión usa únicamente indicadores que el boletín de diciembre de 2024 presenta con una comparación explícita frente a diciembre de 2023. Los informes adicionales quedan descargados y documentados para construir series históricas comparables después de revisar sus definiciones y anexos.

## Fuentes oficiales

- https://mem.gob.do/category/sector-electrico/boletin-de-generacion-y-gestion-de-energia/
- https://mem.gob.do/category/sector-electrico/boletin-de-distribucion-y-comercializacion-de-energia/
- https://mem.gob.do/category/sector-electrico/informe-de-desempeno/
- https://mem.gob.do/category/sector-electrico/informe-de-gestion-comercial/

## Definicion de perdidas del mapa provincial

El informe de Gestion Comercial EDE 2026 define la perdida como energia comprada o suministrada que no llega a facturarse. La tabla provincial utilizada en el mapa reporta la perdida total y no desagrega sus componentes tecnicos y no tecnicos. Por eso el mapa no atribuye automaticamente la diferencia a conexiones ilegales o fraude.
