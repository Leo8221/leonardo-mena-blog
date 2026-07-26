# Extractos del Censo 2022

Estos CSV son extractos reales de PostgreSQL `censo_2022`, no datos sintéticos. Fueron generados y auditados el 20 de junio de 2026 desde `public.personas_limpia`, `diccionarios.cno_2019` y las vistas de estudio-trabajo conservadas en `../../sql/`.

Archivos:

- `resumen_cobertura.csv`: denominadores y cobertura de cruces.
- `calificacion_25_34_educacion.csv`: distribución de calificación ocupacional por sexo y educación.
- `top_ocupaciones_superior_25_34.csv`: primeras ocupaciones entre personas de 25–34 años con educación superior.
- `campos_estudio_graduados_20_65.csv`: campos de estudio válidos entre graduados de 20–65 años.

Limitación principal: entre los 461,601 ocupados de 25–34 años con educación superior, 269,315 tienen descripción ocupacional enlazada al CNO 2019 (58.3%). Los gráficos ocupacionales son condicionales a ese cruce y lo declaran en subtítulo o fuente.

El archivo `../processed/actividad-provincia-censo-2022.csv` contiene la rama CNAE agrupada que concentra más empleo observado en cada provincia, calculada desde `public.personas_limpia` del Censo 2022. Es un indicador de concentración de empleo, no de PIB provincial ni de valor agregado.
