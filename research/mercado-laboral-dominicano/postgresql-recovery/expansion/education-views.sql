-- Preserve the original table. Collapse only rows identical in every source field.
CREATE OR REPLACE VIEW analitica.educacion_pruebas AS
WITH originales AS (
 SELECT p.*,to_jsonb(p)-'fila_origen' AS contenido_fuente,
 count(*) OVER(PARTITION BY to_jsonb(p)-'fila_origen') AS repeticiones_fuente
 FROM minerd.pruebas_nacionales p
), unicos AS (
 SELECT DISTINCT ON (contenido_fuente) * FROM originales
 ORDER BY contenido_fuente,fila_origen
)
SELECT periodo::integer AS anio,convocatoria,regional AS codigo_regional,
 distrito AS codigo_distrito,nivel_modalidad,codigo_de_centro AS codigo_centro_pruebas,
 nombre_de_centro AS centro,espanol,matematicas,sociales,naturales,
 cantidad_de_estudiantes AS estudiantes,cantidad_femenino AS mujeres,
 cantidad_masculino AS hombres,cantidad_promovidos AS promovidos,
 nullif(cantidad_aplazados,'N/A')::integer AS aplazados,
 100.0*cantidad_promovidos/NULLIF(cantidad_de_estudiantes,0) AS porcentaje_promovidos,
 fila_origen,repeticiones_fuente,
 count(*) OVER(PARTITION BY periodo,convocatoria,regional,distrito,nivel_modalidad,codigo_de_centro)>1 AS clave_ambigua,
 cantidad_de_estudiantes<>cantidad_femenino+cantidad_masculino AS diferencia_sexo,
 cantidad_de_estudiantes<>cantidad_promovidos+nullif(cantidad_aplazados,'N/A')::integer AS diferencia_resultado
FROM unicos;

COMMENT ON VIEW analitica.educacion_pruebas IS
'Una fila por contenido fuente distinto. Repeticiones exactamente idénticas colapsadas, sin borrar originales. Otras claves repetidas se conservan y se marcan clave_ambigua. Son participaciones, no personas únicas. Los códigos no enlazan directamente con los de matrícula.';
