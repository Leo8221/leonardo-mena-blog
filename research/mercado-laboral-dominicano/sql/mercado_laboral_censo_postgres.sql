/* =========================================================
   Mercado laboral Censo 2022 - PostgreSQL
   Base esperada: censo_2022
   Tabla base observada en historial pgAdmin: public.personas_limpia
   ========================================================= */

DROP VIEW IF EXISTS public.vw_mercado_laboral_censo;

CREATE VIEW public.vw_mercado_laboral_censo AS
SELECT
    PHOGAR,
    REGION,
    PROVINCIA,
    municipio,
    dmunicipal,
    zona,
    tiporeg,
    p25_orden,
    p26_sexo,
    p27_edad,
    p28_parent,
    p41,
    p42,
    p43,
    p44,
    p46,
    p47,
    p53,
    p54,
    p55,
    p56,
    p57,
    p58,
    p59,
    p60_code,
    p61,
    p62_code,
    p63,
    p64,

    CASE
        WHEN p26_sexo = 1 THEN 'Hombre'
        WHEN p26_sexo = 2 THEN 'Mujer'
        ELSE 'No declarado'
    END AS sexo,

    CASE
        WHEN zona = 1 THEN 'Urbano'
        WHEN zona = 2 THEN 'Rural'
        ELSE 'No declarado'
    END AS zona_lbl,

    CASE
        WHEN p27_edad < 15 THEN '0-14'
        WHEN p27_edad BETWEEN 15 AND 24 THEN '15-24'
        WHEN p27_edad BETWEEN 25 AND 34 THEN '25-34'
        WHEN p27_edad BETWEEN 35 AND 44 THEN '35-44'
        WHEN p27_edad BETWEEN 45 AND 54 THEN '45-54'
        WHEN p27_edad BETWEEN 55 AND 64 THEN '55-64'
        WHEN p27_edad >= 65 THEN '65+'
        ELSE 'No declarado'
    END AS grupo_edad,

    CASE
        WHEN p42 = 3 THEN 'Nunca asistio'
        WHEN p43 = 1 THEN 'Preprimaria'
        WHEN p43 = 2 THEN 'Primaria'
        WHEN p43 = 3 THEN 'Secundaria'
        WHEN p43 = 4 THEN 'Universitaria'
        WHEN p43 = 5 THEN 'Maestria'
        WHEN p43 = 6 THEN 'Doctorado'
        ELSE 'No declarado'
    END AS nivel_educativo,

    CASE WHEN p43 IN (4,5,6) THEN 1 ELSE 0 END AS educ_superior,

    CASE WHEN p43 IN (4,5,6) AND p46 = 1 THEN 1 ELSE 0 END AS graduado_superior,

    CASE WHEN p27_edad >= 15 THEN 1 ELSE 0 END AS pet,

    CASE
        WHEN p27_edad >= 15
         AND (COALESCE(p53, 0) = 1 OR COALESCE(p54, 0) = 1 OR COALESCE(p55, 0) = 1)
        THEN 1 ELSE 0
    END AS ocupado,

    -- P56 pregunta si busco trabajo. P58 es trabajo anterior y, por salto
    -- del cuestionario, queda nulo cuando P56=1; no deben exigirse juntos.
    CASE
        WHEN p27_edad >= 15
         AND COALESCE(p53, 0) <> 1
         AND COALESCE(p54, 0) <> 1
         AND COALESCE(p55, 0) <> 1
         AND p56 = 1
        THEN 1 ELSE 0
    END AS desocupado,

    CASE
        WHEN p27_edad >= 15
         AND (
              COALESCE(p53, 0) = 1
              OR COALESCE(p54, 0) = 1
              OR COALESCE(p55, 0) = 1
              OR (
                  COALESCE(p53, 0) <> 1
                  AND COALESCE(p54, 0) <> 1
                  AND COALESCE(p55, 0) <> 1
                  AND p56 = 1
              )
         )
        THEN 1 ELSE 0
    END AS fuerza_trabajo,

    CASE
        WHEN p27_edad >= 15
         AND COALESCE(p53, 0) <> 1
         AND COALESCE(p54, 0) <> 1
         AND COALESCE(p55, 0) <> 1
         AND p56 = 2
        THEN 1 ELSE 0
    END AS inactivo,

    CASE
        WHEN p27_edad BETWEEN 15 AND 24
         AND COALESCE(p53, 0) <> 1
         AND COALESCE(p54, 0) <> 1
         AND COALESCE(p55, 0) <> 1
         AND p42 IN (2,3)
        THEN 1 ELSE 0
    END AS nini,

    CASE
        WHEN p27_edad >= 15
         AND (COALESCE(p53, 0) = 1 OR COALESCE(p54, 0) = 1 OR COALESCE(p55, 0) = 1)
         AND p61 IN (3,4)
        THEN 1 ELSE 0
    END AS empleo_vulnerable,

    CASE
        WHEN p61 = 1 THEN 'Asalariado'
        WHEN p61 = 2 THEN 'Empleador'
        WHEN p61 = 3 THEN 'Familiar sin paga'
        WHEN p61 = 4 THEN 'Cuenta propia'
        WHEN p61 = 5 THEN 'Otra'
        ELSE 'No declarado'
    END AS posicion_ocupacional,

    CASE
        WHEN (
            COALESCE(NULLIF(p40_1, 9), 1) IN (2,3,4) OR
            COALESCE(NULLIF(p40_2, 9), 1) IN (2,3,4) OR
            COALESCE(NULLIF(p40_3, 9), 1) IN (2,3,4) OR
            COALESCE(NULLIF(p40_4, 9), 1) IN (2,3,4) OR
            COALESCE(NULLIF(p40_5, 9), 1) IN (2,3,4) OR
            COALESCE(NULLIF(p40_6, 9), 1) IN (2,3,4)
        )
        THEN 1 ELSE 0
    END AS discapacidad,

    CASE
        WHEN p27_edad < 15 THEN 'Menor de 15'
        WHEN COALESCE(p53, 0) = 1 OR COALESCE(p54, 0) = 1 OR COALESCE(p55, 0) = 1 THEN 'Ocupado'
        WHEN COALESCE(p53, 0) <> 1
         AND COALESCE(p54, 0) <> 1
         AND COALESCE(p55, 0) <> 1
         AND p56 = 1 THEN 'Desocupado'
        ELSE 'Inactivo'
    END AS condicion_actividad

FROM public.personas_limpia;


/* =========================================================
   Consultas de articulo
   ========================================================= */

-- 1) Resumen nacional
SELECT
    SUM(pet) AS pet,
    SUM(fuerza_trabajo) AS fuerza_trabajo,
    SUM(ocupado) AS ocupados,
    SUM(desocupado) AS desocupados,
    SUM(inactivo) AS inactivos,
    ROUND(100.0 * SUM(fuerza_trabajo) / NULLIF(SUM(pet), 0), 2) AS tasa_participacion,
    ROUND(100.0 * SUM(ocupado) / NULLIF(SUM(pet), 0), 2) AS tasa_ocupacion,
    ROUND(100.0 * SUM(desocupado) / NULLIF(SUM(fuerza_trabajo), 0), 2) AS tasa_desocupacion
FROM public.vw_mercado_laboral_censo
WHERE pet = 1;

-- 2) Tasas por sexo
SELECT
    sexo,
    SUM(pet) AS pet,
    SUM(fuerza_trabajo) AS fuerza_trabajo,
    SUM(ocupado) AS ocupados,
    SUM(desocupado) AS desocupados,
    ROUND(100.0 * SUM(fuerza_trabajo) / NULLIF(SUM(pet), 0), 2) AS tasa_participacion,
    ROUND(100.0 * SUM(ocupado) / NULLIF(SUM(pet), 0), 2) AS tasa_ocupacion,
    ROUND(100.0 * SUM(desocupado) / NULLIF(SUM(fuerza_trabajo), 0), 2) AS tasa_desocupacion
FROM public.vw_mercado_laboral_censo
WHERE pet = 1
GROUP BY sexo
ORDER BY sexo;

-- 3) Cruce educacion superior, edad 25-34 y sexo
SELECT
    sexo,
    educ_superior,
    SUM(pet) AS pet,
    SUM(fuerza_trabajo) AS fuerza_trabajo,
    SUM(ocupado) AS ocupados,
    SUM(empleo_vulnerable) AS ocupados_vulnerables,
    ROUND(100.0 * SUM(fuerza_trabajo) / NULLIF(SUM(pet), 0), 2) AS tasa_participacion,
    ROUND(100.0 * SUM(ocupado) / NULLIF(SUM(pet), 0), 2) AS tasa_ocupacion,
    ROUND(100.0 * SUM(empleo_vulnerable) / NULLIF(SUM(ocupado), 0), 2) AS tasa_empleo_vulnerable
FROM public.vw_mercado_laboral_censo
WHERE p27_edad BETWEEN 25 AND 34
GROUP BY sexo, educ_superior
ORDER BY sexo, educ_superior DESC;

-- 4) Top ocupaciones entre personas con educacion superior
SELECT
    p60_code AS codigo_ocupacion,
    COUNT(*) AS ocupados_con_educ_superior
FROM public.vw_mercado_laboral_censo
WHERE ocupado = 1
  AND educ_superior = 1
  AND p60_code NOT IN (9998, 9999)
GROUP BY p60_code
ORDER BY ocupados_con_educ_superior DESC
LIMIT 20;

-- 5) Top ramas de actividad entre personas con educacion superior
SELECT
    p62_code AS codigo_actividad,
    COUNT(*) AS ocupados_con_educ_superior
FROM public.vw_mercado_laboral_censo
WHERE ocupado = 1
  AND educ_superior = 1
  AND p62_code NOT IN (9998, 9999)
GROUP BY p62_code
ORDER BY ocupados_con_educ_superior DESC
LIMIT 20;
