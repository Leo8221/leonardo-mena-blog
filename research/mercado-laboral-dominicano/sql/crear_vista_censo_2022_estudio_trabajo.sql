CREATE OR REPLACE VIEW public.vw_estudio_trabajo_censo AS
SELECT
    p.phogar,
    p.p25_orden,
    p.region,
    p.provincia,
    p.municipio,
    p.zona,
    p.p26_sexo,
    p.p27_edad,
    p.p42,
    p.p43,
    p.p45_code,
    d.isced_f_2013_code AS campo_estudio_isced_f_2013_code,
    d.campo_amplio AS campo_estudio_amplio,
    d.campo_especifico AS campo_estudio_especifico,
    d.campo_detallado AS campo_estudio_detallado,
    d.estado AS campo_estudio_estado,
    p.p46,
    p.p53,
    p.p54,
    p.p55,
    p.p56,
    p.p57,
    p.p58,
    p.p59,
    p.p60_code,
    p.ocupacion_desc,
    p.ocupacion_nivel,
    p.p61,
    p.p62_code,
    p.actividad_desc,
    p.actividad_nivel,
    CASE
        WHEN p.p26_sexo = 1 THEN 'Hombre'
        WHEN p.p26_sexo = 2 THEN 'Mujer'
        ELSE 'No declarado'
    END AS sexo,
    CASE
        WHEN p.p27_edad BETWEEN 15 AND 24 THEN '15-24'
        WHEN p.p27_edad BETWEEN 25 AND 34 THEN '25-34'
        WHEN p.p27_edad BETWEEN 35 AND 44 THEN '35-44'
        WHEN p.p27_edad BETWEEN 45 AND 54 THEN '45-54'
        WHEN p.p27_edad BETWEEN 55 AND 64 THEN '55-64'
        WHEN p.p27_edad >= 65 THEN '65+'
        ELSE 'Menor de 15'
    END AS grupo_edad,
    CASE
        WHEN p.p43 IN (4,5,6) THEN 1
        ELSE 0
    END AS educ_superior,
    CASE
        WHEN p.p43 IN (4,5,6) AND p.p46 = 1 THEN 1
        ELSE 0
    END AS graduado_superior,
    CASE
        WHEN p.p27_edad >= 15
         AND (COALESCE(p.p53,0) = 1 OR COALESCE(p.p54,0) = 1 OR COALESCE(p.p55,0) = 1)
        THEN 1
        ELSE 0
    END AS ocupado,
    CASE
        WHEN LEFT(p.p60_code::text, 1) IN ('1','2','3') THEN 'Alta calificacion'
        WHEN LEFT(p.p60_code::text, 1) IN ('4','5','6','7','8') THEN 'Media calificacion'
        WHEN LEFT(p.p60_code::text, 1) = '9' THEN 'Ocupaciones elementales'
        ELSE 'No declarado'
    END AS grupo_calificacion_ocupacion
FROM public.personas_limpia p
LEFT JOIN diccionarios.p45_campo_estudio d
    ON d.p45_code = p.p45_code;
