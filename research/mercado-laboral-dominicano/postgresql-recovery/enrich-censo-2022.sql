\set ON_ERROR_STOP on

DROP TABLE IF EXISTS public.personas_limpia CASCADE;
CREATE TABLE public.personas_limpia AS
SELECT
  p.*,
  cno."desc" AS ocupacion_desc,
  cno.nivel AS ocupacion_nivel,
  cnae."desc" AS actividad_desc,
  cnae.nivel AS actividad_nivel,
  CASE
    WHEN p.p62_code IS NULL THEN NULL
    ELSE LPAD(p.p62_code::text, 4, '0')
  END AS actividad_code_match
FROM public.personas p
LEFT JOIN diccionarios.cno_2019 cno
  ON cno.code = p.p60_code::text
LEFT JOIN diccionarios.cnae_2019 cnae
  ON cnae.nivel_n = 4
 AND cnae.code = LPAD(p.p62_code::text, 4, '0');

DROP TABLE IF EXISTS public.vivienda_hogar_limpia CASCADE;
CREATE TABLE public.vivienda_hogar_limpia AS
SELECT * FROM public.vivienda_hogar;

CREATE INDEX personas_limpia_edad_sexo_idx
  ON public.personas_limpia (p27_edad, p26_sexo);
CREATE INDEX personas_limpia_educacion_idx
  ON public.personas_limpia (p43, p45_code);
CREATE INDEX personas_limpia_ocupacion_idx
  ON public.personas_limpia (p60_code, p62_code);
CREATE INDEX personas_limpia_hogar_idx
  ON public.personas_limpia (phogar);
CREATE INDEX vivienda_hogar_limpia_hogar_idx
  ON public.vivienda_hogar_limpia (phogar);
CREATE INDEX vivienda_hogar_limpia_territorio_idx
  ON public.vivienda_hogar_limpia (provincia, municipio, dmunicipal);

ANALYZE public.personas_limpia;
ANALYZE public.vivienda_hogar_limpia;

CREATE OR REPLACE VIEW public.vw_personas_humana AS
SELECT
  p.*,
  CASE p.p26_sexo WHEN 1 THEN 'Hombre' WHEN 2 THEN 'Mujer' ELSE 'No declarado' END AS sexo,
  CASE p.zona WHEN 1 THEN 'Urbana' WHEN 2 THEN 'Rural' ELSE 'No declarada' END AS zona_desc
FROM public.personas_limpia p;

CREATE OR REPLACE VIEW public.vw_vivienda_hogar_humana AS
SELECT
  v.*,
  CASE v.zona WHEN 1 THEN 'Urbana' WHEN 2 THEN 'Rural' ELSE 'No declarada' END AS zona_desc
FROM public.vivienda_hogar_limpia v;
