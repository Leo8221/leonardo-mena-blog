\set ON_ERROR_STOP on

DO $$
DECLARE
  personas_count bigint;
  vivienda_count bigint;
  mortalidad_count bigint;
  coverage_count bigint;
BEGIN
  SELECT COUNT(*) INTO personas_count FROM public.personas_limpia;
  SELECT COUNT(*) INTO vivienda_count FROM public.vivienda_hogar_limpia;
  SELECT COUNT(*) INTO mortalidad_count FROM public.mortalidad;
  SELECT COUNT(*) INTO coverage_count
  FROM public.personas_limpia
  WHERE p27_edad BETWEEN 25 AND 34
    AND p43 IN (4,5,6)
    AND (COALESCE(p53,0)=1 OR COALESCE(p54,0)=1 OR COALESCE(p55,0)=1)
    AND p60_code IS NOT NULL
    AND p60_code NOT IN (9998,9999);

  IF personas_count <> 10773983 THEN
    RAISE EXCEPTION 'Conteo personas inesperado: %', personas_count;
  END IF;
  IF vivienda_count <> 4455060 THEN
    RAISE EXCEPTION 'Conteo vivienda-hogar inesperado: %', vivienda_count;
  END IF;
  IF mortalidad_count <> 158243 THEN
    RAISE EXCEPTION 'Conteo mortalidad inesperado: %', mortalidad_count;
  END IF;
  IF coverage_count <> 269319 THEN
    RAISE EXCEPTION 'Cobertura ocupacional inesperada: %', coverage_count;
  END IF;
END $$;

SELECT
  current_database() AS database,
  current_setting('server_version') AS server_version,
  pg_size_pretty(pg_database_size(current_database())) AS database_size,
  (SELECT COUNT(*) FROM public.personas_limpia) AS personas,
  (SELECT COUNT(*) FROM public.vivienda_hogar_limpia) AS vivienda_hogar,
  (SELECT COUNT(*) FROM public.mortalidad) AS mortalidad;

