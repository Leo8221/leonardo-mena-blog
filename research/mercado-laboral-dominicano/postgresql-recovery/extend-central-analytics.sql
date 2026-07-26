\set ON_ERROR_STOP on

-- Ejecutar en censos_linea_tiempo con -v fdw_password="...".
-- Amplia la base central sin duplicar microdatos.

CREATE EXTENSION IF NOT EXISTS postgres_fdw;

DROP SCHEMA IF EXISTS fdw_enhogar_2024 CASCADE;
DROP SERVER IF EXISTS enhogar_2024_server CASCADE;

CREATE SERVER enhogar_2024_server FOREIGN DATA WRAPPER postgres_fdw
  OPTIONS (host '127.0.0.1', port '5433', dbname 'enhogar_2024');
CREATE USER MAPPING FOR CURRENT_USER SERVER enhogar_2024_server
  OPTIONS (user 'postgres', password :'fdw_password');

CREATE SCHEMA fdw_enhogar_2024;
IMPORT FOREIGN SCHEMA public
  LIMIT TO (personas_analiticas, hogares_analiticos, personas_hogar)
  FROM SERVER enhogar_2024_server INTO fdw_enhogar_2024;

DROP SCHEMA IF EXISTS fdw_one_datos CASCADE;
DROP SERVER IF EXISTS one_datos_server CASCADE;

CREATE SERVER one_datos_server FOREIGN DATA WRAPPER postgres_fdw
  OPTIONS (host '127.0.0.1', port '5433', dbname 'one_datos');
CREATE USER MAPPING FOR CURRENT_USER SERVER one_datos_server
  OPTIONS (user 'postgres', password :'fdw_password');

CREATE SCHEMA fdw_one_datos;
IMPORT FOREIGN SCHEMA public
  LIMIT TO (
    atmosfera_clima_1991_2025,
    atmosfera_clima_ca_2017_2023,
    atmosfera_clima_ca_old,
    eventos_fenomenos_naturales,
    gastos_gobiernos_locales_2022,
    gastos_gobiernos_locales_2023,
    gastos_gobiernos_locales_2024,
    ingresos_gobiernos_locales_2022,
    ingresos_gobiernos_locales_2023,
    ingresos_gobiernos_locales_2024
  )
  FROM SERVER one_datos_server INTO fdw_one_datos;

DROP SCHEMA IF EXISTS fdw_parejas_2002 CASCADE;
DROP SCHEMA IF EXISTS fdw_parejas_2010 CASCADE;
DROP SCHEMA IF EXISTS fdw_parejas_2022 CASCADE;
CREATE SCHEMA fdw_parejas_2010;
CREATE SCHEMA fdw_parejas_2022;

IMPORT FOREIGN SCHEMA analitica
  LIMIT TO (parejas_jefatura_historica)
  FROM SERVER censo_2010_server INTO fdw_parejas_2010;
IMPORT FOREIGN SCHEMA analitica
  LIMIT TO (parejas_jefatura_historica)
  FROM SERVER censo_2022_server INTO fdw_parejas_2022;

CREATE SCHEMA IF NOT EXISTS analitica;
CREATE OR REPLACE VIEW analitica.parejas_historicas AS
  SELECT * FROM fdw_parejas_2010.parejas_jefatura_historica
  UNION ALL
  SELECT * FROM fdw_parejas_2022.parejas_jefatura_historica;

CREATE SCHEMA IF NOT EXISTS catalogo;

CREATE TABLE IF NOT EXISTS catalogo.fuentes (
  fuente_id text PRIMARY KEY,
  titulo text NOT NULL,
  organismo text NOT NULL,
  periodo text NOT NULL,
  base_datos text NOT NULL,
  tabla_canonica text NOT NULL,
  unidad_analisis text NOT NULL,
  es_microdato boolean NOT NULL,
  url text,
  fecha_corte date,
  notas text,
  actualizado_en timestamptz NOT NULL DEFAULT now()
);

INSERT INTO catalogo.fuentes
  (fuente_id, titulo, organismo, periodo, base_datos, tabla_canonica,
   unidad_analisis, es_microdato, url, fecha_corte, notas)
VALUES
  ('cnpv_2002_personas', 'VIII Censo Nacional de Poblacion y Vivienda', 'ONE', '2002',
   'censo_2002', 'armonizado.personas', 'persona', true,
   'https://www.one.gob.do/datos-y-estadisticas/', DATE '2002-12-31',
   'Usar para comparaciones individuales o agregadas. El CSV publico cargado no contiene llaves para enlazar personas dentro del hogar.'),
  ('cnpv_2010_personas', 'IX Censo Nacional de Poblacion y Vivienda', 'ONE', '2010',
   'censo_2010', 'armonizado.personas', 'persona', true,
   'https://www.one.gob.do/datos-y-estadisticas/', DATE '2010-12-31',
   'La zona no esta incluida en el CSV publico de personas usado por la reconstruccion.'),
  ('xcnpv_2022_unificada', 'X Censo Nacional de Poblacion y Vivienda: base unificada', 'ONE', '2022',
   'censo_2022', 'analitica.xcnpv_unificada', 'persona dentro de hogar', true,
   'https://www.one.gob.do/datos-y-estadisticas/', DATE '2022-11-30',
   'Fuente canonica para relaciones intrahogar; hogar_id es una secuencia reproducible derivada del orden oficial.'),
  ('enhogar_2024_personas', 'Encuesta Nacional de Hogares de Propositos Multiples', 'ONE', '2024',
   'enhogar_2024', 'public.personas_analiticas', 'persona encuestada', true,
   'https://www.one.gob.do/datos-y-estadisticas/', DATE '2024-12-31',
   'Usar factores de expansion para estimaciones poblacionales; no tratar conteos muestrales como poblacion.'),
  ('one_atmosfera_clima_1991_2025', 'Atmosfera y clima: serie 1991-2025', 'ONE', '1991-2025',
   'one_datos', 'public.atmosfera_clima_1991_2025', 'observacion climatologica mensual por provincia', false,
   'https://www.one.gob.do/media/0aybenwv/base-de-datos-atm%C3%B3sfera-y-clima-1991-2025.xlsx', DATE '2025-12-31',
   'La tabla conserva el periodo y la provincia reportados por la fuente; revisar unidad_de_medida antes de comparar valores entre indicadores.'),
  ('one_atmosfera_clima_ca_2017_2023', 'Atmosfera y clima: estaciones CA 2017-2023', 'ONE', '2017-2023',
   'one_datos', 'public.atmosfera_clima_ca_2017_2023', 'observacion climatologica por estacion y año', false,
   'https://www.one.gob.do/media/0aybenwv/base-de-datos-atmosfera-y-clima-1991-2025.xlsx', DATE '2023-12-31',
   'Base de estaciones CA; no debe unirse directamente con la serie provincial sin una correspondencia geografica documentada.'),
  ('one_atmosfera_clima_ca_old', 'Atmosfera y clima: estaciones CA historicas', 'ONE', 'historico',
   'one_datos', 'public.atmosfera_clima_ca_old', 'observacion climatologica por estacion', false,
   'https://www.one.gob.do/media/0aybenwv/base-de-datos-atmosfera-y-clima-1991-2025.xlsx', NULL,
   'Serie historica de estaciones CA; el periodo exacto debe filtrarse con los campos de la tabla.'),
  ('one_eventos_fenomenos_naturales', 'Eventos y fenomenos naturales', 'ONE', 'historico',
   'one_datos', 'public.eventos_fenomenos_naturales', 'evento o fenomeno natural registrado', false,
   'https://www.one.gob.do/media/wsfa2mqb/base-de-datos-de-eventos-y-fenomenos-naturales.xlsx', NULL,
   'La fecha y la cobertura territorial dependen del registro de cada evento; no interpretar ausencia como ausencia del fenomeno.'),
  ('one_gastos_gobiernos_locales_2022', 'Gastos de los gobiernos locales', 'ONE', '2022',
   'one_datos', 'public.gastos_gobiernos_locales_2022', 'registro presupuestario municipal', false,
   'https://www.one.gob.do/media/3jwlzsfg/base-de-datos-de-gastos-de-los-gobienos-locales-2022.xlsx', DATE '2022-12-31',
   'Fuente anual; las filas son registros presupuestarios y no municipios unicos.'),
  ('one_gastos_gobiernos_locales_2023', 'Gastos de los gobiernos locales', 'ONE', '2023',
   'one_datos', 'public.gastos_gobiernos_locales_2023', 'registro presupuestario municipal', false,
   'https://www.one.gob.do/media/of4a3wbc/base-de-datos-de-los-gobiernos-locales-2023.xlsx', DATE '2023-12-31',
   'Fuente anual; las filas son registros presupuestarios y no municipios unicos.'),
  ('one_gastos_gobiernos_locales_2024', 'Gastos de los gobiernos locales', 'ONE', '2024',
   'one_datos', 'public.gastos_gobiernos_locales_2024', 'registro presupuestario municipal', false,
   'https://www.one.gob.do/media/2iiomesv/base-de-datos-de-gastos-de-los-gobiernos-locales-2024.xlsx', DATE '2024-12-31',
   'Fuente anual; las filas son registros presupuestarios y no municipios unicos.'),
  ('one_ingresos_gobiernos_locales_2022', 'Ingresos de los gobiernos locales', 'ONE', '2022',
   'one_datos', 'public.ingresos_gobiernos_locales_2022', 'registro presupuestario municipal', false,
   'https://www.one.gob.do/media/2fphze2s/base-de-datos-de-ingresos-de-los-gobiernos-locales-2022.xlsx', DATE '2022-12-31',
   'Fuente anual; la cuenta se armoniza como cuenta en la capa tipada.'),
  ('one_ingresos_gobiernos_locales_2023', 'Ingresos de los gobiernos locales', 'ONE', '2023',
   'one_datos', 'public.ingresos_gobiernos_locales_2023', 'registro presupuestario municipal', false,
   'https://www.one.gob.do/media/ghypspwn/base-de-datos-ingresos-de-los-gobiernos-locales-2023.xlsx', DATE '2023-12-31',
   'Fuente anual; DES_CUENTA se armoniza como cuenta en la capa tipada.'),
  ('one_ingresos_gobiernos_locales_2024', 'Ingresos de los gobiernos locales', 'ONE', '2024',
   'one_datos', 'public.ingresos_gobiernos_locales_2024', 'registro presupuestario municipal', false,
   'https://www.one.gob.do/media/h0wf25ut/base-de-datos-ingresos-de-los-gobiernos-locales-2024.xlsx', DATE '2024-12-31',
   'Fuente anual; DES_CUENTA se armoniza como cuenta en la capa tipada.')
ON CONFLICT (fuente_id) DO UPDATE SET
  titulo = EXCLUDED.titulo,
  organismo = EXCLUDED.organismo,
  periodo = EXCLUDED.periodo,
  base_datos = EXCLUDED.base_datos,
  tabla_canonica = EXCLUDED.tabla_canonica,
  unidad_analisis = EXCLUDED.unidad_analisis,
  es_microdato = EXCLUDED.es_microdato,
  url = EXCLUDED.url,
  fecha_corte = EXCLUDED.fecha_corte,
  notas = EXCLUDED.notas,
  actualizado_en = now();

CREATE TABLE IF NOT EXISTS catalogo.metricas (
  metrica_id text PRIMARY KEY,
  nombre text NOT NULL,
  definicion text NOT NULL,
  numerador text,
  denominador text,
  fuente_id text NOT NULL REFERENCES catalogo.fuentes(fuente_id),
  tabla_canonica text NOT NULL,
  filtros_obligatorios text,
  advertencias text,
  actualizado_en timestamptz NOT NULL DEFAULT now()
);

INSERT INTO catalogo.metricas
  (metrica_id, nombre, definicion, numerador, denominador, fuente_id,
   tabla_canonica, filtros_obligatorios, advertencias)
VALUES
  ('parejas_jefatura_2022', 'Parejas convivientes identificables',
   'Hogares con exactamente una jefatura (P28=1) y una esposa, esposo, companera o companero (P28=2).',
   'Filas de analitica.parejas_jefatura_2022', NULL, 'xcnpv_2022_unificada',
   'censo_2022.analitica.parejas_jefatura_2022',
   'Usar edades_plausibles cuando el analisis requiera relaciones demograficamente consistentes.',
   'No incluye parejas no convivientes ni permite enlazar de forma completa parejas secundarias del hogar.'),
  ('afinidad_campo_estudio_2022', 'Coincidencia de campo de estudio en la pareja',
   'Proporcion de parejas con el mismo campo amplio ISCED-F 2013 entre parejas donde ambos campos son observables.',
   'Parejas con mismo_campo_estudio_amplio=true',
   'Parejas con mismo_campo_estudio_amplio no nulo', 'xcnpv_2022_unificada',
   'censo_2022.analitica.parejas_jefatura_2022',
   'Excluir codigos 9998 y 9999 y declarar la cobertura.',
   'Campo de estudio no equivale a ocupacion actual.'),
  ('afinidad_ocupacional_2022', 'Coincidencia ocupacional en la pareja',
   'Proporcion de parejas en el mismo gran grupo CNO entre parejas donde ambas ocupaciones son observables.',
   'Parejas con mismo_gran_grupo_ocupacional=true',
   'Parejas con mismo_gran_grupo_ocupacional no nulo', 'xcnpv_2022_unificada',
   'censo_2022.analitica.parejas_jefatura_2022',
   'Excluir codigos 9998 y 9999; distinguir ocupacion actual o ultima ocupacion.',
   'El Censo registra la ocupacion actual o la ultima ocupacion, segun trayectoria laboral.')
ON CONFLICT (metrica_id) DO UPDATE SET
  nombre = EXCLUDED.nombre,
  definicion = EXCLUDED.definicion,
  numerador = EXCLUDED.numerador,
  denominador = EXCLUDED.denominador,
  fuente_id = EXCLUDED.fuente_id,
  tabla_canonica = EXCLUDED.tabla_canonica,
  filtros_obligatorios = EXCLUDED.filtros_obligatorios,
  advertencias = EXCLUDED.advertencias,
  actualizado_en = now();

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'analitica_lectura') THEN
    CREATE ROLE analitica_lectura NOLOGIN;
  END IF;
END $$;

GRANT CONNECT ON DATABASE censos_linea_tiempo TO analitica_lectura;
GRANT USAGE ON SCHEMA armonizado, analitica, blog, catalogo, fdw_2002, fdw_2010, fdw_2022, fdw_enhogar_2024, fdw_one_datos, fdw_parejas_2010, fdw_parejas_2022
  TO analitica_lectura;
GRANT SELECT ON ALL TABLES IN SCHEMA armonizado, analitica, blog, catalogo, fdw_2002, fdw_2010, fdw_2022, fdw_enhogar_2024, fdw_one_datos, fdw_parejas_2010, fdw_parejas_2022
  TO analitica_lectura;
ALTER DEFAULT PRIVILEGES IN SCHEMA catalogo GRANT SELECT ON TABLES TO analitica_lectura;

COMMENT ON SCHEMA catalogo IS
  'Fuentes, unidades de analisis, metricas canonicas y advertencias para trabajo reproducible.';
