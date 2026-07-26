\set ON_ERROR_STOP on
DROP TABLE IF EXISTS diccionarios.cno_2019 CASCADE;
CREATE TABLE diccionarios.cno_2019 (code text, nivel_n integer, nivel text, "desc" text);
\copy diccionarios.cno_2019 FROM 'D:/datos_one_censos/2022/cno_2019_diccionario_completo.csv' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8');

DROP TABLE IF EXISTS diccionarios.cnae_2019 CASCADE;
CREATE TABLE diccionarios.cnae_2019 (code text, nivel_n integer, nivel text, parent_code text, "desc" text);
\copy diccionarios.cnae_2019 FROM 'D:/datos_one_censos/2022/cnae_2019_diccionario_completo.csv' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8');

DROP TABLE IF EXISTS public.catalogo_territorial CASCADE;
CREATE TABLE public.catalogo_territorial (
  provincia_code integer,
  municipio_code integer,
  nombre_provincia text,
  nombre_municipio text
);
\copy public.catalogo_territorial FROM 'D:/datos_one_censos/2022/catalogo_municipios_2022.csv' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8');