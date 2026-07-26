\set ON_ERROR_STOP on

-- Ejecutar dentro de censos_linea_tiempo con:
-- psql --set=fdw_password="..." -f create-central-fdw.sql
-- La clave se recibe como variable de psql y no se guarda en este archivo.

CREATE EXTENSION IF NOT EXISTS postgres_fdw;

DROP SCHEMA IF EXISTS armonizado CASCADE;
DROP SCHEMA IF EXISTS fdw_2002 CASCADE;
DROP SCHEMA IF EXISTS fdw_2010 CASCADE;
DROP SCHEMA IF EXISTS fdw_2022 CASCADE;

DROP SERVER IF EXISTS censo_2002_server CASCADE;
DROP SERVER IF EXISTS censo_2010_server CASCADE;
DROP SERVER IF EXISTS censo_2022_server CASCADE;

CREATE SERVER censo_2002_server FOREIGN DATA WRAPPER postgres_fdw
  OPTIONS (host '127.0.0.1', port '5433', dbname 'censo_2002');
CREATE SERVER censo_2010_server FOREIGN DATA WRAPPER postgres_fdw
  OPTIONS (host '127.0.0.1', port '5433', dbname 'censo_2010');
CREATE SERVER censo_2022_server FOREIGN DATA WRAPPER postgres_fdw
  OPTIONS (host '127.0.0.1', port '5433', dbname 'censo_2022');

CREATE USER MAPPING FOR CURRENT_USER SERVER censo_2002_server
  OPTIONS (user 'postgres', password :'fdw_password');
CREATE USER MAPPING FOR CURRENT_USER SERVER censo_2010_server
  OPTIONS (user 'postgres', password :'fdw_password');
CREATE USER MAPPING FOR CURRENT_USER SERVER censo_2022_server
  OPTIONS (user 'postgres', password :'fdw_password');

CREATE SCHEMA fdw_2002;
CREATE SCHEMA fdw_2010;
CREATE SCHEMA fdw_2022;

IMPORT FOREIGN SCHEMA armonizado
  LIMIT TO (personas, hogares, viviendas, metadatos_variables)
  FROM SERVER censo_2002_server INTO fdw_2002;
IMPORT FOREIGN SCHEMA armonizado
  LIMIT TO (personas, hogares, viviendas, metadatos_variables)
  FROM SERVER censo_2010_server INTO fdw_2010;
IMPORT FOREIGN SCHEMA armonizado
  LIMIT TO (personas, hogares, viviendas, metadatos_variables)
  FROM SERVER censo_2022_server INTO fdw_2022;

CREATE SCHEMA armonizado;

CREATE VIEW armonizado.personas AS
  SELECT * FROM fdw_2002.personas
  UNION ALL SELECT * FROM fdw_2010.personas
  UNION ALL SELECT * FROM fdw_2022.personas;

CREATE VIEW armonizado.hogares AS
  SELECT * FROM fdw_2002.hogares
  UNION ALL SELECT * FROM fdw_2010.hogares
  UNION ALL SELECT * FROM fdw_2022.hogares;

CREATE VIEW armonizado.viviendas AS
  SELECT * FROM fdw_2002.viviendas
  UNION ALL SELECT * FROM fdw_2010.viviendas
  UNION ALL SELECT * FROM fdw_2022.viviendas;

CREATE VIEW armonizado.metadatos_variables AS
  SELECT * FROM fdw_2002.metadatos_variables
  UNION ALL SELECT * FROM fdw_2010.metadatos_variables
  UNION ALL SELECT * FROM fdw_2022.metadatos_variables;

CREATE TABLE armonizado.validacion_conteos (
  tabla text NOT NULL,
  anio integer NOT NULL,
  filas bigint NOT NULL,
  validado_en timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (tabla, anio)
);

INSERT INTO armonizado.validacion_conteos (tabla, anio, filas)
VALUES
  ('personas', 2002, 8562541), ('hogares', 2002, 2194162), ('viviendas', 2002, 2446930),
  ('personas', 2010, 9445281), ('hogares', 2010, 2671979), ('viviendas', 2010, 2662800),
  ('personas', 2022, 10773983), ('hogares', 2022, 4455060), ('viviendas', 2022, 4455060);

COMMENT ON SCHEMA armonizado IS
  'Vistas federadas de los Censos ONE 2002, 2010 y 2022; no duplican los microdatos.';
