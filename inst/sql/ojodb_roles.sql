-- PLAN:
--
-- Step 1: Create all the new roles / users, configure their permissions
-- Step 2: Transfer ownership of all tables to ojo_table_owner
-- Step 3: Delete all old / unused roles / users
-- 		   Try to leave stuff alone as much as possible so we don't
--         have to go back and reconfigure a ton of stuff
-- Step 4: Done!

--------------------------------------------------------------------------------
-- DOMAIN: ALL TABLES ----------------------------------------------------------
--------------------------------------------------------------------------------

---------------------------------------------------
-- changing ojo-table-reader => all_table_reader --
---------------------------------------------------

-- DROP ROLE ojo-table-reader;

-- Step 1: Create role

CREATE ROLE all_table_reader WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  -- NOREPLICATION
  -- NOBYPASSRLS
  CONNECTION LIMIT -1;

-- Step 2: Create list of GRANT statements to run

SELECT DISTINCT 'GRANT SELECT ON ALL TABLES IN SCHEMA ' || table_schema || ' TO all_table_reader;'
  FROM information_schema.tables 
  WHERE table_schema 
  IN (SELECT schema_name 
	  FROM information_schema.schemata 
	  WHERE schema_name NOT IN ('information_schema', 'pg_catalog'));

-- this returns a list of GRANT statements, which you can run to grant read access to all tables in all schemas 

-- e.g.
-- GRANT SELECT ON ALL TABLES IN SCHEMA {...} TO all_table_reader;

-- Step 3: Run all the GRANT statements

-- Step 4: Alter default privileges

SELECT DISTINCT 'ALTER DEFAULT PRIVILEGES IN SCHEMA ' || table_schema || ' GRANT SELECT ON TABLES TO all_table_reader;'
  FROM information_schema.tables 
  WHERE table_schema 
  IN (SELECT schema_name 
	  FROM information_schema.schemata 
	  WHERE schema_name NOT IN ('information_schema', 'pg_catalog'));

-- e.g.
-- ALTER DEFAULT PRIVILEGES
--   IN SCHEMA {...}
--   GRANT SELECT ON TABLES TO all_table_reader

---------------------------------------------------
-- creating all_table_writer --
---------------------------------------------------

-- Step 1: Create role and add to all_table_reader 

CREATE ROLE all_table_writer WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;
  
  GRANT all_table_reader
  TO all_table_writer;

-- Step 2: Create list of GRANT statements to run

SELECT DISTINCT 'GRANT INSERT UPDATE ON ALL TABLES IN SCHEMA ' || table_schema || ' TO all_table_writer;'
  FROM information_schema.tables 
  WHERE table_schema 
  IN (SELECT schema_name 
	  FROM information_schema.schemata 
	  WHERE schema_name NOT IN ('information_schema', 'pg_catalog'));

-- this returns a list of GRANT statements, which you can run to grant read access to all tables in all schemas 

-- e.g.
-- GRANT INSERT UPDATE ON ALL TABLES IN SCHEMA {...} TO all_table_writer;

-- Step 3: Run all the GRANT statements

-- Step 4: Alter default privileges

SELECT DISTINCT 'ALTER DEFAULT PRIVILEGES IN SCHEMA ' || table_schema || ' GRANT INSERT, UPDATE ON TABLES TO all_table_writer;'
  FROM information_schema.tables 
  WHERE table_schema 
  IN (SELECT schema_name 
	  FROM information_schema.schemata 
	  WHERE schema_name NOT IN ('information_schema', 'pg_catalog'));

-- e.g.
-- ALTER DEFAULT PRIVILEGES
--   IN SCHEMA {...}
--   GRANT INSERT, UPDATE ON TABLES TO all_table_writer

---------------------------------------------------
-- creating all_table_admin --
---------------------------------------------------

-- DROP ROLE ojo-table-admin

-- Step 1: Create role and add to all_table_writer

CREATE ROLE all_table_admin WITH
  NOCREATEDB
  CREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

-- This actually isn't necessary; could just have all_table_admin not inherit anything
-- GRANT all_table_writer
--    TO all_table_admin;

-- Step 2: Create list of GRANT statements to run

SELECT DISTINCT 'GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA ' || table_schema || ' TO all_table_admin;'
  FROM information_schema.tables 
  WHERE table_schema 
  IN (SELECT schema_name 
	  FROM information_schema.schemata 
	  WHERE schema_name NOT IN ('information_schema', 'pg_catalog'));

-- this returns a list of GRANT statements, which you can run to grant all privileges for all tables in all schemas 

-- e.g.
-- GRANT ALL ON ALL TABLES IN SCHEMA {...} TO all_table_admin;

-- Step 3: Run all the GRANT statements

-- Step 4: Alter default privileges

SELECT DISTINCT 'ALTER DEFAULT PRIVILEGES IN SCHEMA ' || table_schema || ' GRANT ALL ON TABLES TO all_table_admin;'
  FROM information_schema.tables 
  WHERE table_schema 
  IN (SELECT schema_name 
	  FROM information_schema.schemata 
	  WHERE schema_name NOT IN ('information_schema', 'pg_catalog'));

-- e.g.
-- ALTER DEFAULT PRIVILEGES
--   IN SCHEMA {...}
--   GRANT ALL ON TABLES TO all_table_admin

-------------------------------------------------
-- changing ojo-table-owner => ojo_table_owner --
-------------------------------------------------

CREATE ROLE ojo_table_owner WITH
  SUPERUSER -- different from all_table_admin
  CREATEDB
  CREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT all_table_admin
  TO ojo_table_owner;

-------------------------------------------------
-- ojo_analyst --
-------------------------------------------------

CREATE ROLE ojo_analyst WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT all_table_reader
  TO ojo_analyst;

--------------------------------------------------------------------------------
-- DOMAIN: Oklahoma county / OCDC Tables --
--------------------------------------------------------------------------------

-----------------------
-- ocdc_table_reader --
-----------------------

CREATE ROLE ocdc_table_reader WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT SELECT
  ON ALL TABLES IN SCHEMA ocdc 
  TO ocdc_table_reader;

-----------------------
-- ocdc_table_writer --
-----------------------

CREATE ROLE ocdc_table_writer WITH
  NOSUPERUSER
  NOCREATEDB
    NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT oscn_table_reader, INSERT, UPDATE
  ON ALL TABLES IN SCHEMA ocdc 
  TO ocdc_table_writer;

----------------------------
-- product_cjac_dashboard --
----------------------------

-- CREATE ROLE product_cjac_dashboard WITH
--   NOSUPERUSER
--   NOCREATEDB
--   NOCREATEROLE
--   NOLOGIN
--   INHERIT
--   CONNECTION LIMIT -1;
--
-- GRANT ocdc_table_reader
--   TO product_cjac_dashboard;
    
----------------------------
-- product_cjac_api --
----------------------------

CREATE ROLE product_cjac_api WITH
  NOSUPERUSER
  NOCREATEDB
    NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT ocdc_table_writer
    TO product_cjac_api;


--------------------------------------------------------------------------------
-- DOMAIN: ODOC Tables --
--------------------------------------------------------------------------------

-----------------------
-- odoc_table_reader --
-----------------------

CREATE ROLE odoc_table_reader WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT SELECT
  ON ALL TABLES IN SCHEMA odoc
  TO odoc_table_reader;

-----------------------
-- odoc_table_reader --
-----------------------

CREATE ROLE odoc_table_writer WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT odoc_table_reader, INSERT, UPDATE
    ON ALL TABLES IN SCHEMA odoc 
    TO odoc_table_writer;

----------------------------
-- scraper_odoc --
----------------------------

CREATE ROLE scraper_odoc WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT odoc_table_writer
    TO scraper_odoc;


--------------------------------------------------------------------------------
-- DOMAIN: OSCN Tables --
--------------------------------------------------------------------------------

-----------------------
-- oscn_table_reader --
-----------------------

CREATE ROLE oscn_table_reader WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT SELECT
  ON ALL TABLES IN SCHEMA public -- TODO: I think public just has all the oscn stuff? 
  TO oscn_table_reader;

-----------------------
-- oscn_table_writer --
-----------------------

CREATE ROLE oscn_table_writer WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT oscn_table_reader, INSERT, UPDATE
  ON ALL TABLES IN SCHEMA public -- TODO: I think public just has all the oscn stuff? 
    TO oscn_table_writer;

----------------------------
-- scraper_oscn --
----------------------------

CREATE ROLE scraper_oscn WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT oscn_table_writer
    TO scraper_oscn;

--------------------------------------------------------------------------------
-- DOMAIN: Eviction Tables --
--------------------------------------------------------------------------------

---------------------------
-- eviction_table_reader --
---------------------------

CREATE ROLE eviction_table_reader WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT SELECT
  ON ALL TABLES IN SCHEMA eviction_addresses, eviction_dashboard 
  TO eviction_table_reader;
  
---------------------------
-- eviction_table_writer --
---------------------------

CREATE ROLE eviction_table_writer WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT eviction_table_reader, INSERT, UPDATE
  ON ALL TABLES IN SCHEMA eviction_addresses, eviction_dashboard 
  TO eviction_table_writer;

------------------------------------------
-- product_eviction_addresses_dashboard --
------------------------------------------

CREATE ROLE product_eviction_addresses_dashboard WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;
  
GRANT eviction_table_writer
TO product_eviction_dashboard

--------------------------------
-- integration_wjt_eviction --
--------------------------------

CREATE ROLE integration_wjt_eviction WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;
  
GRANT eviction_table_reader
TO integration_wjt_eviction

--------------------------------
-- integration_tps_eviction --
--------------------------------

CREATE ROLE integration_tps_eviction WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;
  
GRANT eviction_table_reader
TO integration_tps_eviction


---------------------------------------
-- integration_restore_hope_eviction --
---------------------------------------

CREATE ROLE integration_restore_hope_eviction WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;
  
GRANT eviction_table_reader
TO integration_restore_hope_eviction


--------------------------------------------------------------------------------
-- DOMAIN: Special roles --
--------------------------------------------------------------------------------

-- Need Asemio role



------------------------------------------------------
-- Altering default privileges for new tables --
------------------------------------------------------

-- ALTER DEFAULT PRIVILEGES
--   IN SCHEMA {...}
--   GRANT SELECT ON TABLES TO all_table_reader

