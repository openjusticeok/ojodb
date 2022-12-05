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

CREATE ROLE all_table_reader WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  -- NOREPLICATION
  -- NOBYPASSRLS
  CONNECTION LIMIT -1;

GRANT SELECT
  ON ALL TABLES IN SCHEMA public -- TODO: replace with SQL statement that returns all tables? https://www.postgresql.org/docs/9.1/sql-grant.html
  TO all_table_reader;

---------------------------------------------------
-- creating all_table_writer --
---------------------------------------------------

CREATE ROLE all_table_writer WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  -- NOREPLICATION
  -- NOBYPASSRLS
  CONNECTION LIMIT -1;

GRANT all_table_reader, INSERT, UPDATE
  ON ALL TABLES IN SCHEMA public -- TODO: replace with SQL statement that returns all tables
  TO all_table_writer;

---------------------------------------------------
-- creating all_table_admin --
---------------------------------------------------

-- DROP ROLE ojo-table-admin

CREATE ROLE all_table_admin WITH
  NOCREATEDB
  CREATEROLE
  NOLOGIN
  INHERIT
  -- NOREPLICATION
  -- NOBYPASSRLS
  CONNECTION LIMIT -1;

  GRANT all_table_writer, DELETE
    ON ALL TABLES IN SCHEMA public -- TODO: replace with SQL statement that returns all tables
    TO all_table_admin;

-------------------------------------------------
-- changing ojo-table-owner => ojo_table_owner --
-------------------------------------------------

CREATE ROLE ojo_table_owner WITH
  SUPERUSER
  CREATEDB
  CREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT
  all_table_admin, CREATE
  ON ALL TABLES IN SCHEMA public
  TO ojo_table_owner;


-------------------------------------------------
-- ojo_researcher --
-- Is this actually necessary if it's the same permissions as all_table_reader? Do we need write permissions?
-------------------------------------------------

CREATE ROLE ojo_researcher WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT
  all_table_reader,
  TO ojo_researcher;

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
  -- NOREPLICATION
  -- NOBYPASSRLS
  CONNECTION LIMIT -1;

GRANT SELECT
  ON TABLE ocdc
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
  -- NOREPLICATION
  -- NOBYPASSRLS
  CONNECTION LIMIT -1;

GRANT oscn_table_reader, INSERT, UPDATE
    ON TABLE ocdc 
    TO ocdc_table_writer;



----------------------------
-- product_cjac_dashboard --
----------------------------

CREATE ROLE product_cjac_dashboard WITH
  NOSUPERUSER
  NOCREATEDB
    NOCREATEROLE
  NOLOGIN
  INHERIT
  -- NOREPLICATION
  -- NOBYPASSRLS
  CONNECTION LIMIT -1;

GRANT ocdc_table_reader
    -- ON TABLE ocdc 
    TO product_cjac_dashboard;


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
  -- NOREPLICATION
  -- NOBYPASSRLS
  CONNECTION LIMIT -1;

GRANT SELECT
  ON TABLE odoc
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
  -- NOREPLICATION
  -- NOBYPASSRLS
  CONNECTION LIMIT -1;

GRANT ocdc_table_reader, INSERT, UPDATE
    ON TABLE ocdc 
    TO ocdc_table_writer;

----------------------------
-- scraper_odoc --
----------------------------

CREATE ROLE scraper_odoc WITH
  NOSUPERUSER
  NOCREATEDB
    NOCREATEROLE
  NOLOGIN
  INHERIT
  -- NOREPLICATION
  -- NOBYPASSRLS
  CONNECTION LIMIT -1;

GRANT ocdc_table_writer
    -- ON TABLE odoc
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
  -- NOREPLICATION
  -- NOBYPASSRLS
  CONNECTION LIMIT -1;

GRANT SELECT
  ON TABLE oscn
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
  -- NOREPLICATION
  -- NOBYPASSRLS
  CONNECTION LIMIT -1;

GRANT oscn_table_reader, INSERT, UPDATE
    ON TABLE ocdc 
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
  -- NOREPLICATION
  -- NOBYPASSRLS
  CONNECTION LIMIT -1;

GRANT oscn_table_writer
    -- ON TABLE oscn 
    TO scraper_oscn;
















------------------------------------------------
-- Altering default privileges for new tables --
------------------------------------------------
7
ALTER DEFAULT PRIVILEGES
  FOR ROLE ojo_table_owner
  IN SCHEMA public
  GRANT SELECT ON TABLES TO all_table_reader

