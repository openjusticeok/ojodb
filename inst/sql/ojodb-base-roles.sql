-- PLAN:
--
-- Step 1: Create all the new roles / users, configure their permissions
-- Step 2: Transfer ownership of all tables to ojo_table_owner
-- Step 2.5: Views, material views, and usage
-- Step 3: Delete all old / unused roles / users
-- 		   Try to leave stuff alone as much as possible so we don't
--         have to go back and reconfigure a ton of stuff
-- Step 4: Done!

--------------------------------------------------------------------------------
-- new analyst roles
--------------------------------------------------------------------------------
CREATE ROLE prozhkova WITH 
	NOSUPERUSER
	NOCREATEDB
	NOCREATEROLE
	INHERIT
	LOGIN
	NOREPLICATION
	NOBYPASSRLS
	CONNECTION LIMIT -1;

grant ojo_analyst to prozhkova;

--

CREATE ROLE aflores WITH 
	NOSUPERUSER
	NOCREATEDB
	NOCREATEROLE
	INHERIT
	LOGIN
	NOREPLICATION
	NOBYPASSRLS
	CONNECTION LIMIT -1;

grant ojo_analyst to aflores;

--------------------------------------------------------------------------------
-- DOMAIN: ALL TABLES ----------------------------------------------------------
--------------------------------------------------------------------------------

---------------------------------------------------
-- changing ojo-table-reader => all_table_reader --
---------------------------------------------------

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
  
 SELECT DISTINCT 'GRANT SELECT ON ALL TABLES IN SCHEMA ' || table_schema || ' TO all_table_reader;'
  FROM information_schema.tables 
  WHERE table_schema 
  IN (SELECT schema_name 
	  FROM information_schema.schemata 
	  WHERE schema_name NOT IN ('information_schema', 'pg_catalog'));
	 
grant usage on schema eviction_addresses to all_table_reader;
grant usage on schema iic TO all_table_reader;
grant usage on schema ocdc TO all_table_reader;
grant usage on schema archive TO all_table_reader;
grant usage on schema public TO all_table_reader;
grant usage on schema odoc TO all_table_reader;
	  
GRANT SELECT ON ALL TABLES IN SCHEMA eviction_addresses TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA iic TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA ocdc TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA archive TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA odoc TO all_table_reader;


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
	 
GRANT INSERT UPDATE ON ALL TABLES IN SCHEMA archive TO all_table_writer;
GRANT INSERT UPDATE ON ALL TABLES IN SCHEMA ocdc TO all_table_writer;
GRANT INSERT UPDATE ON ALL TABLES IN SCHEMA iic TO all_table_writer;
GRANT INSERT UPDATE ON ALL TABLES IN SCHEMA odoc TO all_table_writer;
GRANT INSERT UPDATE ON ALL TABLES IN SCHEMA eviction_addresses TO all_table_writer;
GRANT INSERT UPDATE ON ALL TABLES IN SCHEMA public TO all_table_writer;
 
grant create on schema eviction_addresses to all_table_reader;
grant create on schema iic TO all_table_reader;
grant create on schema ocdc TO all_table_reader;
grant create on schema archive TO all_table_reader;
grant create on schema public TO all_table_reader;
grant create on schema odoc TO all_table_reader;
 
 
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
 
grant all_table_writer to all_table_admin
 
 -------------------------------------------------
-- changing ojo-table-owner => ojo_table_owner --
-------------------------------------------------

CREATE ROLE ojo_table_owner WITH
  -- SUPERUSER -- different from all_table_admin
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
-- domain_ocdc_reader --
-----------------------

CREATE ROLE domain_ocdc_reader WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT SELECT
  ON ALL TABLES IN SCHEMA ocdc 
  TO domain_ocdc_reader;
 
-- Is this needed?
-- grant usage on schema ocdc TO domain_ocdc_reader; 

-----------------------
-- domain_ocdc_writer --
-----------------------

CREATE ROLE domain_ocdc_writer WITH
  NOSUPERUSER
  NOCREATEDB
    NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

 grant domain_ocdc_reader to domain_ocdc_writer

 GRANT INSERT, UPDATE
  ON ALL TABLES IN SCHEMA ocdc 
  TO domain_ocdc_writer;


--------------------------------------------------------------------------------
-- DOMAIN: ODOC Tables --
--------------------------------------------------------------------------------

-----------------------
-- domain_odoc_reader --
-----------------------

CREATE ROLE domain_odoc_reader WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT SELECT
  ON ALL TABLES IN SCHEMA odoc
  TO domain_odoc_reader;

-----------------------
-- odoc_table_reader --
-----------------------

CREATE ROLE domain_odoc_writer WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT domain_odoc_reader to domain_odoc_writer;

grant INSERT, UPDATE
    ON ALL TABLES IN SCHEMA odoc 
    TO domain_odoc_writer;


--------------------------------------------------------------------------------
-- DOMAIN: OSCN Tables --
--------------------------------------------------------------------------------

-----------------------
-- domain_oscn_reader --
-----------------------

CREATE ROLE domain_oscn_reader WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT select
  ON ALL TABLES IN SCHEMA public 
  TO domain_oscn_reader;
 
 revoke select
 on table public.exception_log, public.process_log 
 from domain_oscn_reader;

-----------------------
-- domain_oscn_writer --
-----------------------

CREATE ROLE domain_oscn_writer WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT domain_oscn_reader to domain_oscn_writer;

grant INSERT, UPDATE
  ON ALL TABLES IN SCHEMA public 
    TO domain_oscn_writer;
   
grant select
on table public.exception_log, public.process_log 
to domain_oscn_writer;

--------------------------------------------------------------------------------
-- DOMAIN: Eviction Tables --
--------------------------------------------------------------------------------

-----------------------
-- domain_eviction_addresses_reader --
-----------------------

CREATE ROLE domain_eviction_addresses_reader WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT select
  ON  eviction_addresses.address
  TO domain_eviction_addresses_reader;

-----------------------
-- domain_oscn_writer --
-----------------------

CREATE ROLE domain_eviction_addresses_writer WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;

GRANT domain_eviction_addresses_reader to domain_eviction_addresses_writer;

grant INSERT, UPDATE
  ON ALL TABLES IN SCHEMA eviction_addresses 
    TO domain_eviction_addresses_writer;
 
 
 --------------------------------------------------------------------------------
-- DOMAIN: IIC Tables --
--------------------------------------------------------------------------------

-----------------------
-- domain_iic_reader --
-----------------------
   
 CREATE ROLE domain_iic_reader WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;
 
 grant select
 on all tables in schema iic
 to domain_iic_reader;
   
-----------------------
-- domain_iic_writer --
-----------------------   
   
CREATE ROLE domain_iic_writer WITH
  NOSUPERUSER
  NOCREATEDB
  NOCREATEROLE
  NOLOGIN
  INHERIT
  CONNECTION LIMIT -1;
 
grant domain_iic_reader to domain_iic_writer;

grant INSERT, UPDATE
  ON ALL TABLES IN SCHEMA iic
    TO domain_iic_writer;

   
   
   
   
   
   
   
   
   
   
   