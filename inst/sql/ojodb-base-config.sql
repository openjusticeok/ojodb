--------------------------------------------------------------------------------
-- OJODB Base Configuration Script
--
-- Purpose: Configure all database roles and permissions
--
-- OWNERSHIP MODEL:
-- Only the all_table_owner role has privileges to create new database objects.
-- This ensures consistent ownership across all schemas and tables.
--
-- PLAN:
-- Step 1: Create base and administrative roles
-- Step 2: Create domain-specific roles 
-- Step 3: Create individual user roles and grant appropriate permissions
-- Step 4: Configure schema ownership and privileges
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ALL_TABLE_OWNER ROLE
-- On a fresh db, we'd run this first, set a password, then log in as all_table_owner to run the rest.
--------------------------------------------------------------------------------

-- Create dedicated owner role for all database objects
CREATE ROLE all_table_owner WITH
    NOSUPERUSER
    CREATEDB
    CREATEROLE
    LOGIN
    NOINHERIT
    CONNECTION LIMIT -1;

-- NOTE: all_table_owner is the only role that should have CREATE permissions. This is to hopefully help ensure 
-- that all schemas / tables are always owned by all_table_owner (which can then be independent of any single user). 
-- The downside is we'll have to make sure we log in with this user every time we manage tables / schemas / etc.

-- ALTER ROLE all_table_owner WITH PASSWORD '<password>'; -- Set a password, store in Bitwarden, and log in to manage tables / schemas / etc.

--------------------------------------------------------------------------------
-- CREATE SCHEMAS (if they don't exist already)
--------------------------------------------------------------------------------

CREATE SCHEMA IF NOT EXISTS doc_tracker; -- DOC data
CREATE SCHEMA IF NOT EXISTS eviction_addresses; -- Eviction addresses data
CREATE SCHEMA IF NOT EXISTS eviction_dashboard; -- Eviction dashboard data
CREATE SCHEMA IF NOT EXISTS iic; -- Tulsa Jail / Inmate Information Center data
CREATE SCHEMA IF NOT EXISTS ocdc_new; -- New OCDC data
CREATE SCHEMA IF NOT EXISTS odoc; -- ODOC data
CREATE SCHEMA IF NOT EXISTS ppb; -- PPB data
CREATE SCHEMA IF NOT EXISTS tulsa_county_jail; -- Tulsa County Jail data
-- public schema (used for OSCN data) exists by default
-- NOT creating 'archive', 'oscn', 'ocdc' (old), or 'oscn_test' schemas which currently exist.

--------------------------------------------------------------------------------
-- REVOKE PUBLIC PERMISSIONS
-- This is a security thing; it makes sure that newly created users don't have access to anything at all by default.
--------------------------------------------------------------------------------

-- Revoke default permissions from PUBLIC role
REVOKE ALL ON SCHEMA public FROM PUBLIC; -- PUBLIC is basically just like the default for future created users

-- For each schema, revoke default permissions
REVOKE ALL ON SCHEMA doc_tracker FROM PUBLIC;
REVOKE ALL ON SCHEMA eviction_addresses FROM PUBLIC;
REVOKE ALL ON SCHEMA eviction_dashboard FROM PUBLIC;
REVOKE ALL ON SCHEMA iic FROM PUBLIC;
REVOKE ALL ON SCHEMA ocdc_new FROM PUBLIC;
REVOKE ALL ON SCHEMA odoc FROM PUBLIC;
REVOKE ALL ON SCHEMA ppb FROM PUBLIC;
REVOKE ALL ON SCHEMA tulsa_county_jail FROM PUBLIC;

-- Revoke permissions on existing objects in each schema
REVOKE ALL ON ALL TABLES IN SCHEMA public FROM PUBLIC;
REVOKE ALL ON ALL TABLES IN SCHEMA doc_tracker FROM PUBLIC;
REVOKE ALL ON ALL TABLES IN SCHEMA eviction_addresses FROM PUBLIC;
REVOKE ALL ON ALL TABLES IN SCHEMA eviction_dashboard FROM PUBLIC;
REVOKE ALL ON ALL TABLES IN SCHEMA iic FROM PUBLIC;
REVOKE ALL ON ALL TABLES IN SCHEMA ocdc_new FROM PUBLIC;
REVOKE ALL ON ALL TABLES IN SCHEMA odoc FROM PUBLIC;
REVOKE ALL ON ALL TABLES IN SCHEMA ppb FROM PUBLIC;
REVOKE ALL ON ALL TABLES IN SCHEMA tulsa_county_jail FROM PUBLIC;

-- Set default privileges to prevent PUBLIC access for future tables
ALTER DEFAULT PRIVILEGES REVOKE ALL ON TABLES FROM PUBLIC;

--------------------------------------------------------------------------------
-- ALL_TABLE_READER ROLE
--------------------------------------------------------------------------------

CREATE ROLE all_table_reader WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant schema usage to all_table_reader
GRANT USAGE ON SCHEMA doc_tracker TO all_table_reader;
GRANT USAGE ON SCHEMA eviction_addresses TO all_table_reader;
GRANT USAGE ON SCHEMA eviction_dashboard TO all_table_reader;
GRANT USAGE ON SCHEMA iic TO all_table_reader;
GRANT USAGE ON SCHEMA ocdc_new TO all_table_reader;
GRANT USAGE ON SCHEMA odoc TO all_table_reader;
GRANT USAGE ON SCHEMA ppb TO all_table_reader;
GRANT USAGE ON SCHEMA public TO all_table_reader;
GRANT USAGE ON SCHEMA tulsa_county_jail TO all_table_reader;

-- Grant SELECT permissions on all tables in each schema
GRANT SELECT ON ALL TABLES IN SCHEMA doc_tracker TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA eviction_addresses TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA eviction_dashboard TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA iic TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA ocdc_new TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA odoc TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA ppb TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA tulsa_county_jail TO all_table_reader;

-- Set default privileges for future tables in all schemas
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA doc_tracker GRANT SELECT ON TABLES TO all_table_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA eviction_addresses GRANT SELECT ON TABLES TO all_table_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA eviction_dashboard GRANT SELECT ON TABLES TO all_table_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA iic GRANT SELECT ON TABLES TO all_table_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA ocdc_new GRANT SELECT ON TABLES TO all_table_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA odoc GRANT SELECT ON TABLES TO all_table_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA ppb GRANT SELECT ON TABLES TO all_table_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA public GRANT SELECT ON TABLES TO all_table_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA tulsa_county_jail GRANT SELECT ON TABLES TO all_table_reader;

--------------------------------------------------------------------------------
-- ALL_TABLE_WRITER ROLE
--------------------------------------------------------------------------------

-- Create writer role and inherit reader permissions
CREATE ROLE all_table_writer WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

GRANT all_table_reader TO all_table_writer;

-- Grant INSERT and UPDATE permissions on all tables
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA doc_tracker TO all_table_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA eviction_addresses TO all_table_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA eviction_dashboard TO all_table_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA iic TO all_table_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA ocdc_new TO all_table_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA odoc TO all_table_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA ppb TO all_table_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA public TO all_table_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA tulsa_county_jail TO all_table_writer;

-- Set default privileges for future tables in all schemas
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA doc_tracker GRANT INSERT, UPDATE ON TABLES TO all_table_writer;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA eviction_addresses GRANT INSERT, UPDATE ON TABLES TO all_table_writer;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA eviction_dashboard GRANT INSERT, UPDATE ON TABLES TO all_table_writer;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA iic GRANT INSERT, UPDATE ON TABLES TO all_table_writer;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA ocdc_new GRANT INSERT, UPDATE ON TABLES TO all_table_writer;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA odoc GRANT INSERT, UPDATE ON TABLES TO all_table_writer;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA ppb GRANT INSERT, UPDATE ON TABLES TO all_table_writer;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA public GRANT INSERT, UPDATE ON TABLES TO all_table_writer;
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA tulsa_county_jail GRANT INSERT, UPDATE ON TABLES TO all_table_writer;

--------------------------------------------------------------------------------
-- ALL_TABLE_ADMIN ROLE
--------------------------------------------------------------------------------

CREATE ROLE all_table_admin WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

GRANT all_table_writer TO all_table_admin;

-- Grant admin role to the owner role so it can perform all operations
GRANT all_table_admin TO all_table_owner;

-- Grant usage and limited privileges to all schemas (but not CREATE)
GRANT USAGE ON SCHEMA doc_tracker TO all_table_admin;
GRANT USAGE ON SCHEMA eviction_addresses TO all_table_admin;
GRANT USAGE ON SCHEMA eviction_dashboard TO all_table_admin;
GRANT USAGE ON SCHEMA iic TO all_table_admin;
GRANT USAGE ON SCHEMA ocdc_new TO all_table_admin;
GRANT USAGE ON SCHEMA odoc TO all_table_admin;
GRANT USAGE ON SCHEMA ppb TO all_table_admin;
GRANT USAGE ON SCHEMA public TO all_table_admin;
GRANT USAGE ON SCHEMA tulsa_county_jail TO all_table_admin;

-- Grant ALL PRIVILEGES on all tables in each schema
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA doc_tracker TO all_table_admin;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA eviction_addresses TO all_table_admin;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA eviction_dashboard TO all_table_admin;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA iic TO all_table_admin;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA ocdc_new TO all_table_admin;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA odoc TO all_table_admin;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA ppb TO all_table_admin;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO all_table_admin;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA tulsa_county_jail TO all_table_admin;

--------------------------------------------------------------------------------
-- SCHEMA OWNERSHIP
--------------------------------------------------------------------------------

-- Set all_table_owner as the owner of all schemas
ALTER SCHEMA doc_tracker OWNER TO all_table_owner;
ALTER SCHEMA eviction_addresses OWNER TO all_table_owner;
ALTER SCHEMA eviction_dashboard OWNER TO all_table_owner;
ALTER SCHEMA iic OWNER TO all_table_owner;
ALTER SCHEMA ocdc_new OWNER TO all_table_owner;
ALTER SCHEMA odoc OWNER TO all_table_owner;
ALTER SCHEMA ppb OWNER TO all_table_owner;
ALTER SCHEMA public OWNER TO all_table_owner;
ALTER SCHEMA tulsa_county_jail OWNER TO all_table_owner;

--------------------------------------------------------------------------------
-- OJO_ANALYST ROLE
--------------------------------------------------------------------------------

CREATE ROLE ojo_analyst WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

GRANT all_table_reader TO ojo_analyst;
--------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC ROLES
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- DOMAIN: Document Tracker Tables
--------------------------------------------------------------------------------

-- Create domain_doc_tracker_reader role
CREATE ROLE domain_doc_tracker_reader WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant SELECT permissions on doc_tracker schema
GRANT USAGE ON SCHEMA doc_tracker TO domain_doc_tracker_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA doc_tracker TO domain_doc_tracker_reader;

-- Set default privileges for future tables in doc_tracker schema
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA doc_tracker
    GRANT SELECT ON TABLES TO domain_doc_tracker_reader;

-- Create domain_doc_tracker_writer role
CREATE ROLE domain_doc_tracker_writer WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant reader role to writer and add write permissions
GRANT domain_doc_tracker_reader TO domain_doc_tracker_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA doc_tracker TO domain_doc_tracker_writer;

-- Set default privileges for future tables in doc_tracker schema
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA doc_tracker
    GRANT INSERT, UPDATE ON TABLES TO domain_doc_tracker_writer;

--------------------------------------------------------------------------------
-- DOMAIN: Eviction Tables
--------------------------------------------------------------------------------

-- Create domain_eviction_addresses_reader role
CREATE ROLE domain_eviction_addresses_reader WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant SELECT permissions on all eviction addresses tables
GRANT USAGE ON SCHEMA eviction_addresses TO domain_eviction_addresses_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA eviction_addresses TO domain_eviction_addresses_reader;

-- Set default privileges for future tables
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA eviction_addresses
    GRANT SELECT ON TABLES TO domain_eviction_addresses_reader;

-- Create domain_eviction_addresses_writer role
CREATE ROLE domain_eviction_addresses_writer WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant reader role to writer and add write permissions
GRANT domain_eviction_addresses_reader TO domain_eviction_addresses_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA eviction_addresses TO domain_eviction_addresses_writer;

--------------------------------------------------------------------------------
-- DOMAIN: Eviction Dashboard Tables
--------------------------------------------------------------------------------

-- Create domain_eviction_dashboard_reader role
CREATE ROLE domain_eviction_dashboard_reader WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant SELECT permissions on eviction_dashboard schema
GRANT USAGE ON SCHEMA eviction_dashboard TO domain_eviction_dashboard_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA eviction_dashboard TO domain_eviction_dashboard_reader;

-- Set default privileges for future tables in eviction_dashboard schema
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA eviction_dashboard
    GRANT SELECT ON TABLES TO domain_eviction_dashboard_reader;

-- Create domain_eviction_dashboard_writer role
CREATE ROLE domain_eviction_dashboard_writer WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant reader role to writer and add write permissions
GRANT domain_eviction_dashboard_reader TO domain_eviction_dashboard_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA eviction_dashboard TO domain_eviction_dashboard_writer;

-- Set default privileges for future tables in eviction_dashboard schema
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA eviction_dashboard
    GRANT INSERT, UPDATE ON TABLES TO domain_eviction_dashboard_writer;

--------------------------------------------------------------------------------
-- DOMAIN: IIC Tables
--------------------------------------------------------------------------------

-- Create domain_iic_reader role
CREATE ROLE domain_iic_reader WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant SELECT permissions on IIC schema
GRANT USAGE ON SCHEMA iic TO domain_iic_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA iic TO domain_iic_reader;
   
-- Create domain_iic_writer role   
CREATE ROLE domain_iic_writer WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant reader role to writer and add write permissions
GRANT domain_iic_reader TO domain_iic_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA iic TO domain_iic_writer;

--------------------------------------------------------------------------------
-- DOMAIN: Oklahoma County New / OCDC_NEW Tables
--------------------------------------------------------------------------------

-- Create domain_ocdc_new_reader role
CREATE ROLE domain_ocdc_new_reader WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant SELECT permissions on OCDC_NEW schema
GRANT USAGE ON SCHEMA ocdc_new TO domain_ocdc_new_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA ocdc_new TO domain_ocdc_new_reader;

-- Set default privileges for future tables in ocdc_new schema
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA ocdc_new
    GRANT SELECT ON TABLES TO domain_ocdc_new_reader;

-- Create domain_ocdc_new_writer role
CREATE ROLE domain_ocdc_new_writer WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant reader role to writer and add write permissions
GRANT domain_ocdc_new_reader TO domain_ocdc_new_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA ocdc_new TO domain_ocdc_new_writer;

-- Set default privileges for future tables in ocdc_new schema
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA ocdc_new
    GRANT INSERT, UPDATE ON TABLES TO domain_ocdc_new_writer;

--------------------------------------------------------------------------------
-- DOMAIN: ODOC Tables
--------------------------------------------------------------------------------

-- Create domain_odoc_reader role
CREATE ROLE domain_odoc_reader WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant SELECT permissions on ODOC schema
GRANT USAGE ON SCHEMA odoc TO domain_odoc_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA odoc TO domain_odoc_reader;

-- Set default privileges for future tables in odoc schema
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA odoc
    GRANT SELECT ON TABLES TO domain_odoc_reader;

-- Create domain_odoc_writer role
CREATE ROLE domain_odoc_writer WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant reader role to writer and add write permissions
GRANT domain_odoc_reader TO domain_odoc_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA odoc TO domain_odoc_writer;

-- Set default privileges for future tables in odoc schema
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA odoc
    GRANT INSERT, UPDATE ON TABLES TO domain_odoc_writer;

--------------------------------------------------------------------------------
-- DOMAIN: OSCN Tables
--------------------------------------------------------------------------------

-- Create domain_oscn_reader role
CREATE ROLE domain_oscn_reader WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant SELECT permissions on PUBLIC schema (OSCN data)
GRANT USAGE ON SCHEMA public TO domain_oscn_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO domain_oscn_reader;

-- Revoke select on specific tables
REVOKE SELECT ON TABLE public.exception_log, public.process_log FROM domain_oscn_reader;

-- Set default privileges for future tables in public schema
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA public
    GRANT SELECT ON TABLES TO domain_oscn_reader;

-- Create domain_oscn_writer role
CREATE ROLE domain_oscn_writer WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant reader role to writer and add write permissions
GRANT domain_oscn_reader TO domain_oscn_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA public TO domain_oscn_writer;

-- Grant access to log tables to writers
GRANT SELECT ON TABLE public.exception_log, public.process_log TO domain_oscn_writer;

-- Set default privileges for future tables in public schema
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA public
    GRANT INSERT, UPDATE ON TABLES TO domain_oscn_writer;

--------------------------------------------------------------------------------
-- DOMAIN: PPB Tables
--------------------------------------------------------------------------------

-- Create domain_ppb_reader role
CREATE ROLE domain_ppb_reader WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant SELECT permissions on PPB schema
GRANT USAGE ON SCHEMA ppb TO domain_ppb_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA ppb TO domain_ppb_reader;

-- Set default privileges for future tables in ppb schema
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA ppb
    GRANT SELECT ON TABLES TO domain_ppb_reader;

-- Create domain_ppb_writer role
CREATE ROLE domain_ppb_writer WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant reader role to writer and add write permissions
GRANT domain_ppb_reader TO domain_ppb_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA ppb TO domain_ppb_writer;

-- Set default privileges for future tables in ppb schema
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA ppb
    GRANT INSERT, UPDATE ON TABLES TO domain_ppb_writer;

--------------------------------------------------------------------------------
-- DOMAIN: Tulsa County Jail Tables
--------------------------------------------------------------------------------

-- Create domain_tulsa_county_jail_reader role
CREATE ROLE domain_tulsa_county_jail_reader WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant SELECT permissions on tulsa_county_jail schema
GRANT USAGE ON SCHEMA tulsa_county_jail TO domain_tulsa_county_jail_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA tulsa_county_jail TO domain_tulsa_county_jail_reader;

-- Set default privileges for future tables in tulsa_county_jail schema
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA tulsa_county_jail
    GRANT SELECT ON TABLES TO domain_tulsa_county_jail_reader;

-- Create domain_tulsa_county_jail_writer role
CREATE ROLE domain_tulsa_county_jail_writer WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant reader role to writer and add write permissions
GRANT domain_tulsa_county_jail_reader TO domain_tulsa_county_jail_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA tulsa_county_jail TO domain_tulsa_county_jail_writer;

-- Set default privileges for future tables in tulsa_county_jail schema
ALTER DEFAULT PRIVILEGES FOR ROLE all_table_owner IN SCHEMA tulsa_county_jail
    GRANT INSERT, UPDATE ON TABLES TO domain_tulsa_county_jail_writer;

--------------------------------------------------------------------------------
-- INDIVIDUAL ANALYST ROLES
--------------------------------------------------------------------------------

-- Create individual analyst login roles
CREATE ROLE abell WITH 
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    INHERIT
    LOGIN
    NOREPLICATION
    NOBYPASSRLS
    CONNECTION LIMIT -1;

CREATE ROLE aflores WITH 
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    INHERIT
    LOGIN
    NOREPLICATION
    NOBYPASSRLS
    CONNECTION LIMIT -1;

CREATE ROLE aharvey WITH 
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    INHERIT
    LOGIN
    NOREPLICATION
    NOBYPASSRLS
    CONNECTION LIMIT -1;

CREATE ROLE bgregory WITH 
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    INHERIT
    LOGIN
    NOREPLICATION
    NOBYPASSRLS
    CONNECTION LIMIT -1;

CREATE ROLE prozhkova WITH 
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    INHERIT
    LOGIN
    NOREPLICATION
    NOBYPASSRLS
    CONNECTION LIMIT -1;

-- Grant analyst permissions to individual users
GRANT ojo_analyst TO abell;
GRANT ojo_analyst TO aflores;
GRANT ojo_analyst TO aharvey;
GRANT ojo_analyst TO bgregory;
GRANT ojo_analyst TO prozhkova;

-- Grant admin permissions to specific users
GRANT all_table_admin TO abell;
GRANT all_table_admin TO bgregory;
GRANT all_table_admin TO aflores;

--------------------------------------------------------------------------------
-- SET PASSWORDS FOR LOGIN ROLES (uncomment and replace <password> as needed)
--------------------------------------------------------------------------------
-- ALTER ROLE all_table_owner WITH PASSWORD '<password>';
-- ALTER ROLE abell WITH PASSWORD '<password>';
-- ALTER ROLE aflores WITH PASSWORD '<password>';
-- ALTER ROLE aharvey WITH PASSWORD '<password>';
-- ALTER ROLE bgregory WITH PASSWORD '<password>';
-- ALTER ROLE prozhkova WITH PASSWORD '<password>';
