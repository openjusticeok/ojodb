--------------------------------------------------------------------------------
-- OJODB Base Roles Configuration Script
--
-- Purpose: Configure all database roles and permissions
--
-- PLAN:
-- Step 1: Create all the new roles/users, configure their permissions
-- Step 2: Transfer ownership of all tables to ojo_table_owner
-- Step 3: Configure views, materialized views, and usage permissions
-- Step 4: Remove old/unused roles/users
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- INDIVIDUAL ANALYST ROLES
--------------------------------------------------------------------------------

-- Create individual analyst login roles
CREATE ROLE bgregory WITH 
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    INHERIT
    LOGIN
    NOREPLICATION
    NOBYPASSRLS
    CONNECTION LIMIT -1;

CREATE ROLE abell WITH 
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

CREATE ROLE prozhkova WITH 
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

-- Grant analyst permissions to individual users
GRANT ojo_analyst TO bgregory;
GRANT ojo_analyst TO abell;
GRANT ojo_analyst TO aharvey;
GRANT ojo_analyst TO prozhkova;
GRANT ojo_analyst TO aflores;

--------------------------------------------------------------------------------
-- BASE DATABASE ROLES
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ALL_TABLE_READER ROLE
--------------------------------------------------------------------------------

-- Create base read-only role
CREATE ROLE all_table_reader WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant schema usage to all_table_reader
GRANT USAGE ON SCHEMA eviction_addresses TO all_table_reader;
GRANT USAGE ON SCHEMA iic TO all_table_reader;
GRANT USAGE ON SCHEMA ocdc TO all_table_reader;
GRANT USAGE ON SCHEMA archive TO all_table_reader;
GRANT USAGE ON SCHEMA public TO all_table_reader;
GRANT USAGE ON SCHEMA odoc TO all_table_reader;

-- Grant SELECT permissions on all tables in each schema
GRANT SELECT ON ALL TABLES IN SCHEMA eviction_addresses TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA iic TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA ocdc TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA archive TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO all_table_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA odoc TO all_table_reader;

-- Set default privileges for future tables in all schemas
ALTER DEFAULT PRIVILEGES IN SCHEMA eviction_addresses GRANT SELECT ON TABLES TO all_table_reader;
ALTER DEFAULT PRIVILEGES IN SCHEMA iic GRANT SELECT ON TABLES TO all_table_reader;
ALTER DEFAULT PRIVILEGES IN SCHEMA ocdc GRANT SELECT ON TABLES TO all_table_reader;
ALTER DEFAULT PRIVILEGES IN SCHEMA archive GRANT SELECT ON TABLES TO all_table_reader;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO all_table_reader;
ALTER DEFAULT PRIVILEGES IN SCHEMA odoc GRANT SELECT ON TABLES TO all_table_reader;

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
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA archive TO all_table_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA ocdc TO all_table_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA iic TO all_table_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA odoc TO all_table_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA eviction_addresses TO all_table_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA public TO all_table_writer;

-- Set default privileges for future tables in all schemas
ALTER DEFAULT PRIVILEGES IN SCHEMA eviction_addresses GRANT INSERT, UPDATE ON TABLES TO all_table_writer;
ALTER DEFAULT PRIVILEGES IN SCHEMA iic GRANT INSERT, UPDATE ON TABLES TO all_table_writer;
ALTER DEFAULT PRIVILEGES IN SCHEMA ocdc GRANT INSERT, UPDATE ON TABLES TO all_table_writer;
ALTER DEFAULT PRIVILEGES IN SCHEMA archive GRANT INSERT, UPDATE ON TABLES TO all_table_writer;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT INSERT, UPDATE ON TABLES TO all_table_writer;
ALTER DEFAULT PRIVILEGES IN SCHEMA odoc GRANT INSERT, UPDATE ON TABLES TO all_table_writer;

-- Grant CREATE permissions on schemas
GRANT CREATE ON SCHEMA eviction_addresses TO all_table_reader;
GRANT CREATE ON SCHEMA iic TO all_table_reader;
GRANT CREATE ON SCHEMA ocdc TO all_table_reader;
GRANT CREATE ON SCHEMA archive TO all_table_reader;
GRANT CREATE ON SCHEMA public TO all_table_reader;
GRANT CREATE ON SCHEMA odoc TO all_table_reader;

--------------------------------------------------------------------------------
-- ALL_TABLE_ADMIN ROLE
--------------------------------------------------------------------------------

CREATE ROLE all_table_admin WITH
    NOCREATEDB
    CREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

GRANT all_table_writer TO all_table_admin;

--------------------------------------------------------------------------------
-- OJO_TABLE_OWNER ROLE
--------------------------------------------------------------------------------

CREATE ROLE ojo_table_owner WITH
    CREATEDB
    CREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

GRANT all_table_admin TO ojo_table_owner;

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
-- DOMAIN: Oklahoma County / OCDC Tables
--------------------------------------------------------------------------------

-- Create domain_ocdc_reader role
CREATE ROLE domain_ocdc_reader WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant SELECT permissions on OCDC schema
GRANT USAGE ON SCHEMA ocdc TO domain_ocdc_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA ocdc TO domain_ocdc_reader;

-- Set default privileges for future tables in ocdc schema
ALTER DEFAULT PRIVILEGES IN SCHEMA ocdc
    GRANT SELECT ON TABLES TO domain_ocdc_reader;

-- Create domain_ocdc_writer role
CREATE ROLE domain_ocdc_writer WITH
    NOSUPERUSER
    NOCREATEDB
    NOCREATEROLE
    NOLOGIN
    INHERIT
    CONNECTION LIMIT -1;

-- Grant reader role to writer and add write permissions
GRANT domain_ocdc_reader TO domain_ocdc_writer;
GRANT INSERT, UPDATE ON ALL TABLES IN SCHEMA ocdc TO domain_ocdc_writer;

-- Set default privileges for future tables in ocdc schema
ALTER DEFAULT PRIVILEGES IN SCHEMA ocdc
    GRANT INSERT, UPDATE ON TABLES TO domain_ocdc_writer;

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
ALTER DEFAULT PRIVILEGES IN SCHEMA odoc
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
ALTER DEFAULT PRIVILEGES IN SCHEMA odoc
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

-- Grant SELECT permissions on specific eviction addresses table
GRANT USAGE ON SCHEMA eviction_addresses TO domain_eviction_addresses_reader;
GRANT SELECT ON eviction_addresses.address TO domain_eviction_addresses_reader;

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
