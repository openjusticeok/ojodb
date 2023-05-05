--------------------------------------------------------------------------------
-- integration roles
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- integration_restore_hope_evictions

CREATE ROLE integration_restore_hope_evictions WITH 
	NOSUPERUSER
	NOCREATEDB
	NOCREATEROLE
	INHERIT
	LOGIN
	NOREPLICATION
	NOBYPASSRLS
	CONNECTION LIMIT -1;

-- this one needs read access to eviction case tables via domain_oscn_reader

grant domain_oscn_reader to integration_restore_hope_evictions;
-- grant cloudsqlsuperuser to integration_restore_hope_evictions;

--------------------------------------------------------------------------------
-- integration_tps_evictions

CREATE ROLE integration_tps_evictions WITH 
	NOSUPERUSER
	NOCREATEDB
	NOCREATEROLE
	INHERIT
	LOGIN
	NOREPLICATION
	NOBYPASSRLS
	CONNECTION LIMIT -1;

-- this one needs read access to eviction case tables AND eviction address table

grant domain_oscn_reader to integration_tps_evictions;
grant domain_eviction_addresses_reader to integration_tps_evictions;
-- grant cloudsqlsuperuser to integration_tps_evictions;

--------------------------------------------------------------------------------
-- integration_wjt_evictions

CREATE ROLE integration_wjt_evictions WITH 
	NOSUPERUSER
	NOCREATEDB
	NOCREATEROLE
	INHERIT
	LOGIN
	NOREPLICATION
	NOBYPASSRLS
	CONNECTION LIMIT -1;

-- this one needs read access to IIC schema

grant domain_iic_reader to integration_wjt_evictions;
-- grant cloudsqlsuperuser to integration_wjt_evictions;

--------------------------------------------------------------------------------
-- integration_housing_solutions

CREATE ROLE integration_housing_solutions WITH 
	NOSUPERUSER
	NOCREATEDB
	NOCREATEROLE
	INHERIT
	LOGIN
	NOREPLICATION
	NOBYPASSRLS
	CONNECTION LIMIT -1;

-- this one needs read access to oscn tables and eviction address table

grant domain_oscn_reader to integration_housing_solutions;
grant domain_eviction_addresses_reader to integration_housing_solutions;
-- grant cloudsqlsuperuser to integration_housing_solutions;

--------------------------------------------------------------------------------
-- integration_cjdc

CREATE ROLE integration_cjdc WITH 
	NOSUPERUSER
	NOCREATEDB
	NOCREATEROLE
	INHERIT
	LOGIN
	NOREPLICATION
	NOBYPASSRLS
	CONNECTION LIMIT -1;

-- this one needs read access to oscn tables

grant domain_oscn_reader to integration_cjdc;
-- grant cloudsqlsuperuser to integration_cjdc;

--------------------------------------------------------------------------------
-- integration_9b

CREATE ROLE integration_9b WITH 
	NOSUPERUSER
	NOCREATEDB
	NOCREATEROLE
	INHERIT
	LOGIN
	NOREPLICATION
	NOBYPASSRLS
	CONNECTION LIMIT -1;

-- this one needs read access to IIC schema

grant domain_oscn_reader to integration_9b;
grant domain_eviction_addresses_reader to integration_9b;
-- grant cloudsqlsuperuser to integration_9b;

--------------------------------------------------------------------------------
-- integration_bigquery

CREATE ROLE integration_bigquery WITH 
	NOSUPERUSER
	NOCREATEDB
	NOCREATEROLE
	INHERIT
	LOGIN
	NOREPLICATION
	NOBYPASSRLS
	CONNECTION LIMIT -1;

-- this one needs read access to all tables

grant all_table_reader to integration_bigquery;
-- grant cloudsqlsuperuser to integration_bigquery;

--------------------------------------------------------------------------------
-- integration_eviction_lab

CREATE ROLE integration_eviction_lab WITH 
	NOSUPERUSER
	NOCREATEDB
	NOCREATEROLE
	INHERIT
	LOGIN
	NOREPLICATION
	NOBYPASSRLS
	CONNECTION LIMIT -1;

-- this one needs read access to all oscn tables

grant domain_oscn_reader to integration_eviction_lab;
-- grant cloudsqlsuperuser to integration_eviction_lab;

--------------------------------------------------------------------------------
-- integration_bigquery

CREATE ROLE integration_cjars WITH 
	NOSUPERUSER
	NOCREATEDB
	NOCREATEROLE
	INHERIT
	LOGIN
	NOREPLICATION
	NOBYPASSRLS
	CONNECTION LIMIT -1;

-- this one needs read access to all oscn tables

grant domain_oscn_reader to integration_cjars;
-- grant cloudsqlsuperuser to integration_cjars;
