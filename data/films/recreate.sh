psql -c 'drop database sakila_films'
psql -c 'create database sakila_films'
psql -d sakila_films -a -f films-schema.sql
psql -d sakila_films -a -f films-insert.sql
