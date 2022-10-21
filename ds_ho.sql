-- Adminer 4.8.1 PostgreSQL 10.9 dump

DROP TABLE IF EXISTS "ds_ho";
DROP SEQUENCE IF EXISTS ds_ho_id_seq;
CREATE SEQUENCE ds_ho_id_seq INCREMENT 1 MINVALUE 1 MAXVALUE 2147483647 CACHE 1;

CREATE TABLE "public"."ds_ho" (
    "id" integer DEFAULT nextval('ds_ho_id_seq') NOT NULL,
    "matinh" character varying,
    "mahuyen" character varying,
    "maxa" character varying,
    "ho" character varying,
    "songuoi" character varying,
    CONSTRAINT "ds_ho_pkey" PRIMARY KEY ("id")
) WITH (oids = false);


-- 2022-09-03 17:06:13.724506+02
