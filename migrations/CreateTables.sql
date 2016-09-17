CREATE TABLE author (
  id serial primary key,
  firstname character varying(40) NOT NULL,
  lastName character varying(50),
  email character varying NOT NULL,
  CONSTRAINT unique_email_key UNIQUE (email)
);
CREATE TABLE series (
  id serial primary key,
  name text NOT NULL,
  description text NOT NULL,
  parentid integer
  CONSTRAINT parent_series_fkey FOREIGN KEY (parentid)
      REFERENCES public.series (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION
);
CREATE TABLE post (
  id serial primary key,
  authorid integer NOT NULL,
  seriesid integer,
  title character varying(255) NOT NULL,
  body text,
  synopsis text,
  created timestamp with time zone NOT NULL,
  modified timestamp with time zone,
  pubdate timestamp with time zone,
  CONSTRAINT author_id_post_fk FOREIGN KEY (authorid)
      REFERENCES public.author (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE,
  CONSTRAINT series_id_post_fk FOREIGN KEY (seriesid)
    REFERENCES public.series (id) MATCH SIMPLE
    ON UPDATE NO ACTION ON DELETE NO ACTION
);
