create extension citext;
create extension pgcrypto;

create table auths (
  id bigserial primary key not null,
  pass text not null,
  email citext not null unique,
  email_verification_code text not null,
  is_email_verified boolean not null
);