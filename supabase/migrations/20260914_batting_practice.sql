-- Batting practice (TrackMan "Hitting" practice sessions) mirrored from the
-- Palace Upload panel (R/palace_upload.R, type "bp"). Storage
-- practice/hitting/<date>_<session>.parquet is the app's source of truth; this
-- table is the queryable copy and is optional: the upload skips it when the
-- table does not exist. Run once in the Supabase SQL editor (Project > SQL).
create table if not exists public.batting_practice (
  play_id               text primary key,
  session_id            text not null,
  session_date          date,
  "time"                timestamptz,
  pitch_no              integer,
  batter                text,
  batter_id             text,
  batter_side           text,
  pitcher               text,
  pitcher_id            text,
  pitcher_throws        text,
  tagged_pitch_type     text,
  rel_speed             numeric,
  spin_rate             numeric,
  induced_vert_break    numeric,
  horz_break            numeric,
  plate_loc_side        numeric,
  plate_loc_height      numeric,
  vert_appr_angle       numeric,
  hit_uid               text,
  exit_speed            numeric,
  launch_angle          numeric,
  direction             numeric,
  hit_spin_rate         numeric,
  distance              numeric,
  bearing               numeric,
  hang_time             numeric,
  contact_position_x    numeric,
  contact_position_y    numeric,
  contact_position_z    numeric,
  last_tracked_distance numeric,
  session_type          text,
  external_session_id   text,
  upload_id             text,
  uploaded_at           timestamptz default now(),
  updated_at            timestamptz default now()
);
create index if not exists batting_practice_session_idx on public.batting_practice (session_date, session_id);
create index if not exists batting_practice_batter_idx  on public.batting_practice (batter);

-- The app writes with the secret (service) key, which bypasses RLS; keep the
-- table closed to the anon / authenticated API roles.
alter table public.batting_practice enable row level security;

-- Read access for the read-only Postgres role Bullpen Central uses, if present.
do $$
begin
  if exists (select 1 from pg_roles where rolname = 'bullpen_reader') then
    grant select on public.batting_practice to bullpen_reader;
  end if;
end $$;
