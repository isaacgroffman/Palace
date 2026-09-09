# ===================  NCAA DATA LAYER  ======================
# All-college TrackMan (2025 parquet + 2026 master pbp) opened
# as LAZY Arrow datasets. Nothing is pulled into RAM at startup
# beyond a tiny Pitcher/Team directory — a pitcher's rows are
# collected (and cached) only when that pitcher is selected.
# ============================================================

# Per-row feet repair for plate locations (same logic as advance app)
normalize_plate_location <- function(df) {
  if (!all(c("PlateLocSide", "PlateLocHeight") %in% names(df))) return(df)
  df %>%
    mutate(
      .is_inches = !is.na(PlateLocHeight) & abs(PlateLocHeight) > 8,
      PlateLocSide   = ifelse(.is_inches, PlateLocSide / 12,   PlateLocSide),
      PlateLocHeight = ifelse(.is_inches, PlateLocHeight / 12, PlateLocHeight)
    ) %>%
    select(-.is_inches)
}

# ---- Arrow schema guards --------------------------------------------
# A multi-file parquet dataset unifies schemas across its files. When the
# same column is a string in one file and a time32 in another, the unified
# schema picks time32 and collect() then attempts a string -> time32 cast,
# which Arrow has no kernel for:
#   NotImplemented: Unsupported cast from string to time32 using
#   function cast_time32
# The whole pull dies even though the column is never used downstream.
#
# Nothing here reads clock-time columns, so the fix is to not read them at
# all. Detected from the schema rather than a hardcoded name list, because
# which column trips depends on which files happen to be in the dataset.
.ARROW_DROP_ALWAYS <- c("Time", "UTCTime", "LocalDateTime",
                        "UTCDateTime", "UTCDate", "PitchTime", "GameTime")

.arrow_unreadable_cols <- function(ds) {
  s <- tryCatch(ds$schema, error = function(e) NULL)
  if (is.null(s)) return(character(0))
  nm <- tryCatch(names(s), error = function(e) character(0))
  if (length(nm) == 0) return(character(0))
  bad <- vapply(nm, function(n) {
    ty <- tryCatch(tolower(s[[n]]$type$ToString()), error = function(e) "")
    grepl("^time(32|64)", ty) || grepl("^duration", ty)
  }, logical(1))
  unique(c(nm[bad], intersect(.ARROW_DROP_ALWAYS, nm)))
}

# Collect from an Arrow dataset with the unreadable columns already
# excluded. `pipeline` shapes the query (filter/select) and runs BEFORE
# the drop, so callers keep full control. Falls back to a bare drop-only
# collect if the shaped query still errors.
.arrow_collect <- function(ds, pipeline = identity, context = "arrow pull") {
  if (is.null(ds)) return(NULL)
  drop <- .arrow_unreadable_cols(ds)
  run <- function(q) as.data.frame(dplyr::collect(
    if (length(drop)) dplyr::select(q, -dplyr::any_of(drop)) else q))
  out <- tryCatch(run(pipeline(ds)), error = function(e) e)
  if (!inherits(out, "error")) return(out)
  # second chance: some schemas only fail once a filter has been pushed
  # down, so retry against the raw dataset and filter in memory
  out2 <- tryCatch(run(ds), error = function(e) e)
  if (inherits(out2, "error")) {
    cat("WARNING:", context, "failed -", conditionMessage(out), "\n")
    return(NULL)
  }
  cat("NOTE:", context, "- pushed-down query failed, collected unfiltered",
      "and filtering in memory\n")
  tryCatch(as.data.frame(pipeline(out2)), error = function(e) out2)
}

# ---- Open the two NCAA sources lazily ----
# In serve mode these are promises: the parquet folders are only listed /
# downloaded when a session first scouts an NCAA pitcher. In build mode they
# open at boot so the directory artifact can be computed from them.
delayedAssign("NCAA25_DS", tryCatch({
  p <- hf_cached_file("CoastalBaseball/AdvancePitcher", "new_college_data25.parquet")
  arrow::open_dataset(p, format = "parquet")
}, error = function(e) {
  cat("WARNING: 2025 NCAA parquet unavailable -", conditionMessage(e), "\n")
  NULL
}))

delayedAssign("NCAA26_DS", tryCatch({
  d <- download_master_dataset_dir("CoastalBaseball/2026MasterDataset", "pbp")
  if (length(list.files(d, pattern = "\\.parquet$")) == 0) NULL
  else arrow::open_dataset(d, format = "parquet")
}, error = function(e) {
  cat("WARNING: 2026 NCAA pbp unavailable -", conditionMessage(e), "\n")
  NULL
}))

# ---- Tiny Pitcher/Team directory (2 columns only — fast) ----
.ncaa_dir_pull <- function(ds, src) {
  if (is.null(ds)) return(NULL)
  out <- tryCatch(
    ds %>% dplyr::distinct(Pitcher, PitcherTeam) %>% dplyr::collect() %>% as.data.frame(),
    error = function(e) tryCatch(
      ds %>% dplyr::select(Pitcher, PitcherTeam) %>% dplyr::collect() %>%
        as.data.frame() %>% dplyr::distinct(),
      error = function(e2) NULL))
  if (is.null(out) || nrow(out) == 0) return(NULL)
  out$src <- src
  out
}

# LEGACY coastal set: players with CCU game data / the old bio file. This is
# the spring-era identity — a 2027 transfer is NOT in it, so in spring
# contexts he still resolves to his old team (scouting view, his real data).
coastal_pitcher_set <- unique(c(
  bio$Pitcher_FL, bio$Pitcher,
  if (exists("data")      && is.data.frame(data))      unique(data$Pitcher),
  if (exists("fall25")    && is.data.frame(fall25))    unique(fall25$Pitcher),
  if (exists("prespring") && is.data.frame(prespring)) unique(prespring$Pitcher),
  if (exists("spring26")  && is.data.frame(spring26))  unique(spring26$Pitcher)
))
coastal_pitcher_set <- coastal_pitcher_set[!is.na(coastal_pitcher_set) & nzchar(coastal_pitcher_set)]
coastal_pitcher_norms <- unique(bp_norm_name(coastal_pitcher_set))

is_coastal_pitcher <- function(nm) {
  if (is.null(nm) || !nzchar(nm)) return(FALSE)
  nm %in% coastal_pitcher_set || bp_norm_name(nm) %in% coastal_pitcher_norms
}

# FALL-2026 coastal set: the 2027 roster + anyone with a Supabase pen. These
# players are Coastal ONLY when Fall 2026 is among the selected seasons.
fall26_pitcher_norms <- unique(bp_norm_name(c(
  if (!is.null(roster27)) roster27$Name[roster27$IsPitcher],
  bullpen_pitchers_fl
)))

is_fall26_pitcher <- function(nm) {
  !is.null(nm) && nzchar(nm) && bp_norm_name(nm) %in% fall26_pitcher_norms
}

# Season-aware coastal test: legacy always counts; roster/bullpen membership
# counts only while Fall 2026 is selected.
is_coastal_ctx <- function(nm, seasons) {
  is_coastal_pitcher(nm) ||
    ("Fall26" %in% (seasons %||% character(0)) && is_fall26_pitcher(nm))
}

# Raw Pitcher/Team pairs — kept around so the Team Report can enumerate
# a full opposing staff by team code.
if (palace_serve_mode()) {
  # Pitcher/team directory precomputed by scripts/build_serving_data.R —
  # avoids touching the full NCAA datasets at boot.
  ncaa_dir_pairs <- tryCatch(artifact_read("ncaa_dir_pairs"), error = function(e) {
    cat("WARNING: ncaa_dir_pairs artifact unavailable -", conditionMessage(e), "\n")
    data.frame(Pitcher = character(0), PitcherTeam = character(0),
               display = character(0), team_disp = character(0), src = character(0))
  })
} else {
  ncaa_dir_pairs <- tryCatch({
    dir_raw <- bind_rows(Filter(Negate(is.null), list(
      .ncaa_dir_pull(NCAA25_DS, "2025"),
      .ncaa_dir_pull(NCAA26_DS, "2026")
    )))
    if (nrow(dir_raw) == 0) stop("empty NCAA directory")
    dir_raw %>%
      filter(!is.na(Pitcher), nzchar(Pitcher)) %>%
      mutate(
        display   = ifelse(grepl(",", Pitcher), normalize_lastfirst(Pitcher), Pitcher),
        team_disp = prettify_team(PitcherTeam)
      ) %>%
      distinct(Pitcher, PitcherTeam, display, team_disp, src)
  }, error = function(e) {
    cat("WARNING: NCAA pairs build failed -", conditionMessage(e), "\n")
    data.frame(Pitcher = character(0), PitcherTeam = character(0),
               display = character(0), team_disp = character(0))
  })
}

# Every 2026 arm in Supabase pitchprofiler.pitcher_season joins the directory
# (both modes), so the global search offers the whole scored season.
ncaa_dir_pairs <- tryCatch({
  sb <- pp_directory()
  if (is.null(sb) || nrow(sb) == 0) stop("no Supabase directory")
  sb <- sb %>%
    filter(!is.na(Pitcher), nzchar(Pitcher)) %>%
    mutate(display   = ifelse(grepl(",", Pitcher), normalize_lastfirst(Pitcher), Pitcher),
           team_disp = prettify_team(PitcherTeam))
  out <- bind_rows(harmonize_types(list(ncaa_dir_pairs, sb))) %>%
    distinct(Pitcher, PitcherTeam, display, team_disp, src)
  cat("[pitchprofiler] directory:", nrow(sb), "Supabase pitcher-team pairs merged\n")
  out
}, error = function(e) {
  cat("NOTE: Supabase directory not merged -", conditionMessage(e), "\n")
  ncaa_dir_pairs
})

ncaa_directory <- tryCatch({
  if (nrow(ncaa_dir_pairs) == 0) stop("empty NCAA directory")
  ncaa_dir_pairs %>%
    filter(is.na(PitcherTeam) | PitcherTeam != "COA_CHA") %>%
    filter(!display %in% coastal_pitcher_set) %>%
    group_by(display) %>%
    summarise(
      raw_names = list(unique(Pitcher)),
      teams     = paste(sort(unique(team_disp[!is.na(team_disp)])), collapse = " / "),
      # Which source seasons this arm actually appears in ("2025"/"2026").
      srcs      = list(unique(as.character(src[!is.na(src)]))),
      .groups   = "drop"
    ) %>%
    arrange(display)
}, error = function(e) {
  cat("WARNING: NCAA directory build failed -", conditionMessage(e), "\n")
  data.frame(display = character(0), teams = character(0)) %>%
    mutate(raw_names = list(), srcs = list())
})

# ---- Team-level directory for the Team Report ----
ncaa_team_directory <- tryCatch({
  ncaa_dir_pairs %>%
    filter(!is.na(PitcherTeam), nzchar(PitcherTeam),
           !is.na(team_disp), nzchar(team_disp)) %>%
    group_by(team_disp) %>%
    summarise(codes = list(unique(PitcherTeam)),
              n_arms = dplyr::n_distinct(display), .groups = "drop") %>%
    arrange(team_disp)
}, error = function(e) {
  data.frame(team_disp = character(0), n_arms = integer(0)) %>%
    mutate(codes = list())
})
ncaa_team_codes <- setNames(ncaa_team_directory$codes, ncaa_team_directory$team_disp)

# Lean per-team loader (only the columns the count report reads), lazy
# filter + collect, cached per team for the session.
# ============================================================
