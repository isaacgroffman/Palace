# ============================================================================
# PLAYERS & ROSTER
# Coastal pitcher/hitter bios, the 2027 roster, bullpen-pitcher list from Supabase.
# ============================================================================

bio <- readr::read_csv(palace_file_candidates("CCU_Pitcher_Bio.csv")[1], show_col_types = FALSE)

bio <- bio %>%
  mutate(
    Pitcher_FL = gsub("^(.*),\\s*(.*)$", "\\2 \\1", Pitcher),
    Roster_2026 = `2026_Roster`  
  )

# ============================================================
# CCU_Hitter_Bio.csv — Coastal hitter bios (roster page, headers,
# unified search). Local file first (ships with the Space), HF
# dataset as backstop; NULL-safe everywhere it's consumed.
# ============================================================
hitter_bio <- tryCatch({
  cand <- palace_file_candidates("CCU_Hitter_Bio.csv")
  if (length(cand) == 0) stop("CCU_Hitter_Bio.csv not found")
  cat("Hitter bio source (local):", normalizePath(cand[1]), "\n")
  readr::read_csv(cand[1], show_col_types = FALSE)
}, error = function(e) {
  cat("WARNING: CCU_Hitter_Bio.csv not found -", conditionMessage(e), "\n")
  NULL
})

# ============================================================
# ccu_2027_roster.csv — the 2027 roster (Fall 2026), in the repo.
# Drives the Roster landing cards for BOTH pitchers and hitters.
# ============================================================
roster27 <- tryCatch({
  r <- readr::read_csv("reference/ccu_2027_roster.csv", show_col_types = FALSE)
  names(r) <- trimws(names(r))
  r <- r %>% dplyr::mutate(dplyr::across(dplyr::where(is.character), trimws)) %>%
    dplyr::filter(!is.na(Name), nzchar(Name))
  first <- sub("\\s.*$", "", r$Name)
  last  <- sub("^\\S+\\s+", "", r$Name)
  cls   <- c("1" = "Fr.", "2" = "So.", "3" = "Jr.", "4" = "Sr.",
             "5" = "Gr.")[as.character(r$Year)]
  r$First      <- first
  r$Last       <- last
  r$Name_LF    <- paste0(last, ", ", first)
  r$Class      <- ifelse(is.na(cls), as.character(r$Year), cls)
  r$IsPitcher  <- grepl("HP|^P$|/P\\b", r$Pos)
  cat("[Roster27] loaded:", nrow(r), "players |",
      sum(r$IsPitcher), "pitchers |", sum(!r$IsPitcher), "hitters\n")
  r
}, error = function(e) {
  cat("WARNING: ccu_2027_roster.csv not found -", conditionMessage(e), "\n")
  NULL
})
roster27_pitcher_fl <- if (!is.null(roster27)) roster27$Name[roster27$IsPitcher] else character(0)

# Distinct bullpen pitchers from Supabase (Last, First -> First Last), so the
# global search knows every arm that has thrown a pen — freshmen included.
# A zero-row game frame WITH the full game schema. Used for seasons that
# have no game data (Fall 2026) and for "no season selected": downstream
# filters like safe_as_date(Date) need the columns to exist, otherwise the
# data mask escapes to the environment and finds lubridate::Date (a
# function), crashing with "cannot coerce type 'closure'".
empty_game_frame <- function() {
  for (nm in c("spring26", "prespring", "fall25", "data")) {
    if (exists(nm, inherits = TRUE)) {
      o <- get(nm, inherits = TRUE)
      if (is.data.frame(o) && ncol(o) > 0) return(o[0, , drop = FALSE])
    }
  }
  data.frame()
}

# Resolve a display name against a pool of known names, tolerant of
# order ("Chris Billingsley" vs "Billingsley, Chris"), punctuation, and
# suffixes ("jr."), using the module's token-sorted normalizer.
resolve_pitcher_name <- function(x, pool) {
  if (is.null(x) || !nzchar(x)) return(NULL)
  pool <- unname(as.character(pool)); pool <- pool[nzchar(pool)]
  if (x %in% pool) return(x)
  hit <- pool[bp_norm_name(pool) == bp_norm_name(x)]
  if (length(hit)) hit[1] else NULL
}

bullpen_pitchers_fl <- tryCatch({
  bp <- bullpen_pitches()
  nm <- if (is.null(bp)) character(0) else unique(as.character(bp$Pitcher))
  nm <- bp_fix_names(nm)
  sort(unique(sub("^\\s*([^,]+),\\s*(.+)\\s*$", "\\2 \\1", nm)))
}, error = function(e) {
  cat("WARNING: could not list bullpen pitchers -", conditionMessage(e), "\n")
  character(0)
})

if (!is.null(hitter_bio)) {
  roster_col <- intersect(c("2026 Roster", "2026_Roster", "Roster_2026"),
                          names(hitter_bio))[1]
  hitter_bio <- hitter_bio %>%
    mutate(
      Batter_FL   = gsub("^(.*),\\s*(.*)$", "\\2 \\1", Batter),
      Roster_2026 = if (!is.na(roster_col)) .data[[roster_col]] else "Yes"
    )
  cat("Hitter bio loaded:", nrow(hitter_bio), "rows |",
      sum(toupper(trimws(as.character(hitter_bio$Roster_2026))) %in%
            c("YES","Y","TRUE","1")), "on 2026 roster\n")
}
