# Pro player header (roster-page style)
# Ringed team logo | "#N Player Name" | info card with uppercase
# labels over bold values. Coastal teal ring, peru number.
# ============================================================
HDR_TEAL <- "#006F71"
HDR_PERU <- "peru"

pro_player_header <- function(name, number = "", fields = list(),
                              logo_grob = grid::nullGrob(),
                              accent = HDR_TEAL, number_col = HDR_PERU) {
  cy      <- grid::unit(1, "npc") - grid::unit(58, "pt")
  logo_cx <- grid::unit(66, "pt")

  ring <- grid::circleGrob(
    x = logo_cx, y = cy, r = grid::unit(50, "pt"),
    gp = grid::gpar(fill = "white", col = accent, lwd = 5.5))
  logo <- grid::grobTree(
    logo_grob,
    vp = grid::viewport(x = logo_cx, y = cy,
                        width = grid::unit(72, "pt"),
                        height = grid::unit(72, "pt")))

  has_num <- !is.null(number) && !is.na(number) && nzchar(as.character(number))
  num_g <- grid::textGrob(
    if (has_num) paste0("#", number) else "",
    x = grid::unit(134, "pt"), y = cy, just = "left",
    gp = grid::gpar(fontface = "bold", cex = 2.6, col = number_col))
  name_x <- grid::unit(134, "pt") +
    if (has_num) grid::grobWidth(num_g) + grid::unit(11, "pt") else grid::unit(0, "pt")
  name_g <- grid::textGrob(
    name, x = name_x, y = cy, just = "left",
    gp = grid::gpar(fontface = "bold", cex = 2.6, col = "#1A1A1A"))

  # ---- info card ----
  card_cy <- grid::unit(45, "pt")
  card <- grid::roundrectGrob(
    x = 0.5, y = card_cy,
    width = grid::unit(1, "npc") - grid::unit(20, "pt"),
    height = grid::unit(82, "pt"), r = grid::unit(9, "pt"),
    gp = grid::gpar(fill = "#FFFFFF", col = "#E2E8F0", lwd = 1.4))

  gl <- list(ring, logo, num_g, name_g, card)
  if (length(fields) > 0) {
    labs <- vapply(fields, function(f) f$label, character(1))
    vals <- vapply(fields, function(f) f$value, character(1))
    w    <- vapply(fields, function(f) if (is.null(f$w)) 1 else f$w, numeric(1))
    left <- 0.035; right <- 0.975
    xs <- left + (right - left) * c(0, cumsum(w)[-length(w)]) / sum(w)
    for (i in seq_along(fields)) {
      gl[[length(gl) + 1]] <- grid::textGrob(
        toupper(labs[i]), x = xs[i], y = card_cy + grid::unit(15, "pt"),
        just = "left",
        gp = grid::gpar(cex = 0.85, fontface = "bold", col = "#6B7280"))
      gl[[length(gl) + 1]] <- grid::textGrob(
        vals[i], x = xs[i], y = card_cy - grid::unit(11, "pt"),
        just = "left",
        gp = grid::gpar(cex = 1.35, fontface = "bold", col = "#111827"))
    }
  }
  grid::grobTree(children = do.call(grid::gList, gl))
}

# Coastal logo for the CCU header (DRS row if present, static fallback)
ccu_logo_url <- local({
  u <- "https://www.ncaa.com/sites/default/files/images/logos/schools/bgl/coastal-caro.svg"
  if (!is.null(drs_bio)) {
    r <- drs_bio[!is.na(drs_bio$team_name) &
                 drs_bio$team_name == "Coastal Carolina", , drop = FALSE]
    if (nrow(r) > 0 && !is.na(r$team_logo[1]) && nzchar(r$team_logo[1]))
      u <- r$team_logo[1]
  }
  u
})

# ============================================================
# NCAA player header (DRS26.csv bio, team-aware lookup)
# ============================================================
create_ncaa_player_header <- function(player, pdata = NULL) {
  team_hint <- tm_current_team(player)
  r <- drs_lookup(player, team_hint)

  val_or_na <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x) || x == "") "N/A" else as.character(x)
  }

  pos_txt   <- if (!is.null(r)) val_or_na(r$Position) else "P"
  class_txt <- if (!is.null(r)) val_or_na(r$Class) else "N/A"
  home_txt  <- if (!is.null(r)) val_or_na(r$Hometown) else "N/A"
  ht_txt    <- if (!is.null(r)) val_or_na(r$Height) else "N/A"
  wt_txt    <- if (!is.null(r) && !is.na(r$Weight) && r$Weight != "")
    paste0(r$Weight, " lbs") else "N/A"
  age_txt   <- if (!is.null(r) && !is.na(suppressWarnings(as.numeric(r$Age))) &&
                   suppressWarnings(as.numeric(r$Age)) > 0)
    as.character(r$Age) else "N/A"
  jersey_txt <- if (!is.null(r) && "Jersey" %in% names(r) &&
                    !is.na(r$Jersey) && nzchar(as.character(r$Jersey)))
    as.character(r$Jersey) else ""

  throws_txt <- "N/A"
  if (!is.null(pdata) && is.data.frame(pdata) && nrow(pdata) > 0 &&
      "PitcherThrows" %in% names(pdata)) {
    th <- pdata$PitcherThrows[!is.na(pdata$PitcherThrows)]
    if (length(th) > 0) throws_txt <- th[1]
  }

  logo_g <- if (!is.null(r)) cached_image_grob(r$team_logo, blank_on_fail = FALSE)
            else grid::nullGrob()

  pro_player_header(
    name    = player,
    number  = jersey_txt,
    logo_grob = logo_g,
    fields  = list(
      list(label = "Position", value = pos_txt),
      list(label = "Weight",   value = wt_txt),
      list(label = "Height",   value = ht_txt),
      list(label = "Year",     value = class_txt, w = 0.85),
      list(label = "Hometown", value = home_txt,  w = 2.3),
      list(label = "Age",      value = age_txt,   w = 0.7),
      list(label = "Throws",   value = throws_txt)
    )
  )
}

# ============================================================
# HITTER BIO HELPERS — headers, positions, unified search
# ============================================================
is_coastal_hitter <- function(nm) {
  !is.null(hitter_bio) && !is.null(nm) && length(nm) == 1 && nzchar(nm) &&
    (nm %in% hitter_bio$Batter_FL || nm %in% hitter_bio$Batter)
}

ccu_hitter_bio_row <- function(nm) {
  if (is.null(hitter_bio) || is.null(nm) || !nzchar(nm)) return(NULL)
  r <- hitter_bio[hitter_bio$Batter_FL == nm | hitter_bio$Batter == nm, ,
                  drop = FALSE]
  if (nrow(r) == 0) NULL else r[1, ]
}

# Position string for a hitter: CCU bio first, DRS26 fallback.
hitter_pos_string <- function(nm) {
  r <- ccu_hitter_bio_row(nm)
  if (!is.null(r) && "Position" %in% names(r) &&
      !is.na(r$Position) && nzchar(as.character(r$Position))) {
    return(as.character(r$Position))
  }
  team <- tryCatch(tm_current_batter_team(nm), error = function(e) NULL)
  d <- tryCatch(drs_lookup(nm, team), error = function(e) NULL)
  if (!is.null(d) && "Position" %in% names(d) && !is.na(d$Position) &&
      nzchar(as.character(d$Position))) {
    return(as.character(d$Position))
  }
  ""
}

# "1B/OF" -> which defense tabs a hitter earns. LHP/DH etc. earn none.
classify_hitter_pos <- function(pos) {
  toks <- toupper(trimws(unlist(strsplit(as.character(pos %||% ""),
                                         "[/,;[:space:]]+"))))
  list(
    inf = any(toks %in% c("INF", "IF", "1B", "2B", "3B", "SS", "MIF", "CIF")),
    of  = any(toks %in% c("OF", "LF", "CF", "RF")),
    c   = any(toks %in% c("C", "CA"))
  )
}

# Coastal hitter header — same layout/fields as the pitcher header.
create_hitter_player_header <- function(player) {
  row <- ccu_hitter_bio_row(player)
  if (is.null(row)) {
    return(grid::textGrob(paste("No bio found for", player),
                          gp = grid::gpar(fontface = "bold", cex = 1.2)))
  }
  val_or_na <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x) || x == "") "N/A" else as.character(x)
  }
  hascol <- function(nm) nm %in% names(row)

  position   <- val_or_na(if (hascol("Position")) row$Position else NA)
  class_txt  <- if (hascol("Class"))    val_or_na(row$Class)    else "N/A"
  home_txt   <- if (hascol("Hometown")) val_or_na(row$Hometown) else "N/A"
  bats_txt   <- if (hascol("Bats"))     val_or_na(row$Bats)     else "N/A"
  throws_txt <- if (hascol("Throws"))   val_or_na(row$Throws)   else "N/A"
  ht_txt     <- if (hascol("Height"))   val_or_na(row$Height)   else "N/A"
  wt_txt     <- if (hascol("Weight")) {
    if (is.na(row$Weight) || row$Weight == "") "N/A" else paste0(row$Weight, " lbs")
  } else "N/A"
  birth_txt  <- if (hascol("Birthday")) val_or_na(row$Birthday) else "N/A"

  bt_txt <- if (bats_txt != "N/A" && throws_txt != "N/A") {
    paste0(substr(bats_txt, 1, 1), "-", substr(throws_txt, 1, 1))
  } else if (bats_txt != "N/A") paste0("B: ", substr(bats_txt, 1, 1)) else "N/A"

  bio_num <- if (hascol("Number") && !is.na(row$Number) && row$Number != "")
    as.character(row$Number) else ""
  num_clean <- get_jersey_number(if (!is.na(row$Batter_FL)) row$Batter_FL else player,
                                 "Coastal Carolina", fallback = bio_num)

  pro_player_header(
    name      = if (!is.na(row$Batter_FL) && nzchar(row$Batter_FL)) row$Batter_FL else player,
    number    = num_clean,
    logo_grob = cached_image_grob(ccu_logo_url, blank_on_fail = FALSE),
    fields  = list(
      list(label = "Position", value = position),
      list(label = "Weight",   value = wt_txt),
      list(label = "Height",   value = ht_txt),
      list(label = "Year",     value = class_txt, w = 0.85),
      list(label = "Hometown", value = home_txt,  w = 2.3),
      list(label = "Born",     value = birth_txt, w = 1.2),
      list(label = "B-T",      value = bt_txt,    w = 0.7)
    )
  )
}

# NCAA hitter header — DRS26 bio, Bats side inferred from pitch data.
create_ncaa_hitter_header <- function(player, bdata = NULL) {
  team_hint <- tryCatch(tm_current_batter_team(player), error = function(e) NULL)
  r <- tryCatch(drs_lookup(player, team_hint), error = function(e) NULL)

  val_or_na <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x) || x == "") "N/A" else as.character(x)
  }
  pos_txt   <- if (!is.null(r)) val_or_na(r$Position) else "N/A"
  class_txt <- if (!is.null(r)) val_or_na(r$Class) else "N/A"
  home_txt  <- if (!is.null(r)) val_or_na(r$Hometown) else "N/A"
  ht_txt    <- if (!is.null(r)) val_or_na(r$Height) else "N/A"
  wt_txt    <- if (!is.null(r) && !is.na(r$Weight) && r$Weight != "")
    paste0(r$Weight, " lbs") else "N/A"
  age_txt   <- if (!is.null(r) && !is.na(suppressWarnings(as.numeric(r$Age))) &&
                   suppressWarnings(as.numeric(r$Age)) > 0)
    as.character(r$Age) else "N/A"
  jersey_txt <- if (!is.null(r) && "Jersey" %in% names(r) &&
                    !is.na(r$Jersey) && nzchar(as.character(r$Jersey)))
    as.character(r$Jersey) else ""

  bats_txt <- "N/A"
  if (!is.null(bdata) && is.data.frame(bdata) && nrow(bdata) > 0 &&
      "BatterSide" %in% names(bdata)) {
    bs <- names(sort(table(as.character(bdata$BatterSide)), decreasing = TRUE))
    if (length(bs) > 0 && nzchar(bs[1])) bats_txt <- bs[1]
  }

  logo_g <- if (!is.null(r)) cached_image_grob(r$team_logo, blank_on_fail = FALSE)
            else grid::nullGrob()

  pro_player_header(
    name    = player,
    number  = jersey_txt,
    logo_grob = logo_g,
    fields  = list(
      list(label = "Position", value = pos_txt),
      list(label = "Weight",   value = wt_txt),
      list(label = "Height",   value = ht_txt),
      list(label = "Year",     value = class_txt, w = 0.85),
      list(label = "Hometown", value = home_txt,  w = 2.3),
      list(label = "Age",      value = age_txt,   w = 0.7),
      list(label = "Bats",     value = bats_txt)
    )
  )
}

# Hitter block for the unified header search: Coastal bio hitters first,
# then every NCAA batter labeled "Name — Team (Hitter)". Values carry an
# "HB::" prefix so pitcher-side consumers of input$global_pitcher can
# tell them apart (and are guarded against them).
HB_PREFIX <- "HB::"
is_hitter_pick <- function(v) {
  !is.null(v) && length(v) == 1 && is.character(v) && startsWith(v, HB_PREFIX)
}
build_global_hitter_choices <- function(src_years = NULL) {
  cc <- character(0)
  if (!is.null(hitter_bio)) {
    hn <- unique(as.character(hitter_bio$Batter_FL))
    hn <- hn[!is.na(hn) & nzchar(hn)]
    cc <- setNames(paste0(HB_PREFIX, hn),
                   paste0(hn, " \u2014 Coastal (Hitter)"))
  }
  dir <- tryCatch(ncaa_batter_directory(), error = function(e) NULL)
  if (is.null(dir) || nrow(dir) == 0) return(cc)
  # keep only bats that appear in a selected season's source year(s)
  if (!is.null(src_years) && length(src_years) > 0 && "srcs" %in% names(dir)) {
    keep <- vapply(dir$srcs, function(s) {
      s <- as.character(s)
      length(s) == 0 || any(s %in% src_years)
    }, logical(1))
    dir <- dir[keep, , drop = FALSE]
    if (nrow(dir) == 0) return(cc)
  }
  disp  <- as.character(dir$display)
  teams <- as.character(dir$teams); teams[is.na(teams)] <- ""
  seen  <- if (!is.null(hitter_bio)) unique(hitter_bio$Batter_FL) else character(0)
  ok <- !is.na(disp) & nzchar(disp) & !(disp %in% seen)
  nc <- setNames(paste0(HB_PREFIX, disp[ok]),
                 paste0(disp[ok],
                        ifelse(nzchar(teams[ok]), paste0(" \u2014 ", teams[ok]), ""),
                        " (Hitter)"))
  c(cc, nc)
}

# Season -> calendar window for the hitter's pitch data. The NCAA frames
# span both source years, so the app-level season pick maps onto date
# windows: Spring25 = through Jul '25, Fall25 = Aug-Dec '25,
# PreSpring26 = Jan-mid Feb '26, Spring26 = Feb 13 '26 onward.
hp_season_filter <- function(df, seasons) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 ||
      !"Date" %in% names(df)) return(df)
  seasons <- as.character(seasons)
  seasons <- seasons[!is.na(seasons) & nzchar(seasons)]
  if (length(seasons) == 0) return(df)   # nothing picked = everything
  dt <- safe_as_date(df$Date)
  keep <- rep(FALSE, nrow(df))
  if ("Spring25" %in% seasons)
    keep <- keep | (!is.na(dt) & dt <= as.Date("2025-07-31"))
  if ("Fall25" %in% seasons)
    keep <- keep | (!is.na(dt) & dt >= as.Date("2025-08-01") &
                      dt <= as.Date("2025-12-31"))
  if ("PreSpring26" %in% seasons)
    keep <- keep | (!is.na(dt) & dt >= as.Date("2026-01-01") &
                      dt <= as.Date("2026-02-12"))
  if ("Spring26" %in% seasons)
    keep <- keep | (!is.na(dt) & dt >= as.Date("2026-02-13"))
  df[keep, , drop = FALSE]
}

# Clickable player-name span for DT tables / raw HTML (routes through the
# tp-player-link JS handler -> input$player_link_click).
tp_player_link <- function(name, side = "pitcher") {
  nm <- htmltools::htmlEscape(as.character(name), attribute = TRUE)
  sprintf("<span class='tp-player-link' data-side='%s' data-name='%s'>%s</span>",
          side, nm, htmltools::htmlEscape(as.character(name)))
}

# ---- Traditional-stat formatting for the Overview strips ----
# Rate stats round to their conventional precision (.312 / 3.41 / 1.28);
# counting stats stay whole numbers.
.tp_fmt_stat <- function(ab, v) {
  x <- suppressWarnings(as.numeric(v))
  if (!is.finite(x)) return(as.character(v))
  ab <- toupper(ab)
  if (ab %in% c("AVG", "BA", "OBP", "SLG", "OPS"))
    return(sub("^0\\.", ".", sprintf("%.3f", x)))
  if (ab %in% c("ERA", "WHIP")) return(sprintf("%.2f", x))
  if (ab == "IP") return(sprintf("%.1f", x))
  as.character(round(x))
}

# Conditional color for rate stats on the D1 scale: green = good,
# red = bad, gray = average. Counting stats keep the teal.
.tp_stat_color <- function(ab, v, side = c("bat", "pit")) {
  side <- match.arg(side)
  x <- suppressWarnings(as.numeric(v))
  if (!is.finite(x)) return("#0E6E70")
  rng <- switch(paste0(side, ":", toupper(ab)),
    "bat:AVG"  = c(.240, .340,  1),
    "bat:BA"   = c(.240, .340,  1),
    "bat:OBP"  = c(.320, .440,  1),
    "bat:SLG"  = c(.380, .560,  1),
    "bat:OPS"  = c(.700, 1.000, 1),
    "pit:ERA"  = c(2.50, 6.50, -1),
    "pit:WHIP" = c(1.00, 1.80, -1),
    "pit:BA"   = c(.200, .300, -1),
    "pit:AVG"  = c(.200, .300, -1),
    NULL)
  if (is.null(rng)) return("#0E6E70")
  t <- (x - rng[1]) / (rng[2] - rng[1])
  t <- max(0, min(1, t))
  if (rng[3] < 0) t <- 1 - t
  pal <- scales::gradient_n_pal(c("#E1463E", "#8A8F98", "#00840D"),
                                values = c(0, 0.5, 1))
  pal(t)
}

# ---- Global image-grob cache (headshots, logos) ----
# Header renders were re-downloading the same images on every pitcher
# switch; cache the rasterGrob per URL for the life of the process.
.img_grob_cache <- new.env(parent = emptyenv())

# Cross-session cache for the grade pool distribution (pool data is fixed
# at startup, so the per-arm aggregation only ever needs to run once per
# pool choice for the life of the process, not once per user session).
.grade_dist_cache <- new.env(parent = emptyenv())
cached_image_grob <- function(path_or_url, blank_on_fail = TRUE) {
  if (is.null(path_or_url) || length(path_or_url) == 0 ||
      is.na(path_or_url) || path_or_url == "") return(grid::nullGrob())
  key <- gsub("[^A-Za-z0-9]", "_", as.character(path_or_url))
  g <- .img_grob_cache[[key]]
  if (!is.null(g)) return(g)
  im <- try(image_read(path_or_url), silent = TRUE)
  if (inherits(im, "try-error")) {
    if (!blank_on_fail) return(grid::nullGrob())
    im <- image_blank(400, 400, color = "white")
  }
  g <- grid::rasterGrob(as.raster(im), interpolate = TRUE)
  .img_grob_cache[[key]] <- g
  g
}

# Persistent scouting-notes store (survives restarts on /data)
scout_notes_path <- function() {
  d <- if (dir.exists("/data")) "/data" else tempdir()
  file.path(d, "scouting_notes.rds")
}

# ============================================================
