# PALACE BRAND MARKS
# Inlined as SVG strings rather than hosted files: the Space has no reliable
# www/ mount, and each mark is under a kilobyte, so inlining removes a whole
# class of broken-image failure. Palette: teal #006F71, signal yellow #E8F04A,
# bronze #A27752, deep teal #05393B.
# ============================================================================

# Rounded-square signal mark (the app icon).
PALACE_SIGNAL_MARK <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 200 200" fill="none" role="img" aria-label="Palace">
  <rect x="16" y="16" width="168" height="168" rx="44" fill="#006F71"></rect>
  <path d="M42 143 L74 111 L100 131 L150 65" stroke="#E8F04A" stroke-width="16" stroke-linecap="round" stroke-linejoin="round"></path>
  <circle cx="150" cy="65" r="17" fill="#A27752"></circle>
</svg>'

# Palmetto-state mark: South Carolina silhouette carrying the signal line.
PALACE_STATE_MARK <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 200 172" fill="none" role="img" aria-label="Palace">
  <path d="M6 12 C40 6 70 3 96 2 L100 18 L152 20 L196 72 C170 108 140 138 104 166 C86 130 44 72 6 12 Z" fill="#006F71"></path>
  <path d="M74 96 L94 74 L110 88 L148 44" stroke="#E8F04A" stroke-width="9" stroke-linecap="round" stroke-linejoin="round"></path>
  <circle cx="148" cy="44" r="10" fill="#A27752"></circle>
</svg>'

# Palmetto + waves accent mark.
PALACE_PALMETTO_MARK <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 200 200" fill="none" role="img" aria-label="Palmetto">
  <path d="M94 172 C94 140 92 112 98 80 L112 80 C114 112 108 142 110 172 Z" fill="#006F71"></path>
  <path d="M100 86 C78 62 52 52 30 54 C50 58 72 72 88 92 Z" fill="#006F71"></path>
  <path d="M100 86 C124 60 152 50 174 54 C152 58 128 72 112 92 Z" fill="#006F71"></path>
  <path d="M100 84 C86 54 66 34 44 26 C64 42 80 62 92 90 Z" fill="#006F71"></path>
  <path d="M100 84 C116 54 138 34 158 26 C138 44 122 62 110 90 Z" fill="#006F71"></path>
  <path d="M99 82 C95 54 97 32 104 12 C112 34 110 58 107 82 Z" fill="#006F71"></path>
  <polyline points="28,184 66,166 134,166 172,184" stroke="#A27752" stroke-width="16" stroke-linejoin="round" stroke-linecap="round"></polyline>
</svg>'

# Horizontal lockup. `theme = "light"` for white/light backgrounds, "dark" for
# the bronze header and the photo panel. `mark` picks the glyph, `sub` the
# kicker line under the wordmark.
palace_lockup <- function(theme = c("light", "dark"),
                          mark = c("state", "signal"),
                          sub  = "COASTAL CAROLINA BASEBALL") {
  theme <- match.arg(theme); mark <- match.arg(mark)
  word_fill <- if (theme == "dark") "#FFFFFF" else "#05393B"
  sub_fill  <- if (theme == "dark") "#F2E4D6" else "#006F71"
  glyph <- if (mark == "state") {
    paste0('<g transform="translate(2,16) scale(0.40)">',
           '<path d="M6 12 C40 6 70 3 96 2 L100 18 L152 20 L196 72 C170 108 140 138 104 166 ',
           'C86 130 44 72 6 12 Z" fill="',
           if (theme == "dark") "#0E8B8D" else "#006F71", '"></path>',
           '<path d="M74 96 L94 74 L110 88 L148 44" stroke="#E8F04A" stroke-width="9" ',
           'stroke-linecap="round" stroke-linejoin="round"></path>',
           '<circle cx="148" cy="44" r="10" fill="#A27752"></circle></g>')
  } else {
    paste0('<g transform="translate(2,8) scale(0.44)">',
           '<rect x="24" y="24" width="152" height="152" rx="40" fill="',
           if (theme == "dark") "#0E8B8D" else "#006F71", '"></rect>',
           '<path d="M42 143 L74 111 L100 131 L150 65" stroke="#E8F04A" stroke-width="10" ',
           'stroke-linecap="round" stroke-linejoin="round"></path>',
           '<circle cx="150" cy="65" r="13" fill="#A27752"></circle></g>')
  }
  paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 430 100" fill="none" ',
    'role="img" aria-label="Palace">', glyph,
    '<text x="98" y="52" font-family="Space Grotesk, Helvetica Neue, Helvetica, Arial, sans-serif" ',
    'font-size="40" font-weight="700" letter-spacing="-1.2" fill="', word_fill, '">Palace</text>',
    '<text x="100" y="73" font-family="IBM Plex Mono, SFMono-Regular, Menlo, monospace" ',
    'font-size="10.5" letter-spacing="2.4" fill="', sub_fill, '">', sub, '</text>',
    '</svg>')
}

# Favicon as a data URI so no file has to be served.
PALACE_FAVICON_URI <- paste0(
  "data:image/svg+xml;base64,",
  jsonlite::base64_enc(charToRaw(gsub("\n\\s*", "", PALACE_SIGNAL_MARK))))


login_ui <- fluidPage(
  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = PALACE_FAVICON_URI),
    tags$title("Palace \u2014 Coastal Carolina Baseball"),
    tags$style(HTML("
    :root {
      --white: #ffffff;
      --teal: #006F71;
      --teal-light: #0E8B8D;
      --teal-navy: #05393B;
      --bronze: #A27752;
      --signal: #E8F04A;
      --gray-300: #D8DFE2;
      --gray-500: #6B7A80;
      --gray-700: #3B4B52;
      --radius-md: 10px;
      --shadow-md: 0 6px 18px rgba(5,57,59,.14);
      --transition-fast: .16s ease;
    }
    html, body { height: 100%; margin: 0; }
    .container-fluid { padding: 0 !important; }

    .login-container {
      display: flex; min-height: 100vh;
      font-family: 'Space Grotesk', -apple-system, BlinkMacSystemFont,
                   'Helvetica Neue', Helvetica, Arial, sans-serif;
      color: var(--teal-navy);
      background: var(--teal-navy);
    }

    /* ---------------- Left: photo panel ---------------- */
    .login-image { position: relative; flex: 1; display: none; overflow: hidden; }
    @media (min-width: 900px) { .login-image { display: block; } }
    .login-image img { width: 100%; height: 100%; object-fit: cover; }
    .login-image::after {
      content: '';
      position: absolute; inset: 0;
      background:
        radial-gradient(120% 90% at 12% 100%, rgba(0,111,113,.30) 0%, rgba(0,111,113,0) 60%),
        linear-gradient(140deg, rgba(5,57,59,.72) 0%, rgba(5,57,59,.93) 100%);
    }
    .login-image-content {
      position: absolute; bottom: 56px; left: 56px; right: 56px;
      z-index: 1; color: var(--white);
    }
    .login-image-lockup { width: 320px; max-width: 70%; margin-bottom: 10px; }
    .login-image-lockup svg { width: 100%; height: auto; display: block; }
    .login-image-meta {
      display: flex; align-items: center; gap: 10px;
      margin-top: 6px; font-family: 'IBM Plex Mono', Menlo, monospace;
      font-size: 10.5px; letter-spacing: 2.2px;
      color: rgba(255,255,255,.55); text-transform: uppercase;
    }
    .login-image-meta svg { width: 20px; height: 20px; opacity: .85; }

    /* ---------------- Right: form panel ---------------- */
    .login-form-side {
      flex: 0 0 484px;
      display: flex; flex-direction: column; justify-content: center;
      padding: 48px; background: var(--white);
    }
    @media (max-width: 899px) {
      .login-form-side {
        flex: 1;
        background-image: linear-gradient(rgba(255,255,255,.94), rgba(255,255,255,.94)),
                          url('https://i.imgur.com/NrA0b0X.jpeg');
        background-size: cover; background-position: center;
      }
    }
    .login-form-wrapper { max-width: 348px; margin: 0 auto; width: 100%; }
    .login-logo { margin: 0 auto 30px; width: 292px; max-width: 100%; }
    .login-logo svg { width: 100%; height: auto; display: block; }

    .login-form-wrapper h2 {
      font-size: 23px; font-weight: 700; color: var(--teal-navy);
      margin: 0 0 6px; text-align: center; letter-spacing: -.3px;
    }
    .login-form-wrapper .subtitle {
      font-size: 14px; color: var(--gray-500);
      margin: 0 0 30px; text-align: center;
    }
    .login-form-wrapper .form-group { margin-bottom: 18px; }
    .login-form-wrapper .control-label {
      display: block; font-size: 11px; font-weight: 700;
      color: var(--gray-500) !important; margin-bottom: 7px;
      text-transform: uppercase !important; letter-spacing: .09em !important;
    }
    .login-form-wrapper .form-control {
      width: 100%; padding: 13px 15px !important;
      border: 1.5px solid var(--gray-300) !important;
      border-radius: var(--radius-md) !important;
      font-size: 15px !important; font-family: inherit !important;
      background: #FBFCFC !important; color: var(--teal-navy) !important;
      transition: all var(--transition-fast) !important;
    }
    .login-form-wrapper .form-control:focus {
      border-color: var(--teal) !important; background: var(--white) !important;
      box-shadow: 0 0 0 3px rgba(0,111,113,.13) !important; outline: none !important;
    }
    .login-form-wrapper .btn {
      width: 100%; padding: 14px 20px !important; margin-top: 10px;
      font-family: inherit !important; font-size: 15px !important;
      font-weight: 600 !important; letter-spacing: .01em;
      background: var(--teal) !important; border: none !important;
      border-radius: var(--radius-md) !important; color: var(--white) !important;
      cursor: pointer; transition: all var(--transition-fast) !important;
    }
    .login-form-wrapper .btn:hover {
      background: var(--teal-light) !important;
      transform: translateY(-1px); box-shadow: var(--shadow-md);
    }
    .login-form-wrapper .btn:active { transform: translateY(0); box-shadow: none; }
    #wrong_pass {
      color: #C0392B; font-size: 13.5px; margin-top: 14px;
      text-align: center; font-weight: 600;
    }
    .login-foot {
      margin-top: 30px; text-align: center;
      font-family: 'IBM Plex Mono', Menlo, monospace;
      font-size: 10px; letter-spacing: 1.8px; text-transform: uppercase;
      color: #9AA7AD;
    }
  "))),

  div(class = "login-container",
      # Left: photo, lockup, tagline
      div(class = "login-image",
          tags$img(src = "https://i.imgur.com/NrA0b0X.jpeg",
                   alt = "Coastal Carolina Baseball"),
          div(class = "login-image-content",
              div(class = "login-image-lockup",
                  HTML(palace_lockup("dark", "state", "CONWAY, SOUTH CAROLINA"))),
              div(class = "login-image-meta",
                  HTML(PALACE_PALMETTO_MARK),
                  tags$span("Analytics \u2022 Coastal Carolina"))
          )
      ),
      # Right: lockup + sign in
      div(class = "login-form-side",
          div(class = "login-form-wrapper",
              div(class = "login-logo",
                  HTML(palace_lockup("light", "signal", "COASTAL CAROLINA BASEBALL"))),
              tags$h2("Welcome back"),
              tags$p(class = "subtitle", "Sign in to open the Palace."),
              passwordInput("password", "Password"),
              actionButton("login", "Sign In", class = "btn"),
              textOutput("wrong_pass"),
              div(class = "login-foot", "Coastal Carolina Baseball \u2022 Analytics")
          )
      )
  )
)


# ============================================================================
# UI helpers: skeleton loaders + inline spinners
# ============================================================================

# Skeleton shaped like a data table.
tp_skeleton_table <- function(rows = 6) {
  div(class = "tp-skel-card",
      div(class = "tp-skel tp-skel-title"),
      lapply(seq_len(rows), function(i)
        div(class = "tp-skel tp-skel-tablerow")))
}

app_ui <- fluidPage(
  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = PALACE_FAVICON_URI),
    tags$title("Palace \u2014 Coastal Carolina Baseball"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleFilters', function(open) {
        if (open) { document.body.classList.add('filters-open'); }
        else { document.body.classList.remove('filters-open'); }
      });
      Shiny.addCustomMessageHandler('setOnRoster', function(on) {
        if (on) { document.body.classList.add('on-roster'); document.body.classList.remove('filters-open'); }
        else { document.body.classList.remove('on-roster'); }
      });
      Shiny.addCustomMessageHandler('hideLoading', function(x) {
        var el = document.getElementById('app-loading-overlay');
        if (!el) return;
        // Linger briefly so the first charts/tables finish painting.
        setTimeout(function() {
          el.style.opacity = '0';
          setTimeout(function(){ el.style.display = 'none'; }, 500);
        }, 1800);
      });
      // Highlight the days the selected pitcher actually pitched in the
      // g_dateRange calendars (teal chips; darker when also selected).
      Shiny.addCustomMessageHandler('setPitchedDates', function(dates) {
        if (!document.getElementById('pitched-day-style')) {
          var st = document.createElement('style');
          st.id = 'pitched-day-style';
          st.innerHTML =
            '.datepicker td.pitched-day{background:#BFE3E3 !important;color:#0B3D3E !important;font-weight:700;border-radius:6px;}' +
            '.datepicker td.pitched-day.active{background:#0E6E70 !important;color:#fff !important;}';
          document.head.appendChild(st);
        }
        window._pitchedDates = {};
        (dates || []).forEach(function(d){ window._pitchedDates[d] = true; });
        $('#g_dateRange input').each(function(){
          var dp = $(this).data('datepicker');
          if (!dp) return;
          dp.o.beforeShowDay = function(date){
            var k = date.getFullYear() + '-' +
                    ('0'+(date.getMonth()+1)).slice(-2) + '-' +
                    ('0'+date.getDate()).slice(-2);
            return window._pitchedDates[k]
              ? {classes: 'pitched-day', tooltip: 'Pitched this day'}
              : {};
          };
          try { if (dp.picker && dp.picker.is(':visible')) dp.update(); } catch(e) {}
        });
      });
      // Scouting workbook: row-level remove buttons
      $(document).on('click', '.swb-remove', function(e) {
        e.preventDefault();
        Shiny.setInputValue('swb_remove_click',
          {name: $(this).attr('data-name'), nonce: Math.random()},
          {priority: 'event'});
      });

      // Matchup matrix: delegated cell clicks -> open detail + scroll
      $(document).on('click', '.mm-cell:not(.mm-na)', function() {
        Shiny.setInputValue('mm_cell_click',
          {p: $(this).attr('data-p'), b: $(this).attr('data-b'),
           nonce: Math.random()}, {priority: 'event'});
      });
      // Matchup+ (beta) matrix: delegated cell clicks -> per-pitch breakdown
      $(document).on('click', '.mmp-cell:not(.mmp-na)', function() {
        Shiny.setInputValue('mmp_cell_click',
          {p: $(this).attr('data-p'), b: $(this).attr('data-b'),
           nonce: Math.random()}, {priority: 'event'});
      });
      Shiny.addCustomMessageHandler('mmpScrollDetail', function(x) {
        var el = document.getElementById('mmp-detail-anchor');
        if (el) el.scrollIntoView({behavior: 'smooth', block: 'start'});
      });

      // Detail table: RV/100 pill click -> load charts for that pitch
      $(document).on('click', '.mm-rv-cell', function() {
        Shiny.setInputValue('mm_rv_click',
          {pt: $(this).attr('data-pt'), nonce: Math.random()},
          {priority: 'event'});
      });
      Shiny.addCustomMessageHandler('mmScrollDetail', function(x) {
        var el = document.getElementById('mm-detail-anchor');
        if (el) el.scrollIntoView({behavior: 'smooth', block: 'start'});
      });

      /* ============================================================
         SLIDING TOGGLE THUMB
         Measures the active .btn in each radio/checkbox button group
         and positions the ::before thumb via CSS custom properties.
         ============================================================ */
      function tpPositionThumb(group, animate) {
        if (!group) return;
        var active = group.querySelector('.btn.active');
        if (!active) {
          group.style.setProperty('--tg-o', '0');
          return;
        }
        // Skip if hidden (offsetWidth 0) - remeasure when shown.
        if (!active.offsetWidth) { group.classList.add('tg-nofx'); return; }
        group.classList.remove('tg-nofx');
        if (!animate) group.classList.add('tg-init');
        group.style.setProperty('--tg-w', active.offsetWidth + 'px');
        group.style.setProperty('--tg-x', active.offsetLeft + 'px');
        group.style.setProperty('--tg-o', '1');
        if (!animate) {
          // force reflow then re-enable transitions
          void group.offsetWidth;
          setTimeout(function(){ group.classList.remove('tg-init'); }, 20);
        }
      }

      function tpToggleGroups() {
        return document.querySelectorAll(
          '.shiny-input-radiogroup .btn-group, .shiny-input-checkboxgroup .btn-group');
      }

      function tpRefreshAllThumbs(animate) {
        tpToggleGroups().forEach(function(g) { tpPositionThumb(g, animate); });
      }

      // Reposition on click (active class flips just after the event)
      $(document).on('click',
        '.shiny-input-radiogroup .btn-group .btn, .shiny-input-checkboxgroup .btn-group .btn',
        function() {
          var g = this.closest('.btn-group');
          // let Bootstrap/Shiny apply .active first
          setTimeout(function(){ tpPositionThumb(g, true); }, 0);
          setTimeout(function(){ tpPositionThumb(g, true); }, 60);
        });

      // Reposition when Shiny re-renders inputs, on resize, and on tab shows
      $(document).on('shiny:value shiny:bound shiny:updateinput', function() {
        setTimeout(function(){ tpRefreshAllThumbs(false); }, 30);
      });
      $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function() {
        setTimeout(function(){ tpRefreshAllThumbs(false); }, 40);
      });
      window.addEventListener('resize', function() { tpRefreshAllThumbs(true); });

      /* ============================================================
         FOCUS MODE — Advance / Reports / Leaderboard / Hitters are
         standalone pages, so the pitcher-detail tabs collapse there.
         The server is the source of truth (tpFocusMode message); the
         DOM listener is only a fallback for direct tab clicks.
         ============================================================ */
      // Pitcher/hitter detail now lives in #player_subtabs, so there are no
      // detail chips left in the main bar.
      var TP_DETAIL_TABS = [];
      var TP_FOCUS_TABS  = [];

      // Tabs hidden on standalone pages, matched by NAME. Previously the
      // CSS targeted these by nth-child, which silently retargeted the
      // wrong tab every time a tabPanel was inserted above them.
      var TP_HIDE_IN_FOCUS = [];

      // Every board is a pill inside Leaderboards now, so nothing in the
      // main bar is hidden.
      var TP_ALWAYS_HIDDEN = [];

      // League leaderboards are self-contained: they carry their own
      // season / team / column controls and read nothing from the global
      // pitcher search, date range or filter drawer. Tagging the body
      // lets the CSS hide that chrome so the two filter systems can't be
      // mistaken for each other.
      var TP_STANDALONE_TABS = ['Leaderboards'];
      function tpTagStandalone() {
        var a = document.querySelector('#main_tabs.nav-tabs > li.active > a');
        var t = a ? (a.textContent || '').trim() : '';
        if (TP_STANDALONE_TABS.indexOf(t) !== -1) {
          document.body.classList.add('on-leaderboard');
          document.body.classList.remove('filters-open');
        } else {
          document.body.classList.remove('on-leaderboard');
        }
      }

      function tpTagDetailTabs() {
        var lis = document.querySelectorAll('#main_tabs.nav-tabs > li');
        Array.prototype.forEach.call(lis, function(li) {
          var a = li.querySelector('a');
          if (!a) return;
          var t = (a.textContent || '').trim();
          if (TP_DETAIL_TABS.indexOf(t) !== -1) li.classList.add('tab-pitcher-detail');
          if (TP_HIDE_IN_FOCUS.indexOf(t) !== -1) li.classList.add('tab-hide-in-focus');
          if (TP_ALWAYS_HIDDEN.indexOf(t) !== -1) li.classList.add('tab-always-hidden');
        });
      }

      function tpSetFocusMode(on) {
        if (on) document.body.classList.add('focus-mode');
        else document.body.classList.remove('focus-mode');
        setTimeout(function(){ tpRefreshAllThumbs(false); }, 40);
      }

      function tpApplyFocusMode() {
        var active = document.querySelector('#main_tabs.nav-tabs > li.active > a');
        var t = active ? (active.textContent || '').trim() : '';
        tpSetFocusMode(TP_FOCUS_TABS.indexOf(t) !== -1);
        tpTagStandalone();
      }

      // Fallback: user clicked a tab directly in the bar.
      $(document).on('shown.bs.tab', '#main_tabs a', function() {
        tpTagDetailTabs();
        tpApplyFocusMode();
      });

      // Authoritative: the server tells us which page is active.
      Shiny.addCustomMessageHandler('tpFocusMode', function(on) {
        tpTagDetailTabs();
        tpSetFocusMode(on === true || on === 'true' || on === 1);
      });

      /* ============================================================
         GLOBAL BUSY BAR + BUTTON SPINNERS
         ============================================================ */
      (function() {
        var bar, pending = 0, timer = null;
        function ensureBar() {
          if (!bar) {
            bar = document.createElement('div');
            bar.id = 'tp-busy-bar';
            document.body.appendChild(bar);
          }
          return bar;
        }
        function start() {
          var b = ensureBar();
          b.classList.add('on');
          b.style.width = '18%';
          clearTimeout(timer);
          timer = setTimeout(function(){ b.style.width = '72%'; }, 320);
        }
        function done() {
          if (!bar) return;
          clearTimeout(timer);
          bar.style.width = '100%';
          setTimeout(function(){
            bar.classList.remove('on');
            setTimeout(function(){ if (bar) bar.style.width = '0'; }, 340);
          }, 200);
        }
        $(document).on('shiny:busy', function() { pending++; if (pending === 1) start(); });
        $(document).on('shiny:idle', function() { pending = 0; done(); });
      })();

      /* ============================================================
         SKELETON HOSTS — swap in a shimmer placeholder while the
         wrapped output is recalculating.
         ============================================================ */
      $(document).on('shiny:recalculating', function(e) {
        var host = e.target && e.target.closest ? e.target.closest('.tp-skel-host') : null;
        if (host) host.classList.add('is-loading');
      });
      $(document).on('shiny:recalculated shiny:value shiny:error', function(e) {
        var host = e.target && e.target.closest ? e.target.closest('.tp-skel-host') : null;
        if (host) setTimeout(function(){ host.classList.remove('is-loading'); }, 60);
      });

      // Spinner state on action buttons while Shiny is busy
      (function() {
        var lastBtn = null;
        $(document).on('click', '.btn.tp-spin-on-click', function() {
          lastBtn = this;
          this.classList.add('tp-busy');
        });
        $(document).on('shiny:idle', function() {
          if (lastBtn) { lastBtn.classList.remove('tp-busy'); lastBtn = null; }
        });
      })();

      /* ============================================================
         PLAYER NAME LINKS — any element with .tp-player-link and
         data-name / data-side routes to that player's overview page
         (pitcher -> pitcher Overview, hitter -> hitter Overview).
         ============================================================ */
      $(document).on('click', '.tp-player-link', function(e) {
        e.preventDefault();
        e.stopPropagation();
        Shiny.setInputValue('player_link_click', {
          name: this.getAttribute('data-name') || '',
          side: this.getAttribute('data-side') || 'pitcher'
        }, {priority: 'event'});
      });

      // Safety: never let the overlay stay up longer than 120s.
      document.addEventListener('DOMContentLoaded', function() {
        document.body.classList.add('on-roster'); // app opens on Roster
        setTimeout(function(){ tpTagDetailTabs(); tpApplyFocusMode(); }, 300);
        setTimeout(function(){ tpRefreshAllThumbs(false); }, 400);
        setTimeout(function(){ tpTagDetailTabs(); tpRefreshAllThumbs(false); }, 1400);
        setTimeout(function() {
          var el = document.getElementById('app-loading-overlay');
          if (el && el.style.display !== 'none') {
            el.style.opacity = '0'; setTimeout(function(){ el.style.display='none'; }, 400);
          }
        }, 120000);
      });
    ")),
    tags$style(HTML("
    body, table, .gt_table {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto,
        Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji',
        'Segoe UI Symbol';
    }
    
    /* Header styling */
    /* The header floats on a 14px margin, so the page background is visible
       above and beside it. Pin it to a neutral rather than inheriting. */
    html, body { background: #F5F7F8 !important; }
    body { color: #243746; }

    .app-header {
      display: flex;
      justify-content: flex-start;
      align-items: center;
      gap: 22px;
      padding: 14px 34px;
      background: #8F6645;
      border-radius: 14px;
      margin: 14px 14px 18px;
      box-shadow: 0 4px 14px rgba(0,0,0,.18);
    }

    /* Left cluster: brand, search, and season sit on ONE row so the
       search bar is vertically aligned with the main nav headers. */
    .header-left {
      display: flex;
      flex-direction: row;
      align-items: center;
      gap: 18px;
      flex: 0 0 auto;
    }
    .header-controls {
      display: flex;
      align-items: center;
      gap: 10px;
    }
    /* Nav is pushed to the far right */
    .header-nav { margin-left: auto; }

    .header-brand {
      display: inline-flex;
      align-items: center;
      text-decoration: none;
    }
    .header-brand-logo {
      width: 224px;
      height: auto;
      filter: drop-shadow(0 1px 2px rgba(0,0,0,.22));
      transition: transform 0.15s ease;
    }
    .header-brand-logo svg { width: 100%; height: auto; display: block; }
    .header-brand:hover .header-brand-logo { transform: scale(1.03); }

    .header-nav {
      display: flex;
      align-items: center;
      gap: 34px;
    }
    .header-nav-link {
      color: #ffffff !important;
      font-size: 19px;
      font-weight: 500;
      text-decoration: none !important;
      letter-spacing: 0.3px;
      padding: 4px 2px;
      border-bottom: 2px solid transparent;
      transition: border-color 0.15s ease, opacity 0.15s ease;
    }
    .header-nav-link:hover {
      color: #ffffff !important;
      border-bottom-color: #0E6E70;
      opacity: 0.92;
    }

    /* Compact player search + season picker in the header */
    #header-player-search { width: 240px; margin: 0; }
    #header-player-search .form-group { margin-bottom: 0; }
    #header-player-search .selectize-input {
      min-height: 34px;
      padding: 6px 12px;
      border-radius: 8px;
      font-size: 13px;
      border: 1px solid rgba(255,255,255,.55);
      box-shadow: 0 1px 3px rgba(0,0,0,.15);
    }
    #header-player-search .selectize-input input::placeholder { color:#9CA3AF; }
    #header-player-search .selectize-dropdown { font-size: 13px; }
    /* Search stays visible on every page, including Roster. */

    /* Clickable player names (leaderboards, matchup matrix, etc.) —
       clicking routes to that player's overview page. */
    .tp-player-link { cursor: pointer; }
    .tp-player-link:hover {
      color: #0E6E70 !important;
      text-decoration: underline;
      text-underline-offset: 2px;
    }
    td .tp-player-link { color: #0E6E70; font-weight: 600; }

    /* Season picker sits inline next to the search */
    #header-season-select { width: 230px; margin: 0; }
    #header-season-select .form-group { margin-bottom: 0; }
    #header-season-select .btn.dropdown-toggle {
      min-height: 34px; height: 34px;
      padding: 6px 12px;
      border-radius: 8px;
      font-size: 13px; font-weight: 600;
      background: #FFFFFF !important;
      color: #33474B !important;
      border: 1px solid rgba(255,255,255,.55) !important;
      box-shadow: 0 1px 3px rgba(0,0,0,.15);
      display: flex; align-items: center;
    }
    #header-season-select .btn.dropdown-toggle:focus {
      outline: 2px solid rgba(255,255,255,.75) !important; outline-offset: 1px;
    }
    #header-season-select .filter-option-inner-inner { color: #33474B; }
    #header-season-select .dropdown-menu { font-size: 13px; z-index: 2700; }

    @media (max-width: 768px) {
      .app-header {
        flex-direction: column;
        align-items: stretch;
        padding: 12px 18px;
        gap: 12px;
      }
      .header-left { flex-direction: column; align-items: center; width: 100%; }
      .header-controls { flex-direction: column; width: 100%; }
      .header-brand-logo { width: 190px; }
      .header-nav { gap: 18px; flex-wrap: wrap; justify-content: center;
                    margin-left: 0; }
      .header-nav-link { font-size: 16px; }
      #header-player-search, #header-season-select { width: 100%; margin: 0; }
    }
    
    /* ================= Primary tab bar (flat, underline-indicator) ========= */
    .nav-tabs {
      border: none !important;
      border-bottom: 1px solid #E3E7E9 !important;
      border-radius: 0;
      padding: 0 4px;
      margin: 18px auto 0;
      max-width: 100%;
      background: transparent;
      box-shadow: none;
      position: relative;
      overflow-x: auto;
      -webkit-overflow-scrolling: touch;
      display: flex;
      justify-content: center;
      align-items: flex-end;
      flex-wrap: nowrap;
      gap: 2px;
    }

    .nav-tabs::-webkit-scrollbar { height: 0; }

    .nav-tabs > li { float: none; margin: 0; }

    .nav-tabs > li > a {
      color: #5B6B70 !important;
      border: none !important;
      border-radius: 0 !important;
      background: transparent !important;
      font-weight: 600;
      font-size: 14px;
      padding: 11px 18px 12px;
      white-space: nowrap;
      letter-spacing: 0.15px;
      position: relative;
      margin: 0 !important;
      transition: color .18s ease;
    }

    /* Sliding underline indicator */
    .nav-tabs > li > a::after {
      content: '';
      position: absolute;
      left: 12px; right: 12px; bottom: -1px;
      height: 2px;
      background: #0E6E70;
      border-radius: 2px 2px 0 0;
      transform: scaleX(0);
      transform-origin: center;
      transition: transform .25s cubic-bezier(.4,0,.2,1);
    }

    .nav-tabs > li > a:hover {
      color: #0E6E70 !important;
      background: transparent !important;
      transform: none;
    }
    .nav-tabs > li > a:hover::after { transform: scaleX(.55); background: #A9C4C4; }

    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      background: transparent !important;
      color: #0B3D3E !important;
      font-weight: 700;
      text-shadow: none;
      box-shadow: none;
      border: none !important;
    }
    .nav-tabs > li.active > a::after { transform: scaleX(1); background: #0E6E70; }

    .nav-tabs > li > a:focus-visible {
      outline: 2px solid #0E6E70;
      outline-offset: -3px;
      border-radius: 4px !important;
    }
    .nav-tabs > li > a:focus { outline: none; }

    .tab-content {
      background: #FFFFFF;
      border-radius: 12px;
      padding: 24px;
      margin-top: 16px;
      box-shadow: 0 1px 3px rgba(16,24,40,.06), 0 1px 2px rgba(16,24,40,.04);
      backdrop-filter: none;
      border: 1px solid #E8ECEE;
      position: relative;
      overflow: visible;
    }
    
    #name { 
      font-size: 10px; 
      font-weight: 500; 
      text-align: right; 
      margin-bottom: 8px;
      color: #6C757D;
      letter-spacing: 0.5px;
    }
    
    h3 {
      color: black;
      font-weight: 600;
      margin-top: 25px;
      margin-bottom: 15px;
      padding-bottom: 8px;
      border-bottom: 2px solid #007BA7;
    }
    
    h4 {
      color: white;
      font-weight: 500;
      margin-top: 20px;
      margin-bottom: 12px;
    }
    
    h1 {
      color: #007BA7;
      font-weight: 700;
      margin-bottom: 20px;
      text-shadow: 1px 1px 2px rgba(0,0,0,0.1);
    }
    
    label {
      font-weight: 500;
      color: peru;
      margin-bottom: 5px;
    }
    
    .plot-title {
      text-align: center;
      font-weight: 600;
      color: #2C3E50;
      margin-bottom: 10px;
    }
    
    .dataTables_wrapper .dataTables_length,
    .dataTables_wrapper .dataTables_filter,
    .dataTables_wrapper .dataTables_info,
    .dataTables_wrapper .dataTables_paginate {
      color: #2C3E50;
    }
    
    thead th {
      background-color: #F8F9FA;
      color: #2C3E50;
      font-weight: 600;
      text-align: center !important;
      padding: 10px !important;
    }
    
    .brand-teal { color: darkcyan; }
    .brand-bronze { color: peru; }

    /* ===== Roster card grid ===== */
    .roster-search-wrap { text-align: center; margin: 6px 0 4px; }
    .roster-search {
      width: 320px; max-width: 80%;
      padding: 11px 16px;
      border: 1.5px solid rgba(14,110,112,.4);
      border-radius: 24px;
      font-size: 15px;
      outline: none;
      transition: border-color .15s ease, box-shadow .15s ease;
    }
    .roster-search:focus {
      border-color: #0E6E70;
      box-shadow: 0 0 0 3px rgba(14,110,112,.12);
    }
    .roster-grid {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(240px, 1fr));
      gap: 22px;
      padding: 18px 24px 40px;
      max-width: 1300px;
      margin: 0 auto;
    }
    .roster-card {
      border: 1.5px solid rgba(14,110,112,.35);
      border-radius: 16px;
      background: #ffffff;
      cursor: pointer;
      transition: transform .15s ease, box-shadow .15s ease, border-color .15s ease;
      overflow: hidden;
      display: flex;
      flex-direction: column;
      box-shadow: 0 1px 4px rgba(0,0,0,.05);
    }
    .roster-card:hover {
      transform: translateY(-4px);
      box-shadow: 0 8px 22px rgba(14,110,112,.18);
      border-color: #0E6E70;
    }
    .roster-card-imgwrap {
      display: flex;
      justify-content: center;
      padding: 22px 0 8px;
    }
    .roster-card-img {
      width: 132px;
      height: 132px;
      border-radius: 50%;
      object-fit: cover;
      background: #eef2f3;
      border: 3px solid #eef2f3;
    }
    .roster-card-body { padding: 6px 18px 18px; text-align: center; }
    .roster-card-name {
      font-size: 19px; font-weight: 700; color: #0E6E70; line-height: 1.2;
    }
    .roster-card-role {
      font-size: 13px; font-weight: 600; color: #A27752; font-style: italic; margin: 2px 0 8px;
    }
    .roster-card-line { font-size: 13px; color: #2B2F36; margin-top: 2px; }
    .roster-card-btn {
      margin-top: 14px;
      display: inline-block;
      background: #0E6E70; color: #fff;
      padding: 8px 18px; border-radius: 8px;
      font-weight: 600; font-size: 13px;
    }
    .roster-card:hover .roster-card-btn { background: #0a5557; }

    /* Global control bar */
    .global-control-bar {
      background: #F7FAFA;
      border: 1px solid rgba(0,139,139,.18);
      border-radius: 10px;
      padding: 10px 16px 4px;
      margin: 0 12px 12px;
      box-shadow: 0 2px 8px rgba(0,139,139,.08);
    }

    /* On the Roster landing page, hide the player select bar and Filters button.
       Filters only become available once a player profile is open. */
    body.on-roster #global-control-wrap { display: none !important; }
    body.on-roster #floating-filter-wrap { display: none !important; }
    /* Leaderboards run on their own controls — hide the global season /
       games bar, the filter drawer trigger and the header player search
       so there is exactly one set of filters in play. */
    body.on-leaderboard #global-control-wrap { display: none !important; }
    body.on-leaderboard #floating-filter-wrap { display: none !important; }
    body.on-leaderboard #filter-sidebar,
    body.on-leaderboard #filter-overlay { display: none !important; }
    body.on-leaderboard #header-player-search { display: none !important; }

    /* ===== Slide-out filter sidebar ===== */
    .header-filter-btn { cursor: pointer; }

    /* Per-chart filter dropdown buttons */
    .sw-dropdown .btn,
    .dropdown .btn-default {
      background: #f4faf9;
      border: 1px solid rgba(14,110,112,.3);
      color: #0E6E70;
    }
    .sw-dropdown .btn:hover { background: #e8f4f3; }

    /* Floating Filters button — fixed, accessible while scrolling */
    .floating-filter-btn {
      position: fixed;
      top: 50%;
      left: 0;
      transform: translateY(-50%);
      z-index: 1030;
    }
    .floating-filter-btn a {
      display: flex;
      align-items: center;
      gap: 8px;
      background: #0E6E70;
      color: #fff !important;
      text-decoration: none !important;
      padding: 14px 16px;
      border-radius: 0 12px 12px 0;
      box-shadow: 2px 2px 12px rgba(0,0,0,.25);
      font-weight: 700;
      font-size: 18px;
      writing-mode: vertical-rl;
      text-orientation: mixed;
      transition: padding .15s ease, background .15s ease;
    }
    .floating-filter-btn a .ffb-label { letter-spacing: 1px; }
    .floating-filter-btn a:hover { background: #0E6E70; padding-right: 22px; }

    /* Startup loading overlay */
    .app-loading-overlay {
      position: fixed;
      top: 0; left: 0; right: 0; bottom: 0;
      background: #0B5557;
      z-index: 3000;
      display: flex;
      align-items: center;
      justify-content: center;
      transition: opacity .4s ease;
    }
    .app-loading-inner { text-align: center; color: #fff; }
    .app-loading-spinner {
      width: 58px; height: 58px;
      margin: 0 auto 22px;
      border: 6px solid rgba(255,255,255,.25);
      border-top-color: #fff;
      border-radius: 50%;
      animation: ffb-spin 0.9s linear infinite;
    }
    @keyframes ffb-spin { to { transform: rotate(360deg); } }
    .app-loading-text { font-size: 26px; font-weight: 700; letter-spacing: .5px; }
    .app-loading-sub { font-size: 16px; opacity: .85; margin-top: 8px; }

    .filter-overlay {
      position: fixed;
      top: 0; left: 0; right: 0; bottom: 0;
      background: rgba(0,0,0,.35);
      opacity: 0;
      visibility: hidden;
      transition: opacity .2s ease, visibility .2s ease;
      z-index: 1040;
    }
    .filter-sidebar {
      position: fixed;
      top: 0; left: 0;
      width: 320px;
      max-width: 86vw;
      height: 100%;
      background: #ffffff;
      box-shadow: 4px 0 24px rgba(0,0,0,.18);
      transform: translateX(-100%);
      transition: transform .22s ease;
      z-index: 1050;
      display: flex;
      flex-direction: column;
    }
    body.filters-open .filter-sidebar { transform: translateX(0); }
    body.filters-open .filter-overlay { opacity: 1; visibility: visible; }
    .filter-sidebar-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 16px 20px;
      background: #0E6E70;
      color: #fff;
    }
    .filter-sidebar-title { font-size: 20px; font-weight: 700; letter-spacing: .3px; }
    .filter-close {
      color: #fff !important; font-size: 20px; font-weight: 700;
      text-decoration: none !important; cursor: pointer; line-height: 1;
    }
    .filter-sidebar-body {
      padding: 18px 20px;
      overflow-y: auto;
      flex: 1;
    }
    .filter-sidebar-body .form-group { margin-bottom: 18px; }
    .filter-sidebar-body label { font-weight: 600; color: #0E6E70; }

    /* More Tools hover menu */
    .more-tools-menu {
      position: relative;
      display: inline-block;
    }
    .more-tools-trigger {
      cursor: pointer;
      font-weight: 700;
      color: #0E6E70;
      padding: 8px 14px;
      border-radius: 8px;
      border: 1px solid rgba(14,110,112,.3);
      background: #ffffff;
      white-space: nowrap;
    }
    .more-tools-trigger:hover { background: #e8f4f3; }
    .more-tools-dropdown {
      display: none;
      position: absolute;
      right: 0;
      top: 100%;
      min-width: 200px;
      background: #ffffff;
      border: 1px solid rgba(14,110,112,.25);
      border-radius: 8px;
      box-shadow: 0 6px 20px rgba(0,0,0,.12);
      z-index: 1000;
      padding: 6px 0;
    }
    .more-tools-menu:hover .more-tools-dropdown { display: block; }
    .more-tools-dropdown a {
      display: block;
      padding: 9px 16px;
      color: #2C3E50 !important;
      text-decoration: none;
      font-weight: 600;
    }
    .more-tools-dropdown a:hover {
      background: #0E6E70;
      color: #ffffff !important;
    }

    /* Hide the tool panels from the tab bar — reached via header nav.
       Visible tabs are Roster/Overview/Viz/Trends/Player Plans/Bullpens (1-6);
       the tool panels are the 7th, 8th, 9th tabs. */
    .adv-placeholder {
      max-width: 760px; margin: 30px auto; padding: 26px 32px;
      background: #F7F8F9; border: 1px solid #E5E7EB; border-radius: 14px;
      color: #374151; line-height: 1.55;
    }
    .adv-placeholder h4 { color: #0E6E70; font-weight: 700; margin-top: 0; }

    /* ---- Segmented-control sub-tabs (Advance, Viz, Hitters, Reports) ---- */
    .nav-pills {
      display: inline-flex; flex-wrap: nowrap; justify-content: flex-start;
      gap: 0; margin: 14px auto 20px; border: none;
      background: #F1F4F5; border-radius: 10px; padding: 4px;
      max-width: 100%; overflow-x: auto; -webkit-overflow-scrolling: touch;
      position: relative;
    }
    .nav-pills::-webkit-scrollbar { height: 0; }
    /* center the inline-flex track */
    .tab-pane > .tabbable > .nav-pills,
    .tab-pane > .nav-pills { display: flex; width: fit-content; margin-left: auto; margin-right: auto; }

    .nav-pills > li { float: none; }
    .nav-pills > li > a {
      border-radius: 7px; padding: 7px 18px; font-weight: 600; font-size: 13.5px;
      color: #5B6B70; background: transparent; border: none;
      white-space: nowrap; margin: 0;
      transition: color .2s ease, background-color .2s ease;
    }
    .nav-pills > li > a:hover { background: rgba(255,255,255,.6); color: #0E6E70; }
    .nav-pills > li.active > a,
    .nav-pills > li.active > a:focus,
    .nav-pills > li.active > a:hover {
      background: #FFFFFF !important;
      color: #0B3D3E !important; border-color: transparent; font-weight: 700;
      box-shadow: 0 1px 2px rgba(16,24,40,.10), 0 1px 3px rgba(16,24,40,.06);
    }
    .nav-pills > li > a:focus-visible { outline: 2px solid #0E6E70; outline-offset: -2px; }
    .nav-pills > li > a:focus { outline: none; }

    /* ---- Matchup Projections card ---- */
    .mm-card { background: #fff; border: 1px solid #E5E7EB; border-radius: 16px;
               box-shadow: 0 6px 22px rgba(0,0,0,.07); overflow-x: auto;
               margin: 8px 0 20px; max-width: 100%; }
    .mm-card-header { display: flex; align-items: baseline; gap: 14px;
                      flex-wrap: wrap; padding: 16px 22px;
                      border-bottom: 1px solid #ECEFF1; }
    .mm-title { font-size: 20px; font-weight: 800; color: #0B3D3E; }
    .mm-sub { color: #8A9296; font-size: 13px; }
    .mm-table { border-collapse: separate; border-spacing: 0; width: max-content; min-width: 100%; table-layout: auto; }
    .mm-table thead th { padding: 12px 10px; font-weight: 700; text-align: center;
                         color: #0B3D3E; border-bottom: 2px solid #ECEFF1;
                         font-size: 13px; line-height: 1.25; }
    .mm-table thead th.mm-bh { text-align: left; color: #8A9296; font-weight: 600;
                               font-size: 12px; text-transform: uppercase;
                               letter-spacing: .05em; padding-left: 22px; }
    .mm-table td { padding: 8px 10px; border-bottom: 1px solid #F1F3F4; }
    .mm-table tbody tr:last-child td { border-bottom: none; }
    .mm-table td.mm-namecell { padding-left: 22px; min-width: 360px; max-width: 520px;
                                white-space: normal; vertical-align: top;
                                position: sticky; left: 0; z-index: 1; background: #fff;
                                box-shadow: 8px 0 10px -12px rgba(0,0,0,.35); }
    .mm-table thead th.mm-bh { position: sticky; left: 0; z-index: 2; background: #fff; }
    .mm-hitter-head { display:flex; align-items:flex-start; justify-content:space-between;
                      gap:8px; min-width:0; }
    .mm-hitter-main { min-width:0; }
    .mm-name { font-weight: 600; color: #111827; }
    .mm-hand { color: #9AA1A6; font-size: 12px; margin-left: 7px; }
    .mm-lefty { color: #E1463E !important; }
    .mm-switch { color: #1E88E5 !important; }
    .mm-drs-pill { display: inline-block; margin-top: 3px; padding: 1px 9px;
                   border-radius: 999px; font-size: 11px; font-weight: 800;
                   color: #1F2937; }
    /* Detailed-view chips: quiet, single-weight, no boxes-within-boxes.
     They sit under the grade pills in the name cell so the grid itself
     stays untouched. */
  .mm-detail { margin-top:7px; padding-top:6px; max-width:500px;
               border-top:1px solid rgba(0,0,0,.06); }
  .mm-chiprow { display:flex; flex-wrap:wrap; gap:4px 8px; margin:2px 0; }
  .mm-chip { display:inline-flex; align-items:baseline; gap:5px; min-width:0;
             font-size:10.5px; line-height:1.5; white-space:normal; }
  .mm-chip-l { color:#9AA5A5; font-weight:600; letter-spacing:.02em;
               text-transform:uppercase; font-size:9px; }
  .mm-chip-v { color:#31423F; font-weight:650; overflow-wrap:anywhere;
               font-variant-numeric:tabular-nums; }
  .mm-grades { display: flex; gap: 4px; margin-top: 4px; flex-wrap: wrap; }
    .mm-gpill { display: inline-flex; flex-direction: column; align-items: center;
                min-width: 34px; padding: 1px 6px; border-radius: 7px;
                line-height: 1.1; color: #1F2937; }
    .mm-gpill .mm-glab { font-size: 8px; font-weight: 700; letter-spacing: .04em;
                         text-transform: uppercase; opacity: .8; }
    .mm-gpill .mm-gval { font-size: 12px; font-weight: 800; }
    .mm-gpill.mm-gagg { outline: 1.5px solid rgba(0,111,113,.55); }
    .mm-gpill.mm-gna  { background: #EDEFF1 !important; color: #9AA1A6; }
    /* ---- Beta: Matchup+ (process-model) matrix ---- */
    .mmp-cell { min-width: 96px; max-width: 138px; margin: 0 auto; text-align: center;
                padding: 7px 5px; border-radius: 9px; cursor: pointer;
                transition: transform .08s ease, box-shadow .08s ease; }
    .mmp-cell:hover { transform: translateY(-1px);
                      box-shadow: 0 4px 12px rgba(0,0,0,.16); }
    .mmp-cell.mmp-lc { border: 1.5px dashed rgba(0,0,0,.38); padding: 5.5px 3.5px; }
    .mmp-cell.mmp-na { cursor: default; }
    .mmp-cell.mmp-sel { outline: 2.5px solid #0E6E70; outline-offset: 1px; }
    .mmp-score { font-size: 17px; font-weight: 850; line-height: 1.15;
                 font-variant-numeric: tabular-nums; }
    .mmp-score .mmp-arrow { font-size: 10px; font-weight: 700; opacity: .7;
                            margin-left: 2px; }
    .mmp-sublab { font-size: 7.5px; font-weight: 700; letter-spacing: .07em;
                  text-transform: uppercase; opacity: .62; line-height: 1; }
    .mmp-subrow { display: flex; gap: 3px; justify-content: center;
                  margin-top: 4px; flex-wrap: nowrap; }
    .mmp-subpill { display: inline-flex; flex-direction: column; align-items: center;
                   min-width: 27px; padding: 1px 4px; border-radius: 6px;
                   line-height: 1.12; }
    .mmp-subpill .mmp-sl { font-size: 7.5px; font-weight: 800; letter-spacing: .05em;
                           text-transform: uppercase; opacity: .8; }
    .mmp-subpill .mmp-sv { font-size: 10.5px; font-weight: 800;
                           font-variant-numeric: tabular-nums; }
    .mmp-ptrow { display: flex; gap: 3px; justify-content: center;
                 margin-top: 4px; flex-wrap: wrap; }
    .mmp-ptpill { padding: 1px 4px; border-radius: 5px; font-size: 8.5px;
                  font-weight: 800; letter-spacing: .02em; line-height: 1.45; }
    .mmp-ptpill.mmp-ptlc { border: 1px dashed rgba(0,0,0,.42); padding: 0 3px; }
    /* ---- detailed hitter profile (name column) ---- */
    .hpf-wrap { display: flex; gap: 10px; margin-top: 6px; flex-wrap: wrap; }
    .hpf-side { flex: 1 1 168px; min-width: 156px; }
    .hpf-sidecap { font-size: 8.5px; font-weight: 800; letter-spacing: .09em;
                   text-transform: uppercase; color: #8A9296; margin-bottom: 2px; }
    .hpf-grid { display: grid; grid-template-columns: 22px 1fr; gap: 2px 4px;
                align-items: center; }
    .hpf-rlab { font-size: 8px; font-weight: 800; color: #9AA1A6;
                letter-spacing: .05em; }
    .hpf-row { display: flex; gap: 2px; }
    .hpf-pill { display: inline-flex; flex-direction: column; align-items: center;
                min-width: 27px; padding: 1px 3px; border-radius: 5px;
                line-height: 1.1; background: #F4F6F7; }
    .hpf-pill.hpf-na { background: #F4F6F7; color: #C4CACD; }
    .hpf-lab { font-size: 6.5px; font-weight: 800; letter-spacing: .05em;
               opacity: .78; }
    .hpf-val { font-size: 10px; font-weight: 800;
               font-variant-numeric: tabular-nums; }
    .hpf-call { display: inline-flex; align-items: center; font-size: 8.5px;
                font-weight: 900; letter-spacing: .03em; padding: 0 5px;
                margin-left: 2px; border-radius: 4px; line-height: 1.6;
                background: #0E6E70; color: #fff; white-space: nowrap; }
    .hpf-tagline { display: flex; align-items: center; gap: 3px; margin-top: 3px;
                   flex-wrap: wrap; }
    .hpf-tagcap { font-size: 7.5px; font-weight: 800; letter-spacing: .05em;
                  text-transform: uppercase; color: #8A9296; min-width: 44px; }
    .hpf-tag { font-size: 8.5px; font-weight: 800; padding: 0 5px;
               border-radius: 4px; line-height: 1.5; }
    .hpf-tagnone { font-size: 9px; color: #C4CACD; font-style: italic; }
    .hpf-thin { font-size: 9px; color: #C4CACD; font-style: italic;
                padding: 4px 0; }
    .hpf-heats { display: flex; gap: 10px; flex-wrap: wrap; margin-top: 4px; }
    .hpf-heat { text-align: center; }
    .hpf-heat-cap { font-size: 9px; font-weight: 700; color: #8A9296;
                    letter-spacing: .05em; text-transform: uppercase;
                    margin-bottom: 2px; }
    .mmu-dual { display: flex; align-items: stretch; justify-content: center; gap: 2px; }
    .mmu-half { flex: 1 1 0; padding: 0 1px; }
    .mmu-half-r { border-left: 1px solid rgba(0,0,0,.14); }
    .mmu-dual .mmp-score { font-size: 15px; }
    .mmu-strip { display: flex; justify-content: center; margin-top: 3px; }
    .mmu-chip { font-size: 9px; font-weight: 800; letter-spacing: .03em;
                padding: 1px 6px; border-radius: 5px; }
    .mmu-h2h { font-size: 8.5px; font-weight: 750; letter-spacing: .02em;
               padding: 0 5px; border-radius: 4px; line-height: 1.5;
               background: rgba(255,255,255,.72); color: #31423F; }
    .mmu-h2h-none { opacity: .45; font-style: italic; font-weight: 600; }
    .mmp-legend { display: flex; flex-wrap: wrap; gap: 6px 18px; align-items: center;
                  color: #6B7280; font-size: 11.5px; margin: 2px 0 10px; }
    .mmp-beta-tag { display: inline-block; background: #0E6E70; color: #fff;
                    font-size: 9.5px; font-weight: 800; letter-spacing: .09em;
                    text-transform: uppercase; padding: 2px 8px;
                    border-radius: 999px; vertical-align: middle;
                    margin-left: 8px; }
    .mmp-dtable { border-collapse: separate; border-spacing: 0; width: 100%; }
    .mmp-dtable th { font-size: 10.5px; font-weight: 700; color: #8A9296;
                     text-transform: uppercase; letter-spacing: .05em;
                     padding: 7px 10px; text-align: center;
                     border-bottom: 2px solid #ECEFF1; }
    .mmp-dtable th:first-child { text-align: left; }
    .mmp-dtable td { padding: 7px 10px; text-align: center; font-size: 13px;
                     border-bottom: 1px solid #F1F3F4;
                     font-variant-numeric: tabular-nums; }
    .mmp-dtable td:first-child { text-align: left; font-weight: 650; color: #111827; }
    .mmp-dtable tr:last-child td { border-bottom: none; }
    .mmp-dpill { display: inline-block; min-width: 40px; padding: 2px 9px;
                 border-radius: 7px; font-weight: 800; font-size: 12.5px; }
    .mm-cell { min-width: 84px; max-width: 128px; margin: 0 auto; text-align: center;
               padding: 6px 4px; border-radius: 9px; font-weight: 800; font-size: 14px;
               cursor: pointer; transition: transform .08s ease, box-shadow .08s ease; }
    .mm-cell-score { font-size: 14px; line-height: 1.2; }
    .mm-minirow { display: flex; gap: 3px; justify-content: center;
                  margin-top: 4px; flex-wrap: nowrap; }
    .mm-mini { padding: 1px 5px; border-radius: 6px; font-size: 9px;
               font-weight: 800; color: #1F2937; line-height: 1.5;
               letter-spacing: .02em; }
    .mm-mini.mm-mini-lc { border: 1.2px dashed #757575; padding: 0px 4px; }
    /* Detailed-matrix heatmap columns: Miss% / xSLG zone maps per hand */
    .mm-table thead th.mm-heat-h { font-size: 11px; color: #0B3D3E;
                                   font-weight: 700; line-height: 1.2;
                                   padding: 12px 4px; }
    .mm-table td.mm-heatcell { vertical-align: top; text-align: center;
                               padding: 8px 3px; }
    .mm-heatimg { width: 96px; height: auto; display: block; margin: 0 auto;
                  border-radius: 6px; }
    .mm-cell:hover { transform: scale(1.07); box-shadow: 0 2px 9px rgba(0,0,0,.22); }
    .mm-cell.mm-lc { border: 1.5px dashed #9E9E9E; }
    .mm-cell.mm-na { cursor: default; }
    .mm-cell.mm-na:hover { transform: none; box-shadow: none; }
    .mm-stat-pill { min-width: 54px; margin: 0 auto; text-align: center;
                    padding: 5px 8px; border-radius: 8px; font-weight: 700;
                    font-size: 13px; }
    .mm-stat-plain { text-align: center; font-weight: 600; color: #374151;
                     font-size: 13px; }
    .mm-rv-cell { cursor: pointer; transition: transform .08s ease, box-shadow .08s ease; }
    .mm-rv-cell:hover { transform: scale(1.08); box-shadow: 0 2px 9px rgba(0,0,0,.22); }
    .mm-rv-sel { outline: 2.5px solid #0E6E70; outline-offset: 1px; }
    .mm-comp-pill { margin-left: auto; cursor: default; min-width: 54px; }
    .mm-comp-pill:hover { transform: none; box-shadow: none; }
    .ms-section-title { color: #8B93A7; font-weight: 800; font-size: 13px;
                        text-transform: uppercase; letter-spacing: .08em;
                        margin: 14px 4px 4px; }
    .mm-gameplan { background: #F4F8F8; border: 1px solid #DCE7E7;
                   border-radius: 12px; padding: 14px 20px; margin: 10px 22px 18px; }
    .mm-gameplan-title { color: #0E6E70; font-weight: 800; font-size: 12px;
                         text-transform: uppercase; letter-spacing: .08em;
                         margin-bottom: 6px; }
    .mm-overview-verdict { font-weight: 700; color: #0B3D3E; margin-bottom: 6px;
                           font-size: 14px; }
    .mm-gameplan ul { margin: 0; padding-left: 20px; }
    .mm-gameplan li { color: #374151; font-size: 13.5px; line-height: 1.65; }

    /* ---- Scouting Workbook staff rows ---- */
    .swb-row { display: flex; align-items: center; gap: 14px;
               background: #fff; border: 1px solid #E5E7EB; border-radius: 12px;
               padding: 8px 14px; margin-bottom: 8px;
               box-shadow: 0 2px 6px rgba(0,0,0,.04); }
    .swb-ord { width: 26px; height: 26px; border-radius: 50%; flex: 0 0 26px;
               background: #0E6E70; color: #fff;
               font-weight: 700; font-size: 13px; display: flex;
               align-items: center; justify-content: center; }
    .swb-info { flex: 1 1 auto; min-width: 0; }
    .swb-name { font-weight: 700; color: #111827; }
    .swb-bio  { color: #8A9296; font-size: 12px; }
    .swb-role .form-group { margin-bottom: 0; }
    .swb-remove { flex: 0 0 auto; width: 26px; height: 26px; border-radius: 50%;
                  background: #FBECEC; color: #C0392B !important; font-size: 17px;
                  font-weight: 700; text-decoration: none !important;
                  display: flex; align-items: center; justify-content: center;
                  transition: all .12s ease; }
    .swb-remove:hover { background: #E1463E; color: #fff !important; }

    /* Dropdown-only pages: reachable from the header menus, never shown
       as chips in the tab bar. Tagged by NAME in JS (tpTagDetailTabs) so
       adding or merging a tabPanel can't retarget this at the wrong one. */
    #main_tabs.nav-tabs > li.tab-always-hidden {
      display: none !important;
    }

    /* ==================================================================
       SLIDING BUTTON TOGGLES  (radioGroupButtons / checkboxGroupButtons)
       A single white thumb slides between options instead of each
       button repainting. Thumb position is driven by JS (--tg-x/--tg-w).
       ================================================================== */
    .btn-group-container-sw,
    .shiny-input-radiogroup .btn-group,
    .shiny-input-checkboxgroup .btn-group {
      position: relative;
      background: #F1F4F5;
      border-radius: 9px;
      padding: 3px;
      display: inline-flex;
      isolation: isolate;
    }
    /* the sliding thumb */
    .shiny-input-radiogroup .btn-group::before,
    .shiny-input-checkboxgroup .btn-group::before {
      content: '';
      position: absolute;
      z-index: 0;
      top: 3px; bottom: 3px;
      left: 0;
      width: var(--tg-w, 0px);
      transform: translateX(var(--tg-x, 0px));
      background: #FFFFFF;
      border-radius: 7px;
      box-shadow: 0 1px 2px rgba(16,24,40,.10), 0 1px 3px rgba(16,24,40,.06);
      opacity: var(--tg-o, 0);
      transition: transform .28s cubic-bezier(.4,0,.2,1),
                  width .28s cubic-bezier(.4,0,.2,1),
                  opacity .15s ease;
      pointer-events: none;
    }
    /* no slide animation before first measurement */
    .shiny-input-radiogroup .btn-group.tg-init::before,
    .shiny-input-checkboxgroup .btn-group.tg-init::before { transition: none; }

    .shiny-input-radiogroup .btn-group > .btn,
    .shiny-input-checkboxgroup .btn-group > .btn,
    .shiny-input-radiogroup .btn-group > label.btn,
    .shiny-input-checkboxgroup .btn-group > label.btn {
      position: relative; z-index: 1;
      background: transparent !important;
      border: none !important;
      box-shadow: none !important;
      color: #5B6B70 !important;
      font-weight: 600; font-size: 13px;
      padding: 6px 16px;
      border-radius: 7px !important;
      transition: color .2s ease;
      outline: none !important;
    }
    .shiny-input-radiogroup .btn-group > .btn:hover,
    .shiny-input-checkboxgroup .btn-group > .btn:hover { color: #0E6E70 !important; }

    .shiny-input-radiogroup .btn-group > .btn.active,
    .shiny-input-checkboxgroup .btn-group > .btn.active,
    .shiny-input-radiogroup .btn-group > label.btn.active,
    .shiny-input-checkboxgroup .btn-group > label.btn.active {
      background: transparent !important;
      color: #0B3D3E !important;
      font-weight: 700;
      box-shadow: none !important;
    }
    /* fallback: if JS thumb never initializes, show a solid active chip */
    .shiny-input-radiogroup .btn-group.tg-nofx > .btn.active,
    .shiny-input-checkboxgroup .btn-group.tg-nofx > .btn.active {
      background: #FFFFFF !important;
      box-shadow: 0 1px 2px rgba(16,24,40,.10) !important;
    }

    /* ==================================================================
       HEADER HOVER DROPDOWNS (Advance / Reports)
       ================================================================== */
    .header-nav-item { position: relative; display: inline-flex; }
    .header-nav-item > .header-nav-link { display: inline-flex; align-items: center; gap: 6px; }
    .header-nav-caret {
      display: inline-block; width: 0; height: 0; margin-top: 2px;
      border-left: 4px solid transparent; border-right: 4px solid transparent;
      border-top: 5px solid rgba(255,255,255,.85);
      transition: transform .2s ease;
    }
    .header-nav-item:hover .header-nav-caret { transform: rotate(180deg); }

    .header-dropdown {
      position: absolute; top: 100%; left: 50%;
      transform: translateX(-50%) translateY(6px);
      margin-top: 10px;
      min-width: 232px;
      background: #FFFFFF;
      border: 1px solid #E8ECEE;
      border-radius: 10px;
      box-shadow: 0 10px 28px rgba(16,24,40,.14), 0 2px 6px rgba(16,24,40,.06);
      padding: 6px;
      opacity: 0; visibility: hidden; pointer-events: none;
      transition: opacity .18s ease, transform .18s ease, visibility .18s;
      z-index: 2600;
    }
    /* hover bridge so the gap doesn't close the menu */
    .header-dropdown::before {
      content: ''; position: absolute; top: -12px; left: 0; right: 0; height: 12px;
    }
    .header-nav-item:hover .header-dropdown,
    .header-dropdown:hover,
    .header-nav-item:focus-within .header-dropdown {
      opacity: 1; visibility: visible; pointer-events: auto;
      transform: translateX(-50%) translateY(0);
    }
    .header-dropdown a.hdr-dd-item {
      display: block; padding: 9px 13px; border-radius: 7px;
      color: #33474B !important; font-size: 13.5px; font-weight: 600;
      text-decoration: none !important; white-space: nowrap;
      transition: background-color .15s ease, color .15s ease;
    }
    .header-dropdown a.hdr-dd-item:hover {
      background: #F1F6F6; color: #0E6E70 !important;
    }
    .header-dropdown .hdr-dd-label {
      padding: 7px 13px 5px; font-size: 10.5px; font-weight: 800;
      letter-spacing: .09em; text-transform: uppercase; color: #98A4A8;
    }
    @media (max-width: 768px) {
      .header-dropdown { position: static; transform: none; opacity: 1;
        visibility: visible; pointer-events: auto; box-shadow: none;
        border: none; min-width: 0; display: none; }
      .header-nav-item:hover .header-dropdown { transform: none; }
    }

    /* ==================================================================
       CONTEXT-AWARE MAIN TABS
       Advance / Reports / Leaderboard / Hitters are standalone pages:
       the pitcher-detail tabs collapse so those views stand on their own.

       Tab bar is now just:
         1 Roster   2 Players   3 Pitching Leaderboard
         4 League Pitchers   5 League Pitches   6 Advance   7 Reports
       (3-5 are dropdown-only and always hidden.) Player detail lives in
       the nested #player_subtabs pills inside Players, so NOTHING here is
       matched by position any more -- every rule is class based.
       ================================================================== */
    body.focus-mode #main_tabs.nav-tabs > li.tab-pitcher-detail {
      display: none !important;
    }
    /* On a standalone page also drop Roster + Players so only the
       active page chip remains in the bar. Tagged by name in JS. */
    body.focus-mode #main_tabs.nav-tabs > li.tab-hide-in-focus {
      display: none !important;
    }
    body.focus-mode #main_tabs.nav-tabs { justify-content: center; }

    /* ==================================================================
       TOOLTIPS
       ================================================================== */
    [data-tip] { position: relative; }
    [data-tip]::after {
      content: attr(data-tip);
      position: absolute; bottom: calc(100% + 8px); left: 50%;
      transform: translateX(-50%) translateY(4px);
      background: #1F2A2E; color: #fff;
      font-size: 11.5px; font-weight: 600; line-height: 1.35;
      letter-spacing: .1px;
      padding: 6px 10px; border-radius: 6px;
      white-space: nowrap; max-width: 280px;
      opacity: 0; pointer-events: none; z-index: 3200;
      transition: opacity .16s ease, transform .16s ease;
      box-shadow: 0 4px 14px rgba(16,24,40,.20);
    }
    [data-tip]::before {
      content: ''; position: absolute; bottom: calc(100% + 3px); left: 50%;
      transform: translateX(-50%) translateY(4px);
      border: 5px solid transparent; border-top-color: #1F2A2E;
      opacity: 0; pointer-events: none; z-index: 3200;
      transition: opacity .16s ease, transform .16s ease;
    }
    [data-tip]:hover::after, [data-tip]:hover::before,
    [data-tip]:focus-visible::after, [data-tip]:focus-visible::before {
      opacity: 1; transform: translateX(-50%) translateY(0);
    }
    [data-tip='']::after, [data-tip='']::before { display: none; }

    /* ==================================================================
       SKELETON LOADERS
       ================================================================== */
    @keyframes tp-shimmer {
      0%   { background-position: -420px 0; }
      100% { background-position: 420px 0; }
    }
    .tp-skel {
      background: #EDF1F2;
      background-image: linear-gradient(90deg,
        rgba(255,255,255,0) 0%, rgba(255,255,255,.75) 50%, rgba(255,255,255,0) 100%);
      background-repeat: no-repeat;
      background-size: 420px 100%;
      border-radius: 6px;
      animation: tp-shimmer 1.35s ease-in-out infinite;
    }
    @media (prefers-reduced-motion: reduce) {
      .tp-skel { animation: none; }
      .shiny-input-radiogroup .btn-group::before { transition: none; }
      .nav-tabs > li > a::after { transition: none; }
    }
    .tp-skel-card {
      border: 1px solid #E8ECEE; border-radius: 12px; padding: 18px;
      background: #fff; margin-bottom: 14px;
    }
    .tp-skel-title { height: 18px; width: 210px; margin-bottom: 16px; }
    .tp-skel-tablerow { height: 34px; margin-bottom: 7px; border-radius: 5px; }

    /* Overlay recalculating state for any output */
    .shiny-bound-output.recalculating { opacity: .45; transition: opacity .2s ease; }

    /* ==================================================================
       INLINE SPINNERS
       ================================================================== */
    @keyframes tp-spin { to { transform: rotate(360deg); } }
    /* Spinner inside buttons while an action is pending */
    .btn.tp-busy { position: relative; color: transparent !important; pointer-events: none; }
    .btn.tp-busy::after {
      content: ''; position: absolute; top: 50%; left: 50%;
      width: 15px; height: 15px; margin: -7.5px 0 0 -7.5px;
      border: 2px solid rgba(255,255,255,.35); border-top-color: #fff;
      border-radius: 50%; animation: tp-spin .7s linear infinite;
    }

    /* Skeleton host: shows .tp-skel-ph only while its output recalculates */
    .tp-skel-host { position: relative; }
    .tp-skel-host > .tp-skel-ph { display: none; }
    .tp-skel-host.is-loading > .tp-skel-ph { display: block; }
    .tp-skel-host.is-loading > .shiny-bound-output,
    .tp-skel-host.is-loading > .shiny-html-output { display: none !important; }

    /* Global busy bar at very top of viewport */
    #tp-busy-bar {
      position: fixed; top: 0; left: 0; height: 3px; width: 0;
      background: #0E6E70; z-index: 4000;
      transition: width .3s ease, opacity .35s ease; opacity: 0;
      box-shadow: 0 0 8px rgba(14,110,112,.6);
    }
    #tp-busy-bar.on { opacity: 1; }
  "))
  ),
  
  # Startup loading overlay
  tags$div(id = "app-loading-overlay", class = "app-loading-overlay",
           tags$div(class = "app-loading-inner",
                    tags$div(class = "app-loading-spinner"),
                    tags$div(class = "app-loading-text", "All Data Loading In"),
                    tags$div(class = "app-loading-sub", "This may take a bit\u2026"))),

  # Header with three logos
  div(class = "app-header",
      div(class = "header-left",
          tags$a(href = "https://huggingface.co/spaces/CoastalBaseball/TealPages",
                 target = "_blank", class = "header-brand",
                 title = "Palace",
                 div(class = "header-brand-logo",
                     HTML(palace_lockup("dark", "signal",
                                        "COASTAL CAROLINA BASEBALL")))),
          div(class = "header-controls",
              div(id = "header-player-search",
                  selectizeInput(
                    inputId = "global_pitcher",
                    label   = NULL,
                    choices = NULL,
                    selected = NULL,
                    width = "100%",
                    options = list(placeholder = "Search players\u2026")
                  )),
              div(id = "header-season-select",
                  pickerInput(
                    inputId = "season_type",
                    label   = NULL,
                    choices = c("Spring 2025" = "Spring25", "Fall 2025" = "Fall25",
                                "Pre-Spring 2026" = "PreSpring26",
                                "Spring 2026" = "Spring26",
                                "Fall 2026" = "Fall26"),
                    selected = "Spring26",
                    width = "100%",
                    options = list(
                      `actions-box` = TRUE,
                      `selectAllText` = "Select all seasons",
                      `deselectAllText` = "Deselect all",
                      `none-selected-text` = "Select season",
                      `count-selected-text` = "{0} seasons selected"
                    ),
                    multiple = TRUE
                  ))
          )
      )
      ),

  # Floating Filters button — stays accessible while scrolling (hidden on Roster)
  tags$script(HTML("
    // If Shiny hard-disconnects (gray overlay), reload after 3s so the user
    // lands back in a live session instead of a dead one.
    $(document).on('shiny:disconnected', function() {
      setTimeout(function() { location.reload(); }, 3000);
    });
  ")),
  tags$div(id = "floating-filter-wrap", class = "floating-filter-btn",
           actionLink("go_filters", tagList(tags$span("\u2630"), tags$span("Filters", class = "ffb-label")))),
  # ===== GLOBAL CONTROL BAR (Season / Games) — hidden on Roster =====
  # (Player search now lives in the app header.)
  # ===== GLOBAL CONTROL BAR =====
  # Season select now lives in the app header; only the hidden Hawkeye
  # toggle remains here so downstream `input$include_manual_data` still works.
  div(id = "global-control-wrap", class = "global-control-bar",
      div(style = "display:none;",
          checkboxInput(
            inputId = "include_manual_data",
            label = "Houston Hawkeye Data (Limited)",
            value = TRUE
          )
      )
  ),

  # ===== SLIDE-OUT GLOBAL FILTER SIDEBAR =====
  # Toggled by the "Filters" button in the header. Applies to every tab.
  div(id = "filter-overlay", class = "filter-overlay",
      onclick = "Shiny.setInputValue('close_filters', Math.random())"),
  div(id = "filter-sidebar", class = "filter-sidebar",
      div(class = "filter-sidebar-header",
          tags$span("Filters", class = "filter-sidebar-title"),
          actionLink("close_filters_btn", "\u2715", class = "filter-close")),
      div(class = "filter-sidebar-body",
          dateRangeInput("g_dateRange", "Date Range",
                         start = Sys.Date() - 365, end = Sys.Date()),
          # Per-pitcher game select: lists only the games the selected
          # pitcher actually threw in (date, opponent, pitch count).
          # Leaving it empty means "all games".
          pickerInput("game_filter_spring26", "Games (pitched)",
                      choices = NULL, selected = NULL,
                      options = list(`actions-box` = TRUE,
                                     `selectAllText` = "Select all games",
                                     `deselectAllText` = "All games",
                                     `noneSelectedText` = "All games",
                                     `live-search` = TRUE,
                                     `count-selected-text` = "{0} games selected"),
                      multiple = TRUE),
          pickerInput("global_pool", "Grade Pool",
                      choices = c("D1" = "D1", "Power 5" = "Power 5", "Sun Belt" = "Sun Belt"),
                      selected = "D1", multiple = FALSE),
          pickerInput("g_BatterSide", "Batter Hand",
                      choices = c("Left","Right"), selected = c("Left","Right"),
                      options = list(`actions-box` = TRUE), multiple = TRUE),
          pickerInput("g_pitchtype", "Pitch Type",
                      choices = c("Fastball","Sinker","Cutter","Curveball","Slider","ChangeUp","Splitter","Sweeper"),
                      selected = c("Fastball","Sinker","Cutter","Curveball","Slider","ChangeUp","Splitter","Sweeper"),
                      options = list(`actions-box` = TRUE), multiple = TRUE),
          pickerInput("g_Roster", "2026 Roster",
                      choices = c("Yes","No"), selected = c("Yes","No"),
                      options = list(`actions-box` = TRUE), multiple = TRUE),
          sliderInput("g_velo", "Velocity Range",
                      min = 70, max = 105, value = c(70, 105), step = 0.5),
          pickerInput("g_count", "Count",
                      choices = list(
                        "Groups" = c("Hitters (ahead)" = "grp_hitters",
                                     "Pitchers (ahead)" = "grp_pitchers",
                                     "Even" = "grp_even",
                                     "0 Strikes" = "grp_0k", "1 Strike" = "grp_1k",
                                     "2 Strikes" = "grp_2k",
                                     "0 Balls" = "grp_0b", "1 Ball" = "grp_1b",
                                     "2 Balls" = "grp_2b", "3 Balls" = "grp_3b",
                                     "2 Strikes Not Full" = "grp_2k_notfull",
                                     "Less Than 2 Strikes" = "grp_lt2k"),
                        "Exact" = c("0-0","0-1","0-2","1-0","1-1","1-2",
                                    "2-0","2-1","2-2","3-0","3-1","3-2")
                      ),
                      selected = character(0),
                      options = list(`actions-box` = TRUE,
                                     `live-search` = TRUE,
                                     `none-selected-text` = "All counts",
                                     `count-selected-text` = "{0} count filters"),
                      multiple = TRUE),
          pickerInput("g_result", "Pitch Result",
                      choices = c("Strike (any)" = "res_strike",
                                  "Miss/Whiff" = "res_whiff",
                                  "Strike Looking" = "res_called",
                                  "Foul" = "res_foul",
                                  "Ball" = "res_ball",
                                  "Ball In Dirt" = "res_dirt",
                                  "Hit By Pitch" = "res_hbp",
                                  "In Play" = "res_inplay",
                                  "Swing" = "res_swing",
                                  "Take" = "res_take"),
                      selected = character(0),
                      options = list(`actions-box` = TRUE,
                                     `none-selected-text` = "All results",
                                     `count-selected-text` = "{0} result filters"),
                      multiple = TRUE),
          textInput("g_vs_team", "Vs Team (contains)", value = ""),
          actionButton("reset_filters", "Reset Filters", `data-tip` = "Clear all filters back to defaults",
                       class = "btn-block",
                       style = "margin-top:10px; background:#0E6E70; color:#fff; border:none;")
      )
  ),
  
  tabsetPanel(id = "main_tabs",

    # ============================ ROSTER (landing) ============================
    tabPanel("Roster", value = "Roster",
      div(style = "text-align:center; margin:18px 0 6px;",
          h2("Coastal Carolina Baseball", class = "brand-teal",
             style = "margin-bottom:2px;"),
          div(style = "color:#6B7280; font-size:14px;",
              "2027 roster — click a card to open that player's profile.")),
      div(style = "display:flex; justify-content:center; margin:8px 0 2px;",
          radioGroupButtons("roster_side", label = NULL,
                            choices = c("Pitchers", "Hitters"),
                            selected = "Pitchers", individual = TRUE,
                            size = "sm")),
      div(class = "roster-search-wrap",
          tags$input(id = "roster_search", type = "text",
                     class = "roster-search",
                     placeholder = "Search players\u2026",
                     oninput = "Shiny.setInputValue('roster_search', this.value, {priority:'event'})")),
      uiOutput("roster_cards")
    ),
    # ============================ PLAYERS ============================
    # ONE player page. The header search drives it: pick a pitcher and the
    # pitcher pills show, pick a hitter and the hitter pills show. The
    # server (see "PLAYER MODE" block) flips visibility on player_subtabs.
    tabPanel("Players", value = "Players",
      uiOutput("player_mode_head"),
      tabsetPanel(id = "player_subtabs", type = "pills",

        # ---------------- PITCHER PILLS ----------------
tabPanel("Overview", value = "Overview",

      # ---------------- Player-page styling ----------------
      tags$style(HTML("
        .pp-card{background:#FFFFFF;border:1px solid #ECEFF2;border-radius:16px;
                 padding:16px 18px;margin-bottom:14px;
                 box-shadow:0 1px 3px rgba(16,24,40,.05);}
        .pp-card-tight{padding:12px 14px;}
        .pp-card-title{font-weight:700;color:#243746;letter-spacing:.09em;
                 text-transform:uppercase;font-size:13px;text-align:center;
                 margin:0 0 12px;}
        .pp-bio-top{display:flex;align-items:center;gap:12px;}
        .pp-bio-head{width:64px;height:64px;border-radius:50%;object-fit:cover;
                 object-position:center top;flex:0 0 auto;background:#F2F5F7;
                 border:2.5px solid #0E6E70;
                 box-shadow:0 2px 6px rgba(16,24,40,.14);}
        .pp-bio-logo{width:52px;height:52px;object-fit:contain;flex:0 0 auto;
                 margin-left:auto;}
        .pp-bio-name{font-size:22px;font-weight:800;color:#152535;line-height:1.12;margin:0;}
        .pp-bio-sub{font-size:12.5px;color:#5B6875;margin-top:2px;}
        .pp-bio-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:8px;margin-top:12px;}
        .pp-bio-cell{background:#F7F9FA;border-radius:9px;padding:6px 9px;}
        .pp-bio-lab{font-size:9.5px;letter-spacing:.07em;text-transform:uppercase;color:#8A93A0;}
        .pp-bio-val{font-size:13px;font-weight:700;color:#243746;line-height:1.25;}
        .pp-pct-row{display:flex;align-items:center;gap:10px;margin:5px 0;}
        .pp-pct-lab{width:122px;text-align:right;font-size:11.5px;font-weight:600;
                 color:#3B4754;white-space:nowrap;overflow:hidden;
                 text-overflow:ellipsis;}
        .pp-pct-lab.is-grade{font-weight:800;color:#152535;}
        .pp-pct-track{flex:1;height:14px;background:#F0F3F5;border-radius:7px;
                 position:relative;}
        .pp-pct-gl{position:absolute;top:-3px;bottom:-3px;width:0;
                 border-left:1px dashed #D8DFE4;}
        .pp-pct-fill{position:absolute;left:0;top:0;bottom:0;border-radius:7px;
                 transition:width .55s cubic-bezier(.22,.61,.36,1);}
        .pp-pct-lead{position:absolute;right:-8px;top:50%;height:0;
                 border-top:1px dotted #B9C3CB;}
        .pp-pct-bub{position:absolute;top:50%;transform:translate(-50%,-50%);
                 width:27px;height:27px;border-radius:50%;font-size:11px;
                 font-weight:800;display:flex;align-items:center;justify-content:center;
                 border:2px solid #FFFFFF;box-shadow:0 1px 3px rgba(16,24,40,.22);
                 transition:left .55s cubic-bezier(.22,.61,.36,1);z-index:2;}
        .pp-pct-val{width:56px;text-align:right;font-size:11.5px;font-weight:700;
                 color:#243746;}
        .pp-pct-sec{display:flex;align-items:center;gap:9px;
                 margin:15px 0 7px;}
        .pp-pct-sec span{font-size:10px;font-weight:800;letter-spacing:.1em;
                 text-transform:uppercase;color:#0E6E70;white-space:nowrap;}
        .pp-pct-sec:before{content:'';width:114px;height:0;}
        .pp-pct-sec:after{content:'';flex:1;height:1px;background:#E4EAEE;}
        .pp-pct-sep{height:1px;background:#EDF1F3;margin:9px 0 9px 122px;}
        .pp-zone-strip{display:flex;gap:12px;overflow-x:auto;padding:4px 2px 10px;}
        .pp-zone-cell{background:#FFFFFF;border:1px solid #ECEFF2;border-radius:16px;
                 padding:8px 8px 4px;min-width:212px;flex:0 0 auto;
                 box-shadow:0 1px 2px rgba(16,24,40,.04);}
        .pp-zone-name{text-align:center;font-size:12px;font-weight:700;margin:2px 0 4px;}
        .pp-row-head{display:flex;align-items:center;gap:14px;flex-wrap:wrap;margin-bottom:6px;}
        .pp-row-lab{font-size:12px;font-weight:700;letter-spacing:.07em;
                 text-transform:uppercase;color:#243746;min-width:78px;}
        .pp-slide{animation:ppslide .38s cubic-bezier(.22,.61,.36,1);}
        @keyframes ppslide{from{opacity:0;transform:translateX(22px);}to{opacity:1;transform:none;}}
        .pp-fade{animation:ppfade .38s ease;}
        @keyframes ppfade{from{opacity:0;transform:translateY(7px);}to{opacity:1;transform:none;}}
        .pp-sec-lab{font-size:12px;font-weight:700;letter-spacing:.08em;
                 text-transform:uppercase;color:#0E6E70;margin:14px 0 6px;}
        /* ---- segmented controls (fixes overlapping button labels) ---- */
        .pp-seg .control-label{font-size:11px;letter-spacing:.06em;
                 text-transform:uppercase;color:#8A93A0;font-weight:700;
                 display:block;margin-bottom:5px;}
        .pp-seg .btn-group-container-sw,
        .pp-seg .radiobtn{display:inline-flex;flex-wrap:wrap;gap:6px;
                 float:none;width:auto;}
        .pp-seg .btn{border-radius:8px !important;border:1px solid #E4E8EC !important;
                 background:#F7F9FA !important;color:#5B6875 !important;
                 font-weight:600;font-size:12px;padding:6px 12px;
                 box-shadow:none !important;white-space:nowrap;float:none;width:auto;}
        .pp-seg .btn.active,.pp-seg .btn:active{background:#152535 !important;
                 color:#FFFFFF !important;border-color:#152535 !important;}
        .pp-seg .btn-group{display:inline-flex;gap:6px;}
        .pp-seg .btn-group>.btn{margin-left:0 !important;}

        /* ---- tables fill their container ---- */
        .pp-card .gt_table,.pp-model-tbl .gt_table,.pp-card table.gt_table{
                 width:100% !important;table-layout:auto;}
        .pp-card .gt_table thead th{white-space:nowrap;}

        /* ---- page / subtab transitions ---- */
        @keyframes pppagein{from{opacity:0;transform:translateY(10px);}
                            to{opacity:1;transform:none;}}
        .tab-content>.tab-pane.active{animation:pppagein .34s cubic-bezier(.22,.61,.36,1);}
        #pp_subtabs+.tab-content>.tab-pane.active{
                 animation:pppagein .32s cubic-bezier(.22,.61,.36,1);}
        #pp_subtabs>li>a{transition:color .18s ease,box-shadow .22s ease;}
        .pp-card{transition:box-shadow .2s ease;}

        /* ---- inline spinner over any output that is recalculating ---- */
        @keyframes ppspin{to{transform:rotate(360deg);}}
        .pp-card .shiny-bound-output,.pp-card .shiny-html-output,
        .pp-zone-cell .shiny-bound-output{position:relative;}
        .pp-card .shiny-bound-output.recalculating::after,
        .pp-card .shiny-html-output.recalculating::after,
        .pp-zone-cell .shiny-bound-output.recalculating::after{
                 content:'';position:absolute;top:50%;left:50%;
                 width:30px;height:30px;margin:-15px 0 0 -15px;
                 border:3px solid rgba(21,37,53,.14);
                 border-top-color:#0E6E70;border-radius:50%;
                 animation:ppspin .75s linear infinite;
                 pointer-events:none;z-index:6;}
        .pp-zone-cell .shiny-bound-output.recalculating::after{
                 width:20px;height:20px;margin:-10px 0 0 -10px;border-width:2px;}
        @media (prefers-reduced-motion: reduce){
          .tab-content>.tab-pane.active{animation:none;}
          .pp-card .shiny-bound-output.recalculating::after{animation:none;}
          .pp-slide,.pp-fade{animation:none;}
        }

        /* ---- skeleton loaders: any output still computing shimmers ---- */
        @keyframes ppshimmer{0%{background-position:100% 50%;}
                             100%{background-position:0 50%;}}
        .pp-card .shiny-bound-output.recalculating,
        .pp-card .shiny-html-output.recalculating,
        .pp-zone-cell .shiny-bound-output.recalculating{
                 background-image:linear-gradient(90deg,#F2F4F6 25%,#E7EBEE 37%,#F2F4F6 63%);
                 background-size:400% 100%;
                 animation:ppshimmer 1.25s ease-in-out infinite;
                 border-radius:10px;opacity:.72;min-height:56px;}
        .pp-card .shiny-html-output:empty,
        .pp-card .shiny-bound-output:empty{
                 background-image:linear-gradient(90deg,#F2F4F6 25%,#E7EBEE 37%,#F2F4F6 63%);
                 background-size:400% 100%;
                 animation:ppshimmer 1.25s ease-in-out infinite;
                 border-radius:10px;min-height:120px;display:block;}
        /* plotly sizes itself from the container it was rendered into; a
           subtab is display:none on first paint, so pin the width and let
           the resize kick below re-lay it out when the tab is shown. */
        .pp-card .js-plotly-plot,.pp-card .plotly,.pp-card .plot-container{
                 width:100% !important;}
        .pp-card .js-plotly-plot .svg-container{width:100% !important;}
        .pp-model-tbl{overflow-x:auto;}
        .hp-dt-sm table.dataTable{font-size:11px;}
        .hp-dt-sm table.dataTable td,.hp-dt-sm table.dataTable th{
                 padding:3px 5px !important;white-space:nowrap;}
        .hp-bar-row{display:flex;align-items:center;margin:7px 0;}
        .hp-bar-lab{width:150px;font-size:13px;color:#374151;font-weight:600;}
        .hp-bar-wrap{flex:1;height:10px;background:#EDEFF1;border-radius:6px;
                 position:relative;margin:0 12px;}
        .hp-bar-fill{height:10px;border-radius:6px;}
        .hp-bar-plus{width:60px;text-align:right;font-size:13px;
                 font-weight:700;color:#111827;}
        .pp-model-tbl table{width:100% !important;}
        #pp_subtabs{border-bottom:1px solid #ECEFF2;margin:4px 0 14px;}
        #pp_subtabs>li>a{font-weight:600;color:#77828F;border:none;background:transparent;}
        #pp_subtabs>li.active>a,#pp_subtabs>li.active>a:hover,#pp_subtabs>li.active>a:focus{
                 color:#152535;border:none;background:transparent;
                 box-shadow:inset 0 -2px 0 #152535;}
      ")),

      # Tabs render hidden, so any plotly inside one measures a zero-width
      # container and paints truncated. Nudging a resize on tab-show makes
      # it re-measure against the now-visible card.
      tags$script(HTML("
        $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(){
          setTimeout(function(){ $(window).trigger('resize'); }, 30);
          setTimeout(function(){ $(window).trigger('resize'); }, 420);
        });
        $(document).on('shiny:visualchange shiny:value', function(){
          setTimeout(function(){ $(window).trigger('resize'); }, 60);
        });
      ")),

      # ================= ROW 1: bio + percentile chart =================
      fluidRow(
        column(4, uiOutput("pp_bio_card")),
        column(8, div(class = "pp-card",
                      div(class = "pp-card-title", textOutput("pp_pct_title", inline = TRUE)),
                      uiOutput("pp_percentile_chart")))
      ),

      # ---- Traditional stats (TruMedia API) ----
      fluidRow(column(12, uiOutput("tm_trad_stats"))),

      # ================= ROW 2: grade bars + grade stats =================
      fluidRow(column(12, div(class = "pp-card",
        div(class = "pp-card-title", "Pitcher Grades"),
        div(style = "font-size:11.5px;color:#8A93A0;text-align:center;margin:-8px 0 4px;",
            "Plus-Stat vs ", textOutput("grade_pool_label", inline = TRUE),
            " pool \u2014 100 = average. Bars: vL / All / vR per pillar."),
        plotOutput("grade_bar_chart", height = "340px"),
        div(style = "margin-top:10px;", gt::gt_output("grade_stats_table"))
      ))),

      # ================= SUBTABS =================
      tabsetPanel(id = "pp_subtabs", type = "tabs",

        # ------------------------- METRICS -------------------------
        tabPanel("Metrics", value = "pp_metrics",
          br(),
          fluidRow(
            column(6, div(class = "pp-card",
                     div(class = "pp-card-title",
                         textOutput("pp_rel_title", inline = TRUE)),
                     div(style = "display:flex;justify-content:center;margin:-4px 0 6px;",
                         div(class = "pp-seg",
                             radioGroupButtons("pp_rel_chart", label = NULL,
                               choices = c("Release Points" = "points",
                                           "Consistency"    = "consistency",
                                           "Extension"      = "extension"),
                               selected = "points", size = "xs", individual = TRUE))),
                     # Individual / average toggles only mean something on the
                     # scatter, so they ride with it.
                     conditionalPanel("input.pp_rel_chart == 'points'",
                       div(style = "display:flex;justify-content:center;gap:18px;margin:0 0 4px;",
                           checkboxInput("pp_rel_individual", "Individual Pitches",
                                         value = TRUE),
                           checkboxInput("pp_rel_average", "Averages", value = TRUE)),
                       plotOutput("pp_release_points", height = "406px")),
                     conditionalPanel("input.pp_rel_chart == 'consistency'",
                       plotOutput("release_consistency_plot", height = "440px")),
                     conditionalPanel("input.pp_rel_chart == 'extension'",
                       plotOutput("rel_ext_plot", height = "440px")))),
            column(6, div(class = "pp-card",
                     div(class = "pp-card-title", "Pitch Movement"),
                     div(style = "display:flex;justify-content:center;gap:18px;margin:-4px 0 4px;",
                         checkboxInput("show_individual_pitches", "Individual Pitches", value = TRUE),
                         checkboxInput("show_arm_angle_line", "Arm Angle Line", value = TRUE),
                         checkboxInput("show_expected_mvmt", "Expected Movement", value = FALSE)),
                     plotOutput("square_mvmt_plot", height = "440px")))
          ),
          fluidRow(column(12, div(class = "pp-card",
            div(class = "pp-card-title", "Pitch Metrics"),
            gt::gt_output("pp_metrics_table"))))
        ),

        # ------------------------- RESULTS -------------------------
        tabPanel("Results", value = "pp_results",
          br(),
          div(class = "pp-card",
            div(class = "pp-card-title", "Results Trend"),
            fluidRow(
              column(3, selectInput("pp_res_metric", "Metric",
                                    choices = c("Strike %", "0-0 Zone %", "Swing %",
                                                "Whiff %", "Z-Whiff %", "2K Miss %",
                                                "Chase %", "GB %", "HH %", "AVG", "SLG"),
                                    selected = "Strike %")),
              column(3, dateRangeInput("pp_res_dates", "Date Range",
                                       start = NULL, end = NULL)),
              column(3, div(class = "pp-seg",
                        tags$label(class = "control-label", "Split"),
                        radioGroupButtons("pp_res_split", label = NULL,
                                          choices = c("Individual" = "ind", "Aggregate" = "agg"),
                                          selected = "ind", size = "sm", individual = TRUE))),
              column(3, div(class = "pp-seg",
                        tags$label(class = "control-label", "Batter Side"),
                        radioGroupButtons("pp_res_trend_side", label = NULL,
                                          choices = c("Both" = "B", "vs RHH" = "R", "vs LHH" = "L"),
                                          selected = "B", size = "sm", individual = TRUE)))
            ),
            plotly::plotlyOutput("pp_results_trend", height = "420px", width = "100%")
          ),
          div(class = "pp-card",
            div(class = "pp-card-title", "Results by Pitch"),
            div(style = "display:flex;justify-content:center;margin:-4px 0 10px;",
                div(class = "pp-seg",
                    radioGroupButtons("pp_res_side", label = NULL,
                                      choices = c("Overall" = "A", "vs RHH" = "R", "vs LHH" = "L"),
                                      selected = "A", size = "sm", individual = TRUE))),
            uiOutput("pp_results_table_ui")
          )
        ),

        # -------------------------- ZONE ---------------------------
        tabPanel("Zone", value = "pp_zone",
          br(),
          div(class = "pp-card",
            div(class = "pp-card-title", "Pitch Locations"),
            uiOutput("pp_zone_all_head"),
            uiOutput("pp_zone_all_strip"),
            hr(style = "border-color:#F1F4F6;"),
            uiOutput("pp_zone_rhh_head"),
            uiOutput("pp_zone_rhh_strip"),
            hr(style = "border-color:#F1F4F6;"),
            uiOutput("pp_zone_lhh_head"),
            uiOutput("pp_zone_lhh_strip")
          )
        ),

        # -------------------------- ANGLES -------------------------
        tabPanel("Angles", value = "pp_angles",
          br(),
          fluidRow(column(12, div(class = "pp-card",
            div(class = "pp-card-title", "Vertical Approach Angle by Pitch Type"),
            plotOutput("vaa_percentile_plot", height = "auto")))),
          fluidRow(
            column(6, div(class = "pp-card",
                     div(class = "pp-card-title", "Approach Angles (VAA vs HAA)"),
                     plotly::plotlyOutput("approach_angle_plot", height = "460px", width = "100%"))),
            column(6, div(class = "pp-card",
                     div(class = "pp-card-title", "Release Angles (VRA vs HRA)"),
                     plotly::plotlyOutput("release_angle_plot", height = "460px", width = "100%")))
          )
        ),

        # ------------------------- MODELING ------------------------
        tabPanel("Modeling", value = "pp_modeling",
          br(),
          div(class = "pp-card",
            div(class = "pp-card-title", "Model Trend"),
            fluidRow(
              column(3, selectInput("pp_mod_metric", "Metric",
                                    choices = c("proStuff+", "proLocation+", "proPitching+"),
                                    selected = "proStuff+")),
              column(3, dateRangeInput("pp_mod_dates", "Date Range",
                                       start = NULL, end = NULL)),
              column(3, div(class = "pp-seg",
                        tags$label(class = "control-label", "Split"),
                        radioGroupButtons("pp_mod_split", label = NULL,
                                          choices = c("Individual" = "ind", "Aggregate" = "agg"),
                                          selected = "ind", size = "sm", individual = TRUE))),
              column(3, div(class = "pp-seg",
                        tags$label(class = "control-label", "Batter Side"),
                        radioGroupButtons("pp_mod_trend_side", label = NULL,
                                          choices = c("Both" = "B", "vs RHH" = "R", "vs LHH" = "L"),
                                          selected = "B", size = "sm", individual = TRUE)))
            ),
            plotly::plotlyOutput("pp_modeling_trend", height = "420px", width = "100%")
          ),
          div(class = "pp-card",
            div(class = "pp-card-title", "Model Values by Pitch"),
            fluidRow(
              column(4, div(class = "pp-sec-lab", "Overall"),
                        div(class = "pp-model-tbl", gt::gt_output("pp_model_tbl_all"))),
              column(4, div(class = "pp-sec-lab", "vs RHH"),
                        div(class = "pp-model-tbl", gt::gt_output("pp_model_tbl_r"))),
              column(4, div(class = "pp-sec-lab", "vs LHH"),
                        div(class = "pp-model-tbl", gt::gt_output("pp_model_tbl_l")))
            )
          )
        )
      )
    ),

    # ============================ BULLPENS ============================
    tabPanel("Bullpens", value = "Bullpens",
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/hls.js/1.5.13/hls.min.js"),
      palace_bullpen_ui("bp")
    ),

    # ============================ SCOUTING ============================
    # NCAA (non-Coastal) pitchers only — replaces Player Plans + Bullpens.
    # Notes + full Advance-style PDF scouting report (download & preview).
    tabPanel("Scouting", value = "Scouting",
      div(style = "text-align:center; margin:16px 0 4px;",
          h3("Scouting", class = "brand-teal", style = "margin-bottom:2px;"),
          div(style = "color:#6B7280; font-size:13px;",
              textOutput("scout_pitcher_label", inline = TRUE))),

      fluidRow(
        column(6, div(style = "text-align:center;", h4("Arm Slot", class = "brand-teal")),
               plotOutput("scout_arm_angle", height = "300px")),
        column(6, div(style = "text-align:center;", h4("Movement", class = "brand-teal")),
               plotOutput("scout_movement", height = "300px"))
      ),

      hr(),
      h4("Season Line & Splits", class = "brand-teal",
         style = "display:inline-block; margin-right:8px;"),
      span("via TruMedia", style = "color:#9CA3AF; font-size:12px;"),
      uiOutput("tm_scout_splits_ui"),
      tags$details(style = "margin:4px 0 8px;",
        tags$summary("TruMedia connection debug",
                     style = "color:#9CA3AF; font-size:11px; cursor:pointer;"),
        uiOutput("tm_debug")),

      hr(),
      h4("Scouting Notes", class = "brand-teal"),
      fluidRow(
        column(6, textAreaInput("pdf_delivery_notes", "Delivery Notes:",
                                value = "", rows = 3, width = "100%")),
        column(6, textAreaInput("pdf_matchup_notes", "Matchup Notes:",
                                value = "", rows = 3, width = "100%"))
      ),
      fluidRow(
        column(4, textAreaInput("pdf_stamina_notes", "Stamina/Usage Notes:",
                                value = "", rows = 3, width = "100%")),
        column(4, textAreaInput("pdf_vs_lhh_notes", "vs LHH Notes:",
                                value = "", rows = 3, width = "100%")),
        column(4, textAreaInput("pdf_vs_rhh_notes", "vs RHH Notes:",
                                value = "", rows = 3, width = "100%"))
      ),
      h5("Pitch-Type Notes", class = "brand-teal"),
      uiOutput("scout_pitch_notes_ui"),

      fluidRow(column(12,
        div(style = "margin:14px 0; display:flex; gap:10px; flex-wrap:wrap;",
            actionButton("save_pitcher_notes", "Save Notes", `data-tip` = "Save these notes to this pitcher's profile",
                         style = "background:#0E6E70; color:#fff; border:none;"),
            downloadButton("download_scouting_pdf", "Download Full Scouting Report", `data-tip` = "Download the full scouting report as a PDF",
                           style = "background:#B08D57; color:#fff; border:none;"),
            downloadButton("download_scouting_xlsx", "Export to Excel", `data-tip` = "Export this report to a formatted Excel workbook",
                           style = "background:#1D6F42; color:#fff; border:none;"),
            actionButton("preview_scouting_pdf", class = "tp-spin-on-click", "Preview Report", `data-tip` = "Preview the report before downloading",
                         style = "background:#2C3E50; color:#fff; border:none;"))
      )),
      uiOutput("scout_pdf_preview")
    ),

        # ---------------- HITTER PILLS ----------------
        tabPanel("Overview", value = "hp_overview",
          # Hitter selection lives in the unified top search bar; the
          # selectize survives (hidden) as the reactive driver every
          # hp_* output already hangs off.
          div(style = "display:none;",
              selectizeInput("hp_batter", NULL, choices = NULL, width = "100%")),

          # ---- Row 1: condensed bio + grouped percentile chart ----
          fluidRow(
            column(4, uiOutput("hp_bio_card")),
            column(8, div(class = "pp-card",
                     div(class = "pp-card-title", textOutput("hp_pct_title", inline = TRUE)),
                     uiOutput("hp_percentile_chart")))
          ),

          # ---- Traditional stats (TruMedia API) ----
          fluidRow(column(12, uiOutput("hp_tm_trad_stats"))),

          # ---- Row 2: run value by pitch type ----
          fluidRow(column(12, div(class = "pp-card",
            div(class = "pp-card-title", "Run Value by Pitch Type"),
            uiOutput("hp_pt_value_bars"),
            div(style = "color:#8A93A0;font-size:11.5px;margin-top:6px;text-align:center;",
                "Process runs per 100 pitches of each type. Bars grow right when the ",
                "hitter adds runs against the pitch, left when he loses them; the badge ",
                "is his percentile vs every qualified hitter facing that type.")))),

          # ================= SUBTABS =================
          tabsetPanel(id = "hp_subtabs", type = "tabs",

            # ------------------------ METRICS ------------------------
            tabPanel("Metrics", value = "hp_metrics",
              br(),
              fluidRow(
                column(6, div(class = "pp-card",
                         div(class = "pp-card-title", "Plate Discipline"),
                         gt::gt_output("hp_discipline_table"))),
                column(6, div(class = "pp-card",
                         div(class = "pp-card-title", "Quality of Contact"),
                         gt::gt_output("hp_quality_table")))
              ),
              fluidRow(
                column(6, div(class = "pp-card hp-dt-sm",
                         div(class = "pp-card-title", "Value by Pitch Type \u2014 vs RHP"),
                         DT::dataTableOutput("hp_pitch_table_r"))),
                column(6, div(class = "pp-card hp-dt-sm",
                         div(class = "pp-card-title", "Value by Pitch Type \u2014 vs LHP"),
                         DT::dataTableOutput("hp_pitch_table_l")))
              )
            ),

            # ------------------------ RESULTS ------------------------
            tabPanel("Results", value = "hp_results",
              br(),
              div(class = "pp-card",
                div(class = "pp-card-title", "Results Trend"),
                fluidRow(
                  column(3, selectInput("hp_res_metric", "Metric",
                                        choices = c("Swing %", "Chase %", "Z-Swing %",
                                                    "Whiff %", "Z-Whiff %", "Contact %",
                                                    "Hard-Hit %", "Avg EV", "Sweet-Spot %",
                                                    "AVG", "SLG"),
                                        selected = "Whiff %")),
                  column(3, dateRangeInput("hp_res_dates", "Date Range",
                                           start = NULL, end = NULL)),
                  column(3, div(class = "pp-seg",
                            tags$label(class = "control-label", "Split"),
                            radioGroupButtons("hp_res_split", label = NULL,
                                              choices = c("Individual" = "ind",
                                                          "Aggregate" = "agg"),
                                              selected = "agg", size = "sm",
                                              individual = TRUE))),
                  column(3, div(class = "pp-seg",
                            tags$label(class = "control-label", "Pitcher Hand"),
                            radioGroupButtons("hp_res_trend_side", label = NULL,
                                              choices = c("Both" = "B", "vs RHP" = "R",
                                                          "vs LHP" = "L"),
                                              selected = "B", size = "sm",
                                              individual = TRUE)))
                ),
                plotly::plotlyOutput("hp_results_trend", height = "420px", width = "100%")
              ),
              div(class = "pp-card",
                div(class = "pp-card-title", "Results by Pitch"),
                div(style = "display:flex;justify-content:center;margin:-4px 0 10px;",
                    div(class = "pp-seg",
                        radioGroupButtons("hp_res_side", label = NULL,
                                          choices = c("Overall" = "A", "vs RHP" = "R",
                                                      "vs LHP" = "L"),
                                          selected = "A", size = "sm",
                                          individual = TRUE))),
                uiOutput("hp_results_table_ui")
              )
            ),

            # ------------------------- ZONE --------------------------
            tabPanel("Zone", value = "hp_zone",
              br(),
              div(class = "pp-card",
                div(class = "pp-card-title", "Zone Profile"),
                uiOutput("hp_zone_all_head"),
                uiOutput("hp_zone_all_strip"),
                hr(style = "border-color:#F1F4F6;"),
                uiOutput("hp_zone_rhp_head"),
                uiOutput("hp_zone_rhp_strip"),
                hr(style = "border-color:#F1F4F6;"),
                uiOutput("hp_zone_lhp_head"),
                uiOutput("hp_zone_lhp_strip")
              )
            ),

            # ---------------------- BATTED BALL ----------------------
            tabPanel("Batted Ball", value = "hp_batted",
              br(),
              fluidRow(
                column(7, div(class = "pp-card",
                         div(class = "pp-card-title", "Spray Chart"),
                         plotly::plotlyOutput("hp_spray_plotly", height = "520px",
                                              width = "100%"))),
                column(5, div(class = "pp-card",
                         div(class = "pp-card-title", "Exit Velocity / Launch Angle"),
                         plotOutput("hp_radial_ev", height = "520px")))
              ),
              fluidRow(
                column(6, div(class = "pp-card",
                         div(class = "pp-card-title", "Batted Ball Distribution"),
                         div(style = "display:flex;justify-content:center;margin:-4px 0 8px;",
                             div(class = "pp-seg",
                                 radioGroupButtons("hp_sector_scope", label = NULL,
                                                   choices = c("All BIP" = "all",
                                                               "Ground Balls" = "gb",
                                                               "Air Balls" = "air"),
                                                   selected = "all", size = "xs",
                                                   individual = TRUE))),
                         plotOutput("hp_sector_spray", height = "480px"))),
                column(6, div(class = "pp-card",
                         div(class = "pp-card-title", "Contact Points"),
                         plotOutput("hp_contact_map", height = "480px")))
              ),
              fluidRow(column(12, div(class = "pp-card",
                div(class = "pp-card-title", "Batted Ball Profile"),
                div(style = "color:#8A93A0;font-size:11.5px;text-align:center;margin:-6px 0 8px;",
                    "Launch angle bucket by spray direction, compared with the pool. ",
                    "Green = more often than the pool, red = less often."),
                plotOutput("hp_bb_profile", height = "480px"))))
            ),

            # ------------------------ MODELING -----------------------
            tabPanel("Modeling", value = "hp_modeling",
              br(),
              div(class = "pp-card",
                div(class = "pp-card-title", "Process+ \u2014 Rolling Window"),
                fluidRow(
                  column(4, sliderInput("hp_roll_window", "Rolling window (pitches)",
                                        min = 50, max = 600, value = 250, step = 25)),
                  column(4, dateRangeInput("hp_mod_dates", "Date Range",
                                           start = NULL, end = NULL)),
                  column(4, div(class = "pp-seg",
                            tags$label(class = "control-label", "Pitcher Hand"),
                            radioGroupButtons("hp_mod_side", label = NULL,
                                              choices = c("Both" = "B", "vs RHP" = "R",
                                                          "vs LHP" = "L"),
                                              selected = "B", size = "sm",
                                              individual = TRUE)))
                ),
                plotOutput("hp_rolling_process", height = "460px"),
                div(style = "color:#8A93A0;font-size:11.5px;text-align:center;margin-top:6px;",
                    "Stacked contribution of Decisions, Contact and Power above/below 100; ",
                    "the white line is overall Process+.")
              ),
              div(class = "pp-card",
                div(class = "pp-card-title", "Model Values by Split"),
                fluidRow(
                  column(4, div(class = "pp-sec-lab", "Overall"),
                            div(class = "pp-model-tbl", gt::gt_output("hp_model_tbl_all"))),
                  column(4, div(class = "pp-sec-lab", "vs RHP"),
                            div(class = "pp-model-tbl", gt::gt_output("hp_model_tbl_r"))),
                  column(4, div(class = "pp-sec-lab", "vs LHP"),
                            div(class = "pp-model-tbl", gt::gt_output("hp_model_tbl_l")))
                )
              )
            )
          )
        ),
        # Position-dependent defense tabs — shown/hidden per hitter based
        # on DRS26 + CCU bio positions (OF -> Outfield, INF -> Infield,
        # C -> Catching). All hidden until a hitter with that position
        # is selected.
        tabPanel("Infield Defense", value = "hit_inf",
          uiOutput("hp_def_inf")),
        tabPanel("Outfield Defense", value = "hit_of",
          uiOutput("hp_def_of")),
        tabPanel("Catching", value = "hit_c",
          uiOutput("hp_def_c"))
      )
    ),
    # ========================== LEADERBOARDS ==========================
    # One top-level tab; the three boards are pills inside it, so the main
    # bar stays a single row.
    tabPanel("Leaderboards", value = "Leaderboards",
      tags$style(LB_CSS),
      tabsetPanel(id = "lb_subtabs", type = "pills",
tabPanel('Bullpen Leaderboard',
             palace_bullpen_leaderboard_ui("bplb")
),
tabPanel('Pitching Leaderboard',

             fluidRow(
               column(3, dateRangeInput("dateRange_leader", "Date Range", 
                                        start = Sys.Date() - 365, 
                                        end = Sys.Date())),
               column(2, pickerInput(
                 inputId = "BatterSide_leader",
                 label = HTML("Batter Hand"),
                 choices = c("Left", "Right"),
                 selected = c("Left", "Right"),
                 options = list(`actions-box` = TRUE,
                                `selectAllText` = "Select all",
                                `deselectAllText` = "Deselect all"),
                 multiple = TRUE
               )),
               column(2, pickerInput(
                 inputId = "PitcherHand_leader",
                 label = HTML("Pitcher Hand"),
                 choices = c("Left", "Right"),
                 selected = c("Left", "Right"),
                 options = list(`actions-box` = TRUE,
                                `selectAllText` = "Select all",
                                `deselectAllText` = "Deselect all"),
                 multiple = TRUE
               )),
               column(2, pickerInput(
                 inputId = "pitchtype_leader",
                 label = HTML("Pitch Type"),
                 choices = c("Fastball", "Sinker", "Cutter", "Curveball", "Slider", "ChangeUp", "Splitter", "Sweeper"),
                 selected = c("Fastball", "Sinker", "Cutter", "Curveball", "Slider", "ChangeUp", "Splitter", "Sweeper"),
                 options = list(`actions-box` = TRUE,
                                `selectAllText` = "Select all",
                                `deselectAllText` = "Deselect all"),
                 multiple = TRUE
               )),
               column(2, pickerInput(
                 inputId = "Roster_2026_leader",
                 label = HTML("2026 Roster"),
                 choices = c("Yes", "No"),
                 selected = c("Yes", "No"), 
                 options = list(`actions-box` = TRUE,
                                `selectAllText` = "Select all",
                                `deselectAllText` = "Deselect all"),
                 multiple = TRUE
               ))
             ),
             fluidRow(
               column(3, pickerInput(
                 inputId = "pitcher_leader",
                 label = HTML("Pitcher"),
                 choices = NULL,
                 selected = NULL,
                 options = list(`actions-box` = TRUE,
                                `selectAllText` = "Select all",
                                `deselectAllText` = "Deselect all",
                                `live-search` = TRUE),
                 multiple = TRUE
               )),
               column(2, selectInput(
                 inputId = "team_filter_leader",
                 label = HTML("Show"),
                 choices = c("Individual Players" = "players",
                             "Team Summary" = "team",
                             "Both" = "both"),
                 selected = "both"
               ))
             ),
             mainPanel(
               width = 12,
               conditionalPanel(
                 condition = "input.team_filter_leader == 'players' || input.team_filter_leader == 'both'",
                 h4("Individual Player Statistics"),
                 dataTableOutput("statistics_table")
               ),
               conditionalPanel(
                 condition = "input.team_filter_leader == 'team' || input.team_filter_leader == 'both'",
                 h4("Team Summary"),
                 dataTableOutput("total_table")
               )
             )
    ),
    # ============ LEAGUE LEADERBOARDS (Pitchers / Pitches) ============
    # Same engine, two grains: "Pitchers" is one row per arm, "Pitches"
    # is that arm split by pitch type. Both are league wide off the NCAA
    # parquet datasets, as opposed to the Coastal board above.
    tabPanel("League Pitchers", value = "League Pitchers",
             tags$style(LB_CSS),
             lb_board_ui("pitchers", "Pitchers \u2014 League Wide",
                         "Live from TruMedia. Pick a season and teams, then Build Board.")
    ),
    tabPanel("League Pitches", value = "League Pitches",
             lb_board_ui("pitches", "Pitches \u2014 League Wide",
                         "One row per arm per pitch type \u2014 requires pitch-level load.")
    )
      )
    ),
    # ===================== ADVANCE (gameday dugout material) =====================
    # ============================ SCHEDULE ============================
    # Bullpen calendar (who threw, how much, when) now; the season game
    # schedule joins it here once games start.
    tabPanel("Schedule", value = "Schedule",
      tabsetPanel(id = "sched_subtabs", type = "pills",
        tabPanel("Bullpens", value = "sched_bullpens",
          palace_bullpen_history_ui("sched")),
        tabPanel("Season", value = "sched_season",
          div(style = "padding:48px; text-align:center; color:#999;",
              icon("calendar-days"),
              tags$p(style = "margin-top:8px;",
                     "Season schedule coming soon.")))
      )
    ),

    tabPanel("Advance", value = "Advance",
      div(style = "text-align:center; margin:16px 0 4px;",
          h3("Advance", class = "brand-teal", style = "margin-bottom:2px;"),
          div(style = "color:#6B7280; font-size:13px;",
              "Matchup projections and gameday dugout material.")),
      tabsetPanel(id = "advance_subtabs", type = "pills",

        # ------------- TEAM PITCHER REPORTS (Scouting Workbook) -------------
        tabPanel("Team Pitcher Reports", value = "adv_teamreports",
              div(style = "text-align:center; margin:14px 0 2px; color:#6B7280; font-size:13px;",
                  "Load an opponent staff, drag pitchers into report order, assign roles, ",
                  "then export one Excel workbook with a scouting card per arm."),
              fluidRow(
                column(4, selectizeInput("swb_team", "Opponent Team:",
                                         choices = NULL, width = "100%",
                                         options = list(placeholder = "Type to search teams..."))),
                column(3, div(style = "padding-top:25px;",
                              actionButton("swb_load", class = "tp-spin-on-click", "Load Staff", `data-tip` = "Load the selected staff into the workbook",
                                           style = "background:#0E6E70; color:#fff; border:none; width:100%;"))),
                column(5, div(style = "padding-top:25px;",
                              downloadButton("swb_download", "Export Scouting Workbook (.xlsx)", `data-tip` = "Export one Excel sheet per selected arm",
                                             style = "width:100%;")))
              ),
              fluidRow(
                column(5,
                  selectizeInput("swb_order", "Report Order (drag to reorder):",
                                 choices = NULL, selected = NULL, multiple = TRUE,
                                 width = "100%",
                                 options = list(plugins = list("drag_drop", "remove_button"),
                                                placeholder = "Load a staff first..."))),
                column(7, uiOutput("swb_staff_ui"))
              )
        ),

        # -------------------- MATCHUP MATRIX --------------------
        tabPanel("Matchup Matrix", value = "adv_matrix",
          fluidRow(
            column(3, pickerInput("mm_pitcher_team", "Pitcher Team",
                                  choices = NULL, selected = NULL,
                                  options = list(`live-search` = TRUE,
                                                 `none-selected-text` = "Select team"),
                                  multiple = FALSE)),
            column(3, selectizeInput("mm_pitchers", "Pitchers", choices = NULL,
                                     selected = NULL, multiple = TRUE, width = "100%",
                                     options = list(plugins = list("drag_drop", "remove_button"),
                                                    placeholder = "Roster loads from TruMedia..."))),
            column(3, pickerInput("mm_batter_team", "Batter Team",
                                  choices = NULL, selected = NULL,
                                  options = list(`live-search` = TRUE,
                                                 `none-selected-text` = "Select team"),
                                  multiple = FALSE)),
            column(3, selectizeInput("mm_batters", "Batters", choices = NULL,
                                     selected = NULL, multiple = TRUE, width = "100%",
                                     options = list(plugins = list("drag_drop", "remove_button"),
                                                    placeholder = "Roster loads from TruMedia...")))
          ),
          fluidRow(
            column(2, sliderInput("mm_pitcher_weight", "Pitcher Weight",
                                  min = 0, max = 1, value = 0.5, step = 0.05)),
            column(2, sliderInput("mm_sim_threshold", "Similarity Radius",
                                  min = 0.5, max = 4, value = 2, step = 0.25)),
            column(2, radioGroupButtons("mm_view", "Perspective",
                                        choices = c("Pitcher" = "pitcher",
                                                    "Hitter"  = "hitter"),
                                        selected = "pitcher", size = "sm"),
                      checkboxInput("mm_split", "Decision / Damage split",
                                    value = FALSE)),
            column(3, div(style = "padding-top:25px;",
                          actionButton("mm_build", class = "tp-spin-on-click", "Generate Matrix", `data-tip` = "Generate the matchup matrix for the selected lineup",
                                       style = "background:#0E6E70; color:#fff; border:none; width:100%;"))),
            column(3, div(style = "padding-top:25px;",
                          downloadButton("mm_download_pdf", "Download PDF", `data-tip` = "Download the matchup matrix as a PDF",
                                         style = "width:100%;")))
          ),
          # Detailed view + lineup/rest editor live on THIS page rather than
          # a second tab: the matrix and the hitter card are the same read,
          # so splitting them meant loading the same roster twice.
          fluidRow(
            column(4, div(style = "padding-top:6px;",
              checkboxInput("mm_detailed",
                            HTML("<b>Detailed matrix</b> \u2014 include hitter-card details when generated"),
                            value = TRUE))),
            column(4, div(style = "padding-top:8px;",
              actionLink("mm_toggle_lineup",
                         HTML("&#9776; Edit lineup, rest &amp; export"),
                         style = "font-weight:600; color:#0E6E70;"))),
            column(4, div(style = "padding-top:6px;", uiOutput("mm_rest_note")))
          ),
          conditionalPanel(
            condition = "output.mm_lineup_open == true",
            div(style = paste0("background:#F7FAFA; border:1px solid #DCE7E7;",
                               "border-radius:10px; padding:12px 16px; margin:6px 0 12px;"),
              div(style = "font-weight:700; color:#0E6E70; margin-bottom:8px;",
                  "Lineup order is the Batters box above \u2014 drag to set it. ",
                  "The first nine are treated as starters."),
              fluidRow(
                column(3, dateInput("thr_game_date", "Game Date:",
                                    value = Sys.Date(), width = "100%")),
                column(3, numericInput("thr_rested_days",
                                       "Rested = no appearance in (days) \u2265",
                                       value = 3, min = 1, max = 10, step = 1)),
                column(3, numericInput("thr_nonrested_days",
                                       "Non-rested = pitched within (days) \u2264",
                                       value = 2, min = 0, max = 9, step = 1)),
                column(3, div(style = "padding-top:26px;",
                              downloadButton("thr_download",
                                             "Export Hitter Workbook (.xlsx)",
                                             style = "width:100%;")))
              ),
              uiOutput("thr_staff_ui")
            )
          ),
          div(style = "color:#6B7280; font-size:12px; margin:0 0 8px;",
              "0-100 scores. Pitcher facing: 100 = pitcher advantage. ",
              "Hitter facing: 100 = best hitter matchup. Mini pills rank the ",
              "arsenal by RV vs similar pitches, best first (green). ",
              "Dashed cells are low confidence. Click any cell for the matchup detail."),
          div(class = "tp-skel-host",
              div(class = "tp-skel-ph",
                  tp_skeleton_table(rows = 8)),
              uiOutput("mm_matrix_ui")),
          uiOutput("mm_overview_notes"),
          div(id = "mm-detail-anchor"),
          uiOutput("mm_detail_ui")
        ),

        # ------------- BETA: MATCHUP+ ADVANCED MATRIX -------------
        # Same similarity engine, priced with the hitter Process model
        # instead of DRE. Reads the SAME team/roster selections as the
        # Matchup Matrix tab so the two are a true side-by-side read.
        tabPanel(HTML("Beta: Unified Matchup Matrix"), value = "adv_matrix_plus",
          div(style = "text-align:center; margin:14px 0 2px; color:#6B7280; font-size:13px;",
              "For each pitch in the arm's arsenal, find the pitches this hitter has ",
              "actually seen that look like it, then grade him on them. ",
              tags$b("Process+"), " grades how he handles that shape \u2014 decision, ",
              "contact, power, luck removed. ", tags$b("DRE"), " is the traditional ",
              "score: what he really produced. ",
              tags$span(class = "mmp-beta-tag", "Beta")),
          div(style = "text-align:center; margin:2px 0 10px; color:#9AA1A6; font-size:12px;",
              "Teams and rosters come from the ", tags$b("Matchup Matrix"),
              " tab \u2014 set them there, then build here."),
          fluidRow(
            column(3, div(style = "padding-top:4px;", uiOutput("mmp_selection_note"))),
            column(2, sliderInput("mmp_sim_threshold", "Similarity Radius",
                                  min = 0.5, max = 4, value = 2, step = 0.25)),
            column(2, sliderInput("mmp_shrink", "Platoon Shrinkage (k)",
                                  min = 0, max = 200, value = 60, step = 10)),
            column(2, radioGroupButtons("mmp_view", "Perspective",
                                        choices = c("Hitter"  = "hitter",
                                                    "Pitcher" = "pitcher"),
                                        selected = "hitter", size = "sm"),
                      radioGroupButtons("mmu_primary", "Score",
                                        choices = c("Process+" = "predicted",
                                                    "DRE"      = "actual",
                                                    "Both"     = "both"),
                                        selected = "predicted", size = "sm"),
                      checkboxInput("mmp_relative",
                                    "Colour vs this grid, not the league",
                                    value = FALSE)),
            column(3, div(style = "padding-top:25px;",
                          actionButton("mmp_build", class = "tp-spin-on-click",
                                       "Build Matchup+ Matrix",
                                       `data-tip` = paste0("Run the process-model matrix over the ",
                                                           "Matchup Matrix selections"),
                                       style = "background:#0E6E70; color:#fff; border:none; width:100%;")),
                      div(style = "padding-top:8px;",
                          downloadButton("mmp_download_csv", "Download CSV",
                                         `data-tip` = "Export every cell with its layer breakdown",
                                         style = "width:100%;")))
          ),
          fluidRow(column(12, div(style = "margin:2px 0 6px;",
            checkboxGroupInput("mmu_layers", NULL, inline = TRUE,
              choices = c("Dec / Con / Pow layers" = "layers",
                          "Per-pitch arsenal"      = "arsenal",
                          "Actual DRE score"       = "dre",
                          "Head-to-head history"   = "h2h",
                          "Detailed hitter profile" = "profile",
                          "Heatmaps in detail view" = "heatmaps"),
              selected = c("layers", "arsenal", "profile"))))),
          div(class = "mmp-legend",
              tags$span(tags$b("\u2192 pill"), " on a 1P or 2K row is a pitch to call in ",
                        "that count \u2014 only shown when one offering is a clear ",
                        "outlier he struggles with, not merely his worst."),
              tags$span(tags$b("Process+"), " 100 = average hitter, 10 = 1 SD. ",
                        tags$b("DRE"), " 0-100. Both hitter-facing here \u2014 higher ",
                        "favours the bat \u2014 so the DRE number is the complement of the ",
                        "pitcher-facing value on the Matchup Matrix tab."),
              tags$span(tags$b("DEC / CON / POW"),
                        " — the three layers underneath. Their run-value rates ",
                        "sum to Process+, but each pill is scaled against its own ",
                        "layer distribution, so the numbers won't add up on screen."),
              tags$span("Numbers are always hitter-scale; the perspective toggle flips ",
                        tags$b("color"), " only, so green = the side you picked is winning."),
              tags$span("Dashed cells lean on the platoon prior (thin similar-pitch sample). ",
                        "Click any cell for the per-pitch breakdown.")),
          div(class = "tp-skel-host",
              div(class = "tp-skel-ph", tp_skeleton_table(rows = 8)),
              uiOutput("mmp_matrix_ui")),
          uiOutput("mmp_notes"),
          div(id = "mmp-detail-anchor"),
          uiOutput("mmp_detail_ui")
        ),

        # -------------------- MATCHUP SPRAY --------------------
        tabPanel("Matchup Spray", value = "adv_spray",
          fluidRow(
            column(3, pickerInput("ms_pitcher_team", "Pitcher Team",
                                  choices = NULL, selected = NULL,
                                  options = list(`live-search` = TRUE,
                                                 `none-selected-text` = "Select team"),
                                  multiple = FALSE)),
            column(3, selectizeInput("ms_pitchers", "Pitchers", choices = NULL,
                                     selected = NULL, multiple = TRUE, width = "100%",
                                     options = list(plugins = list("drag_drop", "remove_button"),
                                                    placeholder = "Roster loads from TruMedia..."))),
            column(3, pickerInput("ms_batter_team", "Batter Team",
                                  choices = NULL, selected = NULL,
                                  options = list(`live-search` = TRUE,
                                                 `none-selected-text` = "Select team"),
                                  multiple = FALSE)),
            column(3, selectizeInput("ms_batters", "Batters", choices = NULL,
                                     selected = NULL, multiple = TRUE, width = "100%",
                                     options = list(plugins = list("drag_drop", "remove_button"),
                                                    placeholder = "Roster loads from TruMedia...")))
          ),
          fluidRow(
            column(2, sliderInput("ms_sim_threshold", "Similarity Radius",
                                  min = 0.5, max = 4, value = 2, step = 0.25)),
            column(3, div(style = "padding-top:25px;",
                          actionButton("ms_build", class = "tp-spin-on-click", "Build Spray Grid", `data-tip` = "Build the spray grid for the selected matchup",
                                       style = "background:#0E6E70; color:#fff; border:none; width:100%;"))),
            column(3, div(style = "padding-top:25px;",
                          downloadButton("ms_download_pdf", "Download PDF", `data-tip` = "Download the spray grid as a PDF",
                                         style = "width:100%;"))),
            column(4, div(style = "padding-top:32px; color:#6B7280; font-size:12px;",
                          "Balls in play from each batter's similar-pitch pool vs each ",
                          "arm's arsenal — Overall / Pre-2K / 2-Strike sections."))
          ),
          uiOutput("ms_grid_ui")),

        # -------------------- PITCH TYPE PROJECTIONS (in development) --------------------
        tabPanel("Pitch Type Projections", value = "adv_ptproj",
          div(class = "adv-placeholder",
              h4("Pitch Type Projections"),
              p("One-number summary for how good a particular batter/pitcher is at ",
                "facing/throwing one of six pitch classifications (FB4, FB2, CT, SL, ",
                "CB, CH), generated with a latent variable structure so performance ",
                "against one pitch type informs projections for the others — including ",
                "largely unobserved pitch types for pitchers."),
              p("Built from pitch values (per-type and overall), the correlation ",
                "structure of individual pitch type performance, and mean regression. ",
                "Applications range from hypothetical pitch type performance for a ",
                "pitcher who doesn't throw that pitch, to a one-number strategy summary ",
                "for what to throw a batter that day, summarized in pitch type cards."),
              p(em("In development — data pipeline coming soon.")))),

        # -------------------- PITCH OUTCOMES (in development) --------------------
        tabPanel("Pitch Outcomes", value = "adv_outcomes",
          div(class = "adv-placeholder",
              h4("Pitch Outcomes"),
              p("Predicted pitch outcomes for specific batter-pitcher matchups across ",
                "strike-zone locations, pitch types, and counts. Spatial pitch outcome ",
                "models output predictions for swing, contact, balls in play, and ",
                "contact quality from the pitch trajectory, combined into an overall ",
                "expected run value for each pitch."),
              p("The visuals reveal spatial patterns — where to steal a first-pitch ",
                "strike, or what pitch type / location a batter struggles with late in ",
                "the count — for data-driven pitch selection, location, and player ",
                "development. Alongside overall value plots: swing probability, swing ",
                "decision, and swing value plots."),
              p(em("In development — data pipeline coming soon.")))),

        # -------------------- PITCH ARSENAL (optimal pitch-mix diagnostic) --------------------
        tabPanel("Pitch arsenal", value = "adv_arsenal",
          br(),
          fluidRow(column(12, uiOutput("pa_header"))),
          fluidRow(column(12,
            div(style = paste("background:#f8f9fa; border:1px solid #e5e7eb;",
                              "border-radius:8px; padding:10px 14px; margin-bottom:10px;"),
                div(style = "font-weight:600; font-size:13px; margin-bottom:4px;",
                    "Pitches included"),
                div(style = "font-size:11.5px; color:#6B7280; margin-bottom:6px;",
                    paste("Untick anything mis-tagged (e.g. a splitter logged as a",
                          "changeup). Those pitches are re-tagged onto the pitch",
                          "they most resemble by shape, not discarded.")),
                uiOutput("pa_pitch_picker"),
                uiOutput("pa_retag_note")))),
          fluidRow(column(12, plotOutput("pa_mix_plot", height = "360px"))),
          br(),
          fluidRow(column(12, uiOutput("pa_diag"))),
          br(),
          fluidRow(column(12,
            h4("Modeled run-value impact of the usage change", class = "brand-teal"),
            div(style = "font-size:12px; color:#666; margin-bottom:6px;",
                paste("Value/100 = count-adjusted run value per 100 pitches for that",
                      "pitch shape vs the league (higher = better for the pitcher);",
                      "Thrown = sample size behind it. Modeled estimates, biased toward",
                      "the status quo \u2014 read as 'worth a look', not 'throw this",
                      "instead'. Negative Modeled RV = fewer runs allowed (good).")),
            tableOutput("pa_rv_table"),
            tags$details(style = "margin-top:10px;",
              tags$summary(style = "cursor:pointer; font-size:12px; color:#6B7280;",
                           "Model internals (diagnostics)"),
              verbatimTextOutput("pa_debug"))))
        )
      )
    ),

    # ===================== REPORTS (team-level report artifacts) =====================
    tabPanel("Reports", value = "ReportsTab",
      div(style = "text-align:center; margin:16px 0 4px;",
          h3("Reports", class = "brand-teal", style = "margin-bottom:2px;"),
          div(style = "color:#6B7280; font-size:13px;",
              "Team usage reports and spray charts.")),
      tabsetPanel(id = "reports_subtabs", type = "pills",
    tabPanel(
      "Count Usage Report",
      fluidRow(
        column(3, dateRangeInput("dateRange_countreport", "Date Range",
                                 start = Sys.Date() - 365, end = Sys.Date())),
        column(2, pickerInput(
          inputId = "Roster_2026_countreport",
          label   = HTML("2026 Roster"),
          choices = c("Yes", "No"),
          selected = c("Yes", "No"), 
          options = list(`actions-box` = TRUE),
          multiple = TRUE
        )),
        column(4, pickerInput(
          inputId  = "pitcher_countreport",
          label    = HTML("Select Pitchers (up to 20)"),
          choices  = NULL,
          selected = NULL,
          options  = list(
            `actions-box`      = TRUE,
            `selectAllText`    = "Select all",
            `deselectAllText`  = "Deselect all",
            `live-search`      = TRUE,
            `max-options`      = 20,
            `max-options-text` = "Maximum 20 pitchers"
          ),
          multiple = TRUE
        )),
        column(3,
               div(style = "padding-top: 25px;",
                   downloadButton("download_count_report", "Download PDF Report", `data-tip` = "Download the count usage report as a PDF",
                                  class = "btn-primary", style = "width:100%;"))
        )
      ),
      fluidRow(
        column(12,
               h4("Preview"),
               plotOutput("count_report_preview", height = "900px")
        )
      )
    ),
    tabPanel('vCCU Spray Charts',
             fluidPage(
               fluidRow(
                 column(3, pickerInput(
                   inputId = "pitcher_spray",
                   label = "Pitcher",
                   choices = NULL, selected = NULL, multiple = TRUE,
                   options = list(`actions-box` = TRUE,
                                  `selectAllText` = "Select all",
                                  `deselectAllText` = "Deselect all",
                                  `live-search` = TRUE)
                 )),
                 column(2, pickerInput(
                   inputId = "BatterSide_spray", label = HTML("Batter Hand"),
                   choices = c("Left","Right"), selected = c("Left","Right"),
                   options = list(`actions-box` = TRUE), multiple = TRUE
                 )),
                 column(3, dateRangeInput("dateRange_spray", "Date Range",
                                          start = Sys.Date() - 365, end = Sys.Date())),
                 column(2, pickerInput(
                   inputId = "pitchtype_spray", label = HTML("Pitch Type"),
                   choices = c("Fastball","Sinker","Cutter","Curveball","Slider","ChangeUp","Splitter","Sweeper"),
                   selected = c("Fastball","Sinker","Cutter","Curveball","Slider","ChangeUp","Splitter", "Sweeper"),
                   options = list(`actions-box` = TRUE), multiple = TRUE
                 )),
                 column(2, pickerInput(
                   inputId = "Roster_2026_spray", label = HTML("2026 Roster"),
                   choices = c("Yes","No"), selected = c("Yes", "No"), 
                   options = list(`actions-box` = TRUE), multiple = TRUE
                 ))
               ),
               fluidRow(
                 column(12, plotOutput('spraychart_main', width = '100%', height = '800px'))
               )
             )
    )
      )
    )
  )
)

# ===========================================================================
