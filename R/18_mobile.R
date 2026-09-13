# =============================================================================
# R/18_mobile.R — phone / small-tablet layout.
#
# Everything here is scoped to `@media (max-width: 768px)` (plus a little JS
# that only runs on those widths), so the desktop layout is untouched. The
# rules stack the header, make the tab bars swipeable, cap chart heights,
# put every table in a horizontal scroller, size inputs so iOS does not zoom
# and turn the side "Filters" tab into a round bottom-right button.
# =============================================================================

MOBILE_BREAK <- 768

MOBILE_CSS <- HTML(sprintf("
@media (max-width: %dpx) {
  html { -webkit-text-size-adjust: 100%%; }
  body { font-size: 14px; }
  .container-fluid { padding-left: 0 !important; padding-right: 0 !important; }
  .row { margin-left: -6px !important; margin-right: -6px !important; }
  [class*='col-'] { padding-left: 6px !important; padding-right: 6px !important; }
  h2 { font-size: 22px; } h3 { font-size: 18px; } h4 { font-size: 15px; }

  /* inputs: 16px stops iOS zooming on focus; fatter tap targets */
  input.form-control, select.form-control, textarea.form-control,
  .selectize-input, .selectize-input input, .bootstrap-select .btn.dropdown-toggle,
  .btn, .radio-group-buttons .btn, .checkbox-group-buttons .btn { font-size: 16px !important; }
  .form-control, .selectize-input { min-height: 40px; }
  .btn-sm, .btn-group-sm > .btn { padding: 6px 10px; }
  .dropdown-menu { font-size: 15px; }
  .shiny-input-container:not(.shiny-input-container-inline) { width: 100%% !important; max-width: none; }

  /* ---- header: brand, search, season, user stack; nothing overlaps ---- */
  .app-header { flex-direction: column; align-items: stretch; gap: 10px; padding: 10px 12px 12px;
                margin: 8px 8px 12px; border-radius: 12px; }
  .header-left { flex-direction: column; align-items: stretch; gap: 8px; width: 100%%; }
  .header-brand { justify-content: center; }
  .header-brand-logo { width: 170px; }
  .header-controls { flex-direction: column; width: 100%%; gap: 8px; }
  #header-player-search, #header-season-select { width: 100%% !important; margin: 0; }
  .header-user { position: static; flex-direction: row; flex-wrap: wrap; justify-content: center; align-items: center;
                 gap: 4px 12px; width: 100%%; padding-top: 6px; border-top: 1px solid rgba(255,255,255,.18); }
  .header-user-link { margin-left: 0; font-size: 12.5px; }

  /* ---- tab bars swipe sideways; first tab never clipped ---- */
  .nav-tabs { justify-content: flex-start !important; margin-top: 8px; padding: 0 8px;
              scroll-snap-type: x proximity; scrollbar-width: none; }
  .nav-tabs > li { scroll-snap-align: start; }
  .nav-tabs > li > a { font-size: 14px; padding: 10px 12px 11px; }
  .nav-pills { display: flex; flex-wrap: nowrap; overflow-x: auto; -webkit-overflow-scrolling: touch;
               gap: 4px; padding-bottom: 4px; scrollbar-width: none; }
  .nav-pills::-webkit-scrollbar { display: none; }
  .nav-pills > li { float: none; flex: 0 0 auto; }
  .nav-pills > li > a { white-space: nowrap; padding: 8px 12px; font-size: 14px; }

  /* ---- Filters: round button bottom-right, drawer full width ---- */
  .floating-filter-btn { top: auto; bottom: 18px; left: auto; right: 14px; transform: none; }
  .floating-filter-btn a { writing-mode: horizontal-tb; border-radius: 999px; padding: 12px 16px; font-size: 15px;
                           box-shadow: 0 4px 14px rgba(0,0,0,.28); }
  .floating-filter-btn a:hover { padding-right: 16px; }
  .filter-sidebar { width: 100%%; max-width: 100%%; }
  .filter-sidebar-body { padding-bottom: 80px; }

  /* ---- cards, tables, charts ---- */
  .pp-card { padding: 12px 12px; border-radius: 12px; margin-bottom: 10px; }
  .pp-card-title { font-size: 12px; margin-bottom: 8px; }
  .pp-bio-grid { grid-template-columns: repeat(2, 1fr); }
  .pp-bio-name { font-size: 19px; }
  .pp-pct-lab { width: 92px; font-size: 11px; }
  .pp-pct-sec:before { width: 84px; }
  .pp-pct-sep { margin-left: 92px; }
  .pp-pct-val { width: 46px; }
  .shiny-plot-output, .plotly.html-widget, .js-plotly-plot { max-width: 100%%; }
  .shiny-plot-output.mob-capped, .plotly.html-widget.mob-capped { height: 380px !important; }
  .dataTables_wrapper, .gt_table_wrapper, .shiny-html-output > .gt_table, .lb-table-wrap, .ps-card,
  .table-responsive, .tm-table-wrap { overflow-x: auto; -webkit-overflow-scrolling: touch; max-width: 100%%; }
  .shiny-html-output:has(> table), .shiny-html-output:has(> .gt_table) { overflow-x: auto; -webkit-overflow-scrolling: touch; }
  table.dataTable { font-size: 12.5px; }
  table.dataTable td, table.dataTable th { padding: 6px 8px !important; white-space: nowrap; }
  .dataTables_wrapper .dataTables_filter { float: none; text-align: left; }
  .dataTables_wrapper .dataTables_filter input { width: 100%%; margin-left: 0; }
  .dataTables_wrapper .dataTables_paginate { float: none; text-align: center; }
  .dataTables_wrapper .dataTables_info { float: none; text-align: center; }
  .gt_table { font-size: 12px !important; }

  /* season table card */
  .ps-title { font-size: 20px; }
  .ps-head { gap: 8px; }
  .ps-note { margin-left: 0; width: 100%%; }
  table.ps-table { font-size: 13px; }
  table.ps-table td, table.ps-table th { padding: 7px 6px; }

  /* roster landing: two cards per row */
  .roster-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px; padding: 0 8px; }
  .roster-card-body { padding: 4px 8px 12px; }
  .roster-card-name { font-size: 14px; }
  .roster-card-line { font-size: 11.5px; }
  .roster-card-btn { font-size: 12px; padding: 6px 8px; }

  /* league leaderboards: filters stack, table scrolls */
  .lb-filters .lb-row { gap: 10px 12px; }
  .lb-filters .lb-field, .lb-filters .lb-field.w-sm, .lb-filters .lb-field.w-lg { min-width: 0; flex: 1 1 46%%; }
  .lb-filters .lb-field.w-lg { flex-basis: 100%%; }
  .lb-explore-head { flex-direction: column; align-items: flex-start; gap: 2px; }
  .lb-explore-head h4 { font-size: 17px; }

  /* modals edge to edge */
  .modal-dialog, .modal-lg { width: auto; margin: 8px; }
  .modal-body { padding: 12px; }

  /* the startup overlay text */
  .app-loading-text { font-size: 18px; }

  /* mainPanel / sidebar layouts stack */
  .col-sm-3, .col-sm-4, .col-sm-6, .col-sm-8, .col-sm-9, .col-sm-12 { width: 100%%; }
}
@media (max-width: 480px) {
  .roster-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
  .nav-tabs > li > a { font-size: 13.5px; padding: 9px 10px 10px; }
  .header-brand-logo { width: 150px; }
  .pp-bio-grid { grid-template-columns: repeat(2, 1fr); }
}
", MOBILE_BREAK))

# On small screens: cap tall charts (then let plotly/ggplot re-render to the
# new box), keep the active tab in view, and nudge widgets to resize after
# a rotation. Nothing runs above the breakpoint.
MOBILE_JS <- HTML(sprintf("
(function(){
  var BREAK = %d;
  function mobile(){ return window.innerWidth <= BREAK; }
  function capCharts(){
    if (!mobile()) return;
    var els = document.querySelectorAll('.shiny-plot-output, .plotly.html-widget');
    var changed = false;
    Array.prototype.forEach.call(els, function(el){
      if (el.classList.contains('mob-capped')) return;
      var h = parseFloat((el.style.height || '').replace('px','')) || el.offsetHeight;
      if (h > 420) { el.classList.add('mob-capped'); changed = true; }
    });
    if (changed) setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 60);
  }
  function scrollActiveTab(){
    if (!mobile()) return;
    document.querySelectorAll('.nav-tabs > li.active > a, .nav-pills > li.active > a').forEach(function(a){
      try { a.scrollIntoView({block: 'nearest', inline: 'center', behavior: 'smooth'}); } catch(e) {}
    });
  }
  $(document).on('shiny:value', function(){ setTimeout(capCharts, 30); });
  $(document).on('shown.bs.tab', function(){ setTimeout(function(){ capCharts(); scrollActiveTab(); }, 30); });
  $(document).on('shiny:connected', function(){ setTimeout(function(){ capCharts(); scrollActiveTab(); }, 300); });
  window.addEventListener('orientationchange', function(){
    setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 250);
  });
})();
", MOBILE_BREAK))

mobile_head <- function() tagList(tags$style(MOBILE_CSS), tags$script(MOBILE_JS))
