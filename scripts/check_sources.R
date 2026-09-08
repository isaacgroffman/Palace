#!/usr/bin/env Rscript
# Parse every R file in the repo. Cheap pre-commit check for syntax errors.
files <- c("app.R", list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE),
           list.files("scripts", pattern = "\\.R$", full.names = TRUE))
bad <- character(0)
for (f in files) {
  msg <- tryCatch({ parse(f, keep.source = FALSE); NULL }, error = function(e) conditionMessage(e))
  if (!is.null(msg)) { bad <- c(bad, f); cat("PARSE ERROR ", f, "\n  ", msg, "\n") }
}
cat(length(files), "files checked,", length(bad), "with errors\n")
quit(status = if (length(bad)) 1 else 0)
