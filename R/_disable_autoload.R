# Shiny sources R/*.R automatically before app.R runs, ahead of every library() call.
# app.R sources these files itself, in order, so autoload is turned off here.
