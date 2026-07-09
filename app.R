# app.R: entry point for Posit Connect Cloud.
# The app itself lives in global.R (data + functions), ui.R (layout),
# and server.R (logic); this file just assembles them.

library(shiny)
source("global.R", local = FALSE)
source("ui.R",     local = FALSE)
source("server.R", local = FALSE)

shinyApp(ui = ui, server = server)
