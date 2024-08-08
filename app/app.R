# this is for local use of an application - not for deployment.
# when deploying with the multiple file set up (ui.R, server.R)
# you need to ensure your scripts etc. are all sourced - good place for this
# is global.R which is automatically sourced.

library(shiny)
library(here)
library(magrittr)

# Temporary to allow for live reloads in an RStudio background job or app running in a terminal:
options(shiny.autoreload=TRUE)
options(shiny.port = 7775)
options(shiny.host = "127.0.0.1")

# Source Business logic/helper functions
source(here("app/R/functions.R"))

# Source modules
source(here("app/modules/embed_posts_module.R"))
source(here("app/modules/permalink_upload_module.R"))

# Source App Files
source(here("app/ui.R"))
source(here("app/server.R"))

if(interactive()){
  shinyApp(ui, server)
} else {
  shiny::runApp(here::here("app"))
}
