# This assumes we're in a deployed environment, so we'll use file paths that start at app/
library(shiny)
library(magrittr)

source("R/functions.R")
source("modules/embed_posts_module.R")
source("modules/permalink_upload_module.R")
