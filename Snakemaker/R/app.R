library(shiny)
library(callr)
library(later)
library(rstudioapi)
library(httr)
library(jsonlite)
library(bslib)

source("R/ui.R")
source("R/server.R")
source("R/addin.R")

shinyApp(
  ui = create_ui(
    history = character(),    # Initially empty; updated via reactiveFileReader
    term_history = character(), # Initially empty; updated via reactiveFileReader
    archived_rules = character(),
    selected_model = if (file.exists("selected_model.txt")) {
      readLines("selected_model.txt", warn = FALSE)
    } else {
      "llama3-8b-8192"
    }
  ),
  server = server
)
