server <- function(input, output, session) {

  # Create reactive values to store the current history
  current_r_history <- reactiveVal(character())
  current_bash_history <- reactiveVal(character())

  # Create reactive readers for the history files
  r_history <- reactiveFileReader(
    intervalMillis = 1000,
    session = session,
    filePath = "r_history.txt",
    readFunc = readLines
  )
  
  bash_history <- reactiveFileReader(
    intervalMillis = 1000,
    session = session,
    filePath = "bash_history.txt",
    readFunc = readLines
  )
  
  archived_rules <- reactiveVal(character())
  
  # Initialize the current history with the content of the history files
  observe({
    if (file.exists("r_history.txt")) {
      current_r_history(readLines("r_history.txt"))
    }
    if (file.exists("bash_history.txt")) {
      current_bash_history(readLines("bash_history.txt"))
    }
  })
  
  # Update the select inputs based on the current history, excluding archived rules
  observe({
    filtered_r_history <- setdiff(current_r_history(), archived_rules())
    filtered_bash_history <- setdiff(current_bash_history(), archived_rules())
    updateSelectInput(session, "selected_line", choices = filtered_r_history)
    updateSelectInput(session, "selected_term", choices = filtered_bash_history)
  })
  
  observeEvent(input$open_settings, {
    selected_model <- if (file.exists("selected_model.txt")) {
      readLines("selected_model.txt", warn = FALSE)
    } else {
      "llama3-8b-8192"
    }
    showModal(modalDialog(
      title = "Configuration",
      fluidPage(
        fluidRow(
          column(6,
                 selectInput("selected_model_options", "Select Model to Use:",
                             choices = c("llama3-8b-8192", "nvidia/llama-3.1-nemotron-70b-instruct"),
                             selected = selected_model,
                             width = "100%")
          ),
          column(6,
                 actionButton("change_key_options",
                              span(icon("edit"), " Change API key"),
                              class = "btn btn-info",
                              style = "width:100%")
          )
        )
      ),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("save_model_options", "Save", class = "btn btn-primary")
      ),
      class = "modal-content"
    ))
  })

  observeEvent(input$change_key_options, {
    showModal(modalDialog(
      title = "Change API Key",
      "This will delete the currently stored API key for the selected model. The addin will restart immediately and next time you generate a rule, you will be prompted for a new key.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_change_key_options", "Confirm")
      ),
      class = "modal-content"
    ))
  })

  observeEvent(input$confirm_change_key_options, {
    model_key <- isolate(input$selected_model_options)
    Sys.unsetenv(model_key)
    flag_file <- paste0("delete_api_key_", model_key, ".txt")
    writeLines("delete", flag_file)
    removeModal()
    writeLines("restart", "restart_flag.txt")
    stopApp()
    later::later(function() {
      my_addin()
    }, delay = 0.1)
  })

  observeEvent(input$save_model_options, {
    selected_model <- isolate(input$selected_model_options)
    writeLines(selected_model, "selected_model.txt")
    showNotification("Model selection saved.", duration = 2)
    removeModal()
  })

  observeEvent(input$insert, {
    selected_line_raw <- if (input$menu_choice == "history")
      input$selected_line else input$selected_term
    selected_line <- if (is.null(selected_line_raw)) "" else paste(selected_line_raw, collapse = "\n")
    if (length(selected_line) > 0L && selected_line != "") {
      writeLines(selected_line, con = "selected_line.txt")
      current_archived_rules <- archived_rules()
      if (!selected_line %in% current_archived_rules) {
        archived_rules(c(current_archived_rules, selected_line))
        updateSelectInput(session, "archived_rules_select", choices = archived_rules())
      }
      # Remove the parsed command from history if coming from the history box
      if (input$menu_choice == "history") {
        updated_history <- setdiff(current_r_history(), selected_line)
        current_r_history(updated_history)
        updateSelectInput(session, "selected_line", choices = updated_history)
      }
    }
  })

  observeEvent(input$refresh, {
    # Update the current history with the content of the history files
    if (file.exists("r_history.txt")) {
      current_r_history(readLines("r_history.txt"))
    }
    if (file.exists("bash_history.txt")) {
      current_bash_history(readLines("bash_history.txt"))
    }
    showNotification("History updated.", duration = 2)
  })

  observeEvent(input$close, {
    # Remove all .txt files in the current directory
    txt_files <- list.files(pattern = "\\.txt$", full.names = TRUE)
    if (length(txt_files) > 0) {
      file.remove(txt_files)
    }
    stopApp()
  })

  observeEvent(input$parse_again, {
    selected_rule <- input$archived_rules_select
    if (length(selected_rule) > 0L && selected_rule != "") {
      writeLines(selected_rule, con = "selected_line.txt")
      source_flag <- readLines("source_flag.txt", warn = FALSE)
      showNotification(paste("Command taken from:", source_flag), duration = 2)
    }
  })

  output$archived_rules_output <- renderText({
    paste(archived_rules(), collapse = "\n")
  })
}

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
