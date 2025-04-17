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
  
  # Add reactive values to store importance classification
  r_history_importance <- reactiveVal(list())
  bash_history_importance <- reactiveVal(list())

  # Function to classify a line as important or not using the LLM
  classify_line_importance <- function(line, model = NULL) {
    if (is.null(model)) {
      model <- if (file.exists("selected_model.txt")) {
        readLines("selected_model.txt", warn = FALSE)
      } else {
        "llama3-8b-8192"
      }
    }
    prompt <- paste0(
      "I have the following history line: ", line, 
      ", you need to classify it as important or not, usually important commands have input and output or modify files. Only answer 'important' or 'not important'."
    )
    # Use the same LLM call as in addin.R (assume generate_rule or similar is available)
    # For simplicity, use a local function here (user should adapt as needed)
    api_key <- tryCatch(keyring::key_get(service = paste0("Snakemaker_", model), username = Sys.info()[["user"]]), error = function(e) "")
    url <- if (model == "nvidia/llama-3.1-nemotron-70b-instruct") {
      "https://integrate.api.nvidia.com/v1/chat/completions"
    } else {
      "https://api.groq.com/openai/v1/chat/completions"
    }
    body <- list(
      model = model,
      messages = list(list(role = "user", content = prompt))
    )
    resp <- tryCatch({
      httr::POST(
        url,
        httr::add_headers(`Content-Type` = "application/json", Authorization = paste("Bearer", api_key)),
        encode = "json",
        body = body
      )
    }, error = function(e) NULL)
    if (is.null(resp)) return("not important")
    parsed <- tryCatch(jsonlite::fromJSON(httr::content(resp, as = "text"), simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$choices) || length(parsed$choices) == 0) return("not important")
    answer <- tolower(trimws(parsed$choices[[1]]$message$content))
    if (grepl("important", answer) && !grepl("not", answer)) {
      "important"
    } else {
      "not important"
    }
  }

  # Helper to update importance for new lines
  update_importance <- function(history, importance_map, set_importance) {
    lines <- history()
    imp_map <- importance_map()
    new_lines <- setdiff(lines, names(imp_map))
    if (length(new_lines) > 0) {
      for (line in new_lines) {
        imp <- classify_line_importance(line)
        imp_map[[line]] <- imp
      }
      set_importance(imp_map)
    }
  }

  # On history update, classify new lines
  observe({
    update_importance(current_r_history, r_history_importance, r_history_importance)
    update_importance(current_bash_history, bash_history_importance, bash_history_importance)
  })

  # Helper to toggle importance
  toggle_importance <- function(line, importance_map, set_importance) {
    imp_map <- importance_map()
    current <- imp_map[[line]]
    imp_map[[line]] <- if (!is.null(current) && current == "important") "not important" else "important"
    set_importance(imp_map)
  }

  # Render R history with clickable symbols
  output$r_history_ui <- renderUI({
    lines <- setdiff(current_r_history(), archived_rules())
    imp_map <- r_history_importance()
    lapply(seq_along(lines), function(i) {
      line <- lines[i]
      sym <- if (!is.null(imp_map[[line]]) && imp_map[[line]] == "important") "★" else "⭘"
      div(
        style = "display:flex; align-items:center; margin-bottom:2px;",
        actionLink(
          inputId = paste0("toggle_r_", i),
          label = sym,
          style = "font-size:18px; margin-right:6px;"
        ),
        span(line)
      )
    })
  })

  # Render Bash history with clickable symbols
  output$bash_history_ui <- renderUI({
    lines <- setdiff(current_bash_history(), archived_rules())
    imp_map <- bash_history_importance()
    lapply(seq_along(lines), function(i) {
      line <- lines[i]
      sym <- if (!is.null(imp_map[[line]]) && imp_map[[line]] == "important") "★" else "⭘"
      div(
        style = "display:flex; align-items:center; margin-bottom:2px;",
        actionLink(
          inputId = paste0("toggle_bash_", i),
          label = sym,
          style = "font-size:18px; margin-right:6px;"
        ),
        span(line)
      )
    })
  })

  # Render clickable importance buttons for R history
  output$r_history_buttons <- renderUI({
    lines <- setdiff(current_r_history(), archived_rules())
    imp_map <- r_history_importance()
    tagList(
      lapply(seq_along(lines), function(i) {
        line <- lines[i]
        sym <- if (!is.null(imp_map[[line]]) && imp_map[[line]] == "important") "★" else "⭘"
        actionButton(
          inputId = paste0("toggle_r_", i),
          label = sym,
          style = "padding:0 6px; font-size:14px; margin-right:4px; min-width:24px; height:24px;",
          title = if (sym == "★") "Mark as not important" else "Mark as important"
        )
      })
    )
  })

  # Render clickable importance buttons for Bash history
  output$bash_history_buttons <- renderUI({
    lines <- setdiff(current_bash_history(), archived_rules())
    imp_map <- bash_history_importance()
    tagList(
      lapply(seq_along(lines), function(i) {
        line <- lines[i]
        sym <- if (!is.null(imp_map[[line]]) && imp_map[[line]] == "important") "★" else "⭘"
        actionButton(
          inputId = paste0("toggle_bash_", i),
          label = sym,
          style = "padding:0 6px; font-size:14px; margin-right:4px; min-width:24px; height:24px;",
          title = if (sym == "★") "Mark as not important" else "Mark as important"
        )
      })
    )
  })

  # Observe clicks for R history importance buttons
  observe({
    lines <- setdiff(current_r_history(), archived_rules())
    for (i in seq_along(lines)) {
      local({
        idx <- i
        line <- lines[idx]
        observeEvent(input[[paste0("toggle_r_", idx)]], {
          toggle_importance(line, r_history_importance, r_history_importance)
        }, ignoreInit = TRUE)
      })
    }
  })

  # Observe clicks for Bash history importance buttons
  observe({
    lines <- setdiff(current_bash_history(), archived_rules())
    for (i in seq_along(lines)) {
      local({
        idx <- i
        line <- lines[idx]
        observeEvent(input[[paste0("toggle_bash_", idx)]], {
          toggle_importance(line, bash_history_importance, bash_history_importance)
        }, ignoreInit = TRUE)
      })
    }
  })

  # Update the select inputs based on the current history, excluding archived rules, and add symbol
  observe({
    filtered_r_history <- setdiff(current_r_history(), archived_rules())
    filtered_bash_history <- setdiff(current_bash_history(), archived_rules())
    imp_r <- r_history_importance()
    imp_b <- bash_history_importance()
    symbolize <- function(line, imp_map) {
      sym <- if (!is.null(imp_map[[line]]) && imp_map[[line]] == "important") "★" else "⭘"
      paste0(sym, " ", line)
    }
    r_choices <- setNames(filtered_r_history, vapply(filtered_r_history, symbolize, character(1), imp_map = imp_r))
    b_choices <- setNames(filtered_bash_history, vapply(filtered_bash_history, symbolize, character(1), imp_map = imp_b))

    # For archived rules, try to find the symbol from either R or Bash importance maps
    archived <- archived_rules()
    archived_choices <- setNames(
      archived,
      vapply(archived, function(line) {
        sym <- if (!is.null(imp_r[[line]])) {
          if (imp_r[[line]] == "important") "★" else "⭘"
        } else if (!is.null(imp_b[[line]])) {
          if (imp_b[[line]] == "important") "★" else "⭘"
        } else {
          "⭘"
        }
        paste0(sym, " ", line)
      }, character(1))
    )

    updateSelectInput(session, "selected_line", choices = r_choices)
    updateSelectInput(session, "selected_term", choices = b_choices)
    updateSelectInput(session, "archived_rules_select", choices = archived_choices)
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
      "This will delete the currently stored API key for the selected model. You will be prompted for a new key the next time you generate a rule.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_change_key_options", "Confirm", class = "btn btn-danger")
      ),
      class = "modal-content"
    ))
  })

  observeEvent(input$confirm_change_key_options, {
    model_key <- isolate(input$selected_model_options)
    service <- paste0("Snakemaker_", model_key)
    # Remove any logic that writes files or calls stopApp()
    try({
      keyring::key_delete(service = service, username = Sys.info()[["user"]])
    }, silent = TRUE)
    showNotification("API key deleted from keyring. You will be prompted for a new key when needed.", duration = 2)
    removeModal()
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
    source_flag <- if (input$menu_choice == "history") "R history" else "Terminal history"
    if (length(selected_line) > 0L && selected_line != "") {
      writeLines(selected_line, con = "selected_line.txt")
      writeLines(source_flag, con = "source_flag.txt")
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

  observeEvent(input$toggle_importance_btn, {
    if (input$menu_choice == "history") {
      selected <- input$selected_line
      imp_map <- r_history_importance()
      for (line in selected) {
        current <- imp_map[[line]]
        imp_map[[line]] <- if (!is.null(current) && current == "important") "not important" else "important"
      }
      r_history_importance(imp_map)
    } else if (input$menu_choice == "term_history") {
      selected <- input$selected_term
      imp_map <- bash_history_importance()
      for (line in selected) {
        current <- imp_map[[line]]
        imp_map[[line]] <- if (!is.null(current) && current == "important") "not important" else "important"
      }
      bash_history_importance(imp_map)
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
