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
        ),
        fluidRow(
          column(12,
                 div(style = "margin-top:18px;",
                     actionButton("toggle_theme_settings", 
                                  HTML("<span>&#9790;</span> Dark Mode"), 
                                  class = "btn btn-secondary", 
                                  style = "width:100%;"))
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

  # Add observeEvent for the new dark mode button in settings
  observeEvent(input$toggle_theme_settings, {
    session$sendCustomMessage(type = "jsCode", list(
      code = "$('body').toggleClass('dark-mode'); var btn = $('#toggle_theme_settings'); if ($('body').hasClass('dark-mode')) { btn.html('<span>&#9788;</span> Light Mode'); } else { btn.html('<span>&#9790;</span> Dark Mode'); }"
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

  show_chat <- reactiveVal(FALSE)

  observeEvent(input$open_chat, { show_chat(TRUE) })
  observeEvent(input$close_chat, { show_chat(FALSE) })

  # Chat state and logic (full-featured, as in chat_llm_addin.R)
  chatData <- reactiveValues(
    current = list(),
    sessions = list(),
    document_context = ""
  )

  observeEvent(input$new_chat, {
    if (length(chatData$current) > 0) {
      chatData$sessions[[length(chatData$sessions) + 1]] <- chatData$current
    }
    chatData$current <- list()
  })

  observeEvent(input$send, {
    req(input$user_message)
    old_messages <- chatData$current
    history_text <- if (length(old_messages) > 0) {
      paste(sapply(old_messages, function(msg) {
        if (msg$sender == "user") paste("User:", msg$text) else paste("LLM:", msg$text)
      }), collapse = "\n")
    } else {
      ""
    }
    selected_model <- if (file.exists("selected_model.txt")) {
      readLines("selected_model.txt", warn = FALSE)
    } else {
      "llama3-8b-8192"
    }
    docContext <- ""
    if (nchar(chatData$document_context) > 0) {
      docContext <- paste("Document context:", chatData$document_context)
    }
    prompt <- paste(
      "You are an expert in bioinfomratics analysis. We are in the context of RStudio, so the question could be related to R. This is our previous conversation, use it as context for the next response. If there is no previous conversation do not mention it. Prefer shorter, more concise responses, and do not preface your answer with any context.",
      docContext,
      history_text,
      "User:",
      input$user_message,
      sep = "\n"
    )
    userMsg <- list(sender = "user", text = input$user_message)
    chatData$current <- append(old_messages, list(userMsg))
    updateTextInput(session, "user_message", value = "")
    response <- tryCatch({
      api_key <- tryCatch(keyring::key_get(service = paste0("Snakemaker_", selected_model), username = Sys.info()[["user"]]), error = function(e) "")
      url <- if (selected_model == "nvidia/llama-3.1-nemotron-70b-instruct") {
        "https://integrate.api.nvidia.com/v1/chat/completions"
      } else {
        "https://api.groq.com/openai/v1/chat/completions"
      }
      body <- list(
        model = selected_model,
        messages = list(list(role = "user", content = prompt))
      )
      resp <- httr::POST(
        url,
        httr::add_headers(`Content-Type` = "application/json", Authorization = paste("Bearer", api_key)),
        encode = "json",
        body = body
      )
      parsed <- jsonlite::fromJSON(httr::content(resp, as = "text"), simplifyVector = FALSE)
      if (!is.null(parsed$error)) stop(parsed$error$message)
      parsed$choices[[1]]$message$content
    }, error = function(e) paste("Error:", e$message))
    llmMsg <- list(sender = "llm", text = response)
    chatData$current <- append(chatData$current, list(llmMsg))
  })

  output$chat_output <- renderUI({
    tags$div(
      style = "display:flex; flex-direction:column; gap:10px;",
      lapply(chatData$current, function(msg) {
        align <- if (msg$sender == "user") "flex-end" else "flex-start"
        msg_class <- if (msg$sender == "user") "chat-message user" else "chat-message llm"
        processedText <- if (msg$sender == "llm") {
          HTML(commonmark::markdown_html(enc2utf8(as.character(msg$text))))
        } else {
          msg$text
        }
        tags$div(
          class = msg_class,
          style = paste("align-self:", align, ";"),
          processedText
        )
      })
    )
  })

  observeEvent(input$show_doc, {
    activeDocContext <- tryCatch(
      expr = rstudioapi::getActiveDocumentContext(),
      error = function(e) {
        showNotification("Failed to retrieve active document context.", type = "error", duration = 5)
        return(NULL)
      }
    )
    if (is.null(activeDocContext)) return()
    activeDoc <- tryCatch(
      expr = activeDocContext$contents,
      error = function(e) {
        showNotification("Failed to access document contents.", type = "error", duration = 5)
        return(NULL)
      }
    )
    if (is.null(activeDoc)) return()
    activeDoc <- paste(activeDoc, collapse = "\n")
    maxChars <- 20000
    if (nchar(activeDoc) > maxChars) {
      activeDoc <- paste0(substr(activeDoc, 1, maxChars), "\n...\n(Content truncated)")
    }
    chatData$document_context <- activeDoc
    session$sendCustomMessage("updateButtonStyle",
                              list(buttonId = "show_doc", style = "background-color: #ffcccc;"))
    showNotification("Active document set as context", duration = 2)
  })

  observeEvent(input$show_history, {
    if (length(chatData$sessions) == 0) {
      showNotification("No chat history available.", type = "error", duration = 3)
      return()
    }
    sessionChoices <- setNames(seq_along(chatData$sessions),
                               paste("Chat Session", seq_along(chatData$sessions)))
    showModal(modalDialog(
      title = "Select a Chat Session",
      selectInput("selected_history_session", "Chat History:", choices = sessionChoices),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("load_history_session", "Load Chat", class = "btn btn-primary")
      )
    ))
  })

  observeEvent(input$load_history_session, {
    req(input$selected_history_session)
    selectedIndex <- as.integer(input$selected_history_session)
    chatData$current <- chatData$sessions[[selectedIndex]]
    removeModal()
  })

  observeEvent(input$generate_rmd, {
    activeDocContext <- tryCatch(
      expr = rstudioapi::getActiveDocumentContext(),
      error = function(e) {
        showNotification("Failed to retrieve active document context.", type = "error", duration = 5)
        return(NULL)
      }
    )
    if (is.null(activeDocContext))
      return()
    if (!nzchar(activeDocContext$path)) {
      showNotification("Please save the active document before generating RMarkdown.", type = "error", duration = 5)
      return()
    }
    activeDoc <- tryCatch(
      expr = {
        content <- activeDocContext$contents
        paste(content, collapse = "\n")
      },
      error = function(e) {
        showNotification("Failed to access document contents.", type = "error", duration = 5)
        return(NULL)
      }
    )
    if (is.null(activeDoc))
      return()
    maxChars <- 20000
    if (nchar(activeDoc) > maxChars) {
      activeDoc <- paste0(substr(activeDoc, 1, maxChars), "\n...\n(Content truncated)")
    }
    prompt_rmd <- paste(
      "You are an expert in RStudio and RMarkdown. This is an R script; I need to generate a well-defined RMarkdown document where the code is well formatted. Please add a description for each significant part and choose wisely how to split the code, keeping the context in mind.",
      activeDoc,
      sep = "\n"
    )
    selected_model <- if (file.exists("selected_model.txt")) {
      readLines("selected_model.txt", warn = FALSE)
    } else {
      "llama3-8b-8192"
    }
    rmd_response <- tryCatch(
      {
        api_key <- tryCatch(keyring::key_get(service = paste0("Snakemaker_", selected_model), username = Sys.info()[["user"]]), error = function(e) "")
        url <- if (selected_model == "nvidia/llama-3.1-nemotron-70b-instruct") {
          "https://integrate.api.nvidia.com/v1/chat/completions"
        } else {
          "https://api.groq.com/openai/v1/chat/completions"
        }
        body <- list(
          model = selected_model,
          messages = list(list(role = "user", content = prompt_rmd))
        )
        resp <- httr::POST(
          url,
          httr::add_headers(`Content-Type` = "application/json", Authorization = paste("Bearer", api_key)),
          encode = "json",
          body = body
        )
        parsed <- jsonlite::fromJSON(httr::content(resp, as = "text"), simplifyVector = FALSE)
        if (!is.null(parsed$error)) stop(parsed$error$message)
        parsed$choices[[1]]$message$content
      },
      error = function(e) {
        showNotification(paste("Error generating RMarkdown:", e$message), type = "error", duration = 5)
        return(NULL)
      }
    )
    # Optionally: showModal(modalDialog(title="Generated RMarkdown", rmd_response, easyClose=TRUE))
  })

  # Header Chat/Back button logic
  output$header_chat_or_back_btn <- renderUI({
    if (isTRUE(show_chat())) {
      div(
        style = "margin-left: auto; display: flex; align-items: center; gap: 10px;",
        actionButton("close_chat", label = tagList(icon("arrow-left"), "Back"), class = "btn btn-secondary")
      )
    } else {
      div(
        style = "margin-left: auto; display: flex; align-items: center; gap: 10px;",
        actionButton("open_chat", label = tagList(icon("comments"), "Chat"), class = "btn btn-secondary")
      )
    }
  })

  output$main_or_chat_ui <- renderUI({
    if (isTRUE(show_chat())) {
      create_chat_ui()
    } else {
      # ...existing main UI code...
      fluidPage(
        div(class = "radio-buttons",
            radioButtons("menu_choice", "Choose Menu:",
                         choices = c("R History" = "history", "Terminal history" = "term_history"))
        ),
        div(class = "select-input",
            conditionalPanel(
              condition = "input.menu_choice == 'history'",
              tagList(
                selectInput("selected_line", "Select a line:",
                            choices = character(0),
                            selectize = FALSE,
                            multiple = TRUE,
                            size = 5,
                            width = "100%")
              )
            ),
            conditionalPanel(
              condition = "input.menu_choice == 'term_history'",
              tagList(
                selectInput("selected_term", "Select a line:",
                            choices = character(0),
                            selectize = FALSE,
                            size = 5,
                            width = "100%")
              )
            )
        ),
        # Importance button now inline with other action buttons
        div(class = "action-buttons",
            actionButton("insert", span(icon("file-import"), " Generate rule"), class = "btn btn-primary"),
            actionButton("refresh", span(icon("sync"), " Update History"), class = "btn btn-warning"),
            actionButton("toggle_importance_btn", NULL, icon = icon("star"), class = "btn btn-secondary btn-sm", style = "height:32px; width:32px;", title = "Toggle Importance")
        ),
        hr(),
        h4("Archived Rules"),
        div(class = "archived-rules",
            selectInput("archived_rules_select", "Select an archived rule:",
                        choices = character(0),
                        selectize = FALSE,
                        size = 5,
                        width = "100%"),
            actionButton("parse_again", span(icon("redo"), " Parse Again"), class = "btn btn-success")
        )
      )
    }
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
