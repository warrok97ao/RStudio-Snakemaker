library(shiny)
library(httr)
library(jsonlite)
library(rstudioapi)

get_api_key <- function(model) {
  api_key <- Sys.getenv(model)
  if (api_key == "") {
    api_key <- rstudioapi::askForPassword(paste("Enter your API key for", model))
    if (is.null(api_key) || nchar(api_key) == 0) stop("API key is required.")
    Sys.setenv(model = api_key)
  }
  api_key
}

generate_chat <- function(input_text, model) {
  api_key <- get_api_key(model)
  url <- if (model == "nvidia/llama-3.1-nemotron-70b-instruct") {
    "https://integrate.api.nvidia.com/v1/chat/completions"
  } else {
    "https://api.groq.com/openai/v1/chat/completions"
  }
  body <- list(
    model = model,
    messages = list(list(role = "user", content = input_text))
  )
  response <- httr::POST(
    url,
    httr::add_headers(`Content-Type` = "application/json", Authorization = paste("Bearer", api_key)),
    encode = "json",
    body = body
  )
  parsed <- jsonlite::fromJSON(httr::content(response, as = "text"), simplifyVector = FALSE)
  if (!is.null(parsed$error)) stop(parsed$error$message)
  parsed$choices[[1]]$message$content
}

#' Launch Chat LLM Addin
#'
#' @export
chat_llm_addin <- function() {
  ui <- fluidPage(
    tags$head(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('updateButtonStyle', function(message) {
          var btn = document.getElementById(message.buttonId);
          if(btn){
            btn.style.cssText += message.style;
          }
        });
        $(document).on('input', 'textarea', function () {
          var maxHeight = 150;
          this.style.height = 'auto';
          this.style.height = Math.min(this.scrollHeight, maxHeight) + 'px';
        });
      "))
    ),
    tags$div(
      style = 'padding-bottom: 200px;',  # ensure chat output not overlapped
      uiOutput('chat_output')
    ),
    tags$div(
      style = 'position: fixed; bottom: 0; left: 0; right: 0; background: #f0f0f0; padding: 10px; border-top: 1px solid #ccc;',
      # First row: message input and send button
      fluidRow(
        column(8,
               textAreaInput("user_message", "Message:", "", width = "100%", rows = 1,
                             resize = "vertical",
                             placeholder = "Type your message here...")
        ),
        column(4,
               actionButton("send", "Send", style = "margin-top: 1px; width:100%;")
        )
      ),
      # Second row: history, new chat, active document, and settings buttons (each ~10% width)
      div(
        style = "margin-top: 5px; display: flex; justify-content: center; gap: 10px;",
        div(
          style = "width:10%;",
          actionButton("new_chat", label = "", icon = icon("plus-circle"),
                       class = "btn btn-success btn-sm", style = "width:100%;")
        ),
        div(
          style = "width:10%;",
          actionButton("show_history", label = "", icon = icon("history"),
                       class = "btn btn-warning btn-sm", style = "width:100%;")
        ),
        div(
          style = "width:10%;",
          actionButton("show_doc", label = "", icon = icon("file-code"),
                       class = "btn btn-secondary btn-sm", style = "width:100%;")
        ),
        div(
          style = "width:10%;",
          actionButton("open_settings", label = "", icon = icon("cog"),
                       class = "btn btn-info btn-sm", style = "width:100%;")
        ),
        div(
          style = "width:10%;",
          actionButton("generate_rmd", label = "", icon = icon("file-alt"),
                       class = "btn btn-primary btn-sm", style = "width:100%;")
        )
      )
    )
  )

  server <- function(input, output, session) {
    # Create reactiveValues to hold chat sessions and document context
    chatData <- reactiveValues(
      current = list(),
      sessions = list(),  # holds previous sessions
      document_context = ""
    )

    # Model settings modal
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
        )
      ))
    })

    observeEvent(input$change_key_options, {
      showModal(modalDialog(
        title = "Change API Key",
        "This will delete the currently stored API key for the selected model. The addin will restart immediately and next time you generate a rule, you will be prompted for a new key.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_change_key_options", "Confirm", class = "btn btn-danger")
        )
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
    })

    observeEvent(input$save_model_options, {
      selected_model <- isolate(input$selected_model_options)
      writeLines(selected_model, "selected_model.txt")
      showNotification("Model selection saved.", duration = 2)
      removeModal()
    })

    observeEvent(input$new_chat, {
      # Save the current chat session if it contains messages
      if (length(chatData$current) > 0) {
        chatData$sessions[[length(chatData$sessions) + 1]] <- chatData$current
      }
      # Reset the current chat session to start fresh
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
      # Read selected model or use default
      selected_model <- if (file.exists("selected_model.txt")) {
        readLines("selected_model.txt", warn = FALSE)
      } else {
        "llama3-8b-8192"
      }

      # Include document context if available
      docContext <- ""
      if (nchar(chatData$document_context) > 0) {
        docContext <- paste("Document context:", chatData$document_context)
      }

      prompt <- paste("You are an expert in bioinfomratics analysis. We are in the context of RStudio, so the question could be related to R. This is our previous conversation, use it as context for the next response. If there is no previous conversation do not mention it. Prefer shorter, more concise responses, and do not preface your answer with any context.",
                      docContext,
                      history_text,
                      "User:",
                      input$user_message,
                      sep = "\n")

      userMsg <- list(sender = "user", text = input$user_message)
      chatData$current <- append(old_messages, list(userMsg))

      # Clear the text input after sending
      updateTextInput(session, "user_message", value = "")

      response <- tryCatch(
        generate_chat(prompt, selected_model),
        error = function(e) paste("Error:", e$message)
      )
      llmMsg <- list(sender = "llm", text = response)
      chatData$current <- append(chatData$current, list(llmMsg))
    })

    output$chat_output <- renderUI({
      tags$div(
        style = "display:flex; flex-direction:column; gap:10px;",
        lapply(chatData$current, function(msg) {
          align <- if (msg$sender == "user") "flex-end" else "flex-start"
          bg <- if (msg$sender == "user") "#cce5ff" else "#e2e3e5"
          processedText <- if (msg$sender == "llm") {
            HTML(commonmark::markdown_html(enc2utf8(as.character(msg$text))))
          } else {
            msg$text
          }
          tags$div(
            style = paste("display:inline-block; max-width:100%; padding:8px; background-color:", bg,
                          "; border-radius:10px; align-self:", align, "; word-wrap: break-word;"),
            processedText
          )
        })
      )
    })

    # Observer for showing active document content
    observeEvent(input$show_doc, {
      # Attempt to get the active document context with error handling
      activeDocContext <- tryCatch(
        expr = rstudioapi::getActiveDocumentContext(),
        error = function(e) {
          showNotification("Failed to retrieve active document context.", type = "error", duration = 5)
          return(NULL)
        }
      )

      if (is.null(activeDocContext)) return() # Exit if retrieval failed

      # Get the content safely
      activeDoc <- tryCatch(
        expr = activeDocContext$contents,
        error = function(e) {
          showNotification("Failed to access document contents.", type = "error", duration = 5)
          return(NULL)
        }
      )

      if (is.null(activeDoc)) return()  # Exit if content access failed

      # Combine lines into a single string
      activeDoc <- paste(activeDoc, collapse = "\n")

      # Cap the content length to prevent overly large data in context
      maxChars <- 20000
      if (nchar(activeDoc) > maxChars) {
        activeDoc <- paste0(substr(activeDoc, 1, maxChars), "\n...\n(Content truncated)")
      }

      # Store the active document content into chatData for use as context.
      chatData$document_context <- activeDoc

      # Send a custom message to update the style (color) of the active document button.
      session$sendCustomMessage("updateButtonStyle",
                                list(buttonId = "show_doc", style = "background-color: #ffcccc;"))

      # Optional: Show a notification that the active document is now used as context.
      showNotification("Active document set as context", duration = 2)
    })

    # Observer for opening the Chat History modal.
    observeEvent(input$show_history, {
      if (length(chatData$sessions) == 0) {
        showNotification("No chat history available.", type = "error", duration = 3)
        return()
      }
      # Prepare choices for previous sessions.
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

    # Observer for loading the selected chat session.
    observeEvent(input$load_history_session, {
      req(input$selected_history_session)
      selectedIndex <- as.integer(input$selected_history_session)
      chatData$current <- chatData$sessions[[selectedIndex]]
      removeModal()
    })

    observeEvent(input$generate_rmd, {
      # Attempt to get the active document context with error handling
      activeDocContext <- tryCatch(
        expr = rstudioapi::getActiveDocumentContext(),
        error = function(e) {
          showNotification("Failed to retrieve active document context.", type = "error", duration = 5)
          return(NULL)
        }
      )

      if (is.null(activeDocContext))
        return()  # Exit if retrieval failed

      # Ensure the document has been saved; a file path must be available
      if (!nzchar(activeDocContext$path)) {
        showNotification("Please save the active document before generating RMarkdown.", type = "error", duration = 5)
        return()
      }

      # Get the content and combine lines into a single string
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

      # Optionally cap the content length (you can adjust the cap as needed)
      maxChars <- 20000
      if (nchar(activeDoc) > maxChars) {
        activeDoc <- paste0(substr(activeDoc, 1, maxChars), "\n...\n(Content truncated)")
      }

      # Build the prompt for RMarkdown generation
      prompt_rmd <- paste(
        "You are an expert in RStudio and RMarkdown. This is an R script; I need to generate a well-defined RMarkdown document where the code is well formatted. Please add a description for each significant part and choose wisely how to split the code, keeping the context in mind.",
        activeDoc,
        sep = "\n"
      )

      # Read selected model or use default
      selected_model <- if (file.exists("selected_model.txt")) {
        readLines("selected_model.txt", warn = FALSE)
      } else {
        "llama3-8b-8192"
      }

      # Call the LLM to generate the RMarkdown text
      rmd_response <- tryCatch(
        generate_chat(prompt_rmd, selected_model),
        error = function(e) {
          showNotification(paste("Error generating RMarkdown:", e$message), type = "error", duration = 5)
          return(NULL)
        }
      )
    })  # Close observeEvent(input$generate_rmd)
  }  # Close server

  app <- shinyApp(ui, server)
  # Run the app in background on port 8081
  bg_process <- callr::r_bg(function(app, port) {
    shiny::runApp(app, port = port)
  }, args = list(app = app, port = 8081))
  
  Sys.sleep(1)  # wait briefly for the app to launch
  viewer <- getOption("viewer")
  url <- "http://localhost:8081"
  if (!is.null(viewer)) {
    viewer(url)
  } else {
    message("Shiny app is running at ", url)
  }
}
