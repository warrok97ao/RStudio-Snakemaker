library(shiny)
library(httr)
library(jsonlite)
library(rstudioapi)
library(keyring)

get_api_key <- function(model) {
  service <- paste0("Snakemaker_", model)
  # Try to get the key from keyring
  api_key <- tryCatch(key_get(service = service, username = Sys.info()[["user"]]), error = function(e) "")
  if (identical(api_key, "")) {
    api_key <- rstudioapi::askForPassword(paste("Enter your API key for", model))
    if (is.null(api_key) || nchar(api_key) == 0) stop("API key is required.")
    key_set_with_value(service = service, username = Sys.info()[["user"]], password = api_key)
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
  # Suppress shiny's missing context error (optional, for smoother startup)
  options(shiny.suppressMissingContextError = TRUE)

  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        body, .container-fluid {
          font-family: 'Segoe UI', 'Roboto', 'Helvetica Neue', Arial, sans-serif;
          background: #f8f9fb;
        }
        .chat-panel {
          background: #fff;
          border-radius: 18px;
          box-shadow: 0 2px 12px rgba(0,0,0,0.07);
          padding: 24px 24px 12px 24px;
          margin: 24px auto 0 auto;
          max-width: 700px;
        }
        .chat-footer {
          background: #fff;
          border-radius: 18px 18px 0 0;
          box-shadow: 0 -2px 12px rgba(0,0,0,0.04);
          padding: 18px 24px 18px 24px;
          max-width: 700px;
          margin: 0 auto;
        }
        .chat-btn-row {
          margin-top: 8px;
          display: flex;
          justify-content: center;
          gap: 16px;
        }
        /* Only minimal override for .btn, so Bootstrap color classes are visible */
        .chat-btn-row .btn {
          border-radius: 10px;
          box-shadow: none;
          font-weight: 500;
          border: none;
          opacity: 0.97;
          transition: box-shadow 0.2s, opacity 0.2s;
        }
        .chat-btn-row .btn:hover {
          opacity: 1;
          box-shadow: 0 2px 8px rgba(0,0,0,0.07);
        }
        .chat-btn-row .btn:focus {
          outline: none;
          box-shadow: 0 0 0 2px #b3d4fc;
        }
        .chat-message {
          display: inline-block;
          max-width: 100%;
          padding: 12px 18px;
          margin-bottom: 6px;
          border-radius: 14px;
          word-break: break-word;
          font-size: 1.08em;
        }
        .chat-message.user {
          background: #dbeafe;
          align-self: flex-end;
        }
        .chat-message.llm {
          background: #f3f4f6;
          align-self: flex-start;
        }
        .chat-input-area textarea {
          border-radius: 10px;
          border: 1px solid #e3e7ef;
          padding: 10px;
          font-size: 1.08em;
          background: #f8f9fb;
          box-shadow: none;
        }
        .chat-input-area textarea:focus {
          border-color: #b3d4fc;
          outline: none;
        }
        .chat-send-btn {
          border-radius: 10px;
          background: #2563eb;
          color: #fff;
          border: none;
          width: 100%;
          font-weight: 500;
          transition: background 0.2s;
        }
        .chat-send-btn:hover {
          background: #1d4ed8;
        }
      ")),
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
      class = "chat-panel",
      style = 'padding-bottom: 200px;',  # ensure chat output not overlapped
      uiOutput('chat_output')
    ),
    tags$div(
      class = "chat-footer",
      style = 'position: fixed; bottom: 0; left: 0; right: 0; border-top: none; box-shadow: 0 -2px 12px rgba(0,0,0,0.04);',
      # First row: message input and send button
      fluidRow(
        class = "chat-input-area",
        column(8,
               textAreaInput("user_message", NULL, "", width = "100%", rows = 1,
                             resize = "vertical",
                             placeholder = "Type your message here...")
        ),
        column(4,
               actionButton("send", "Send", class = "chat-send-btn", style = "margin-top: 1px; width:100%;")
        )
      ),
      # Second row: history, new chat, active document, and settings buttons (each ~10% width)
      div(
        class = "chat-btn-row",
        div(
          style = "width:10%;",
          actionButton("new_chat", label = NULL, icon = icon("plus-circle"),
                       class = "btn btn-success btn-sm", style = "width:100%;",
                       title = "Start a new Chat")
        ),
        div(
          style = "width:10%;",
          actionButton("show_history", label = NULL, icon = icon("history"),
                       class = "btn btn-warning btn-sm", style = "width:100%;",
                       title = "Chat History")
        ),
        div(
          style = "width:10%;",
          actionButton("show_doc", label = NULL, icon = icon("file-code"),
                       class = "btn btn-secondary btn-sm", style = "width:100%;",
                       title = "Use active document as context")
        ),
        div(
          style = "width:10%;",
          actionButton("open_settings", label = NULL, icon = icon("cog"),
                       class = "btn btn-info btn-sm", style = "width:100%;",
                       title = "Settings")
        ),
        div(
          style = "width:10%;",
          actionButton("generate_rmd", label = NULL, icon = icon("file-alt"),
                       class = "btn btn-primary btn-sm", style = "width:100%;",
                       title = "Generate RMarkdown")
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
        textInput("new_api_key", "Enter new API key:", value = "", width = "100%"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_change_key_options", "Save", class = "btn btn-primary")
        )
      ))
    })

    observeEvent(input$confirm_change_key_options, {
      model_key <- isolate(input$selected_model_options)
      new_key <- isolate(input$new_api_key)
      if (is.null(new_key) || nchar(new_key) == 0) {
        showNotification("API key cannot be empty.", type = "error")
      } else {
        service <- paste0("Snakemaker_", model_key)
        key_set_with_value(service = service, username = Sys.info()[["user"]], password = new_key)
        showNotification("API key updated in keyring.", duration = 2)
        removeModal()
      }
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
  # Suppress all messages and warnings from shiny::runApp
  suppressMessages(suppressWarnings(
    shiny::runApp(app, port = 8081, launch.browser = getOption("viewer", NULL))
  ))
  # Optionally, if you want to open in RStudio viewer or browser:
  # url <- "http://localhost:8081"
  # viewer <- getOption("viewer")
  # if (!is.null(viewer)) {
  #   viewer(url)
  # } else {
  #   utils::browseURL(url)
  # }
}
