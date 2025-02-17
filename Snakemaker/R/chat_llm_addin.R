chat_llm_addin <- function() {
  library(shiny)
  library(httr)
  library(jsonlite)
  library(rstudioapi)
  
  get_api_key <- function(model) {
    api_key <- Sys.getenv(model)
    if (api_key == "") {
      api_key <- rstudioapi::askForPassword(paste("Enter your API key for", model))
      if (!is.null(api_key) && nchar(api_key) > 0) {
        env_var <- list()
        env_var[[model]] <- api_key
        do.call(Sys.setenv, env_var)
      } else {
        stop(paste("API key for", model, "is required."))
      }
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
      messages = list(
        list(role = "user", content = input_text),
        list(
          role = "system",
          content = "Your role is to act as a bioinformatitian. Answer the following question concisely in a bioinformatics context."
        )
      )
    )
    if (model == "nvidia/llama-3.1-nemotron-70b-instruct") {
      body$temperature <- 0.6
      body$top_p <- 0.7
      body$max_tokens <- 4096
      body$stream <- FALSE
    }
    response <- httr::POST(
      url,
      httr::add_headers(`Content-Type` = "application/json", Authorization = paste("Bearer", api_key)),
      encode = "json",
      body = body
    )
    if (httr::http_type(response) != "application/json") {
      stop("Unexpected response format: ", httr::content(response, as = "text", encoding = "UTF-8"))
    }
    parsed <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
    if (!is.null(parsed$error)) stop(parsed$error$message)
    if (!is.null(parsed$choices) && length(parsed$choices) > 0) {
      choice <- parsed$choices[[1]]
      if (!is.null(choice$message$content)) {
        return(choice$message$content)
      } else {
        stop("Unexpected response format: 'content' missing in 'message'.")
      }
    } else {
      stop("Unexpected response format: 'choices' missing or empty.")
    }
  }
  
  ui <- fluidPage(
    titlePanel("LLM Chat Addin"),
    sidebarLayout(
      sidebarPanel(
        textInput("user_message", "Message:", ""),
        actionButton("send", "Send")
      ),
      mainPanel(
        verbatimTextOutput("chat_output")
      )
    )
  )
  
  server <- function(input, output, session) {
    chat <- reactiveVal("")
    observeEvent(input$send, {
      req(input$user_message)
      tryCatch({
        model <- "llama3-8b-8192"  # Adjust model if needed
        response <- generate_chat(input$user_message, model)
        new_chat <- paste(chat(), "User:", input$user_message, "\nLLM:", response, sep = "\n")
        chat(new_chat)
        output$chat_output <- renderText({ chat() })
      }, error = function(e) {
        chat(paste(chat(), "\nError:", e$message, sep = " "))
        output$chat_output <- renderText({ chat() })
      })
    })
  }
  
  app <- shinyApp(ui, server)
  port <- httpuv::randomPort()
  bg <- callr::r_bg(function(app, port) {
    shiny::runApp(app, port = port)
  }, args = list(app, port))
  attempts <- 0
  while (!is_port_ready(port, retries = 10) && attempts < 10) {
    Sys.sleep(0.2)
    attempts <- attempts + 1
  }
  url <- paste0("http://127.0.0.1:", port)
  viewer <- getOption("viewer")
  if (!is.null(viewer) && is.function(viewer)) {
    viewer(url)
  } else {
    browseURL(url)
  }
}

if (interactive()) {
  chat_llm_addin()
}

is_port_ready <- function(port = 8080, retries = 5) {
  for (i in 1:retries) {
    tryCatch({
      con <- suppressWarnings(socketConnection("localhost", port = port,
                                               server = FALSE,
                                               blocking = FALSE,
                                               timeout = 1))
      close(con)
      return(TRUE)
    }, error = function(e) {
      Sys.sleep(0.2)
      FALSE
    })
  }
  FALSE
}
