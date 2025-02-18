my_addin <- function() {
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(callr))
suppressPackageStartupMessages(library(later))
suppressPackageStartupMessages(library(rstudioapi))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(bslib))
  
  get_api_key <- function(model) {
    api_key <- Sys.getenv(model)
    if (api_key == "") {
      api_key <- rstudioapi::askForPassword(paste("Enter your API key for", model))
      if (!is.null(api_key) && api_key != "") {
        env_var <- list()
        env_var[[model]] <- api_key
        do.call(Sys.setenv, env_var)
      } else stop(paste("API key for", model, "is required."))
    }
    api_key
  }

  generate_rule <- function(input_text, model) {
    api_key <- get_api_key(model)
    url <- if (model == "nvidia/llama-3.1-nemotron-70b-instruct") {
      "https://integrate.api.nvidia.com/v1/chat/completions"
    } else {
      "https://api.groq.com/openai/v1/chat/completions"
    }

    source_flag <- if (file.exists("source_flag.txt")) {
      readLines("source_flag.txt", warn = FALSE)
    } else {
      "Either R or Bash"
    }

    body <- list(
      model = model,
      messages = list(
        list(role = "user", content = input_text),
        list(
          role = "system",
          content = paste0("
I have the following command line from ", source_flag, ".
Convert this command into a valid Snakemake rule. Your output must include only the Snakemake rule definition and no additional text, comments, or explanations. Follow these guidelines:

1. Deduce the input and output filenames from the command. If inputs or outputs are unavailable, use - for none or Unknown if uncertain.
2. Generate rule name based on the command.
3. If possible, use wildcards in inputs/outputs where appropriate.
4. Include a log directive in the rule (e.g., log: logs/{rule}.log) if the rule processes files or if wildcards are present.
5. Preserve any newlines present in the command, adjusting only if necessary for rule readability.
6. Ensure that your entire output consists solely of the Snakemake rule.

Example of acceptable output:
rule [rulename]:
    input: ...
    output: ...
    log: logs/{rulename}.log
    shell: ...

Provide ONLY the Snakemake rule based on the given command, no additional text, no comments, etc.
")
        )
      )
    )

    if (model == "nvidia/llama-3.1-nemotron-70b-instruct") {
      body$temperature <- 0.6
      body$top_p <- 0.7
      body$max_tokens <- 4096
      body$stream <- FALSE
    }

    response <- POST(
      url,
      add_headers(`Content-Type` = "application/json", Authorization = paste("Bearer", api_key)),
      encode = "json",
      body = body
    )

    if (http_type(response) != "application/json") {
      stop("Unexpected response format: ", content(response, as = "text", encoding = "UTF-8"))
    }

    parsed <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
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

  get_r_history <- function() {
    history_file <- tempfile(fileext = ".Rhistory")
    tryCatch({
      utils::savehistory(history_file)
      if (file.exists(history_file)) {
        hist_lines <- unique(rev(readLines(history_file)))
        filtered <- hist_lines[!grepl("Snakemaker:::my_addin\\(\\)", hist_lines)]
        writeLines(filtered, "r_history.txt")
        filtered
      } else {
        "No R history available!"
      }
    }, error = function(e) paste("Error accessing R history:", e$message))
  }

  get_term_history <- function() {
    history_file <- path.expand("~/.bash_history")
    if (file.exists(history_file)) {
      history_lines <- readLines(history_file)
      unique_lines <- unique(rev(history_lines))
      writeLines(unique_lines, "bash_history.txt")
      unique_lines
    } else {
      "No terminal history available!"
    }
  }

  rstudioapi::registerCommandCallback("newTerminal", function(...) {

    later::later(function() {
      termId <- rstudioapi::terminalVisible()
      rstudioapi::terminalSend(termId, "PROMPT_COMMAND='history -a'\n")
    }, delay = 3)
  })

  history <- get_r_history()
  term_history <- get_term_history()

  ui <- create_ui(
    history,
    term_history,
    character(),
    selected_model = if (file.exists("selected_model.txt")) {
      readLines("selected_model.txt", warn = FALSE)
    } else {
      "llama3-8b-8192"
    }
  )

  app <- shinyApp(ui, server)

  shiny_bg_process <- callr::r_bg(function(app) {
    shiny::runApp(app, port = 8080)
  }, args = list(app))

  Sys.sleep(0.5)
  attempts <- 0
  while (!is_port_ready() && attempts < 10) {
    Sys.sleep(0.2)
    attempts <- attempts + 1
  }

  Sys.sleep(1)
  viewer <- getOption("viewer")
  if (!is.null(viewer)) {
    viewer("http://localhost:8080")
  } else {
    showNotification("Shiny app is running at http://localhost:8080", duration = 5, type = "message")
  }

  recursive_check <- function(interval = 0.2) {
    current_model <- if (file.exists("selected_model.txt")) {
      sel <- readLines("selected_model.txt", warn = FALSE)
      if (length(sel) == 0 || sel == "") "llama3-8b-8192" else sel
    } else {
      "llama3-8b-8192"
    }

    flag_file <- paste0("delete_api_key_", current_model, ".txt")
    if (file.exists(flag_file)) {
      Sys.unsetenv(current_model)
      file.remove(flag_file)
    }

    if (file.exists("restart_flag.txt")) {
      file.remove("restart_flag.txt")
      later::later(function() {
        my_addin()
      }, delay = 1)
      return()
    }

    if (file.exists("selected_line.txt")) {
      selected_line_lines <- readLines("selected_line.txt")
      writeLines("", "selected_line.txt")
      selected_line <- paste(selected_line_lines, collapse = "\n")
    } else {
      selected_line <- ""
    }
    if (nchar(selected_line) > 0) {
      context <- rstudioapi::getActiveDocumentContext()
      if (!is.null(context)) {
        tryCatch({
          snakemake_rule <- generate_rule(selected_line, model = current_model)
          rstudioapi::insertText(location = context$selection[[1]]$range, text = snakemake_rule)
        }, error = function(e) {
          message <- paste("Error generating rule:", e$message)
          rstudioapi::showDialog("Error", message)
        })
      }
    }

    # Update history files
    get_r_history()
    get_term_history()

    later::later(recursive_check, interval)
  }

  recursive_check()
}

