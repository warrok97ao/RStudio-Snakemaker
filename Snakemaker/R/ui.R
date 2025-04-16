create_ui = function(history, term_history, archived_rules, selected_model) {
  fluidPage(
    tags$head(
      tags$style(HTML("
        body {
          font-family: 'Inter', 'Segoe UI', Arial, sans-serif;
          font-size: 13px;
          background: #f7f8fa;
          color: #23272f;
        }
        #header {
          position: fixed;
          top: 0; left: 0; right: 0;
          z-index: 1000;
          display: flex;
          justify-content: space-between;
          align-items: center;
          padding: 10px 18px;
          background: #fff;
          border-bottom: 1px solid #e5e7eb;
          box-shadow: 0 2px 8px rgba(0,0,0,0.03);
        }
        #title {
          font-weight: 600;
          font-size: 15px;
          letter-spacing: 0.5px;
        }
        .btn {
          border-radius: 6px !important;
          font-size: 12px !important;
          padding: 6px 14px !important;
          border: none !important;
          box-shadow: none !important;
          transition: background 0.15s, color 0.15s;
        }
        .btn-info {
          background: #e0e7ef !important;
          color: #3b4252 !important;
        }
        .btn-secondary {
          background: #f3f4f6 !important;
          color: #23272f !important;
        }
        .btn-primary {
          background: #2563eb !important;
          color: #fff !important;
        }
        .btn-warning {
          background: #fbbf24 !important;
          color: #23272f !important;
        }
        .btn-danger {
          background: #ef4444 !important;
          color: #fff !important;
        }
        .btn-success {
          background: #22c55e !important;
          color: #fff !important;
        }
        .btn:hover, .btn:focus {
          filter: brightness(0.95);
        }
        #main-content {
          margin-top: 64px;
          padding: 24px 18px 0 18px;
        }
        .radio-buttons label {
          font-weight: 500;
          margin-bottom: 6px;
        }
        .select-input select, .archived-rules select {
          width: 100%;
          border-radius: 6px;
          border: 1px solid #e5e7eb;
          background: #fff;
          padding: 6px 10px;
          font-size: 13px;
          margin-bottom: 10px;
          box-shadow: 0 1px 2px rgba(0,0,0,0.01);
          transition: border 0.15s;
        }
        .select-input select:focus, .archived-rules select:focus {
          border-color: #2563eb;
          outline: none;
        }
        /* Make select options wrap text and break long words */
        .select-input option,
        .archived-rules option {
          white-space: normal !important;
          word-break: break-all;
        }
        .action-buttons {
          display: flex;
          gap: 10px;
          margin-bottom: 18px;
        }
        hr {
          border: none;
          border-top: 1px solid #e5e7eb;
          margin: 24px 0 18px 0;
        }
        h4 {
          font-size: 14px;
          font-weight: 600;
          margin-bottom: 10px;
        }
        /* Dark mode */
        body.dark-mode {
          background: #181a20;
          color: #e5e7eb;
        }
        body.dark-mode #header {
          background: #23272f;
          border-bottom: 1px solid #23272f;
          box-shadow: 0 2px 8px rgba(0,0,0,0.10);
        }
        body.dark-mode #main-content {
          background: #181a20;
        }
        body.dark-mode .btn-info {
          background: #23272f !important;
          color: #e5e7eb !important;
        }
        body.dark-mode .btn-secondary {
          background: #23272f !important;
          color: #e5e7eb !important;
        }
        body.dark-mode .btn-primary {
          background: #2563eb !important;
          color: #fff !important;
        }
        body.dark-mode .btn-warning {
          background: #fbbf24 !important;
          color: #23272f !important;
        }
        body.dark-mode .btn-danger {
          background: #ef4444 !important;
          color: #fff !important;
        }
        body.dark-mode .btn-success {
          background: #22c55e !important;
          color: #fff !important;
        }
        body.dark-mode .select-input select,
        body.dark-mode .archived-rules select {
          background: #23272f !important;
          color: #e5e7eb !important;
          border: 1px solid #393e4a !important;
        }
        body.dark-mode hr {
          border-top: 1px solid #23272f;
        }
        /* Make select options wrap text and break long words in dark mode */
        body.dark-mode .select-input option,
        body.dark-mode .archived-rules option {
          white-space: normal !important;
          word-break: break-all;
        }
      ")),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Inter:400,500,600&display=swap"),
      tags$script(HTML("
        $(document).on('click', '#toggle_theme', function() {
          $('body').toggleClass('dark-mode');
          var btn = $(this);
          if ($('body').hasClass('dark-mode')) {
            btn.html('<span>&#9788;</span> Light Mode');
          } else {
            btn.html('<span>&#9790;</span> Dark Mode');
          }
        });
      "))
    ),
    div(
      id = "header",
      actionButton("open_settings", label = "", icon = icon("cog"), class = "btn btn-info"),
      div(id = "title", "Snakemaker"),
      actionButton("toggle_theme", span(icon("moon"), " Dark Mode"), class = "btn btn-secondary")
    ),
    div(
      id = "main-content",
      fluidPage(
        div(class = "radio-buttons",
            radioButtons("menu_choice", "Choose Menu:",
                         choices = c("R History" = "history", "Terminal history" = "term_history"))
        ),
        div(class = "select-input",
            conditionalPanel(
              condition = "input.menu_choice == 'history'",
              selectInput("selected_line", "Select a line:",
                          choices = history,
                          selectize = FALSE,
                          multiple = TRUE,
                          size = 5,
                          width = "100%")
            ),
            conditionalPanel(
              condition = "input.menu_choice == 'term_history'",
              selectInput("selected_term", "Select a line:",
                          choices = term_history,
                          selectize = FALSE,
                          size = 5,
                          width = "100%")
            )
        ),
        div(class = "action-buttons",
            actionButton("insert", span(icon("file-import"), " Generate rule"), class = "btn btn-primary"),
            actionButton("refresh", span(icon("sync"), " Update History"), class = "btn btn-warning"),
            actionButton("close", span(icon("times"), " Close"), class = "btn btn-danger")
        ),
        hr(),
        h4("Archived Rules"),
        div(class = "archived-rules",
            selectInput("archived_rules_select", "Select an archived rule:",
                        choices = archived_rules,
                        selectize = FALSE,
                        size = 5,
                        width = "100%"),
            actionButton("parse_again", span(icon("redo"), " Parse Again"), class = "btn btn-success")
        )
      )
    )
  )
}
