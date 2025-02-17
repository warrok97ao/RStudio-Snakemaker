create_ui = function(history, term_history, archived_rules, selected_model) {
  fluidPage(
    tags$head(
      tags$style(HTML("
        body { font-family: 'Arial', sans-serif; font-size: 12px; background-color: #fff; }
        /* Header styles */
        #header {
          position: fixed;
          top: 0;
          left: 0;
          right: 0;
          z-index: 1000;
          display: flex;
          justify-content: space-between;
          align-items: center;
          padding: 5px 10px;
          background-color: #fff;
          border-bottom: 1px solid #ccc;
        }
        /* Title styles */
        #title {
          font-weight: bold;
          font-size: 12px;
        }
        /* When dark mode is active */
        body.dark-mode {
          background-color: #1e1e1e;
          color: #d4d4d4;
        }
        body.dark-mode #header {
          background-color: #1e1e1e;
          border-bottom: 1px solid #444;
        }
        /* General button styles */
        .btn {
          font-size: 10px;
          padding: 4px 8px;
        }
        /* Dark mode button styles */
        body.dark-mode .btn-info {
          background-color: #007acc !important;
          border-color: #007acc !important;
        }
        body.dark-mode .btn-secondary {
          background-color: #6a5acd !important;
          border-color: #6a5acd !important;
        }
        body.dark-mode .btn-primary {
          background-color: #569cd6 !important;
          border-color: #569cd6 !important;
        }
        body.dark-mode .btn-warning {
          background-color: #dcdcaa !important;
          border-color: #dcdcaa !important;
          color: #1e1e1e !important;
        }
        body.dark-mode .btn-danger {
          background-color: #d16969 !important;
          border-color: #d16969 !important;
        }
        body.dark-mode .btn-success {
          background-color: #4caf50 !important;
          border-color: #4caf50 !important;
        }
        /* Main content margin to account for fixed header */
        #main-content {
          margin-top: 60px;
        }
        /* Style for select inputs */
        select[multiple] {
          width: 100%;
          height: auto !important;
          overflow-y: auto;
          border-radius: 4px;
          border: 1px solid #ddd;
        }
        .select-input option {
          padding: 4px;
          white-space: normal;
          word-wrap: break-word;
          text-overflow: ellipsis;
          overflow: hidden;
        }
        .select-input option:hover {
          background-color: #f5f5f5;
        }
        /* Dark mode select input adjustments */
        body.dark-mode .select-input,
        body.dark-mode .select-input select {
          background-color: #1e1e1e !important;
          color: #d4d4d4 !important;
          border: 1px solid #444 !important;
        }
        /* Modal dialog dark mode */
        body.dark-mode .modal-content {
          background-color: #1e1e1e;
          color: #d4d4d4;
        }
        body.dark-mode .modal-footer .btn {
          background-color: #333;
          border-color: #555;
          color: #d4d4d4;
        }
        body.dark-mode .archived-rules select {
          background-color: #1e1e1e;
          color: #d4d4d4;
          border: 1px solid #444;
        }
      ")),
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
    # Fixed header with Settings (left), Title (center), and Dark Mode toggle (right)
    div(
      id = "header",
      actionButton("open_settings", label = "", icon = icon("cog"), class = "btn btn-info"),
      div(id = "title", "Snakemaker"),
      actionButton("toggle_theme", span(icon("moon"), " Dark Mode"), class = "btn btn-secondary")
    ),
    # Main content
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
                          size = 2,
                          width = "100%")
            ),
            conditionalPanel(
              condition = "input.menu_choice == 'term_history'",
              selectInput("selected_term", "Select a line:",
                          choices = term_history,
                          selectize = FALSE,
                          size = 2,
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
