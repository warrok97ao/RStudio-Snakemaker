create_chat_ui <- function() {
  fluidPage(
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
      style = 'padding-bottom: 200px;',
      uiOutput('chat_output')
    ),
    tags$div(
      class = "chat-footer",
      style = 'position: fixed; bottom: 0; left: 0; right: 0; border-top: none; box-shadow: 0 -2px 12px rgba(0,0,0,0.04);',
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
}

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
        body-dark-mode .archived-rules option {
          white-space: normal !important;
          word-break: break-all;
        }
      ")),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Inter:400,500,600&display=swap"),
      tags$script(HTML("
        $(document).on('click', '#toggle_theme_settings', function() {
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
      div(
        style = "display: flex; align-items: center; gap: 10px;",
        actionButton("open_settings", label = "", icon = icon("cog"), class = "btn btn-info"),
        div(id = "title", "Snakemaker")
      ),
      # Show "Chat" button when in Snakemaker, "Back" when in Chat
      uiOutput("header_chat_or_back_btn")
    ),
    div(
      id = "main-content",
      # Show chat panel or main UI based on input$show_chat
      uiOutput("main_or_chat_ui")
    )
  )
}
