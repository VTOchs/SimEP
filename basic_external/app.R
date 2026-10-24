library(shiny)
library(ggplot2)

empty_donut <- function() {
  ggplot(
    data.frame(category = "Noch keine Abstimmung", value = 1),
    aes(x = 2, y = value)
  ) +
    geom_col(width = 0.7, fill = "#e6ebf2", color = "white") +
    coord_polar(theta = "y") +
    xlim(0, 2.5) +
    theme_void() +
    annotate(
      "text",
      x = 0.01,
      y = 0.5,
      label = "Noch keine Abstimmung",
      size = 5,
      color = "#718096"
    )
}

result_donut <- function(votes) {
  chart_data <- data.frame(
    category = factor(c("Ja", "Nein"), levels = c("Ja", "Nein")),
    value = as.numeric(votes[c("yes", "no")])
  )

  ggplot(chart_data, aes(x = 2, y = value, fill = category)) +
    geom_col(width = 0.7, color = "white", linewidth = 0.8) +
    coord_polar(theta = "y") +
    xlim(0, 2.5) +
    scale_fill_manual(
      values = c(
        "Ja" = "#2e9f5b",
        "Nein" = "#d9534f"
      ),
      guide = "none"
    ) +
    theme_void() +
    theme(
      plot.margin = margin(10, 10, 0, 10)
    ) +
    annotate(
      "text",
      x = 0.01,
      y = 0.5,
      label = paste0("Enthaltungen:\n", votes[["abstention"]]),
      size = 5,
      color = "#475569"
    )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      :root {
        --ink: #1e293b;
        --muted: #64748b;
        --blue: #3659c9;
        --blue-dark: #243b8f;
        --surface: #ffffff;
        --background: #f3f6fb;
      }
      body {
        min-height: 100vh;
        margin: 0;
        color: var(--ink);
        background: var(--background);
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }
      .app-header {
        padding: 36px 20px 42px;
        color: white;
        background: linear-gradient(135deg, var(--blue-dark), #5276e2);
        box-shadow: 0 8px 24px rgba(36, 59, 143, .2);
      }
      .app-header-inner {
        max-width: 900px;
        margin: 0 auto;
      }
      .app-kicker {
        margin: 0 0 8px;
        color: #dbe5ff;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 1.5px;
        text-transform: uppercase;
      }
      .app-header h1 {
        margin: 0;
        font-size: 34px;
        font-weight: 800;
      }
      .app-subtitle {
        max-width: 620px;
        margin: 10px 0 0;
        color: #eef2ff;
        font-size: 15px;
      }
      .app-content {
        max-width: 900px;
        margin: 24px auto 40px;
        padding: 0 20px;
      }
      .nav-tabs {
        border: 0;
      }
      .nav-tabs > li > a {
        margin-right: 8px;
        border: 0;
        border-radius: 10px 10px 0 0;
        color: var(--muted);
        font-weight: 600;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        border: 0;
        color: var(--blue-dark);
        background: var(--surface);
      }
      .tab-content {
        min-height: 520px;
        padding: 30px;
        border-radius: 0 14px 14px 14px;
        background: var(--surface);
        box-shadow: 0 12px 35px rgba(30, 41, 59, .08);
      }
      .vote-card {
        max-width: 760px;
        margin: 0 auto;
      }
      .section-intro {
        margin-bottom: 24px;
      }
      .section-intro h2 {
        margin: 0 0 7px;
        font-size: 23px;
        font-weight: 700;
      }
      .section-intro p {
        margin: 0;
        color: var(--muted);
      }
      .form-group {
        margin-bottom: 20px;
      }
      label {
        color: var(--ink);
        font-size: 13px;
        font-weight: 700;
      }
      textarea.form-control,
      input.form-control {
        border: 1px solid #d7deea;
        border-radius: 9px;
        box-shadow: none;
      }
      textarea.form-control:focus,
      input.form-control:focus {
        border-color: var(--blue);
        box-shadow: 0 0 0 3px rgba(54, 89, 201, .12);
      }
      .vote-inputs {
        padding: 20px;
        border: 1px solid #e3e8f1;
        border-radius: 12px;
        background: #f8faff;
      }
      .vote-inputs-title {
        margin: 0 0 14px;
        font-size: 15px;
        font-weight: 700;
      }
      .vote-inputs .form-group {
        margin-bottom: 0;
      }
      .btn-primary {
        padding: 11px 22px;
        border: 0;
        border-radius: 8px;
        background: var(--blue);
        box-shadow: 0 4px 10px rgba(54, 89, 201, .22);
        font-weight: 700;
      }
      .btn-primary:hover,
      .btn-primary:focus {
        background: var(--blue-dark);
      }
      .result-card {
        padding: 20px;
        border: 1px solid #e3e8f1;
        border-radius: 12px;
        background: #f8faff;
      }
      .result-title {
        margin-top: 0;
        color: var(--blue-dark);
        font-weight: 800;
      }
      .result-outcome {
        display: flex;
        align-items: center;
        gap: 10px;
        margin: 0 0 18px;
        font-size: 23px;
        font-weight: 800;
      }
      .result-outcome.accepted {
        color: #21864b;
      }
      .result-outcome.rejected {
        color: #c53d3d;
      }
      .result-outcome.pending {
        color: var(--muted);
      }
      .result-icon {
        font-size: 30px;
        line-height: 1;
      }
      .btn-default {
        margin-left: 8px;
        padding: 11px 18px;
        border: 1px solid #d7deea;
        border-radius: 8px;
        color: #475569;
        font-weight: 600;
      }
      .result-card h4 {
        margin-top: 24px;
        font-size: 14px;
        font-weight: 700;
      }
      .result-card p {
        line-height: 1.6;
      }
      .result-card ul {
        margin-bottom: 8px;
        padding-left: 20px;
        line-height: 1.8;
      }
      .total-votes {
        color: var(--muted);
        font-size: 13px;
        font-weight: 600;
      }
      .empty-result {
        margin-bottom: 22px;
      }
      @media (max-width: 600px) {
        .app-header {
          padding: 28px 20px 34px;
        }
        .app-header h1 {
          font-size: 28px;
        }
        .tab-content {
          min-height: 0;
          padding: 20px 16px 28px;
        }
        .nav-tabs > li > a {
          padding: 10px 12px;
        }
      }
    "))
  ),
  div(
    class = "app-header",
    div(
      class = "app-header-inner",
      h1("Abstimmung")
    )
  ),
  div(
    class = "app-content",
    tabsetPanel(
      id = "main_tabs",
      tabPanel(
        title = "Eingabe",
        value = "input",
        div(
          class = "vote-card",
          br(),
          textAreaInput(
            inputId = "motion",
            label = "Antrag",
            placeholder = "Antrag hier eingeben ...",
            rows = 8,
            width = "100%"
          ),
          div(
            class = "vote-inputs",
            p(class = "vote-inputs-title", "Stimmen"),
            fluidRow(
              column(4, numericInput("yes", "Ja", value = NA, min = 0, step = 1)),
              column(4, numericInput("no", "Nein", value = NA, min = 0, step = 1)),
              column(4, numericInput("abstention", "Enthaltung", value = NA, min = 0, step = 1))
            )
          ),
          br(),
          actionButton("calculate", "Ergebnis", class = "btn-primary"),
          actionButton("reset", "Zurücksetzen", class = "btn-default"),
          br(),
          br(),
          uiOutput("input_message")
        )
      ),
      tabPanel(
        title = "Ergebnis",
        value = "result",
        div(
          class = "vote-card",
          br(),
          fluidRow(
            column(width = 6, uiOutput("result")),
            column(width = 6, plotOutput("result_chart", height = "430px"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  result <- reactiveVal(NULL)
  input_message <- reactiveVal(NULL)

  observeEvent(input$calculate, {
    votes <- c(
      yes = input$yes,
      no = input$no,
      abstention = input$abstention
    )

    if (any(is.na(votes))) {
      input_message("Bitte für Ja, Nein und Enthaltung jeweils eine Zahl eingeben.")
      return()
    }

    outcome <- if (votes[["yes"]] > votes[["no"]]) {
      "Der Antrag ist angenommen."
    } else {
      "Der Antrag ist abgelehnt."
    }

    result(list(
      votes = votes,
      outcome = outcome
    ))
    input_message(NULL)
    updateTabsetPanel(session, "main_tabs", selected = "result")
  })

  observeEvent(input$reset, {
    updateTextAreaInput(session, "motion", value = "")
    updateNumericInput(session, "yes", value = NA)
    updateNumericInput(session, "no", value = NA)
    updateNumericInput(session, "abstention", value = NA)
    result(NULL)
    input_message(NULL)
    updateTabsetPanel(session, "main_tabs", selected = "input")
  })

  output$input_message <- renderUI({
    message <- input_message()
    if (is.null(message)) {
      return(NULL)
    }

    div(class = "text-danger", message)
  })

  output$result <- renderUI({
    current_result <- result()
    current_motion <- trimws(if (is.null(input$motion)) "" else input$motion)
    outcome <- if (is.null(current_result)) "Noch kein Ergebnis" else current_result$outcome
    outcome_class <- if (is.null(current_result)) {
      "pending"
    } else if (current_result$votes[["yes"]] > current_result$votes[["no"]]) {
      "accepted"
    } else {
      "rejected"
    }
    outcome_icon <- if (is.null(current_result)) {
      ""
    } else if (current_result$votes[["yes"]] > current_result$votes[["no"]]) {
      "\u2713"
    } else {
      "\u2717"
    }
    votes <- if (is.null(current_result)) {
      c(yes = 0, no = 0, abstention = 0)
    } else {
      current_result$votes
    }

    div(
      class = "result-card",
      div(
        class = paste("result-outcome", outcome_class),
        span(class = "result-icon", outcome_icon),
        span(outcome)
      ),
      h4("Antrag"),
      p(
        if (nzchar(current_motion)) {
          HTML(htmltools::htmlEscape(current_motion))
        } else {
          span(class = "text-muted", "Noch kein Antrag eingegeben.")
        }
      ),
      h4("Stimmen"),
      tags$ul(
        tags$li(paste("Ja:", votes[["yes"]])),
        tags$li(paste("Nein:", votes[["no"]])),
        tags$li(paste("Enthaltung:", votes[["abstention"]]))
      ),
      p(
        class = "total-votes",
        paste0("Gesamtstimmen: ", sum(votes))
      )
    )
  })

  output$result_chart <- renderPlot({
    current_result <- result()
    if (is.null(current_result)) {
      empty_donut()
    } else {
      result_donut(current_result$votes)
    }
  })
}

shinyApp(ui, server)
