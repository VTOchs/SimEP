library(diffobj)
library(ggplot2)
library(grid)
library(rsconnect)
library(shiny)
library(shinydashboard)
library(stringr)
library(tidyverse)
library(rlang)

source("Scripts/app_helper.R")

# rsconnect::setAccountInfo(name='jef-bayern',
# 			  token=Sys.getenv("shinyToken"),
# 			  secret=Sys.getenv("shinySecret"))


# --- Party Info List -----------------------------------------------------

party_info <- list(
  evp   = list(name = "EVP",   logo = "EPP.png",   color = "#003399", var = "evp"),
  sd    = list(name = "S&D",   logo = "S&D.png",   color = "#E2011A", var = "sd"),
  renew = list(name = "Renew", logo = "Renew.png", color = "#FFD700", var = "renew"),
  green = list(name = "Grüne", logo = "Greens.png", color = "#00A86B", var = "green"),
  pfe   = list(name = "PfE",   logo = "PfE.png",   color = "#000000", var = "pfe"),
  left  = list(name = "Linke", logo = "Left.png", color = "#5f013b", var = "left")
)

party_choices <- setNames(names(party_info), lapply(party_info, function(x) x$name))

# parse teams string like "evp+sd; renew+green" -> list(list(a='evp',b='sd'),...)
parse_teams <- function(s) {
  if (is.null(s) || !nzchar(s)) return(list())
  parts <- unlist(strsplit(s, ";"))
  out <- list()
  for (p in parts) {
    p2 <- gsub("\\s+", "", p)
    if (!nzchar(p2)) next
    # split by + or ,
    members <- unlist(strsplit(p2, "\\+|,"))
    members <- members[members %in% names(party_info)]
    if (length(members) >= 1) {
      out[[length(out) + 1]] <- list(members = members)
    }
  }
  out
}

# build party tabs excluding parties listed in exclude_ids
party_tabs_filtered <- function(mode, fifth_id, exclude_ids = character(0)) {
  ids <- c("evp", "sd", "renew", fifth_id, "pfe")
  ids <- ids[!ids %in% exclude_ids]
  tab_builder <- switch(mode, input = party_input_ui, output = party_output_ui)
  lapply(ids, function(id) tab_builder(id, party_info[[id]]))
}

state_dir <- file.path(getwd(), "temp", "simep_state")
if (!dir.exists(state_dir)) {
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
}

config_state_file <- file.path(state_dir, "simep_config_state.rds")
default_config_state <- list(
  topic = "Armee",
  fifthGroup = "Linke",
  motion_id = 56456,
  teams = "",
  vize_evp = "",
  vize_sd = "",
  vize_renew = "",
  vize_green = "",
  vize_pfe = "",
  vize_left = ""
)

if (!file.exists(config_state_file)) {
  saveRDS(default_config_state, config_state_file)
}

read_config_state <- function(path) {
  if (file.exists(path)) readRDS(path) else default_config_state
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || !nzchar(as.character(x))) y else x
}

current_config_state <- read_config_state(config_state_file)

tot_state_file <- file.path(state_dir, "simep_tot_state.rds")
default_tot_state <- list(
  type = "empty",
  res = "",
  plot_data = NULL
)

read_tot_state <- function(path) {
  if (file.exists(path)) readRDS(path) else default_tot_state
}


# --- UI -----------------------------------------------------------------

header <- dashboardHeader(title = "Plenardebatte")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Config", tabName = "config_tab"),
    menuItem("Eingabe", tabName = "input_tab"),
    menuItem("Ergebnis", tabName = "output_tab")
  )
)

party_tabs_for_mode <- function(mode, fifth_id) {
  ids <- c("evp", "sd", "renew", fifth_id, "pfe")
  tab_builder <- switch(
    mode,
    input = party_input_ui,
    output = party_output_ui
  )
  lapply(ids, function(id) tab_builder(id, party_info[[id]]))
}

topic_result_label <- function(topic) {
  if (topic == "Armee") {
    "Entschließung"
  } else {
    "Gesetzesentwurf"
  }
}

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "config_tab",
      tabBox(
        title = "Config",
        width = 12,
        fluidRow(
          box(
            width = 6,
            selectInput(
              "config_topic",
              "Thema:",
              choices = c("Green Deal", "Asyl", "Armee"),
              selected = default_config_state$topic
            ),
            selectInput(
              "config_fifthGroup",
              "Fünfte Fraktion:",
              choices = c("Grüne", "Linke"),
              selected = default_config_state$fifthGroup
            ),
            numericInput("config_num_teams", "Anzahl Teams:", value = 0, min = 0, step = 1),
            uiOutput("config_team_checkboxes"),
            helpText("Wählen Sie für jedes Team eine oder mehrere Fraktionen."),
            numericInput(
              "config_motion_id",
              "Motion ID:",
              value = default_config_state$motion_id,
              min = 1,
              step = 1
            ),
            tags$hr(),
            tags$h4("Fraktionsvize-Namen"),
            textInput("config_vize_evp", "EVP:", value = current_config_state$vize_evp %||% ""),
            textInput("config_vize_sd", "S&D:", value = current_config_state$vize_sd %||% ""),
            textInput("config_vize_renew", "Renew:", value = current_config_state$vize_renew %||% ""),
            textInput("config_vize_green", "Grüne:", value = current_config_state$vize_green %||% ""),
            textInput("config_vize_pfe", "PfE:", value = current_config_state$vize_pfe %||% ""),
            textInput("config_vize_left", "Linke:", value = current_config_state$vize_left %||% ""),
            actionButton("config_save", "Einstellungen speichern")
          ),
          box(
            width = 6,
            tags$head(tags$style(HTML("
              #config_current {
                font-size: 16px;
                color: #333;
                padding: 15px;
                background-color: #f5f5f5;
                border-radius: 5px;
                border-left: 4px solid #00A86B;
              }
              .config-label {
                font-weight: bold;
                color: #003399;
                display: inline-block;
                min-width: 120px;
              }
              .config-value {
                font-weight: bold;
                color: #00A86B;
              }
            "))),
            uiOutput("config_current")
          )
        )
      )
    ),
    tabItem(
      tabName = "input_tab",
      uiOutput("input_tabbox")
    ),
    tabItem(
      tabName = "output_tab",
      uiOutput("output_tabbox")
    )
  )
)

ui <- dashboardPage(skin = "green", header, sidebar, body)

# --- Server -------------------------------------------------------------

server <- function(input, output, session) {
  config_state <- reactiveFileReader(1000, session, config_state_file, read_config_state)
  lookup_vize_name <- function(id, display_name = NULL) {
    cfg <- config_state()
    value <- cfg[[paste0("vize_", id)]]
    if (is.null(value) || !nzchar(as.character(value))) "" else as.character(value)
  }

  # Keep a registry of dynamic team servers registered so we don't duplicate
  registered_teams <- reactiveVal(character(0))

  register_team_if_needed <- function(members) {
    tid <- paste(members, collapse = "_")
    if (!(tid %in% registered_teams())) {
      infos <- lapply(members, function(m) party_info[[m]])
      team_server(
        members,
        infos,
        input,
        output,
        session,
        motion_id = reactive(config_state()$motion_id),
        vize_lookup_fn = lookup_vize_name
      )
      registered_teams(c(registered_teams(), tid))
    }
  }

  observe({
    teams_list <- parse_teams(config_state()$teams)
    lapply(teams_list, function(t) register_team_if_needed(t$members))
  })

  observeEvent(input$config_save, {
    # build teams string from checkbox groups
    teams_list <- c()
    n <- ifelse(is.null(input$config_num_teams), 0, as.integer(input$config_num_teams))
    for (i in seq_len(n)) {
      sel <- input[[paste0("config_team_", i)]]
      if (!is.null(sel) && length(sel) >= 1) {
        teams_list <- c(teams_list, paste(sel, collapse = "+"))
      }
    }
    teams_str <- paste(teams_list, collapse = "; ")
    saveRDS(list(
      topic = input$config_topic,
      fifthGroup = input$config_fifthGroup,
      teams = teams_str,
      motion_id = input$config_motion_id,
      vize_evp = input$config_vize_evp %||% "",
      vize_sd = input$config_vize_sd %||% "",
      vize_renew = input$config_vize_renew %||% "",
      vize_green = input$config_vize_green %||% "",
      vize_pfe = input$config_vize_pfe %||% "",
      vize_left = input$config_vize_left %||% ""
    ), config_state_file)
  })

  output$config_current <- renderUI({
    # build teams display
    teams_html <- ""
    teams_parsed <- parse_teams(config_state()$teams)
    if (length(teams_parsed) > 0) {
      parts <- lapply(teams_parsed, function(t) {
        members <- t$members
        names_list <- sapply(members, function(m) if (m %in% names(party_info)) party_info[[m]]$name else m)
        paste0("<div>", paste(names_list, collapse = " + "), "</div>")
      })
      teams_html <- paste0("<div style='margin-top:12px;'><span class='config-label'>Teams:</span>", paste(parts, collapse = ""), "</div>")
    }
    vize_html <- paste0(
      "<div style='margin-top:12px;'><span class='config-label'>Fraktionsvize:</span>",
      "<div><span class='config-label'>EVP:</span> <span class='config-value'>", config_state()$vize_evp %||% "", "</span></div>",
      "<div><span class='config-label'>S&amp;D:</span> <span class='config-value'>", config_state()$vize_sd %||% "", "</span></div>",
      "<div><span class='config-label'>Renew:</span> <span class='config-value'>", config_state()$vize_renew %||% "", "</span></div>",
      "<div><span class='config-label'>Grüne:</span> <span class='config-value'>", config_state()$vize_green %||% "", "</span></div>",
      "<div><span class='config-label'>PfE:</span> <span class='config-value'>", config_state()$vize_pfe %||% "", "</span></div>",
      "<div><span class='config-label'>Linke:</span> <span class='config-value'>", config_state()$vize_left %||% "", "</span></div>",
      "</div>"
    )
    HTML(paste0(
      "<div id='config_current'>",
      "<div style='margin-bottom: 12px;'>",
      "<span class='config-label'>Thema:</span> ",
      "<span class='config-value'>", config_state()$topic, "</span>",
      "</div>",
      "<div style='margin-bottom: 12px;'>",
      "<span class='config-label'>Fraktion:</span> ",
      "<span class='config-value'>", config_state()$fifthGroup, "</span>",
      "</div>",
      "<div>",
      "<span class='config-label'>Motion ID:</span> ",
      "<span class='config-value'>", config_state()$motion_id, "</span>",
      "</div>",
      teams_html,
      vize_html,
      "</div>"
    ))
  })

  output$input_party_tabs <- renderUI({
    fifth_id <- if (config_state()$fifthGroup == "Grüne") "green" else "left"
    tagList(party_tabs_for_mode("input", fifth_id))
  })

  output$output_party_tabs <- renderUI({
    fifth_id <- if (config_state()$fifthGroup == "Grüne") "green" else "left"
    tagList(party_tabs_for_mode("output", fifth_id))
  })

  output$input_tabbox <- renderUI({
    fifth_id <- if (config_state()$fifthGroup == "Grüne") "green" else "left"
    teams_list <- parse_teams(config_state()$teams)
    exclude_ids <- unique(unlist(lapply(teams_list, function(t) t$members)))
    tabs <- c(list(title = "Eingabe", width = 12), party_tabs_filtered("input", fifth_id, exclude_ids))
    if (length(teams_list) > 0) {
      for (t in teams_list) {
        members <- t$members
        infos <- lapply(members, function(m) party_info[[m]])
        tabs <- c(tabs, list(team_input_ui(members, infos)))
      }
    }
    tabs <- c(tabs, list(
      tabPanel("Abschlussabstimmung",
        fluidRow(
          box(width = 3,
            numericInput("tot_yes", "Ja:", value = NA),
            numericInput("tot_no", "Nein:", value = NA),
            numericInput("tot_abst", "Enthaltung:", value = NA),
            actionButton("tot_button", "Ergebnis")
          )
        )
      )
    ))
    do.call(tabBox, tabs)
  })

  # render dynamic team checkboxes in config
  output$config_team_checkboxes <- renderUI({
    n <- ifelse(is.null(input$config_num_teams), 0, as.integer(input$config_num_teams))
    if (n <= 0) return(NULL)
    boxes <- list()
    choices <- party_choices
    for (i in seq_len(n)) {
      sel <- NULL
      # try to preselect from saved config
      cfg <- config_state()
      if (!is.null(cfg$teams) && nzchar(cfg$teams)) {
        parsed <- parse_teams(cfg$teams)
        if (length(parsed) >= i) {
          sel <- parsed[[i]]$members
        }
      }
      boxes[[i]] <- box(width = 12, checkboxGroupInput(paste0("config_team_", i), paste0("Team ", i, ":"), choices = choices, selected = sel))
    }
    do.call(tagList, boxes)
  })

  output$output_tabbox <- renderUI({
    fifth_id <- if (config_state()$fifthGroup == "Grüne") "green" else "left"
    teams_list <- parse_teams(config_state()$teams)
    exclude_ids <- unique(unlist(lapply(teams_list, function(t) t$members)))
    tabs <- c(list(title = "Abstimmungsergebnis", width = 12), party_tabs_filtered("output", fifth_id, exclude_ids))
    if (length(teams_list) > 0) {
      for (t in teams_list) {
        members <- t$members
        infos <- lapply(members, function(m) party_info[[m]])
        tabs <- c(tabs, list(team_output_ui(members, infos)))
      }
    }
    tabs <- c(tabs, list(
      tabPanel("Abschlussabstimmung",
        fluidRow(
          box(width = 3,
            tags$head(tags$style("#tot_res_print{color: black; font-size: 30px; font-style: bold;}")),
            textOutput("tot_res_print"),
            uiOutput("tot_res_img")
          ),
          box(width = 9,
            tags$figure(class = "centerFigure", tags$img(src = "EP_Logo.png", height = 142))
          )
        ),
        fluidRow(plotOutput("tot_chart"))
      )
    ))
    do.call(tabBox, tabs)
  })

  # Party tabs
  lapply(names(party_info), function(id) {
    party_server(
      id,
      party_info[[id]],
      input,
      output,
      session,
      motion_id = reactive(config_state()$motion_id),
      vize_lookup_fn = lookup_vize_name
    )
  })
  # Abschlussabstimmung
  tot_state <- reactiveFileReader(1000, session, tot_state_file, read_tot_state)
  output$tot_chart <- renderPlot({
    if (tot_state()$type == "empty" || is.null(tot_state()$plot_data)) {
      plot_empty_circle()
    } else {
      plot_result_circle(tot_state()$plot_data, "tot")
    }
  })
  observeEvent(input$tot_button, {
    tot_new_data <- data.frame(
      cat = factor(c('Ja', 'Nein', 'Enthaltung'), levels = c('Ja', 'Nein', 'Enthaltung')),
      tot = c(input$tot_yes, input$tot_no, input$tot_abst)
    )
    tot_res <- ""
    if (is.na(input$tot_yes) | is.na(input$tot_no) | is.na(input$tot_abst)) {
      tot_res <- ""
    } else if (input$tot_yes > input$tot_no) {
      if (config_state()$topic == "Armee") {
        tot_res <- "Die Entschließung ist angenommen!"
      } else{
        tot_res <- "Der Gesetzesentwurf ist angenommen!"  
      }
    } else {
      if (config_state()$topic == "Armee") {
        tot_res <- "Die Entschließung ist abgelehnt!"
      } else{
        tot_res <- "Der Gesetzesentwurf ist abgelehnt!"  
      }
    }
    saveRDS(
      list(type = "result", res = tot_res, plot_data = tot_new_data),
      tot_state_file
    )
  })
  output$tot_res_print <- renderText(tot_state()$res)
  output$tot_res_img <- renderUI({
    if ((tot_state()$res == "Die Entschließung ist angenommen!") | (tot_state()$res == "Der Gesetzesentwurf ist angenommen!")) {
      img(src = "angenommen.png", height = "100px", width = "100px")
    } else if ((tot_state()$res == "Die Entschließung ist abgelehnt!") | (tot_state()$res == "Der Gesetzesentwurf ist abgelehnt!")) {
      img(src = "abgelehnt.png", height = "100px", width = "100px")
    }
  })
}

shinyApp(ui = ui, server = server)