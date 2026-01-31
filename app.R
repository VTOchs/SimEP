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
  pfe   = list(name = "PfE",   logo = "PfE.png",   color = "#00BFFF", var = "pfe")
)


# --- UI -----------------------------------------------------------------

header <- dashboardHeader(title = "Plenardebatte")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Eingabe", tabName = "input_tab"),
    menuItem("Ergebnis", tabName = "output_tab")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "input_tab",
      tabBox(
        title = "Eingabe",
        width = 12,
        tabPanel("EVP",
          textInput("evp_vize", "Fraktionsvize:"),
          textInput("evp_section", "Abschnitt:"),
          textInput("evp_old", "alter Text:"),
          textInput("evp_new", "neuer Text:")
        ),
        tabPanel("S&D",
          textInput("sd_vize", "Fraktionsvize:"),
          textInput("sd_section", "Abschnitt:"),
          textInput("sd_old", "alter Text:"),
          textInput("sd_new", "neuer Text:")
        ),
        tabPanel("Renew",
          textInput("renew_vize", "Fraktionsvize:"),
          textInput("renew_section", "Abschnitt:"),
          textInput("renew_old", "alter Text:"),
          textInput("renew_new", "neuer Text:")
        ),
        tabPanel("Grüne",
          textInput("green_vize", "Fraktionsvize:"),
          textInput("green_section", "Abschnitt:"),
          textInput("green_old", "alter Text:"),
          textInput("green_new", "neuer Text:")
        ),
        tabPanel("PfE",
          textInput("pfe_vize", "Fraktionsvize:"),
          textInput("pfe_section", "Abschnitt:"),
          textInput("pfe_old", "alter Text:"),
          textInput("pfe_new", "neuer Text:")
        ),
        tabPanel("Abschlussabstimmung",
          radioButtons("topic", "Thema:", choices = c("Green Deal/Migration", "Armee"), selected = "Green Deal/Migration")
        )
      )
    ),
    tabItem(
      tabName = "output_tab",
      tabBox(
        title = "Abstimmungsergebnis",
        width = 12,
        party_tab_ui("evp", party_info$evp),
        party_tab_ui("sd", party_info$sd),
        party_tab_ui("renew", party_info$renew),
        party_tab_ui("green", party_info$green),
        party_tab_ui("pfe", party_info$pfe),
        tabPanel("Abschlussabstimmung",
          fluidRow(
            box(width = 3,
              tags$head(tags$style("#tot_res_print{color: black; font-size: 30px; font-style: bold;}")),
              numericInput("tot_yes", "Ja:", value = NA),
              numericInput("tot_no", "Nein:", value = NA),
              numericInput("tot_abst", "Enthaltung:", value = NA),
              actionButton("tot_button", "Ergebnis")
            ),
            box(width = 6,
              tags$figure(
                textOutput("tot_res_print"),
                uiOutput("tot_res_img")
              )
            ),
            box(width = 3,
              tags$figure(class = "centerFigure", tags$img(src = "EP_Logo.png", height = 142))
            )
          ),
          fluidRow(plotOutput("tot_chart"))
        )
      )
    )
  )
)

ui <- dashboardPage(skin = "green", header, sidebar, body)

# --- Server -------------------------------------------------------------

server <- function(input, output, session) {
  # Party tabs
  lapply(names(party_info), function(id) {
    party_server(id, party_info[[id]], input, output, session)
  })
  # Abschlussabstimmung (kept as is)
  tot_type <- reactiveVal("empty")
  tot_res <- reactiveVal("")
  tot_plot_data <- reactiveVal(NULL)
  output$tot_chart <- renderPlot({
    if (tot_type() == "empty") {
      plot_empty_circle()
    } else {
      plot_result_circle(tot_plot_data(), "tot")
    }
  })
  observeEvent(input$tot_button, {
    tot_type("result")
    tot_new_data <- data.frame(
      cat = factor(c('Ja', 'Nein', 'Enthaltung'), levels = c('Ja', 'Nein', 'Enthaltung')),
      tot = c(input$tot_yes, input$tot_no, input$tot_abst)
    )
    tot_plot_data(tot_new_data)
    if (is.na(input$tot_yes) | is.na(input$tot_no) | is.na(input$tot_abst)) {
      tot_res("")
    } else if (input$tot_yes > input$tot_no) {
      if (input$topic == "Green Deal/Migration") {
        tot_res("Der Gesetzesentwurf ist angenommen!")
      } else{
        tot_res("Die Entschließung ist angenommen!")  
      }
    } else {
      if (input$topic == "Green Deal/Migration") {
        tot_res("Der Gesetzesentwurf ist abgelehnt!")
      } else{
        tot_res("Die Entschließung ist abgelehnt!")  
      }
    }
  })
  output$tot_res_print <- renderText(tot_res())
  output$tot_res_img <- renderUI({
    if ((tot_res() == "Die Entschließung ist angenommen!") | (tot_res() == "Der Gesetzesentwurf ist angenommen!")) {
      img(src = "angenommen.png", height = "100px", width = "100px")
    } else if ((tot_res() == "Die Entschließung ist abgelehnt!") | (tot_res() == "Der Gesetzesentwurf ist abgelehnt!")) {
      img(src = "abgelehnt.png", height = "100px", width = "100px")
    }
  })
}

shinyApp(ui = ui, server = server)