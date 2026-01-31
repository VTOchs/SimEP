library(shiny)
library(shinydashboard)  # for box() function
library(dplyr)           # for %>%, mutate(), filter(), pull()
library(ggplot2)         # for plotting functions
library(grid)            # for grid.newpage(), grid.lines(), gpar()



# --- Helper Functions ------------------------------------------------------

collapse_diff <- function(str_list){
  for (i in seq_along(str_list)) {
    if (names(str_list)[i] == "Diff") {
      str_list[i] <- paste0("<b>", str_list[i], "</b>")
    }
  }
  str_list |> unlist() |> paste(collapse = "")
}

plot_result_circle <- function(df, party){
  df_plot <- df %>%
    mutate(x = 3) |> 
    filter(cat == "Ja" | cat == "Nein")
  abst <- df %>% filter(cat == "Enthaltung") %>% pull(party)
  main_plot <- ggplot(df_plot, aes(x = x, y = !!sym(party), fill = cat)) +
    geom_col(color = "black") +
    coord_polar(theta = "y", start = pi, direction = -1) +
    xlim(c(0.2, 3 + 0.5)) +
    theme_void() +
    scale_fill_manual(values = c('#00A86B', '#D32F2F')) +
    theme(legend.position = "none") +
    geom_text(aes(label = !!sym(party)), position = position_stack(vjust = 0.5), color = "black", size = 6) +
    annotate("text", x = 0.3, y = 0, 
             label = paste("Enthaltungen: \n", abst), 
             hjust = 0.5, vjust = 0.5, size = 5) 
  grid.newpage()
  print(main_plot)
  grid.lines(x = c(0.5, 0.5), y = c(0.79, 0.89), gp = gpar(col = "black", lwd = 4, lty = "solid", alpha = 0.8))
}

plot_empty_circle <- function(){
  df_empty <- data.frame(cat = c("Ja", "Nein"), portion = c(1,1), x = c(3,3))
  ggplot(df_empty, aes(x = x, y = portion)) +
    geom_col(color = "black", fill = NA) +
    coord_polar(theta = "y", start = pi) +
    xlim(c(0.2, 3 + 0.5)) +
    theme_void() +
    theme(legend.position = "none")
}

# Generalized word-diff highlighting for old/new text
highlight_diff <- function(old, new, mode = c("old", "new")) {
  mode <- match.arg(mode)
  old_words <- strsplit(old, "\\s+")[[1]]
  new_words <- strsplit(new, "\\s+")[[1]]
  if (mode == "old") {
    deleted_words <- setdiff(old_words, new_words)
    result_text <- old
    for (word in deleted_words) {
      result_text <- gsub(paste0("\\b", word, "\\b"), paste0("<span style='background-color: #ffdddd; font-weight: bold;'>", word, "</span>"), result_text)
    }
  } else {
    added_words <- setdiff(new_words, old_words)
    result_text <- new
    for (word in added_words) {
      result_text <- gsub(paste0("\\b", word, "\\b"), paste0("<span style='background-color: #ddffdd; font-weight: bold;'>", word, "</span>"), result_text)
    }
  }
  result_text
}


# --- UI Helper for Party Tabs --------------------------------------------

party_tab_ui <- function(id, info) {
  tabPanel(info$name,
           fluidRow(
             box(width = 3,
                 tags$figure(class = "centerFigure", tags$img(src = "EP_Logo.png", width = 150)),
                 uiOutput(paste0(id, "_logo"))
             ),
             box(width = 9, align = "left",
                 tags$head(tags$style(HTML(paste0(
                   "#", id, "_vize{color: black; font-size: 24px; font-style: bold;}",
                   "#", id, "_section_print{color: black; font-size: 24px; font-style: bold;}",
                   "#", id, "_old_print{color: black; font-size: 20px;}",
                   "#", id, "_new_print{color: black; font-size: 20px;}"
                 )))),
                 uiOutput(paste0(id, "_vize")),
                 uiOutput(paste0(id, "_section_print")),
                 uiOutput(paste0(id, "_old_print")),
                 uiOutput(paste0(id, "_new_print"))
             )
           ),
           fluidRow(
             box(width = 3,
                 tags$head(tags$style(HTML(paste0("#", id, "_res_print{color: black; font-size: 30px; font-style: bold;}")))),
                 numericInput(paste0(id, "_yes"), "Ja:", value = NA),
                 numericInput(paste0(id, "_no"), "Nein:", value = NA),
                 numericInput(paste0(id, "_abst"), "Enthaltung:", value = NA),
                 actionButton(paste0(id, "_button"), "Ergebnis"),
                 textOutput(paste0(id, "_res_print")),
                 uiOutput(paste0(id, "_res_img"))
             ),
             box(width = 9, plotOutput(paste0(id, "_chart")))
           )
  )
}



# --- Server Helper for Party Logic --------------------------------------

party_server <- function(id, info, input, output, session) {
  type <- reactiveVal("empty")
  res <- reactiveVal("")
  plot_data <- reactiveVal(NULL)
  observeEvent(input[[paste0(id, "_button")]], {
    type("result")
    yes <- as.numeric(input[[paste0(id, "_yes")]])
    no <- as.numeric(input[[paste0(id, "_no")]])
    abst <- as.numeric(input[[paste0(id, "_abst")]])
    new_data <- data.frame(
      cat = factor(c('Ja', 'Nein', 'Enthaltung'), levels = c('Ja', 'Nein', 'Enthaltung')),
      val = c(yes, no, abst)
    )
    colnames(new_data)[2] <- id
    plot_data(new_data)
    if (is.na(yes) | is.na(no) | is.na(abst)) {
      res("")
    } else if (yes > no) {
      res("Der Änderungsantrag ist angenommen!")
    } else {
      res("Der Änderungsantrag ist abgelehnt!")
    }
  })
  output[[paste0(id, "_chart")]] <- renderPlot({
    if (type() == "empty") {
      plot_empty_circle()
    } else {
      plot_result_circle(plot_data(), id)
    }
  })
  output[[paste0(id, "_vize")]] <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Fraktionsvize:</div> ", input[[paste0(id, "_vize")]]))
  })
  output[[paste0(id, "_section_print")]] <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Abschnitt:</div> ", input[[paste0(id, "_section")]]))
  })
  output[[paste0(id, "_old_print")]] <- renderUI({
    req(input[[paste0(id, "_old")]], input[[paste0(id, "_new")]])
    if (input[[paste0(id, "_old")]] == "" || input[[paste0(id, "_new")]] == "") {
      return(HTML("<div>Please enter text to compare</div>"))
    }
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Alter Text:</div> ", highlight_diff(input[[paste0(id, "_old")]], input[[paste0(id, "_new")]], mode = "old")))
  })
  output[[paste0(id, "_new_print")]] <- renderUI({
    req(input[[paste0(id, "_old")]], input[[paste0(id, "_new")]])
    if (input[[paste0(id, "_old")]] == "" || input[[paste0(id, "_new")]] == "") {
      return(HTML("<div>Please enter text to compare</div>"))
    }
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Neuer Text:</div> ", highlight_diff(input[[paste0(id, "_old")]], input[[paste0(id, "_new")]], mode = "new")))
  })
  output[[paste0(id, "_logo")]] <- renderUI({tags$img(src = info$logo, width = 150, height = 100)})
  output[[paste0(id, "_res_print")]] <- renderText(res())
  output[[paste0(id, "_res_img")]] <- renderUI({
    if (res() == "Der Änderungsantrag ist angenommen!") {
      img(src = "angenommen.png", height = "100px", width = "100px")
    } else if (res() == "Der Änderungsantrag ist abgelehnt!") {
      img(src = "abgelehnt.png", height = "100px", width = "100px")
    }
  })
}