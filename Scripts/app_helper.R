library(shiny)
library(shinydashboard)  # for box() function
library(dplyr)           # for %>%, mutate(), filter(), pull()
library(ggplot2)         # for plotting functions
library(grid)            # for grid.newpage(), grid.lines(), gpar()

source("Scripts/antragsgruen_api.R")



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

state_dir <- file.path(getwd(), "temp", "simep_state")
if (!dir.exists(state_dir)) {
  dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
}

party_state_file <- function(id) {
  file.path(state_dir, paste0("simep_party_", id, "_state.rds"))
}

team_state_file <- function(idp) {
  file.path(state_dir, paste0("simep_team_", idp, "_state.rds"))
}

default_party_state <- list(
  type = "empty",
  res = "",
  plot_data = NULL,
  import = NULL,
  section = ""
)

read_party_state <- function(path) {
  st <- if (file.exists(path)) readRDS(path) else default_party_state
  if (is.null(st$section)) st$section <- ""
  st
}

default_team_state <- list(
  type = "empty",
  res = "",
  plot_data = NULL,
  import = NULL,
  section = ""
)

read_team_state <- function(path) {
  st <- if (file.exists(path)) readRDS(path) else default_team_state
  if (is.null(st$section)) st$section <- ""
  st
}

party_input_ui <- function(id, info) {
  tabPanel(info$name,
           fluidRow(
             box(width = 6,
                 # Fraktionsvize is read from Vizes.xlsx; no input here anymore
                 textInput(paste0(id, "_section"), "Abschnitt:"),
                 uiOutput(paste0(id, "_amendment_ui"))
             ),
             box(width = 6,
                 numericInput(paste0(id, "_yes"), "Ja:", value = NA),
                 numericInput(paste0(id, "_no"), "Nein:", value = NA),
                 numericInput(paste0(id, "_abst"), "Enthaltung:", value = NA),
                 actionButton(paste0(id, "_button"), "Ergebnis")
             )
           )
  )
}

# Team (combined caucus) UI helpers (support arbitrary number of members)
team_input_ui <- function(members, infos) {
  idp <- paste0("team_", paste(members, collapse = "_"), "_")
  title <- paste(sapply(infos, function(i) i$name), collapse = " + ")
  tabPanel(title,
           fluidRow(
             box(width = 6,
                 textInput(paste0(idp, "section"), "Abschnitt:"),
                 uiOutput(paste0(idp, "amendment_ui"))
             ),
             box(width = 6,
                 numericInput(paste0(idp, "yes"), "Ja:", value = NA),
                 numericInput(paste0(idp, "no"), "Nein:", value = NA),
                 numericInput(paste0(idp, "abst"), "Enthaltung:", value = NA),
                 actionButton(paste0(idp, "button"), "Ergebnis")
             )
           )
  )
}

team_output_ui <- function(members, infos) {
  idp <- paste0("team_", paste(members, collapse = "_"), "_")
  title <- paste(sapply(infos, function(i) i$name), collapse = " + ")
  # build logos html
  logos_html <- paste0(sapply(infos, function(i) as.character(tags$img(src = i$logo, width = 80))), collapse = "")
  tabPanel(title,
           fluidRow(
             box(width = 3,
                 tags$figure(class = "centerFigure",
                             tags$img(src = "EP_Logo.png", height = 142),
                             tags$div(style = "margin-top: 8px;", HTML(logos_html))
                 )
             ),
             box(width = 9, align = "left",
                 # create CSS for each vize id and section
                 tags$head(tags$style(HTML(paste0(
                   paste0(sapply(members, function(m) paste0("#", idp, "vize_", m, "{color: black; font-size: 24px;}")), collapse = ""),
                   "#", idp, "section_print{color: black; font-size: 24px;}",
                   "#", idp, "amendment_print{color: black; font-size: 24px;}"
                 )))),
                 # vize outputs for each member
                 tagList(lapply(members, function(m) uiOutput(paste0(idp, "vize_", m, "_print")))),
                 uiOutput(paste0(idp, "section_print")),
                 uiOutput(paste0(idp, "amendment_print"))
             )
           ),
           fluidRow(
             box(width = 3,
                 tags$head(tags$style(HTML(paste0("#", idp, "res_print{color: black; font-size: 28px; font-weight: bold;}")))),
                 textOutput(paste0(idp, "res_print")),
                 uiOutput(paste0(idp, "res_img"))
             ),
             box(width = 9, plotOutput(paste0(idp, "chart")))
           )
  )
}

# Server helper for a combined team (creates outputs/observers using dynamic ids)
team_server <- function(members, infos, input, output, session, motion_id = reactive(56456), vize_lookup_fn = NULL) {
  idp <- paste0("team_", paste(members, collapse = "_"), "_")
  state_file <- team_state_file(idp)
  state <- reactiveFileReader(1000, session, state_file, read_team_state)

  save_team_state <- function(updated_state) {
    saveRDS(updated_state, state_file)
  }

  amendments_data <- reactive({
    mid <- motion_id()
    if (is.null(mid) || !is.numeric(mid) || mid <= 0) return(data.frame(id = numeric(0), prefix = character(0), initiators = character(0), url_json = character(0), stringsAsFactors = FALSE))
    tryCatch(antragsgruen_get_amendments(mid), error = function(e) data.frame(id = numeric(0), prefix = character(0), initiators = character(0), url_json = character(0), stringsAsFactors = FALSE))
  })

  output[[paste0(idp, "amendment_ui")]] <- renderUI({
    amendments <- amendments_data()
    if (nrow(amendments) == 0) return(selectInput(paste0(idp, "amendment"), "Änderungsantrag auswählen:", choices = c("-- Keine Änderungsanträge gefunden --" = "")))
    choices <- setNames(as.character(amendments$url_json), paste0("[", amendments$prefix, "] ID: ", amendments$id, " (", amendments$initiators, ")"))
    selectInput(paste0(idp, "amendment"), "Änderungsantrag auswählen:", choices = c("-- Wählen Sie einen Änderungsantrag --" = "", choices))
  })

  observeEvent(input[[paste0(idp, "section")]], {
    current_state <- state()
    save_team_state(list(
      type = if (!is.null(current_state$type)) current_state$type else "empty",
      res = if (!is.null(current_state$res)) current_state$res else "",
      plot_data = if (!is.null(current_state$plot_data)) current_state$plot_data else NULL,
      import = if (!is.null(current_state$import)) current_state$import else NULL,
      section = input[[paste0(idp, "section")]]
    ))
  }, ignoreInit = TRUE)

  observeEvent(input[[paste0(idp, "amendment")]], {
    selected_url <- input[[paste0(idp, "amendment")]]
    if (!is.null(selected_url) && nzchar(selected_url)) {
      tryCatch({
        imp <- antragsgruen_import_case(selected_url)
        current_state <- state()
        save_team_state(list(
          type = if (!is.null(current_state$type) && current_state$type == "result") current_state$type else "imported",
          res = if (!is.null(current_state$res)) current_state$res else "",
          plot_data = if (!is.null(current_state$plot_data)) current_state$plot_data else NULL,
          import = imp,
          section = if (!is.null(input[[paste0(idp, "section")]]) && nzchar(input[[paste0(idp, "section")]])) input[[paste0(idp, "section")]] else if (!is.null(current_state$section)) current_state$section else ""
        ))
      }, error = function(e) {
        save_team_state(list(type = "error", res = conditionMessage(e), plot_data = NULL, import = NULL, section = if (!is.null(input[[paste0(idp, "section")]]) && nzchar(input[[paste0(idp, "section")]])) input[[paste0(idp, "section")]] else if (!is.null(state()$section)) state()$section else ""))
      })
    }
  })
  # render vize outputs for each member
  for (i in seq_along(members)) {
    m <- members[i]
    info <- infos[[i]]
    local({m_local <- m; info_local <- info
      output[[paste0(idp, "vize_", m_local, "_print")]] <- renderUI({
        display <- if (!is.null(info_local$name)) info_local$name else m_local
        vname <- ""
        if (is.function(vize_lookup_fn)) {
          vname <- vize_lookup_fn(m_local, display)
        }
        HTML(paste0("<div id=\"", idp, "vize_", m_local, "\">", "<div style='font-weight: bold; display: inline-block;'>Fraktionsvize (", display, "):</div> ", vname, "</div>"))
      })
    })
  }
  output[[paste0(idp, "section_print")]] <- renderUI({ HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Abschnitt:</div> ", state()$section)) })

  observeEvent(input[[paste0(idp, "button")]], {
    yes <- as.numeric(input[[paste0(idp, "yes")]])
    no <- as.numeric(input[[paste0(idp, "no")]])
    abst <- as.numeric(input[[paste0(idp, "abst")]])
    new_data <- data.frame(cat = factor(c('Ja', 'Nein', 'Enthaltung'), levels = c('Ja', 'Nein', 'Enthaltung')), val = c(yes, no, abst))
    colnames(new_data)[2] <- paste0(idp, "val")
    res_value <- ""
    if (is.na(yes) | is.na(no) | is.na(abst)) {
      res_value <- ""
    } else if (yes > no) {
      res_value <- "Der Änderungsantrag ist angenommen!"
    } else {
      res_value <- "Der Änderungsantrag ist abgelehnt!"
    }
    save_team_state(list(
      type = "result",
      res = res_value,
      plot_data = new_data,
      import = if (!is.null(state()$import)) state()$import else NULL,
      section = if (!is.null(input[[paste0(idp, "section")]]) && nzchar(input[[paste0(idp, "section")]])) input[[paste0(idp, "section")]] else if (!is.null(state()$section)) state()$section else ""
    ))
  })

  output[[paste0(idp, "chart")]] <- renderPlot({
    st <- state()
    if (is.null(st) || st$type == "empty" || is.null(st$plot_data)) {
      plot_empty_circle()
    } else {
      # use the second column name as value column
      plot_result_circle(st$plot_data, colnames(st$plot_data)[2])
    }
  })

  output[[paste0(idp, "res_print")]] <- renderText({ state()$res })
  output[[paste0(idp, "res_img")]] <- renderUI({
    if (state()$res == "Der Änderungsantrag ist angenommen!") {
      img(src = "angenommen.png", height = "100px", width = "100px")
    } else if (state()$res == "Der Änderungsantrag ist abgelehnt!") {
      img(src = "abgelehnt.png", height = "100px", width = "100px")
    }
  })

  output[[paste0(idp, "amendment_print")]] <- renderUI({
    imported_case <- state()$import
    if (!is.null(imported_case) && !is.null(imported_case$sections) && length(imported_case$sections) > 0) {
      amendment_html <- lapply(imported_case$sections, function(section) {
        html <- section$new_html
        if (!antragsgruen_has_text(html)) html <- section$old_html
        if (!antragsgruen_has_text(html)) html <- section$diff_html
        if (antragsgruen_has_text(html) && !grepl("Kein Text aus der API verf", html)) html else NULL
      })
      amendment_html <- Filter(Negate(is.null), amendment_html)
      if (length(amendment_html) > 0) {
        content <- paste(amendment_html, collapse = "")
        content <- gsub("Von Zeile\\s*\\d+\\s*bis\\s*\\d+[^:]*:\\s*", "", content)
        return(HTML(paste0("<div style='font-weight: bold; display: inline-block; margin-bottom: 10px;'>Änderungsantrag:</div>", "<div class='paragraph'><div class='text motionTextFormattings'>", content, "</div></div>")))
      }
    }
    return(NULL)
  })
}

party_output_ui <- function(id, info) {
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
                   "#", id, "_amendment_print{color: black; font-size: 24px; font-style: bold;}"
                 )))),
                 uiOutput(paste0(id, "_vize")),
                 uiOutput(paste0(id, "_section_print")),
                 uiOutput(paste0(id, "_amendment_print"))
             )
           ),
           fluidRow(
             box(width = 3,
                 tags$head(tags$style(HTML(paste0("#", id, "_res_print{color: black; font-size: 30px; font-style: bold;}")))),
                 textOutput(paste0(id, "_res_print")),
                 uiOutput(paste0(id, "_res_img"))
             ),
             box(width = 9, plotOutput(paste0(id, "_chart")))
           )
  )
}

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

party_server <- function(id, info, input, output, session, motion_id = reactive(56456), vize_lookup_fn = NULL) {
  state_file <- party_state_file(id)
  state <- reactiveFileReader(1000, session, state_file, read_party_state)

  save_party_state <- function(updated_state) {
    saveRDS(updated_state, state_file)
  }

  # Fetch amendments when motion_id changes
  amendments_data <- reactive({
    mid <- motion_id()
    if (is.null(mid) || !is.numeric(mid) || mid <= 0) {
      return(data.frame(
        id = numeric(0),
        prefix = character(0),
        initiators = character(0),
        url_json = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    tryCatch(
      antragsgruen_get_amendments(mid),
      error = function(e) {
        data.frame(
          id = numeric(0),
          prefix = character(0),
          initiators = character(0),
          url_json = character(0),
          stringsAsFactors = FALSE
        )
      }
    )
  })

  # Render amendment dropdown UI
  output[[paste0(id, "_amendment_ui")]] <- renderUI({
    amendments <- amendments_data()
    if (nrow(amendments) == 0) {
      return(selectInput(
        paste0(id, "_amendment"),
        "Änderungsantrag auswählen:",
        choices = c("-- Keine Änderungsanträge gefunden --" = "")
      ))
    }
    
    # Create choices list with display labels
    choices <- setNames(
      as.character(amendments$url_json),
      paste0("[", amendments$prefix, "] ID: ", amendments$id, " (", amendments$initiators, ")")
    )
    
    selectInput(
      paste0(id, "_amendment"),
      "Änderungsantrag auswählen:",
      choices = c("-- Wählen Sie einen Änderungsantrag --" = "", choices)
    )
  })

  # Auto-populate source URL when amendment is selected
  observeEvent(input[[paste0(id, "_amendment")]], {
    selected_url <- input[[paste0(id, "_amendment")]]
    if (!is.null(selected_url) && nzchar(selected_url)) {
      tryCatch(
        {
          imported_case <- antragsgruen_import_case(selected_url)
          current_state <- state()
          save_party_state(list(
            type = if (!is.null(current_state$type) && current_state$type == "result") current_state$type else "imported",
            res = if (!is.null(current_state$res)) current_state$res else "",
            plot_data = if (!is.null(current_state$plot_data)) current_state$plot_data else NULL,
            import = imported_case,
            section = if (!is.null(input[[paste0(id, "_section")]]) && nzchar(input[[paste0(id, "_section")]])) input[[paste0(id, "_section")]] else if (!is.null(current_state$section)) current_state$section else ""
          ))
        },
        error = function(e) {
          current_state <- state()
          save_party_state(list(
            type = "error",
            res = conditionMessage(e),
            plot_data = NULL,
            import = NULL,
            section = if (!is.null(input[[paste0(id, "_section")]]) && nzchar(input[[paste0(id, "_section")]])) input[[paste0(id, "_section")]] else if (!is.null(current_state$section)) current_state$section else ""
          ))
        }
      )
    }
  })

  observeEvent(input[[paste0(id, "_section")]], {
    current_state <- state()
    save_party_state(list(
      type = if (!is.null(current_state$type)) current_state$type else "empty",
      res = if (!is.null(current_state$res)) current_state$res else "",
      plot_data = if (!is.null(current_state$plot_data)) current_state$plot_data else NULL,
      import = if (!is.null(current_state$import)) current_state$import else NULL,
      section = input[[paste0(id, "_section")]]
    ))
  }, ignoreInit = TRUE)

  observeEvent(input[[paste0(id, "_button")]], {
    yes <- as.numeric(input[[paste0(id, "_yes")]])
    no <- as.numeric(input[[paste0(id, "_no")]])
    abst <- as.numeric(input[[paste0(id, "_abst")]])
    new_data <- data.frame(
      cat = factor(c('Ja', 'Nein', 'Enthaltung'), levels = c('Ja', 'Nein', 'Enthaltung')),
      val = c(yes, no, abst)
    )
    colnames(new_data)[2] <- id
    res_value <- ""
    if (is.na(yes) | is.na(no) | is.na(abst)) {
      res_value <- ""
    } else if (yes > no) {
      res_value <- "Der Änderungsantrag ist angenommen!"
    } else {
      res_value <- "Der Änderungsantrag ist abgelehnt!"
    }
    save_party_state(list(
      type = "result",
      res = res_value,
      plot_data = new_data,
      import = if (!is.null(state()$import)) state()$import else NULL,
      section = if (!is.null(input[[paste0(id, "_section")]]) && nzchar(input[[paste0(id, "_section")]])) input[[paste0(id, "_section")]] else if (!is.null(state()$section)) state()$section else ""
    ))
  })
  output[[paste0(id, "_chart")]] <- renderPlot({
    if (state()$type == "empty" || is.null(state()$plot_data)) {
      plot_empty_circle()
    } else {
      plot_result_circle(state()$plot_data, id)
    }
  })
  output[[paste0(id, "_vize")]] <- renderUI({
    vname <- ""
    if (is.function(vize_lookup_fn)) {
      vname <- vize_lookup_fn(id, info$name)
    }
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Fraktionsvize:</div> ", vname))
  })
  output[[paste0(id, "_section_print")]] <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Abschnitt:</div> ", state()$section))
  })
  output[[paste0(id, "_amendment_print")]] <- renderUI({
    imported_case <- state()$import
    if (!is.null(imported_case)) {
      # Get all sections and extract just the content HTML without titles
      if (!is.null(imported_case$sections) && length(imported_case$sections) > 0) {
        # Collect all non-empty section HTML
        amendment_html <- lapply(imported_case$sections, function(section) {
          html <- section$new_html
          if (!antragsgruen_has_text(html)) {
            html <- section$old_html
          }
          if (!antragsgruen_has_text(html)) {
            html <- section$diff_html
          }
          if (antragsgruen_has_text(html) && !grepl("Kein Text aus der API verf", html)) {
            html
          } else {
            NULL
          }
        })
        # Filter out NULLs and combine
        amendment_html <- Filter(Negate(is.null), amendment_html)
        if (length(amendment_html) > 0) {
          content <- paste(amendment_html, collapse = "")
          # Remove "Von Zeile X bis Y:" pattern
            content <- gsub("Von Zeile\\s*\\d+\\s*bis\\s*\\d+[^:]*:\\s*", "", content)
          return(HTML(paste0(
            "<div style='font-weight: bold; display: inline-block; margin-bottom: 10px;'>Änderungsantrag:</div>",
            "<div class='paragraph'><div class='text motionTextFormattings'>", content, "</div></div>"
          )))
        }
      }
    }
    return(NULL)
  })
  output[[paste0(id, "_logo")]] <- renderUI({tags$img(src = info$logo, width = 150, height = 100)})
  output[[paste0(id, "_res_print")]] <- renderText(state()$res)
  output[[paste0(id, "_res_img")]] <- renderUI({
    if (state()$res == "Der Änderungsantrag ist angenommen!") {
      img(src = "angenommen.png", height = "100px", width = "100px")
    } else if (state()$res == "Der Änderungsantrag ist abgelehnt!") {
      img(src = "abgelehnt.png", height = "100px", width = "100px")
    }
  })
}

