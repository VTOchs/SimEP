library(tidyverse)

# Funktionen --------------------------------------------------------------

groupsEP4 <- c("EVP", "SD", "RE", "PfE")

countries <-  c("AUT", "BEL", "BGR", "HRV",
                "CYP", "CZE", "DNK", "EST",
                "FIN", "FRA", "DEU", "GRC",
                "HUN", "IRL", "ITA", "LVA",
                "LTU", "LUX", "MLT", "NLD",
                "POL", "PRT", "ROU", "SVK",
                "SVN", "ESP", "SWE")



# für die LaTeX-Dokumente
translation_data_latex <- data.frame(
  en = c("EPP", "S&D", "Renew", "G / EFA", "PfE", "ECR", "The Left", "ESN", "Verts/ALE", "PPE", "Greens/EFA"),
  de = c("EVP", "SD", "RE", "Green", "PfE", "EKR", "Left", "ESN", "Green", "EVP", "Green")
)

translation_data_country <- read.csv("Daten/data_landerpapiere.csv", stringsAsFactors = FALSE)[, c("iso", "iso2c", "country")]

get_slido_link <- function(city, group){
  slido_link <- read_excel("Daten/data_slido.xlsx") |> 
    filter(Stadt == city) |> 
    pull(group)
}

translate_latex <- function(group){
  translation_data_latex[translation_data_latex$en == group, "de"]
}

normalize_country_codes <- function(country_values, strict = TRUE, context = "country values") {
  raw_values <- as.character(unlist(country_values, use.names = FALSE))
  raw_values <- trimws(raw_values)
  raw_values <- raw_values[!is.na(raw_values) & nzchar(raw_values)]

  if (length(raw_values) == 0) {
    return(character(0))
  }

  iso3 <- toupper(trimws(as.character(translation_data_country$iso)))
  iso2 <- toupper(trimws(as.character(translation_data_country$iso2c)))
  country <- tolower(trimws(as.character(translation_data_country$country)))

  valid_iso3 <- !is.na(iso3) & nzchar(iso3)
  iso3 <- iso3[valid_iso3]
  iso2 <- iso2[valid_iso3]
  country <- country[valid_iso3]

  normalized <- rep(NA_character_, length(raw_values))
  raw_upper <- toupper(raw_values)
  raw_lower <- tolower(raw_values)

  idx_iso3 <- match(raw_upper, iso3)
  hit_iso3 <- !is.na(idx_iso3)
  normalized[hit_iso3] <- iso3[idx_iso3[hit_iso3]]

  remaining <- is.na(normalized)
  if (any(remaining)) {
    idx_iso2 <- match(raw_upper[remaining], iso2)
    hit_iso2 <- !is.na(idx_iso2)
    normalized[which(remaining)[hit_iso2]] <- iso3[idx_iso2[hit_iso2]]
  }

  remaining <- is.na(normalized)
  if (any(remaining)) {
    idx_country <- match(raw_lower[remaining], country)
    hit_country <- !is.na(idx_country)
    normalized[which(remaining)[hit_country]] <- iso3[idx_country[hit_country]]
  }

  unknown_values <- unique(raw_values[is.na(normalized)])
  if (strict && length(unknown_values) > 0) {
    stop(
      paste0(
        "Unbekannte Länderwerte in ", context, ": ",
        paste(unknown_values, collapse = ", "),
        ". Erwartet werden ISO3, ISO2 oder Landname aus Daten/data_landerpapiere.csv."
      )
    )
  }

  normalized
}

translate_country_iso <- function(country_code) {
  if (length(country_code) == 0) {
    return(character(0))
  }

  normalized <- normalize_country_codes(country_code, strict = FALSE, context = "translate_country_iso")
  iso3 <- toupper(trimws(as.character(translation_data_country$iso)))
  translated <- translation_data_country$country[match(normalized, iso3)]
  ifelse(is.na(translated), normalized, translated)
}

translate_sus_group <- function(group) {
  translation_data_group <- c(
    EVP = "EVP",
    SD = "S&D",
    RE = "Renew",
    PfE = "PfE",
    Green = "Grüne",
    Left = "Linke"
  )

  translated <- unname(translation_data_group[group])
  ifelse(is.na(translated), group, translated)
}

build_sus_overview <- function(susFrakLand) {
  all_countries <- normalize_country_codes(
    unlist(susFrakLand, use.names = FALSE),
    strict = TRUE,
    context = "build_sus_overview"
  )
  all_countries <- sort(unique(all_countries))

  country_names <- translate_country_iso(all_countries)
  ord <- order(country_names)
  country_names <- country_names[ord]
  all_countries <- all_countries[ord]

  overview <- data.frame(
    Land = country_names,
    stringsAsFactors = FALSE
  )

  group_names <- names(susFrakLand)
  for (group in group_names) {
    group_countries <- normalize_country_codes(
      susFrakLand[[group]],
      strict = TRUE,
      context = paste0("Fraktion ", group)
    )

    overview[[translate_sus_group(group)]] <- vapply(all_countries, function(country) {
      sum(group_countries == country)
    }, integer(1))
  }

  count_cols <- setdiff(names(overview), "Land")
  overview$Gesamt <- rowSums(overview[, count_cols, drop = FALSE])
  # Sort alphabetically by German country name
  overview <- overview[order(overview$Land), ]

  overview
}

save_sus_overview_pdf <- function(susOverview, file_path, title) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  pdf(file_path, width = 11.69, height = 8.27, family = "Helvetica")
  on.exit(dev.off(), add = TRUE)

  grid::grid.newpage()
  grid::pushViewport(grid::viewport(width = 0.94, height = 0.92))

  grid::grid.text(
    title,
    x = 0.5,
    y = 0.97,
    gp = grid::gpar(fontsize = 20, fontface = "bold", col = "#16361f")
  )

  grid::grid.text(
    "Anzahl der Länder je Fraktion; alphabetisch nach Land sortiert.",
    x = 0.5,
    y = 0.935,
    gp = grid::gpar(fontsize = 10.5, col = "#4b4b4b")
  )

  table_top <- 0.88
  table_bottom <- 0.06
  table_height <- table_top - table_bottom
  n_rows <- nrow(susOverview) + 1
  n_cols <- ncol(susOverview)
  row_height <- table_height / n_rows
  col_widths <- c(0.28, rep((1 - 0.28) / (n_cols - 1), n_cols - 1))
  col_lefts <- c(0, cumsum(col_widths)[-n_cols])

  header_fill <- "#1f6b45"
  row_fill_a <- "#f5f9f6"
  row_fill_b <- "#e8f1eb"
  border_col <- "#b7c8bd"

  draw_cell <- function(x, y, width, height, label, fill, col = "#1f1f1f", fontface = "plain", just = "center") {
    grid::grid.rect(
      x = x + width / 2,
      y = y - height / 2,
      width = width,
      height = height,
      gp = grid::gpar(fill = fill, col = border_col, lwd = 0.8)
    )
    grid::grid.text(
      label,
      x = x + if (just == "left") 0.01 else width / 2,
      y = y - height / 2,
      just = just,
      gp = grid::gpar(fontsize = 10.5, col = col, fontface = fontface)
    )
  }

  y_top <- table_top
  for (col_idx in seq_len(n_cols)) {
    x_left <- col_lefts[col_idx]
    width <- col_widths[col_idx]
    draw_cell(
      x = x_left,
      y = y_top,
      width = width,
      height = row_height,
      label = names(susOverview)[col_idx],
      fill = header_fill,
      col = "white",
      fontface = "bold",
      just = if (col_idx == 1) "left" else "center"
    )
  }

  for (row_idx in seq_len(nrow(susOverview))) {
    y_top <- table_top - row_height * row_idx
    fill <- if (row_idx %% 2 == 1) row_fill_a else row_fill_b
    for (col_idx in seq_len(n_cols)) {
      x_left <- col_lefts[col_idx]
      width <- col_widths[col_idx]
      value <- susOverview[row_idx, col_idx, drop = TRUE]
      draw_cell(
        x = x_left,
        y = y_top,
        width = width,
        height = row_height,
        label = as.character(value),
        fill = fill,
        just = if (col_idx == 1) "left" else "center"
      )
    }
  }

  grid::grid.text(
    paste("Datei:", basename(file_path)),
    x = 0,
    y = 0.015,
    just = "left",
    gp = grid::gpar(fontsize = 8.5, col = "#666666")
  )
}

dhondt <- function (parties, votes, n_seats){
  divisors <- 1:n_seats
  votes <- tibble(PARTY = as.character(parties), VOTES = votes)
  quotiens <- as_tibble(expand.grid(PARTY = parties, DIVISOR = divisors)) %>% 
    mutate(PARTY = as.character(PARTY)) %>% left_join(votes, 
                                                      by = "PARTY") %>% mutate(QUOTIENTS = VOTES/DIVISOR) %>% 
    mutate(ORDER = rank(-QUOTIENTS, ties.method = "max"))
  seats <- quotiens %>% arrange(ORDER) %>% filter(ORDER <= 
                                                    length(divisors)) %>% group_by(PARTY) %>% summarise(SEATS = n())
  
  
  undisputed <- quotiens %>% arrange(ORDER) %>% filter(ORDER <= length(divisors))
  
  candidates <- quotiens %>% filter(ORDER > length(divisors)) %>% 
    mutate(TIES_ORDER = rank(ORDER, ties.method = "min")) %>%
    filter(TIES_ORDER == 1) |> 
    arrange(desc(VOTES)) |> 
    head(n_seats - undisputed$ORDER |> max())
  
  seats <- seats |> mutate(SEATS = case_when(PARTY %in% candidates$PARTY ~ SEATS + 1,
                                             !PARTY %in% candidates$PARTY ~ SEATS))
  if (seats$SEATS |> sum() == n_seats) {
    seats |> select(PARTY, SEATS)
  } else {
    print("Sitzaufteilungsfehler")
  }
}



get_sus_dist <- function(numSuS, groupsEP, landDist = T){
  if (landDist) {
    listTC <- readRDS("Daten/country_party.rds")
    attributes(listTC)$names <- sapply(attributes(listTC)$names, translate_latex) |> unlist()
    dfCG <- get_sus_dist(numSuS = numSuS, groupsEP, landDist = F) 
    listDist <- list()
    for (group in groupsEP) {
      numDep <- dfCG |> filter(Fraktion == group) |> select(SuS)
      if (is.null(listTC[[group]])) {
        stop(paste0("Keine Länderliste für Fraktion gefunden: ", group))
      }

      listDist[[group]] <- normalize_country_codes(
        sample(listTC[[group]], numDep$SuS, replace = T),
        strict = TRUE,
        context = paste0("country_party.rds / ", group)
      )
    }
    listDist
  } else {
    df_caucus <- read.csv("Daten/caucus_data.csv")
    df_caucus$party <- df_caucus$party |> sapply(translate_latex)
    df_caucus <- df_caucus |> filter(party %in% groupsEP)
    
    partyDist <- dhondt(parties = df_caucus$party,
                        votes = df_caucus$total,
                        n_seats = numSuS)
    
    partyDist |> arrange(desc(SEATS)) |> rename(SuS = SEATS,
                                                Fraktion = PARTY)
  }
}


move_temp_files <- function(target_dir, file_ext, source_dir = "."){
  
  files_to_move <- list.files(path = source_dir, 
                              pattern = paste0("\\.", file_ext, "$"), 
                              full.names = FALSE)
  
  # Create the target directory if it doesn't exist
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
  for (file in files_to_move) {
    file_name <- basename(file)
    file.rename(from = paste0(source_dir, "/", file_name), to = file.path(target_dir, file_name))
  }  
}

compile_tex_checked <- function(tex_path, clean = TRUE, max_runs = 3) {
  if (!file.exists(tex_path)) {
    stop(paste("TeX-Datei nicht gefunden:", tex_path))
  }

  tex_file <- basename(tex_path)
  pdf_name <- sub("\\.tex$", ".pdf", tex_file)
  log_name <- sub("\\.tex$", ".log", tex_file)
  aux_names <- c(
    sub("\\.tex$", ".aux", tex_file),
    sub("\\.tex$", ".log", tex_file),
    sub("\\.tex$", ".out", tex_file),
    sub("\\.tex$", ".nav", tex_file),
    sub("\\.tex$", ".toc", tex_file),
    sub("\\.tex$", ".snm", tex_file)
  )

  find_artifact <- function(file_name) {
    candidates <- unique(c(
      file.path(getwd(), file_name),
      file.path(dirname(tex_path), file_name)
    ))

    existing <- candidates[file.exists(candidates)]
    if (length(existing) == 0) {
      return(NA_character_)
    }

    existing[[1]]
  }

  for (run_idx in seq_len(max_runs)) {
    tryCatch(
      tools::texi2pdf(tex_path, clean = FALSE),
      error = function(error) {
        stop(paste("LaTeX-Kompilierung fehlgeschlagen:", tex_path, "\n", conditionMessage(error)))
      }
    )

    pdf_path <- find_artifact(pdf_name)
    if (is.na(pdf_path) || !file.exists(pdf_path)) {
      stop(paste("PDF konnte nicht erzeugt werden:", pdf_path))
    }

    log_path <- find_artifact(log_name)
    if (is.na(log_path)) {
      return(pdf_path)
    }

    # Read as raw bytes to avoid locale/encoding conversion issues in TeX logs.
    log_lines <- tryCatch(
      readLines(log_path, warn = FALSE, encoding = "bytes"),
      error = function(error) readLines(log_path, warn = FALSE)
    )

    rerun_pattern <- "Rerun to get cross-references right|Label\\(s\\) may have changed|Rerun filecheck"
    rerun_needed <- any(grepl(rerun_pattern, log_lines, useBytes = TRUE))

    if (!rerun_needed) {
      if (clean) {
        for (artifact_name in aux_names) {
          for (artifact_path in unique(c(
            file.path(getwd(), artifact_name),
            file.path(dirname(tex_path), artifact_name)
          ))) {
            if (file.exists(artifact_path)) {
              file.remove(artifact_path)
            }
          }
        }
      }
      return(pdf_path)
    }
  }

  stop(paste("LaTeX-Build wurde nach", max_runs, "Durchläufen nicht stabil:", tex_path))
}


# Server helper functions moved from Drucker.R
get_committee_summary <- function() {
  if (file.exists("Daten/committee_data.csv")) {
    committee_df <- read.csv("Daten/committee_data.csv")
    committee_df$meps <- as.numeric(committee_df$meps)
    return(list(
      totcoms = nrow(committee_df),
      minmems = min(committee_df$meps, na.rm = TRUE),
      maxmems = max(committee_df$meps, na.rm = TRUE)
    ))
  }

  list(totcoms = 0, minmems = 0, maxmems = 0)
}


generate_draft_pdf <- function(topic) {
  tex_path <- file.path("LaTeX", "Gesetzesentwürfe", paste0("Entwurf_", topic, ".tex"))
  pdf_name <- paste0("Entwurf_", topic, ".pdf")
  pdf_path <- file.path("LaTeX", "Gesetzesentwürfe", pdf_name)

  if (!file.exists(tex_path)) {
    stop(paste("Gesetzesentwurf nicht gefunden:", tex_path))
  }

  compile_tex_checked(tex_path, clean = T)

  if (file.exists(pdf_name)) {
    file.copy(pdf_name, pdf_path, overwrite = TRUE)
    file.remove(pdf_name)
  }

  if (!file.exists(pdf_path)) {
    stop(paste("PDF konnte nicht erzeugt werden:", pdf_path))
  }

  pdf_path
}


generate_ausschussuebersicht <- function(committees, motion_id, fifth_group, city) {
  output_dir <- file.path(city, "Ausschüsse")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  pdfs <- character(0)
  for (committee in committees) {
    args <- c("Scripts/generate_antragsgruen_compare_tex.R", committee, as.character(motion_id), "--force-left=1")
    if (identical(fifth_group, "Grüne")) {
      args <- c("Scripts/generate_antragsgruen_compare_tex.R", committee, as.character(motion_id))
    }

    status <- system2("Rscript", args = args, stdout = TRUE, stderr = TRUE)
    slide_snippet <- file.path("LaTeX", "Meta", sprintf("antragsgruen_slide_%s.tex", committee))

    if (!file.exists(slide_snippet)) {
      warning(sprintf(
        "Ausschussübersicht konnte für %s nicht aktualisiert werden. Ausgabe: %s",
        committee,
        paste(status, collapse = "\n")
      ))
      next
    }

    sink("LaTeX/Meta/var.tex")
    paste0("\\newcommand\\Committee{", committee, "}\n") |> cat()
    paste0("\\newcommand\\Fraktion{LEER}\n") |> cat()
    sink()

    compile_tex_checked("LaTeX/Folien/Ausschusssitzung.tex", clean = TRUE)
    target_pdf <- file.path(output_dir, paste0(committee, ".pdf"))
    if (!file.rename("Ausschusssitzung.pdf", target_pdf)) {
      warning(sprintf("Konnte erzeugtes PDF nicht nach %s verschieben.", target_pdf))
      next
    }

    pdfs <- c(pdfs, target_pdf)
  }

  pdfs
}


get_committees_for_topic <- function(topic) {
  switch(topic,
    "Green Deal" = c("AGRI", "BUDG", "ITRE", "TRAN"),
    "Asyl" = c("BUDG", "DROI", "EMPL", "LIBE"),
    "Armee" = c("BUDG", "LIBE", "SEDE"),
    c("BUDG", "LIBE", "SEDE")  # default
  )
}