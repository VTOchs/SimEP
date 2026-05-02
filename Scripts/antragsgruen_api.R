antragsgruen_has_text <- function(value) {
  !is.null(value) && length(value) > 0 && !is.na(value) && nzchar(value)
}

antragsgruen_value_or <- function(value, fallback = "") {
  if (!antragsgruen_has_text(value)) {
    fallback
  } else {
    value
  }
}

antragsgruen_is_json_input <- function(source) {
  grepl("^\\s*[\\[{]", source)
}

antragsgruen_read_payload <- function(source) {
  if (!nzchar(source)) {
    stop("Please enter an Antragsgrün REST URL or JSON payload.")
  }

  jsonlite::fromJSON(source, simplifyVector = FALSE)
}

antragsgruen_section_title <- function(section, fallback = "") {
  antragsgruen_value_or(section$title, fallback)
}

antragsgruen_section_html <- function(section) {
  antragsgruen_value_or(section$html, "")
}

antragsgruen_merge_sections <- function(old_sections, new_sections) {
  old_sections <- if (is.null(old_sections)) list() else old_sections
  new_sections <- if (is.null(new_sections)) list() else new_sections

  if (length(old_sections) == 0 && length(new_sections) == 0) {
    return(list())
  }

  merged <- list()
  max_count <- max(length(old_sections), length(new_sections))

  for (index in seq_len(max_count)) {
    old_section <- if (index <= length(old_sections)) old_sections[[index]] else NULL
    new_section <- if (index <= length(new_sections)) new_sections[[index]] else NULL

    if (is.null(new_section)) {
      new_section <- old_section
    }
    if (is.null(old_section)) {
      old_section <- new_section
    }

    title_candidates <- c(
      antragsgruen_section_title(new_section),
      antragsgruen_section_title(old_section),
      paste("Abschnitt", index)
    )
    title_idx <- which(nzchar(title_candidates))[1]
    section_title <- if (is.na(title_idx)) paste("Abschnitt", index) else title_candidates[[title_idx]]

    merged[[index]] <- list(
      title = section_title,
      old_html = antragsgruen_section_html(old_section),
      new_html = antragsgruen_section_html(new_section),
      diff_html = antragsgruen_section_html(new_section)
    )
  }

  merged
}

antragsgruen_normalize_payload <- function(payload, source = "") {
  if (!is.list(payload)) {
    stop("Antragsgrün payload must be JSON object data.")
  }

  payload_type <- antragsgruen_value_or(payload$type, "")
  if (payload_type == "") {
    payload_type <- if (!is.null(payload$motion)) "amendment" else "motion"
  }

  if (payload_type == "amendment") {
    motion_payload <- NULL
    motion_url <- antragsgruen_value_or(payload$motion$url_json, "")
    if (nzchar(motion_url)) {
      motion_payload <- antragsgruen_read_payload(motion_url)
    }

    sections <- antragsgruen_merge_sections(
      if (!is.null(motion_payload)) motion_payload$sections else list(),
      payload$sections
    )

    return(list(
      source = source,
      type = payload_type,
      title = antragsgruen_value_or(payload$title_with_prefix, antragsgruen_value_or(payload$title, "")),
      prefix = antragsgruen_value_or(payload$prefix, ""),
      motion_title = if (!is.null(payload$motion)) antragsgruen_value_or(payload$motion$title_with_prefix, antragsgruen_value_or(payload$motion$title, "")) else "",
      section_title = if (length(sections) > 0) sections[[1]]$title else antragsgruen_value_or(payload$title, ""),
      sections = sections,
      raw = payload
    ))
  }

  sections_source <- payload$sections
  if (is.null(sections_source)) {
    sections_source <- list()
  }

  sections <- lapply(sections_source, function(section) {
    list(
      title = antragsgruen_section_title(section),
      old_html = antragsgruen_section_html(section),
      new_html = "",
      diff_html = antragsgruen_section_html(section)
    )
  })

  list(
    source = source,
    type = payload_type,
    title = antragsgruen_value_or(payload$title_with_prefix, antragsgruen_value_or(payload$title, "")),
    prefix = antragsgruen_value_or(payload$prefix, ""),
    motion_title = antragsgruen_value_or(payload$title_with_prefix, antragsgruen_value_or(payload$title, "")),
    section_title = if (length(sections) > 0) sections[[1]]$title else antragsgruen_value_or(payload$title, ""),
    sections = sections,
    raw = payload
  )
}

antragsgruen_import_case <- function(source) {
  source <- trimws(source)
  payload <- antragsgruen_read_payload(source)
  antragsgruen_normalize_payload(payload, source)
}

antragsgruen_render_sections <- function(import_case, mode = c("old", "new")) {
  mode <- match.arg(mode)

  if (is.null(import_case) || is.null(import_case$sections) || length(import_case$sections) == 0) {
    return(character(0))
  }

  pieces <- lapply(import_case$sections, function(section) {
    html <- if (mode == "old") section$old_html else section$new_html
    if (!antragsgruen_has_text(html)) {
      html <- if (mode == "old") section$new_html else section$old_html
    }
    if (!antragsgruen_has_text(html)) {
      html <- section$diff_html
    }

    paste0(
      "<div class='importedSection'>",
      "<div style='font-weight: bold; display: inline-block;'>",
      htmltools::htmlEscape(section$title),
      "</div>",
      if (antragsgruen_has_text(html)) {
        paste0("<div class='paragraph'><div class='text motionTextFormattings'>", html, "</div></div>")
      } else {
        "<div class='paragraph'><em>Kein Text aus der API verfügbar</em></div>"
      },
      "</div>"
    )
  })

  unlist(pieces, use.names = FALSE)
}

# Get list of amendments for a given motion ID from the REST API
antragsgruen_get_amendments <- function(
  motion_id,
  consultation_path = "simep-bayern",
  base_url = "https://simep-bayern.antragsgruen.de/rest"
) {
  # Fetch consultation data
  consultation_url <- paste0(base_url, "/", consultation_path)
  consultation_data <- antragsgruen_read_payload(consultation_url)
  
  # Find motion with matching ID
  motion <- NULL
  for (m in consultation_data$motion_links) {
    if (m$id == motion_id) {
      motion <- m
      break
    }
  }
  
  if (is.null(motion)) {
    stop(sprintf("Motion ID %d not found in consultation '%s'", motion_id, consultation_path))
  }
  
  # If amendments are already in motion_links, return them
  if (!is.null(motion$amendment_links) && length(motion$amendment_links) > 0) {
    amendments <- motion$amendment_links
    if (!is.list(amendments[[1]])) {
      # Single amendment case - wrap in list
      amendments <- list(amendments)
    }
    return(data.frame(
      id = sapply(amendments, \(a) a$id),
      prefix = sapply(amendments, \(a) a$prefix),
      initiators = sapply(amendments, \(a) a$initiators_html),
      url_json = sapply(amendments, \(a) a$url_json),
      stringsAsFactors = FALSE
    ))
  }
  
  # If no amendments in motion_links, return empty data frame
  data.frame(
    id = numeric(0),
    prefix = character(0),
    initiators = character(0),
    url_json = character(0),
    stringsAsFactors = FALSE
  )
}
