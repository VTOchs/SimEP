#!/usr/bin/env Rscript

source(file.path("Scripts", "antragsgruen_api.R"))

latex_escape <- function(text) {
  text <- as.character(text)
  text <- gsub("\\\\", "\\textbackslash{}", text, fixed = TRUE)
  text <- gsub("&", "\\&", text, fixed = TRUE)
  text <- gsub("%", "\\%", text, fixed = TRUE)
  text <- gsub("$", "\\$", text, fixed = TRUE)
  text <- gsub("#", "\\#", text, fixed = TRUE)
  text <- gsub("_", "\\_", text, fixed = TRUE)
  text <- gsub("{", "\\{", text, fixed = TRUE)
  text <- gsub("}", "\\}", text, fixed = TRUE)
  text <- gsub("~", "\\textasciitilde{}", text, fixed = TRUE)
  text <- gsub("^", "\\textasciicircum{}", text, fixed = TRUE)
  text
}

html_entity_unescape <- function(text) {
  text <- gsub("&amp;", "&", text, fixed = TRUE)
  text <- gsub("&lt;", "<", text, fixed = TRUE)
  text <- gsub("&gt;", ">", text, fixed = TRUE)
  text <- gsub("&quot;", '"', text, fixed = TRUE)
  text <- gsub("&#39;", "'", text, fixed = TRUE)
  text
}

normalize_umlauts <- function(text) {
  consonants <- "bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ"
  prefix <- "(^|[^A-Za-zÄÖÜäöü])"
  suffix <- paste0("(?=[", consonants, "]|$)")
  infix <- paste0("(?<=[", consonants, "])")

  text <- gsub(paste0(prefix, "Ae", suffix), "\\1Ä", text, perl = TRUE)
  text <- gsub(paste0(prefix, "Oe", suffix), "\\1Ö", text, perl = TRUE)
  text <- gsub(paste0(prefix, "Ue", suffix), "\\1Ü", text, perl = TRUE)
  text <- gsub(paste0(prefix, "ae", suffix), "\\1ä", text, perl = TRUE)
  text <- gsub(paste0(prefix, "oe", suffix), "\\1ö", text, perl = TRUE)
  text <- gsub(paste0(prefix, "ue", suffix), "\\1ü", text, perl = TRUE)
  text <- gsub(paste0(infix, "ae", suffix), "ä", text, perl = TRUE)
  text <- gsub(paste0(infix, "oe", suffix), "ö", text, perl = TRUE)
  text <- gsub(paste0(infix, "ue", suffix), "ü", text, perl = TRUE)
  text
}

strip_html <- function(html) {
  if (!antragsgruen_has_text(html)) {
    return("")
  }
  text <- gsub("<[^>]+>", " ", html)
  text <- html_entity_unescape(text)
  text <- normalize_umlauts(text)
  text <- gsub("[[:space:]]+", " ", text)
  trimws(text)
}

html_diff_to_tex <- function(html) {
  if (!antragsgruen_has_text(html)) {
    return("-")
  }

  text <- html
  text <- gsub("<h4 class=\"lineSummary\">.*?</h4>", " ", text, perl = TRUE)
  text <- gsub("<p[^>]*class=\"deleted\"[^>]*>(.*?)</p>", "[[DEL_OPEN]]\\1[[DEL_CLOSE]]", text, perl = TRUE)
  text <- gsub("<p[^>]*class=\"inserted\"[^>]*>(.*?)</p>", "[[INS_OPEN]]\\1[[INS_CLOSE]]", text, perl = TRUE)
  text <- gsub("<span[^>]*class=\"deleted\"[^>]*>", "[[DEL_OPEN]]", text, perl = TRUE)
  text <- gsub("<span[^>]*class=\"inserted\"[^>]*>", "[[INS_OPEN]]", text, perl = TRUE)
  text <- gsub("<del[^>]*>", "[[DEL_OPEN]]", text, perl = TRUE)
  text <- gsub("</del>", "[[DEL_CLOSE]]", text, perl = TRUE)
  text <- gsub("<ins[^>]*>", "[[INS_OPEN]]", text, perl = TRUE)
  text <- gsub("</ins>", "[[INS_CLOSE]]", text, perl = TRUE)
  text <- gsub("<br\\s*/?>", " ", text, perl = TRUE)
  text <- gsub("</?(p|div)>", " ", text, perl = TRUE)
  text <- gsub("<[^>]+>", " ", text, perl = TRUE)
  text <- html_entity_unescape(text)

  parts <- unlist(strsplit(text, "(\\[\\[DEL_OPEN\\]\\]|\\[\\[DEL_CLOSE\\]\\]|\\[\\[INS_OPEN\\]\\]|\\[\\[INS_CLOSE\\]\\])", perl = TRUE))
  tags <- unlist(regmatches(text, gregexpr("(\\[\\[DEL_OPEN\\]\\]|\\[\\[DEL_CLOSE\\]\\]|\\[\\[INS_OPEN\\]\\]|\\[\\[INS_CLOSE\\]\\])", text, perl = TRUE)))

  if (length(parts) == 1 && length(tags) == 0) {
    return(trimws(latex_escape(parts)))
  }

  out <- character(0)
  for (i in seq_along(parts)) {
    piece <- parts[[i]]
    if (nzchar(piece)) {
      out <- c(out, latex_escape(normalize_umlauts(piece)))
    }
    if (i <= length(tags)) {
      tag <- tags[[i]]
      if (tag == "[[DEL_OPEN]]") {
        out <- c(out, "\\textcolor{red}{\\sout{")
      } else if (tag == "[[DEL_CLOSE]]") {
        out <- c(out, "}}")
      } else if (tag == "[[INS_OPEN]]") {
        out <- c(out, "\\textcolor{green}{\\uline{")
      } else if (tag == "[[INS_CLOSE]]") {
        out <- c(out, "}}")
      }
    }
  }

  text <- paste0(out, collapse = "")
  text <- gsub("[[:space:]]+", " ", text)
  trimws(text)
}

read_shinyin_value <- function(key, default = "", path = file.path("LaTeX", "Meta", "shinyin.tex")) {
  if (!file.exists(path)) return(default)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  pattern <- sprintf("\\\\newcommand\\\\%s\\{[^}]+\\}", key)
  match <- regmatches(lines, regexpr(pattern, lines))
  match <- match[nzchar(match)]
  if (length(match) == 0) return(default)
  value <- sub(".*\\{([^}]*)\\}.*", "\\1", match[1])
  if (!nzchar(value)) default else value
}

read_fifth_group <- function(path = file.path("LaTeX", "Meta", "shinyin.tex")) {
  read_shinyin_value("fifthGroup", "Left", path)
}

read_shinyin_city <- function(path = file.path("LaTeX", "Meta", "shinyin.tex")) {
  read_shinyin_value("city", "", path)
}

parse_args <- function(args) {
  named <- list()
  positional <- character(0)
  for (arg in args) {
    if (grepl("^--[^=]+=", arg)) {
      key <- sub("^--([^=]+)=.*$", "\\1", arg)
      value <- sub("^--[^=]+=", "", arg)
      named[[key]] <- value
    } else {
      positional <- c(positional, arg)
    }
  }
  list(named = named, positional = positional)
}

sanitize <- function(x) {
  x2 <- gsub("\\s+", "_", x, perl = TRUE)
  gsub("[^A-Za-z0-9_-]", "", x2, perl = TRUE)
}

find_party_amendment <- function(amendments, party_pattern, committee = "") {
  if (length(amendments) == 0) {
    return(NULL)
  }

  candidates <- vapply(amendments, function(amendment) {
    paste(
      strip_html(amendment$initiators_html),
      strip_html(amendment$prefix),
      strip_html(amendment$title),
      sep = " | "
    )
  }, character(1))

  party_hits <- grepl(party_pattern, candidates, ignore.case = TRUE)
  if (nzchar(committee)) {
    committee_hits <- grepl(committee, candidates, ignore.case = TRUE)
    party_hits <- party_hits & committee_hits
  }

  index <- which(party_hits)
  if (length(index) == 0) {
    return(NULL)
  }

  amendments[[index[[1]]]]
}

resolve_party_rows <- function(amendments, committee, fifth_group) {
  row_specs <- list(
    list(label = "EVP", logo = "EVP.png", pattern = "EVP"),
    list(label = "S&D", logo = "SD.png", pattern = "S\\s*&\\s*D|S&D|S\\+D|SD"),
    list(label = "Renew", logo = "RE.png", pattern = "Renew|RE"),
    list(label = "PfE", logo = "PfE.png", pattern = "PfE"),
    list(
      label = if (identical(fifth_group, "Grüne")) "Grüne" else "Linke",
      logo = if (identical(fifth_group, "Grüne")) "Green.png" else "Left.png",
      pattern = if (identical(fifth_group, "Grüne")) "Grüne|Green" else "Linke|Left"
    )
  )

  rows <- vector("list", length(row_specs))
  for (i in seq_along(row_specs)) {
    spec <- row_specs[[i]]
    amendment <- find_party_amendment(amendments, spec$pattern, committee)
    if (is.null(amendment)) {
      rows[[i]] <- list(label = spec$label, logo = spec$logo, diff = "-")
      next
    }

    section_html <- if (length(amendment$sections) > 0) amendment$sections[[1]]$html else ""
    rows[[i]] <- list(
      label = spec$label,
      logo = spec$logo,
      diff = html_diff_to_tex(section_html)
    )
  }

  rows
}

build_tex <- function(committee, motion_id, amendments, fifth_group) {
  rows <- resolve_party_rows(amendments$items, committee, fifth_group)
  row_lines <- vapply(
    rows,
    function(row) {
      paste0(
        "\\includegraphics[width=2cm,height=1cm,keepaspectratio]{Bilder/", row$logo, "} & ",
        row$diff,
        " \\\\"
      )
    },
    character(1)
  )

  paste0(
    "\\begin{frame}{Änderungsanträge (", latex_escape(committee), ")}\n",
    "\\tiny\n",
    "\\vspace{-0.45cm}\n",
    "\\renewcommand{\\arraystretch}{0.9}\n",
    "\\setlength{\\tabcolsep}{2pt}\n",
    "\\setlength{\\abovetopsep}{0pt}\n",
    "\\setlength{\\belowrulesep}{0pt}\n",
    "\\begin{tabularx}{\\textwidth}{@{}>{\\centering\\arraybackslash}m{1.55cm} >{\\RaggedRight\\arraybackslash}m{\\dimexpr\\linewidth-1.55cm-2\\tabcolsep\\relax}@{}}\n",
    "\\toprule\n",
    "\\multicolumn{1}{m{1.55cm}}{\\centering\\textbf{Fraktion}} & \\multicolumn{1}{m{\\dimexpr\\linewidth-1.55cm-2\\tabcolsep\\relax}}{\\centering\\textbf{Änderungsantrag}} \\\\\n",
    "\\midrule\n",
    paste0(row_lines, collapse = "\n"), "\n",
    "\\bottomrule\n",
    "\\end{tabularx}\n",
    "\\end{frame}\n"
  )
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  committee <- if (!is.null(args$named$committee)) args$named$committee else if (length(args$positional) >= 1) args$positional[[1]] else "SEDE"
  motion_id <- if (!is.null(args$named$motion)) args$named$motion else if (length(args$positional) >= 2) args$positional[[2]] else "56456"
  fifth_group <- NULL
  if (!is.null(args$named$`fifth-group`)) {
    fifth_group <- args$named$`fifth-group`
  } else if (nzchar(Sys.getenv("SIMEP_FIFTH_GROUP", ""))) {
    fifth_group <- Sys.getenv("SIMEP_FIFTH_GROUP")
  } else {
    fifth_group <- read_fifth_group()
  }

  # Allow an explicit override to force the fifth group to Linke (Left)
  if (!is.null(args$named$`force-left`) && args$named$`force-left` %in% c("1", "true", "TRUE", "True")) {
    fifth_group <- "Linke"
  }

  if (!nzchar(committee)) {
    stop("Missing committee argument.")
  }
  if (!nzchar(motion_id)) {
    stop("Missing motion id argument.")
  }

  base <- antragsgruen_read_payload("https://simep-bayern.antragsgruen.de/rest/simep-bayern")
  motion <- NULL
  for (m in base$motion_links) {
    if (as.character(m$id) == as.character(motion_id)) {
      motion <- m
      break
    }
  }
  if (is.null(motion)) {
    stop(sprintf("Motion %s was not found in the consultation feed.", motion_id))
  }

  items <- list()
  for (amendment_link in motion$amendment_links) {
    amendment <- antragsgruen_read_payload(amendment_link$url_json)
    items[[length(items) + 1]] <- amendment
  }

  out_dir <- file.path("LaTeX", "Meta")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  out_file <- file.path(out_dir, sprintf("antragsgruen_slide_%s.tex", committee))
  tex <- build_tex(committee, motion_id, list(items = items), fifth_group)
  writeLines(tex, out_file, useBytes = TRUE)
  message(sprintf("Wrote %s", out_file))
}

main()