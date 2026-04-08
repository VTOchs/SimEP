
library("data.table")
library(httr)
library(janitor)
library(lubridate)
library(polite)
library(readr)
library(rvest)
library(stringr)
library(tidyverse)
library(xml2)


pres_of_EP <- "Roberta Metsola"

# manuelles Übersetzen der Ländernamen
# zweimal Tschechien/Tschechische Rep.

translation_data_country <- data.frame(
  en = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Czechia", "Denmark", "Estonia",
         "European Union", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia",
          "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden"),
  de = c("Österreich", "Belgien", "Bulgarien", "Kroatien", "Zypern", "Tschechien", "Tschechien", "Dänemark", "Estland",
         "Europäische Union", "Finnland", "Frankreich", "Deutschland", "Griechenland", "Ungarn", "Irland", "Italien", "Lettland",
          "Litauen", "Luxemburg", "Malta", "Niederlande", "Polen", "Portugal", "Rumänien", "Slowakei", "Slowenien", "Spanien", "Schweden"),
  iso = c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "CZE", "DNK", "EST", "EUU", "FIN", "FRA", "DEU", "GRC",
          "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")
)

translate_country <- function(country, source = "en", target = "de"){
  translation_data_country[translation_data_country[,source] == country, target]
}


translation_data_group <- data.frame(
  en = c("EPP", "S&D", "Renew", "G / EFA", "PfE", "ECR", "The Left", "ESN", "Verts/ALE", "PPE", "Greens/EFA"),
  de = c("EVP", "S&D", "Renew", "Grüne", "PfE", "EKR", "Die Linke", "ESN", "Grüne", "EVP", "Grüne")
)

translate_group <- function(group){
  translation_data_group[translation_data_group$en == group, "de"]
}



translation_data_rank <- data.frame(
  num = 1:8,
  word = c("größte", "zweitgrößte", "drittgrößte", "viertgrößte", "fünftgrößte",
           "sechstgrößte", "siebtgrößte", "achtgrößte")
)

translate_rank <- function(rank){
  translation_data_rank[translation_data_rank$num == rank, "word"]
}

capitalize_first_letter <- function(text) {
  # Split the text into words

  words <- strsplit(text, "[ -]")[[1]]
  
  # Ausnahme für McAllister
  if (words[2] == "MCALLISTER") {
    return("David McAllister")
  }else{
    # Capitalize the first letter of each word and make the rest lowercase
    words <- sapply(words, function(word) {
      if (nchar(word) > 0) {
        paste0(toupper(substr(word, 1, 1)), tolower(substr(word, 2, nchar(word))))
      } else {
        word
      }
    })
    
    # Combine the words back into a single string
    result <- paste(words, collapse = " ")
    return(result)
  }
}

# Seats -------------------------------------------------------------------
# Rough flow:
# 1) Scrape EP seats table.
# 2) Build valid country-party combinations and save as RDS.
# 3) Compute caucus totals and rank basis.


url_seats <- "https://www.europarl.europa.eu/meps/de/search/table"

df_all <- url_seats |> 
          read_html(as.data.frame = T, stringsAsFactors = TRUE) |> 
          html_nodes("table") %>%
          .[[1]] %>%
          html_table()

# get the parties which are present in each member state
# to ensure only proper country-party combinations
party_per_country <- !df_all |> select(-c(Land, NI, insgesamt)) |> is.na() |> as.data.frame()
party_per_country <- party_per_country[-nrow(party_per_country),] # drop EU

dfPC <- df_all[,"Land"]
dfPC <- dfPC[-nrow(dfPC),] # drop EU
dfPC <- dfPC |> as.data.frame()
dfPC[,colnames(party_per_country)] <- party_per_country

# get ISO codes of countries
isoVec <- sapply(dfPC$Land, function (x) translate_country(x, source = "de", target = "iso")) |>
            unlist() |> 
            unique()

# transpose so parties are row indices
dfPCT <- data.frame(t(dfPC))
colnames(dfPCT) <- isoVec
dfPCT <- dfPCT[-1, ]


listTC <- list()
for (party in colnames(party_per_country)) {
  landVec <- c()
  for (country in colnames(dfPCT)) {
    ifCheck <- dfPCT[rownames(dfPCT) == party, country] |> as.logical()
    if (ifCheck) {
      landVec[length(landVec)+1] <- country
    } 
  }
  listTC[[party]] <- landVec
}

saveRDS(listTC, "Daten/country_party.rds")


# get Rank of each Caucus
caucus_seats <- df_all |>
                  filter(Land != "EU") |>
                  select(-c(Land, insgesamt, NI)) |>
                  colSums(na.rm = T) 
caucus_ranks <- rank(-caucus_seats)


df_seats <-  df_all |>
              select(EPP, 'S&D', Renew, 'Greens/EFA', PfE, 'The Left')
              # select(EVP, 'S&D', Renew, 'Grüne/EFA', PfE)


# Caucus ------------------------------------------------------------------
# Rough flow:
# 1) Load EP leadership XML and normalize political-group labels.
# 2) Build one presidents-string per caucus.
# 3) Merge with seats and rank text, then write caucus_data.csv.

map_political_group <- function(group) {
  case_when(
    group == "Group of the European People's Party (Christian Democrats)" ~ "EPP",
    group == "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament" ~ "S&D",
    group == "Patriots for Europe Group" ~ "PfE",
    group == "European Conservatives and Reformists Group" ~ "ECR",
    group == "Renew Europe Group" ~ "Renew",
    group == "Group of the Greens/European Free Alliance" ~ "Greens/EFA",
    group == "The Left group in the European Parliament - GUE/NGL" ~ "The Left",
    group == "Europe of Sovereign Nations Group" ~ "ESN",
    TRUE ~ NA_character_
  )
}

fetch_group_presidents <- function() {
  url_pres <- "https://www.europarl.europa.eu/meps/en/download/advanced/xml?name=&euPoliticalGroupBodyRefNum=&countryCode=&bodyType=OTH&bodyReferenceNum=6631"
  xml_data <- read_xml(url_pres)
  meps <- xml_find_all(xml_data, ".//mep")

  full_names <- c()
  countries <- c()
  political_groups <- c()

  for (mep in meps) {
    full_names <- c(full_names, xml_text(xml_find_first(mep, ".//fullName")))
    countries <- c(countries, xml_text(xml_find_first(mep, ".//country")))
    political_groups <- c(political_groups, xml_text(xml_find_first(mep, ".//politicalGroup")))
  }

  data.frame(
    fullName = lapply(full_names, capitalize_first_letter) |> unlist(),
    country = countries,
    politicalGroup = political_groups,
    stringsAsFactors = FALSE
  ) |>
    filter(fullName != pres_of_EP) |>
    mutate(
      politicalGroup = map_political_group(politicalGroup),
      country = lapply(country, translate_country) |> unlist()
    )
}

build_caucus_presidents <- function(df_pres, caucus_names) {
  out <- data.frame()

  for (caucus in caucus_names) {
    name_caucus <- df_pres |>
      filter(politicalGroup == caucus) |>
      pull(fullName) |>
      paste(collapse = "/")

    country_caucus <- df_pres |>
      filter(politicalGroup == caucus) |>
      pull(country) |>
      paste(collapse = "/")

    out <- rbind(out, c(caucus, paste0(name_caucus, " (", country_caucus, ")")))
  }

  names(out) <- c("party", "presidents")
  out
}


df_pres <- fetch_group_presidents()

df_caucus <- data.frame(total = c(df_seats[nrow(df_seats),]) |> unlist(),
                        countries = 27 - df_seats |> sapply(function(x) sum(is.na(x))))

df_pres_paste <- build_caucus_presidents(df_pres, rownames(df_caucus))

df_caucus <- merge(df_pres_paste, df_caucus, by.x = "party", by.y = "row.names")


# translate rank into words
df_ranks <- data.frame(
  party = names(caucus_ranks),
  rank = translate_rank(caucus_ranks)
)

df_caucus <- merge(df_caucus, df_ranks, by = "party")

write_csv(df_caucus, "Daten/caucus_data.csv", quote = "none")



# Committees --------------------------------------------------------------
# Rough flow:
# 1) Fetch committee slugs from EP list-of-committees page.
# 2) Parse members page per committee.
# 3) Keep only Chair for president field and count non-substitutes.
# 4) Write committee_data.csv.


safe_translate <- function(value, translator) {
  clean_value <- str_trim(value)
  translated <- translator(clean_value)

  if (length(translated) == 0 || is.na(translated) || translated == "") {
    clean_value
  } else {
    translated
  }
}

get_committee_slugs <- function() {
  committee_url <- "https://www.europarl.europa.eu/committees/en/about/list-of-committees"

  com_list <- committee_url |>
    read_html() |>
    html_elements("a[href*='/committees/en/']") |>
    html_attr("href") |>
    str_extract("(?<=/committees/en/)[a-z0-9-]+(?=$|/)") |>
    discard(is.na) |>
    keep(~ str_detect(.x, "^[a-z]{4}$")) |>
    unique() |>
    sort()

  if (length(com_list) == 0) {
    stop("No EP committee slugs found on https://www.europarl.europa.eu/committees/en/about/list-of-committees")
  }

  com_list
}

parse_member_card <- function(card) {
  infos <- card |>
    html_elements(".sln-additional-info") |>
    html_text2() |>
    str_trim()

  data.frame(
    name = card |> html_element(".es_title-h4") |> html_text2() |> str_trim(),
    status = ifelse(length(infos) >= 1, infos[1], NA),
    party = ifelse(length(infos) >= 2, infos[2], NA),
    country = ifelse(length(infos) >= 3, infos[3], NA)
  )
}

build_chair_label <- function(df_com) {
  chair <- df_com |>
    filter(str_to_lower(str_trim(status)) == "chair")

  if (nrow(chair) == 0) {
    return("N/A")
  }

  chair_party <- sapply(chair$party, safe_translate, translator = translate_group) |>
    unlist()
  chair_country <- sapply(chair$country, safe_translate, translator = translate_country) |>
    unlist()

  paste0(chair$name, " (", chair_party, "/", chair_country, ")") |>
    paste(collapse = "/")
}

fetch_committee_row <- function(committee) {
  tryCatch({
    xml_com <- paste0("https://www.europarl.europa.eu/committees/en/", committee, "/home/members") |>
      read_html() |>
      as_xml_document()

    df_com <- xml_com |>
      html_elements(".es_member-list-item") |>
      purrr::map_dfr(parse_member_card)

    num_com <- df_com |>
      filter(status != "Substitute") |>
      nrow()

    data.frame(
      name = toupper(committee),
      pres = build_chair_label(df_com),
      meps = num_com
    )
  }, error = function(e) {
    NULL
  })
}

com_list <- get_committee_slugs()

df_com_paste <- com_list |>
  purrr::map(fetch_committee_row) |>
  purrr::compact() |>
  bind_rows()

df_com_paste$meps <- as.integer(df_com_paste$meps)

write_csv(df_com_paste, "Daten/committee_data.csv", quote = "none")
