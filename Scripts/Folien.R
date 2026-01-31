
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
              select(EPP, 'S&D', Renew, 'Greens/EFA', PfE)
              # select(EVP, 'S&D', Renew, 'Grüne/EFA', PfE)


{url_pres <- "https://www.europarl.europa.eu/meps/en/download/advanced/xml?name=&euPoliticalGroupBodyRefNum=&countryCode=&bodyType=OTH&bodyReferenceNum=6631"
xml_data <- read_xml(url_pres)
meps <- xml_find_all(xml_data, ".//mep")
full_names <- c()
countries <- c()
political_groups <- c()
# Loop through each MEP node and extract the desired information
for (mep in meps) {
  full_names <- c(full_names, xml_text(xml_find_first(mep, ".//fullName")))
  countries <- c(countries, xml_text(xml_find_first(mep, ".//country")))
  political_groups <- c(political_groups, xml_text(xml_find_first(mep, ".//politicalGroup")))
}
# Create a data frame to store the extracted information
df_pres <- data.frame(
  fullName = lapply(full_names, capitalize_first_letter) |> unlist(),
  country = countries,
  politicalGroup = political_groups,
  stringsAsFactors = FALSE
)

df_pres <- df_pres |> 
              filter(fullName != pres_of_EP) |>
              mutate(politicalGroup = case_when(politicalGroup == "Group of the European People's Party (Christian Democrats)" ~ "EPP",
                                                 politicalGroup == "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament" ~ "S&D",
                                                 politicalGroup == "Patriots for Europe Group" ~ "PfE",
                                                 politicalGroup == "European Conservatives and Reformists Group" ~ "ECR",
                                                 politicalGroup == "Renew Europe Group" ~ "Renew",
                                                 politicalGroup == "Group of the Greens/European Free Alliance" ~ "Greens/EFA",
                                                 politicalGroup == "The Left group in the European Parliament - GUE/NGL" ~ "The Left",
                                                 politicalGroup == "Europe of Sovereign Nations Group" ~ "ESN"))

df_pres$country <- lapply(df_pres$country, translate_country) |> unlist()
}

df_caucus <- data.frame(total = c(df_seats[nrow(df_seats),]) |> unlist(),
                        countries = 27 - df_seats |> sapply(function(x) sum(is.na(x))))

df_pres_paste <- data.frame()
for (caucus in df_caucus |> rownames()) {
  name_caucus <- df_pres |> 
    filter(politicalGroup == caucus) |> 
    pull(fullName) |>
    paste(collapse = "/")
  
  country_caucus <- df_pres |> 
    filter(politicalGroup == caucus) |> 
    pull(country) |>
    paste(collapse = "/")
  
  df_pres_paste <- rbind(df_pres_paste, c(caucus, paste0(name_caucus, " (",  country_caucus, ")")))
  
}
names(df_pres_paste) <- c("party", "presidents")

df_caucus <- merge(df_pres_paste, df_caucus, by.x = "party", by.y = "row.names")


# translate rank into words
df_ranks <- data.frame(
  party = names(caucus_ranks),
  rank = translate_rank(caucus_ranks)
)

df_caucus <- merge(df_caucus, df_ranks, by = "party")

write_csv(df_caucus, "Daten/caucus_data.csv", quote = "none")



# Committees --------------------------------------------------------------

df_com_paste <- data.frame()
com_list <- c("afet", "agri", "budg", "droi", "empl", "itre", "libe", "sede", "tran")

for (committee in com_list) {
  
  xml_com <- paste0("https://www.europarl.europa.eu/committees/en/", committee, "/home/members") |> 
                read_html() |>
                as_xml_document()
  
  list_name <- xml_find_all(xml_com, "//*[@id='docMembersList']//*[@class='erpl_title-h4 t-item']") |> 
    xml_text() |> 
    lapply(capitalize_first_letter) |> 
    unlist()
  
  list_misc <- xml_find_all(xml_com, "//*[@id='docMembersList']//*[@class='sln-additional-info']") |> 
    xml_text() |> 
    lapply(function(x) x[[1]]) |> unlist()
  
  df_com <- data.frame(name = list_name,
                        status = list_misc[seq(1, length(list_misc)-2, 3)],
                        party = list_misc[seq(2, length(list_misc)-1, 3)],
                        country = list_misc[seq(3, length(list_misc), 3)])
  num_com <- df_com |> 
              filter(status != "Substitute") |> 
              nrow()
  
  chair <- df_com |> 
            filter(status == "Chair")
  
  df_com_paste <- rbind(df_com_paste, c(toupper(committee), paste0(chair$name, " (", translate_group(chair$party), "/", translate_country(chair$country), ")"), num_com))
}
names(df_com_paste) <- c("name", "pres", "meps")

write_csv(df_com_paste, "Daten/committee_data.csv", quote = "none")