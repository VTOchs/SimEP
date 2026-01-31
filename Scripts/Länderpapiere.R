
library(data.table)
library(haven)
library(jsonlite)
library(readr)
library(rvest)
library(stringr)
library(tidyverse)

# Funktionen --------------------------------------------------------------

# manuelles Übersetzen der Ländernamen
translation_data <- data.frame(
  en = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Czech Republic", "Denmark",
         "Estonia", "European Union", "Finland", "France", "Germany", "Greece", "Hungary",
         "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
         "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden"),
  de = c("Österreich", "Belgien", "Bulgarien", "Kroatien", "Zypern", "Tschechien", "Tschechien", "Dänemark",
         "Estland", "Europäische Union", "Finnland", "Frankreich", "Deutschland", "Griechenland", "Ungarn",
         "Irland", "Italien", "Lettland", "Litauen", "Luxemburg", "Malta", "Niederlande",
         "Polen", "Portugal", "Rumänien", "Slowakei", "Slowenien", "Spanien", "Schweden")
)

translate_country <- function(country){
  translation_data[translation_data$en == country, "de"]
}

# manuelles Übersetzen von Yes/No
translation_data_sub <- data.frame(
  en = c("Yes", "No", "De jure"),
  de = c("Ja", "Nein", "Nein (de facto)")
)

translate_sub <- function(varx){
  translation_data_sub[translation_data_sub$en == varx, "de"]
}


set_point <- function(x){
  formatC(x, digits = nchar(as.integer(x)), big.mark=".", decimal.mark = ",")
}

euMember <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Czech Republic", "Denmark",
              "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
              "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
              "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

scrape_OWID <- function(indicator, df, indName="indicator", firstOnly = F){
  urlOWID <- paste0("https://ourworldindata.org/grapher/", indicator, "?tab=table&time=latest")
  
  links <- xml2::read_html(urlOWID) |>
    html_nodes("link")
  
  all_urls <- links[html_attr(links, "rel") == "preload"] |> 
    html_attr("href")
  
  json_urls <- grep("json$", all_urls, value = TRUE)
  dfOWID <- df
  if (firstOnly) {
    data <- fromJSON(json_urls[1])|> data.frame()
    meta <- fromJSON(json_urls[2])
    data <- data |> 
      left_join(meta$dimensions$entities$values, join_by(entities == id)) |> 
      slice_max(years) |>
      filter(name %in% euMember) 
    
    meanValue <- data$values |> mean()
    year <- data$years |> max()
    
    data <- data |>
      rbind(c(
        meanValue,
        year,
        0,
        "European Union",
        "EUU"
      ))
    
    col_name <- str_split(str_split(meta$name, pattern = " \\(%")[[1]][1], pattern = " ")[[1]][1]
    
    dfOWID <- data |> 
      select(values, code) |> 
      rename(!!col_name := values, iso3c := code) |> 
      right_join(dfOWID, join_by(iso3c == iso3c))
  } else {
    for (i in seq(1, length(json_urls), 2)) {
      data <- fromJSON(json_urls[i])|> data.frame()
      meta <- fromJSON(json_urls[i+1])
      
      data <- data |> 
        left_join(meta$dimensions$entities$values, join_by(entities == id)) |> 
        slice_max(years) |>
        filter(name %in% euMember) 
      
      meanValue <- data$values |> mean()
      year <- data$years |> max()
      
      data <- data |>
        rbind(c(
          meanValue,
          year,
          0,
          "European Union",
          "EUU"
        ))
      
      col_name <- str_split(str_split(meta$name, pattern = " \\(%")[[1]][1], pattern = " ")[[1]][1]
      
      dfOWID <- data |> 
        select(values, code) |> 
        rename(!!col_name := values, iso3c := code) |> 
        right_join(dfOWID, join_by(iso3c == iso3c))
    }
  }
  if (ncol(dfOWID) == 4) {
    names(dfOWID) <- c(indName, "iso3c", "iso2c", "country")
  }
  dfOWID
}


# OWID Crawlen -----------------------------------------------------

df_landerpapiere <- data.frame(
  iso3c = c(
    "AUT", "BEL", "BGR", "HRV",
    "CYP", "CZE", "DNK", "EST",
    "FIN", "FRA", "DEU", "GRC",
    "HUN", "IRL", "ITA", "LVA",
    "LTU", "LUX", "MLT", "NLD",
    "POL", "PRT", "ROU", "SVK",
    "SVN", "ESP", "SWE", "EUU"),
  iso2c = c(
    "AT", "BE", "BG", "HR",
    "CY", "CZ", "DK", "EE",
    "FI", "FR", "DE", "GR",
    "HU", "IE", "IT", "LV",
    "LT", "LU", "MT", "NL",
    "PL", "PT", "RO", "SK",
    "SI", "ES", "SE", "EU"),
  country = c(
    "Österreich", "Belgien", "Bulgarien", "Kroatien", "Zypern", "Tschechien", "Dänemark",
    "Estland", "Finnland", "Frankreich", "Deutschland", "Griechenland", "Ungarn",
    "Irland", "Italien", "Lettland", "Litauen", "Luxemburg", "Malta", "Niederlande",
    "Polen", "Portugal", "Rumänien", "Slowakei", "Slowenien", "Spanien", "Schweden", "Europäische Union"))

df_landerpapiere <- scrape_OWID("share-elec-by-source", df_landerpapiere) |> 
          left_join(scrape_OWID("per-capita-ghg-emissions", df_landerpapiere, indName="coCap")[, c("coCap", "iso3c")], by = "iso3c") |> 
          left_join(scrape_OWID("military-spending-as-a-share-of-gdp-sipri", df_landerpapiere, indName="milPerc", firstOnly = T)[, c("milPerc", "iso3c")], by = "iso3c") |> 
          left_join(scrape_OWID("gdp-per-capita-worldbank", df_landerpapiere, indName="gdpCap", firstOnly = T)[, c("gdpCap", "iso3c")], by = "iso3c") |> 
          left_join(scrape_OWID("gdp-worldbank", df_landerpapiere, indName="gdp")[, c("gdp", "iso3c")], by = "iso3c") |> 
          left_join(scrape_OWID("population", df_landerpapiere, indName="totPop")[, c("totPop", "iso3c")], by = "iso3c") |>
          left_join(scrape_OWID("refugee-population-by-country-or-territory-of-asylum", df_landerpapiere, indName="refPop")[, c("refPop", "iso3c")], by = "iso3c") |>
          left_join(scrape_OWID("agriculture-share-gdp", df_landerpapiere, indName="agriPerc")[, c("agriPerc", "iso3c")], by = "iso3c") |>
          left_join(scrape_OWID("population-young-working-elderly", df_landerpapiere)[, c("Population.x", "iso3c")], by = "iso3c")

df_landerpapiere <- df_landerpapiere |> 
                      rename(
                        workAgePop = Population.x
                      ) |> 
                      mutate(across(c(gdp,
                                      gdpCap,
                                      totPop,
                                      refPop,
                                      workAgePop,
                                      agriPerc,
                                      coCap,
                                      milPerc,
                                      Bioenergy,
                                      Other,
                                      Nuclear,
                                      Oil,
                                      Wind,
                                      Solar,
                                      Hydro,
                                      Gas,
                                      Coal),
                                    as.numeric)) |> 
                      mutate(refPerc = (refPop*100)/totPop,
                             workAgePerc = (workAgePop * 100)/totPop,
                             gdp = gdp / 1e9) |> 
                      mutate(across(gdpCap, \(x) round(x, -2))) |> 
                      mutate(across(milPerc, \(x) round(x, 2))) |>
                      mutate(across(gdp, \(x) round(x, 0))) |>
                      mutate(across(c(workAgePerc, refPerc, agriPerc, coCap), \(x) round(x, 1))) |> 
                      mutate(across(c(refPerc, agriPerc, coCap), ~gsub("\\.", ",", formatC(., digits = 2, decimal.mark = ","))))|>
                      mutate(across(c(workAgePerc, milPerc), ~gsub("\\.", ",", formatC(., digits = 3, decimal.mark = ",")))) |> 
                      mutate(fossilShare = round((Oil + Gas + Coal), 0),
                             nuclearShare = round(Nuclear, 0),
                             renewShare = round((Bioenergy + Other + Wind + Solar+ Hydro), 0))

df_landerpapiere$gdpCap <- df_landerpapiere$gdpCap |> 
                            sapply(function(x) set_point(x))
df_landerpapiere$gdp <- df_landerpapiere$gdp |> 
                            sapply(function(x) set_point(x))

# Manuelle Zuweisung EFTA-Außengrenze -------------------------------------

df_landerpapiere$border <- c("Nein", # AUT
                             "Nein", # BEL
                             "Ja", # BGR
                             "Ja", # HRV
                             "Ja", # CYP
                             "Nein", # CZE
                             "Nein", # DNK
                             "Ja", # EST
                             "Ja", # FIN
                             "Ja", # FRA
                             "Nein", # DEU
                             "Ja", # GRC
                             "Ja", # HUN
                             "Ja", # IRL
                             "Ja", # ITA
                             "Ja", # LVA
                             "Ja", # LTU
                             "Nein", # LUX
                             "Ja", # MLT
                             "Nein", # NLD
                             "Ja", # POL
                             "Ja", # PRT
                             "Ja", # ROU
                             "Ja", # SVK
                             "Nein", # SVN
                             "Ja", # ESP
                             "Nein", # SWE 
                             "-") # EUU


# Umfrage EU-Verteidigung -------------------------------------------------

# Download "ZA8764_v1-0-0.dta" from https://search.gesis.org/research_data/ZA8764
{df_eurob <- read_dta("Daten/ZA8764_v1-0-0.dta")
  df_eurob <- df_eurob |>
    select(isocntry, q6_2) |>
    # Russia’s invasion of Ukraine shows the EU needs to
    # increase military cooperation between Member States
    rename(country = isocntry,
           incr_coop = q6_2)
  num_resp <- df_eurob$country |> table() 
  
  df_coop <- df_eurob |> 
    group_by(country) |> 
    summarise(agree = sum(incr_coop %in% c(1, 2)),
              disagr = sum(incr_coop %in% c(3, 4))) |> 
    mutate(agree = round(agree * 100/num_resp, 0),
           disagr = round(disagr * 100/num_resp, 0)) |>
    as_tibble()
  df_coop <- rbind(df_coop, list("EU", round(mean(df_coop$agree), 0), round(mean(df_coop$disagr), 0)))}

# Wehrpflicht -------------------------------------------------------------

{webpage <- "https://worldpopulationreview.com/country-rankings/countries-with-mandatory-military-service" |> 
    read_html()
  df_subs <- webpage %>%
    html_element("table.wpr-table") %>%
    html_table()
  # Rename the columns to ensure all have valid names
  colnames(df_subs) <- c("empty_col", "Country", "subscription", "Details")
  # Select only the columns you need
  df_subs <- df_subs %>%
    select(Country, subscription) %>% 
    filter(Country %in% euMember)

  df_subs$subscription <- df_subs$subscription |> 
    sapply(function(x) translate_sub(x)) |> 
    unlist() |>
    unname()
  df_subs$Country <- df_subs$Country |> 
    sapply(function(x) translate_country(x)) |> 
    unlist() |>
    unname()
}

df_landerpapiere <- df_landerpapiere |> 
  left_join(df_coop, join_by(iso2c == country)) |> 
  left_join(df_subs, join_by(country == Country))

df_landerpapiere <- df_landerpapiere |> 
                      mutate(gdpEu = df_landerpapiere[df_landerpapiere$country == "Europäische Union", "gdp"],
                             gdpCapEu = df_landerpapiere[df_landerpapiere$country == "Europäische Union", "gdpCap"],
                             refPercEu = df_landerpapiere[df_landerpapiere$country == "Europäische Union", "refPerc"],
                             workAgePercEu = df_landerpapiere[df_landerpapiere$country == "Europäische Union", "workAgePerc"],
                             agriPercEu = df_landerpapiere[df_landerpapiere$country == "Europäische Union", "agriPerc"],
                             coCapEu = df_landerpapiere[df_landerpapiere$country == "Europäische Union", "coCap"],
                             fossilShareEu = df_landerpapiere[df_landerpapiere$country == "Europäische Union", "fossilShare"],
                             nuclearShareEu = df_landerpapiere[df_landerpapiere$country == "Europäische Union", "nuclearShare"],
                             renewShareEu = df_landerpapiere[df_landerpapiere$country == "Europäische Union", "renewShare"],
                             milPercEU = df_landerpapiere[df_landerpapiere$country == "Europäische Union", "milPerc"],
                             agreeEU = df_landerpapiere[df_landerpapiere$country == "Europäische Union", "agree"],
                             disagrEU = df_landerpapiere[df_landerpapiere$country == "Europäische Union", "disagr"]) |> 
                      select(iso3c, iso2c, country, gdp, gdpCap, refPerc, agriPerc, coCap, 
                             fossilShare, nuclearShare, renewShare, workAgePerc, 
                             milPerc, agree, disagr, border, subscription,
                             gdpEu, gdpCapEu, refPercEu, workAgePercEu, agriPercEu,
                             coCapEu, fossilShareEu, nuclearShareEu, renewShareEu,
                             milPercEU, agreeEU, disagrEU) |> 
                      rename(iso = iso3c) |> 
                      filter(country != "Europäische Union")

write.csv(df_landerpapiere, "Daten/data_landerpapiere.csv")
