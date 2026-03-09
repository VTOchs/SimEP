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
  de = c("EVP", "SD", "RE", "Green", "PfE", "EKR", "Die Linke", "ESN", "Green", "EVP", "Green")
)

get_slido_link <- function(city, group){
  slido_link <- read_excel("Daten/data_slido.xlsx") |> 
    filter(Stadt == city) |> 
    pull(group)
}

translate_latex <- function(group){
  translation_data_latex[translation_data_latex$en == group, "de"]
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


get_sus_dist <- function(numSuS, landDist = T){
  if (landDist) {
    listTC <- readRDS("Daten/country_party.rds")
    attributes(listTC)$names <- sapply(attributes(listTC)$names, translate_latex) |> unlist()
    dfCG <- get_sus_dist(numSuS = numSuS, landDist = F) 
    listDist <- list()
    for (group in groupsEP) {
      numDep <- dfCG |> filter(Fraktion == group) |> select(SuS)
      listDist[[group]] <- sample(listTC[[group]], numDep$SuS, replace = T)
    }
    listDist
  } else {
    df_caucus <- read.csv("Daten/caucus_data.csv")
    df_caucus$party <- df_caucus$party |> sapply(translate_latex)
    
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