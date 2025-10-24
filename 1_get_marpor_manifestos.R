# ====================================================================== #
# Replication R script for: "Parties' ideological cores and peripheries" #
# ====================================================================== #

# Last updated: 24-10-2025



# ----------------------------------------------------------------------------------- #
# SET-UP                                                                              #
# ----------------------------------------------------------------------------------- #

# Load packages
library(tidyverse)
library(manifestoR)

# Set API token (obtain it here: https://manifesto-project.wzb.eu)
KEY = 'MY_KEY'



# ----------------------------------------------------------------------------------- #
# CORPUS OF WESTERN EUROPEAN COUNTRIES                                                #
# ----------------------------------------------------------------------------------- #

# Set relevant countries
relevant_countries <- c("Austria", "Belgium", "Cyprus", "Denmark", "Finland", "Germany", 
                        "Greece", "Iceland", "Ireland", "Luxembourg", "Netherlands", "Norway", 
                        "Portugal", "Spain", "Sweden", "Switzerland", "United Kingdom")

# Check which manifestos are available and annotated
available <- mp_availability(countryname %in% relevant_countries & edate > "1945-01-01", apikey = KEY) %>%
  filter(annotations == TRUE)

# Corpus version: "2024-1"
mp_which_corpus_version()

# Manually re-written version of formatids() function
formatids <- function(ids) {
  names(ids) <- tolower(names(ids))
  ids <- ids[, intersect(c("party", "date", "edate"), names(ids))]
  
  if ("date" %in% names(ids) & "edate" %in% names(ids)) {
    ids <- dplyr::mutate(ids, date = ifelse(is.na(date),
                                            as.numeric(format(edate, format = "%Y%m")),
                                            date))
  }
  
  n.before <- nrow(ids)
  ids <- ids[which(!is.na(ids$party) & !is.na(ids$date)), ]
  n.after <- nrow(ids)
  
  if (n.after < n.before) {
    warning(paste(n.before - n.after, "rows were omitted from querying the database,",
                  "because they are NULL or NA."))
  }
  
  return(ids)
}

# Manually re-written version of as.metaids() function
as.metaids <- function(ids, apikey = NULL, cache = TRUE, envir = parent.frame(), attach_meta = TRUE) {
  id_is_df <- tryCatch(is.data.frame(eval(ids, envir = envir)), error = function(e) FALSE)
  
  if (id_is_df) {
    ids <- eval(ids, envir = envir)
  } else {
    search_data <- mp_maindataset(apikey = apikey, cache = cache) %>%
      dplyr::bind_rows(mp_southamerica_dataset(apikey = apikey, cache = cache)) %>%
      attach_year()
    
    ids <- search_data[eval(ids, envir = search_data, enclos = envir), ]
  }
  
  if (attach_meta && !("ManifestoMetadata" %in% class(ids))) {
    ids <- mp_metadata(ids, apikey = apikey, cache = cache)
  }
  
  if ("is_primary_doc" %in% names(ids)) {
    ids <- subset(ids, is.na(is_primary_doc) | is_primary_doc)
  }
  
  return(ids)
}

# Custom function to extract URLs instead of opening them in browser tabs
get_urls <- function(ids, apikey = NULL, cache = TRUE, base_url = "https://manifesto-project.wzb.eu") {
  ids <- as.metaids(substitute(ids), apikey = apikey, cache = cache, envir = parent.frame())
  ids <- subset(ids, !is.na(url_original))
  
  if (nrow(ids) == 0) {
    warning("No URLs found for the provided criteria.")
    return(character(0)) # Return an empty character vector if no URLs are found
  }
  
  # Construct the full URLs by appending the url_original to the base URL
  full_urls <- paste0(base_url, ids$url_original)
  
  return(full_urls) # Return the full URLs
}

# Get a list of manifesto URLs to scrape
urls <- get_urls(countryname %in% relevant_countries & edate > "1945-01-01",
                 apikey = KEY)


# Get parties dataset to select relevant manifestos
marpor_parties <- mp_parties(version = "MPDS2024a", apikey = KEY) %>% 
  filter(countryname %in% relevant_countries) %>%
  select(-c(max_presvote, year_max_presvote)) %>%
  mutate(time_period = year_max - year_min) %>% 
  filter(time_period > 0) %>% # at least contested two elections
  filter(max_pervote >= 2) %>% # at least 2% vote share in any election
  filter(year_max >= 1945) # year of the most recent election at least 1945

available <- available %>% 
  filter(party %in% unique(marpor_parties$party))

# Get MARPOR corpus as tibble
marpor_corpus_quasi <- mp_corpus(available, as_tibble = TRUE, apikey = KEY); beepr::beep()

# Exclude quasi-sentences that lack annotation
marpor_corpus_quasi <- marpor_corpus_quasi %>% 
  filter(!is.na(cmp_code))

# Create an eyear variable
marpor_corpus_quasi <- marpor_corpus_quasi %>% 
  mutate(eyear = as.integer(substr(as.character(date), 1, 4)))

# Create a country column
marpor_corpus_quasi <- marpor_corpus_quasi %>% 
  mutate(country = str_sub(party, 1, 2)) %>%
  mutate(country = case_when(
    country == "42" ~ "Austria",
    country == "21" ~ "Belgium",
    country == "55" ~ "Cyprus",
    country == "13" ~ "Denmark",
    country == "14" ~ "Finland",
    country == "41" ~ "Germany",
    country == "34" ~ "Greece",
    country == "15" ~ "Iceland",
    country == "53" ~ "Ireland",
    country == "23" ~ "Luxembourg",
    country == "22" ~ "Netherlands",
    country == "12" ~ "Norway",
    country == "35" ~ "Portugal",
    country == "33" ~ "Spain",
    country == "11" ~ "Sweden",
    country == "43" ~ "Switzerland",
    country == "51" ~ "United Kingdom",
    TRUE ~ NA_character_
  ))

# Get overview
marpor_corpus_quasi %>%
  group_by(country, party, date) %>%
  summarize(n = n()) %>%
  print(n = Inf)

# Export corpus as .RDS file and then classify in Python
write_rds(marpor_corpus_quasi,  file = "marpor_corpus_quasi.RDS")