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
library(ggeffects)
library(fixest)

# Set plot theme
ggthemr::ggthemr('fresh')



# ----------------------------------------------------------------------------------- #
# DATA IMPORT                                                                         #
# ----------------------------------------------------------------------------------- #

# Read quasi-sentence-level classifications
marpor_quasi_classified_a <- read_rds("marpor_quasi_classified_a.RDS")
marpor_quasi_classified_b <- read_rds("marpor_quasi_classified_b.RDS") %>% select(-ID)
marpor_quasi_classified_c <- read_rds("marpor_quasi_classified_c.RDS") %>% select(-ID)

# Merge individual datasets
marpor_quasi_classified <- dplyr::bind_cols(
  marpor_quasi_classified_a, marpor_quasi_classified_b, marpor_quasi_classified_c,
  .name_repair = "unique"
)

rm(marpor_quasi_classified_a, marpor_quasi_classified_b, marpor_quasi_classified_c)



# ----------------------------------------------------------------------------------- #
# DATA PREPARATION                                                                    #
# ----------------------------------------------------------------------------------- #

# Transform into a tibble
marpor_quasi_classified <- marpor_quasi_classified %>%
  as_tibble()

# Transform the subcategories introduced in MARPOR's V5 handbook to V4 codes
marpor_quasi_classified <- marpor_quasi_classified %>% 
  mutate(cmp_code = str_remove(cmp_code, "\\.\\d"))

# Remove quasi-sentences coded as one of the following: "000", "6014", "H"
marpor_quasi_classified <- marpor_quasi_classified %>% 
  filter(!cmp_code %in% c("000", "6014", "H"))

# Check how many sentences are longer than the character limit for truncation in Python
marpor_quasi_classified %>%
  filter(nchar(text) > 200)

# Bar chart of remaining codes
marpor_quasi_classified %>% 
  group_by(cmp_code) %>%
  summarize(n = n()) %>%
  ggplot(aes(y = reorder(cmp_code, n), x = n)) + geom_col() + labs(y = NULL, x = "Frequency")

# Range of top category scores
summary(marpor_quasi_classified$top1_score)



# Check fit with MARPOR categories and only keep where there is overlap

# Classified categories from labels to codes
marpor_quasi_classified <- marpor_quasi_classified %>%
  mutate(across(ends_with("_label"),
                ~ str_sub(.x, 1, 3),
                .names = "{str_remove(.col, '_label')}_cat"))

# Check if the classified labels contain the human coded category
marpor_quasi_classified <- marpor_quasi_classified %>%
  rowwise() %>%
  mutate(
    is_contained_top1 = cmp_code == top1_cat,
    is_contained_top2 = cmp_code %in% c(top1_cat, top2_cat),
    is_contained_top3 = cmp_code %in% c(top1_cat, top2_cat, top3_cat),
    is_contained_top4 = cmp_code %in% c(top1_cat, top2_cat, top3_cat, top4_cat),
    is_contained_top5 = cmp_code %in% c(top1_cat, top2_cat, top3_cat, top4_cat, top5_cat),
    is_contained_top6 = cmp_code %in% c(top1_cat, top2_cat, top3_cat, top4_cat, top5_cat, top6_cat),
    is_contained_top7 = cmp_code %in% c(top1_cat, top2_cat, top3_cat, top4_cat, top5_cat, top6_cat, top7_cat),
    is_contained_top8 = cmp_code %in% c(top1_cat, top2_cat, top3_cat, top4_cat, top5_cat, top6_cat, top7_cat, top8_cat),
    is_contained_top9 = cmp_code %in% c(top1_cat, top2_cat, top3_cat, top4_cat, top5_cat, top6_cat, top7_cat, top8_cat, top9_cat),
    is_contained_top10 = cmp_code %in% c(top1_cat, top2_cat, top3_cat, top4_cat, top5_cat, top6_cat, top7_cat, top8_cat, top9_cat, top10_cat)
  ) %>%
  ungroup(); beepr::beep()

# Get a table by country
marpor_quasi_classified %>%
  group_by(country) %>%
  summarize(
    is_contained_top1 = mean(is_contained_top1, na.rm = TRUE),
    is_contained_top2 = mean(is_contained_top2, na.rm = TRUE),
    is_contained_top3 = mean(is_contained_top3, na.rm = TRUE),
    is_contained_top4 = mean(is_contained_top4, na.rm = TRUE),
    is_contained_top5 = mean(is_contained_top5, na.rm = TRUE),
    is_contained_top6 = mean(is_contained_top6, na.rm = TRUE),
    is_contained_top7 = mean(is_contained_top7, na.rm = TRUE),
    is_contained_top8 = mean(is_contained_top8, na.rm = TRUE),
    is_contained_top9 = mean(is_contained_top9, na.rm = TRUE),
    is_contained_top10 = mean(is_contained_top10, na.rm = TRUE)
  ) %>% 
  mutate(across(starts_with("is_contained"), ~scales::percent(.x, accuracy = 0.01))) %>% 
  knitr::kable("latex", booktabs = TRUE) %>%
  kableExtra::kable_styling(latex_options = c("striped", "scale_down"))

# Inspect sentences where MARPOR policy code is not contained in Top-10 predicted labels
marpor_quasi_classified %>% 
  filter(is_contained_top10 != TRUE)

# Filter for those sentences where MARPOR category is contained in the TOP-10 predictions
marpor_quasi_classified <- marpor_quasi_classified %>% 
  filter(is_contained_top10 == TRUE)

# Density plot: Top-1 prediction probability
marpor_quasi_classified %>%
  select(ends_with("_score")) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(name = str_remove(name, "_score") %>% str_replace(., "top", "Top "),
         name = factor(name, levels = paste("Top", 1:10))) %>%
  ggplot(aes(value)) + 
  geom_histogram(aes(y = after_stat(density)), color = "#FFFFFFD9", alpha = 0.75) +
  geom_density(adjust = 1, trim = FALSE, color = "#111111") +
  facet_wrap(~name, scales = "free_y", ncol = 2) +
  labs(x = "Probability of predicted label", y = "Density")

ggsave(filename = "probability_densities.jpg", units = "mm", height = 140, width = 120, dpi = 900)



# Normalize the top1_score through top10_score columns so that they sum to 1 for each row
marpor_quasi_classified <- marpor_quasi_classified %>%
  rowwise() %>%
  mutate(across(ends_with("_score"), ~ . / sum(c_across(ends_with("_score")), na.rm = TRUE), .names = "norm_{col}")) %>%
  ungroup(); beepr::beep()

# Calculate entropy using normalized probabilities
marpor_quasi_classified <- marpor_quasi_classified %>%
  rowwise() %>%
  mutate(entropy = -sum({
    probs <- c_across(starts_with("norm_top"))
    probs * log(probs)
  }, na.rm = TRUE)) %>%
  ungroup(); beepr::beep()

# Calculate the Effective Number of Policy Labels
marpor_quasi_classified <- marpor_quasi_classified %>%
  rowwise() %>%
  mutate(
    N_eff = 1 / sum({
      probs <- c_across(starts_with("norm_top"))
      probs^2
    }, na.rm = TRUE)
  ) %>%
  ungroup(); beepr::beep()

# Plot entropy
marpor_quasi_classified %>% 
  mutate(entropy = scales::rescale(entropy, to = c(0,1))) %>%
  ggplot(aes(entropy)) + geom_histogram(aes(y = after_stat(density)), color = "#FFFFFFD9", alpha = 0.75) +
  geom_density(adjust = 1, trim = FALSE, color = "#111111") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  facet_wrap(~country) +
  labs(x = "Entropy", y = "Density")

ggsave(filename = "probability_entropy.jpg", units = "mm", height = 140, width = 180, dpi = 900)



# Get information on political parties (requires API token: https://manifesto-project.wzb.eu)
marpor_main <- mp_maindataset(version = "MPDS2024a", apikey = 'MY_KEY') %>% 
  filter(countryname %in% sort(unique(marpor_quasi_classified$country))) %>%
  filter(party %in% sort(unique(marpor_quasi_classified$party))) %>%
  mutate(country = countryname, 
         countryname = NULL,
         countrycode = countrycode::countrycode(country, "country.name", "iso2c") %>% tolower(),
         perseat = absseat / totseats * 100,
         eyear = as.integer(substr(as.character(date), 1, 4)),
         decade = (eyear %/% 10) * 10,
         decade = as.factor(decade)) %>%
  arrange(country, party, date) %>%
  group_by(country, party) %>%
  mutate(pervote_lag1 = lag(pervote, 1),
         pervote_lag2 = lag(pervote, 2),
         pervote_change_lag1 = pervote - pervote_lag1,
         pervote_change_lag2 = pervote_lag1 - pervote_lag2) %>%
  mutate(perseat_lag1 = lag(perseat, 1),
         perseat_lag2 = lag(perseat, 2),
         perseat_change_lag1 = perseat - perseat_lag1,
         perseat_change_lag2 = perseat_lag1 - perseat_lag2) %>% 
  ungroup() %>%
  mutate(parfamname = case_when(parfam == 10 ~ "ECO",
                                parfam == 20 ~ "LEF",
                                parfam == 30 ~ "SOC",
                                parfam == 40 ~ "LIB",
                                parfam == 50 ~ "CHR",
                                parfam == 60 ~ "CON",
                                parfam == 70 ~ "NAT",
                                parfam == 80 ~ "AGR",
                                parfam == 90 ~ "ETH",
                                parfam == 95 ~ "SIP",
                                TRUE ~ NA_character_))

# Manually recode "UKIP" from "SIP" into "NAT"
marpor_main <- marpor_main %>%
  mutate(parfam = ifelse(party == 51951, 70, parfam)) %>% 
  mutate(parfamname = ifelse(party == 51951, "NAT", parfamname))

# Remove "AGR" and "SIP"
marpor_main <- marpor_main %>%
  filter(!parfamname %in% c("AGR", "SIP"))

# Narrow down to relevant columns
marpor_main <- marpor_main %>%
  select(country, countrycode, eumember, edate, date, eyear, decade, party, partyname, partyabbrev, 
         parfam, parfamname, progtype, pervote, perseat, rile, total,
         pervote_change_lag1, pervote_change_lag2, 
         perseat_change_lag1, perseat_change_lag2,
         matches("per\\d{3,}"))

# Remove sub-categories for Central and Eastern European countries (per1011 – per7062)
marpor_main <- marpor_main %>% 
  select(-matches("per\\d{4}"))

# Remove sub-categories from  MARPOR's V5 handbook
marpor_main <- marpor_main %>% 
  select(-matches("\\d_"))

# Scale categories to add up to 100%
marpor_main <- marpor_main %>%
  rowwise() %>%
  mutate(row_sum = sum(c_across(matches("per\\d{3,}")), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(matches("per\\d{3,}"), ~ . / row_sum * 100)) %>%
  select(-row_sum)

# Core salience (change)
marpor_main <- marpor_main %>%
  rowwise() %>%
  mutate(core = case_when(parfamname == "CHR" ~ sum(per203, per504, per601, per603, per703, na.rm = TRUE),
                          parfamname == "CON" ~ sum(per401, per410, per414, per601, per605, na.rm = TRUE),
                          parfamname == "ECO" ~ sum(per106, per201, per202, per416, per501, na.rm = TRUE),
                          parfamname == "ETH" ~ sum(per109, per301, per303, per502, per602, per705, na.rm = TRUE),
                          parfamname == "LEF" ~ sum(per103, per404, per412, per413, per415, per503, per701, na.rm = TRUE),
                          parfamname == "LIB" ~ sum(per303, per401, per407, per410, per414, na.rm = TRUE),
                          parfamname == "NAT" ~ sum(per109, per110, per406, per601, per605, per608, na.rm = TRUE),
                          parfamname == "SOC" ~ sum(per402, per403, per503, per504, per701, na.rm = TRUE),
                          TRUE ~ NA_real_)) %>%
  arrange(parfamname, country, date) %>%
  group_by(parfamname, country) %>%
  mutate(core_change_lag1 = core - lag(core, 1),
         core_change_lag2 = lag(core, 1) - lag(core, 2)) %>%
  ungroup()

# Exclude where core salience: 0%
marpor_main <- marpor_main %>% 
  filter(core > 0)

# Remove policy categories
marpor_main <- marpor_main %>% 
  select(-matches("per\\d{3,}"))

# Define party family colors
family_colors <- c(
  CHR = "#000000",
  CON = "#1E90FF",
  ECO = "#008000",
  ETH = "#808080",
  LEF = "#800000",
  LIB = "#FFEA00",
  NAT = "#00008B",
  SOC = "#FF0000"
)

# Plot core salience
marpor_main %>% 
  group_by(country, parfamname) %>% 
  summarize(core = mean(core, na.rm=T) / 100, 
            countrycode = first(countrycode),
            .groups = 'drop') %>% 
  ggplot(aes(x = tidytext::reorder_within(parfamname, core, country), ymin = 0, y = core, ymax = core, country = countrycode)) +
  tidytext::scale_x_reordered() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~country, scales = "free_y") +
  geom_linerange(linewidth = 0.8, alpha = 0.5) +
  ggflags::geom_flag(size = 4) +
  theme_minimal() +
  coord_flip() +
  labs(y = "Mean size of a party family’s core", x = NULL)

ggsave(filename = "mean_core_size.jpg", units = "mm", height = 140, width = 220, dpi = 900)

# Plot core salience over time
marpor_main %>% 
  group_by(country, parfamname, edate) %>% 
  summarize(core = mean(core, na.rm=T) / 100, 
            countrycode = first(countrycode),
            .groups = 'drop') %>% 
  ggplot(aes(x = edate, y = core, color = parfamname)) +
  scale_color_manual(values = family_colors) +
  facet_wrap(~country, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(se = FALSE, span = 0.75, size = 0.8) +
  theme_minimal() +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") +
  labs(y = "Mean size of a party family’s core over time", x = "Election year", color = NULL)

ggsave(filename = "mean_core_size_time.jpg", units = "mm", height = 140, width = 220, dpi = 900)

# Plot core salience and core salience change distributions
marpor_main %>% 
  group_by(country, parfamname, edate) %>% 
  summarize(core = mean(core, na.rm=T) / 100, 
            core_change_lag1 = mean(core_change_lag1, na.rm=T) / 100,
            countrycode = first(countrycode),
            .groups = 'drop') %>%
  na.omit() %>%
  pivot_longer(cols = c(core, core_change_lag1), names_to = "core_type", values_to = "value") %>%
  mutate(core_type = ifelse(core_type == "core", "Core salience", paste("Core salience", "\u0394"))) %>%
  ggplot(aes(x = value)) +
  geom_histogram(color = "#FFFFFFD9", alpha = 0.75) +
  geom_density(adjust = 1, trim = FALSE, color = "#111111") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~core_type, scales = "free_x") +
  labs(x = NULL, y = "Density") +
  theme(panel.spacing = unit(2, "lines"))

ggsave(filename = "y_distribution.jpg", units = "mm", height = 140, width = 180, dpi = 900)

# Manifestos with highest and lowest core salience
bind_rows(marpor_main %>% slice_max(core, n = 10) %>% mutate(is_max = "Top 10"),
          marpor_main %>% slice_min(core, n = 10) %>% mutate(is_max = "Bottom 10")) %>% 
  select(country, countrycode, eumember, edate, partyname, parfamname, core, is_max) %>%
  mutate(core = core / 100,
         year = year(edate),
         partyname_year = paste0(partyname, " (", year, ")")) %>%
  ggplot(aes(x = tidytext::reorder_within(partyname_year, core, is_max), ymin = 0, ymax = core, y = core, country = countrycode)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  tidytext::scale_x_reordered() +
  geom_linerange(linewidth = 0.8, alpha = 0.5) +
  ggflags::geom_flag(size = 4) +
  theme_minimal() +
  facet_wrap(~is_max, scales = "free") +
  labs(x = NULL, y = "Size of a party’s core in percent of its election manifesto") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8))

ggsave(filename = "core_min_max.jpg", units = "mm", height = 140, width = 180, dpi = 900)



# Get parlgov parties
parlgov_parties <- read_csv("https://parlgov.org/data/parlgov-development_csv-utf-8/view_party.csv", guess_max = Inf, show_col_types = FALSE) %>% 
  select(party_id, cmp) %>% 
  filter(!is.na(cmp))

# Get parlgov cabinets and join with parties (normalize where parties' status changed during the legislative period)
parlgov_cabinets <- read_csv("https://parlgov.org/data/parlgov-development_csv-utf-8/view_cabinet.csv", guess_max = Inf, show_col_types = FALSE) %>% 
  select(election_date, caretaker, cabinet_party, prime_minister, left_right, party_id) %>% 
  group_by(party_id, election_date) %>% 
  summarize(caretaker = max(caretaker, na.rm = TRUE),
            cabinet_party = max(cabinet_party, na.rm = TRUE),
            prime_minister = max(prime_minister, na.rm = TRUE),
            left_right = mean(left_right, na.rm = TRUE),
            .groups = 'drop')

# Join the two datasets
parlgov <- left_join(parlgov_cabinets, parlgov_parties, by = "party_id") %>% 
  filter(!is.na(cmp)) %>% 
  select(-party_id) %>%
  rename(caretaker_pg = caretaker, cabinet_party_pg = cabinet_party, prime_minister_pg = prime_minister, left_right_pg = left_right, edate = election_date, party = cmp) %>%
  mutate(across(.cols = c(caretaker_pg:party), ~ifelse(is.nan(.), NA, .)))

# Join with MARPOR
marpor_main <- left_join(marpor_main, parlgov, by = c("party", "edate"))

# Remove parlgov
rm(parlgov_parties, parlgov_cabinets, parlgov)

# Read manually coded incumbency data
pg_missing_coded <- readxl::read_xlsx("pg_missing_coded.xlsx")

# Update missing values in marpor_main using pg_missing_coded
marpor_main <- marpor_main %>%
  rows_patch(pg_missing_coded %>% select(-partyname), by = c("country", "edate", "party"))

# Code niche party
marpor_main <- marpor_main %>%
  mutate(NICHE = ifelse(parfamname %in% c("CHR", "CON", "SOC"), 0, 1))

# Join party information with classified quasi-sentences
marpor_quasi_classified <- left_join(marpor_quasi_classified, marpor_main %>% select(-eyear), 
                                            by = c("country", "party", "date"))

# Core count (i.e., core dummy based on classifications)
marpor_quasi_classified <- marpor_quasi_classified %>%
  mutate(core_count = case_when(parfamname == "CHR" & cmp_code %in% c("203", "504", "601", "603", "703") ~ 1,
                                parfamname == "CON" & cmp_code %in% c("401", "410", "414", "601", "605") ~ 1,
                                parfamname == "ECO" & cmp_code %in% c("106", "201", "202", "416", "501") ~ 1,
                                parfamname == "ETH" & cmp_code %in% c("109", "301", "303", "502", "602", "705") ~ 1,
                                parfamname == "LEF" & cmp_code %in% c("103", "404", "412", "413", "415", "503", "701") ~ 1,
                                parfamname == "LIB" & cmp_code %in% c("303", "401", "407", "410", "414") ~ 1,
                                parfamname == "NAT" & cmp_code %in% c("109", "110", "406", "601", "605", "608") ~ 1,
                                parfamname == "SOC" & cmp_code %in% c("402", "403", "503", "504", "701") ~ 1,
                                TRUE ~ 0))

# Remove rows where family name is missing
marpor_quasi_classified <- marpor_quasi_classified %>% 
  filter(!is.na(parfamname))

# Remove rows where pervote is missing (due to short-term electoral alliances)
marpor_quasi_classified <- marpor_quasi_classified %>% 
  filter(!is.na(pervote))

# Remove the "eu_code" column
marpor_quasi_classified <- marpor_quasi_classified %>% 
  select(-eu_code)

# Table of list of included parties
marpor_quasi_classified %>% 
  select(country, partyname, party) %>%
  unique() %>% 
  arrange(country, partyname) %>%
  knitr::kable(format = "latex", booktabs = TRUE, caption = "Overview of included political parties across 17 countries") %>%
  writeLines("party_table.tex")



# ----------------------------------------------------------------------------------- #
# POLICY PROFILES AND IDEOLOGICAL SCALING: EXPLAINING EXTREMITY AND POSITIONAL CHANGE #
# ----------------------------------------------------------------------------------- #

# Scale LR partially by core vs. periphery, using all top-10 labels
LONG_FORMAT <- marpor_quasi_classified %>%
  select(-starts_with(c("is_contained_"))) %>%
  select(-c(annotations, translation_en)) %>% 
  pivot_longer(cols = ends_with("_label"), names_to = "top", values_to = "label") %>%
  pivot_longer(cols = starts_with("norm_"), names_to = "top_score", values_to = "score") %>%
  mutate(top = gsub("_label", "", top),
         top_score = str_extract(top_score, "top\\d+")) %>%
  filter(top == top_score) %>%
  select(-top_score) %>% 
  mutate(manifesto_core_index = paste0(manifesto_id, "_", core_count)); beepr::beep()

# Calculate length of core and periphery per document in N of quasi-sentences
LONG_FORMAT <- LONG_FORMAT %>% 
  group_by(manifesto_core_index) %>%
  mutate(manifesto_qs_count = n() / 10) %>% 
  ungroup()

# Remove top*_cat and top*_score
LONG_FORMAT <- LONG_FORMAT %>% 
  select(-matches("^top([1-9]|10)_cat$"), -matches("^top([1-9]|10)_score$")) %>% 
  select(-c(text, manifesto_core_index, pos, top)) %>% 
  relocate(c("edate", "eyear", "decade"), .after = "date") %>%
  relocate(c("countrycode", "eumember"), .after = "country") %>%
  relocate("manifesto_qs_count", .after = "manifesto_id")

# Aggregate: Sum of probability scores per label divided by total sentence count
LONG_FORMAT_AGGR <- LONG_FORMAT %>%
  group_by(manifesto_id, core_count, label) %>% 
  summarize(country = first(country),
            countrycode = first(countrycode),
            edate = first(edate),
            eyear = mean(eyear, na.rm=T),
            party = first(party),
            parfamname = first(parfamname),
            eumember = mean(eumember, na.rm=T),
            core = mean(core, na.rm=T),
            pervote = mean(pervote, na.rm=T),
            pervote_change_lag1 = mean(pervote_change_lag1, na.rm=T),
            pervote_change_lag2 = mean(pervote_change_lag2, na.rm=T),
            perseat = mean(perseat, na.rm=T),
            perseat_change_lag1 = mean(perseat_change_lag1, na.rm=T),
            perseat_change_lag2 = mean(perseat_change_lag2, na.rm=T),
            cabinet_party_pg = mean(cabinet_party_pg, na.rm=T),
            prime_minister_pg = mean(prime_minister_pg, na.rm=T),
            left_right_pg = mean(left_right_pg, na.rm=T), 
            NICHE = mean(NICHE, na.rm=T),
            manifesto_qs_count = mean(manifesto_qs_count, na.rm=T), 
            sum_label_probability = sum(score, na.rm=T),
            .groups = 'drop')

# Get mean label probability
LONG_FORMAT_AGGR <- LONG_FORMAT_AGGR %>% 
  mutate(mean_label_probability = sum_label_probability / manifesto_qs_count * 100)

# Modify the label columns to prepend "cat_" to the numerical prefix of each label
LONG_FORMAT_AGGR <- LONG_FORMAT_AGGR %>%
  mutate(label = paste0("cat_", str_extract(label, "^[0-9]+")))

# Pivot wider
WIDE_FORMAT <- LONG_FORMAT_AGGR %>%
  select(-c(sum_label_probability, manifesto_qs_count)) %>%
  pivot_wider(names_from = label, values_from = mean_label_probability, values_fill = list(mean_label_probability = 0))

# How many different labels are there in total? All of them (56).
length(unique(LONG_FORMAT_AGGR$label))

# Do the cat-scores add up to 100%? They do.
WIDE_FORMAT %>% 
  rowwise() %>%
  mutate(SUM = sum(c_across(starts_with("cat_")))) %>%
  ungroup() %>%
  pull(SUM) %>% 
  table()

# Setting the policy categories for scaling
WIDE_FORMAT <- WIDE_FORMAT %>% 
  mutate(
    # Economic: "Our basic combination"
    Econ_Left_OurBasic = cat_403 + cat_404 + cat_406 + cat_409 + cat_412 + cat_413 + cat_415 + cat_504 + cat_701,
    Econ_Right_OurBasic = cat_401 + cat_402 + cat_407 + cat_410 + cat_414 + cat_505 + cat_702,
    # Economic: "Our extended combination"
    Econ_Left_OurExtended = cat_403 + cat_404 + cat_405 + cat_406 + cat_409 + cat_412 + cat_413 + cat_415 + cat_416 + cat_504 + cat_701,
    Econ_Right_OurExtended = cat_401 + cat_402 + cat_407 + cat_410 + cat_414 + cat_505 + cat_702,
    # Economic: "Krause 2020"
    Econ_Left_Krause2020 = cat_403 + cat_404 + cat_406 + cat_409 + cat_412 + cat_413 + cat_415 + cat_504 + cat_701,
    Econ_Right_Krause2020 = cat_401 + cat_402 + cat_407 + cat_410 + cat_411 + cat_414 + cat_505 + cat_702,
    # Economic: "MARPOR 2022"
    Econ_Left_MARPOR2022 = cat_403 + cat_404 + cat_405 + cat_406 + cat_409 + cat_412 + cat_413 + cat_415 + cat_504 + cat_506 + cat_701,
    Econ_Right_MARPOR2022 = cat_401 + cat_402 + cat_407 + cat_410 + cat_414 + cat_505 + cat_507 + cat_702,
    # Cultural: "Our basic combination"
    Cult_Left_OurBasic = cat_602 + cat_604 + cat_607,
    Cult_Right_OurBasic = cat_601 + cat_603 + cat_605 + cat_608,
    # Cultural: "Our extended combination"
    Cult_Left_OurExtended = cat_501 + cat_602 + cat_604 + cat_607 + cat_706,
    Cult_Right_OurExtended = cat_601 + cat_603 + cat_605 + cat_608,
    # Cultural: "Krause 2020"
    Cult_Left_Krause2020 = cat_103 + cat_105 + cat_106 + cat_107 + cat_202 + cat_416 + cat_501 + cat_602 + cat_604 + cat_606 + cat_607 + cat_705,
    Cult_Right_Krause2020 = cat_104 + cat_109 + cat_305 + cat_601 + cat_603 + cat_605 + cat_608,
    # Cultural: "MARPOR 2022"
    Cult_Left_MARPOR2022 = cat_201 + cat_202 + cat_503 + cat_602 + cat_604 + cat_607,
    Cult_Right_MARPOR2022 = cat_601 + cat_603 + cat_605 + cat_608 + cat_704
  )

# Calculate Left-Right positions
WIDE_FORMAT <- WIDE_FORMAT %>% 
  mutate(
    # Calculate Economic Left-Right scale positions
    Econ_LR_OurBasic = (Econ_Right_OurBasic - Econ_Left_OurBasic) / (Econ_Right_OurBasic + Econ_Left_OurBasic),
    Econ_LR_OurExtended = (Econ_Right_OurExtended - Econ_Left_OurExtended) / (Econ_Right_OurExtended + Econ_Left_OurExtended),
    Econ_LR_Krause2020 = (Econ_Right_Krause2020 - Econ_Left_Krause2020) / (Econ_Right_Krause2020 + Econ_Left_Krause2020),
    Econ_LR_MARPOR2022 = (Econ_Right_MARPOR2022 - Econ_Left_MARPOR2022) / (Econ_Right_MARPOR2022 + Econ_Left_MARPOR2022),
    # Calculate Cultural Left-Right scale positions
    Cult_LR_OurBasic = (Cult_Right_OurBasic - Cult_Left_OurBasic) / (Cult_Right_OurBasic + Cult_Left_OurBasic),
    Cult_LR_OurExtended = (Cult_Right_OurExtended - Cult_Left_OurExtended) / (Cult_Right_OurExtended + Cult_Left_OurExtended),
    Cult_LR_Krause2020 = (Cult_Right_Krause2020 - Cult_Left_Krause2020) / (Cult_Right_Krause2020 + Cult_Left_Krause2020),
    Cult_LR_MARPOR2022 = (Cult_Right_MARPOR2022 - Cult_Left_MARPOR2022) / (Cult_Right_MARPOR2022 + Cult_Left_MARPOR2022)
  )

# Salience of economic and cultural issues
WIDE_FORMAT <- WIDE_FORMAT %>% 
  mutate(
    ECONSALIENCE_OurBasic = (Econ_Right_OurBasic + Econ_Left_OurBasic),
    CULTSALIENCE_OurBasic = (Cult_Right_OurBasic + Cult_Left_OurBasic),
    ECONSALIENCE_OurExtended = (Econ_Right_OurExtended + Econ_Left_OurExtended),
    CULTSALIENCE_OurExtended = (Cult_Right_OurExtended + Cult_Left_OurExtended),
    ECONSALIENCE_Krause2020 = (Econ_Right_Krause2020 + Econ_Left_Krause2020),
    CULTSALIENCE_Krause2020 = (Cult_Right_Krause2020 + Cult_Left_Krause2020),
    ECONSALIENCE_MARPOR2022 = (Econ_Right_MARPOR2022 + Econ_Left_MARPOR2022),
    CULTSALIENCE_MARPOR2022 = (Cult_Right_MARPOR2022 + Cult_Left_MARPOR2022)
  )

# Change in salience of economic and cultural issues
WIDE_FORMAT <- WIDE_FORMAT %>%
  arrange(country, party, core_count, edate) %>%
  group_by(country, party, core_count) %>%
  mutate(
    ECONSALIENCECHANGE_OurBasic = ECONSALIENCE_OurBasic - lag(ECONSALIENCE_OurBasic),
    CULTSALIENCECHANGE_OurBasic = CULTSALIENCE_OurBasic - lag(CULTSALIENCE_OurBasic),
    ECONSALIENCECHANGE_OurExtended = ECONSALIENCE_OurExtended - lag(ECONSALIENCE_OurExtended),
    CULTSALIENCECHANGE_OurExtended = CULTSALIENCE_OurExtended - lag(CULTSALIENCE_OurExtended),
    ECONSALIENCECHANGE_Krause2020 = ECONSALIENCE_Krause2020 - lag(ECONSALIENCE_Krause2020),
    CULTSALIENCECHANGE_Krause2020 = CULTSALIENCE_Krause2020 - lag(CULTSALIENCE_Krause2020),
    ECONSALIENCECHANGE_MARPOR2022 = ECONSALIENCE_MARPOR2022 - lag(ECONSALIENCE_MARPOR2022),
    CULTSALIENCECHANGE_MARPOR2022 = CULTSALIENCE_MARPOR2022 - lag(CULTSALIENCE_MARPOR2022)
  ) %>%
  ungroup()

# Calculate distance, separately for economy and culture
WIDE_FORMAT <- WIDE_FORMAT %>%
  mutate(Econ_LR_OurBasic_DIST    = abs(Econ_LR_OurBasic),
         Econ_LR_OurExtended_DIST = abs(Econ_LR_OurExtended),
         Econ_LR_Krause2020_DIST  = abs(Econ_LR_Krause2020),
         Econ_LR_MARPOR2022_DIST  = abs(Econ_LR_MARPOR2022),
         Cult_LR_OurBasic_DIST    = abs(Cult_LR_OurBasic),
         Cult_LR_OurExtended_DIST = abs(Cult_LR_OurExtended),
         Cult_LR_Krause2020_DIST  = abs(Cult_LR_Krause2020),
         Cult_LR_MARPOR2022_DIST  = abs(Cult_LR_MARPOR2022))

WIDE_FORMAT <- WIDE_FORMAT %>%
  mutate(Econ_LR_OurBasic_DIST    = Econ_LR_OurBasic_DIST / max(Econ_LR_OurBasic_DIST, na.rm = TRUE) * 100,
         Econ_LR_OurExtended_DIST = Econ_LR_OurExtended_DIST / max(Econ_LR_OurExtended_DIST, na.rm = TRUE) * 100,
         Econ_LR_Krause2020_DIST  = Econ_LR_Krause2020_DIST / max(Econ_LR_Krause2020_DIST, na.rm = TRUE) * 100,
         Econ_LR_MARPOR2022_DIST  = Econ_LR_MARPOR2022_DIST / max(Econ_LR_MARPOR2022_DIST, na.rm = TRUE) * 100,
         Cult_LR_OurBasic_DIST    = Cult_LR_OurBasic_DIST / max(Cult_LR_OurBasic_DIST, na.rm = TRUE) * 100,
         Cult_LR_OurExtended_DIST = Cult_LR_OurExtended_DIST / max(Cult_LR_OurExtended_DIST, na.rm = TRUE) * 100,
         Cult_LR_Krause2020_DIST  = Cult_LR_Krause2020_DIST / max(Cult_LR_Krause2020_DIST, na.rm = TRUE) * 100,
         Cult_LR_MARPOR2022_DIST  = Cult_LR_MARPOR2022_DIST / max(Cult_LR_MARPOR2022_DIST, na.rm = TRUE) * 100)

# Combined extremity
WIDE_FORMAT <- WIDE_FORMAT %>%
  rowwise() %>%
  mutate(LR_OurBasic_EXTREME    = sqrt(Econ_LR_OurBasic^2 + Cult_LR_OurBasic^2),
         LR_OurExtended_EXTREME = sqrt(Econ_LR_OurExtended^2 + Cult_LR_OurExtended^2),
         LR_Krause2020_EXTREME  = sqrt(Econ_LR_Krause2020^2 + Cult_LR_Krause2020^2),
         LR_MARPOR2022_EXTREME  = sqrt(Econ_LR_MARPOR2022^2 + Cult_LR_MARPOR2022^2)) %>%
  ungroup() %>%
  group_by(country, edate) %>%
  mutate(LR_OurBasic_EXTREME    = LR_OurBasic_EXTREME / max(LR_OurBasic_EXTREME, na.rm = TRUE) * 100,
         LR_OurExtended_EXTREME = LR_OurExtended_EXTREME / max(LR_OurExtended_EXTREME, na.rm = TRUE) * 100,
         LR_Krause2020_EXTREME  = LR_Krause2020_EXTREME / max(LR_Krause2020_EXTREME, na.rm = TRUE) * 100,
         LR_MARPOR2022_EXTREME  = LR_MARPOR2022_EXTREME / max(LR_MARPOR2022_EXTREME, na.rm = TRUE) * 100)

# Position change
WIDE_FORMAT <- WIDE_FORMAT %>%
  arrange(country, party, core_count, edate) %>%
  group_by(country, party, core_count) %>% 
  mutate(Cult_Position_Change = abs(Cult_LR_OurExtended - lag(Cult_LR_OurExtended)),
         Econ_Position_Change = abs(Econ_LR_OurExtended - lag(Econ_LR_OurExtended)),
         Combined_Position_Change = sqrt((Econ_LR_OurExtended - lag(Econ_LR_OurExtended))^2 + 
                                           (Cult_LR_OurExtended - lag(Cult_LR_OurExtended))^2)) %>%
  ungroup()

WIDE_FORMAT <- WIDE_FORMAT %>%
  arrange(country, party, core_count, edate) %>%
  group_by(country, party, core_count) %>% 
  mutate(Cult_Position_Change_Krause2020 = abs(Cult_LR_Krause2020 - lag(Cult_LR_Krause2020)),
         Econ_Position_Change_Krause2020 = abs(Econ_LR_Krause2020 - lag(Econ_LR_Krause2020)),
         Combined_Position_Change_Krause2020 = sqrt((Econ_LR_Krause2020 - lag(Econ_LR_Krause2020))^2 + 
                                                      (Cult_LR_Krause2020 - lag(Cult_LR_Krause2020))^2)) %>%
  ungroup()

WIDE_FORMAT <- WIDE_FORMAT %>%
  arrange(country, party, core_count, edate) %>%
  group_by(country, party, core_count) %>% 
  mutate(Cult_Position_Change_MARPOR2022 = abs(Cult_LR_MARPOR2022 - lag(Cult_LR_MARPOR2022)),
         Econ_Position_Change_MARPOR2022 = abs(Econ_LR_MARPOR2022 - lag(Econ_LR_MARPOR2022)),
         Combined_Position_Change_MARPOR2022 = sqrt((Econ_LR_MARPOR2022 - lag(Econ_LR_MARPOR2022))^2 +
                                                      (Cult_LR_MARPOR2022 - lag(Cult_LR_MARPOR2022))^2)) %>%
  ungroup()

# Use logged change, given that change is skewed towards low values
WIDE_FORMAT <- WIDE_FORMAT %>%
  mutate(Log_Cult_Position_Change = log(Cult_Position_Change + 1),
         Log_Econ_Position_Change = log(Econ_Position_Change + 1),
         Log_Combined_Position_Change = log(Combined_Position_Change + 1),
         Log_Combined_Position_Change_Krause2020 = log(Combined_Position_Change_Krause2020 + 1),
         Log_Combined_Position_Change_MARPOR2022 = log(Combined_Position_Change_MARPOR2022 + 1))

WIDE_FORMAT %>%
  select(starts_with("Log_")) %>%
  pivot_longer(cols = starts_with("Log_"), names_to = "Change_dimension", values_to = "Change") %>%
  mutate(Change_dimension = case_when(Change_dimension == "Log_Cult_Position_Change" ~ "Cultural change",
                                      Change_dimension == "Log_Econ_Position_Change" ~ "Economic change",
                                      Change_dimension == "Log_Combined_Position_Change" ~ "Combined change",
                                      TRUE ~ NA_character_)) %>%
  mutate(Change_dimension = factor(Change_dimension, levels = c("Cultural change", "Economic change", "Combined change"))) %>%
  ggplot(aes(Change)) + 
  geom_histogram(aes(y = after_stat(density)), color = "#FFFFFFD9", alpha = 0.75, bins = 30) +
  geom_density(adjust = 1, trim = FALSE, color = "#111111") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Positional change in party positions", y = "Density") +
  facet_wrap(~Change_dimension)

ggsave(filename = "change_densities.jpg", units = "mm", height = 120, width = 180, dpi = 900)

WIDE_FORMAT %>%
  mutate(core_count = ifelse(core_count == 1, "Core", "Periphery")) %>% 
  select(core_count, starts_with("Log_")) %>%
  pivot_longer(cols = starts_with("Log_"), names_to = "Change_dimension", values_to = "Change") %>%
  mutate(Change_dimension = case_when(Change_dimension == "Log_Cult_Position_Change" ~ "Cultural change",
                                      Change_dimension == "Log_Econ_Position_Change" ~ "Economic change",
                                      Change_dimension == "Log_Combined_Position_Change" ~ "Combined change",
                                      TRUE ~ NA_character_)) %>%
  mutate(Change_dimension = factor(Change_dimension, levels = c("Cultural change", "Economic change", "Combined change"))) %>%
  filter(Change_dimension != "Combined change") %>%
  ggplot(aes(Change, color = core_count, fill = core_count)) +
  geom_density(aes(Change), fill = "transparent", color = "#111111", linetype = "dashed") +
  geom_density(adjust = 1, trim = FALSE) +
  scale_color_manual(values = c("Core" = RColorBrewer::brewer.pal(3, "Set1")[1] %>% scales::alpha(0.75),
                                "Periphery" = RColorBrewer::brewer.pal(3, "Set1")[2] %>% scales::alpha(0.5))) +
  scale_fill_manual(values = c("Core" = RColorBrewer::brewer.pal(3, "Set1")[1] %>% scales::alpha(0.75),
                               "Periphery" = RColorBrewer::brewer.pal(3, "Set1")[2] %>% scales::alpha(0.5))) +
  scale_x_continuous(limits = c(-0.1, 1.1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  labs(x = "Positional change in party positions", y = "Density", fill = NULL, color = NULL) +
  facet_wrap(~Change_dimension) +
  theme(legend.position = "bottom")

ggsave(filename = "change_densities_by_core_and_dim.jpg", units = "mm", height = 120, width = 180, dpi = 900)



WIDE_FORMAT %>%
  mutate(core_count = ifelse(core_count == 1, "Core", "Periphery")) %>%
  ggplot(aes(Log_Combined_Position_Change, color = core_count, fill = core_count)) +
  geom_density(aes(Log_Combined_Position_Change), fill = "transparent", color = "#111111", linetype = "dashed") +
  geom_density(adjust = 1, trim = FALSE) +
  scale_color_manual(values = c("Core" = RColorBrewer::brewer.pal(3, "Set1")[1] %>% scales::alpha(0.75),
                                "Periphery" = RColorBrewer::brewer.pal(3, "Set1")[2] %>% scales::alpha(0.5))) +
  scale_fill_manual(values = c("Core" = RColorBrewer::brewer.pal(3, "Set1")[1] %>% scales::alpha(0.75),
                               "Periphery" = RColorBrewer::brewer.pal(3, "Set1")[2] %>% scales::alpha(0.5))) +
  scale_x_continuous(limits = c(-0.1, 1.1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  labs(x = "Position change", y = "Density", fill = NULL, color = NULL) +
  theme(legend.position = "bottom")

ggsave(filename = "change_density_by_core.jpg", units = "mm", height = 120, width = 140, dpi = 900)

# Scatter plot of party positions (uncentered!)
WIDE_FORMAT %>% 
  group_by(country, parfamname, core_count) %>% 
  summarize(Econ_LR_OurExtended = mean(Econ_LR_OurExtended),
            Cult_LR_OurExtended = mean(Cult_LR_OurExtended)) %>%
  filter(country %in% c("Austria", "Belgium", "Cyprus", "Denmark", "Finland", "Germany")) %>%
  #UNCOMMENT THIS: filter(country %in% c("Greece", "Iceland", "Ireland", "Luxembourg", "Netherlands", "Norway")) %>%
  #UNCOMMENT THIS: filter(country %in% c("Portugal", "Spain", "Sweden", "Switzerland", "United Kingdom")) %>%
  ggplot(aes(x = Econ_LR_OurExtended*100, y = Cult_LR_OurExtended*100, color = parfamname, label = parfamname, alpha = as.numeric(core_count))) + ##### year(edate), size = year(edate))) + # , alpha = v3_core #, size = as.numeric(edate)
  geom_hline(aes(yintercept = 0), color = "darkgrey", linewidth = 1.25, alpha = 0.5) +
  geom_vline(aes(xintercept = 0), color = "darkgrey", linewidth = 1.25, alpha = 0.5) +
  geom_point(size = 3) +
  scale_alpha_continuous(range = c(0.35, 0.95)) +
  ggrepel::geom_text_repel(max.overlaps = Inf, size = 2.5) +
  facet_wrap(~country, scales = "free", ncol = 2) +
  scale_color_manual(values = family_colors) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Economic Left-Right", y = "Cultural Left-Right")

ggsave(filename = "pos_scaled_1.jpg", units = "mm", height = 180, width = 140, dpi = 900)

# Box plot of two-dimensional distance from the party system's center (uncentered!)
WIDE_FORMAT %>% 
  pivot_longer(cols = c(Econ_LR_OurExtended_DIST, Cult_LR_OurExtended_DIST), names_to = "Dimension", values_to = "Distance") %>%
  mutate(core_count = ifelse(core_count == 1, "Core", "Periphery"),
         Dimension = ifelse(Dimension == "Econ_LR_OurExtended_DIST", "Economic dimension", "Cultural dimension")) %>%
  ggplot(aes(x = parfamname, y = Distance, color = core_count, fill = core_count)) +
  facet_wrap(~Dimension) +
  geom_boxplot(position = position_dodge(width = 1), size = 0.1) +
  scale_color_manual(values = c("Core" = RColorBrewer::brewer.pal(3, "Set1")[1] %>% scales::alpha(0.95),
                               "Periphery" = RColorBrewer::brewer.pal(3, "Set1")[2] %>% scales::alpha(0.55))) +
  scale_fill_manual(values = c("Core" = RColorBrewer::brewer.pal(3, "Set1")[1] %>% scales::alpha(0.95),
                               "Periphery" = RColorBrewer::brewer.pal(3, "Set1")[2] %>% scales::alpha(0.55))) +
  labs(x = NULL, y = "Ideological distance from the center of the party system", color = NULL, fill = NULL) +
  theme(legend.position = "bottom", axis.line = element_blank(), panel.spacing = unit(2, "lines"))

ggsave(filename = "pos_extremity.jpg", units = "mm", height = 140, width = 180, dpi = 900)

# Density plot of ideological extremity
ggplot(WIDE_FORMAT, aes(LR_OurExtended_EXTREME)) + 
  geom_histogram(aes(y = after_stat(density)), color = "#FFFFFFD9", alpha = 0.75, bins = 30) +
  geom_density(adjust = 1, trim = FALSE, color = "#111111") +
  labs(x = "Combined extremity", y = "Density")

ggsave(filename = "extremity_density.jpg", units = "mm", height = 120, width = 140, dpi = 900)

# Density plot of ideological extremity by core vs. periphery
WIDE_FORMAT %>%
  mutate(core_count = if_else(core_count == 1, "Core", "Periphery")) %>%
  ggplot(aes(LR_OurExtended_EXTREME, color = core_count, fill = core_count)) +
  geom_density(aes(LR_OurExtended_EXTREME), fill = "transparent", color = "#111111", linetype = "dashed") +
  geom_density(adjust = 1, trim = FALSE) +
  scale_color_manual(values = c("Core" = RColorBrewer::brewer.pal(3, "Set1")[1] %>% scales::alpha(0.75),
                                "Periphery" = RColorBrewer::brewer.pal(3, "Set1")[2] %>% scales::alpha(0.5))) +
  scale_fill_manual(values = c("Core" = RColorBrewer::brewer.pal(3, "Set1")[1] %>% scales::alpha(0.75),
                               "Periphery" = RColorBrewer::brewer.pal(3, "Set1")[2] %>% scales::alpha(0.5))) +
  scale_x_continuous(limits = c(-5, 115), breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(0, 0.03)) +
  labs(x = "Extremity", y = "Density", fill = NULL, color = NULL) +
  theme(legend.position = "bottom")

ggsave(filename = "extremity_density_by_core.jpg", units = "mm", height = 120, width = 140, dpi = 900)



# Transform dummies into characters and factors for modeling
WIDE_FORMAT <- WIDE_FORMAT %>%
  mutate(core_count_chr = as.character(core_count),
         core_count_fct = factor(core_count_chr, levels = c("0", "1")),
         NICHE_chr = as.character(NICHE),
         NICHE_fct = factor(NICHE_chr, levels = c("0", "1")),
         edate_fct = as.factor(edate),
         periphery_count_chr = rev(core_count_chr))

# Create incumbency factor
WIDE_FORMAT <- WIDE_FORMAT %>%
  mutate(incumbency = case_when(cabinet_party_pg == 1 & prime_minister_pg == 1 ~ "head of government",
                                cabinet_party_pg == 1 & prime_minister_pg == 0 ~ "coalition party",
                                cabinet_party_pg == 0 & prime_minister_pg == 0 ~ "opposition party",
                                TRUE ~ NA_character_),
         incumbency = factor(incumbency, levels = c("opposition party", "coalition party", "head of government")))

# Create index (country x election)
WIDE_FORMAT$country_election <- interaction(WIDE_FORMAT$country, WIDE_FORMAT$edate_fct)

# Create index (country x election x party)
WIDE_FORMAT$country_election_party <- interaction(WIDE_FORMAT$country, WIDE_FORMAT$edate_fct, WIDE_FORMAT$party)



# ----------------------------------------------------------------------------------- #
# MODELS: EXTREMITY                                                                   #
# ----------------------------------------------------------------------------------- #
#| - H1: core
#| - H3: core * niche
#| - H5: core * vote share change

# MAIN
extremity.1 <- feols(LR_OurExtended_EXTREME ~ core_count_fct + pervote + NICHE_fct + incumbency + ECONSALIENCE_OurExtended + CULTSALIENCE_OurExtended | country + edate_fct, 
                     cluster = ~ country_election, data = WIDE_FORMAT)
extremity.2 <- feols(LR_OurExtended_EXTREME ~ core_count_fct * NICHE_fct + pervote + incumbency + ECONSALIENCE_OurExtended + CULTSALIENCE_OurExtended | country + edate_fct, 
                     cluster = ~ country_election, data = WIDE_FORMAT)
extremity.3 <- feols(LR_OurExtended_EXTREME ~ core_count_fct * pervote_change_lag1 + NICHE_fct + incumbency + ECONSALIENCE_OurExtended + CULTSALIENCE_OurExtended | country + edate_fct, 
                     cluster = ~ country_election, data = WIDE_FORMAT)
extremity.4 <- feols(LR_OurExtended_EXTREME ~ core_count_fct * pervote_change_lag2 + NICHE_fct + incumbency + ECONSALIENCE_OurExtended + CULTSALIENCE_OurExtended | country + edate_fct, 
                     cluster = ~ country_election, data = WIDE_FORMAT)

texreg::screenreg(list(extremity.1, extremity.2, extremity.3, extremity.4))

# CULTURE
extremity.1.c <- feols(Cult_LR_OurExtended_DIST ~ core_count_fct + pervote + NICHE_fct + incumbency + ECONSALIENCE_OurExtended + CULTSALIENCE_OurExtended | country + edate_fct, 
                       cluster = ~ country_election, data = WIDE_FORMAT)
extremity.2.c <- feols(Cult_LR_OurExtended_DIST ~ core_count_fct * NICHE_fct + pervote + incumbency + ECONSALIENCE_OurExtended + CULTSALIENCE_OurExtended | country + edate_fct, 
                       cluster = ~ country_election, data = WIDE_FORMAT)
extremity.3.c <- feols(Cult_LR_OurExtended_DIST ~ core_count_fct * pervote_change_lag1 + NICHE_fct + incumbency + ECONSALIENCE_OurExtended + CULTSALIENCE_OurExtended | country + edate_fct, 
                       cluster = ~ country_election, data = WIDE_FORMAT)
extremity.4.c <- feols(Cult_LR_OurExtended_DIST ~ core_count_fct * pervote_change_lag2 + NICHE_fct + incumbency + ECONSALIENCE_OurExtended + CULTSALIENCE_OurExtended | country + edate_fct, 
                       cluster = ~ country_election, data = WIDE_FORMAT)

texreg::screenreg(list(extremity.1.c, extremity.2.c, extremity.3.c, extremity.4.c))

# ECONOMY
extremity.1.e <- feols(Econ_LR_OurExtended_DIST ~ core_count_fct + pervote + NICHE_fct + incumbency + ECONSALIENCE_OurExtended + CULTSALIENCE_OurExtended | country + edate_fct,
                       cluster = ~ country_election, data = WIDE_FORMAT)
extremity.2.e <- feols(Econ_LR_OurExtended_DIST ~ core_count_fct * NICHE_fct + pervote + incumbency + ECONSALIENCE_OurExtended + CULTSALIENCE_OurExtended | country + edate_fct, 
                       cluster = ~ country_election, data = WIDE_FORMAT)
extremity.3.e <- feols(Econ_LR_OurExtended_DIST ~ core_count_fct * pervote_change_lag1 + NICHE_fct + incumbency + ECONSALIENCE_OurExtended + CULTSALIENCE_OurExtended | country + edate_fct, 
                       cluster = ~ country_election, data = WIDE_FORMAT)
extremity.4.e <- feols(Econ_LR_OurExtended_DIST ~ core_count_fct * pervote_change_lag2 + NICHE_fct + incumbency + ECONSALIENCE_OurExtended + CULTSALIENCE_OurExtended | country + edate_fct, 
                       cluster = ~ country_election, data = WIDE_FORMAT)

texreg::screenreg(list(extremity.1.e, extremity.2.e, extremity.3.e, extremity.4.e))

# KRAUSE
extremity.1.krause <- feols(LR_Krause2020_EXTREME ~ core_count_fct + pervote + NICHE_fct + incumbency + ECONSALIENCE_Krause2020 + CULTSALIENCE_Krause2020 | country + edate_fct, 
                            cluster = ~ country_election, data = WIDE_FORMAT)
extremity.2.krause <- feols(LR_Krause2020_EXTREME ~ core_count_fct * NICHE_fct + pervote + incumbency + ECONSALIENCE_Krause2020 + CULTSALIENCE_Krause2020 | country + edate_fct, 
                            cluster = ~ country_election, data = WIDE_FORMAT)
extremity.3.krause <- feols(LR_Krause2020_EXTREME ~ core_count_fct * pervote_change_lag1 + NICHE_fct + incumbency + ECONSALIENCE_Krause2020 + CULTSALIENCE_Krause2020 | country + edate_fct, 
                            cluster = ~ country_election, data = WIDE_FORMAT)
extremity.4.krause <- feols(LR_Krause2020_EXTREME ~ core_count_fct * pervote_change_lag2 + NICHE_fct + incumbency + ECONSALIENCE_Krause2020 + CULTSALIENCE_Krause2020 | country + edate_fct,
                            cluster = ~ country_election, data = WIDE_FORMAT)

texreg::screenreg(list(extremity.1.krause, extremity.2.krause, extremity.3.krause, extremity.4.krause))

# MARPOR
extremity.1.marpor <- feols(LR_MARPOR2022_EXTREME ~ core_count_fct + pervote + NICHE_fct + incumbency + ECONSALIENCE_MARPOR2022 + CULTSALIENCE_MARPOR2022 | country + edate_fct, 
                            cluster = ~ country_election, data = WIDE_FORMAT)
extremity.2.marpor <- feols(LR_MARPOR2022_EXTREME ~ core_count_fct * NICHE_fct + pervote + incumbency + ECONSALIENCE_MARPOR2022 + CULTSALIENCE_MARPOR2022 | country + edate_fct, 
                            cluster = ~ country_election, data = WIDE_FORMAT)
extremity.3.marpor <- feols(LR_MARPOR2022_EXTREME ~ core_count_fct * pervote_change_lag1 + NICHE_fct + incumbency + ECONSALIENCE_MARPOR2022 + CULTSALIENCE_MARPOR2022 | country + edate_fct, 
                            cluster = ~ country_election, data = WIDE_FORMAT)
extremity.4.marpor <- feols(LR_MARPOR2022_EXTREME ~ core_count_fct * pervote_change_lag2 + NICHE_fct + incumbency + ECONSALIENCE_MARPOR2022 + CULTSALIENCE_MARPOR2022 | country + edate_fct,
                            cluster = ~ country_election, data = WIDE_FORMAT)

texreg::screenreg(list(extremity.1.marpor, extremity.2.marpor, extremity.3.marpor, extremity.4.marpor))



# Regression table
esttex(list(extremity.1, extremity.2, extremity.3, extremity.4), 
       dict = c(core_count_fct1 = "Core",
                pervote = "Vote Share",
                pervote_change_lag1 = "Vote Share $\\Delta$ $_{t-1}$",
                pervote_change_lag2 = "Vote Share $\\Delta$ $_{t-2}$",
                incumbencycoalitionparty = "Incumbency: Coalition",
                incumbencyheadofgovernment = "Incumbency: Head of Gov't",
                NICHE_fct1 = "Niche Party",
                ECONSALIENCE_OurExtended = "Salience $_{(economy)}$",
                CULTSALIENCE_OurExtended = "Salience $_{(culture)}$",
                country = "Country",
                edate_fct = "Election Date",
                parfamname = "Party Family"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
       fitstat = ~ n + r2 + ar2,
       digits = 2)

# Appendix table (1)
esttex(list(extremity.1.c, extremity.2.c, extremity.3.c, extremity.4.c,
            extremity.1.e, extremity.2.e, extremity.3.e, extremity.4.e), 
       dict = c(core_count_fct1 = "Core",
                pervote = "Vote Share",
                pervote_change_lag1 = "Vote Share $\\Delta$ $_{t-1}$",
                pervote_change_lag2 = "Vote Share $\\Delta$ $_{t-2}$",
                incumbencycoalitionparty = "Incumbency: Coalition",
                incumbencyheadofgovernment = "Incumbency: Head of Gov't",
                NICHE_fct1 = "Niche Party",
                ECONSALIENCE_OurExtended = "Salience $_{(economy)}$",
                CULTSALIENCE_OurExtended = "Salience $_{(culture)}$",
                country = "Country",
                edate_fct = "Election Date",
                parfamname = "Party Family"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
       fitstat = ~ n + r2 + ar2,
       digits = 2)

# Appendix table (2)
esttex(list(extremity.1.krause, extremity.2.krause, extremity.3.krause, extremity.4.krause,
            extremity.1.marpor, extremity.2.marpor, extremity.3.marpor, extremity.4.marpor), 
       dict = c(core_count_fct1 = "Core",
                pervote = "Vote Share",
                pervote_change_lag1 = "Vote Share $\\Delta$ $_{t-1}$",
                pervote_change_lag2 = "Vote Share $\\Delta$ $_{t-2}$",
                incumbencycoalitionparty = "Incumbency: Coalition",
                incumbencyheadofgovernment = "Incumbency: Head of Gov't",
                NICHE_fct1 = "Niche Party",
                ECONSALIENCE_OurExtended = "Salience $_{(economy)}$",
                CULTSALIENCE_OurExtended = "Salience $_{(culture)}$",
                country = "Country",
                edate_fct = "Election Date",
                parfamname = "Party Family"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
       fitstat = ~ n + r2 + ar2,
       digits = 2)



# ----------------------------------------------------------------------------------- #
# MODELS: POSITION CHANGE                                                             #
# ----------------------------------------------------------------------------------- #
#| - H2: core
#| - H4: niche * core
#| - H6: vote share change * core

# MAIN
change.1 <- feols(Log_Combined_Position_Change ~ core_count_fct + pervote + NICHE_fct + incumbency + ECONSALIENCECHANGE_OurExtended + CULTSALIENCECHANGE_OurExtended | country + edate_fct, 
                  cluster = ~ country_election, data = WIDE_FORMAT)
change.2 <- feols(Log_Combined_Position_Change ~ core_count_fct * NICHE_fct + pervote + incumbency + ECONSALIENCECHANGE_OurExtended + CULTSALIENCECHANGE_OurExtended | country + edate_fct, 
                  cluster = ~ country_election, data = WIDE_FORMAT)
change.3 <- feols(Log_Combined_Position_Change ~ core_count_fct * pervote_change_lag1 * NICHE_fct + incumbency + ECONSALIENCECHANGE_OurExtended + CULTSALIENCECHANGE_OurExtended | country + edate_fct, 
                  cluster = ~ country_election, data = WIDE_FORMAT)
change.4 <- feols(Log_Combined_Position_Change ~ core_count_fct * pervote_change_lag2 * NICHE_fct + incumbency + ECONSALIENCECHANGE_OurExtended + CULTSALIENCECHANGE_OurExtended | country + edate_fct, 
                  cluster = ~ country_election, data = WIDE_FORMAT)

texreg::screenreg(list(change.1, change.2, change.3, change.4))

# CULTURE
change.1.c <- feols(Log_Cult_Position_Change ~ core_count_fct + pervote + NICHE_fct + incumbency + ECONSALIENCECHANGE_OurExtended + CULTSALIENCECHANGE_OurExtended | country + edate_fct, 
                    cluster = ~ country_election, data = WIDE_FORMAT)
change.2.c <- feols(Log_Cult_Position_Change ~ core_count_fct * NICHE_fct + pervote + incumbency + ECONSALIENCECHANGE_OurExtended + CULTSALIENCECHANGE_OurExtended | country + edate_fct, 
                    cluster = ~ country_election, data = WIDE_FORMAT)
change.3.c <- feols(Log_Cult_Position_Change ~ core_count_fct * pervote_change_lag1 * NICHE_fct + incumbency + ECONSALIENCECHANGE_OurExtended + CULTSALIENCECHANGE_OurExtended | country + edate_fct, 
                    cluster = ~ country_election, data = WIDE_FORMAT)
change.4.c <- feols(Log_Cult_Position_Change ~ core_count_fct * pervote_change_lag2 * NICHE_fct + incumbency + ECONSALIENCECHANGE_OurExtended + CULTSALIENCECHANGE_OurExtended | country + edate_fct, 
                    cluster = ~ country_election, data = WIDE_FORMAT)

texreg::screenreg(list(change.1.c, change.2.c, change.3.c, change.4.c))

# ECONOMY
change.1.e <- feols(Log_Econ_Position_Change ~ core_count_fct + pervote + NICHE_fct + incumbency + ECONSALIENCECHANGE_OurExtended + CULTSALIENCECHANGE_OurExtended | country + edate_fct, 
                    cluster = ~ country_election, data = WIDE_FORMAT)
change.2.e <- feols(Log_Econ_Position_Change ~ core_count_fct * NICHE_fct + pervote + incumbency + ECONSALIENCECHANGE_OurExtended + CULTSALIENCECHANGE_OurExtended | country + edate_fct, 
                    cluster = ~ country_election, data = WIDE_FORMAT)
change.3.e <- feols(Log_Econ_Position_Change ~ core_count_fct * pervote_change_lag1 * NICHE_fct + incumbency + ECONSALIENCECHANGE_OurExtended + CULTSALIENCECHANGE_OurExtended | country + edate_fct, 
                    cluster = ~ country_election, data = WIDE_FORMAT)
change.4.e <- feols(Log_Econ_Position_Change ~ core_count_fct * pervote_change_lag2 * NICHE_fct + incumbency + ECONSALIENCECHANGE_OurExtended + CULTSALIENCECHANGE_OurExtended | country + edate_fct, 
                    cluster = ~ country_election, data = WIDE_FORMAT)

texreg::screenreg(list(change.1.e, change.2.e, change.3.e, change.4.e))

# KRAUSE
change.1.krause <- feols(Log_Combined_Position_Change_Krause2020 ~ core_count_fct + pervote + NICHE_fct + incumbency + ECONSALIENCECHANGE_Krause2020 + CULTSALIENCECHANGE_Krause2020 | country + edate_fct, 
                         cluster = ~ country_election, data = WIDE_FORMAT)
change.2.krause <- feols(Log_Combined_Position_Change_Krause2020 ~ core_count_fct * NICHE_fct + pervote + incumbency + ECONSALIENCECHANGE_Krause2020 + CULTSALIENCECHANGE_Krause2020 | country + edate_fct, 
                         cluster = ~ country_election, data = WIDE_FORMAT)
change.3.krause <- feols(Log_Combined_Position_Change_Krause2020 ~ core_count_fct * pervote_change_lag1 * NICHE_fct + incumbency + ECONSALIENCECHANGE_Krause2020 + CULTSALIENCECHANGE_Krause2020 | country + edate_fct, 
                         cluster = ~ country_election, data = WIDE_FORMAT)
change.4.krause <- feols(Log_Combined_Position_Change_Krause2020 ~ core_count_fct * pervote_change_lag2 * NICHE_fct + incumbency + ECONSALIENCECHANGE_Krause2020 + CULTSALIENCECHANGE_Krause2020 | country + edate_fct, 
                         cluster = ~ country_election, data = WIDE_FORMAT)

texreg::screenreg(list(change.1.krause, change.2.krause, change.3.krause, change.4.krause))

# MARPOR
change.1.marpor <- feols(Log_Combined_Position_Change_MARPOR2022 ~ core_count_fct + pervote + NICHE_fct + incumbency + ECONSALIENCECHANGE_MARPOR2022 + CULTSALIENCECHANGE_MARPOR2022 | country + edate_fct, 
                         cluster = ~ country_election, data = WIDE_FORMAT)
change.2.marpor <- feols(Log_Combined_Position_Change_MARPOR2022 ~ core_count_fct * NICHE_fct + pervote + incumbency + ECONSALIENCECHANGE_MARPOR2022 + CULTSALIENCECHANGE_MARPOR2022 | country + edate_fct, 
                         cluster = ~ country_election, data = WIDE_FORMAT)
change.3.marpor <- feols(Log_Combined_Position_Change_MARPOR2022 ~ core_count_fct * pervote_change_lag1 * NICHE_fct + incumbency + ECONSALIENCECHANGE_MARPOR2022 + CULTSALIENCECHANGE_MARPOR2022 | country + edate_fct, 
                         cluster = ~ country_election, data = WIDE_FORMAT)
change.4.marpor <- feols(Log_Combined_Position_Change_MARPOR2022 ~ core_count_fct * pervote_change_lag2 * NICHE_fct + incumbency + ECONSALIENCECHANGE_MARPOR2022 + CULTSALIENCECHANGE_MARPOR2022 | country + edate_fct, 
                         cluster = ~ country_election, data = WIDE_FORMAT)

texreg::screenreg(list(change.1.marpor, change.2.marpor, change.3.marpor, change.4.marpor))



# Regression table
esttex(list(change.1, change.2, change.3, change.4), 
       dict = c(core_count_fct1 = "Core",
                pervote = "Vote Share",
                pervote_change_lag1 = "Vote Share $\\Delta$ $_{t-1}$",
                pervote_change_lag2 = "Vote Share $\\Delta$ $_{t-2}$",
                incumbencycoalitionparty = "Incumbency: Coalition",
                incumbencyheadofgovernment = "Incumbency: Head of Gov't",
                NICHE_fct1 = "Niche Party",
                ECONSALIENCECHANGE_OurExtended = "Salience $\\Delta$ $_{(economy)}$",
                CULTSALIENCECHANGE_OurExtended = "Salience $\\Delta$ $_{(culture)}$",
                country = "Country",
                edate_fct = "Election Date",
                parfamname = "Party Family"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
       fitstat = ~ n + r2 + ar2,
       digits = 2)

# Appendix table (1b)
esttex(list(change.1.c, change.2.c, change.3.c, change.4.c,
            change.1.e, change.2.e, change.3.e, change.4.e), 
       dict = c(core_count_fct1 = "Core",
                pervote = "Vote Share",
                pervote_change_lag1 = "Vote Share $\\Delta$ $_{t-1}$",
                pervote_change_lag2 = "Vote Share $\\Delta$ $_{t-2}$",
                incumbencycoalitionparty = "Incumbency: Coalition",
                incumbencyheadofgovernment = "Incumbency: Head of Gov't",
                NICHE_fct1 = "Niche Party",
                ECONSALIENCECHANGE_OurExtended = "Salience $\\Delta$ $_{(economy)}$",
                CULTSALIENCECHANGE_OurExtended = "Salience $\\Delta$ $_{(culture)}$",
                country = "Country",
                edate_fct = "Election Date",
                parfamname = "Party Family"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
       fitstat = ~ n + r2 + ar2,
       digits = 2)

# Appendix table (2b)
esttex(list(change.1.krause, change.2.krause, change.3.krause, change.4.krause,
            change.1.marpor, change.2.marpor, change.3.marpor, change.4.marpor), 
       dict = c(core_count_fct1 = "Core",
                pervote = "Vote Share",
                pervote_change_lag1 = "Vote Share $\\Delta$ $_{t-1}$",
                pervote_change_lag2 = "Vote Share $\\Delta$ $_{t-2}$",
                incumbencycoalitionparty = "Incumbency: Coalition",
                incumbencyheadofgovernment = "Incumbency: Head of Gov't",
                NICHE_fct1 = "Niche Party",
                
                ECONSALIENCECHANGE_Krause2020 = "Salience $\\Delta$ $_{(economy)}$",
                CULTSALIENCECHANGE_Krause2020 = "Salience $\\Delta$ $_{(culture)}$",
                
                ECONSALIENCECHANGE_MARPOR2022 = "Salience $\\Delta$ $_{(economy)}$",
                CULTSALIENCECHANGE_MARPOR2022 = "Salience $\\Delta$ $_{(culture)}$",
                
                country = "Country",
                edate_fct = "Election Date",
                parfamname = "Party Family"),
       signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
       fitstat = ~ n + r2 + ar2,
       digits = 2)
