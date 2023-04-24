#read in data for setup----
library(tidyverse)
library(readxl)
#data and functions for US & world regions----
source("code/get_regions.R")

#pull in datasets----
#cleaned full Carnegie data set
clean_carn_data <- read_csv("output/cleaned_carnegie_data_2023-03-28.csv") %>% 
  select(NAME, STABBR, DOCRSDEG, HBCU, HSI, MSI, `S&ER&D`, 
         SOCSC_RSD, STEM_RSD, LANDGRNT,	MEDICAL,	TRIBAL, WOMENS)

#cleaned list of unique respondent-provided institutions
clean_surv_inst <- read_csv("output/cleaned_survey_inst_2023-04-11.csv")

#data set of unique respondent-provided institutions matched with listed institutions
#nr_row <- data.frame(inst_name = "Nr", NAME = "Nr")

all_matches <- read_csv("output/carnegie_inst_matches_2023-04-11.csv") #%>% 
  #rows_insert(nr_row)

#data set of non-carn universities/institutions
non_carn_unis <- read_csv("data/non-carnegie_unis.csv") %>% 
  select(-inst_prestige) %>% 
  mutate(Institution = str_to_title(Institution),
         Institution = str_remove_all(Institution, "'"),
         STABBR = "",
         US_region = "",
         HBCU = "", HSI = "", MSI = "", `S&ER&D` = "", 
         SOCSC_RSD = "", STEM_RSD = "", LANDGRNT = "",	
         MEDICAL = "",	TRIBAL = "", WOMENS = "") %>% 
  rename(NAME = Institution)

#Merge US region w/ carn data ----
region_carnegie_join <- left_join(clean_carn_data, us_regions, 
                      by = c("STABBR" = "State_abbvr")) %>% 
  #select(-STABBR) %>% 
  distinct() %>% 
  mutate(PUI_RI = if_else(DOCRSDEG >= 21, "RI", "PUI")) %>% #determine ri vs pui status
  rename(US_region = Region, State_Providence = State_name) %>% 
  mutate(Country = "USA", 
         Other_inst_type = "US college or university") %>% 
  select(-DOCRSDEG) %>% 
  rbind(., non_carn_unis) %>% 
  rowid_to_column(var = "inst_id")

#Join data set w/ Carnegie/region data----
matched_survey_inst <- left_join(clean_surv_inst, all_matches, 
                                  by = "inst_name") %>% 
  #filter(!is.na(NAME)) %>% 
  left_join(., region_carnegie_join, by = "NAME") %>% 
  mutate(world_region = map(Country, get_world_region),
         world_region = unlist(world_region)) %>% 
  distinct()

#get unique inst identifiers
all_inst_data <- matched_survey_inst %>% 
  mutate(inst_id = as.character(inst_id),
         inst_id = if_else(is.na(inst_id), "0", inst_id)) %>% 
  select(-inst_name, -NAME, -STABBR, -State_Providence, -Country)
  
#save final matched dataset----
write_csv(all_inst_data, paste0("output/final_survey_inst_data_2019-2022_", 
                                Sys.Date(),".csv"))

