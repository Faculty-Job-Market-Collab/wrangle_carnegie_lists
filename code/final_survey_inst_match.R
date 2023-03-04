#read in data for setup----
#data and functions for US & world regions
source("code/get_regions.R")

source("code/get_final_match_functions.R")

#cleaned full Carnegie data set
clean_carn_data <- read_csv("data/clean_carnegie_data.csv") %>% 
  select(NAME, STABBR, DOCRSDEG, HBCU, HSI, MSI, `S&ER&D`, 
         SOCSC_RSD, STEM_RSD, LANDGRNT,	MEDICAL,	TRIBAL, WOMENS)

#cleaned list of unique respondent-provided institutions
clean_surv_inst <- read_csv("data/cleaned_survey_inst.csv")

#data set of unique respondent-provided institutions that matched Carnegie-listed instituttions
all_matches <- read_csv("data/carnegie_inst_matches.csv")

#data set of non-carn universities/institutions
non_carn_unis <- read_csv("data/non-carnegie_unis.csv") %>% ## Need to work on matching inst names for non-carn inst
  mutate(Institution = str_to_title(Institution),
         Institution = str_remove_all(Institution, "'"),
         `Full Institution Name` = str_remove_all(
           `Full Institution Name`, "'"),
         `Full Institution Name` = str_to_title(
           `Full Institution Name`))

#Merge US region w/ carn data ----
region_carnegie_join <- left_join(clean_carn_data, us_regions, 
                      by = c("STABBR" = "State_abbvr")) %>% 
  #select(-STABBR) %>% 
  distinct() %>% 
  mutate(PUI_RI = if_else(DOCRSDEG >= 21, "RI", "PUI")) #determine ri vs pui status

#Reduce Carnegie/region data using respondent-provided inst matches
short_carnegie_data <- all_matches %>% 
  select(-inst_name) %>% 
  left_join(., region_carnegie_join, by = c("NAME")) %>% 
  rename(US_region = Region, State_Providence = State_name) %>% 
  mutate(Country = "USA")

#Join data set w/ Carnegie/region data----
carn_survey_inst <- left_join(clean_surv_inst, all_matches, 
                                  by = "inst_name") %>% 
  filter(!is.na(NAME)) %>% 
  left_join(., short_carnegie_data, by = "NAME")

#identify & prep non-Carnegie institutions----
non_carn_survey_inst <- left_join(clean_surv_inst, all_matches, 
                     by = "inst_name") %>% 
  filter(is.na(NAME)) %>% 
  mutate(inst_name = map(inst_name, fix_non_carn),
         inst_name = unlist(inst_name))

#merge non-carn data w/ inst missing data----
non_carn_join <- left_join(non_carn_survey_inst, non_carn_unis, 
                           by = c("inst_name" = "Institution")) %>% 
  left_join(., non_carn_unis, 
            by = c("inst_name" = "Full Institution Name")) %>% 
  mutate(State_Providence = if_else(!is.na(State_Providence.x), State_Providence.x, State_Providence.y),
         Country = if_else(!is.na(Country.x), Country.x, Country.y),
         PUI_RI = if_else(!is.na(PUI_RI.x), PUI_RI.x, PUI_RI.y),
         Other_inst_type = if_else(!is.na(Other_inst_type.x), Other_inst_type.x, Other_inst_type.y)) %>% 
  select(-contains(".x"), -contains(".y")) %>% 
  left_join(., us_regions, by = c("State_Providence" = "State_abbvr")) %>% 
  select(-State_name) %>% 
  rename(US_region = Region, NAME = inst_name) %>% 
  mutate(world_region = map(Country, get_world_region),
         world_region = unlist(world_region))

all_inst_data <- bind_rows(carn_region_join, non_carn_join) %>% 
  mutate(world_region = if_else(is.na(world_region), Country, world_region))

#save final matched dataset----
write_csv(all_inst_data, "data/final_survey_inst_data_2019-2022.csv")

