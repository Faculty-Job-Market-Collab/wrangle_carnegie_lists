#requires "wrangle_herc_inst_names.R" to be completed and saved as a csv

library(tidyverse)

#get us and world region data and functions
source("code/univ_categories/us_region_functions.R")

#function to use the location data to identify the US & world regions
get_herc_location <- function(df){
  
  df_location <- df %>% 
    mutate(LocationDisplay = map(LocationDisplay, function(x){str_remove(x, ".*, |\\.")}),
           LocationDisplay = unlist(LocationDisplay),
           US_region = map(EmployerName, get_us_region),
           US_region = unlist(US_region),
           US_region = map2(STABBR, US_region, get_us_region),
           US_region = unlist(US_region),
           US_region = map2(LocationDisplay, US_region, get_us_region),
           US_region = unlist(US_region),
           Country = case_when(
             !is.na(US_region) ~ "USA", 
             TRUE ~ Country),
           world_region = map(LocationDisplay, get_world_region),
           world_region = unlist(world_region),
           world_region = map2(Country, world_region, get_world_region),
           world_region = unlist(world_region),
           Description = unlist(Description)) %>% 
    select(-LocationDisplay, -Country, -STABBR, -State_Providence)
  
  return(df_location)
}

get_herc_countries <- function(x){
  case_when(
  str_detect(x, "Abidjan") ~ "Cote de Ivoire",
  str_detect(x, "Abu Dhabi|UAE|United Arab") ~ "United Arab Emirates",
  str_detect(x, "Aukland") ~ "New Zealand",
  str_detect(x, "Buenos aires") ~ "Argentina",
  str_detect(x, "Cape Town|AFRICA|Africa") ~ "South Africa",
  str_detect(x, "CERN|Geneva") ~ "Switzerland",
  str_detect(x, "Doha|Quatar") ~ "Quatar",
  str_detect(x, "Canada|CAN") ~ "Canada",
  str_detect(x, "France") ~ "France",
  str_detect(x, "Germany") ~ "Germany",
  str_detect(x, "Ethiopia") ~ "Ethiopia",
  str_detect(x, "Eswatini") ~ "Eswatini",
  str_detect(x, "Grenada") ~ "Grenada",
  str_detect(x, "China|Shanghai|Weihai") ~ "China",
  str_detect(x, "Kingdom|UK|London|Oxford") ~ "United Kingdom",
  str_detect(x, "Liberia") ~ "Liberia",
  str_detect(x, "Nairobi|Kenya") ~ "Kenya",
  str_detect(x, "Rwanda") ~ "Rwanda",
  str_detect(x, "Sydney") ~ "Australia",
  str_detect(x, "Oesterr") ~ "Austria",
  str_detect(x, "Kathmandu") ~ "Nepal",
  str_detect(x, "Singapore") ~ "Singapore",
  str_detect(x, "Palestine") ~ "Palestine",
  str_detect(x, "Nigeria") ~ "Nigeria",
  str_detect(x, "India(?!.)") ~ "India",
  str_detect(x, "Israel") ~ "Israel",
  str_detect(x, "Saudi") ~ "Saudi Arabia",
  str_detect(x, "Kinshasa") ~ "Democratic Republic of the Congo",
  str_detect(x, "Madrid") ~ "Spain",
  str_detect(x, "Kazakhstan") ~ "Kazakhstan",
  str_detect(x, "outside|overseas") ~ "Unknown, not in the USA",
  TRUE ~ "USA")
  }

#pull in data
herc_matches <- read_csv("data/herc_intermediate.csv") %>% 
  mutate(LocationDisplay = map(LocationDisplay, function(x){str_remove(x, ".*, |\\.")}),
         LocationDisplay = unlist(LocationDisplay)) %>% 
  select(-Country)

#pull unique instances of locations from herc data
unique_locations <- herc_matches %>% 
  select(EmployerName, LocationDisplay, STABBR, State_Providence) %>% 
  distinct()

herc_locations_cleaned <- unique_locations %>% 
  mutate(US_region = map(EmployerName, get_us_region),
         US_region = unlist(US_region),
         US_region = map2(STABBR, US_region, get_us_region),
         US_region = unlist(US_region),
         US_region = map2(LocationDisplay, US_region, get_us_region),
         US_region = unlist(US_region)) %>% 
  mutate(Country = map(LocationDisplay, get_herc_countries),
         Country = unlist(Country),
         US_region = case_when(Country != "USA" ~ "Null", TRUE ~ US_region),
         world_region = map(LocationDisplay, get_world_region),
         world_region = unlist(world_region),
         world_region = map2(Country, world_region, get_world_region),
         world_region = unlist(world_region)) %>% 
  select(-STABBR, -State_Providence)

#need to left join w/ herc_matches & unlist the description
full_herc_data <- left_join(herc_matches, herc_locations_cleaned, 
                            by = c("EmployerName", "LocationDisplay")) %>% 
  select(-STABBR, - State_Providence) %>% 
  distinct() %>% 
  mutate(Description = unlist(Description))

##generate a list of equally-sized dfs to parallelize---- figured out to do joins instead of in parallel
#num_dfs = 32
#
#clean_herc_list <- herc_matches %>% 
#  group_by((row_number()-1) %/% (n()/num_dfs)) %>% nest() %>% pull(data)
#
##run in parallel----
#registerDoParallel()
#herc_matches_world <- foreach (i = clean_herc_list, .combine = rbind) %dopar% {
#  get_herc_location(i)
#}
#
#stopImplicitCluster()

#generate full dataset----
write_csv(full_herc_data, "data/final_herc_clean_data.csv")
