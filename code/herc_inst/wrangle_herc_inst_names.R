
source("code/herc_analysis/load_herc_data.R")

source("code/univ_categories/get_carn_data.R")

source("code/univ_categories/us_region_functions.R")

source("code/univ_categories/wrangle_uni_functions.R")

fix_herc_inst <- function(x){
  case_when(
    str_detect(x, "Lone") ~ "Lone Star College System",
    str_detect(x, "Vanderbilt|Tufts|Rush") ~ str_remove(x, " Medical Center"),
    str_detect(x, "^Westchester") ~ paste0("Suny ", x),
    str_detect(x, "Tarrant|Cuyahoga") ~ paste0(x, " District"),
    str_detect(x, "Contra Costa") ~ "Contra Costa College",
    str_detect(x, "Miracosta") ~ str_remove(x, " Community"),
    str_detect(x, "Fairleigh") ~ paste0(x, " Metropolitan"),
    str_detect(x, "Maryville U") ~ paste0(x, "  Of St Louis"),
    str_detect(x, "Bronx|Of Manhattan|Laguardia|Queensborough|Kingsborough|Staten Island|Lehman|Evers|Brooklyn|Hunter") ~ paste0("Cuny ", x),
    str_detect(x, "Oswego") ~ "Suny College Oswego",
    str_detect(x, "Hobart And") ~ "Hobart William Smith Colleges",
    str_detect(x, "Alfred State") ~ "Suny College Of Technology Alfred",
    str_detect(x, "Tennessee Institute Of Agri") ~ "University Of Tennessee Knoxville",
    str_detect(x, "Nazareth") ~ "Nazareth College",
    str_detect(x, "Concordia") ~ "Concordia College Moorhead",
    str_detect(x, "Rosalind") ~ "Rosalind Franklin University Of Medicine And Science",
    str_detect(x, "Temple U") ~ "Temple University",
    str_detect(x, "Maritime") ~ "California State University Maritime Academy",
    str_detect(x, "Des Moines") ~ "Des Moines University Osteopathic Medical Center",
    str_detect(x, "Christie") ~ str_replace(x, "Christie", "Christi"),
    str_detect(x, "Golden Gate") ~ "Golden Gate University San Francisco",
    str_detect(x, "Of South$") ~ "Sewanee University Of South",
    str_detect(x, "Of Memphis") ~ "University Of Memphis",
    str_detect(x, "Purdue") ~ "Purdue University Main",
    str_detect(x, "Brockport|Geneseo") ~ str_replace(x, "Suny", "Suny College"),
    str_detect(x, "Pierce College") ~ "Pierce College Puyallup",
    str_detect(x, "Hostos") ~ "Cuny Hostos Community College",
    str_detect(x, "Guttman") ~ "Stella And Charles Guttman Community College",
    str_detect(x, "Grossmont") ~ "Grossmont College",
    str_detect(x, "Harper") ~ "William Rainey Harper College",
    str_detect(x, "Baruch") ~ "Cuny Bernard M Baruch College",
    str_detect(x, "John Jay") ~ "Cuny John Jay College Of Criminal Justice",
    str_detect(x, "City College|Nyc College") ~ "Cuny New York City College Of Technology",
    str_detect(x, "York College Of City") ~ "Cuny York College",
    str_detect(x, "Permian") ~ "University Of Texas Of Permian Basin",
    str_detect(x, "Tennessee Tech") ~ "Tennessee Technological University",
    str_detect(x, "Nc State") ~ "North Carolina State",
    str_detect(x, "Columbia University") ~ "Columbia University City Of New York",
    str_detect(x, "Unc ") ~ str_replace(x, "Unc", "University of North Carolina"),
    str_detect(x, "Ut Health") ~ str_replace(x, "Ut", "University of Tennessee"),
    str_detect(x, "Geneseo") ~ "Suny College Geneseo",
    str_detect(x, "Virginia Tech") ~ "Virginia Polytechnic Institute And State University",
    str_detect(x, "Minneapolis College") ~ "Minneapolis College Of Art And Design",
    str_detect(x, "Lewis And Clark") ~ "Lewis And Clark College",
    str_detect(x, "Southeast Technical") ~ "Minnesota State College Southeast",
    str_detect(x, "^College Brockport") ~ "Suny College Brockport",
    str_detect(x, "North Carolina State$") ~ "North Carolina State University Raleigh",
    str_detect(x, "Lasell") ~ "Lasell College",
    TRUE ~ x
  )
}

drop_phrases <- c("Texarkana|Dept Of Psychology|(?<=Thomas) Houston|Faculty Of Computer Science| Center For Advancement| Fraud|Kctcs|Space Science And Engineering Center |Penn Medicine")

#Prep HERC job data to join-----

herc_inst_list <- clean_herc_data %>% 
  distinct() %>% 
  mutate(EmployerName = str_to_title(EmployerName),
         EmployerName = str_replace(EmployerName, "Saint |^St ", "St "),
         EmployerName = str_replace_all(EmployerName, "&", "And"),
         EmployerName = str_replace_all(EmployerName, "Aandm|AAndM", "A And M"),
         EmployerName = str_replace_all(EmployerName, "Aandt|AAndT", "A And T"),
         EmployerName = str_replace(EmployerName, " At | In |/", " "),
         EmployerName = str_remove_all(EmployerName, ",|The |\\.|Campus|Tuscaloosa|\\(.+| (?=,)|'"),
         EmployerName = str_replace_all(EmployerName, "-", " "),
         EmployerName = str_remove_all(EmployerName, drop_phrases),
         EmployerName = str_trim(EmployerName),
         EmployerName = map(EmployerName, replace_uny),
         EmployerName = unlist(EmployerName),
         EmployerName = str_squish(EmployerName),
         EmployerName = map(EmployerName, fix_campus),
         EmployerName = unlist(EmployerName),
         EmployerName = map(EmployerName, fix_herc_inst),
         EmployerName = unlist(EmployerName),
         Description = unlist(Description)
         )

#join herc data w/ carn data----

herc_join <- herc_inst_list %>% 
  mutate(EmployerName = str_to_title(EmployerName),
         EmployerName = str_squish(EmployerName)) %>% 
  #mutate(EmployerName = map(EmployerName, fix_herc_inst),
  #       EmployerName = unlist(EmployerName)) %>% 
  left_join(., pui_carn, 
                       by = c("EmployerName" = "NAME"), keep = TRUE) %>% 
  distinct()

#separate matches and identify non-matches----

all_herc_matches <- herc_join %>% 
  filter(EmployerName == NAME) %>% 
  select(-NAME) %>% 
  distinct()

no_carn_herc_inst <- anti_join(herc_join, all_herc_matches, 
                               by = c("EmployerName")) %>% 
  mutate(EmployerName = map(EmployerName, fix_non_carn),
         EmployerName = unlist(EmployerName)) %>% 
  select(EmployerName:MonthPosted)

#join non-matches w/ hand-compiled non-carn inst----

non_carn_herc_join <- left_join(no_carn_herc_inst, non_carn_unis, 
                           by = c("EmployerName" = "Institution")) %>% 
  left_join(., non_carn_unis, 
            by = c("EmployerName" = "Full Institution Name")) %>% 
  mutate(State_Providence = if_else(!is.na(State_Providence.x), State_Providence.x, State_Providence.y),
         Country = if_else(!is.na(Country.x), Country.x, Country.y),
         PUI_RI = if_else(!is.na(PUI_RI.x), PUI_RI.x, PUI_RI.y),
         Other_inst_type = if_else(!is.na(Other_inst_type.x), Other_inst_type.x, Other_inst_type.y)) %>% 
  select(-contains(".x"), -contains(".y"), -contains("Institution"))

#join herc data together & wrangle for full data set----
herc_matches <- bind_rows(all_herc_matches, non_carn_herc_join) %>% 
  distinct() 

write_csv(herc_matches, "data/herc_intermediate.csv")
