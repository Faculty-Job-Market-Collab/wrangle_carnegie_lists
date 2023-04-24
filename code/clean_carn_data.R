#import carnegie data----
raw_carnegie_data <- read_excel("data/CCIHE2018-PublicData.xlsx", 
                                sheet = 4, col_names = TRUE) %>% 
  mutate(NAME = str_to_title(NAME),
         NAME = str_replace(NAME, "Saint |^St ", "St "),
         NAME = str_replace_all(NAME, "&", "And"),
         NAME = str_replace_all(NAME, "Aandm|AAndM", "A And M"),
         NAME = str_replace_all(NAME, "Aandt|AAndT", "A And T"),
         NAME = str_replace(NAME, " At | In |/", " "))

carnegie_data <- raw_carnegie_data %>% 
  mutate(
    NAME = str_replace_all(NAME, "-", " "),
    NAME = str_remove(NAME, "Penn State "),
    NAME = str_remove_all(NAME, "The |Campus"),
    NAME = map(NAME, replace_uny),
    NAME = unlist(NAME),
    NAME = str_squish(NAME),
    NAME = str_replace_all(NAME, ",|'|\\.", ""))

#raw Carnegie inst names -- needed for the multi-campus lists
raw_carnegie_inst_names <- raw_carnegie_data %>% select(NAME)

#clean Carnegie inst names
carnegie_inst_names <- carnegie_data %>% select(NAME)

#Institution names for both colleges & universities (e.g., Temple College & Temple University)
drop_inst <- c("Smith|Centre |Rochester.*|Princeton|Louisiana|Denison|Cabrini|Allen|Lawrence|Princeton Theological|National|American|Allen Institute|Rochester Ins.*|Connecticut|Scripps|Heidelberg| Northwestern|Cornell|Boston|Florida|Wesleyan|Georgetown|Northwest|Temple|Pacific|Seattle|Illinois") 

#colleges: generate unique lists of college names to compare against responses using stringr----
mult_loc_college_last_list <- raw_carnegie_inst_names %>% 
  filter(str_detect(NAME, "College-")) %>% #identify colleges w/ multiple sites ("-" precedes location)
  mutate(NAME = str_replace(NAME, " College", ""), 
         NAME = str_replace(NAME, "-", " ")) %>% #remove punctuation
  pull(NAME) %>% str_c(collapse = "|") #compile list from column

mult_loc_coll_of_list <- raw_carnegie_inst_names %>% #Inst w/ "College Of" names & multiple sites
  filter(str_detect(NAME, "^College Of") == TRUE & str_detect(NAME, "-") == TRUE) %>% #identify multiple sites ("-" precedes location)
  mutate(NAME = str_replace(NAME, " College Of", ""), #ensure comparison at beginning of string 
         NAME = str_replace(NAME, "-", " ")) %>% #remove punctuation
  pull(NAME) %>% str_c(collapse = "|") #compile list from column

state_college_list <- carnegie_inst_names %>% #List of state college names
  filter(str_detect(NAME, "State College$") == TRUE) %>% 
  mutate(#NAME = str_replace(NAME, "(?<=State) College$", ""),
         NAME = str_replace(NAME, "(?<=State) College$", "$") #ensure comparison at end of string
  )
#create search list
x_college_list <- carnegie_inst_names %>% 
  filter(str_count(NAME, " ") == 1) %>% 
  filter(str_detect(NAME, "College") == TRUE) %>% 
  filter(str_detect(NAME, drop_inst) == FALSE) %>% #remove names that could be uni or college
  mutate(NAME = paste0("^", str_replace(NAME, " College", ""))) %>% #signals start of string
  mutate(NAME = str_replace(NAME, " College", "")) %>% 
  rbind(., state_college_list) %>% 
  pull(NAME) %>% unique() %>% str_c(collapse = "|") 

#unis: generate several unique list of universities----
mult_loc_uni_last_list <- raw_carnegie_inst_names %>%
  filter(str_detect(NAME, "University-") == TRUE) %>% 
  mutate(NAME = str_replace(NAME, " University", ""),
         NAME = str_replace(NAME, "-", " ")
         #NAME = str_replace(NAME, "-", "-?")
  ) %>% 
  pull(NAME) %>% str_c(collapse = "|")

mult_loc_uni_of_list <- raw_carnegie_inst_names %>% 
  filter(str_detect(NAME, "^University Of") & str_detect(NAME, "-")) %>% 
  mutate(NAME = str_replace(NAME, "University Of ", ""),
         #NAME = str_replace(NAME, "-", "-?")
         NAME = str_replace(NAME, "-", " ")
  ) %>% 
  pull(NAME) %>% str_c(collapse = "|")

#generate a unique list of universities
state_uni_list <- carnegie_inst_names %>% 
  filter(str_detect(NAME, "State University$") == TRUE) %>% 
  mutate(#NAME = str_replace(NAME, "(?<=State) University$", "")
         NAME = str_replace(NAME, "(?<=State) University$", "$")
  )

x_uni_list <- carnegie_inst_names %>% 
  filter(str_count(NAME, " ")==1) %>% 
  filter(str_detect(NAME, "University") == TRUE) %>% 
  filter(str_detect(NAME, drop_inst) == FALSE) %>% 
  mutate(NAME = paste0("^", str_replace(NAME, " University", ""))) %>% 
  #mutate(NAME = str_replace(NAME, " University", "")) %>% 
  rbind(., state_uni_list) %>% 
  pull(NAME) %>% str_c(collapse = "|")

mult_loc_uni_of_list <- str_c(mult_loc_uni_of_list, 
                               "|Northern Iowa")

#option to save clean data----
write_csv(carnegie_data, paste0("output/cleaned_carnegie_data_", 
                                Sys.Date(), ".csv"))
