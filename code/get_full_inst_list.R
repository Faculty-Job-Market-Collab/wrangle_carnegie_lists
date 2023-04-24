library(tidyverse) #for data wrangling

#corollary to %in% function
`%not_in%` <- Negate(`%in%`)

#load full data file----
raw_data <- read_csv("data/merged_data_19-22.csv") %>% 
  select(id, contains("instit")) %>% 
  select(-num_institution_contacted)

#separate lists of institutions separated by ";"----
raw_inst_list <- raw_data %>%  
  gather(2:6, key = "inst_type", value = "inst_name") %>% 
  distinct() %>%
  mutate(inst_name = if_else(is.na(inst_name), "NR", inst_name),#add placeholder for missing responses
         inst_name = str_to_title(inst_name)) %>% 
  separate("inst_name", c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", 
                          "x9", "x10", "x11", "x12", "x13", "x14", "x15", 
                          "x16", "x17", "x18", "x19", "x20", "x21", "x22",
                          "x23", "x24", "x25", "x26", "x27", "x28", "x29"), 
           sep = ";", extra = "merge") %>% 
  gather(x1:x29, key = "x_col", value = "inst_name") %>% 
  select(-x_col) %>% 
  filter(!is.na("inst_name"))# & str_detect(inst_name, "[[:digit:]]") == FALSE)

#separate institution lists by ","----
comma_drop_list <- c(12, 19, 23, 25, 29, 37, 40, 50, 55, 58, 59, 61,
                     64, 79, 80, 93, 94, 100, 116, 121, 127, 131, 139, 
                     143, 146, 149, 151, 155, 159, 161, 166, 175, 179, 
                     181, 184, 187, 189, 191, 194, 200, 204,
                     206, 211, 213, 215, 218:220, 223:225, 227, 
                     229:271, 273:275, 277:322, 324, 326, 327, 329:331,
                     334, 335, 337, 338, 345, 346, 349, 351:354,
                     357, 358, 360:363, 366, 367, 368, 374, 377, 379:388,
                     390:395, 398:404, 406:410, 412, 413,
                     415, 417:422, 424:433, 435, 436, 438, 439, 441:447,
                     449:459, 462, 464:470, 472, 473, 475:478, 
                     480, 481, 483:486, 488:499, 501:507, 509:522, 524:546
                     ) #entries with commas that are not lists

other_split_list <- c(75, 155, 177, 178, 184, 185, 
                      196, 205, 211, 219, 220)#lists split with punctuation other than commas or semicolons

comma_inst_list <- raw_inst_list %>%
  filter(str_count(inst_name, "Univ") >= 2 | str_detect(inst_name, "in Can|,|And") == TRUE | 
           str_detect(inst_name, "Uni.*Coll|Coll.*Uni")
         ) %>% #find remaining institution lists
  #filter(inst_type != "phd_institution") %>% #only 1 phd inst should be named, drop these entries
  slice(-comma_drop_list) %>% #drop names of single institutions w/ a comma
  slice(-other_split_list) 

comma_names <- comma_inst_list %>% pull(inst_name) #full list of items to split by comma

comma_split_data <- comma_inst_list %>% #split inst names by comma
  mutate(inst_name = str_replace(inst_name, 
                                 "University - University", 
                                 "University, University")) %>% 
  separate("inst_name", c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9",
                          "x10", "x11", "x12", "x13", "x14"), 
           sep = ",", extra = "merge") %>% #max list of 8
  gather(x1:x14, key = "x_col", value = "inst_name") %>% 
  select(-x_col) %>% 
  filter(!is.na(inst_name))

#split lists using other forms of punctuation----
other_inst_list <- raw_inst_list %>%
  filter(str_count(inst_name, "Univ") >= 2 | str_detect(inst_name, "in Can|,|And") == TRUE | 
           str_detect(inst_name, "Uni.*Coll|Coll.*Uni")) %>% #find remaining institution lists
  #filter(inst_type != "phd_institution") %>% #only 1 phd inst should be named, drop these entries
  slice(-comma_drop_list) %>% #drop names of single institutions w/ a comma
  slice(other_split_list)

other_names <- other_inst_list %>% pull(inst_name) #full list of items to split by comma

other_split_data <- other_inst_list %>% #split inst names by comma
  mutate(inst_name = str_replace(inst_name, 
                                 "University - University", 
                                 "University, University"),
         inst_name = str_replace(inst_name, 
                                 "Of Cincinnatil University Of ",
                                 "Of Cincinnati, University Of "),
         inst_name = str_remove_all(inst_name, 
                                 "Medschool,"),
         inst_name = str_replace(inst_name,
                                 " \\(.* Of Chicago\\)",
                                 ", University of Chicago"),
         inst_name = str_replace(inst_name,
                                 "1 Wayne.* India",
                                 "Wayne State University, National Chemical Laboratory India")) %>% 
  separate("inst_name", c("x1", "x2", "x3", "x4"), 
           sep = ",|\\.|:|And (?!Design)|\\(|\\)", extra = "merge") %>% #max list of 8
  gather(x1:x4, key = "x_col", value = "inst_name") %>% 
  select(-x_col) %>% 
  filter(!is.na(inst_name))

#repair missing data----

#list of unis from survey data separated into individual rows----
split_inst_list <- rbind(raw_inst_list, comma_split_data, other_split_data) %>% 
  filter(inst_name %not_in% comma_names) %>% #remove items not split by semicolon
  filter(inst_name %not_in% other_names) #remove items not split by semicolon or comma

#save final list as csv----
write_csv(split_inst_list, paste0("output/full_inst_list_2019-2022_", 
                                  Sys.Date(), ".csv"))