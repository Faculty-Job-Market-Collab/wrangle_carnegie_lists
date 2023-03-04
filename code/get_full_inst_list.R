library(tidyverse) #for data wrangling
library(data.table) #for setnames()

#correllary to %in% function
`%not_in%` <- Negate(`%in%`)

#load full data files
data_files <- c("data/2019-2020_raw_job_survey_data.csv",
                "data/2020-2021_raw_job_survey_data.csv",
                "data/2021-2022_raw_job_survey_data.csv")

raw_data <- map(data_files, read_csv) #load data files

#get questions & column names
q_list_19 <- read_csv("data/question_legend_19.csv") #csv of q numbers, full q, & column names for 2019-2020 data

q_list_20 <- read_csv("data/question_legend_20-22.csv") #csv of q numbers, full q, & column names for 2020-2022 data

q_num_19 <- q_list_19 %>% pull(Q_number)#list of q numbers for 2019-2020

q_num_20 <- q_list_20 %>% pull(Q_number)#list of q numbers for 2020-2022

q_data_19 <- q_list_19 %>% pull(Data) #list of column names for 2019-2020

q_data_20 <- q_list_20 %>% pull(Data) #list of column names for 2020-2022

#drop extra columns & fix data column names
data_19 <- raw_data[[1]][-c(1,2),] %>% #drop non-data rows
  select(-(1:17)) #drop unnecessary data columns

setnames(data_19, old = q_num_19, new = q_data_19) #rename columns

data_20 <- raw_data[[2]][-c(1,2),] %>% #drop non-data rows
  select(-(1:17)) #drop unnecessary data columns

setnames(data_20, old = q_num_20, new = q_data_20) #rename columns

data_21 <- raw_data[[3]][-c(1,2),] %>% #drop non-data rows
  select(-(1:17)) #drop unnecessary data columns

setnames(data_21, old = q_num_20, new = q_data_20) #rename columns

#collect institution names
data_19_inst <- data_19 %>% 
  select(contains("instit")) %>% 
  select(-num_institution_contacted) %>% 
  mutate(survey_year = "2019-2020") %>% 
  rowid_to_column("id")

data_20_inst <- data_20 %>% 
  select(contains("instit")) %>% 
  select(-num_institution_contacted) %>% 
  mutate(survey_year = "2020-2021") %>% 
  rowid_to_column("id")

data_21_inst <- data_21 %>% 
  select(contains("instit")) %>% 
  select(-num_institution_contacted) %>% 
  mutate(survey_year = "2021-2022") %>% 
  rowid_to_column("id")

#separate lists of institutions separated by ";"
raw_inst_list <- rbind(data_19_inst, data_20_inst, data_21_inst) %>% 
  gather(on_site_institutions:postdoc_institution, 
         key = "inst_type", value = "inst_name") %>% 
  distinct() %>% 
  filter(!is.na(inst_name)) %>% 
  mutate(inst_name = str_to_title(inst_name)) %>% 
  separate("inst_name", c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", 
                          "x9", "x10", "x11", "x12", "x13", "x14", "x15", 
                          "x16", "x17", "x18", "x19", "x20", "x21", "x22"), 
           sep = ";", extra = "merge") %>% 
  gather(x1:x22, key = "x_col", value = "inst_name") %>% 
  select(-x_col) %>% 
  filter(!is.na("inst_name") & str_detect(inst_name, "[[:digit:]]") == FALSE)

#separate institution lists by ","
comma_drop_list <- c(9, 14, 20, 24, 32, 36, 39, 42, 44, 48, 53, 58, 
                     67, 71, 77, 79, 81:83, 87, 89, 93, 95, 100, 103, 
                     106:108, 111, 114:193, 195, 197:200, 203, 205, 206,
                     213, 214, 217, 219, 220, 223:226, 230, 236, 
                     240:244, 246, 247, 249:252, 255:259, 261:265,
                     270:275, 277:279, 281:285, 287, 289, 291:297, 
                     299:310)

comma_inst_list <- raw_inst_list %>%
  filter(str_count(inst_name, "Univ") >= 2 | str_detect(inst_name, "in Can|,") == TRUE) %>% #find remaining institution lists
  #filter(inst_type != "phd_institution") %>% #only 1 phd inst should be named, drop these entries
  slice(-comma_drop_list) #drop names of single institutions w/ a comma

comma_names <- comma_inst_list %>% pull(inst_name) #full list of items to split by comma

comma_split_data <- comma_inst_list %>% #split inst names by comma
  mutate(inst_name = str_replace(inst_name, 
                                 "University - University", 
                                 "University, University")) %>% 
  separate("inst_name", c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9"), 
           sep = ",", extra = "merge") %>% #max list of 8
  gather(x1:x9, key = "x_col", value = "inst_name") %>% 
  select(-x_col) %>% 
  filter(!is.na(inst_name))

#list of unis from survey data - separated into individual rows
split_inst_list <- rbind(raw_inst_list, comma_split_data) %>% 
  filter(inst_name %not_in% comma_names) #remove items not split by comma

write_csv(split_inst_list, paste0("output/full_inst_list_2019-2022_", 
                                  Sys.Date(), ".csv"))