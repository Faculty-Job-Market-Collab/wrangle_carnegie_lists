#from load_data.R

all_survey_inst <- data %>% 
  select(contains("institution")) %>% 
  select(-num_institution_contacted) %>% 
  unite("inst", sep = ";") %>% 
  pull(inst) %>% 
  str_split(pattern = ";") %>% unlist() %>% tibble()

addl_split_list <- c(1690, 1621, 4035, 1605, 1564,3958, 4068, 4023, 4024, 3817, 3786, 
                     3787, 1530, 1531, 1475, 3636, 3786, 3530, 3426, 3433, 3216, 3277, 
                     3172, 3147, 3148, 2803, 2756, 2219, 1834, 1744, 1780, 1741, 1694, 
                     1387, 1367, 1308, 1304, 1303, 1094, 1077, 1078, 1036, 918, 140, 878,
                     809, 802, 801, 800, 799, 596, 599, 493, 461, 411, 378, 299, 191, 184, 
                     183, 153, 156)

further_split <- all_survey_inst %>% 
  slice(addl_split_list) %>% 
  pull() %>% 
  str_split(pattern = ",") %>% 
  unlist() %>% unique() %>% 
  tibble()

clean_survey_inst <- all_survey_inst %>% 
  slice(-addl_split_list) %>% 
  bind_rows(further_split) %>% 
  rename(inst = ".") %>% 
  mutate(inst = str_replace(inst, pattern = "\\(.+", replacement = "")) %>%
  filter(str_detect(inst, pattern = "post|Post") == FALSE) %>% 
  mutate(inst = str_to_title(inst),
         inst = str_trim(inst)) %>% 
  distinct() %>% 
  filter(str_detect(inst, pattern = "^[:digit:]|-") == FALSE)

herc_inst <- clean_herc_data %>% select(EmployerName) %>% 
  rename("inst" = EmployerName) %>% 
  distinct()

full_uni_list <- bind_rows(clean_survey_inst, herc_inst) %>% 
  mutate(inst = str_replace(inst, "(?<=\\sU).+[:space:]", " "),
         inst = str_replace(inst, "(?<=U)ni.+$", ""), 
         inst = str_replace_all(inst, pattern = "&", replacement = "and"),
         inst = str_to_title(inst)) %>% 
  filter(str_detect(inst, "N/A|Note|Interview|Cancel") == FALSE) %>% 
  filter(str_length(inst) >= 3) %>% 
  distinct() %>% 
  mutate(inst_type = "",
         inst_country = "",
         inst_state_providence = "")

write_csv(full_uni_list, "data/uni_list.csv")
