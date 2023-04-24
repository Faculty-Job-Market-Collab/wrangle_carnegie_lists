#import US region data set----
us_regions <- read_csv("data/us_region_list.csv")

#US region-related functions & code----
#generate state lists based on the US region
get_us_region_list <- function(x){
  
  state_list <- us_region_prep %>% 
    filter(Region == x) %>% 
    pull(state) %>% 
    str_c(collapse = "(?=\\b)|")
  
  return(state_list)
}

#prep US region data for get_us_region_list
us_region_prep <- us_regions %>% 
  mutate(State_abbvr_TC = str_to_title(State_abbvr)) %>% 
  unite(state, c("State_name", "State_abbvr", "State_abbvr_TC"), 
        sep = "(?=\\b)|")

#make lists for each US region
SE_list <- get_us_region_list("Southeast")

NC_list <- get_us_region_list("Noncontiguous")

SW_list <- get_us_region_list("Southwest")

Pacific_list <- get_us_region_list("Pacific")

RM_list <- get_us_region_list("Rocky Mountains")

MW_list <- get_us_region_list("Midwest")

NE_list <- get_us_region_list("Northeast")

#function to identify US region from state name & abbreviations
get_us_region <- function(x, y = NULL){
  
  region <- case_when(
    str_detect(x, SE_list) ~ "Southeast",
    str_detect(x, NC_list) ~ "Noncontiguous",
    str_detect(x, SW_list) ~ "Southwest",
    str_detect(x, Pacific_list) ~ "Pacific",
    str_detect(x, RM_list) ~ "Rocky Mountains",
    str_detect(x, MW_list) ~ "Midwest",
    str_detect(x, NE_list) ~ "Northeast"
  )
  
  if(is.null(y)){
    
    return(region)
    
  }else{
    
    check_region <- if_else(is.na(region), y, region)
    
    return(check_region)
  }
}

#world region related functions & code----
AFR <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", 
         "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", 
         "Ivory Coast", "Democratic Republic of the Congo", "Equatorial Guinea", 
         "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", 
         "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", 
         "Mali", "Mauritania", "Mauritius", "Mozambique", "Namibia", "Niger", 
         "Nigeria", "Republic of the Congo", "Rwanda", "Sao Tome and Principe",
         "Sa?o Tome? and Pri?ncipe","Senegal", "Seychelles", "Sierra Leone", 
         "Somalia", "South Africa", "South Sudan", "Eswatini", "Togo", "Uganda", 
         "Tanzania", "Zambia", "Zimbabwe", "Cote de Ivoire", "Eswatini")

AMR <- c("Antigua and Barbuda", "Argentina", "Bahamas", "Barbados", "Belize", 
         "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Cuba", 
         "Dominica", "Dominican Republic", "Ecuador", "El Salvador", "Grenada", 
         "Guatemala", "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico", 
         "Nicaragua", "Panama", "Paraguay", "Peru", "Saint Kitts and Nevis", 
         "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", 
         "Trinidad and Tobago", "Uruguay", "Venezuela")

SEAR <- c("Bangladesh", "Bhutan", "North Korea", "India", "Indonesia", 
          "Maldives", "Myanmar", "Nepal", "Sri Lanka", "Thailand", 
          "Timor-Leste")

EUR <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", 
         "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", 
         "Czech Republic", "Denmark", "Estonia", "Finland", "France", #"Georgia", 
         "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", 
         "Kazakhstan", "Kyrgyzstan", "Latvia", "Lithuania", "Luxembourg", 
         "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", 
         "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", 
         "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", 
         "Switzerland", "Tajikistan", "Turkey", "Turkmenistan", "Ukraine", 
         "United Kingdom", "Uzbekistan")

EMR <- c("Afghanistan", "Bahrain", "Djibouti", "Egypt", "Iran", "Iraq", 
         "Jordan", "Kuwait", "Lebanon", "Libya", "Morocco", "Oman", "Pakistan", 
         "Palestine", "Qatar", "Saudi Arabia", "Somalia", "Sudan", "Syria", 
         "Tunisia", "United Arab Emirates", "Yemen")

WPR <- c("Australia", "Brunei", "Cambodia", "China", "Cook Islands", "Fiji", 
         "Japan", "Kiribati", "Laos", "Malaysia", "Marshall Islands", 
         "Micronesia", "Mongolia", "Nauru", "New Zealand", "Niue", "Palau", 
         "Papua New Guinea", "Philippines", "Samoa", "Singapore", 
         "Solomon Islands", "South Korea", "Taiwan", "Tonga", "Tuvalu", 
         "Vanuatu", "Vietnam", "Hong Kong")

#function to identify world regions from country names
get_world_region <- function(x, y = NULL){
  region <- case_when(
    x %in% AFR ~ "African",
    x %in% AMR ~ "The Americas",
    x %in% SEAR ~ "South-East Asian",
    x %in% EUR ~ "European",
    x %in% EMR ~ "Eastern Mediterranean",
    x %in% WPR ~ "Western Pacific",
    x == "Canada" | x == "canada" ~ "Canada",
    x == "USA" | x == "Nationwide" ~ "USA"
  )
  
  if(is.null(y)){
    
    return(region)
    
  }else{
    
    check_region <- if_else(is.na(region), y, region)
    
    return(check_region)
  }
  
}

#test <- herc_matches %>% select(EmployerName, LocationDisplay, Country) %>% 
#  distinct() %>% 
#  head(n = 1000)
#
#test_region <- test %>% 
#  mutate(LocationDisplay = map(LocationDisplay, function(x){str_remove(x, ".*, |\\.")}),
#         LocationDisplay = unlist(LocationDisplay),
#         US_region = map(EmployerName, get_us_region),
#         US_region = unlist(US_region),
#         US_region = map2(LocationDisplay, US_region, get_us_region),
#         US_region = unlist(US_region),
#         Country = if_else(!is.na(US_region), "USA", Country),
#         world_region = map(Country, get_world_region),
#         world_region = unlist(world_region),
#         world_region = map2(LocationDisplay, world_region, get_world_region),
#         world_region = unlist(world_region))
