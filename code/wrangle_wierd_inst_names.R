#read in data for setup----
library(tidyverse)
library(readxl)

source("code/get_wrangle_uni_functions.R")

#import carnegie data----
source("code/clean_carn_data.R")

#cleaned full Carnegie data set
wierd_inst_data <- read_csv("output/wierd_inst_list2023-03-28.csv")


