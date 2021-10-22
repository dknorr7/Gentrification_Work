# Manage Historical Places data retrieved from : https://www.nps.gov/nr/research/index.htm 

library(pacman)
p_load(tidyverse, ggplot2, readxl)

raw_data <- read_xlsx("national-register-listed-properties-20171205.xlsx") %>% filter(State == "TN" & County == "Davidson")

write.csv(raw_data, "Places2GeoCode.csv")

#tons of address errors