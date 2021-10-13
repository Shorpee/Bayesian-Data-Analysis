library(janitor)
library(dplyr)
#importing flat file as and reading it as a table
data <- read.table("https://raw.githubusercontent.com/torkar/dat321/master/data_autumn2020.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)
clean <- clean_names(data)
colnames(clean)
tabyl(clean, technique)
tabyl(clean, subject)
tabyl(clean, category)