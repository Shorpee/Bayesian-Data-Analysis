library(janitor)
library(dplyr)
library(tidyverse)

#importing flat file as and reading it as a table
data <- read.csv("https://raw.githubusercontent.com/torkar/dat321/master/data_autumn2020.csv", sep=";", stringsAsFactors = FALSE)

#assign data to another variable to be cleaned
clean <- clean_names(data)
colnames(clean)

#use tabyl function to check each column and content
tabyl(clean, technique)

#use gsub function to replace 0T with OT
clean <- data.frame(lapply(data, function(x) {
  gsub("0T", "OT", x)
}))
tabyl(clean, technique)

#assign the cleaned data to another variable
d <- clean
