library(janitor)
library(dplyr)
library(tidyverse)
library(stringr)

#importing flat file as and reading it as a table
data <- read.csv("https://raw.githubusercontent.com/torkar/dat321/master/data_autumn2020.csv", sep=";", stringsAsFactors = FALSE)
min(data$tp)
max(data$tp)
min(data$technique)
max(data$technique)
#glimpse(data)


#assign data to another variable to be cleaned
clean <- clean_names(data)
colnames(clean)


#use tabyl function to check each column and content
tabyl(clean, technique)

#use str_replace function to replace 0T with OT
#tabyl(clean, technique)
clean$technique = str_replace(clean$technique, "0T", "OT")


#assign the cleaned data to another variable
glimpse(clean)

#basic statistical analysis on data
min(clean$tp)
max(clean$tp)
min(clean$subject)
max(clean$subject)
mean(clean$tp)
median(clean$tp)
mean(clean$subject)
median(clean$subject)
min(clean$technique)
max(clean$technique)
cor(clean$category, clean$tp, method = "pearson")
mean(clean$category)
mean(clean$tp)
sd(clean$tp)
