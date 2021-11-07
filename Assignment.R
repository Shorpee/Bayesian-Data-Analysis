library(janitor)
library(dplyr)
library(tidyverse)
library(stringr)
library(rethinking)

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
mean(clean$tp)
median(clean$tp)
var(clean$tp)
sd(clean$tp)

hist(clean$tp)
dens(clean)
quantile(clean, 0.8)
precis(clean)

#data-list for cleaned data 
dat <- list(
  tp = clean$tp,
  Tech = ifelse(clean$technique== "NT", 1L, 2L),
  Cat = ifelse(clean$category== "LE", 1L, 2L)
)
glimpse(dat)

#intercept only Poisson model
m_null <- ulam(
  alist(
    tp ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(0, .5)
  ),data = dat, chains = 4, cores = 4, iter = 2000 , log_lik = TRUE
)
precis(m_null)
extract.prior(m_null)
