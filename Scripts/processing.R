################################################# 
### Manipulation data
### Septiember 2021
### Francisco Villarroel
### Thesis Name: “Aves del mismo plumaje se emocionan juntas” Encuesta experimental sobre la 
### influencia de la homofília en la construcción de conductas políticas en el Chile Contemporáneo.
################################################

#Load Packages

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("tidyverse","dplyr","haven","ggplot2","readxl","summarytools", "patchwork")
ipak(packages)


## Load data ##


thesis <-read.csv("Data/InputData/Villarroel_Homophily_September 30, 2021_18.19.csv")

## SELECT VARIABLES ##  

thesis <-select(thesis, Age:SC0)

# Eliminate two rows

thesis <-thesis%>%
  slice(-c(1,2))

## Create three new variables: Homophily index, digital citizenship and political position

# Homophily Index

thesis<- thesis%>% 
  mutate_at(c(6:26), as.numeric)

thesis$count <-rowSums(thesis[6:12], na.rm = T)

thesis$HomoIndex <-ifelse(thesis$count<=39,0,1)


#Digital CitizenShip

thesis$DigiCount <-rowSums(thesis[13:26], na.rm = T)
thesis$DigitIndex <-ifelse(thesis$DigiCount<=56,0,1)

table(thesis$HomoIndex)
table(thesis$DigitIndex)

# Political position

table(thesis$X.E3T4b2)


## RENAME VARIABLEAS ##






