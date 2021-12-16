########################################################################################################
### Manipulation data                                                                               ####          
### Septiember 2021                                                                                 ####
### Francisco Villarroel                                                                            ####
### Thesis Name: “Aves del mismo plumaje se emocionan juntas” Encuesta experimental sobre la        ####
### influencia de la homofília en la construcción de conductas políticas en el Chile Contemporáneo. ####
########################################################################################################

#Load Packages

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("tidyverse","dplyr","haven","ggplot2","readxl","summarytools", "patchwork","stringr",
              "tidyr","kableExtra","psych", "MASS", "foreign", "data,table","gtools","lubridate","AER",
              "xtable","pBrackets","Hmisc","ri","ggpubr", "stargazer", "Rmisc","wesanderson")
ipak(packages)

#crosstab funtion
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")


## Load data ##

thesis <-read.csv("Data/InputData/test3.csv")

# select just the valid surveys


thesis <-thesis%>%
  slice(-c(1,2))

thesis <-filter(thesis, Consent=='1')%>%
  dplyr::filter(Finished=="1")

thesis <-dplyr::select(thesis, Age:SC0)


# Eliminate two rows

thesis <-thesis%>%
  slice(-c(69,71,72,73,74,75,76))


## Create three new variables: Homophily index, digital citizenship and political position

# Homophily Index

thesis<- thesis%>% 
  mutate_at(c(6:26), as.numeric)

thesis$count <-rowSums(thesis[6:12], na.rm = T)

thesis$HomoIndex <-ifelse(thesis$count<=39,0,1)


#Digital CitizenShip

thesis$DigiCount <-rowSums(thesis[13:26], na.rm = T)
thesis$DigitIndex <-ifelse(thesis$DigiCount<=62,0,1)

table(thesis$HomoIndex)
table(thesis$DigitIndex)

table(thesis$DigitIndex)

### Recode personal atributes ####

## Age ##

thesis <-mutate(thesis, AgeRecod = dplyr::recode(thesis$Age, "1" = "18 a 29 años","2" = "30 a 40 años","3" = "41 a 65 años",
                                                 "4" = "+66 años"))
table(thesis$AgeRecod)

## Education ##

# Waiting final DF

## Income ##

thesis <-mutate(thesis, IncomeRecod = dplyr::recode(thesis$NivEco, "1" = "Menos de $224.000", "2" = "Entre $224.001 - $448.000",
                                                 "3" = "Ente $448.001 y $1.000.000", "4" = "Entre $1.000.001 - $3.000.000","5" = "Más de $3.000.000"))
table(thesis$IncomeRecod)

## Genre ##

thesis <-mutate(thesis, GenRecod = dplyr::recode(thesis$Genre, "1" = "Masculino", "2" = "Femenino", "3" = "Otro"))
table(thesis$GenRecod)

## Political position ##

# recode self position

thesis <-mutate(thesis, IdeoRec = car::recode(thesis$Ideolo_1, "1:4 = 1; 5:6 = 2; 7:9 = 3; 20 = 3")) 
# Edit in final DF. "20" shouldn't exist
thesis <-mutate(thesis, IdeoRec = dplyr::recode(thesis$IdeoRec, "1" = "Izquierda","2" = "centro","3" = "Derecha"))
table(thesis$IdeoRec)

thesis<-mutate(thesis, identity = dplyr::recode(thesis$IdePol, "28" = "Ninguno","1" = ""))

#Merge with "no ideology" column

thesis$Ideologia <-paste(thesis$IdeoRec, thesis$identity)

thesis$Ideologia <-str_c(thesis$IdeoRec, '' ,thesis$identity)


#Biding variables Experiment 1

#thesis<- thesis%>% 
#  mutate_at(c(38:42), as.numeric)

# Biding outcomes Experiment N°1

#merge treatments variable

thesis$E1 <-paste(thesis$E1T1a_1, thesis$E1T1b_1, thesis$E1T2a_1, thesis$E1T2b_1, thesis$E1TC_1)
class(thesis$E1)
thesis$E1 <-as.numeric(thesis$E1)

table(thesis$E1)

thesis$E1Treat <- ifelse(thesis$E1T1a_1!='','Afin',
                         ifelse(thesis$E1T1b_1!='','Afin',
                               ifelse(thesis$E1T2a_1!='','Opuesto',
                                      ifelse(thesis$E1T2b_1!='','Opuesto',
                                             ifelse(thesis$E1TC_1!='','Control',NA)))))

# Merge experiment 1 outcomes

#nueva <-dplyr::select(thesis, E1T1a_1,E1T1b_1,E1T2a_1,E1T2b_1,E1TC_1,E1Treat)



## Biding outcomes Experiment 2

# Merges all treatments columns

thesis$E2Treat <- ifelse(thesis$E2T1a_1!='','Afin',
                         ifelse(thesis$E2T1b_1!='','Afin',
                                ifelse(thesis$E2T2b_2!='','Opuesto',
                                ifelse(thesis$E2T1c_1!='','Afin',
                                       ifelse(thesis$E2T1c_1!='','Afin',
                                              ifelse(thesis$E2T1d_1!='','Afin',
                                              ifelse(thesis$E2T2a_1!='','Opuesto',
                                                     ifelse(thesis$E2T2b_10!='','Opuesto',
                                                     ifelse(thesis$E2T2b_1!='','Opuesto',
                                                            ifelse(thesis$E2T2c_1!='','Opuesto',
                                                                   ifelse(thesis$E2T2b_3!='','Opuesto',
                                                                   ifelse(thesis$E2TC_12!='','Control',
                                                                          ifelse(thesis$E2TC_6!='','Control',
                                                                          ifelse(thesis$E2TC_2!='','Control',NA))))))))))))))
table(thesis$E2Treat)

## Biding outcomes Experiment 3

# Merges all treatments columns : broke or mantain social ties

thesis$E3Treat <- ifelse(thesis$E3T1a1_1!='','Amigo-validado',
                         ifelse(thesis$E3T1b1_1!='','Amigo-validado',
                                ifelse(thesis$E3T2a1_1!='','Amigo-Misinfo',
                                       ifelse(thesis$E3T2b1_1!='','Amigo-Misinfo',
                                              ifelse(thesis$E3T2b2!='','Amigo-Misinfo',
                                              ifelse(thesis$E3T3a1_1!='','Conocido-validado',
                                                     ifelse(thesis$E3T3b1_1!='','Conocido-validado',
                                                            ifelse(thesis$E3T4a1_1!='','Conocido-Misinfo',
                                                                   ifelse(thesis$E3T4b1_1!='','Conocido-Misinfo',NA)))))))))

# Merges outcomes

thesis$E3 <-paste(thesis$E3T1a2, thesis$E3T1b2, thesis$E3T2a2, thesis$E3T2b2, thesis$E3T3a2,
                  thesis$E3T3b2, thesis$E3T4a2, thesis$X.E3T4b2)
thesis$E3 <-as.numeric(thesis$E3)

#nueva <-data_frame(thesis$E3T1a2, thesis$E3T1b2, thesis$E3T2a2, thesis$E3T2b2, thesis$E3T3a2,
#                thesis$E3T3b2, thesis$E3T4a2, thesis$X.E3T4b2, thesis$E3, thesis$E3Treat)


# Merges emotions 

thesis$E3Angry <-paste(thesis$E3T1a1_1, thesis$E3T1b1_1, thesis$E3T2a1_1, thesis$E3T2b1_1,
                       thesis$E3T3a1_1, thesis$E3T3b1_1, thesis$E3T4a1_1, thesis$E3T4b1_1)
thesis$E3Angry <-as.numeric(thesis$E3Angry)

thesis$E3Joy <-paste(thesis$E3T1a1_2, thesis$E3T1b1_2, thesis$E3T2a1_2, thesis$E3T2b1_2,
                     thesis$E3T3a1_4, thesis$E3T3b1_2, thesis$E3T4a1_2, thesis$E3T4b1_2)
thesis$E3Joy<-as.numeric(thesis$E3Joy)

thesis$E3Fear <-paste(thesis$E3T1a1_4, thesis$E3T1b1_4, thesis$E3T2a1_4, thesis$E3T2b1_4,
                      thesis$E3T3a1_5, thesis$E3T3b1_4, thesis$E3T4b1_4, thesis$E3T4b1_4)
thesis$E3Fear<-as.numeric(thesis$E3Fear)

thesis$E3Sad <-paste(thesis$E3T1a1_5, thesis$E3T1b1_5, thesis$E3T2a1_5, thesis$E3T2b1_5,
                     thesis$E3T3a1_6, thesis$E3T3b1_5, thesis$E3T4a1_5, thesis$E3T4b1_5)
thesis$E3Sad<-as.numeric(thesis$E3Sad)



## Biding outcomes Experiment 4
thesis$E4Treat <- ifelse(thesis$E4T1a2!='','Familia-Politico',
                         ifelse(thesis$E4T1b2!='','Familia-Politico',
                                ifelse(thesis$E4T2a2!='','Amigo-Politico',
                                       ifelse(thesis$E4T2b2!='','Amigo-Politico',
                                              ifelse(thesis$E4T3a2!='','Familia-No-Politico',
                                                            ifelse(thesis$E4T4a2!='','Amigo-No-Politico',NA))))))


# AAREGLAR ESTE MERGE QUE AUN NO FUNCIONA!


# Merges all treatments columns : broke or mantain social ties

thesis$E4 <- paste(thesis$E4T1a2, thesis$E4T1b2, thesis$E4T2a2, thesis$E4T2b2, thesis$E4T3a2,thesis$E4T4a2)
thesis$E4 <-as.numeric(thesis$E4)


# Merges emotions 

thesis$E4Angry <-paste(thesis$E4T1a1_1, thesis$E4T1b1_1, thesis$E4T2a1_1, thesis$E4T2b1_1,
                       thesis$E4T3a1_1, thesis$E4T3b1_1, thesis$E4T4a1_1, thesis$E4T4b1_1)
thesis$E4Angry <-as.numeric(thesis$E4Angry)

thesis$E4Joy <-paste(thesis$E4T1a1_2, thesis$E4T1b1_2, thesis$E4T2a1_2, thesis$E4T2b1_2,
                     thesis$E4T3a1_4, thesis$E4T3b1_2, thesis$E4T4a1_2, thesis$E4T4b1_2)
thesis$E4Joy<-as.numeric(thesis$E4Joy)

thesis$E4Fear <-paste(thesis$E4T1a1_4, thesis$E4T1b1_4, thesis$E4T2a1_4, thesis$E4T2b1_4,
                      thesis$E4T3a1_5, thesis$E4T3b1_4, thesis$E4T4b1_4, thesis$E4T4b1_4)
thesis$E4Fear<-as.numeric(thesis$E4Fear)

thesis$E4Sad <-paste(thesis$E3T1a1_5, thesis$E3T1b1_5, thesis$E3T2a1_5, thesis$E3T2b1_5,
                     thesis$E3T3a1_6, thesis$E3T3b1_5, thesis$E3T4a1_5, thesis$E3T4b1_5)
thesis$E4Sad<-as.numeric(thesis$E3Sad)

## Create new data frame

finalDF <-dplyr::select(thesis, Age:Educ,SC0:E4Sad)


saveRDS(finalDF, file = "Data/Analysis-Data/DF-final.RDS")


