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
              "tidyr","kableExtra","psych", "MASS", "foreign")
ipak(packages)

#crosstab funtion
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")


## Load data ##

thesis <-read.csv("Data/InputData/test2.csv")

## SELECT VARIABLES ##  

thesis<-dplyr::select(thesis, Age:SC0)

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

## Political position

# recode self position

thesis <-mutate(thesis, IdeoRec = car::recode(thesis$Ideolo_1, "1:4 = 1; 5:6 = 2; 7:9 = 3"))
thesis <-mutate(thesis, IdeoRec = dplyr::recode(thesis$IdeoRec, "1" = "Izquierda","2" = "centro","3" = "Derecha"))
table(thesis$IdeoRec)

thesis<-mutate(thesis, identity = dplyr::recode(thesis$IdePol,
                                                "No. No soy ni de izquierda ni de derecha ni de centro" = "Ninguno",
                                                "Sí, puedo clasificarme de esta forma" = ""))

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

nueva <-dplyr::select(thesis, E1T1a_1,E1T1b_1,E1T2a_1,E1T2b_1,E1TC_1,E1Treat)



## Biding outcomes Experiment 2

# Merges all treatments columns

thesis$E2Treat <- ifelse(thesis$E2T1a_1!='','Afin',
                         ifelse(thesis$E2T1b_1!='','Afin',
                                ifelse(thesis$E2T1c_1!='','Afin',
                                       ifelse(thesis$E2T1c_1!='','Afin',
                                              ifelse(thesis$E2T1d_1!='','Afin',
                                              ifelse(thesis$E2T2a_1!='','Opuesto',
                                                     ifelse(thesis$E2T2b_10!='','Opuesto',
                                                     ifelse(thesis$E2T2b_1!='','Opuesto',
                                                            ifelse(thesis$E2T2c_1!='','Opuesto',
                                                                   ifelse(thesis$E2T2b_3!='','Opuesto',
                                                                   ifelse(thesis$E2TC_12!='','Control',
                                                                          ifelse(thesis$E2TC_2!='','Control',NA))))))))))))

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

nueva <-data_frame(thesis$E3T1a2, thesis$E3T1b2, thesis$E3T2a2, thesis$E3T2b2, thesis$E3T3a2,
                 thesis$E3T3b2, thesis$E3T4a2, thesis$X.E3T4b2, thesis$E3, thesis$E3Treat)


# Merges emotions 

thesis$E3Angry <-paste(thesis$E3T1a1_1, thesis$E3T1b1_1, thesis$E3T2a1_1, thesis$E3T2b1_1,
                       thesis$E3T3a1_1, thesis$E3T3b1_1, thesis$E3T4a1_1, thesis$E3T4b1_1)
thesis$E3Angry <-as.numeric(thesis$E3Angry)

mean(thesis$E3Angry, na.rm = T)
sd(thesis$E3Angry, na.rm = T)

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


######################
### first outcomes ###
######################

## ARREGLAR LOS GROUP_BY DE CASI TODOS (MENOS LOS DOS PRIMEROS EXPERIMENTOS) PORQUE NO FUNCIONAN

## Experiment 1 ##

E1 <-thesis%>%
  dplyr::group_by(E1Treat)%>%
  dplyr::summarise(mean1 = mean(E1, na.rm = T), desviacion = sd(thesis$E1, na.rm = T))

## Experiment 2 ##
thesis$SC0 <-as.numeric(thesis$SC0)

E2 <-thesis%>%
  group_by(E2Treat)%>%
  summarise(mean2 = mean(SC0, na.rm = T),
            SD = sd(thesis$E1, na.rm = T))

## Experiment 3 ##

## Mantain or broke ties ##

E3 <-ctable(thesis$E3, thesis$E3Treat, prop = "c")

#Emotions #

# Angry
E3Angry <-thesis%>%
  group_by(E3Treat)%>%
  summarise(media = mean(thesis$E3Angry, na.rm = T))

# Joy-Hapiness

E3Joy <-thesis%>%
  dplyr::group_by(E3Treat)%>%
  dplyr::summarise(media = mean(thesis$E3Joy, na.rm = T), DE = sd(thesis$E3Joy, na.rm = T))

# sadness

E3Sad <-thesis%>%
  dplyr::group_by(E3Treat)%>%
  dplyr::summarise(media = mean(thesis$E3Sad, na.rm = T), DE = sd(thesis$E3Sad, na.rm = T))

# Fear

E3Fear <-thesis%>%
  dplyr::group_by(E3Treat)%>%
  dplyr::summarise(media = mean(thesis$E3Fear, na.rm = T), DE = sd(thesis$E3Fear, na.rm = T))


## Experiment 4 ##

# get a talk or avoid conversation

E4 <-ctable(thesis$E4, thesis$E4Treat, prop = "c", chisq = T)

# Emotions exp. 4

# Angry

E4Angry <-thesis%>%
  dplyr::group_by(E4Treat)%>%
  dplyr::summarise(media = mean(thesis$E4Angry, na.rm = T), SD = sd(thesis$E4Angry, na.rm = T))

# Joy-Hapiness

E4Joy <-thesis%>%
  dplyr::group_by(E4Treat)%>%
  dplyr::summarise(media = mean(thesis$E4Joy, na.rm = T), DE = sd(thesis$E4Joy, na.rm = T))

# sadness

E4Sad <-thesis%>%
  dplyr::group_by(E4Treat)%>%
  dplyr::summarise(media = mean(thesis$E4Sad, na.rm = T), DE = sd(thesis$E4Sad, na.rm = T))

# Fear

E4Fear<-thesis%>%
  group_by(E4Treat)%>%
  summarise(media = mean(thesis$E4Fear, na.rm = T))

## Create new data frame

finalDF <-dplyr::select(thesis, Age:Educ,SC0:E4Joy)


finalDF <-saveRDS(finalDF, file = "Data/Analysis-Data/DF-final.RDS")


thesis%>%
  dplyr::group_by(E1Treat, DigitIndex)%>%
  dplyr::summarise(mean1 = mean(E1, na.rm = T), SD = sd(thesis$E1, na.rm = T))


thesis%>%
  group_by(E2Treat, HomoIndex)%>%
  summarise(mean2 = mean(SC0, na.rm = T), SD = sd(thesis$E1, na.rm = T))

## TAREAS POR REALIZAR ##

# Corregir el ifelse del E4Treat que no está funcionando
# Corregir los group_by + summarize que no están funcionando.
# Integrar las tablas ya listas al Rmarkdown para el informe.

# Posible solucion: copiar y pegar los códigos que sí están funcionando en cada caso.

TablaExperi <-list(E1, E2, E3, E4)

TablaEmo <-list(E3Angry, E3Joy, E3Sad, E3Fear, E4Angry, E4Joy, E4Sad, E4Fear)

#Save tables

saveRDS(TablaExperi, file = "Results/Tables/Tabla-resultados-experimentos.rds")
saveRDS(TablaEmo, file = "Results/Tables/Tabla-emociones.rds")

