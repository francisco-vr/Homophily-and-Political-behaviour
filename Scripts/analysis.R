########################################################################################################
### Analysis Data                                                                                   ####          
### Septiember 2021                                                                                 ####
### Francisco Villarroel                                                                            ####
### df Name: “Aves del mismo plumaje se emocionan juntas” Encuesta experimental sobre la            ####
### influencia de la homofília en la construcción de conductas políticas en el Chile Contemporáneo. ####
########################################################################################################

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("tidyverse","dplyr","haven","ggplot2","readxl","summarytools", "patchwork","stringr",
              "tidyr","kableExtra","psych", "MASS", "foreign", "data,table","gtools","lubridate","AER",
              "xtable","pBrackets","Hmisc","ri","ggpubr", "stargazer")
ipak(packages)

#crosstab funtion
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")


# load DF

df <-readRDS("Data/Analysis-Data/DF-final.RDS")


### TESTING RANDOMIZATION ###

##################
## Experiment 1 ##
##################

# Treatment by Age #

AE1Age <-ctable(df$Age, df$E1Treat, style = 'rmarkdown', prop = "r", useNA = "no")


# Treatment by Gender #

AE1Gen <-ctable(df$Genre, df$E1Treat, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by Income #

AE1Inc <-ctable(df$NivEco, df$E1Treat, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by political position #

AE1Pol <-ctable(df$Ideologia, df$E1Treat, style = 'rmarkdown', prop = "r", useNA = "no")

##################
## Experiment 2 ##
##################


# Treatment by Age #

AE2Age <-ctable(df$Age, df$E2Treat, style = 'rmarkdown', prop = "r", useNA = "no")


# Treatment by Gender #

AE2Gen <-ctable(df$Genre, df$E2Treat, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by Income #

AE2Inc <-ctable(df$NivEco, df$E2Treat, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by political position #

AE2Pol <-ctable(df$Ideologia, df$E2Treat, style = 'rmarkdown', prop = "r", useNA = "no")

##################
## Experiment 3 ##
##################

# Treatment by Age #

AE3Age <-ctable(df$Age, df$E3Treat, style = 'rmarkdown', prop = "r", useNA = "no")


# Treatment by Gender #

AE3Gen <-ctable(df$Genre, df$E3Treat, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by Income #

AE3Inc <-ctable(df$NivEco, df$E3Treat, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by political position #

AE3Pol <-ctable(df$Ideologia, df$E3Treat, style = 'rmarkdown', prop = "r", useNA = "no")

##################
## Experiment 4 ##
##################


# Treatment by Age #

AE4Age <-ctable(df$Age, df$E4Treat, style = 'rmarkdown', prop = "r", useNA = "no")


# Treatment by Gender #

AE4Gen <-ctable(df$Genre, df$E4Treat, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by Income #

AE4Inc <-ctable(df$NivEco, df$E4Treat, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by political position #

AE4Pol <-ctable(df$Ideologia, df$E4Treat, style = 'rmarkdown', prop = "r", useNA = "no")


### Diferencias entre Medias (cuadros decentes)  ###


######################
### first outcomes ###
######################

## ARREGLAR LOS GROUP_BY DE CASI TODOS (MENOS LOS DOS PRIMEROS EXPERIMENTOS) PORQUE NO FUNCIONAN

## Experiment 1 ##

detach("package:ggpubr", unload = TRUE)

E1 <-df%>%
  dplyr::group_by(E1Treat)%>%
  dplyr::summarize(Media = mean(E1, na.rm = T), desviacion = sd(E1, na.rm = T))

## Kruskal wallis test ##

kruskal.test(E1 ~ E1Treat, data = df)
pairwise.wilcox.test(df$E1, df$E1Treat)

E1Homo <-df%>%
  dplyr::group_by(HomoIndex,E1Treat,)%>%
  dplyr::summarize(Media = mean(E1, na.rm = T), desviacion = sd(E1, na.rm = T))

E1Digit <-df%>%
  dplyr::group_by(DigitIndex,E1Treat,)%>%
  dplyr::summarize(Media = mean(E1, na.rm = T), desviacion = sd(E1, na.rm = T))


## Experiment 2 ##
df$SC0 <-as.numeric(df$SC0)

E2 <-df%>%
  dplyr::group_by(HomoIndex,E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
            Desviacion = sd(SC0, na.rm = T))

## Kruskal wallis test ##

kruskal.test(SC0 ~ E2Treat, data = df)
pairwise.wilcox.test(df$SC0, df$E2Treat)

E2Homo <-df%>%
  dplyr::group_by(HomoIndex,E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T))

E2Digit<-df%>%
  dplyr::group_by(DigitIndex,E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T))


## Experiment 3 ##

## Mantain or broke ties ##

E3 <-ctable(df$E3, df$E3Treat, prop = "c",useNA = "no")

df$E3 <-as.factor(df$E3)
model1 <-glm(E3 ~ E3Treat + HomoIndex + DigitCit, data = df, family = "binomial")
summary(model1)

stargazer(model1, out="Results/Tables/E3regression", type="latex",
          covariate.labels = c("jdk","hola", "quetal","Baja Homofilia", "Alta Homofilia",
                               "Baja Ciudadania Digital", "Alta Ciudadania Digital"), 
          dep.var.labels = c("T.1", "T.2", "T.3", "T.4"), # keep.stat=c("n", "ll"),
          dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l",
          label="tbl:E3Regre",
          title = "Mantención o ruptura de lazos", no.space=TRUE)

#Emotions #

# Angry
E3Angry <-df%>%
  dplyr::group_by(E3Treat)%>%
  dplyr::summarize(Media = mean(E3Angry, na.rm = T), Desviación = sd(E3Angry, na.rm = T))

E3AngryHomo <-df%>%
  dplyr::group_by(HomoIndex, E3Treat)%>%
  dplyr::summarize(Media = mean(E3Angry, na.rm = T), Desviación = sd(E3Angry, na.rm = T))

E3AngryDigit <-df%>%
  dplyr::group_by(DigitIndex,E3Treat)%>%
  dplyr::summarize(Media = mean(E3Angry, na.rm = T), Desviación = sd(E3Angry, na.rm = T))

kruskal.test(E3Angry ~ E3Treat, data = df)
pairwise.wilcox.test(df$E3Angry, df$E3Treat)

# Joy-Hapiness

E3Joy <-df%>%
  dplyr::group_by(E3Treat)%>%
  dplyr::summarise(Media = mean(E3Joy, na.rm = T), DE = sd(E3Joy, na.rm = T))

kruskal.test(E3Joy ~ E3Treat, data = df)
pairwise.wilcox.test(df$E3Joy, df$E3Treat)

E3JoyHomo<-df%>%
  dplyr::group_by(HomoIndex,E3Treat)%>%
  dplyr::summarize(Media = mean(E3Joy, na.rm = T), DE = sd(E3Joy, na.rm = T))

E3JoyDigit<-df%>%
  dplyr::group_by(DigitIndex,E3Treat)%>%
  dplyr::summarise(Media = mean(E3Joy, na.rm = T), DE = sd(E3Joy, na.rm = T))


# sadness

E3Sad <-df%>%
  dplyr::group_by(E3Treat)%>%
  dplyr::summarise(Media = mean(E3Sad, na.rm = T), DE = sd(E3Sad, na.rm = T))

kruskal.test(E3Sad ~ E3Treat, data = df)
pairwise.wilcox.test(df$E3Sad, df$E3Treat)

E3SadHomo <-df%>%
  dplyr::group_by(HomoIndex,E3Treat)%>%
  dplyr::summarise(Media = mean(E3Sad, na.rm = T), DE = sd(E3Sad, na.rm = T))


E3SadDigit <-df%>%
  dplyr::group_by(DigitIndex,E3Treat)%>%
  dplyr::summarise(Media = mean(E3Sad, na.rm = T), DE = sd(E3Sad, na.rm = T))


# Fear

E3Fear <-df%>%
  dplyr::group_by(E3Treat)%>%
  dplyr::summarize(Media = mean(E3Fear, na.rm = T), Desviación = sd(E3Fear, na.rm = T))

kruskal.test(E3Fear ~ E3Treat, data = df)
pairwise.wilcox.test(df$E3Fear, df$E3Treat)

E3FearHomo<-df%>%
  dplyr::group_by(HomoIndex,E3Treat)%>%
  dplyr::summarize(Media = mean(E3Fear, na.rm = T), Desviación = sd(E3Fear, na.rm = T))

E3FearDigit <-df%>%
  dplyr::group_by(DigitIndex,E3Treat)%>%
  dplyr::summarize(Media = mean(E3Fear, na.rm = T), Desviación = sd(E3Fear, na.rm = T))

## Experiment 4 ##

# get a talk or avoid conversation

E4 <-ctable(df$E4, df$E4Treat, prop = "c", chisq = T, useNA = "no")

# Emotions exp. 4

# Angry

E4Angry <-df%>%
  dplyr::group_by(E4Treat)%>%
  dplyr::summarise(Media = mean(E4Angry, na.rm = T), SD = sd(E4Angry, na.rm = T))

kruskal.test(E4Angry ~ E4Treat, data = df)
pairwise.wilcox.test(df$E4Angry, df$E4Treat)

E4AngryHomo <-df%>%
  dplyr::group_by(HomoIndex, E4Treat)%>%
  dplyr::summarize(Media = mean(E4Angry, na.rm = T), Desviación = sd(E4Angry, na.rm = T))

E4AngryDigit <-df%>%
  dplyr::group_by(DigitIndex,E4Treat)%>%
  dplyr::summarize(Media = mean(E4Angry, na.rm = T), Desviación = sd(E4Angry, na.rm = T))



# Joy-Hapiness

E4Joy <-df%>%
  dplyr::group_by(E4Treat)%>%
  dplyr::summarise(Media = mean(E4Joy, na.rm = T), Desviación = sd(E4Joy, na.rm = T))

kruskal.test(E4Joy ~ E4Treat, data = df)
pairwise.wilcox.test(df$E4Joy, df$E4Treat)

E4JoyHomo<-df%>%
  dplyr::group_by(HomoIndex,E4Treat)%>%
  dplyr::summarize(Media = mean(E4Joy, na.rm = T), Desviación = sd(E4Joy, na.rm = T))

E4JoyDigit<-df%>%
  dplyr::group_by(DigitIndex,E3Treat)%>%
  dplyr::summarise(Media = mean(E3Joy, na.rm = T), Desviación = sd(E3Joy, na.rm = T))


# sadness

E4Sad <-df%>%
  dplyr::group_by(E4Treat)%>%
  dplyr::summarise(Media = mean(df$E4Sad, na.rm = T), DE = sd(df$E4Sad, na.rm = T))


kruskal.test(E4Sad ~ E4Treat, data = df)
pairwise.wilcox.test(df$E4Sad, df$E4Treat)

E4SadHomo<-df%>%
  dplyr::group_by(HomoIndex,E4Treat)%>%
  dplyr::summarize(Media = mean(E4Sad, na.rm = T), Desviación = sd(E4Sad, na.rm = T))

E4SadDigit <-df%>%
  dplyr::group_by(DigitIndex,E4Treat)%>%
  dplyr::summarize(Media = mean(E4Sad, na.rm = T), Desviación = sd(E4Sad, na.rm = T))

# Fear

E4Fear<-df%>%
  dplyr::group_by(E4Treat)%>%
  dplyr::summarize(Media = mean(E4Fear, na.rm = T), Desviación = sd(E4Fear, na.rm = T))

kruskal.test(E4Fear ~ E4Treat, data = df)
pairwise.wilcox.test(df$E4Fear, df$E4Treat)

E4FearHomo<-df%>%
  dplyr::group_by(HomoIndex,E4Treat)%>%
  dplyr::summarize(Media = mean(E4Fear, na.rm = T), Desviación = sd(E4Fear, na.rm = T))

E4FearDigit <-df%>%
  dplyr::group_by(DigitIndex,E4Treat)%>%
  dplyr::summarize(Media = mean(E4Fear, na.rm = T), Desviación = sd(E4Fear, na.rm = T))


## Create new data frame


finalDF <-saveRDS(finalDF, file = "Data/Analysis-Data/DF-final.RDS")


## TAREAS POR REALIZAR ##

# Corregir el ifelse del E4Treat que no está funcionando
# Integrar las tablas ya listas al Rmarkdown para el informe.

anexo <-list(AE1Age, AE1Gen, AE1Inc, AE1Pol, AE2Age, AE2Gen, AE2Inc, AE2Pol, AE3Age, AE2Gen, AE3Inc, AE3Pol,
             AE4Age, AE4Gen, AE4Inc, AE4Pol)

TablaExperi <-list(E1, E1Homo, E1Digit,E2, E2Homo, E2Digit,E3, E4)

TablaEmo <-list(E3Angry, E3AngryHomo, E3AngryDigit, E3Joy, E3JoyHomo, E3JoyDigit, E3Sad, E3SadHomo, E3SadDigit,
                E3Fear, E3FearHomo, E3FearDigit, E4Angry, E4AngryHomo, E4AngryDigit, E4Joy, E4JoyHomo, E4JoyDigit,
                E4Sad, E4Sad, E4SadHomo, E4SadDigit, E4Fear, E4FearHomo, E4FearDigit)

#Save tables

saveRDS(anexo, file="Results/Tables/Tabla-anexos.rds")
saveRDS(TablaExperi, file = "Results/Tables/Tabla-resultados-experimentos.rds")
saveRDS(TablaEmo, file = "Results/Tables/Tabla-emociones.rds")


#############################

#testing block randomization

ctable(df$HomoIndex, df$E1Treat, prop = "r",useNA = "no")
ctable(df$DigitIndex, df$E1Treat, prop = "r",useNA = "no")

ctable(df$HomoIndex, df$E2Treat, prop = "r",useNA = "no")
ctable(df$DigitIndex, df$E2Treat, prop = "r",useNA = "no")

ctable(df$HomoIndex, df$E3Treat, prop = "r",useNA = "no")
ctable(df$DigitIndex, df$E3Treat, prop = "r",useNA = "no")

ctable(df$HomoIndex, df$E4Treat, prop = "r",useNA = "no")
ctable(df$DigitIndex, df$E4Treat, prop = "r",useNA = "no")


