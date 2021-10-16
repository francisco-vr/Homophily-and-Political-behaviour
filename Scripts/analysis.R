########################################################################################################
### Processing Data                                                                                 ####          
### Septiember 2021                                                                                 ####
### Francisco Villarroel                                                                            ####
### Thesis Name: “Aves del mismo plumaje se emocionan juntas” Encuesta experimental sobre la        ####
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
              "xtable","pBrackets","Hmisc","ri","ggpubr")
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

ctable(df$Age, df$E1Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")


# Treatment by Gender #

ctable(df$Genre, df$E1Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by Income #

ctable(df$NivEco, df$E1Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by political position #

ctable(df$Ideologia, df$E1Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")

##################
## Experiment 2 ##
##################


# Treatment by Age #

ctable(df$Age, df$E2Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")


# Treatment by Gender #

ctable(df$Genre, df$E2Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by Income #

ctable(df$NivEco, df$E2Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by political position #

ctable(df$Ideologia, df$E2Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")

##################
## Experiment 3 ##
##################

# Treatment by Age #

ctable(df$Age, df$E3Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")


# Treatment by Gender #

ctable(df$Genre, df$E3Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by Income #

ctable(df$NivEco, df$E3Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by political position #

ctable(df$Ideologia, df$E3Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")

##################
## Experiment 4 ##
##################


# Treatment by Age #

ctable(df$Age, df$E4Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")


# Treatment by Gender #

ctable(df$Genre, df$E4Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by Income #

ctable(df$NivEco, df$E4Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")

# Treatment by political position #

ctable(df$Ideologia, df$E4Treat, chisq = T, style = 'rmarkdown', prop = "r", useNA = "no")


### Diferencias entre medias (cuadros decentes)  ###

hola <- mean(df$E1[df$E1Treat == "TRATAMIENTO"],
               na.rm = T) - mean($prob_act_st[dat$treat_assign == "C"],
                                 na.rm = T)



