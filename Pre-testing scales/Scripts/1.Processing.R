#Análisis de encuesta sobre posición política

library(tidyverse)
library(dplyr)
library(summarytools)

pol <-read.csv("Pre-testing scales/Data/Original Data/results-survey536757.csv")

## MANEJO DE DATOS ##

#cambio nombres

pol <-rename(pol, sexo = SE02,
             date_birth = SE01,
             santiago = SE03,
             pos_pol = IDE01.SQ001.,
             esc_migra = IDE02.SQ001.,
             esc_abort = IDE02.SQ002.,
             esc_strate = IDE02.SQ003.,
             esc_liber = IDE02.SQ011.,
             esc_const = IDE02.SQ004.,
             esc_rich = IDE02.SQ005.,
             esc_effort = IDE02.SQ010.,
             esc_afp = IDE02.SQ006.,
             esc_policia = IDE02.SQ007.,
             esc_mapuche = IDE02.SQ008.,
             esc_autorita = IDE02.SQ009.,
             homo_self = HO01.SQ001.,
             homo_interest = HO02.SQ001.,
             homo_trustRRSS = HO03.SQ001.,
             homo_likeme = HO04.SQ001.,
             homo_valores = HO05.SQ001.)

#recodificación de edad

pol$edad <-2020-pol$date_birth

pol <- mutate(pol, Edadrec = car::recode(pol$edad, "18:39 = 1; 40:64 = 2;
                                                           65:99 = 3"))
pol <- mutate(pol, Edadrec = dplyr::recode(pol$Edadrec, "1" = "Postdictadura",
                                                             "2" = "Dictadura","3" = "Predictadura"))

#Recodificación de posición política

pol <-mutate(pol, posicionRec = car::recode(pol$pos_pol, "0:4 = 1; 5:6 = 2; 7:10 = 3"))
pol <-mutate(pol, posicionRec = dplyr::recode(pol$posicionRec, "1" = "Izquierda", "2" = "centro", "3" = "derecha"))

#recodificación de escala política

pol <-mutate(pol, esc_migra = dplyr::recode(pol$esc_migra, "Muy de acuerdo" = "4", "De acuerdo" = "3",
                                            "En desacuerdo" = "2", "Muy en desacuerdo" = "1"))
pol$esc_migra <- as.numeric(pol$esc_migra)

############

pol <-mutate(pol, esc_abort = dplyr::recode(pol$esc_abort, "Muy de acuerdo" = "1", "De acuerdo" = "2",
                                            "En desacuerdo" = "3", "Muy en desacuerdo" = "4"))
pol$esc_abort <- as.numeric(pol$esc_abort)

#############

pol <-mutate(pol, esc_strate = dplyr::recode(pol$esc_strate, "Muy de acuerdo" = "1", "De acuerdo" = "2",
                                            "En desacuerdo" = "3", "Muy en desacuerdo" = "4"))
pol$esc_strate <-as.numeric(pol$esc_strate)

#############

pol <-mutate(pol, esc_liber = dplyr::recode(pol$esc_liber, "Muy de acuerdo" = "4", "De acuerdo" = "3",
                                            "En desacuerdo" = "2", "Muy en desacuerdo" = "1"))
pol$esc_liber <- as.numeric(pol$esc_liber)

#############

pol <-mutate(pol, esc_const = dplyr::recode(pol$esc_const, "Muy de acuerdo" = "1", "De acuerdo" = "2",
                                             "En desacuerdo" = "3", "Muy en desacuerdo" = "4"))
pol$esc_const <-as.numeric(pol$esc_const)

#############

pol <-mutate(pol, esc_rich = dplyr::recode(pol$esc_rich, "Muy de acuerdo" = "1", "De acuerdo" = "2",
                                            "En desacuerdo" = "3", "Muy en desacuerdo" = "4"))

pol$esc_rich <- as.numeric(pol$esc_rich)

############

pol <-mutate(pol, esc_effort = dplyr::recode(pol$esc_effort, "Muy de acuerdo" = "4", "De acuerdo" = "3",
                                            "En desacuerdo" = "2", "Muy en desacuerdo" = "1"))

pol$esc_effort <- as.numeric(pol$esc_effort)

############

pol <-mutate(pol, esc_afp = dplyr::recode(pol$esc_afp, "Muy de acuerdo" = "1", "De acuerdo" = "2",
                                             "En desacuerdo" = "3", "Muy en desacuerdo" = "4"))

pol$esc_afp <-as.numeric(pol$esc_afp)

###########

pol <-mutate(pol, esc_policia = dplyr::recode(pol$esc_policia, "Muy de acuerdo" = "4", "De acuerdo" = "3",
                                             "En desacuerdo" = "2", "Muy en desacuerdo" = "1"))

pol$esc_policia <-as.numeric(pol$esc_policia)

###########

pol <-mutate(pol, esc_mapuche = dplyr::recode(pol$esc_mapuche, "Muy de acuerdo" = "1", "De acuerdo" = "2",
                                          "En desacuerdo" = "3", "Muy en desacuerdo" = "4"))
pol$esc_mapuche <-as.numeric(pol$esc_mapuche)

##########

pol <-mutate(pol, esc_autorita = dplyr::recode(pol$esc_autorita, "Muy de acuerdo" = "4", "De acuerdo" = "3",
                                              "En desacuerdo" = "2", "Muy en desacuerdo" = "1"))
pol$esc_autorita <-as.numeric(pol$esc_autorita)

View(pol)

#Calculo de homofilias

pol$homo_interest <-as.numeric(pol$homo_interest)
pol$homo_likeme <-as.numeric(pol$homo_likeme)
pol$homo_self <-as.numeric(pol$homo_self)
pol$homo_trustRRSS <-as.numeric(pol$homo_trustRRSS)
pol$homo_valores<-as.numeric(pol$homo_valores)

homofilia <-rowSums(pol[23:27], na.rm = T)

pol <-data.frame(pol, homofilia)


pol <- mutate(pol, HomoRec = car::recode(pol$homofilia, "1:25 = 1; 26:50 = 2"))
pol <- mutate(pol, HomoRec = dplyr::recode(pol$HomoRec, "1" = "Homofilia baja",
                                           "2" = "Homofilia alta"))
tablita <-table(pol$HomoRec)
round((prop.table(tablita)*100),2)

#selección de variables
pol <-select(pol, sexo, pos_pol, esc_migra, esc_strate, esc_const:homo_valores, edad:HomoRec)

#Conteo de filas para calcular si es de izquierda o derecha

PolitPos <-rowSums(pol[3:11], na.rm = T)

pol <-data.frame(pol, PolitPos)

pol <-mutate(pol, RL = ifelse(PolitPos>17, "Derecha", "Izquierda"))


#calculo de posición política - auto representación

tabla <-table(pol$posicionRec)
round((prop.table(tabla)*100),2)

#calculo posición politica - escala

tabla2 <-table(pol$RL)
round((prop.table(tabla2)*100),2)

#Porcentaje homofilia

tabla3 <-table(pol$HomoRec)
round((prop.table(tabla3)*100),2)

#Tabla cruzada de los elementos de aleatorización

ctable(pol$RL, pol$HomoRec, prop = "c", headings=FALSE, na.rm=T)
ctable(pol$RL, pol$HomoRec, prop = "r", headings=FALSE, na.rm=T)
ctable(pol$RL, pol$HomoRec, prop = "t", headings=FALSE, na.rm=T)

#Guardado

save(pol, file = "Pre-testing scales/Data/Processing/pre-test.RData")

load(file = "Pre-testing scales/Data/Processing/pre-test.RData")

DFscale <-select(pol, esc_migra:esc_autorita)

save(DFscale, file = "Pre-testing scales/Data/Processing/scale.RData")

#Create randomized data frame with just 200 cases for EFA

n <-200
DFscale <- data.frame(DFscale)
ind <- sample(c(TRUE, FALSE),n, replace = TRUE, prob = c(0.696,0.304))
AFE <-DFscale[ind, ]
rest <-DFscale[!ind, ]

#Save just de AFE Dataframe, cause the other DF to use is DFscale with all cases for CFA

save(AFE, file = "Pre-testing scales/Data/Processing/AFE.RData")

#Cleaning environment
rm(list=ls())
