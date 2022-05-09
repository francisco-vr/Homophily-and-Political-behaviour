### análysis for thesis dissertation

prueba <-readRDS(file = "Data/Analysis-Data/DF-final.RDS")

## Experiment 1 analysis

prueba$PosPol_2 <-as.numeric(prueba$PosPol_2)

prueba <-mutate(prueba, DDHHRec = car::recode(prueba$PosPol_2, "1:2 = 1; 3:4 = 2")) 

# Edit in final prueba. "20" shouldn't exist
prueba <-mutate(prueba, DDHHRec = dplyr::recode(prueba$DDHHRec, "1" = "Desacuerdo","2" = "Acuerdo"))


table(prueba$DDHHRec)

agree <-filter(prueba, DDHHRec == "Acuerdo")

disagree <-filter(prueba, DDHHRec == "Desacuerdo")

## eXPERIMENT 1 - Agree persons

compE1 <-list(c("Control","Afin"), c("Control", "Opuesto"), c("Afin", "Opuesto"))

E1Agree <-agree%>%
  dplyr::filter(!is.na(E1Treat))%>%
  ggplot(data = agree, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.05) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  labs(title = "Experimento N°1 revisado: Ira por cercanía social en \n subsección personas que creen que en Chile se violaron DDHH",
       x = "Tratamiento", y = "Nivel de ira (Max 7)",
       caption = "Fuente: Elaboración propia") +
  geom_text(data = data.frame(x = 1.99323459079714, y = 7.06367300320391, 
                              label = "Wilcoxon test"), mapping = aes(x = x, y = y, label = label), 
            size = 4.23, vjust = 0L, inherit.aes = FALSE)+
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, vjust = -1.5) +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE1) +
  stat_compare_means(label.y = 9)


E1AgreeHomo <-ggplot(data = agree, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"))+
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=4, vjust = -2) +
  geom_text(data = data.frame(x = 1.97856251472404, y = 6.99398493549631, 
                              label = "WIlcoxon Test", HomoIndex = 0L),
            mapping = aes(x = x,y = y, label = label), size = 3.17, inherit.aes = FALSE) +
  geom_text(data = data.frame(x = 1.98265356948427, y = 6.96350427825525, 
                              label = "WIlcoxon Test", HomoIndex = 1L),
            mapping = aes(x = x,y = y, label = label), size = 3.17, inherit.aes = FALSE) +
  labs(title = "Niveles de Ira según membresía a Cámaras de Eco",
       y = "Nivel de ira (Max 7)", x = "Tratamientos",
       caption = "Fuente: Elaboración propia") + 
  facet_wrap(~HomoIndex, nrow = 1, labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                     '1'="Alta membresia a Cámaras de eco"))) +
  stat_compare_means(comparisons = compE1) +
  stat_compare_means(label.y = 9.2)


E1AgreeDigit <-ggplot(data = agree, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"))+
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=3.5, vjust = -2) +
  stat_compare_means(comparisons = compE1) +
  geom_text(data = data.frame(x = 1.98815731442137, y = 7.00733768984006, 
                              label = "Wilcoxon test", DigitIndex = 1L),
            mapping = aes(x = x,y = y, label = label), size = 3.17, inherit.aes = FALSE) +
  geom_text(data = data.frame(x = 1.98226559070716, y = 7.00733768984006, 
                              label = "Wilcoxon test", DigitIndex = 0L),
            mapping = aes(x = x, y = y, label = label), size = 3.17, inherit.aes = FALSE) +
  stat_compare_means(label.y = 9.2) +
  labs(title = "Niveles de Ira segun nivel de Ciudadanìa Digital",
       y = "Nivel de ira (Max 7)", x = "Tratamientos",
       caption = "Fuente: Elaboración propia") +
  facet_wrap(~DigitIndex, nrow = 1, labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                       '1'="Alta Ciudadanía Digital")))

PlotE1Agree <-(E1Agree | E1AgreeHomo / E1AgreeDigit)

PlotE1Agree

## Experiment 1 - Disagree persons


E1Disagree <-disagree%>%
  dplyr::filter(!is.na(E1Treat))%>%
  ggplot(data = disagree, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.05) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  labs(title = "Experimento N°1: Nivel de ira por cercanía social, \n según tipo de tratamiento",
       x = "Tratamiento", y = "Nivel de ira (Max 7)",
       caption = "Fuente: Elaboración propia") +
  geom_text(data = data.frame(x = 1.99323459079714, y = 7.06367300320391, 
                              label = "Wilcoxon test"), mapping = aes(x = x, y = y, label = label), 
            size = 4.23, vjust = 0L, inherit.aes = FALSE)+
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, vjust = -1.5) +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE1) +
  stat_compare_means(label.y = 9)


E1DisagreeHomo <-ggplot(data = disagree, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"))+
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=4, vjust = -2) +
  geom_text(data = data.frame(x = 1.97856251472404, y = 6.99398493549631, 
                              label = "WIlcoxon Test", HomoIndex = 0L),
            mapping = aes(x = x,y = y, label = label), size = 3.17, inherit.aes = FALSE) +
  geom_text(data = data.frame(x = 1.98265356948427, y = 6.96350427825525, 
                              label = "WIlcoxon Test", HomoIndex = 1L),
            mapping = aes(x = x,y = y, label = label), size = 3.17, inherit.aes = FALSE) +
  labs(title = "Niveles de Ira según membresía a Cámaras de Eco",
       y = "Nivel de ira (Max 7)", x = "Tratamientos",
       caption = "Fuente: Elaboración propia") + 
  facet_wrap(~HomoIndex, nrow = 1, labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                     '1'="Alta membresia a Cámaras de eco"))) +
  stat_compare_means(comparisons = compE1) +
  stat_compare_means(label.y = 9.2)


E1DisagreeDigit <-ggplot(data = disagree, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"))+
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=3.5, vjust = -2) +
  stat_compare_means(comparisons = compE1) +
  geom_text(data = data.frame(x = 1.98815731442137, y = 7.00733768984006, 
                              label = "Wilcoxon test", DigitIndex = 1L),
            mapping = aes(x = x,y = y, label = label), size = 3.17, inherit.aes = FALSE) +
  geom_text(data = data.frame(x = 1.98226559070716, y = 7.00733768984006, 
                              label = "Wilcoxon test", DigitIndex = 0L),
            mapping = aes(x = x, y = y, label = label), size = 3.17, inherit.aes = FALSE) +
  stat_compare_means(label.y = 9.2) +
  labs(title = "Niveles de Ira segun nivel de Ciudadanìa Digital",
       y = "Nivel de ira (Max 7)", x = "Tratamientos",
       caption = "Fuente: Elaboración propia") +
  facet_wrap(~DigitIndex, nrow = 1, labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                       '1'="Alta Ciudadanía Digital")))

PlotE1Disagree <-(E1Disagree | E1DisagreeHomo / E1DisagreeDigit)

PlotE1Disagree

## Experiment 2 - Agree

ctable(agree$E3, agree$E3Treat, dnn = c('Probabilidad de romper lazos', 'Tratamiento'), prop = "c",useNA = "no",
       chisq = T, headings = TRUE, style = 'rmarkdown')

##by echo chambers

E3Homo <-ordered(agree$E3, levels = c("Romper lazos", "Mantener Lazos"))%>%
  stby(data = list(x= agree$E3, y = agree$E3Treat),
       INDICES = agree$HomoIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE)


E3Digi <-ordered(agree$E3, levels = c("Romper lazos", "Mantener Lazos"))%>%
  stby(data = list(x= agree$E3, y = agree$E3Treat),
       INDICES = agree$DigitIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE)

## by type of social tie and type of argumentation

E3Friend <-ctable(agree$E3, agree$E3TTie, dnn = c('Probabilidad de romper lazos', 'Tratamiento'), useNA = "no", chisq = T,
                  OR = T, RR = T, headings = FALSE, style = 'rmarkdown')

E3Arg <-ctable(agree$E3, agree$E3TArg, dnn = c('Probabilidad de romper lazos', 'Tratamiento'), useNA = "no", chisq = T,
               OR = T, RR = T, headings = FALSE, style = 'rmarkdown')

## subdivided by digital citizenship - social ties

levels(agree$E3) <-c("Mantener Lazos", "Romper lazos")


E3FrHomo<-ordered(agree$E3, levels = c("Mantener Lazos", "Romper lazos"))%>%
  stby(data = list(x= agree$E3, y = agree$E3TTie),
       INDICES = agree$HomoIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE,
       OR = TRUE,
       RR = TRUE)



E3FrDigi <-ordered(agree$E3, levels = c("Mantener Lazos", "Romper lazos"))%>%
  stby(data=list(x = agree$E3, y = agree$E3TTie),
       INDICES = agree$DigitIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE,
       OR = TRUE,
       RR = TRUE)

## subdivided by argumentation

E3ArgHomo <-ordered(agree$E3, levels = c("Romper lazos", "Mantener Lazos"))%>%
  stby(data=list(agree$E3, y = agree$E3TArg),
       INDICES = agree$HomoIRec,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE,
       OR = TRUE,
       RR = TRUE)


E3ArgDigi <-ordered(agree$E3, levels = c("1", "0"))%>%
  stby(data=list(x = agree$E3, y = agree$E3TArg),
       INDICES = agree$DigitIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE,
       OR = TRUE,
       RR = TRUE)

agree$E3 <-as.factor(agree$E3)
E3M1 <-glm(E3 ~ E3Treat, data = agree, family = "binomial")
E3M2 <-glm(E3 ~ E3Treat + HomoIndex, data = agree, family = "binomial")
E3M3 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex, data = agree, family = "binomial")
E3M4 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex + AgeRecod, data = agree, family = "binomial")
E3M5 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex + AgeRecod + EducRec, data = agree, family = "binomial")
E3M6 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex + AgeRecod + EducRec + GenRecod, data = agree, family = "binomial")
E3M7 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex + AgeRecod + EducRec + GenRecod + ideologia, data = agree, family = "binomial")


# Logistic regression with balanced covariates

summary(E3M1)

E3agree <-stargazer(E3M1, E3M2, E3M3,
                   title = "Probabilidad de romper lazos sociales por RRSS, con covariables balanceadas",
                   dep.var.caption = "Probabilidad de ruptura",
                   dep.var.labels = c("Modelo 1", "Modelo 2", "Modelo 3"),
                   covariate.labels = c("Lazo Fuerte - Validado", "Lazo Débi - Desinformación", "Lazo Fuerte - Validado",
                                        "Pertenencia a cámaras de eco", "Ciudadanía Digital", "Constante"),
                   star.cutoffs = c(0.05, 0.01, 0.001),
                   column.sep.width = "1pt",
                   notes.label = "Niveles de significancia",
                   type = "latex",
                   out = "Results/Tables/E3-balanced.html")

# Experiment 3 - disagree

ctable(disagree$E3, disagree$E3Treat, dnn = c('Probabilidad de romper lazos', 'Tratamiento'), prop = "c",useNA = "no",
       chisq = T, headings = TRUE, style = 'rmarkdown')

##by echo chambers

E3Homo <-ordered(disagree$E3, levels = c("Romper lazos", "Mantener Lazos"))%>%
  stby(data = list(x= disagree$E3, y = disagree$E3Treat),
       INDICES = disagree$HomoIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE)


E3Digi <-ordered(disagree$E3, levels = c("Romper lazos", "Mantener Lazos"))%>%
  stby(data = list(x= disagree$E3, y = disagree$E3Treat),
       INDICES = disagree$DigitIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE)

## by type of social tie and type of argumentation

E3Friend <-ctable(disagree$E3, disagree$E3TTie, dnn = c('Probabilidad de romper lazos', 'Tratamiento'), useNA = "no", chisq = T,
                  OR = T, RR = T, headings = FALSE, style = 'rmarkdown')

E3Arg <-ctable(disagree$E3, disagree$E3TArg, dnn = c('Probabilidad de romper lazos', 'Tratamiento'), useNA = "no", chisq = T,
               OR = T, RR = T, headings = FALSE, style = 'rmarkdown')

## subdivided by digital citizenship - social ties

levels(disagree$E3) <-c("Mantener Lazos", "Romper lazos")


E3FrHomo<-ordered(disagree$E3, levels = c("Mantener Lazos", "Romper lazos"))%>%
  stby(data = list(x= disagree$E3, y = disagree$E3TTie),
       INDICES = disagree$HomoIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE,
       OR = TRUE,
       RR = TRUE)



E3FrDigi <-ordered(disagree$E3, levels = c("Mantener Lazos", "Romper lazos"))%>%
  stby(data=list(x = disagree$E3, y = disagree$E3TTie),
       INDICES = disagree$DigitIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE,
       OR = TRUE,
       RR = TRUE)

## subdivided by argumentation

E3ArgHomo <-ordered(disagree$E3, levels = c("Romper lazos", "Mantener Lazos"))%>%
  stby(data=list(disagree$E3, y = disagree$E3TArg),
       INDICES = disagree$HomoIRec,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE,
       OR = TRUE,
       RR = TRUE)


E3ArgDigi <-ordered(disagree$E3, levels = c("1", "0"))%>%
  stby(data=list(x = disagree$E3, y = disagree$E3TArg),
       INDICES = disagree$DigitIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE,
       OR = TRUE,
       RR = TRUE)

disagree$E3 <-as.factor(disagree$E3)
E3M1 <-glm(E3 ~ E3Treat, data = disagree, family = "binomial")
E3M2 <-glm(E3 ~ E3Treat + HomoIndex, data = disagree, family = "binomial")
E3M3 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex, data = disagree, family = "binomial")
E3M4 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex + AgeRecod, data = disagree, family = "binomial")
E3M5 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex + AgeRecod + EducRec, data = disagree, family = "binomial")
E3M6 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex + AgeRecod + EducRec + GenRecod, data = disagree, family = "binomial")
E3M7 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex + AgeRecod + EducRec + GenRecod + ideologia, data = disagree, family = "binomial")


# Logistic regression with balanced covariates

summary(E3M1)

E3disagree <-stargazer(E3M1, E3M2, E3M3,
                    title = "Probabilidad de romper lazos sociales por RRSS, con covariables balanceadas",
                    dep.var.caption = "Probabilidad de ruptura",
                    dep.var.labels = c("Modelo 1", "Modelo 2", "Modelo 3"),
                    covariate.labels = c("Lazo Fuerte - Validado", "Lazo Débi - Desinformación", "Lazo Fuerte - Validado",
                                         "Pertenencia a cámaras de eco", "Ciudadanía Digital", "Constante"),
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    column.sep.width = "1pt",
                    notes.label = "Niveles de significancia",
                    type = "latex",
                    out = "Results/Tables/E3-balanced.html")





## Experiment 3 - subset by ideology

left <-filter(prueba, ideologia == "Izquierda")

right <-filter(prueba, ideologia == "Derecha")

center <-filter(prueba, ideologia == "centro")

none <-filter(prueba, ideologia == "Ninguno")



## tables with ideology - left

left$SC0 <-as.numeric(left$SC0)

E2Left <-left%>%
  dplyr::group_by("Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T),"Quantil 25%" = quantile(E1, probs = q[1]),
                   "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))

xtable(E2, Caption = "Resultados descriptivos de Experimento N°3: Precisión de distinción entre noticias verdaderas y falsas")


## Kruskal wallis test ##

kruskal.test(SC0 ~ E2Treat, data = left)
pairwise.wilcox.test(left$SC0, left$E2Treat)


E2Homo <-left%>%
  dplyr::group_by("Cámaras de eco"=HomoIndex,"Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T), "Quantil 25%" = quantile(E1, probs = q[1]), "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))

xtable(E2Homo, Caption = "Resultados descriptivos de Experimento N°3: Precisión de distinción entre noticias verdaderas y falsas")



E2Digit<-left%>%
  dplyr::group_by("Ciudadanía Digital"=DigitIndex,"Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T),"Quantil 25%" = quantile(E1, probs = q[1]), "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))

## tables with ideology - right

right$SC0 <-as.numeric(right$SC0)

E2right <-right%>%
  dplyr::group_by("Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T),"Quantil 25%" = quantile(E1, probs = q[1]),
                   "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))

xtable(E2, Caption = "Resultados descriptivos de Experimento N°3: Precisión de distinción entre noticias verdaderas y falsas")


## Kruskal wallis test ##

kruskal.test(SC0 ~ E2Treat, data = left)
pairwise.wilcox.test(left$SC0, left$E2Treat)


E2RightHomo <-right%>%
  dplyr::group_by("Cámaras de eco"=HomoIndex,"Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T), "Quantil 25%" = quantile(E1, probs = q[1]), "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))

xtable(E2Homo, Caption = "Resultados descriptivos de Experimento N°3: Precisión de distinción entre noticias verdaderas y falsas")



E2RightDigit<-right%>%
  dplyr::group_by("Ciudadanía Digital"=DigitIndex,"Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T),"Quantil 25%" = quantile(E1, probs = q[1]), "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))

## tables with ideology - center

center$SC0 <-as.numeric(center$SC0)

E2Center <-center%>%
  dplyr::group_by("Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T),"Quantil 25%" = quantile(E1, probs = q[1]),
                   "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))

xtable(E2, Caption = "Resultados descriptivos de Experimento N°3: Precisión de distinción entre noticias verdaderas y falsas")


## Kruskal wallis test ##

kruskal.test(SC0 ~ E2Treat, data = left)
pairwise.wilcox.test(left$SC0, left$E2Treat)


E2CenterHomo <-center%>%
  dplyr::group_by("Cámaras de eco"=HomoIndex,"Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T), "Quantil 25%" = quantile(E1, probs = q[1]), "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))

xtable(E2Homo, Caption = "Resultados descriptivos de Experimento N°3: Precisión de distinción entre noticias verdaderas y falsas")



E2CenterDigit<-center%>%
  dplyr::group_by("Ciudadanía Digital"=DigitIndex,"Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T),"Quantil 25%" = quantile(E1, probs = q[1]), "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))


## tables with ideology - none

none$SC0 <-as.numeric(none$SC0)

E2Center <-none%>%
  dplyr::group_by("Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T),"Quantil 25%" = quantile(E1, probs = q[1]),
                   "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))

xtable(E2, Caption = "Resultados descriptivos de Experimento N°3: Precisión de distinción entre noticias verdaderas y falsas")


## Kruskal wallis test ##

kruskal.test(SC0 ~ E2Treat, data = none)
pairwise.wilcox.test(none$SC0, none$E2Treat)


E2NoneHomo <-none%>%
  dplyr::group_by("Cámaras de eco"=HomoIndex,"Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T), "Quantil 25%" = quantile(E1, probs = q[1]), "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))

xtable(E2Homo, Caption = "Resultados descriptivos de Experimento N°3: Precisión de distinción entre noticias verdaderas y falsas")



E2NoneDigit<-none%>%
  dplyr::group_by("Ciudadanía Digital"=DigitIndex,"Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T),"Quantil 25%" = quantile(E1, probs = q[1]), "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))

