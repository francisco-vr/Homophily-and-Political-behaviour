########################################################################################################
### Make Graphics                                                                                   ####          
### September 2021                                                                                 ####
### Francisco Villarroel                                                                            ####
### Thesis Name: “Aves del mismo plumaje se emocionan juntas” Encuesta experimental sobre la        ####
### influencia de la homofília en la construcción de conductas políticas en el Chile Contemporáneo. ####
########################################################################################################


# Experiment 1

E1general <-df%>%
  dplyr::filter(!is.na(E1Treat))%>%
  ggplot(data = df, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.06) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Experimento N°1: Nivel de ira por cercanía social, \n según tipo de tratamiento",
       x = "Tratamiento", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun.y=mean, geom="text", size=7, vjust = -4) +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic"))

E1homo <-ggplot(data = df, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  geom_boxplot(aes(colour = )) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"))+
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun.y=mean, geom="text", size=4, vjust = -2) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Niveles de Ira segun nivel de Homofilia polìtica",
       y = "Nivel de Ira", x = "",
       caption = "Fuente: Elaboración propia") + 
  facet_wrap(~HomoIndex, nrow = 1, labeller = labeller(HomoIndex = c('0'="Baja Homofilia",
                                                       '1'="Alta Homofilia")))
  

E1digit <-ggplot(data = df, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"))+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun.y=mean, geom="text", size=4, vjust = -2) +
  labs(title = "Niveles de Ira segun nivel de Ciudadanìa Digital",
       y = "Nivel de Ira", x = "",
       caption = "Fuente: Elaboración propia") +
  facet_wrap(~DigitIndex, nrow = 1, labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                       '1'="Alta Ciudadanía Digital")))

PlotE1 <-(E1general | E1homo / E1digit)

ggsave(PlotE1, filename = "Results/Plots/Experimento1.png",
       dpi = 400, width = 13, height = 7)



#####################
#### Experiment 2 ###
#####################

E2general<-df%>%
  dplyr::filter(!is.na(E2Treat))%>%
  ggplot(data = df, mapping = aes(x = E2Treat, y = SC0, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun.y=mean, geom="text", size=7, vjust = -2) +
  labs(title = "Experimento N°2: Puntajes de  de acertividad ante \n titulares de noticias falsos o verdaderos",
       x = "Tratamiento", y = "Puntaje (max 7)",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic"))

E2homo <-ggplot(data = df, mapping = aes(x = E2Treat, y = SC0, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun.y=mean, geom="text", size=4, vjust = -2) +
  scale_fill_manual(values = wes_palette("Darjeeling1")) +
  labs(title = "Según nivel de Homofilia",
       x = "Tratamiento", y = "Puntaje (max 7)",
       caption = "Fuente: Elaboración propia") +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja Homofilia",
                                                                    '1'="Alta Homofilia")))

E2digit <-ggplot(data = df, mapping = aes(x = E2Treat, y = SC0, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun.y=mean, geom="text", size=4, vjust = -2) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  labs(title = "Según nivel de Ciudadania Digital",
       x = "Tratamiento", y = "Puntaje (max 7)",
       caption = "Fuente: Elaboración propia") +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital")))

PlotE2 <-(E2general | E2homo / E2digit)

ggsave(PlotE2, filename = "Results/Plots/Experimento2.png",
       dpi = 400, width = 15, height = 7)

#####################
#### Experiment 3 ###
#####################


### Maintain/Broke ties ###




### Emotions ###

## Angry

Plot3Angry <-df%>%
  dplyr::filter(!is.na(E3Treat))%>%
  ggplot(data = df, mapping = aes(x = E3Treat, y = E3Angry, fill = E3Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_fill_manual(values = wes_palette(n=4, name="Darjeeling1")) +
  labs(title = "Experimento N°4: Ira al confrontar \n una discusión con distintos lazos sociales",
       x = "Tratamiento", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 12, face = "bold"),
        plot.caption = element_text(face = "italic"))

## By Homophily and Digital citizen ##

# Homphily

E3AngryHomo <-ggplot(data = df, mapping = aes(x = E3Treat, y = E3Angry, fill = E3Treat)) +
  geom_boxplot(aes(colour = )) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~HomoIndex, nrow = 1, labeller = labeller(HomoIndex = c('0'="Baja Homofilia",
                                                                     '1'="Alta Homofilia")))

#Digital citizenship

E3AngryDigit <-ggplot(data = df, mapping = aes(x = E3Treat, y = E3Angry, fill = E3Treat)) +
  geom_boxplot(aes(colour = )) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital")))

PlotE3Angry <-(Plot3Angry | E3AngryHomo / E3AngryDigit)

ggsave(PlotE3Angry, filename = "Results/Plots/Exp3angry.png",
       dpi = 600, width = 22, height = 9)


## JOy 

Plot3Joy <-df%>%
  dplyr::filter(!is.na(E3Treat))%>%
  ggplot(data = df, mapping = aes(x = E3Treat, y = E3Joy, fill = E3Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_fill_manual(values = wes_palette(n=4, name="Darjeeling1")) +
  labs(title = "Experimento N°4: Felicidad al confrontar \n una discusión con distintos lazos sociales",
       x = "Tratamiento", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 12, face = "bold"),
        plot.caption = element_text(face = "italic"))

## By Homophily and Digital citizen ##

# Homphily

E3JoyHomo <-ggplot(data = df, mapping = aes(x = E3Treat, y = E3Joy, fill = E3Treat)) +
  geom_boxplot(aes(colour = )) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~HomoIndex, nrow = 1, labeller = labeller(HomoIndex = c('0'="Baja Homofilia",
                                                                     '1'="Alta Homofilia")))

#Digital citizenship

E3JoyDigit <-ggplot(data = df, mapping = aes(x = E3Treat, y = E3Joy, fill = E3Treat)) +
  geom_boxplot(aes(colour = )) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital")))

PlotE3Joy <-(Plot3Joy | E3JoyHomo / E3JoyDigit)

ggsave(PlotE3Joy, filename = "Results/Plots/Exp3Joy.png",
       dpi = 600, width = 22, height = 9)


## Sad

Plot3Sad <-df%>%
  dplyr::filter(!is.na(E3Treat))%>%
  ggplot(data = df, mapping = aes(x = E3Treat, y = E3Sad, fill = E3Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_fill_manual(values = wes_palette(n=4, name="Darjeeling1")) +
  labs(title = "Experimento N°4: Tristeza al confrontar \n una discusión con distintos lazos sociales",
       x = "Tratamiento", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 12, face = "bold"),
        plot.caption = element_text(face = "italic"))


## By Homophily and Digital citizen ##

# Homphily

E3SadHomo <-ggplot(data = df, mapping = aes(x = E3Treat, y = E3Sad, fill = E3Treat)) +
  geom_boxplot(aes(colour = )) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~HomoIndex, nrow = 1, labeller = labeller(HomoIndex = c('0'="Baja Homofilia",
                                                                     '1'="Alta Homofilia")))

#Digital citizenship

E3SadDigit <-ggplot(data = df, mapping = aes(x = E3Treat, y = E3Sad, fill = E3Treat)) +
  geom_boxplot(aes(colour = )) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital")))

PlotE3Sad <-(Plot3Sad | E3SadHomo / E3SadDigit)

ggsave(PlotE3Sad, filename = "Results/Plots/Exp3sad.png",
       dpi = 600, width = 22, height = 9)


## Fear

Plot3Fear <-df%>%
  dplyr::filter(!is.na(E3Treat))%>%
  ggplot(data = df, mapping = aes(x = E3Treat, y = E3Fear, fill = E3Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_fill_manual(values = wes_palette(n=4, name="Darjeeling1")) +
  labs(title = "Experimento N°4: Temor al confrontar \n una discusión con distintos lazos sociales",
       x = "Tratamiento", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 12, face = "bold"),
        plot.caption = element_text(face = "italic"))

## By Homophily and Digital citizen ##

# Homphily

E3FearHomo <-ggplot(data = df, mapping = aes(x = E3Treat, y = E3Fear, fill = E3Treat)) +
  geom_boxplot(aes(colour = )) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~HomoIndex, nrow = 1, labeller = labeller(HomoIndex = c('0'="Baja Homofilia",
                                                                     '1'="Alta Homofilia")))

#Digital citizenship

E3FearDigit <-ggplot(data = df, mapping = aes(x = E3Treat, y = E3Fear, fill = E3Treat)) +
  geom_boxplot(aes(colour = )) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital")))

PlotE3Fear <-(Plot3Fear | E3FearHomo / E3FearDigit)

ggsave(PlotE3Fear, filename = "Results/Plots/Exp3fear.png",
       dpi = 600, width = 22, height = 9)


## Ploting four emnotions

PlotE3Emotions <-(Plot3Joy / Plot3Angry | Plot3Fear / Plot3Sad)

ggsave(PlotE3Emotions, filename = "Results/Plots/E3Emotions.png",
       dpi = 600, width = 16, height = 9)


#####################
#### Experiment 4 ###
#####################

### Discuss or avoid ###




### Emotions throughy that

## Angry

Plot4Angry <-df%>%
  dplyr::filter(!is.na(E4Treat))%>%
  ggplot(data = df, mapping = aes(x = E4Treat, y = E4Angry, fill = E4Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_fill_manual(values = wes_palette(n=4, name="Darjeeling1")) +
  labs(title = "Experimento N°4: Ira al confrontar \n una discusión con distintos lazos sociales",
       x = "Tratamiento", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic"))

## By Homophily and Digital citizen ##

# Homphily

E4AngryHomo <-ggplot(data = df, mapping = aes(x = E4Treat, y = E4Angry, fill = E4Treat)) +
  geom_boxplot(aes(colour = )) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~HomoIndex, nrow = 1, labeller = labeller(HomoIndex = c('0'="Baja Homofilia",
                                                                     '1'="Alta Homofilia")))

#Digital citizenship

E4AngryDigit <-ggplot(data = df, mapping = aes(x = E4Treat, y = E4Angry, fill = E4Treat)) +
  geom_boxplot(aes(colour = )) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital")))

PlotE4Angry <-(Plot4Angry | E4AngryHomo / E4AngryDigit)

ggsave(PlotE4Angry, filename = "Results/Plots/Exp4angry.png",
       dpi = 600, width = 22, height = 9)


## Joy

Plot4Joy <-df%>%
  dplyr::filter(!is.na(E4Treat))%>%
  ggplot(data = df, mapping = aes(x = E4Treat, y = E4Joy, fill = E4Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6)+
  geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Experimento N°4: Felicidad al confrontar \n una discusión con distintos lazos sociales",
       x = "Tratamiento", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic"))

  ## By Homophily and Digital citizen ##
  
  # Homphily
  
  E4JoyHomo <-ggplot(data = df, mapping = aes(x = E4Treat, y = E4Joy, fill = E4Treat)) +
    geom_boxplot(aes(colour = )) +
    geom_jitter(color="black", size=0.4, alpha=0.9)+
    facet_wrap(~HomoIndex, nrow = 1, labeller = labeller(HomoIndex = c('0'="Baja Homofilia",
                                                                       '1'="Alta Homofilia")))
  
  #Digital citizenship
  
  E4JoyDigit <-ggplot(data = df, mapping = aes(x = E4Treat, y = E4Joy, fill = E4Treat)) +
    geom_boxplot(aes(colour = )) +
    geom_jitter(color="black", size=0.4, alpha=0.9)+
    facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                        '1'="Alta Ciudadanía Digital")))
  
  PlotE4Joy <-(Plot4Joy | E4JoyHomo / E4JoyDigit)
  
  ggsave(PlotE4Joy, filename = "Results/Plots/Exp4Joy.png",
         dpi = 600, width = 22, height = 9)  
  
  
## Sad
  
Plot4Sad <-df%>%
    dplyr::filter(!is.na(E4Treat))%>%
    ggplot(data = df, mapping = aes(x = E4Treat, y = E4Sad, fill = E4Treat)) +
    stat_boxplot(geom ='errorbar', width = 0.6) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9)+
    scale_fill_manual(values = wes_palette(n=4, name="Darjeeling1")) +
    labs(title = "Experimento N°4: Tristeza al confrontar \n una discusión con distintos lazos sociales",
         x = "Tratamiento", y = "Nivel de ira",
         caption = "Fuente: Elaboración propia") +
    theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
          plot.caption = element_text(face = "italic"))
  
  
## By Homophily and Digital citizen ##

# Homphily

E4SadHomo <-ggplot(data = df, mapping = aes(x = E4Treat, y = E4Sad, fill = E4Treat)) +
  geom_boxplot(aes(colour = )) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~HomoIndex, nrow = 1, labeller = labeller(HomoIndex = c('0'="Baja Homofilia",
                                                                     '1'="Alta Homofilia")))

#Digital citizenship

E4SadDigit <-ggplot(data = df, mapping = aes(x = E4Treat, y = E4Sad, fill = E4Treat)) +
  geom_boxplot(aes(colour = )) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital")))

PlotE4Sad <-(Plot4Sad | E4SadHomo / E4SadDigit)

ggsave(PlotE4Sad, filename = "Results/Plots/Exp4sad.png",
       dpi = 600, width = 22, height = 9)


## Fear  
  
  Plot4Fear <-df%>%
    dplyr::filter(!is.na(E4Treat))%>%
    ggplot(data = df, mapping = aes(x = E4Treat, y = E4Fear, fill = E4Treat)) +
    stat_boxplot(geom ='errorbar', width = 0.6)+
  geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9)+
    labs(title = "Experimento N°4: Temor al confrontar \n una discusión con distintos lazos sociales",
         x = "Tratamiento", y = "Nivel de felicidad",
         caption = "Fuente: Elaboración propia") +
    theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
          plot.caption = element_text(face = "italic"))
  
  ## By Homophily and Digital citizen ##
  
  # Homphily
  
  E4FearHomo <-ggplot(data = df, mapping = aes(x = E4Treat, y = E4Fear, fill = E4Treat)) +
    geom_boxplot(aes(colour = )) +
    geom_jitter(color="black", size=0.4, alpha=0.9)+
    facet_wrap(~HomoIndex, nrow = 1, labeller = labeller(HomoIndex = c('0'="Baja Homofilia",
                                                                       '1'="Alta Homofilia")))
  
  #Digital citizenship
  
  E4FearDigit <-ggplot(data = df, mapping = aes(x = E4Treat, y = E4Fear, fill = E4Treat)) +
    geom_boxplot(aes(colour = )) +
    geom_jitter(color="black", size=0.4, alpha=0.9)+
    facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                        '1'="Alta Ciudadanía Digital")))
  
  PlotE4Fear <-(Plot4Fear | E4FearHomo / E4FearDigit)
  
  ggsave(PlotE4Fear, filename = "Results/Plots/Exp4fear.png",
         dpi = 600, width = 22, height = 9)
  

  #Plot all emotions exp. 4
  
  PlotE4Emotions <-(Plot4Joy / Plot4Angry | Plot4Fear / Plot4Sad)
  
  ggsave(PlotE4Emotions, filename = "Results/Plots/Exp4Emotions.png",
         dpi = 600, width = 22, height = 9)
