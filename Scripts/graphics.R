########################################################################################################
### Make Graphics                                                                                   ####          
### Septiember 2021                                                                                 ####
### Francisco Villarroel                                                                            ####
### Thesis Name: “Aves del mismo plumaje se emocionan juntas” Encuesta experimental sobre la        ####
### influencia de la homofília en la construcción de conductas políticas en el Chile Contemporáneo. ####
########################################################################################################

#DELETE WHEN THE FINAL DF ITS OK.
df <-df%>%
  slice(-c(37,38,39))


# Experiment 1

indiceHomo <-list('0'="Baja Homofilia",
              '1'="Alta Homofilia")
indicecitizen <-list('0'="Baja Ciudanía Digital",
                     '1'="Alta Ciudadanía Digital")


E1general <-df%>%
  dplyr::filter(!is.na(E1Treat))%>%
  ggplot(data = df, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Experimento N°1: Nivel de ira por cercanía social, \n según tipo de tratamiento",
       x = "Tratamiento", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic"))

E1homo <-ggplot(data = df, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  geom_boxplot(aes(colour = )) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~HomoIndex, nrow = 1)
  

E1digit <-ggplot(data = df, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"))+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~DigitIndex, nrow = 1)

(E1general | E1homo / E1digit)

#Experiment 2

E2general<-df%>%
  dplyr::filter(!is.na(E2Treat))%>%
  ggplot(data = df, mapping = aes(x = E2Treat, y = SC0, fill = E2Treat)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Experimento N°2: Puntajes de respuestas correctas ante \n titulares de noticias falsos o verdaderos",
       x = "Tratamiento", y = "Puntaje (max 7)",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic"))

E2homo <-ggplot(data = df, mapping = aes(x = E2Treat, y = SC0)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~HomoIndex, nrow = 1)

E2digit <-ggplot(data = df, mapping = aes(x = E2Treat, y = SC0)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  facet_wrap(~DigitIndex, nrow = 1)

(E2general | E2homo / E2digit)

#Experiment 3
Plot3Angry <-df%>%
  dplyr::filter(!is.na(E3Treat))%>%
  ggplot(data = df, mapping = aes(x = E3Treat, y = E3Angry, fill = E3Treat)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=4, name="Darjeeling1")) +
  labs(title = "Experimento N°4: Ira al confrontar \n una discusión con distintos lazos sociales",
       x = "Tratamiento", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 12, face = "bold"),
        plot.caption = element_text(face = "italic"))

Plot3Joy <-df%>%
  dplyr::filter(!is.na(E3Treat))%>%
  ggplot(data = df, mapping = aes(x = E3Treat, y = E3Joy, fill = E3Treat)) +
  geom_boxplot() +
  labs(title = "Experimento N°4: Felicidad al confrontar \n una discusión con distintos lazos sociales",
       x = "Tratamiento", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 12, face = "bold"),
        plot.caption = element_text(face = "italic"))


Plot3Sad <-df%>%
  dplyr::filter(!is.na(E3Treat))%>%
  ggplot(data = df, mapping = aes(x = E3Treat, y = E3Sad, fill = E3Treat)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=4, name="Darjeeling1")) +
  labs(title = "Experimento N°4: Tristeza al confrontar \n una discusión con distintos lazos sociales",
       x = "Tratamiento", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 12, face = "bold"),
        plot.caption = element_text(face = "italic"))

Plot3Fear <-df%>%
  dplyr::filter(!is.na(E3Treat))%>%
  ggplot(data = df, mapping = aes(x = E3Treat, y = E3Fear, fill = E3Treat)) +
  geom_boxplot() +
  labs(title = "Experimento N°4: Temor al confrontar \n una discusión con distintos lazos sociales",
       x = "Tratamiento", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 12, face = "bold"),
        plot.caption = element_text(face = "italic"))

(Plot3Joy / Plot3Angry | Plot3Fear / Plot3Sad)

#Experiment 4

Plot4Angry <-df%>%
  dplyr::filter(!is.na(E4Treat))%>%
  ggplot(data = df, mapping = aes(x = E4Treat, y = E4Angry, fill = E4Treat)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=4, name="Darjeeling1")) +
  labs(title = "Experimento N°4: Ira al confrontar \n una discusión con distintos lazos sociales",
       x = "Tratamiento", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic"))

Plot4Joy <-df%>%
  dplyr::filter(!is.na(E4Treat))%>%
  ggplot(data = df, mapping = aes(x = E4Treat, y = E4Joy, fill = E4Treat)) +
  geom_boxplot() +
  labs(title = "Experimento N°4: Felicidad al confrontar \n una discusión con distintos lazos sociales",
       x = "Tratamiento", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic"))

(Plot4Joy | Plot4Angry)
