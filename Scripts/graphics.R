########################################################################################################
### Make Graphics                                                                                   ####          
### Septiember 2021                                                                                 ####
### Francisco Villarroel                                                                            ####
### Thesis Name: “Aves del mismo plumaje se emocionan juntas” Encuesta experimental sobre la        ####
### influencia de la homofília en la construcción de conductas políticas en el Chile Contemporáneo. ####
########################################################################################################



# Experiment 1


E1general <-ggplot(data = df, mapping = aes(x = E1Treat, y = E1)) +
  geom_boxplot(aes(colour = factor(Ideologia)))

E1homo <-ggplot(data = df, mapping = aes(x = E1Treat, y = E1)) +
  geom_boxplot(aes(colour = factor(Ideologia))) +
  facet_wrap(~HomoIndex, nrow = 1)

E1digit <-ggplot(data = df, mapping = aes(x = E1Treat, y = E1)) +
  geom_boxplot(aes(colour = factor(Ideologia))) +
  facet_wrap(~DigitIndex, nrow = 1)

(E1general | E1homo / E1digit)

#Experiment 2

E2general <-ggplot(data = df, mapping = aes(x = E2Treat, y = SC0)) +
  geom_boxplot() 

E2homo <-ggplot(data = df, mapping = aes(x = E2Treat, y = SC0)) +
  geom_boxplot() +
  facet_wrap(~HomoIndex, nrow = 1)

E2digit <-ggplot(data = df, mapping = aes(x = E2Treat, y = SC0)) +
  geom_boxplot() +
  facet_wrap(~DigitIndex, nrow = 1)

(E2general | E2homo / E2digit)


