#Análisis factorial de escala de posición política

#Análisis factorial exploratorio


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("parameters","apa","haven","ggplot2","ggpubr","gridExtra","apaTables", "reshape", "GPArotation",
              "mvtnorm", "psych", "psychometric", "lavaan", "nFactors", "semPlot", "lavaan", "MVN", "semTools")
ipak(packages)

#load Data Frame
load(file = "Pre-testing scales/AFE.RData")

#KMO Test
KMO(AFE)

##EXPLORATORY FACTORIAL ANALYSIS ##

# N Factors to extract
results_nfactorASI<-n_factors(AFE, rotate = "Promax", fm = "mle", n = NULL)
plot(results_nfactorASI)
results_nfactorASI
as.data.frame(results_nfactorASI)
summary(results_nfactorASI)

#Exploratory Factorial ANalysis ASI
ASIfactor<-fa(AFE,nfactors = 2,fm = "ml",rotate ="Promax",cor = "poly")
print(ASIfactor,digits = 2,cut = .40,sort=TRUE)

## CONFIRMATORIAL FACTORYAL ANALYSIS ##

#load data frame with all cases

load(file = "Pre-testing scales/scale.RData")

#CFA

DFscale

#Especificación del modelo conceptual (primero unidimensional, luego bifactorial)
Onefactor<-'Ideology =~ esc_migra + esc_abort + esc_strate + esc_const + esc_rich + esc_effort 
+ esc_afp + esc_policia + esc_mapuche + esc_autorita
'
Twofactor<-'Left =~ esc_rich + esc_strate + esc_afp + esc_const + esc_mapuche 
Right =~ esc_policia + esc_autorita + esc_abort + esc_migra + esc_effort'

#realización del AFC para la primera estructura
CFAone <- cfa(Onefactor,orthogonal=TRUE, data=DFscale, estimator="WLSMV",ordered =names(DFscale))
summary(CFAone, fit.measures=TRUE)

#Análisis Factorial Confirmatorio para la segunda dimensionalidad.
CFAtworele <- cfa(Twofactor,orthogonal=FALSE, data=DFscale, estimator="WLSMV",ordered =names(DFscale))
summary(CFAtworele, fit.measures=TRUE)
fitMeasures(CFAtworele)
semPaths(CFAtworele, intercepts = FALSE,edge.label.cex=2.5, optimizeLatRes = TRUE, groups = "lat",
         pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",esize = 6, label.prop=2,
         sizeLat = 6,"std", layout="circle2")


