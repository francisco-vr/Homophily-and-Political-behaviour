#Análisis factorial de escala de posición política

#Análisis factorial exploratorio


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("parameters","apa","haven","ggplot2","ggpubr","gridExtra","apaTables", "reshape",
              "GPArotation", "mvtnorm", "psych", "psychometric", "lavaan", "nFactors", "semPlot",
              "lavaan", "MVN", "semTools")
ipak(packages)

#load Data Frame
load(file = "Pre-testing scales/Data/Processing/AFE.RData")

#Correlation test, Barlett test & KMO Test
#cORRELATION
corr.test(AFE)

#Barlett
correlaciones <-corr.test(AFE)
correlaciones$r
r<-as.matrix(correlaciones$r)
cortest.bartlett(r, n=214)

#KMO
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
print(ASIfactor,digits = 2,cut = .60,sort=TRUE)

## CONFIRMATORIAL FACTORYAL ANALYSIS ##

#load data frame with all cases

load(file = "Pre-testing scales/Data/Processing/scale.RData")

#CFA

#Especificación del modelo conceptual (primero unidimensional, luego bifactorial)
Onefactor<-'Ideol =~ esc_migra + esc_strate + esc_const + esc_rich + esc_effort 
+ esc_afp + esc_policia + esc_mapuche + esc_autorita
'
Twofactor<-'Favor =~ esc_rich + esc_strate + esc_afp + esc_const + esc_mapuche 
Contra =~ esc_policia + esc_autorita +esc_migra + esc_effort'

#realización del AFC para la primera estructura
CFAone <- cfa(Onefactor,orthogonal=FALSE, data=DFscale, estimator="WLSMV",ordered =names(DFscale))
summary(CFAone, fit.measures=TRUE)

#Análisis Factorial Confirmatorio para la segunda dimensionalidad.
CFAtworele <- cfa(Twofactor,orthogonal=FALSE, data=DFscale, estimator="WLSMV",ordered =names(DFscale))
summary(CFAtworele, fit.measures=TRUE)
fitMeasures(CFAtworele)
semPaths(CFAtworele, intercepts =FALSE,edge.label.cex=2, optimizeLatRes = TRUE,
         groups = "lat",pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",esize = 4,
         label.prop=2,sizeLat = 7,"std", layout="circle2")


