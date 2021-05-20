################################################# 
### Block Ranndomization 
### Abril 2021
### Denise Laroze / Francisco Villarroel
################################################

#No incluir el llamado a librerias individuales, sino la carpeta donde estÃÂÃÂ¡n instaladas
#.libPaths=("/usr/lib64/R/library/")

require(plyr)
#require(ggplot2)
#theme_set(theme_bw())
#require(scales)
#require(gridExtra)
#require(xtable)
#require(RColorBrewer)
#require(htmlTable)
#library(gridBase)
#require(grid)
require(forcats)
require(pacman)
pacman::p_load(ggplot2, extrafont, scales)
require(purrr, warn.conflicts = FALSE, quietly = TRUE)
#require(magick,  warn.conflicts = FALSE, quietly = TRUE)
#require(scales)
#require(OpenImageR)

#Amazon Server
#setwd("/var/www/r.cess.cl/public_html/")
setwd("~/Documentos/Git/Master_Thesis/Code")


#Parameters
#path<-"/var/www/r.cess.cl/public_html/sp/"

####################################################################
###########################block randmisation####################### 

#Editar al final cuando tengamos claridad de numero de argumentos
#if(args[22] == "reset_database"){
#  time <- Sys.time()
#  time <- gsub("[:alph:]", "", time)
#  time <- gsub(" ", "_", time)
  
#  file.copy("/var/www/r.cess.cl/public_html/sp/new.RData", sprintf("rdata_bak_%s.Rdata", time))
#  file.copy("/var/www/r.cess.cl/public_html/sp/new_orig.RData", "new.RData", overwrite = T)
#  stop()
#}

#args <- as.vector(t(sim.data[i, ]))
#if(length(args) != 22){
#  stop()
#}

# Load data
#load(file="/var/www/r.cess.cl/public_html/sp/new.RData")
load(file="new.RData")

#argumentos

#args <- c("QID35","4","6","2","3","2","10","8","7","7","4","6","3","7","4","7","7","7","5","5","6","7","7","5","4")
#args <-c("QI35","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1")
#args <-c("QI22","1","4","6",10","10","8","9","1","1","1","3","4","2","4","4","3","2","3","1","1","1","1","1","1")
args <-c("45434","9","8","9","8","5","6","10","5","5","3","6","7","7","7","7","4","5","1","4","5","6","5","7","7")


#ID

QID = args[1]

#### Data Management

#Cambio de nombres en bdata y part.data

names(bdata$orig) <-c("QID","DigiCit","HomoIndex","Tr")
names(bdata$x) <-c("QID", "DigiCit","HomoIndex","Tr")

names(part.data)<-c("QID", "DigiCit","HomoIndex")


#Cambiar de caracter a número

args<-as.numeric(args)

#### Ciudadanía Digital

DigitCount <-sum(args[2:15])

#bdata$orig$

DigiCit <-ifelse(DigitCount<=56,0,1)


#bdata$x$DigiCit <-ifelse(DigitCount<=56,"0",
#                         ifelse(DigitCount>=57,"1"))


# Homofilia política

HomoCount <-sum(args[15:21])

#bdata$orig$HomoIndex <-ifelse(HomoCount<=39,"0",
#                              ifelse(HomoCount>=40,"1"))

HomoIndex <-ifelse(HomoCount<=39,0,1)

#bdata$x$HomoIndex <-ifelse(HomoCount<=39,"0",
#                           ifelse(HomoCount>=40,"1"))


 #### Block Randomization

###### Concentrarse acá

if(sum(part.data$QID %in% QID)>0){
  # Retuen value to PHP via stdout
  tr <- bdata$x$Tr[which(bdata$x$QID==QID)[1]] #Chequeo para revisr si el QID ya estaba antes
  
} else {
  # update the data.frame
  part.data <- rbind(part.data, 
                     data.frame(QID=args[1], 
                                DigiCit=DigiCit+rnorm(1,sd=.001), #ciudadanía digital
                                HomoIndex=HomoIndex+rnorm(1,sd=.001))) # Homofilia
  # update the seqblock objects
  n.idx <- nrow(part.data)
  bdata <- seqblock2k(object.name = "bdata", 
                      id.vals = part.data[n.idx, "QID"],  
                      covar.vals = part.data[n.idx,-c(1)], 
                      verbose = FALSE)
  
  tr1 <- bdata$x$Tr[length(bdata$x$Tr)] 
  
  # Save data
  save(mahal,seqblock1,seqblock2k,bdata,part.data,file="/var/www/r.cess.cl/public_html/sp/new.RData")
}


load("/var/www/r.cess.cl/public_html/sp/nuevaBDfinal.RData") # Revisar si es del coódigo antiguo

##b.data2


if(sum(part.data$QID %in% QID)>0){   # rreglar para que todo quede en bdata2 o part-data2 segun sea el caso
  # Retuen value to PHP via stdout
  tr <- bdata$x$Tr[which(bdata2$x$QID==QID)[1]] #Chequeo para revisr si el QID ya estaba antes
  
} else {
  # update the data.frame
  part.data <- rbind(part.data, 
                     data.frame(QID=args[1], 
                                DigiCit=DigiCit+rnorm(1,sd=.001), #ciudadanía digital
                                HomoIndex=HomoIndex+rnorm(1,sd=.001))) # Homofilia
  # update the seqblock objects
  n.idx <- nrow(part.data)
  bdata <- seqblock2k(object.name = "bdata", 
                      id.vals = part.data[n.idx, "QID"],  
                      covar.vals = part.data[n.idx,-c(1)], 
                      verbose = FALSE)
  
  tr1 <- bdata$x$Tr[length(bdata$x$Tr)] 
  
  # Save data
  save(mahal,seqblock1,seqblock2k,bdata,part.data,file="/var/www/r.cess.cl/public_html/sp/new.RData")
}


#envio de datos a qualtrics
to_qs<-c(tr1, tr2, tr3, tr4)
cat(sprintf("%s,%s,%s,%s", to_qs[1], to_qs[2], to_qs[3], to_qs[4]))





