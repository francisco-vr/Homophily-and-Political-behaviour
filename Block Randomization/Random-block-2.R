################################################# 
### Block Ranndomization 
### Abril 2021
### Denise Laroze / Francisco Villarroel
################################################

#No incluir el llamado a librerias individuales, sino la carpeta donde estÃÂÃÂ¡n instaladas

args <- commandArgs(TRUE)

.libPaths=("/usr/lib64/R/library/")

require(plyr)
require(forcats)
require(pacman)
pacman::p_load(ggplot2, extrafont, scales)
require(purrr, warn.conflicts = FALSE, quietly = TRUE)

#Amazon Server
#setwd("/var/www/r.cess.cl/public_html/")
setwd("~/Documentos/Git/Master_Thesis/Block Randomization/")


#Parameters
#path<-"/var/www/r.cess.cl/public_html/sp/"

####################################################################
###########################block randmisation####################### 

#Editar al final cuando tengamos claridad de numero de argumentos
#if(args[1] == "reset_database"){
#  time <- Sys.time()
#  time <- gsub("[:alph:]", "", time)
#  time <- gsub(" ", "_", time)
#  
#  file.copy("~/Documentos/Git/Master_Thesis/Code", sprintf("rdata_bak_%s.Rdata", time))
#  file.copy("~/Documentos/Git/Master_Thesis/Code", "new.RData", overwrite = T)
#  stop()
#}

#args <- as.vector(t(sim.data[i, ]))
#if(length(args) != 22){
#  stop()
#}

# Load data
#load(file="/var/www/html/new.RData")
load(file="new.RData")

#argumentos

#args <- c("QID35","4","6","2","3","2","10","8","7","7","4","6","3","7","4","7","7","7","5","5","6","7","7","5","4")
#args <-c("QI35","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1")
#args <-c("QI22","1","4","6","10","10","8","9","1","1","1","3","4","2","4","4","3","2","3","1","1","1","1","1","1")
#args <-c("45434","9","8","9","8","5","6","10","5","5","3","6","7","7","7","7","4","5","1","4","5","6","5","7","7")


#ID

QID = args[1]

#### Data Management

#Cambio de nombres en bdata y part.data y tratamientos

names(bdata$orig) <-c("QID","DigiCit","HomoIndex","Tr")
names(bdata$x) <-c("QID", "DigiCit","HomoIndex","Tr")

bdata$trn <-bdata$trn[-c(4:10)]
bdata$trn <-c(1,2,3,4)

bdata$tr.sort<-bdata$tr.sort[-c(4:10)]
bdata$tr.sort<-c(1,2,3,4)
names(part.data)<-c("QID", "DigiCit","HomoIndex")

bdata$ncv <-c("DigiCit","HomoIndex")
bdata$ocv <-c("DigiCit","HomoIndex")

bdata$orig$Tr <-1

bdata$x$Tr <-1


#Cambiar de caracter a número

args<-as.numeric(args)

#### Ciudadanía Digital

DigitCount <-sum(args[9:22])

DigiCit <-ifelse(DigitCount>=63,1,0)

# Homofilia política

HomoCount <-sum(args[2:8])

HomoIndex <-ifelse(HomoCount>=40,1,0)


#División de bases y guardado

bdata3=bdata
bdata4=bdata


part.data3=part.data
saveRDS(bdata3, file = "new3.RData")

part.data4=part.data
saveRDS(bdata4, file = "new4.RData")


#### Block Randomization

##Pregunta 3 (b.data3)


if(sum(part.data3$QID %in% QID)>0){   # rreglar para que todo quede en bdata2 o part-data2 segun sea el caso
  # Retuen value to PHP via stdout
  tr <- bdata3$x$Tr[which(bdata3$x$QID==QID)[1]] #Chequeo para revisr si el QID ya estaba antes
  
} else {
  # update the data.frame
  part.data3 <- rbind(part.data3,
                      data.frame(QID=args[1],
                                 DigiCit=DigiCit+rnorm(1,sd=.001), #ciudadanía digital
                                 HomoIndex=HomoIndex+rnorm(1,sd=.001))) # Homofilia
  # update the seqblock objects
  n.idx <- nrow(part.data3)
  bdata3 <- seqblock2k(object.name ="bdata3",
                       id.vals = part.data3[n.idx,"QID"],
                       covar.vals = part.data3[n.idx,-c(1)],
                       verbose = FALSE)
  
  tr3 <- bdata3$x$Tr[length(bdata3$x$Tr)] 
  
  # Save data
  save(mahal,seqblock1,seqblock2k,bdata3,part.data3,file="new3.RData")
}

#Pregunta 4 (b.date4)

if(sum(part.data4$QID %in% QID)>0){   # rreglar para que todo quede en bdata2 o part-data2 segun sea el caso
  # Retuen value to PHP via stdout
  tr <- bdata4$x$Tr[which(bdata4$x$QID==QID)[1]] #Chequeo para revisr si el QID ya estaba antes
  
} else {
  # update the data.frame
  part.data4 <- rbind(part.data4,
                      data.frame(QID=args[1],
                                 DigiCit=DigiCit+rnorm(1,sd=.001), #ciudadanía digital
                                 HomoIndex=HomoIndex+rnorm(1,sd=.001))) # Homofilia
  # update the seqblock objects
  n.idx <- nrow(part.data4)
  bdata4 <- seqblock2k(object.name ="bdata4",
                       id.vals = part.data4[n.idx,"QID"],
                       covar.vals = part.data4[n.idx,-c(1)],
                       verbose = FALSE)
  
  tr4 <- bdata4$x$Tr[length(bdata4$x$Tr)] 
  
  # Save data
  save(mahal,seqblock1,seqblock2k,bdata4,part.data4,file="new4.RData")
}



#envio de datos a qualtrics
to_qs<-c(tr3, tr4)
cat(sprintf("%s,%s", to_qs[1], to_qs[2]))
