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
require(extrafont)
require(ggplot2)
require(scales)
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
args <-c("QI35","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1")



#ID

QID = args[1]

#### Data Management

#Cambio de nombres en bdata y part.data y tratamientos

names(bdata$orig) <-c("QID","DigiCit","HomoIndex","Tr")
names(bdata$x) <-c("QID", "DigiCit","HomoIndex","Tr")

bdata$trn <-bdata$trn[-c(4:10)]
bdata$trn <-c(1,2,3)

bdata$tr.sort<-bdata$tr.sort[-c(4:10)]
bdata$tr.sort<-c(1,2,3)
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

bdata1=bdata
bdata2=bdata


part.data1=part.data
saveRDS(bdata1, file = "new1.RData")

part.data2=part.data
saveRDS(bdata2, file = "new2.RData")


 #### Block Randomization

##Pregunta1 (bdata1)

if(sum(part.data1$QID %in% QID)>0){
  # Retuen value to PHP via stdout
  tr <- bdata1$x$Tr[which(bdata1$x$QID==QID)[1]] #Chequeo para revisr si el QID ya estaba antes
  
} else {
  # update the data.frame
  part.data1 <- rbind(part.data1, 
                     data.frame(QID=args[1], 
                                DigiCit=DigiCit+rnorm(1,sd=.001), #ciudadanía digital
                                HomoIndex=HomoIndex+rnorm(1,sd=.001))) # Homofilia
  # update the seqblock objects
  n.idx <- nrow(part.data1)
  bdata1 <- seqblock2k(object.name ="bdata1",
                       id.vals = part.data1[n.idx,"QID"],
                       covar.vals = part.data1[n.idx,-c(1)],
                       verbose = FALSE)
  
tr1 <- bdata1$x$Tr[length(bdata1$x$Tr)] 
  
  # Save data
  save(mahal,seqblock1,seqblock2k,bdata1,part.data1,file="new1.RData")
}



##Pregunta 2 (b.data2)


if(sum(part.data2$QID %in% QID)>0){   # rreglar para que todo quede en bdata2 o part-data2 segun sea el caso
  # Retuen value to PHP via stdout
  tr <- bdata2$x$Tr[which(bdata2$x$QID==QID)[1]] #Chequeo para revisr si el QID ya estaba antes
  
} else {
  # update the data.frame
  part.data2 <- rbind(part.data2,
                      data.frame(QID=args[1],
                                 DigiCit=DigiCit+rnorm(1,sd=.001), #ciudadanía digital
                                 HomoIndex=HomoIndex+rnorm(1,sd=.001))) # Homofilia
  # update the seqblock objects
  n.idx <- nrow(part.data2)
  bdata2 <- seqblock2k(object.name ="bdata2",
                       id.vals = part.data2[n.idx,"QID"],
                       covar.vals = part.data2[n.idx,-c(1)],
                       verbose = FALSE)
  
  tr2 <- bdata2$x$Tr[length(bdata2$x$Tr)] 
  
  # Save data
  save(mahal,seqblock1,seqblock2k,bdata2,part.data2,file="new2.RData")
}


#envio de datos a qualtrics
to_qs<-c(tr1, tr2)

cat(sprintf("%s,%s", to_qs[1], to_qs[2]))






