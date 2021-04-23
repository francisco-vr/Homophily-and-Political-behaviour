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
#args <-c("QI22","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1")
args <-c("QI44","9","8","9","8","5","6","10","5","5","3","6","7","7","7","7","4","5","1","4","5","6","5","7","7")


#ID

QID = args[1]

#### Data Management

#Cambio de nombres en bdata

names(bdata$orig) <-c("QID","DigiCit","HomoIndex","Tr")
names(bdata$x) <-c("QID", "DigiCit","HomoIndex","Tr")


#Cambiar de caracter a número

args<-as.numeric(args)

#### Ciudadanía Digital

DigitCount <-sum(args[2:15])

bdata$orig$DigiCit <-ifelse(DigitCount<=56,"0",
                            ifelse(DigitCount>=57,"1"))

bdata$x$DigiCit <-ifelse(DigitCount<=56,"0",
                         ifelse(DigitCount>=57,"1"))


# Homofilia política

HomoCount <-sum(args[15:21])

bdata$orig$HomoIndex <-ifelse(HomoCount<=39,"0",
                              ifelse(HomoCount>=40,"1"))

bdata$x$HomoIndex <-ifelse(HomoCount<=39,"0",
                           ifelse(HomoCount>=40,"1"))


 #### Block Randomization

###### Concentrarse acá

if(sum(part.data$QID %in% QID)>0){
  # Retuen value to PHP via stdout
  tr <- bdata$x$Tr[which(bdata$x$QID==QID)[1]]
  
} else {
  # update the data.frame
  part.data <- rbind(part.data, 
                     data.frame(QID=args[6], 
                                DigitalCit=as.numeric(args[1])+rnorm(1,sd=.001), #ciudadanía digital
                                HomoIndex=as.numeric(args[2])+rnorm(1,sd=.001))) # Homofilia
  # update the seqblock objects
  n.idx <- nrow(part.data)
  bdata <- seqblock2k(object.name = "bdata", 
                      id.vals = part.data[n.idx, "QID"],  
                      covar.vals = part.data[n.idx,-c(1)], 
                      verbose = FALSE)
  
  tr <- bdata$x$Tr[length(bdata$x$Tr)]
  
  # Save data
  save(mahal,seqblock1,seqblock2k,bdata,part.data,file="/var/www/r.cess.cl/public_html/sp/new.RData")
}


####Hasta acá

tr<-strsplit(tr,split = ",")[[1]]
tr<-as.numeric(tr)

load("/var/www/r.cess.cl/public_html/sp/nuevaBDfinal.RData")


#if(length(args) != 5){
#  stop()
#}

genderQ<-args[1]   ## Genero
econQ<-args[2]    ## SES
QID <- args[6]



#### list of treatment functions
#### Random selection of treatment without replacement
selected<-c(namedVF[tr[1]],namedVF[tr[2]])
selectedQID<-names(selected) ## list of selected treatments to send to Qualtrics


### executing treatments
selected[[1]](gender, econ, pairvct[1], pair, v=1)

selected[[2]](gender, econ, pairvct[2], pair, v=2)


#### Payment lists for treatments

fcn.payment <- function(gender, econ, mode, pair, selectedTreat){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  
  payment<-all.files[all.files$id==id, c("razon_social", "val_uf_pension", "val_pesos_pension" ,"VPN")]
  
  mn<-15-nrow(payment)
  
  payment[nrow(payment)+mn,] <- NA
  
  # treatment if condition for payment$opcion  order
  payment<-if(selectedTreat=="control") {payment[order(-payment$val_uf_pension),]
  } else if (selectedTreat=="treat1") { payment[order(-payment$val_pesos_pension),]
  } else if (selectedTreat=="treat2") { payment[order(-payment$val_pesos_pension),]
  } else if (selectedTreat=="treat3") { payment[order(-payment$VPN),]
  } else {payment[order(-payment$VPN),]}
  
  payment$opcion<-1:15
  
  payment$VPN<-ifelse(is.na(payment$VPN), 0, payment$VPN )
  
  n <- 15
  payment$pay<- ifelse(payment$VPN==0, 0, 
                       ifelse(payment$VPN==max(payment$VPN, na.rm=T), 1500, 
                              ifelse(payment$VPN==sort(payment$VPN,partial=n-1)[n-1], 1400,
                                     ifelse(payment$VPN==sort(payment$VPN,partial=n-2)[n-2], 1200, 
                                            ifelse(payment$VPN==sort(payment$VPN,partial=n-3)[n-3], 900,
                                                   ifelse(payment$VPN==sort(payment$VPN,partial=n-4)[n-4], 500, 
                                                          ifelse(payment$VPN==sort(payment$VPN,partial=n-5)[n-5], 100, 
                                                                 ifelse(payment$VPN==sort(payment$VPN,partial=n-6)[n-6], 50, 0
                                                                 ) ) ) ) ) ) ) )
  
  
  row.names(payment)<-payment$opcion
  pay.list<-payment[, "pay"]
  pay.list <- as.list(as.data.frame(t(pay.list)))
  
  return(pay.list)
}

pay.op1<-fcn.payment(gender, econ, pairvct[1], pair, selectedQID[1])

pay.op2<-fcn.payment(gender, econ, pairvct[2], pair, selectedQID[2])


### Largo Opciones por tratamiento
fcn.opcion <- function(gender, econ, mode, pair, selectedTreat){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  
  tbl<-all.files[all.files$id==id, c("razon_social", "VPN")]
  length<-nrow(tbl)
  return(length)
}

length1<-fcn.opcion(gender, econ, pairvct[1], pair)
length2<-fcn.opcion(gender, econ, pairvct[2], pair)


#envio de datos a qualtrics
to_qs<-c(pay.op1, pay.op2, selectedQID, length1, length2)
cat(sprintf("%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s", to_qs[1], to_qs[2], to_qs[3], to_qs[4], to_qs[5], to_qs[6], to_qs[7], to_qs[8], to_qs[9], to_qs[10], to_qs[11],to_qs[12], to_qs[13], to_qs[14], to_qs[15], to_qs[16], to_qs[17], to_qs[18], to_qs[19], to_qs[20], to_qs[21],to_qs[22], to_qs[23], to_qs[24], to_qs[25], to_qs[26], to_qs[27], to_qs[28], to_qs[29], to_qs[30], to_qs[31], to_qs[32], to_qs[33], to_qs[34]))





