################################################# 
### Block Ranndomization 
### Abril 2021
### Denise Laroze / Francisco Villarroel
################################################

args <- commandArgs(TRUE)

#No incluir el llamado a librerias individuales, sino la carpeta donde estÃÂÃÂ¡n instaladas
.libPaths=("/usr/lib64/R/library/")

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
setwd("~/GitHub/Master_Thesis/Code")

#Parameters
path<-"/var/www/r.cess.cl/public_html/sp/"

####################################################################
###########################block randmisation####################### 

#Editar al final cuando tengamos claridad de numero de argumentos
if(args[6] == "reset_database"){
  time <- Sys.time()
  time <- gsub("[:alph:]", "", time)
  time <- gsub(" ", "_", time)
  
  file.copy("/var/www/r.cess.cl/public_html/sp/new.RData", sprintf("rdata_bak_%s.Rdata", time))
  file.copy("/var/www/r.cess.cl/public_html/sp/new_orig.RData", "new.RData", overwrite = T)
  stop()
}

#args <- as.vector(t(sim.data[i, ]))
if(length(args) != 3){
  stop()
}

# Load data
#load(file="/var/www/r.cess.cl/public_html/sp/new.RData")
load(file="new.RData")

QID = args[1]

#### Data Management

#Homofilia

args<-c("Xvar1", "Xvar2", "Xvar3", "Xvar4", "Xvar5", "Xvar6", "Xvar7") #valor observado de todos 

#valor observado de cada item
digital1<-args[1]  
digital2<-args[2]  
digital3<-args[3]  
digital4<-args[4]
digital5<-args[5]
digital6<-args[6]
digital7<-args[7]
digital8<-args[8]
digital9<-args[9]
digital10<-args[10]
digital11<-args[11]
digital12<-args[12]
digital13<-args[13]
digital14<-args[14]

homofilia1<-args[15]
homofilia2<-args[16] 
homofilia3<-args[17]
homofilia4<-args[18]
homofilia5<-args[19]
homofilia6<-args[20]
homofilia7<-args[21]


#Cambio de nombres en bdata

names(bdata$orig) <-c("QID","DigiCit","HomoIndex","Tr")
names(bdata$x) <-c("QID", "DigiCit","HomoIndex","Tr")


#Cambiar de caracter a número

args<-as.numeric(args)

#### Funcion de agregar itemes

DigitCount <-digital1+digital2+digital3+digital4+digital5+digital6+digital7+digital8+digital9+digital10+digital11
+digital12+digital13+digital14

DigiCit <-function(x){
  if (DigitCount <= 56) {
    0
  } else {
    1
  }
}


HomoIndex <-function(x){
  if (HomoCount <= 39) {
    0
  } else {
    1
  }
}


#producir un valor único por persona




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





