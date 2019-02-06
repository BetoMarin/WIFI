install.packages("anytime")
install.packages("esquisse")
install.packages("spatial")
install.packages("caret")
install.packages("plotly")
install.packages("dplyr")
install.packages("plyr")
#install.packages("spData")
install.packages("reshape2")
install.packages("spdep")
install.packages("ggplot2")
install.packages("dataCompareR")
install.packages("arsenal")
install.packages("randomForest")
install.packages("e1071")
install.packages("tidyverse")
install.packages("kknn")
library(anytime)
library(spatial)
library(spData)
library(spdep)
library(ggplot2)
library(plotly)
library(dplyr)
library(reshape2)
library(esquisse)
library(caret)
library(plyr)
library(dataCompareR)
library(arsenal)
library(randomForest)
library(e1071)
library(kknn)
library(tidyverse)

#############################################################################

# DATA CLEANING

#############################################################################
####### cargo las dos tablas training y validation


wifitraining <- read.table("trainingData.csv", sep=",", header=TRUE)
valwifi <- read.table("validationData.csv", sep=",", header=TRUE)

summary(wifitraining[521:529])
summary(valwifi[521:529])

######## Cambio el formato del tiempo para que sea entendible

time <- anytime(wifitraining$TIMESTAMP)
wifitraining$time <- time

summary(wifitraining[521:530])
str(valwifi105[360:376])

time <- anytime(valwifi$TIMESTAMP)
valwifi$time <- time

summary(valwifi[521:530])
str(wifitraining[521:530])


###############  ploteo las coordenadas

nc.coord <- cbind(wifitraining$LONGITUDE,wifitraining$LATITUDE, wifitraining$BUILDINGID)

plot(nc.coord)

## PARA VER EL NUMERO DE REGISTROS QUE HAY REPARTIDOS ENTRE LOS EDIFICIOS Y SUS PISOS  #######

buildingdistrib <- wifitraining %>%
  group_by(BUILDINGID) %>%


ggplot(data = buildingdistrib) +
  aes(x = BUILDINGID, y = FLOOR, fill = n, size = n) +
  geom_point(color = '#0c4c8a') +
  theme_minimal()


#######  IDENTIFICAR LAS FINGERPRINTS QUE SUMAN 52000 QUE SON LAS QUE NO HAN DADO SEÑAL A NADIE O SOLO A 2 FP #########

# crea una columna que se llame ifp en la tabla wifitraining que sea la suma de las columnas 1 a la 520
wifitraining$ifp <- rowSums(wifitraining[1:520])

valwifi$ifp <- rowSums(valwifi[1:520])

# muestrame el resumen de las columans 519 a 531 para comprobar que se ha creado la columna ifp
summary(wifitraining[519:531])
summary(valwifi[519:531])

# crea una nueva tabla (que se llama igual que la tabla original para reescribirla) que extraiga solo las filas que el valor de la columna ipf sea diferente de 52000
wifitraining <- wifitraining %>%
  filter(ifp < 51900)

valwifi <- valwifi %>%
  filter(ifp < 51900)

##################  IDENTIFICAR DUPLICADOS ##############################

# Duplicados en trainning

wifitraining$duplicados <-  duplicated(wifitraining, by = WAPS)

wifitraining_dupli <- wifitraining

wifitraining_dupli <- filter(wifitraining_dupli, wifitraining_dupli$duplicados == TRUE)

head(wifitraining_dupli[470:478],35)
unique(wifitraining_dupli$USERID)
unique(wifitraining_dupli$PHONEID)
unique(wifitraining_dupli$builandfloor)

# Duplicados en validation

valwifi105$duplicados <-  duplicated(valwifi, by = WAPS)

valwifi105_dupli <- valwifi105

valwifi105_dupli <- filter(valwifi105_dupli, valwifi105_dupli$duplicados == TRUE)

head(valwifi105_dupli[470:478],35)
unique(valwifi105_dupli$USERID)
unique(valwifi105_dupli$PHONEID)
unique(wifitraining_dupli$builandfloor)






wifitraining_sin_dupli <- unique(wifitraining)

head(wifitraining_sin_dupli[500:532])

###################### IDENTIFICAR FINGERPRINTS QUE TIENEN MENOS DE 2 O MENOS SEÑALES EMITIDAS ###########################

# lo quiero hacer pq una de las predicciones con más error venía de una fp que solo tenía señal de 2 waps.


##########  IDENTIFICAR QUE WAPS NO HAN DADO SEÑAL Y ELIMNAR  ###########

# para probar una nueva funcion nos sugiere utilizar la funcion nearzerovar que compara la variación los valores de dentro de una misma columna 

#Arguments
#x = a numeric vector or matrix, or a data frame with all numeric data
#freqCut = el valor de corte para la relación del valor más común al segundo valor más común
#uniqueCut = el punto de corte para el porcentaje de valores distintos del número de muestras totales
#saveMetrics = a logical. Si es falso, se devuelven las posiciones de los predictores cero o casi cero. Si es verdadero, se devuelve un marco de datos con información del predictor.
#names = a logical. Si es falso, se devuelven los índices de columna. Si es verdadero, se devuelven los nombres de columna.
#foreach = ¿Debería usarse el paquete foreach para los cálculos? Si es VERDADERO, debe usarse menos memoria.
#allowParallel = ¿Debería usarse el procesamiento paralelo a través del paquete foreach para los cálculos? Si es VERDADERO, se utilizará más memoria pero el tiempo de ejecución debería ser más corto.
#y = a factor vector with at least two levels
#index = a list. Each element corresponds to the training set samples in x for a given resample

#wapsinutiles <- nearZeroVar(wifitraining)
#class(wapsinutiles)
#str(wapsinutiles)
#wapsinutiles
#rm(wapsinutiles)
#wapsinutiles %>%
 # filter(zeroVar==TRUE)

#devuelve esto

                     #freqRatio percentUnique zeroVar   nzv
#WAP001            2480.375000   0.030209959   FALSE  TRUE
#WAP002            1984.200000   0.015104980   FALSE  TRUE
#WAP003               0.000000   0.005034993    TRUE  TRUE
#WAP004               0.000000   0.005034993    TRUE  TRUE
#WAP005            1415.785714   0.050349932   FALSE  TRUE
#WAP006             592.515152   0.181259755   FALSE  TRUE
#WAP007             550.942857   0.261819647   FALSE  TRUE

# compruebo que el el wap 3 solo tiene valores igual a 100

summary(wifitraining[1:5])

wapsinutiles <- nearZeroVar(wifitraining, saveMetrics = TRUE)
tablawapsinutiles <- wapsinutiles[wapsinutiles$zeroVar==TRUE,]
class(tablawapsinutiles)
names.inutiles <- row.names(tablawapsinutiles)
dim(select(wifitraining,-names.inutiles))
wifitraining <- select(wifitraining,-names.inutiles)

wapsinutilesval <- nearZeroVar(valwifi, saveMetrics = TRUE)
tablawapsinutilesval <- wapsinutilesval[wapsinutilesval$zeroVar==TRUE,]
names.inutilesval <- row.names(tablawapsinutilesval)
dim(select(valwifi,-names.inutilesval))
valwifi <- select(valwifi,-names.inutilesval)

#### Merge Building & Floor  ##################

wifitraining$builandfloor <- as.numeric(paste(wifitraining$BUILDINGID, wifitraining$FLOOR, sep = ""))
summary(wifitraining[465:477])
head(wifitraining[465:477]) 

valwifi$builandfloor <- as.numeric(paste(valwifi$BUILDINGID, valwifi$FLOOR, sep = ""))


############### Cambiar el 100 por -105  #####################

wifitraining105 <- wifitraining
valwifi105 <- valwifi

# no funciona revalue(wifitraining105, c("100"="-105"))

# no funciona replace(wifitraining(wifitraining105$WAP001 == 100, -105))

# si funciona pero solo sirve para una columna
#wifitraining$WAP001[which(wifitraining$WAP001==100)] <- -105

# no funciona 
#wifitraining105[ ,1:465][which(wifitraining105[,1:465])] <-"-105"

# esto si que funciiona pero no he mirado si había algun 100 en las columnas del final asi que no debería valer
#wifitraining105[ wifitraining105 == 100 ] <- "-105"
#summary(wifitraining[464:477])

# ifelse si se cumple la condición x = 100, entonces x será igual a -105 y sino cumple la condición x será igual x esta x no es real no la escribe.
#wifitraining.105 <- as.data.frame(apply(wifitraining105[,1:465], 2,function(x) ifelse(x == 100,x <--105, x<-x)))

#wifitraining$WAP001[which(wifitraining$WAP001==100)] <- "-105"


#View(wifitraining105[455:467])

### for loop que busca en las columnas 1 a 465 el valor 100 y lo sustituye por -105

for (i in 1:465){
  
  wifitraining105[,i][which(wifitraining105[,i]==100)] <- -105
  # me convirte la clase de la columna a character en vez de numeric
  #wifitraining105[,i]as.numeric(wifitraining105[,i])
  
}   

summary(valwifi105[368:376])


for (i in 1:368){
  
  valwifi105[,i][which(valwifi105[,i]==100)] <- -105
  
} 

#apply(valwifi105, 2, function(x) x == 100, -105, X)


str(wifitraining105[1:30])
str(wifitraining105[465:477])
summary(wifitraining105[1:47])


summary(wifitraining105[455:477])
summary(wifitraining105[466:477])
unique(wifitraining105)

################### compararar la columnas de ambas dataset para elimnar las que no comparten #########################

#str(valwifi105)
#wifitraining105

colval <- colnames(valwifi105)
coltrain <- colnames(wifitraining105)

comunes <- intersect(colval,coltrain)

#rm(comuneswaps)

comuneswapstrain <- wifitraining105[comunes]
comuneswapsval <- valwifi105[comunes]

#coltrainval <- cbind(colval$colnames(valwifi105), coltrain$colnames(wifitraining105))

#trainandval <- bind_cols(coltrain, colval, .id=NULL)

#?bind_cols



#rCompare(valwifi105,wifitraining105, keys = NA)


#matchColumns(valwifi105, wifitraining105)

#dataCompareR::matc

#summary(compare(valwifi105, wifitraining105, by=y))

# no funciona, a pesar de haber cargado el package y la library no reconoce la funcion


#c <- cbind(b[, which(colnames(b)%in% colnames(a))],
#           a[, which(colnames(a)%in% colnames(b))])

#trainandval <- cbind(wifitraining105[, which(colnames(wifitraining105)%in% colnames(valwifi105))],
#                     valwifi105[, which(colnames(valwifi105)%in% colnames(wifitraining105))])

#Error in data.frame(..., check.names = FALSE) : 
#  arguments imply differing number of rows: 19861, 1111

#########################################
############# plotear mucho ##############
##########################################

# Numero de señales emitidas por cada wap


señales_wap_01 <- as.data.frame(sum(wifitraining105$WAP001 != -105))
colnames(señales_wap_01)
señales_wap_01 <- rename(señales_wap_01, c("sum(wifitraining105$WAP001 != -105)" = "WAP001"))

#comprobar que hay 18 datos por debajo de -105

wifitraining105 %>%
  select(WAP001) %>%
  arrange(desc(WAP001))

apply(wifitraining105, 2, function(x) sum(x != -105))

senales_por_wap <- as.data.frame(apply(wifitraining105, 2, function(x) sum(x != -105)))
colnames(senales_por_wap)
senales_por_wap <- rename(senales_por_wap, c("apply(wifitraining105, 2, function(x) sum(x != -105))"="WAP001"))

wifitraining_solo_wap <- as.data.frame(filter(senales_por_wap, WAP001 < 6000))

esquisser(wifitraining_solo_wap)

ggplot(data = wifitraining_solo_wap) +
  aes(y = WAP001, x = seq_along(WAP001)) +
  geom_line(color = '#0c4c8a') +
  labs(title = 'Numero de señales enviadas por WAP',
       x = 'WAP',
       y = 'Nº de señales') +
  theme_minimal()

# Numero de señales recibidas por FP

señales_por_fp <- as.data.frame(apply(wifitraining105, 1, function(x) sum(x != -105)))
señales_por_fp <- rename(señales_por_fp, c("apply(wifitraining105, 1, function(x) sum(x != -105))"="Num_Fingerprint_signal"))
head(señales_por_fp)

esquisser(señales_por_fp)

ggplot(data = señales_por_fp) +
  aes(y = Num_Fingerprint_signal, x = seq_along(Num_Fingerprint_signal)) +
  geom_point(color = '#0c4c8a') +
  labs(title = 'Nº de señales recibidas por fingerprint',
       x = 'Fingerprint',
       y = 'Nº de señales') +
  theme_minimal()

#Numero de señales en cada edificio

wapsinutiles <- data.frame(nearZeroVar(wifitraining, saveMetrics = TRUE))

dfwapsinutiles <- sort(wapsinutiles$percentUnique, decreasing = FALSE)

obsxedif <- wifitraining105 %>%
  group_by(BUILDINGID) %>%
  

esquisser(obsxedif)

ggplot(data = obsxedif) +
  aes(x = BUILDINGID, y = n) +
  geom_point(color = '#0c4c8a') +
  labs(title = 'Nº de observaciones por edificio',
       x = 'Building',
       y = 'Floor') +
  theme_minimal()

# Numero de observaciones por edificio y por planta

obsxedifyplanta <- wifitraining105 %>%
  group_by(BUILDINGID, FLOOR) %>%
  count_(WAPS < -105)
esquisser(obsxedifyplanta)

ggplot(data = obsxedifyplanta) +
  aes(x = BUILDINGID, y = FLOOR, size = n) +
  geom_point(color = '#1f9e89') +
  labs(title = 'Nº de señales emitidas en cada piso y planta') +
  theme_minimal()

# Numero de observaciones por user

obsxuser <- wifitraining105 %>%
  group_by(USERID, BUILDINGID, FLOOR) %>%
  count_(WAPS < -105)

prueba <- wifitraining105 %>%
  filter(USERID==8, BUILDINGID==1, FLOOR==3)

prueba2 <- prueba %>%
  group_by(BUILDINGID) %>%
  summarise(freq = n())
rlang::last_error()

  count(prueba$WAP016 > -105)


# Numero de observaciones por phone

wifitraining105 %>%
  group_by(PHONEID) %>%
  count_(WAPS < -105)

# Numero de observaciones por wap de mas de -105

esquisser(obsxedif)

esquisser(data=wifitraining105)

WAPS <- grep("WAP",names(wifitraining105),value=TRUE)

ggplot(data = wifitraining105) +
  aes(x = BUILDINGID, y = FLOOR, size = WAPS) +
  geom_point(color = '#0c4c8a') +
  theme_minimal()



# ver los waps de cada tipo de movil.

GTI81601 <- wifitraining[which(wifitraining$PHONEID == 1),]

GTI81601 <- wifitraining105 %>%
  select(1:477) %>%
  filter(PHONEID == 1)

#######ayuda de ignacio#######
  
split(wifitraining,wifitraining$PHONEID)
split(wifitraining,wifitraining$PHONEID)[[1]][,1:465] %>% summary()

kvec <- c()
for (i in 1:length(unique(wifitraining105$PHONEID))) {
   cat("PHONEID: ",i)
   #j <- unique(wifitraining$PHONEID)[i]
   kk <- as.data.frame(split(wifitraining105,wifitraining105$PHONEID)[[i]][1:520] %>% summary() )
   colnames(kk) <- c("PHONEID","WAP","Freq")
   kk$PHONEID <- unique(wifitraining$PHONEID)[i]
   kvec <- rbind(kvec,kk)
   rm(kk)
   #print(kk)
}   

# plot el valor min y max de cada wap con cada tipo de movil


GTI81601 <- wifitraining105 %>%
  filter(PHONEID == 1) %>%
  group_by(BUILDINGID)

summary(wifitraining105[450:477])
summary(GTI81601[1:20])
summary(GTI81601[450:477])

GTI81601MIN <- as.data.frame(apply(GTI81601, 2, min))
colnames(GTI81601MIN)[1] <- "min"
GTI81601MAX <- as.data.frame(apply(GTI81601, 2, max))

BGTI <- as.data.frame(apply(GTI81601, 2, max))

colnames(GTI81601MAX)[1] <- "max"
nameswap <- row.names(GTI81601MIN)

GTIMINMAX <- as.data.frame(cbind(nameswap, GTI81601MIN$min, GTI81601MAX$max))

GTIMINMAX <- rename(GTIMINMAX, c("nameswap"="WAP", "V2"="MIN", "V3"="MAX"))

View(GTI81601MIN)


ggplot(GTI81601MIN, aes(x=(1:10)))+
  labs(x="WAP", y="MIN") +
  ggtitle("INTENSIDAD MINIMA POR WAP")



#WAPS <- grep(“WAP”,names(wifilocation),value=TRUE)
#wifilocation$max_wap <- colnames(wifilocation[,WAPS])[apply(wifilocation[,WAPS],1,which.max)]


#WAPS <- grep("WAP",names(wifitraining),value=TRUE)
#wifitraining$max_wap <- colnames(wifitraining[,WAPS])[apply(wifitraining[,WAPS],1,which.max)]
#wifitraining$max_value <- colnames(wifitraining[,WAPS])[apply(wifitraining[,WAPS],1,which.max)]


summary(wifitraining[465:478])
head(wifitraining[465:478])  
  



#################  mirar las intensidades de wap por telfono o por usuario   ###################






############## identificar el peso de los moviles en las muestras de trainning y val ###############

peso_x_moviltrain <- wifitraining105 %>%
  group_by(PHONEID) %>%
  tally() #counts

esquisser(peso_x_movil)

ggplot(data = peso_x_movil) +
  aes(x = PHONEID, y = n) +
  geom_point(color = '#0c4c8a') +
  labs(title = 'Number of fingerprints per PHONEID IN TRAINNING') +
  theme_minimal()

peso_x_moviltest <- wifitraining105 %>%
  group_by(PHONEID) %>%
  tally() #counts

esquisser(peso_x_moviltest)

ggplot(data = peso_x_moviltest) +
  aes(x = PHONEID, y = n) +
  geom_point(color = '#0c4c8a') +
  labs(title = 'Number of fingerprints per PHONEID IN TEST') +
  theme_minimal()

# EL MOVIL 13 Y 14 TIENEN UN PESO MUCHO MAYOR EN LAS DATASET HAY QUE VER SI TIENEN BUENAS SEÑALES

#train
phone13train <- filter(comuneswapstrain, comuneswapstrain$PHONEID == 13)
phone14train <- filter(comuneswapstrain, comuneswapstrain$PHONEID == 14)

unique(phone13y14train$BUILDINGID)
unique(phone13y14train$builandfloor)


phone13y14train <- bind_rows(phone13train, phone14train)

summary(phone13y14train[465:479])

#test
phone13test <- filter(comuneswapsval, comuneswapsval$PHONEID == 13)
phone14test <- filter(comuneswapsval, comuneswapsval$PHONEID == 14)

phone13y14test <- bind_rows(phone13test, phone14test)

summary(phone13y14test[375:382])

######  Eliminar las waps que alguna de sus señales tienen valores superiores a -30 #############

summary(comuneswapstrain[1:25])

summary(comuneswapstrain[312:321])

# creo una tabla de solo los waps para que pueda sacar el la intensidad de señal maxima de los waps 

solowaps <- comuneswapstrain[1:313]

# añado una columna en la dataset de comunes que me indique la señal maxima

comuneswapstrain$maxsignal <- apply(solowaps, 1, max)

summary(comuneswapstrain[312:322])

# creo una dataset nueva con solo los fingerprints con señales maximas inferiores a -30

comuneswapstrainsignal <- filter(comuneswapstrain, comuneswapstrain$maxsignal < -30)

summary(comuneswapstrainsignal[312:322])

# hago lo mismo en validation

summary(comuneswapsval[312:321])

WAPS <- grep("WAP",names(comuneswapsval),value=TRUE)

solowapstest <- comuneswapsval[WAPS]

comuneswapsval$maxsignal <- apply(solowapstest, 1, max)

summary(comuneswapsval[312:322])

comuneswapstestsignal <- filter(comuneswapsval, comuneswapsval$maxsignal < -30)

summary(comuneswapstrainsignal[312:322])

comuneswapsval <- comuneswapstestsignal
comuneswapstrain <- comuneswapstrainsignal

################## fingerprints que solo tienen 2 señales waps ##########################

summary(comuneswapstrain[312:323])
View(comuneswapstrain[312:323])

comuneswapstrain$num_señales <- apply(comuneswapstrain[1:313], 1, function(x) sum(x != -105))

comprobacion <- filter(comuneswapstrain, num_señales == 2| num_señales == 3 & maxsignal < -90)

class(comprobacion$num_señales)

comprobacion$num_señales <- as.factor(comprobacion$num_señales)

View(comprobacion[312:323])
esquisser(comprobacion)

ggplot(data = comprobacion) +
  aes(x = LATITUDE, y = LONGITUDE, color = builandfloor) +
  geom_point() +
  scale_color_distiller(palette = "Paired") +
  labs(title = 'Fingerprints with low RSSI and 3 or less signals') +
  theme_minimal()

##########  fingerprints que solo tienen 2 o 3 señales ##############

summary(comuneswapsval[312:322])
View(comuneswapstrain[312:323])

comuneswapstrain$num_señales <- apply(comuneswapstrain[1:313], 1, function(x) sum(x != -105))

comprobacion <- filter(comuneswapstrain, num_señales == 2| num_señales == 3 & maxsignal < -90)

class(comprobacion$num_señales)

comprobacion$num_señales <- as.factor(comprobacion$num_señales)

View(comprobacion[312:322])
esquisser(comprobacion)



#####################   intentar localizar los waps   #########################

waploc <- filter(comuneswapsval)

summary(waploc[312:322])
waploc <- waploc[313:322]
summary(waploc[1:10])
















##############################################

#APLICACION DE MODELOS

#############################################


#APPLY RANDOM FOREST TO PREDICT BUILDING

# training dataset
summary(comuneswapstrain[312:321])
trainpredbuiltding <- comuneswapstrain[-c(313,314,315,317,318:321)]
summary(trainpredbuiltding[300:313])
str(trainpredbuiltding[300:313])
trainpredbuiltding$BUILDINGID <- as.factor(trainpredbuiltding$BUILDINGID)
  
  
  
#testing datset

summary(comuneswapsval[312:321])
testpredbuiltding <- comuneswapsval[-c(313,314,315,317,318:321)]
summary(testpredbuiltding[300:313])
testpredbuiltding$BUILDINGID <- as.factor(testpredbuiltding$BUILDINGID)


#predbuilding <- train(BUILDINGID~., data = trainpredbuiltding, method = "parRF", metric = "RMSE", ntree = 10)
set.seed(123)
system.time(rfpredbuilding <- randomForest(BUILDINGID ~ ., trainpredbuiltding, ntree=300))

testpredbuilding <- predict(rfpredbuilding, newdata = testpredbuiltding)

rfpredbuilding <- testpredbuilding
rfpredbuilding

valwifi105$BUILDINGID <- as.factor(valwifi105$BUILDINGID)
actualbuilding <- valwifi105$BUILDINGID

predandactualbuilding <- data.frame(rfpredbuilding, actualbuilding)

predandactualbuilding$rfpredbuilding <- as.integer(predandactualbuilding$rfpredbuilding)
predandactualbuilding$actualbuilding <- as.integer(predandactualbuilding$actualbuilding)
str(predandactualbuilding)

predandactualbuilding$errors <- predandactualbuilding$rfpredbuilding-predandactualbuilding$actualbuilding

str(predandactualbuilding)

esquisser(predandactualbuilding)

ggplot(data = predandactualbuilding) +
  aes(x = errors) +
  geom_histogram(bins = 30, fill = '#0c4c8a') +
  labs(title = 'Error in building') +
  theme_minimal()

confusionMatrix(rfpredbuilding, valwifi105$BUILDINGID)

#######################################################
#######################################################

#APPLY RANDOM FOREST TO PREDICT FLOOR

# training dataset
summary(comuneswapstrain[312:321])
trainpredfloor <- comuneswapstrain[-c(313,314,316,317,318:321)]
summary(trainpredfloor[300:313])
str(trainpredfloor[300:313])
trainpredfloor$FLOOR <- as.factor(trainpredfloor$FLOOR)
str(trainpredfloor[300:313])


#testing datset

summary(comuneswapsval[312:321])
testpredfloor <- comuneswapsval[-c(313,314,316,317,318:321)]
summary(testpredfloor[300:313])
testpredfloor$FLOOR <- as.factor(testpredfloor$FLOOR)

set.seed(123)
system.time(rfpredfloor <- randomForest(FLOOR ~ ., trainpredfloor, ntree=100))

rftestpredfloor <- predict(rfpredfloor, newdata = testpredfloor)

rfpredfloor <- rftestpredfloor
rfpredfloor

valwifi105$FLOOR <- as.factor(valwifi105$FLOOR)
actualfloor <- valwifi105$FLOOR

predandactualfloor <- data.frame(rfpredfloor, actualfloor)

predandactualfloor$rfpredfloor <- as.integer(predandactualfloor$rfpredfloor)
predandactualfloor$actualfloor <- as.integer(predandactualfloor$actualfloor)
str(predandactualfloor)

predandactualfloor$errors<-predandactualfloor$rfpredfloor-predandactualfloor$actual
str(predandactualfloor)

esquisser(predandactualfloor)

ggplot(data = predandactualfloor) +
  aes(x = errors) +
  geom_histogram(bins = 18, fill = '#0c4c8a') +
  labs(title = 'Errors in FLOOR') +
  theme_minimal()

confusionMatrix(rfpredfloor, valwifi105$FLOOR)

#######################################################
#######################################################

#APPLY RANDOM FOREST TO PREDICT BUILDING-FLOOR

# training dataset

summary(comuneswapstrain[312:323])
trainpredbf <- comuneswapstrain[-c(313:320,322,323)]
summary(trainpredbf[300:313])
str(trainpredbf[300:313])
trainpredbf$builandfloor <- as.factor(trainpredbf$builandfloor)
str(trainpredbf[300:313])

unique(trainpredbf$builandfloor)

#testing datset

summary(comuneswapsval[312:322])
testpredbf <- comuneswapsval[-c(313:320,322)]
summary(testpredbf[300:313])
testpredbf$builandfloor <- as.factor(testpredbf$builandfloor)

unique(testpredbf$builandfloor)

set.seed(123)
system.time(predbf <- randomForest(builandfloor ~ ., trainpredbf, ntree=300))

summary(testpredbf[300:314])

testpredbf <- predict(predbf, newdata = testpredbf)

rfpredbf <- testpredbf
rfpredbf

valwifi105$builandfloor <- as.factor(valwifi105$builandfloor)
actualbuilandfloor <- valwifi105$builandfloor
unique(actualbuilandfloor)
#str(predandactualbf$rfpredbf)
#str(predandactualbf$actual)

predandactualbf <- data.frame(rfpredbf, actualbuilandfloor)
str(predandactualbf)

esquisser(predandactualbf)

confusionMatrix(rfpredbf, valwifi105$builandfloor)

# creo una tabla que contiene los wapscomunes de validation y añado la columna de predicción 

predbferror <- cbind(comuneswapsval, predandactualbf$rfpredbf)
colnames(predbferror)

# por la confusion matrix he identificado que hay errores en varios casos, por el momemnto me voy a fijar en el building 1 piso 1

error_edif_1_planta_1_2_y_3 <- filter(predbferror, predbferror$builandfloor == 11)


summary(error_edif_1_planta_1_2_y_3[300:323])
str(error_edif_1_planta_1_2_y_3[300:322])
class(error_edif_1_planta_1_2_y_3$builandfloor)
class(error_edif_1_planta_1_2_y_3$"predandactualbf$rfpredbf")
View(error_edif_1_planta_1_2_y_3[310:323])
unique(error_edif_1_planta_1_2_y_3$"predandactualbf$rfpredbf")

names(error_edif_1_planta_1_2_y_3)[323] <- "predictions"

error_edif_1_planta_1_2_y_3$error <- ifelse(error_edif_1_planta_1_2_y_3$predictions == 11, "Good prediction", 
                                            ifelse(error_edif_1_planta_1_2_y_3$predictions == 13, "Mistaken of 2 floors",
                                                   ifelse(error_edif_1_planta_1_2_y_3$predictions == 12, "Mistaken for the floor above","Mistaken for the floor below")))


esquisser(error_edif_1_planta_1)

ggplot(data = error_edif_1_planta_1_2_y_3) +
  aes(x = LONGITUDE, y = LATITUDE, color = error) +
  geom_point() +
  scale_colour_manual(values = c("Good prediction" = "darkgreen", "Mistaken of 2 floors" = "red", "Mistaken for the floor above" = "orange", "Mistaken for the floor below" = "lightblue")) +
  labs(title = "Visualización de errores en la predicción de pisos del building 1 en el piso 1") +
  theme_minimal()



#error11 <- filter(error_edif_1_planta_1_2_y_3, error_edif_1_planta_1_2_y_3$error != 0)

#unique(error11$PHONEID)
#[1]  0  5 13 20 12  2  9 15  4 (no parece haber un error especifico....)


# creo una nueva tabla para visualizar los errores del edificio 1 en la planta 0

error_buildandfloor_10 <- filter(predbferror, predbferror$builandfloor == 10)

# cambio de nombre la columna de predicciones

names(error_buildandfloor_10)[323] <- "predictions"

# añado un columna que me calcule la diferencia entre predicho y real para ello primero tengo que saber las variantes posibles

summary(error_buildandfloor_10[310:323])

unique(error_buildandfloor_10$predictions)

error_buildandfloor_10$error <- ifelse(error_buildandfloor_10$predictions == 10, "Good prediction", 
                                            ifelse(error_buildandfloor_10$predictions == 13, "Mistaken of 3 floors above",
                                                   ifelse(error_buildandfloor_10$predictions == 12, "Mistaken of 2 floors above","Mistaken for the floor above")))

View(error_buildandfloor_10[310:323])

ggplot(data = error_buildandfloor_10) +
  aes(x = LONGITUDE, y = LATITUDE, color = error) +
  geom_point() +
  scale_colour_manual(values = c("Good prediction" = "darkgreen", "Mistaken of 3 floors above" = "red", "Mistaken of 2 floors above" = "orange", "Mistaken for the floor above" = "lightblue")) +
  labs(title = "Visualización de errores en la predicción de pisos del building 1 en el piso 0") +
  theme_minimal()


error_buildandfloor_24 <- filter(predbferror, predbferror$builandfloor == 24)

# cambio de nombre la columna de predicciones

names(error_buildandfloor_24)[323] <- "predictions"

# añado un columna que me calcule la diferencia entre predicho y real para ello primero tengo que saber las variantes posibles

summary(error_buildandfloor_24[310:323])

unique(error_buildandfloor_24$predictions)

error_buildandfloor_24$error <- ifelse(error_buildandfloor_24$predictions == 24, "Good prediction", 
                                       ifelse(error_buildandfloor_24$predictions == 21, "Mistaken of 3 floors below","Mistaken for the floor below"))
                                            

View(error_buildandfloor_24[310:323])

ggplot(data = error_buildandfloor_24) +
  aes(x = LONGITUDE, y = LATITUDE, color = error) +
  geom_point() +
  scale_colour_manual(values = c("Good prediction" = "darkgreen", "Mistaken of 3 floors below" = "red", "Mistaken for the floor below" = "lightblue")) +
  labs(title = "Visualización de errores en la predicción de pisos del building 2 en el piso 4") +
  theme_minimal()

# intento descubrir si hay alguna relación entre los errores y los phoneid

error_buildandfloor_10 %>%
  filter(error != "Good prediction") %>%
  group_by(error) %>%
  summarise(PHONEID, predictions, error) 

#Parece que el movil que mas errores tiene es el 13 (HTC Wildfire S)

# creo una nueva tabla para visualizar los errores del edificio 0 en la planta 0

error_buildandfloor_0 <- filter(predbferror, predbferror$builandfloor == 0)

# cambio de nombre la columna de predicciones

names(error_buildandfloor_0)[323] <- "predictions"

# añado un columna que me calcule la diferencia entre predicho y real para ello primero tengo que saber las variantes posibles

View(error_buildandfloor_0[310:322])

unique(error_buildandfloor_0$predictions)

error_buildandfloor_0$error <- ifelse(error_buildandfloor_0$predictions == 0, "Good prediction", 
                                       ifelse(error_buildandfloor_0$predictions == 1, "Mistaken for the floor above","Mistaken of 2 floors above"))
                                              

View(error_buildandfloor_0[310:323])

ggplot(data = error_buildandfloor_0) +
  aes(x = LONGITUDE, y = LATITUDE, color = error) +
  geom_point() +
  scale_colour_manual(values = c("Good prediction" = "darkgreen", "Mistaken for the floor above" = "orange", "Mistaken of 2 floors above" = "red")) +
  labs(title = "Visualización de errores en la predicción de pisos del building 0 en el piso 0") +
  theme_minimal()

# intento descubrir si hay alguna relación entre los errores y los phoneid

error_buildandfloor_0 %>%
  filter(error != "Good prediction") %>%
  group_by(predictions) %>%
  summarise(PHONEID, predictions, error) 

# El movil que da porblemas es el nº 20 (Nexus 4)


# creo una nueva tabla para visualizar los errores del edificio 0 en la planta 1

error_buildandfloor_1 <- filter(predbferror, predbferror$builandfloor == 1)

# cambio de nombre la columna de predicciones

names(error_buildandfloor_1)[323] <- "predictions"

# añado un columna que me calcule la diferencia entre predicho y real para ello primero tengo que saber las variantes posibles

View(error_buildandfloor_1[310:322])

unique(error_buildandfloor_1$predictions)

error_buildandfloor_1$error <- ifelse(error_buildandfloor_1$predictions == 1, "Good prediction", 
                                      ifelse(error_buildandfloor_1$predictions == 2, "Mistaken for the floor above",
                                      ifelse(error_buildandfloor_1$predictions == 0, "Mistaken for the floor below","Mistaken of floor and building")))


View(error_buildandfloor_1[310:323])

ggplot(data = error_buildandfloor_1) +
  aes(x = LONGITUDE, y = LATITUDE, color = error) +
  geom_point() +
  scale_colour_manual(values = c("Good prediction" = "darkgreen", "Mistaken for the floor above" = "lightblue", "Mistaken for the floor below" = "orange", "Mistaken of floor and building" = "red")) +
  labs(title = "Visualización de errores en la predicción de pisos del building 0 en el piso 1") +
  theme_minimal()

# intento descubrir si hay alguna relación entre los errores y los phoneid

error_buildandfloor_1 %>%
  filter(error != "Good prediction") %>%
  group_by(predictions) %>%
  summarise(PHONEID, predictions, error) 

# Investigar el ifp 51651

filter(error_buildandfloor_1, error_buildandfloor_1$ifp == 51651)


# creo una nueva tabla para visualizar los errores del edificio 0 en la planta 2

error_buildandfloor_2 <- filter(predbferror, predbferror$builandfloor == 2)

# cambio de nombre la columna de predicciones

names(error_buildandfloor_2)[323] <- "predictions"

# añado un columna que me calcule la diferencia entre predicho y real para ello primero tengo que saber las variantes posibles

View(error_buildandfloor_2[310:323])

unique(error_buildandfloor_2$predictions)

error_buildandfloor_2$error <- ifelse(error_buildandfloor_2$predictions == 2, "Good prediction", 
                                      ifelse(error_buildandfloor_2$predictions == 0, "Mistaken of 2 floors below","Mistaken for the floor below"))


View(error_buildandfloor_2[310:323])

ggplot(data = error_buildandfloor_2) +
  aes(x = LONGITUDE, y = LATITUDE, color = error) +
  geom_point() +
  scale_colour_manual(values = c("Good prediction" = "darkgreen", "Mistaken for the floor below" = "orange", "Mistaken of 2 floors below" = "red")) +
  labs(title = "Visualización de errores en la predicción de pisos del building 0 en el piso 2") +
  theme_minimal()

# intento descubrir si hay alguna relación entre los errores y los phoneid

error_buildandfloor_2 %>%
  filter(error != "Good prediction") %>%
  group_by(predictions) %>%
  summarise(PHONEID, predictions, error) 

# De los 3 erroes 2 han sido del phoneid 20 y 1 del phoneid 0

FP49924 <-filter(error_buildandfloor_2, error_buildandfloor_2$ifp == 49924)
FP49924

# Este fingerprint esta bien tiene 10 señales y de buena intensidad, esta posicionado justo en medio del edificio y se ha equivocado en 2 plantas

#######################################################
#######################################################

#SPLIT DATA SET IN THE 3 BUILDINGS AND APPLY RANDOM FOREST IN EACH ONE

# training dataset building 2

summary(comuneswapstrain[312:321])
head(comuneswapstrain)
#trainpredbuilding0 <- as.data.frame(filter(comuneswapstrain, comuneswapstrain$BUILDINGID == 0))
#trainpredbuilding1 <- as.data.frame(filter(comuneswapstrain, comuneswapstrain$BUILDINGID == 1))
trainpredfloorbuild2 <- as.data.frame(filter(comuneswapstrain, comuneswapstrain$BUILDINGID == 2))

summary(trainpredfloorbuild2[312:315])
trainpredfloorbuild2 <- trainpredfloorbuild2[-c(314,315,316)]
summary(trainpredfloorbuild2[300:313])
str(trainpredfloorbuild2[300:313])
trainpredfloorbuild2$FLOOR <- as.factor(trainpredfloorbuild2$FLOOR)

#testing datset building 2

summary(comuneswapsval[312:321])
head(comuneswapsval)
#testpredbuilding0 <- as.data.frame(filter(comuneswapsval, comuneswapsval$BUILDINGID == 0))
#testpredbuilding1 <- as.data.frame(filter(comuneswapsval, comuneswapsval$BUILDINGID == 1))
testpredfloorbuild2 <- as.data.frame(filter(comuneswapsval, comuneswapsval$BUILDINGID == 2))

summary(testpredfloorbuild2[312:315])
testpredfloorbuild2 <- testpredfloorbuild2[-c(314,315,316)]
summary(testpredfloorbuild2[300:313])
#testpredfloorbuild2$FLOOR <- as.factor(testpredfloorbuild2$FLOOR)
class(testpredfloorbuild2)

# establezco el set.seed
set.seed(123)
# pongo la funcion de cronometro y entreno el modelo randomForest para predecir FLOOR en base a una dataset que solo tiene building 2
system.time(predfloorbuild2 <- randomForest(FLOOR ~ ., trainpredfloorbuild2, ntree=100))
# pruebo el modelo con la dataframe test
testpredicfloorbuild2 <- predict(predfloorbuild2, newdata = testpredfloorbuild2)

rfpredfloorbuild2 <- testpredicfloorbuild2
rfpredfloorbuild2
################################
#valwifi105$builandfloor <- as.factor(valwifi105$builandfloor)
#actualbuilandfloor <- valwifi105$builandfloor
#predandactualbf <- data.frame(rfpredbf, actualbuilandfloor)
#predandactualbf$rfpredbf <- as.integer(predandactualbf$rfpredbf)
#predandactualbf$actualbuilandfloor <- as.integer(predandactualbf$actualbuilandfloor)
#str(predandactualbf)
#predandactualbf$errors<-predandactualbf$rfpredbf-predandactualbf$actual
#############################

testpredfloorbuild2$FLOOR <- as.factor(testpredfloorbuild2$FLOOR)
actualfloorbuild2 <- testpredfloorbuild2$FLOOR

predandactualfloorbuild2 <- data.frame(rfpredfloorbuild2, actualfloorbuild2)

predandactualfloorbuild2$rfpredfloorbuild2 <- as.integer(predandactualfloorbuild2$rfpredfloorbuild2)
predandactualfloorbuild2$actualfloorbuild2 <- as.integer(predandactualfloorbuild2$actualfloorbuild2)
str(predandactualfloorbuild2)

predandactualfloorbuild2$errors<-predandactualfloorbuild2$rfpredfloorbuild2-predandactualfloorbuild2$actualfloorbuild2

esquisser(predandactualbuilding)

library(ggplot2)

ggplot(data = predandactualbuilding) +
  aes(x = errors) +
  geom_histogram(bins = 30, fill = '#0c4c8a') +
  labs(title = 'Errores para predecir FLOOR sólo del edificio 2') +
  theme_minimal()

confusionMatrix(rfpredfloorbuild2, actualfloorbuild2)

#Reference
#Prediction   0   1   2   3   4
#0  21   1   0   0   0
#1   3 109   5   0   1
#2   0   1  42   0   0
#3   0   0   7  37   8
#4   0   0   0   3  30

#Overall Statistics

#Accuracy : 0.8918          
#95% CI : (0.8483, 0.9263)
#No Information Rate : 0.4142          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.852     

# training dataset building 1

summary(comuneswapstrain[312:323])
head(comuneswapstrain)
#trainpredfloorbuilding0 <- as.data.frame(filter(comuneswapstrain, comuneswapstrain$BUILDINGID == 0))
trainpredfloorbuild1 <- as.data.frame(filter(comuneswapstrain, comuneswapstrain$BUILDINGID == 1))
#trainpredfloorbuild2 <- as.data.frame(filter(comuneswapstrain, comuneswapstrain$BUILDINGID == 2))

summary(trainpredfloorbuild1[310:323])
trainpredfloorbuild1 <- trainpredfloorbuild1[-c(313,314,316,317,318:323)]
summary(trainpredfloorbuild1[300:313])
str(trainpredfloorbuild1[300:313])
trainpredfloorbuild1$FLOOR <- as.factor(trainpredfloorbuild1$FLOOR)

#testing datset building 1

summary(comuneswapsval[312:322])
head(comuneswapsval)
#testpredfloorbuild0 <- as.data.frame(filter(comuneswapsval, comuneswapsval$BUILDINGID == 0))
testpredfloorbuild1 <- as.data.frame(filter(comuneswapsval, comuneswapsval$BUILDINGID == 1))
#testpredfloorbuild2 <- as.data.frame(filter(comuneswapsval, comuneswapsval$BUILDINGID == 2))

summary(testpredfloorbuild1[310:322])
testpredfloorbuild1 <- testpredfloorbuild1[-c(313,314,316,317,318:322)]
summary(testpredfloorbuild1[300:313])
testpredfloorbuild1$FLOOR <- as.factor(testpredfloorbuild1$FLOOR)
#testpredfloorbuild2$FLOOR <- as.factor(testpredfloorbuild2$FLOOR)
#class(testpredfloorbuild1)

# establezco el set.seed
set.seed(123)
# pongo la funcion de cronometro y entreno el modelo randomForest para predecir FLOOR en base a una dataset que solo tiene building 2
system.time(predfloorbuild1 <- randomForest(FLOOR ~ ., trainpredfloorbuild1, ntree=100))
# pruebo el modelo con la dataframe test
testpredicfloorbuild1 <- predict(predfloorbuild1, newdata = testpredfloorbuild1)

rfpredfloorbuild1 <- testpredicfloorbuild1
rfpredfloorbuild1

#class(testpredfloorbuild1)
#testpredfloorbuild2 <- as.data.frame(testpredfloorbuild2, row.names = TRUE)




#testpredfloorbuild2$FLOOR <- testpredfloorbuild2
actualfloorbuild1 <- testpredfloorbuild1$FLOOR

#plotpredandactualbuilding2 <- plot(predandactual$actual, predandactual$rfpredictions)

#predandactualfloorbuild1 <- as.data.frame(rfpredfloorbuild1, actualfloorbuild1)

#predandactualfloorbuild1$errors<-predandactualfloorbuild1$rfpredfloorbuild1-predandactualfloorbuild1$actualfloorbuild1

#esquisser(predandactualbuilding)

confusionMatrix(rfpredfloorbuild1, actualfloorbuild1)

# training dataset building 0

summary(comuneswapstrain[312:321])
head(comuneswapstrain)
trainpredfloorbuild0 <- as.data.frame(filter(comuneswapstrain, comuneswapstrain$BUILDINGID == 0))
#trainpredfloorbuild1 <- as.data.frame(filter(comuneswapstrain, comuneswapstrain$BUILDINGID == 1))
#trainpredfloorbuild2 <- as.data.frame(filter(comuneswapstrain, comuneswapstrain$BUILDINGID == 2))

summary(trainpredfloorbuild0[312:323])
trainpredfloorbuild0 <- trainpredfloorbuild0[-c(313,314,316,317,318:323)]
summary(trainpredfloorbuild0[300:313])
str(trainpredfloorbuild0[300:313])
trainpredfloorbuild0$FLOOR <- as.factor(trainpredfloorbuild0$FLOOR)

#testing datset building 0

summary(comuneswapsval[312:321])
head(comuneswapsval)
testpredfloorbuild0 <- as.data.frame(filter(comuneswapsval, comuneswapsval$BUILDINGID == 0))
#testpredfloorbuild0 <- as.data.frame(filter(comuneswapsval, comuneswapsval$BUILDINGID == 1))
#testpredfloorbuild2 <- as.data.frame(filter(comuneswapsval, comuneswapsval$BUILDINGID == 2))

summary(testpredfloorbuild0[312:322])
testpredfloorbuild0 <- testpredfloorbuild0[-c(313,314,316,317,318:322)]
summary(testpredfloorbuild0[300:313])
#testpredfloorbuild2$FLOOR <- as.factor(testpredfloorbuild2$FLOOR)
#class(testpredfloorbuild1)

# establezco el set.seed
set.seed(123)
# pongo la funcion de cronometro y entreno el modelo randomForest para predecir FLOOR en base a una dataset que solo tiene building 2
system.time(predfloorbuild0 <- randomForest(FLOOR ~ ., trainpredfloorbuild0, ntree=100))
# pruebo el modelo con la dataframe test
testpredicfloorbuild0 <- predict(predfloorbuild0, newdata = testpredfloorbuild0)

rfpredfloorbuild0 <- testpredicfloorbuild0
rfpredfloorbuild0

#class(testpredfloorbuild1)
#testpredfloorbuild2 <- as.data.frame(testpredfloorbuild2, row.names = TRUE)


testpredfloorbuild0$FLOOR <- as.factor(testpredfloorbuild0$FLOOR)

#testpredfloorbuild2$FLOOR <- testpredfloorbuild2
actualfloorbuild0 <- testpredfloorbuild0$FLOOR

#plotpredandactualbuilding2 <- plot(predandactual$actual, predandactual$rfpredictions)

#predandactualfloorbuild1 <- as.data.frame(rfpredfloorbuild1, actualfloorbuild1)

#predandactualfloorbuild1$errors<-predandactualfloorbuild1$rfpredfloorbuild1-predandactualfloorbuild1$actualfloorbuild1

#esquisser(predandactualbuilding)

confusionMatrix(rfpredfloorbuild0, actualfloorbuild0)


#####################################################################
#####################################################################


# SVM  para predecir latitud

# training dataset
summary(comuneswapstrain[312:321])
trainpredlat <- comuneswapstrain[-c(313,315,316:321)]
summary(trainpredlat[300:313])
str(trainpredlat[300:313])

#testing datset

summary(comuneswapsval[312:321])
testpredlat <- comuneswapsval[-c(313,315,316:321)]
summary(testpredlat[300:313])

set.seed(123)

system.time(predlat <- svm(LATITUDE ~ ., trainpredlat))

testpredlat <- predict(predlat, newdata = testpredlat)

rfpredlat <- testpredlat
rfpredlat

actuallat <- valwifi105$LATITUDE


#plotpredandactualbuilding <- plot(predandactual$actual, predandactual$rfpredictions)

#predandactualbuilding <- data.frame(rfpredbuilding, actual)

#predandactualbuilding$errors<-predandactualbuilding$rfpredbuilding-predandactualbuilding$actual

#esquisser(predandactualbuilding)


postResample(rfpredlat, valwifi105$LATITUDE)

# SVM  para predecir latitud con edificio

# training dataset
summary(comuneswapstrain[312:321])
trainpredlated <- comuneswapstrain[-c(313,315,317:321)]
summary(trainpredlated[300:314])
str(trainpredlated[300:314])
trainpredlated$BUILDINGID <- as.factor(trainpredlated$BUILDINGID)

#testing datset

summary(comuneswapsval[312:321])
testpredlated <- comuneswapsval[-c(313,315,317:321)]
summary(testpredlated[300:314])
testpredlated$BUILDINGID <- as.factor(testpredlated$BUILDINGID)

set.seed(123)

system.time(predlated <- svm(LATITUDE ~ ., trainpredlated))

testpredlated <- predict(predlated, newdata = testpredlated)

rfpredlated <- testpredlated
rfpredlated

actuallated <- valwifi105$LATITUDE


#plotpredandactualbuilding <- plot(predandactual$actual, predandactual$rfpredictions)

#predandactualbuilding <- data.frame(rfpredbuilding, actual)

#predandactualbuilding$errors<-predandactualbuilding$rfpredbuilding-predandactualbuilding$actual

#esquisser(predandactualbuilding)


postResample(rfpredlated, valwifi105$LATITUDE)

# SVM  para predecir longitud con edificio

# training dataset
summary(comuneswapstrain[312:321])
trainpredlong <- comuneswapstrain[-c(314,315,317:321)]
summary(trainpredlong[300:314])
str(trainpredlong[300:314])
trainpredlong$BUILDINGID <- as.factor(trainpredlong$BUILDINGID)

#testing datset

summary(comuneswapsval[312:321])
testpredlong <- comuneswapsval[-c(314,315,317:321)]
summary(testpredlong[300:314])
testpredlong$BUILDINGID <- as.factor(testpredlong$BUILDINGID)

set.seed(123)

system.time(predlong <- svm(LONGITUDE ~ ., trainpredlong))

svmtestpredlong <- predict(predlong, newdata = testpredlong)

svmpredlong <- svmtestpredlong
svmpredlong

actuallong <- valwifi105$LONGITUDE


#plotpredandactualbuilding <- plot(predandactual$actual, predandactual$rfpredictions)

#predandactualbuilding <- data.frame(rfpredbuilding, actual)

#predandactualbuilding$errors<-predandactualbuilding$rfpredbuilding-predandactualbuilding$actual

#esquisser(predandactualbuilding)


postResample(svmpredlong, actuallong)


# KNN  para predecir latitud con edificio

# training dataset
summary(comuneswapstrain[312:321])
trainpredlated <- comuneswapstrain[-c(313,315,317:321)]
summary(trainpredlated[300:314])
str(trainpredlated[300:314])
trainpredlated$BUILDINGID <- as.factor(trainpredlated$BUILDINGID)

#testing datset

summary(comuneswapsval[312:321])
testpredlated <- comuneswapsval[-c(313,315,317:321)]
summary(testpredlated[300:314])
testpredlated$BUILDINGID <- as.factor(testpredlated$BUILDINGID)

set.seed(123)

system.time(knn.latitud <- train.kknn(LATITUDE~., trainpredlated, kernel = "rectangular"))

knntestpredlated <- predict(knn.latitud, newdata = testpredlated)

knnpredlated <- knntestpredlated
knnpredlated

knnactuallated <- valwifi105$LATITUDE


#plotpredandactualbuilding <- plot(predandactual$actual, predandactual$rfpredictions)

#predandactualbuilding <- data.frame(rfpredbuilding, actual)

#predandactualbuilding$errors<-predandactualbuilding$rfpredbuilding-predandactualbuilding$actual

#esquisser(predandactualbuilding)


postResample(knnpredlated, valwifi105$LATITUDE)

# visualización de los errores con todos los edificios

valwifi105$latpredictions <- knntestpredlated

summary(valwifi105[370:377])

valwifi105$laterror <- valwifi105$LATITUDE - valwifi105$latpredictions

summary(valwifi105[368:378])

head(valwifi105[368:378])

#listado de los errores de latidud ordenado de menor a mayor

valwifi105ordenado <- valwifi105[order(valwifi105$laterror),]
head(valwifi105ordenado[368:378], 35)

# el phoneid 13 y 20 siguen saliento mucho

#listado de los errores de latidud ordenado de mayor a menor

valwifi105ordenadodemasamenos <- valwifi105[order(-valwifi105$laterror),]
head(valwifi105ordenadodemasamenos[368:378], 35)

phone13 <- filter(wifitraining105, wifitraining105$PHONEID == 13)

valwifi105ordenadodemasamenos <- valwifi105[order(-valwifi105$laterror),]
head(phone13[450:478], 35)


esquisser(valwifi105)

ggplot(data = valwifi105) +
  aes(x = laterror) +
  geom_histogram(bins = 30, fill = '#0c4c8a') +
  labs(title = 'Visulización de los errores de latitud por finger print',
       x = 'Error de latitud') +
  theme_minimal()

# para segmentar mejor el error lo separo por edificio 0

valwifi105$latpredictions <- knntestpredlated

valwifi105edif0 <- filter(valwifi105, valwifi105$BUILDINGID == 0)

summary(valwifi105edif0[370:377])

valwifi105edif0$laterror <- valwifi105edif0$LATITUDE - valwifi105edif0$latpredictions

summary(valwifi105edif0[370:378])

esquisser(valwifi105edif0)

ggplot(data = valwifi105edif0) +
  aes(x = laterror) +
  geom_histogram(bins = 30, fill = '#0c4c8a') +
  labs(title = 'Error de latitud en el edificio 0',
       x = 'Error de latitud') +
  theme_minimal()

# 

# para segmentar mejor el error lo separo por edificio 1

valwifi105$latpredictions <- knntestpredlated

valwifi105edif1 <- filter(valwifi105, valwifi105$BUILDINGID == 1)

summary(valwifi105edif1[370:377])

valwifi105edif1$laterror <- valwifi105edif1$LATITUDE - valwifi105edif1$latpredictions

summary(valwifi105edif1[370:378])

esquisser(valwifi105edif1)

ggplot(data = valwifi105edif1) +
  aes(x = laterror) +
  geom_histogram(bins = 30, fill = '#0c4c8a') +
  labs(title = 'Error de latitud en el edificio 1',
       x = 'Latitud error') +
  theme_minimal()

# para segmentar mejor el error lo separo por edificio 2

valwifi105$latpredictions <- knntestpredlated

valwifi105edif2 <- filter(valwifi105, valwifi105$BUILDINGID == 2)

summary(valwifi105edif2[370:377])

valwifi105edif2$laterror <- valwifi105edif2$LATITUDE - valwifi105edif2$latpredictions

summary(valwifi105edif2[370:378])

esquisser(valwifi105edif2)

ggplot(data = valwifi105edif2) +
  aes(x = laterror) +
  geom_histogram(bins = 30, fill = '#0c4c8a') +
  labs(title = 'Error de latitud en el edificio 2',
       x = 'Error latitud') +
  theme_minimal()

# KNN  para predecir latitud sin edificio

# training dataset
summary(comuneswapstrain[312:321])
trainpredlated <- comuneswapstrain[-c(313,315,316,317:321)]
summary(trainpredlated[300:313])
str(trainpredlated[300:313])


#testing datset

summary(comuneswapsval[312:321])
testpredlated <- comuneswapsval[-c(313,315,316,317:321)]
summary(testpredlated[300:313])


set.seed(123)

system.time(knn.latitud <- train.kknn(LATITUDE~., trainpredlated, kernel = "rectangular"))

knntestpredlated <- predict(knn.latitud, newdata = testpredlated)

knnpredlated <- knntestpredlated
knnpredlated

knnactuallated <- valwifi105$LATITUDE


#plotpredandactualbuilding <- plot(predandactual$actual, predandactual$rfpredictions)

#predandactualbuilding <- data.frame(rfpredbuilding, actual)

#predandactualbuilding$errors<-predandactualbuilding$rfpredbuilding-predandactualbuilding$actual

#esquisser(predandactualbuilding)


postResample(knnpredlated, valwifi105$LATITUDE)

# visualización de los errores con todos los edificios

valwifi105$latpredictions <- knntestpredlated

summary(valwifi105[370:377])

valwifi105$laterror <- valwifi105$LATITUDE - valwifi105$latpredictions

summary(valwifi105[368:378])

head(valwifi105[368:378])



# KNN  para predecir longitud con edificio

# training dataset
summary(comuneswapstrain[312:321])
trainpredlong <- comuneswapstrain[-c(314,315,317:321)]
summary(trainpredlong[300:314])
str(trainpredlong[300:314])
trainpredlong$BUILDINGID <- as.factor(trainpredlong$BUILDINGID)

#testing datset

summary(comuneswapsval[312:321])
testpredlong <- comuneswapsval[-c(314,315,317:321)]
summary(testpredlong[300:314])
testpredlong$BUILDINGID <- as.factor(testpredlong$BUILDINGID)

set.seed(123)

system.time(knn.longitude <- train.kknn(LONGITUDE~., trainpredlong, kernel = "rectangular"))

knntestpredlong <- predict(knn.longitude, newdata = testpredlong)

knnpredlong <- knntestpredlong
knnpredlong

class(knnpredlong)

knnactuallong <- valwifi105$LONGITUDE



#plotpredandactualbuilding <- plot(predandactual$actual, predandactual$rfpredictions)

#predandactualbuilding <- data.frame(rfpredbuilding, actual)

#predandactualbuilding$errors<-predandactualbuilding$rfpredbuilding-predandactualbuilding$actual

#esquisser(predandactualbuilding)


postResample(knnpredlong, valwifi105$LONGITUDE)


# RANDOM FOREST  para predecir latitud sin edificio

# training dataset
summary(comuneswapstrain[312:323])
trainpredlated <- comuneswapstrain[-c(313,315:323)]
summary(trainpredlated[300:313])
str(trainpredlated[300:313])


#testing datset

summary(comuneswapsval[312:322])
testpredlated <- comuneswapsval[-c(313,315,316,317:322)]
summary(testpredlated[300:313])


set.seed(123)
system.time(rf_latitud <- randomForest(LATITUDE ~ ., trainpredlated, ntree=100))
rf_latitud_prediction <- predict(rf_latitud, newdata = testpredlated)
postResample(rf_latitud_prediction, valwifi105$LATITUDE)


valwifi105$rf_latitud_prediction <- rf_latitud_prediction
summary(valwifi105[370:379])
valwifi105$rflaterror <- valwifi105$LATITUDE - valwifi105$rf_latitud_prediction
summary(valwifi105[370:380])
esquisser(valwifi105)

ggplot(data = valwifi105) +
  aes(x = rflaterror) +
  geom_histogram(bins = 30, fill = '#0c4c8a') +
  labs(title = 'Error de latitude en todos los edificios (RF)') +
  theme_minimal()

# RANDOM FOREST  para predecir longitud sin edificio

summary(comuneswapstrain[312:323])
trainpredlong <- comuneswapstrain[-c(314,315:323)]
summary(trainpredlong[300:313])


#testing datset

summary(comuneswapsval[312:322])
testpredlong <- comuneswapsval[-c(314,315:322)]
summary(testpredlong[300:313])



set.seed(123)
system.time(rf_longitude <- randomForest(LONGITUDE ~ ., trainpredlong, ntree=100))
rf_longitude_prediction <- predict(rf_longitude, newdata = testpredlong)
postResample(rf_longitude_prediction, valwifi105$LONGITUDE)


valwifi105$rf_longitude_prediction <- rf_longitude_prediction
summary(valwifi105[370:379])
valwifi105$rflongerror <- valwifi105$LONGITUDE - valwifi105$rf_longitude_prediction
summary(valwifi105[370:381])
esquisser(valwifi105)

ggplot(data = valwifi105) +
  aes(x = rflongerror) +
  geom_histogram(bins = 30, fill = '#0c4c8a') +
  labs(title = 'Error de longitude en todos los edificios (RF)') +
  theme_minimal()

esquisser(valwifi105)

ggplot(data = valwifi105) +
  aes(x = rf_longitude_prediction, y = latpredictions, color = FLOOR) +
  geom_point() +
  labs(title = 'Latitude and longitude prediction by floor') +
  theme_minimal()

ggplot(data = valwifi105) +
  aes(x = rf_longitude_prediction, y = rf_latitud_prediction) +
  geom_point(color = '#e41a1c') +
  labs(title = 'Predicted latitude and longitude by floor') +
  theme_minimal() +
  facet_wrap(valwifi105$FLOOR)



ggplot(data = valwifi105) +
  aes(x = LONGITUDE, y = LATITUDE) +
  geom_point(color = '#35b779') +
  labs(title = 'Real latitude and longitud by floor') +
  theme_minimal() +
facet_wrap(valwifi105$FLOOR)

# RANDOM FOREST  para predecir longitud CON edificio

summary(comuneswapstrain[312:323])
trainpredlong <- comuneswapstrain[-c(314,315,317,318:323)]
summary(trainpredlong[300:314])


#testing datset

summary(comuneswapsval[312:322])
testpredlong <- comuneswapsval[-c(314,315,317:322)]
summary(testpredlong[300:314])



set.seed(123)
system.time(rf_longitudeconedif <- randomForest(LONGITUDE ~ ., trainpredlong, ntree=100))
rf_longitude_prediction_con_edif <- predict(rf_longitudeconedif, newdata = testpredlong)
postResample(rf_longitude_prediction_con_edif, valwifi105$LONGITUDE)


valwifi105$rf_longitude_prediction_con_edif <- rf_longitude_prediction_con_edif
summary(valwifi105[370:379])
valwifi105$rflongerrorconedif <- valwifi105$LONGITUDE - valwifi105$rf_longitude_prediction_con_edif
summary(valwifi105[370:381])
esquisser(valwifi105)

ggplot(data = valwifi105) +
  aes(x = rflongerrorconedif) +
  geom_histogram(bins = 30, fill = '#0c4c8a') +
  labs(title = 'Error de longitude en todos los edificios (RF)') +
  theme_minimal()

esquisser(valwifi105)

ggplot(data = valwifi105) +
  aes(x = rf_longitude_prediction, y = latpredictions, color = FLOOR) +
  geom_point() +
  labs(title = 'Latitude and longitude prediction by floor') +
  theme_minimal()

ggplot(data = valwifi105) +
  aes(x = rf_longitude_prediction, y = rf_latitud_prediction) +
  geom_point(color = '#e41a1c') +
  labs(title = 'Predicted latitude and longitude by floor') +
  theme_minimal() +
  facet_wrap(valwifi105$FLOOR)



ggplot(data = valwifi105) +
  aes(x = LONGITUDE, y = LATITUDE) +
  geom_point(color = '#35b779') +
  labs(title = 'Real latitude and longitud by floor') +
  theme_minimal() +
  facet_wrap(valwifi105$FLOOR)


# RANDOM FOREST  para predecir latitud con edificio

# training dataset
summary(comuneswapstrain[312:323])
trainpredlated <- comuneswapstrain[-c(313,315,317:323)]
summary(trainpredlated[300:314])
str(trainpredlated[300:314])


#testing datset

summary(comuneswapsval[312:322])
testpredlated <- comuneswapsval[-c(313,315,317:322)]
summary(testpredlated[300:314])


set.seed(123)
system.time(rf_latitudconedif <- randomForest(LATITUDE ~ ., trainpredlated, ntree=100))
rf_latitud_predictionconedif <- predict(rf_latitudconedif, newdata = testpredlated)
postResample(rf_latitud_predictionconedif, valwifi105$LATITUDE)


valwifi105$rf_latitud_predictionconedif <- rf_latitud_predictionconedif
summary(valwifi105[370:379])
valwifi105$rflaterror <- valwifi105$LATITUDE - valwifi105$rf_latitud_predictionconedif
summary(valwifi105[370:380])
esquisser(valwifi105)

ggplot(data = valwifi105) +
  aes(x = rflaterror) +
  geom_histogram(bins = 30, fill = '#0c4c8a') +
  labs(title = 'Error de latitude en todos los edificios con edificio (RF)') +
  theme_minimal()
  

# RANDOM FOREST  para predecir longitud sin edificio y solo con los moviles 13 y 14

#summary(phone13y14train[312:321])
#trainpredlong13y14 <- phone13y14train[-c(314,315:321)]
#summary(trainpredlong13y14[300:313])


#testing datset

#summary(phone13y14test[312:321])
#testpredlong13y14 <- phone13y14test[-c(314,315:321)]
#summary(testpredlong13y14[300:313])



#set.seed(123)
#system.time(rf_longitude13y14 <- randomForest(LONGITUDE ~ ., trainpredlong13y14, ntree=150))
#rf_longitude_prediction13y14 <- predict(rf_longitude13y14, newdata = testpredlong13y14)
#postResample(rf_longitude_prediction13y14, phone13y14test$LONGITUDE)


#phone13y14test$rf_longitude_prediction13y14 <- rf_longitude_prediction13y14
#summary(valwifi105[370:379])
#phone13y14test$rflongerror13y14 <- phone13y14test$LONGITUDE - phone13y14test$rf_longitude_prediction13y14
#summary(valwifi105[370:381])
#esquisser(phone13y14test)

#ggplot(data = phone13y14test) +
#  aes(x = rflongerror13y14) +
#  geom_histogram(bins = 30, fill = '#0c4c8a') +
#  labs(title = 'Error de longitude en todos los edificios pero solo de los moviles 13 y 14 (RF)') +
#  theme_minimal()

buscar infor de Vector field


