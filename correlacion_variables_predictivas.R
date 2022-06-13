######DIRECTORIO DE TRABAJO######
setwd("F:/tesis/CLUSTER")
#################################
#################################
#INSTALACION Y CARGA DE PAQUETES#
#################################
#################################
install.packages("rgeos", dep=TRUE)
install.packages("HH", dep=TRUE)
install.packages("ape", dep=TRUE)

######CARGA LAS LIBRERIAS NECESARIAS#####
library(raster) #TRABAJO CON DATOS RASTER
library(HH) #VARIANCE INFLATION FACTOR
library(rgeos) #OPERACIONES GEOM√âTRICAS CON INFO GEOGR√ÅFICA
library(dismo) #LIBRERIA PARA MODELOS DE DISTRIBUCION
library(ape) #LIBRERIA PARA graficas dendrogramas
##########################################
##########################################

######################################################
######################################################
#ANALISIS DE CORRELACION DE LAS VARIABLES PREDICTIVAS#
######################################################
######################################################

################################################
#CREA EL DIRECTORIO PARA GUARDAR LOS RESULTADOS#
################################################
dir.create("correlacion")

#################################################
##############LISTADO DE VARIABLES###############
#################################################
lista.variables <- list.files(path="./Variablesp",pattern='*.asc', full.names=TRUE)
lista.variables
extent(lista.variables)

##################################################
###########stack Y brick# PREPARAN LAS VARIABLES EN UN UNICO OBJETO ESPACIAL
####################################################
variables <- stack(lista.variables)

#################################################
########TRANSFORMA LOS MAPAS EN UNA TABLA########
#################################################

variables.tabla<-as.data.frame(variables)

#################################################
#############ELIMINA LOS VALORES NULOS###########
#################################################
variables.tabla<-na.omit(variables.tabla)


##################################################
###############MATRIZ DE CORRELACION##############
##################################################
help(cor)
variables.correlacion<-cor(variables.tabla)

###################################################
###########MATRIZ DE DISTANCIAS################# ('ABS' = VALOR ABSOLUTO, PARA ELIMINAR CORRELACIONES NEGATIVAS)
###################################################
help(as.dist)
help(abs)
variables.dist<-as.dist(abs(variables.correlacion))


#####################################################
#CLUSTER DE VARIABLES SEGUN LA DISTANCIA# (MENOR DISTANCIA = MAYOR CORRELACI√ìN)
######################################################
help(hclust)
variables.cluster<-hclust(variables.dist)
plot(variables.cluster, hang = -1, xlab="Variables", main="Cluster de correlaciÛn entre variables")

######################################################
######################################################
###################ANALISIS DE CODO###################
######################################################
######################################################
wss <- (nrow(variables.tabla)-1)*sum(apply(variables.tabla,2,var))
  for (i in 2:19) wss[i] <- sum(kmeans(variables.tabla,
                                       centers=i)$withinss)
plot(1:19, wss, type="b", xlab="N˙mero de clusters",
     ylab="Grupos suma de cuadrados")

######################################
######################################
#GRAFICO DEL CLUSTER DE CORRELACIONES#
######################################
######################################
lx <-c(20,0.82) 
ly <-c(0.82,0.82) 
######################################################
#######Dibujar cluster con linea de corte#############
######################################################
par(col.axis = "blue",font=1, cex=0.6)
plot(variables.cluster, hang = -1, xlab="Variables", main="Cluster de correlaciÛn entre variables")
points (lx,ly, type="l", col="red")


################################################
################################################
############GRAFICO EXPORTADO A PDF#############
################################################
################################################

pdf("./con_altura_2070_85.pdf", width=12, height=12, pointsize=20)
par(col.axis = "blue",font=1, cex=0.6)
plot(variables.cluster, hang = -1, xlab="Variables", main="Cluster de correlaciÛn entre variables")
points (lx,ly, type="l", col="red")

plot(1:19, wss, type="b", xlab="N˙mero de clusters",
     ylab="Grupos suma de cuadrados")
dev.off()



