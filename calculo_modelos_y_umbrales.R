#CARGAR LIBRERIAS
library(sp)
library(raster)
library(dismo)
setwd("E:/tesis/PRUEBA R-MAXENT")

#Cargar datos
occs <- read.csv(file.choose()) #Datos de presencia
#Ver datos
View(occs)
#Cargar capas
layers <- stack(list.files("./capas","asc",full.names=TRUE))
#Graficar capa
plot(layers[[2]])
#Poner los puntos sobre la capa
points(occs[,2:3],pch=18,cex=0.6)


#Explorar directorio y el html, comparar con el obtenido mediante la consola

#Formato SWD (samples with data)
pres.covs<-extract(layers, occs[,2:3])
pres.covs<-na.omit(pres.covs)
pres.covs<-unique(pres.covs)
View(pres.covs)

bkg.covs<-sampleRandom(layers,10000,cells=T)
bkg.covs<-unique(bkg.covs)
bkg.covs<-bkg.covs[,-1] #Elimina columna de número de celda

#Condensar todo en una sola tabla
env.values<-data.frame(rbind(pres.covs,bkg.covs)) #rbind une tablas verticalmente

#Etiquetar presencias (1) y background (0)
y <- c(rep(1,nrow(pres.covs)), rep(0,nrow(bkg.covs)))

#Configuración "personalizada" de Maxent
#response(me)
mxnt.args=c("responsecurves=TRUE",
		"pictures=TRUE",
		"jackknife=TRUE",
		"outputfiletype=asc",
		"outputformat=Logistic",
		"replicates=5",
		"maximumiterations=10000",
		"addsamplestobackground=true")
modelo <- maxent(env.values, y,mxnt.args,path="./resultados_brasiliensis")
#response(modelo)
#Generar predicción

map <- predict(modelo, layers, format=asc, progress="text")
plot(map)
modelof <- stack(map)
#Promediar
final_map <- mean(modelof)
plot(final_map)

#Guardar los resultados

writeRaster(final_map,"./resultados_brasiliensis/mapa_presente.asc",overwrite=T)
save(modelo,file="./resultados_brasiliensis/mx_obj.RData")


#Generar predicción FUTURO
layers_2050_26 <- stack(list.files("./capas 26-2050","asc",full.names=TRUE))
layers_2050_45 <- stack(list.files("./capas 45-2050","asc",full.names=TRUE))
layers_2050_60 <- stack(list.files("./capas 60-2050","asc",full.names=TRUE))
layers_2050_85 <- stack(list.files("./capas 85-2050","asc",full.names=TRUE))
layers_2070_26 <- stack(list.files("./capas 26-2070","asc",full.names=TRUE))
layers_2070_45 <- stack(list.files("./capas 45-2070","asc",full.names=TRUE))
layers_2070_60 <- stack(list.files("./capas 60-2070","asc",full.names=TRUE))
layers_2070_85 <- stack(list.files("./capas 85-2070","asc",full.names=TRUE))

map_26_2050 <- predict(modelo, layers_2050_26, format=asc, progress="text")
map_45_2050 <- predict(modelo, layers_2050_45, format=asc, progress="text")
map_60_2050 <- predict(modelo, layers_2050_60, format=asc, progress="text")
map_85_2050 <- predict(modelo, layers_2050_85, format=asc, progress="text")
map_26_2070 <- predict(modelo, layers_2070_26, format=asc, progress="text")
map_45_2070 <- predict(modelo, layers_2070_45, format=asc, progress="text")
map_60_2070 <- predict(modelo, layers_2070_60, format=asc, progress="text")
map_85_2070 <- predict(modelo, layers_2070_85, format=asc, progress="text")

plot(map)
modelof26_50 <- stack(map_26_2050)
modelof45_50 <- stack(map_45_2050)
modelof60_50 <- stack(map_60_2050)
modelof85_50 <- stack(map_85_2050)
modelof26_70 <- stack(map_26_2070)
modelof45_70 <- stack(map_45_2070)
modelof60_70 <- stack(map_60_2070)
modelof85_70 <- stack(map_85_2070)
#Promediar
final_map26_50 <- mean(modelof26_50)
final_map45_50 <- mean(modelof45_50)
final_map60_50 <- mean(modelof60_50)
final_map85_50 <- mean(modelof85_50)
final_map26_70 <- mean(modelof26_70)
final_map45_70 <- mean(modelof45_70)
final_map60_70 <- mean(modelof60_70)
final_map85_70 <- mean(modelof85_70)
plot(final_map)


map_ord <- final_map 
map_ord <- unique(map_ord,na.rm=T)
map_ord <-sort(map_ord, decreasing = FALSE)
str(map_ord) 
min_max <-c(map_ord [1],map_ord [992887])
min_max 

map_ord <- final_map26_50
map_ord <- unique(map_ord,na.rm=T)
map_ord <-sort(map_ord, decreasing = FALSE)
str(map_ord) 
min_max <-c(map_ord [1],map_ord [993280])
min_max 

map_ord <- final_map45_50
map_ord <- unique(map_ord,na.rm=T)
map_ord <-sort(map_ord, decreasing = FALSE)
str(map_ord) 
min_max <-c(map_ord [1],map_ord [993241])
min_max 

map_ord <- final_map60_50
map_ord <- unique(map_ord,na.rm=T)
map_ord <-sort(map_ord, decreasing = FALSE)
str(map_ord) 
min_max <-c(map_ord [1],map_ord [993301])
min_max 

map_ord <- final_map85_50
map_ord <- unique(map_ord,na.rm=T)
map_ord <-sort(map_ord, decreasing = FALSE)
str(map_ord) 
min_max <-c(map_ord [1],map_ord [992850])
min_max 

map_ord <- final_map26_70
map_ord <- unique(map_ord,na.rm=T)
map_ord <-sort(map_ord, decreasing = FALSE)
str(map_ord) 
min_max <-c(map_ord [1],map_ord [993232])
min_max 

map_ord <- final_map45_70
map_ord <- unique(map_ord,na.rm=T)
map_ord <-sort(map_ord, decreasing = FALSE)
str(map_ord) 
min_max <-c(map_ord [1],map_ord [993221])
min_max 

map_ord <- final_map60_70
map_ord <- unique(map_ord,na.rm=T)
map_ord <-sort(map_ord, decreasing = FALSE)
str(map_ord) 
min_max <-c(map_ord [1],map_ord [993020])
min_max 

map_ord <- final_map85_70
map_ord <- unique(map_ord,na.rm=T)
map_ord <-sort(map_ord, decreasing = FALSE)
str(map_ord) 
min_max <-c(map_ord [1],map_ord [991340])
min_max 

writeRaster(final_map26_50,"./resultados_brasiliensis/mapa_26_2050.asc",overwrite=T)
writeRaster(final_map45_50,"./resultados_brasiliensis/mapa_45_2050.asc",overwrite=T)
writeRaster(final_map60_50,"./resultados_brasiliensis/mapa_60_2050.asc",overwrite=T)
writeRaster(final_map85_50,"./resultados_brasiliensis/mapa_85_2050.asc",overwrite=T)
writeRaster(final_map26_70,"./resultados_brasiliensis/mapa_26_2070.asc",overwrite=T)
writeRaster(final_map45_70,"./resultados_brasiliensis/mapa_45_2070.asc",overwrite=T)
writeRaster(final_map60_70,"./resultados_brasiliensis/mapa_60_2070.asc",overwrite=T)
writeRaster(final_map85_70,"./resultados_brasiliensis/mapa_85_2070.asc",overwrite=T)

#Evaluación usando kfold partitioning
#Ejemplo para 1 fold
fold <- kfold(pres.covs, k=5) #Genera un indice aleatorio de los folds
occtest <- pres.covs[fold == 1, ]
occtrain <- pres.covs[fold != 1, ]
y<-c(rep(1,nrow(occtrain)), rep(0,nrow(bkg.covs)))
env.values<-data.frame(rbind(occtrain, bkg.covs))
View(env.values)


me1 <- maxent(env.values, y, args=c("addsamplestobackground=true"), path="./resultados_brasiliensis3/1")
me2 <- maxent(env.values, y, args=c("addsamplestobackground=true"), path="./resultados_brasiliensis3/2")
me3 <- maxent(env.values, y, args=c("addsamplestobackground=true"), path="./resultados_brasiliensis3/3")
me4 <- maxent(env.values, y, args=c("addsamplestobackground=true"), path="./resultados_brasiliensis3/4")
me5 <- maxent(env.values, y, args=c("addsamplestobackground=true"), path="./resultados_brasiliensis3/5")

e1 <- evaluate(me1, p=occtest, a=bkg.covs) 
e2 <- evaluate(me2, p=occtest, a=bkg.covs) 
e3 <- evaluate(me3, p=occtest, a=bkg.covs) 
e44 <- evaluate(me4, p=occtest, a=bkg.covs) 
e5 <- evaluate(me5, p=occtest, a=bkg.covs) 



TH1=round(e1@t[which.max(e1@TPR + e1@TNR)],digits=3) #Sum of Sensitivity + Specificity - Maximized
TH12=round(e2@t[which.max(e2@TPR + e2@TNR)],digits=3) #Sum of Sensitivity + Specificity - Maximized
TH13=round(e3@t[which.max(e3@TPR + e3@TNR)],digits=3) #Sum of Sensitivity + Specificity - Maximized
TH14=round(e44@t[which.max(e44@TPR + e44@TNR)],digits=3) #Sum of Sensitivity + Specificity - Maximized
TH15=round(e5@t[which.max(e5@TPR + e5@TNR)],digits=3) #Sum of Sensitivity + Specificity - Maximized
TH1F= mean(TH1,TH12,TH13,TH14,TH15)

TH2=round(min(map[cellFromXY(map, occs[,2:3])],na.rm=TRUE),digits=3) #LPT: lowest presence threshold

TH3=round(e1@t[which.max(e1@kappa)],digits=3) # Max Kappa
TH32=round(e2@t[which.max(e2@kappa)],digits=3) # Max Kappa
TH33=round(e3@t[which.max(e3@kappa)],digits=3) # Max Kappa
TH34=round(e44@t[which.max(e44@kappa)],digits=3) # Max Kappa
TH35=round(e5@t[which.max(e5@kappa)],digits=3) # Max Kappa
TH3F=mean(TH3,TH32,TH33,TH34,TH35)

# para TH4
eTH4=NULL
for( j in 1:dim(data.frame(e1@t))[1])
{
e4=abs(e1@TPR[j]-e1@TNR[j])
eTH4=rbind(eTH4,e4)
}
emin4=min(eTH4)
Pos4=match(emin4,eTH4)
TH4=round(e1@t[Pos4],digits=3) # Sensitivity vs Specificity cuerve cross

eTH4=NULL
for( j in 1:dim(data.frame(e2@t))[1])
{
e4=abs(e2@TPR[j]-e2@TNR[j])
eTH4=rbind(eTH4,e4)
}
emin4=min(eTH4)
Pos4=match(emin4,eTH4)
TH42=round(e2@t[Pos4],digits=3) # Sensitivity vs Specificity cuerve cross

eTH4=NULL
for( j in 1:dim(data.frame(e3@t))[1])
{
e4=abs(e3@TPR[j]-e3@TNR[j])
eTH4=rbind(eTH4,e4)
}
emin4=min(eTH4)
Pos4=match(emin4,eTH4)
TH43=round(e3@t[Pos4],digits=3) # Sensitivity vs Specificity cuerve cross

eTH4=NULL
for( j in 1:dim(data.frame(e44@t))[1])
{
e4=abs(e44@TPR[j]-e44@TNR[j])
eTH4=rbind(eTH4,e4)
}
emin4=min(eTH4)
Pos4=match(emin4,eTH4)
TH44=round(e44@t[Pos4],digits=3) # Sensitivity vs Specificity cuerve cross

eTH4=NULL
for( j in 1:dim(data.frame(e5@t))[1])
{
e4=abs(e5@TPR[j]-e5@TNR[j])
eTH4=rbind(eTH4,e4)
}
emin4=min(eTH4)
Pos4=match(emin4,eTH4)
TH45=round(e5@t[Pos4],digits=3) # Sensitivity vs Specificity cuerve cross

TH4F=mean(TH4,TH42,TH43,TH44,TH45)
# fin TH4


# para TH5
ey=NULL
for( j in 1:dim(data.frame(e1@t))[1])
{
y45= abs(1-e1@FPR[j])
ee=abs(e1@TPR[j]-y45)
ey=rbind(ey,ee)
}
emin=min(ey)
Pos5=match(emin,ey)
TH5=round(e1@t[Pos5],digits=3) # Shortest distance to (0,1) in ROC plot 

ey=NULL
for( j in 1:dim(data.frame(e2@t))[1])
{
y45= abs(1-e2@FPR[j])
ee=abs(e2@TPR[j]-y45)
ey=rbind(ey,ee)
}
emin=min(ey)
Pos5=match(emin,ey)
TH52=round(e2@t[Pos5],digits=3) # Shortest distance to (0,1) in ROC plot 

ey=NULL
for( j in 1:dim(data.frame(e3@t))[1])
{
y45= abs(1-e3@FPR[j])
ee=abs(e3@TPR[j]-y45)
ey=rbind(ey,ee)
}
emin=min(ey)
Pos5=match(emin,ey)
TH53=round(e3@t[Pos5],digits=3) # Shortest distance to (0,1) in ROC plot 

ey=NULL
for( j in 1:dim(data.frame(e44@t))[1])
{
y45= abs(1-e44@FPR[j])
ee=abs(e44@TPR[j]-y45)
ey=rbind(ey,ee)
}
emin=min(ey)
Pos5=match(emin,ey)
TH54=round(e44@t[Pos5],digits=3) # Shortest distance to (0,1) in ROC plot 

ey=NULL
for( j in 1:dim(data.frame(e5@t))[1])
{
y45= abs(1-e5@FPR[j])
ee=abs(e5@TPR[j]-y45)
ey=rbind(ey,ee)
}
emin=min(ey)
Pos5=match(emin,ey)
TH55=round(e5@t[Pos5],digits=3) # Shortest distance to (0,1) in ROC plot 

TH5F=mean(TH5,TH52,TH53,TH54,TH55)
# fin TH5

TH6=round(quantile(map[cellFromXY(map,occs[,2:3])],.1,na.rm=TRUE),digits=3) #T10
TH7=round(quantile(map[cellFromXY(map,occs[,2:3])],.25,na.rm=TRUE),digits=3) #T10







