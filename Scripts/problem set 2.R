remove(list = ls())
set.seed(10101)
library(leaps)

library(elasticnet)
library(ggplot2)
library(sf)
library(readr)
library(dplyr)
library(Rfast)
library(table1)
library(gdistance)
library(tidyverse)
library(caret)
library(mice)
library(spdep)
library(spatialreg)


##2.1a
evanston <- st_read('C:/Users/57320/Desktop/la/evanston.shp')
cblock<-st_read('C:/Users/57320/Desktop/la/cblock.shp')
raillines<-st_read('C:/Users/57320/Desktop/la/Rail Lines.shp')
metrastops<-st_read('C:/Users/57320/Desktop/la/Metra Stops.shp')
majorroads<-st_read('C:/Users/57320/Desktop/la/Major Roads.shp')
lakemichigan<-st_read('C:/Users/57320/Desktop/la/Lake Michigan.shp')
elestacions<-st_read('C:/Users/57320/Desktop/la/El Stations.shp')
ellines<-st_read('C:/Users/57320/Desktop/la/El Lines.shp')
ctract<-st_read('C:/Users/57320/Desktop/la/ctract.shp')


evanston <- st_set_crs(evanston, 4269)
cblock <- st_set_crs(cblock, 4269)
raillines <- st_transform(raillines, 4269)
metrastops <- st_transform(metrastops, 4269)
majorroads <- st_transform(majorroads, 4269)
lakemichigan<- st_transform(lakemichigan, 4269)
elestacions <- st_transform(elestacions, 4269)
elline <- st_transform(ellines, 4269)
ctract <- st_set_crs(ctract, 4269)

cblock1 <- st_transform(cblock, 2163) 
majorroads1<- st_transform(majorroads, 2163)
evanstone1<- st_transform(evanston, 2163)
lakemichigan1<- st_transform(lakemichigan, 2163) 
metrastops1<- st_transform(metrastops, 2163) 
elline1<- st_transform(elline, 2163) 
railllines1<- st_transform(raillines, 2163) 
a<-st_intersection(evanstone1, lakemichigan1)
b<-st_intersection(evanstone1, elline1)
c<-st_intersection(evanstone1, railllines1)



ggplot() +
   geom_sf(data = evanston ) +  geom_sf(data = a,aes( colour = 'lake michigan')) +  geom_sf(data = cblock, aes( colour='Cblock') )+
   geom_sf(data =c, aes( colour='Rail lines') )+ geom_sf(data = metrastops,aes(  colour='Metro stop') )+ geom_sf(data = majorroads,aes(  colour='Major roads') )+ 
   geom_sf(data = ctract, aes( colour='Ctract'),fill=NA )+ geom_sf(data = elestacions,aes(  colour='El estacions') )+ geom_sf(data =b, aes( colour='El line') )+
   theme_bw() +coord_sf()+
   ggtitle("Mapa Evanstone")

##2.1b

urlfile='https://raw.githubusercontent.com/ECON-4676-UNIANDES/Problem_Sets/master/Problem_Set2/data/evanston_parcel_data.csv'
mydata<-read_csv(url(urlfile))
mydata<-as.data.frame(mydata)
New <- data.frame("land_square" = mydata$`Land Square Feet`,"type_residence" = mydata$`Type of Residence`, "rooms" = mydata$Rooms, "building_squ"=mydata$`Building Square Feet`, 'garage_size'=mydata$`Garage 1 Size`, 'assesed_value'=mydata$`Assessed Value`, 'sale_price'=mydata$`Sale Price`,'Latitude'=mydata$Latitude, 'Longitude'=mydata$Longitude) 
mydata<-New
##imputacion de data
impute<-mice(mydata[,1:7],m=1)
data<-complete(impute,1)
mydata$land_square<-data$land_square
mydata$type_residence<-data$type_residence
mydata$rooms<-data$rooms
mydata$building_squ<-data$building_squ
mydata$garage_size<-data$garage_size
mydata$assesed_value<-data$assesed_value
mydata$sale_price<-data$sale_price

mydata_sf= st_as_sf(mydata, coords = c("Longitude", "Latitude"), 
                    crs = st_crs(ctract))


mydata_sf1 <- st_transform(mydata_sf, 2163) 
pnts <- mydata_sf1 %>% mutate(
   intersection = as.integer(st_intersects(geometry, cblock1))
   , area = if_else(is.na(intersection), '', cblock1$SP_ID[intersection])
   
) 




pnts$batfa<- (pnts$building_squ)/(pnts$land_square)

a<-pnts %>%                                    
   group_by(intersection) %>%                      
   summarise_at(vars(batfa, assesed_value),              
                list(name = mean))  
df <- st_drop_geometry(a)
cblock <- cbind(index = rownames(cblock), cblock)
rownames(cblock) <- 1:nrow(cblock)

mergedData <- merge(df, cblock, by.x='intersection', by.y='index',all.y =TRUE)
y<-drop_na(mergedData)
quantile(y$batfa_name)
quantile(y$assesed_value_name)

mergedData$batfa<-cut(mergedData$batfa_name, c(0.004656931,0.250792628 ,0.321033059,0.382079442, 4.178394450 ))
mergedData$assesed_value<-cut(mergedData$assesed_value_name, c(76770.83,286524.58 ,442591.57,649037.84, 4900770.00 ))

ggplot() +
   geom_sf(data = evanston )+labs(fill = "promedio area edificio sobre area lote por Cblock")+geom_sf(data = mergedData$geometry, aes( fill=mergedData$batfa, color = 'cblock' ) )+scale_fill_grey()+
   theme_bw() +coord_sf()+geom_sf(data =c, aes( infraestructura='Rail lines') )+ geom_sf(data = metrastops,aes(  infraestructura='Metro stop') )+ geom_sf(data = majorroads,aes(  infraestructura='Major roads') )
+
   ggtitle("Mapa promedio area edificio sobre area lote por Cblock")

ggplot() +
   geom_sf(data = evanston )+labs(fill = "promedio valor abordado por Cblock")+   geom_sf(data = mergedData$geometry, aes( fill=mergedData$assesed_value,color = 'cblock') )+scale_fill_grey()+
   theme_bw() +coord_sf()+geom_sf(data =c, aes( infraestructura='Rail lines') )+ geom_sf(data = metrastops,aes(  infraestructura='Metro stop') )+ geom_sf(data = majorroads,aes(  infraestructura='Major roads') )+
   geom_sf(data = mergedData$geometry, aes( fill=mergedData$assesed_value) )+
   ggtitle("Mapa promedio area edificio sobre area lote por Cblock")


##2.1.c

#2.2.a



bla<-st_distance(pnts, lakemichigan1)
pnts$mindistancelake<-rowMins(bla, value = TRUE)

du<-st_distance(pnts, majorroads1)
pnts$minimymds_majo<-rowMins(du, value = TRUE)

bu<-st_distance(pnts, elline1)
pnts$minimymds_elline1<-rowMins(bu, value = TRUE)

lu<-st_distance(pnts,metrastops1)
pnts$minimymds_metrastops1<-rowMins(lu, value = TRUE)


table1::table1(~batfa+sale_price +garage_size +rooms +type_residence+ mindistancelake+minimymds_elline1+ minimymds_majo+ minimymds_metrastops1, data = pnts)


#2.2.b



indic<-sample(1:nrow(pnts),floor(.7*nrow(pnts)))

train<-pnts[indic,]

test<-pnts[-indic,]

reg<-lm(assesed_value ~ batfa+sale_price +garage_size +rooms +type_residence+ mindistancelake+minimymds_elline1+ minimymds_majo+ minimymds_metrastops1  ,data=train)

test$yhat_reg<-predict(reg,test)

sqrt(mean((test$assesed_value-test$yhat_reg)^2))

##2.2.c
train.control <- trainControl(method = "cv", number = 10)
model2<-train(assesed_value ~ batfa+sale_price +garage_size +rooms +type_residence+ mindistancelake+minimymds_elline1+ minimymds_majo+ minimymds_metrastops1  ,data=pnts ,method = "lm",
              trControl = train.control)
print(model2)

##2.2.d
model3<-train(assesed_value ~ batfa+sale_price +garage_size +rooms +type_residence+ mindistancelake+minimymds_elline1+ minimymds_majo+ minimymds_metrastops1  ,data=pnts ,method = "ridge",
              trControl = train.control)
print(model3)

##2.2.e

mejores_modelos <- regsubsets(assesed_value ~ batfa+sale_price +garage_size +rooms +type_residence+ mindistancelake+minimymds_elline1+ minimymds_majo+ minimymds_metrastops1, data=pnts, nvmax = 19)

model4<-train(assesed_value ~ batfa+sale_price +garage_size +rooms +type_residence+ mindistancelake+minimymds_elline1+ minimymds_majo+ minimymds_metrastops1  ,data=pnts ,method = "lasso",
              trControl = train.control)
print(model4)


#2.2.f

indic_train<-sample(1:nrow(train),floor(.5*nrow(train)))
train<-train[indic_train,]

indic_test<-sample(1:nrow(test),floor(.5*nrow(test)))
test<-test[indic_test,]

W_train <- dnearneigh(train$geometry, 0, 1000, row.names = row.names(train))
W_train <- nb2listw(W_train, style = 'W', zero.policy = T)


W_test <- dnearneigh(test$geometry, 0, 1000, row.names = row.names(test))
W_test <- nb2listw(W_test, style = 'W', zero.policy = T)

model5 <- lagsarlm(assesed_value ~ batfa+sale_price +garage_size +rooms +type_residence+ mindistancelake+minimymds_elline1+ minimymds_majo+ minimymds_metrastops1, data=train, W_train)
test$yhat_sar<-predict(model5, newdata = test, listw = W_test)

sqrt(mean((test$assesed_value-test$yhat_sar)^2))


