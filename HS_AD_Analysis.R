rm(list=ls(all=TRUE))
graphics.off()

setwd('~/Documents/HS/')

library('xlsx')
library(kSamples)

elev.data<-read.csv('Nectarivore Modified.csv')
elev.data<-elev.data[,1:24]

troch.elev<-subset(elev.data,Family=='TROCHILIDAE')
nectar.elev<-subset(elev.data,Family=='NECTARINIIDAE')

lat.data<-read.csv('Nectarivore Latitudinal Data.csv')

troch.lat<-subset(lat.data,Family=='Trochilidae')
promerop.lat<-subset(lat.data,Family=='Promeropidae')
dicae.lat<-subset(lat.data,Family=='Dicaeidae')
nectar.lat<-subset(lat.data,Family=='Nectariniidae')

hbird.lat<-subset(lat.data,Type=='Hummingbird')
sbird.lat<-subset(lat.data,Type=='Sunbird')

elev.type<-c('ModMin','ModMax','Midpoint')

lat.type<-c('MinLat','MaxLat','AvgLat','MinPole','MaxPole','AvgPole','ExpctPole')

family.ad.results<-type.ad.results<-as.data.frame(matrix(nrow=length(c(elev.type,lat.type)),ncol=7))

family.ad.names<-paste('Family',c(elev.type,lat.type),'AD Results')
type.ad.names<-paste('Type',c(elev.type,lat.type),'AD Results')

ad.result.names<-c('Hbird Num','Sbird Num','Total Num','Sigma','AD Criterion','Standardized AD','p-value')

rownames(family.ad.results)<-family.ad.names
rownames(type.ad.results)<-type.ad.names

colnames(family.ad.results)<-colnames(type.ad.results)<-ad.result.names

for(i in 1:length(elev.type)){
	type<-elev.type[i]
	family.ad<-ad.test(troch.elev[!is.na(troch.elev[,type]),type],nectar.elev[,type])
	family.ad.results[i,1:2]<-family.ad$ns
	family.ad.results[i,3]<-family.ad$N
	family.ad.results[i,4]<-family.ad$sig
	family.ad.results[i,5:7]<-family.ad$ad[1,]

	type.ad<-ad.test(troch.elev[!is.na(troch.elev[,type]),type],nectar.elev[,type])
	type.ad.results[i,1:2]<-type.ad$ns
	type.ad.results[i,3]<-type.ad$N
	type.ad.results[i,4]<-type.ad$sig
	type.ad.results[i,5:7]<-type.ad$ad[1,]
}

for(i in 1:length(lat.type)+length(elev.type)){
	type<-lat.type[i-length(elev.type)]
	family.ad<-ad.test(troch.lat[,type],nectar.lat[,type])
	family.ad.results[i,1:2]<-family.ad$ns
	family.ad.results[i,3]<-family.ad$N
	family.ad.results[i,4]<-family.ad$sig
	family.ad.results[i,5:7]<-family.ad$ad[1,]

	type.ad<-ad.test(hbird.lat[,type],sbird.lat[,type])
	type.ad.results[i,1:2]<-type.ad$ns
	type.ad.results[i,3]<-type.ad$N
	type.ad.results[i,4]<-type.ad$sig
	type.ad.results[i,5:7]<-type.ad$ad[1,]
}

ad.results<-rbind(family.ad.results,' '=NA,type.ad.results)

ad.results[,'p-value']<-ad.results[,'p-value']/2

write.csv(ad.results,'~/Documents/HS/AD Results (one-tailed).csv',na='')
