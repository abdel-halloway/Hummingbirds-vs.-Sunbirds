rm(list=ls(all=TRUE))
graphics.off()

this.dir<-dirname(parent.frame(2)$ofile)

setwd(this.dir)

#library('xlsx')

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

family.ks.results<-type.ks.results<-as.data.frame(matrix(nrow=length(c(elev.type,lat.type)),ncol=2))

family.ks.names<-paste('Family',c(elev.type,lat.type),'KS Results')
type.ks.names<-paste('Type',c(elev.type,lat.type),'KS Results')

ks.result.names<-c('D statistic','p value')

rownames(family.ks.results)<-family.ks.names
rownames(type.ks.results)<-type.ks.names

colnames(family.ks.results)<-colnames(type.ks.results)<-ks.result.names

for(i in 1:length(elev.type)){
	type<-elev.type[i]
	family.ks<-ks.test(troch.elev[!is.na(troch.elev[,type]),type],nectar.elev[,type],alternative = "less")
	family.ks.results[i,1]<-family.ks$statistic
	family.ks.results[i,2]<-family.ks$p.value

	type.ks<-ks.test(troch.elev[!is.na(troch.elev[,type]),type],nectar.elev[,type],alternative = "less")
	type.ks.results[i,1]<-type.ks$statistic
	type.ks.results[i,2]<-type.ks$p.value
}

for(i in 1:length(lat.type)+length(elev.type)){
	type<-lat.type[i-length(elev.type)]
	family.ks<-ks.test(troch.lat[,type],nectar.lat[,type],alternative = "less")
	family.ks.results[i,1]<-family.ks$statistic
	family.ks.results[i,2]<-family.ks$p.value

	type.ks<-ks.test(hbird.lat[,type],sbird.lat[,type],alternative = "less")
	type.ks.results[i,1]<-type.ks$statistic
	type.ks.results[i,2]<-type.ks$p.value
}

ks.results<-rbind(family.ks.results,' '=NA,type.ks.results)

write.csv(ks.results,'~/Documents/HS/KS Results (one-tailed).csv',na='')
