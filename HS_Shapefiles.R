rm(list=ls(all=TRUE))
graphics.off()

setwd('~/Documents/HS')

library(sp)
library(raster)
library(rasterVis)
library(maptools)
library(rgeos)

troch.dir.loc<-'/mnt/A8C40583C405554A/Users/Abdel Halloway/Documents/Research/Hawkmoths, Hummingbirds, and Sunbirds/Hummingbirds v. Sundbirds/Map Files/Trochilidae/'
promerop.dir.loc<-'/mnt/A8C40583C405554A/Users/Abdel Halloway/Documents/Research/Hawkmoths, Hummingbirds, and Sunbirds/Hummingbirds v. Sundbirds/Map Files/Promeropidae/'
dicae.dir.loc<-'/mnt/A8C40583C405554A/Users/Abdel Halloway/Documents/Research/Hawkmoths, Hummingbirds, and Sunbirds/Hummingbirds v. Sundbirds/Map Files/Dicaeidae/'
nectar.dir.loc<-'/mnt/A8C40583C405554A/Users/Abdel Halloway/Documents/Research/Hawkmoths, Hummingbirds, and Sunbirds/Hummingbirds v. Sundbirds/Map Files/Nectariniidae/'

troch.file.names<-list.files(path=troch.dir.loc,pattern='.shp')
promerop.file.names<-list.files(path=promerop.dir.loc,pattern='.shp')
dicae.file.names<-list.files(path=dicae.dir.loc,pattern='.shp')
nectar.file.names<-list.files(path=nectar.dir.loc,pattern='.shp')

troch.lat<-data.frame(matrix(nrow=length(troch.file.names),ncol=7))
promerop.lat<-data.frame(matrix(nrow=length(promerop.file.names),ncol=7))
dicae.lat<-data.frame(matrix(nrow=length(dicae.file.names),ncol=7))
nectar.lat<-data.frame(matrix(nrow=length(nectar.file.names),ncol=7))

troch.lat[,1]<-"Hummingbird"
promerop.lat[,1]<-dicae.lat[,1]<-nectar.lat[,1]<-"Sunbird"

troch.lat[,2]<-"Trochilidae"
promerop.lat[,2]<-"Promeropidae"
dicae.lat[,2]<-"Dicaeidae"
nectar.lat[,2]<-"Nectariniidae"

troch.pole<-matrix(nrow=length(troch.file.names),ncol=4)
promerop.pole<-matrix(nrow=length(promerop.file.names),ncol=4)
dicae.pole<-matrix(nrow=length(dicae.file.names),ncol=4)
nectar.pole<-matrix(nrow=length(nectar.file.names),ncol=4)

colnames(troch.lat)<-colnames(promerop.lat)<-colnames(dicae.lat)<-colnames(nectar.lat)<-c('Type','Family','MinLat','MaxLat','AvgLat','RoundMinLat','RoundMaxLat')
colnames(troch.pole)<-colnames(promerop.pole)<-colnames(dicae.pole)<-colnames(nectar.pole)<-c('MinPole','MaxPole','AvgPole','ExpctPole')

for(i in 1:length(troch.file.names)){
	assign(troch.file.names[i],readShapePoly(paste(troch.dir.loc,troch.file.names[i],sep='')))
	troch.lat[i,3]<-ymin(get(troch.file.names[i]))
	troch.lat[i,4]<-ymax(get(troch.file.names[i]))
}

for(i in 1:length(promerop.file.names)){
	assign(promerop.file.names[i],readShapePoly(paste(promerop.dir.loc,promerop.file.names[i],sep='')))
	promerop.lat[i,3]<-ymin(get(promerop.file.names[i]))
	promerop.lat[i,4]<-ymax(get(promerop.file.names[i]))
}

for(i in 1:length(dicae.file.names)){
	assign(dicae.file.names[i],readShapePoly(paste(dicae.dir.loc,dicae.file.names[i],sep='')))
	dicae.lat[i,3]<-ymin(get(dicae.file.names[i]))
	dicae.lat[i,4]<-ymax(get(dicae.file.names[i]))
}

for(i in 1:length(nectar.file.names)){
	assign(nectar.file.names[i],readShapePoly(paste(nectar.dir.loc,nectar.file.names[i],sep='')))
	nectar.lat[i,3]<-ymin(get(nectar.file.names[i]))
	nectar.lat[i,4]<-ymax(get(nectar.file.names[i]))
}

troch.lat[,5]=rowMeans(troch.lat[,3:4])
promerop.lat[,5]=rowMeans(promerop.lat[,3:4])
dicae.lat[,5]=rowMeans(dicae.lat[,3:4])
nectar.lat[,5]=rowMeans(nectar.lat[,3:4])

troch.lat[,6]<-floor(troch.lat[,3])
troch.lat[,7]<-ceiling(troch.lat[,4])
promerop.lat[,6]<-floor(promerop.lat[,3])
promerop.lat[,7]<-ceiling(promerop.lat[,4])
dicae.lat[,6]<-floor(dicae.lat[,3])
dicae.lat[,7]<-ceiling(dicae.lat[,4])
nectar.lat[,6]<-floor(nectar.lat[,3])
nectar.lat[,7]<-ceiling(nectar.lat[,4])

hbird.lat<-troch.lat
sbird.lat<-rbind(promerop.lat,dicae.lat,nectar.lat)
bird.lat<-rbind(hbird.lat,sbird.lat)

for(i in 1:length(troch.file.names)){
	if(troch.lat[i,6]<0 & troch.lat[i,7]>0){
		abs.min<-abs(troch.lat[i,6]); abs.max<-abs(troch.lat[i,7])
		troch.pole[i,1]=0
		troch.pole[i,2]=max(abs.min,abs.max)
		troch.pole[i,4]=(abs.min^2+abs.max^2)/(2*abs.min+2*abs.max)
	}else{
		troch.pole[i,1]=min(abs(troch.lat[i,6:7]))
		troch.pole[i,2]=max(abs(troch.lat[i,6:7]))
		troch.pole[i,4]=abs(mean(as.numeric(troch.lat[i,6:7])))
	}
}

for(i in 1:length(promerop.file.names)){
	if(promerop.lat[i,6]<0 & promerop.lat[i,7]>0){
		abs.min<-abs(promerop.lat[i,6]); abs.max<-abs(promerop.lat[i,7])
		promerop.pole[i,1]=0
		promerop.pole[i,2]=max(abs.min,abs.max)
		promerop.pole[i,4]=(abs.min^2+abs.max^2)/(2*abs.min+2*abs.max)
	}else{
		promerop.pole[i,1]=min(abs(promerop.lat[i,6:7]))
		promerop.pole[i,2]=max(abs(promerop.lat[i,6:7]))
		promerop.pole[i,4]=abs(mean(as.numeric(promerop.lat[i,6:7])))
	}
}

for(i in 1:length(dicae.file.names)){
	if(dicae.lat[i,6]<0 & dicae.lat[i,7]>0){
		abs.min<-abs(dicae.lat[i,6]); abs.max<-abs(dicae.lat[i,7])
		dicae.pole[i,1]=0
		dicae.pole[i,2]=max(abs.min,abs.max)
		dicae.pole[i,4]=(abs.min^2+abs.max^2)/(2*abs.min+2*abs.max)
	}else{
		dicae.pole[i,1]=min(abs(dicae.lat[i,6:7]))
		dicae.pole[i,2]=max(abs(dicae.lat[i,6:7]))
		dicae.pole[i,4]=abs(mean(as.numeric(dicae.lat[i,6:7])))
	}
}

for(i in 1:length(nectar.file.names)){
	if(nectar.lat[i,6]<0 & nectar.lat[i,7]>0){
		abs.min<-abs(nectar.lat[i,6]); abs.max<-abs(nectar.lat[i,7])
		nectar.pole[i,1]=0
		nectar.pole[i,2]=max(abs.min,abs.max)
		nectar.pole[i,4]=(abs.min^2+abs.max^2)/(2*abs.min+2*abs.max)
	}else{
		nectar.pole[i,1]=min(abs(nectar.lat[i,6:7]))
		nectar.pole[i,2]=max(abs(nectar.lat[i,6:7]))
		nectar.pole[i,4]=abs(mean(as.numeric(nectar.lat[i,6:7])))
	}
}

troch.pole[,3]=rowMeans(troch.pole[,1:2])
promerop.pole[,3]=rowMeans(promerop.pole[,1:2])
dicae.pole[,3]=rowMeans(dicae.pole[,1:2])
nectar.pole[,3]=rowMeans(nectar.pole[,1:2])

hbird.pole<-troch.pole
sbird.pole<-rbind(promerop.pole,dicae.pole,nectar.pole)
bird.pole<-rbind(hbird.pole,sbird.pole)

bird.data<-cbind(bird.lat,bird.pole)

bird.output<-'Nectarivore Latitudinal Data.csv'

write.table(bird.data,bird.output,row.names=FALSE,sep=',')

