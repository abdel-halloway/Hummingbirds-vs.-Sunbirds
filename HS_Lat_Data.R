rm(list=ls())
graphics.off()

#require('xlsx')
require('gdata')
require('rootSolve')

setwd('~/Documents/HS/')

data<-read.csv('Nectarivore Latitudinal Data.csv')

#data<-data[,1:24]

Trochilidae<-subset(data,Family=='Trochilidae')

Nectariniidae<-subset(data,Family=='Nectariniidae')

Troch.Max<-Trochilidae$MaxPole
Troch.Min<-Trochilidae$MinPole

Nectar.Max<-Nectariniidae$MaxPole
Nectar.Min<-Nectariniidae$MinPole

Max.Troch.Lat<-ceiling(max(Troch.Max)/5)
Max.Nectar.Lat<-ceiling(max(Nectar.Max)/5)

Troch<-matrix(0,nrow=length(Troch.Max),ncol=Max.Troch.Lat)
Nectar<-matrix(0,nrow=length(Nectar.Max),ncol=Max.Nectar.Lat)

for(i in 1:length(Troch.Max)){
	j<-ceiling(Troch.Min[i]/5)
	k<-ceiling(Troch.Max[i]/5)
	if(j==0){
		j=1
	}
	presence<-rep(1,k-j+1)
	Troch[i,j:k]<-presence
}

for(i in 1:length(Nectar.Max)){
	j<-ceiling(Nectar.Min[i]/5)
	k<-ceiling(Nectar.Max[i]/5)
	if(j==0){
		j=1
	}
	presence<-rep(1,k-j+1)
	Nectar[i,j:k]<-presence
}

Troch.Dist<-colSums(Troch)

Nectar.Dist<-colSums(Nectar)

Troch.Dist.Norm<-Troch.Dist/max(Troch.Dist)

Nectar.Dist.Norm<-c(Nectar.Dist/max(Nectar.Dist),rep(0,length(Troch.Dist)-length(Nectar.Dist)))

Range<-(seq(1,Max.Troch.Lat,1)/2-0.25)*10

Summary<-read.csv('Elevation and Latitude Data.csv')

Lat.Data<-cbind(Range,Nectar.Dist.Norm,Troch.Dist.Norm)

New.Sum<-cbindX(Summary[,1:3],Lat.Data)

colnames(New.Sum)<-c('Elevation','NectarElev','TrochElev','Latitude','NectarLat','TrochLat')

write.csv(New.Sum,'Elevation and Latitude Data 3.0.csv',row.names=FALSE,na='')

