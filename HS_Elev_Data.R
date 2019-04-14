rm(list=ls())
graphics.off()

require('xlsx')
require('gdata')
require('rootSolve')

setwd('~/Documents/HS/')

data<-read.csv('Nectarivore Modified.csv')

data<-data[,1:24]

Trochilidae<-subset(data,Family=='TROCHILIDAE')

Nectariniidae<-subset(data,Family=='NECTARINIIDAE')

Troch.Max<-Trochilidae$ModMax
Troch.Min<-Trochilidae$ModMin

Nectar.Max<-Nectariniidae$ModMax
Nectar.Min<-Nectariniidae$ModMin

Troch.Min<-Troch.Min[!is.na(Troch.Min)]
Troch.Max<-Troch.Max[!is.na(Troch.Max)]

Nectar.Min<-Nectar.Min[!is.na(Nectar.Min)]
Nectar.Max<-Nectar.Max[!is.na(Nectar.Max)]

Max.Troch.Elev<-ceiling(max(Troch.Max)/500)
Max.Nectar.Elev<-ceiling(max(Nectar.Max)/500)

Troch<-matrix(0,nrow=length(Troch.Max),ncol=Max.Troch.Elev)
Nectar<-matrix(0,nrow=length(Nectar.Max),ncol=Max.Nectar.Elev)

for(i in 1:length(Troch.Max)){
	j<-ceiling(Troch.Min[i]/500)
	k<-ceiling(Troch.Max[i]/500)
	if(j==0){
		j=1
	}
	presence<-rep(1,k-j+1)
	Troch[i,j:k]<-presence
}

for(i in 1:length(Nectar.Max)){
	j<-ceiling(Nectar.Min[i]/500)
	k<-ceiling(Nectar.Max[i]/500)
	if(j==0){
		j=1
	}
	presence<-rep(1,k-j+1)
	Nectar[i,j:k]<-presence
}

Troch.Dist<-colSums(Troch)

Nectar.Dist<-colSums(Nectar)

Troch.Dist.Norm<-Troch.Dist/max(Troch.Dist)

Nectar.Dist.Norm<-Nectar.Dist/max(Nectar.Dist)

Range<-seq(1,Max.Troch.Elev,1)/2-0.25

Summary<-read.csv("Elevation Latitude Summary.csv")

Elev.Data<-cbind(Range,Nectar.Dist.Norm,Troch.Dist.Norm)

New.Sum<-cbindX(Elev.Data,Summary[,1:3+3])

colnames(New.Sum)<-c('Elevation','NectarElev','TrochElev','Latitude','NectarLat','TrochLat')

write.csv(New.Sum,'Elevation and Latitude Data.csv',row.names=FALSE,na='')

