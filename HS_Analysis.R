rm(list=ls())
graphics.off()

library(rootSolve)

this.dir<-dirname(parent.frame(2)$ofile)
work.dir<-paste0(this.dir,'/4.0/')
setwd(work.dir)

Bird.Data<-read.csv("Elevation and Latitude Data 4.0.csv")
# Bird.Data[which(Bird.Data[,'NectarLat']==0),'NectarLat']=NA
Nectar.Lat <- nls(NectarLat ~ 1/(1 + theta1*exp(theta2*Latitude)), start=list(theta1 = 0.078, theta2 = 0.135), data=Bird.Data, trace=TRUE)
Troch.Lat <- nls(TrochLat ~ 1/(1 + theta1*exp(theta2*Latitude)), start=list(theta1 = 0.020, theta2 = 0.177), data=Bird.Data, trace=TRUE)
Nectar.Elev <- nls(NectarElev ~ 1/(1 + theta1*exp(theta2*Elevation)), start=list(theta1 = 0.004, theta2 = 2.273), data=Bird.Data, trace=TRUE)
Troch.Elev <- nls(TrochElev ~ 1/(1 + theta1*exp(theta2*Elevation)), start=list(theta1 = 0.007, theta2 = 1.644), data=Bird.Data, trace=TRUE)

Inflection.Point<-function(b){
	log(b[1]^-(1/b[2]))
}

logis.func<-function(y,b){
	a=b[1]
	b=b[2]
	1/(1+a*exp(b*y))
}

first.der<-function(y,b){
	a=b[1]
	b=b[2]
	-a*b*exp(b*y)*((1+a*exp(b*y))^-2)
}

second.der<-function(y,b){
	a=b[1]
	b=b[2]
	(a*(b^2)*exp(b*y)*(a*exp(b*y)-1))/((a*exp(b*y)+1)^3)
}

third.der<-function(y,b){
	(-b[1]*(b[2]^3)*exp(b[2]*y)*((b[1]^2)*exp(2*b[2]*y)-4*b[1]*exp(b[2]*y)+1))/((b[1]*exp(b[2]*y)+1)^4)
}

curvature.func<-function(y,b){
	second.der(y,b)*(1+(first.der(y,b))^2)^(-3/2)
}

curvature.der<-function(y,b){
	(third.der(y,b)+third.der(y,b)*(first.der(y,b))^2-3*(second.der(y,b))^2*first.der(y,b))*(1+(first.der(y,b))^2)^(-5/2)
}

Bird.Output<-matrix(NA,nrow=4,ncol=15)

rownames(Bird.Output)<-c('NectarLat','TrochLat','NectarElev','TrochElev')
colnames(Bird.Output)<-c('a','Std Error','t','p','b','Std Error','t','p','Inflection Point','Jerk 1','Jerk 2','Curve 1','Curve 2','Residual Sum of Squares','Residual Std Error')

x<-seq(0,4.75,0.01)
z<-seq(0,62.50,0.01)

#curvature.line<-abs(curvature.func(x,Bird.Output[j,1],Bird.Output[j,2]))

#curvature.one<-max(curvature.line[1:])

Bird.Output['NectarLat',1:8]<-as.vector(t(coefficients(summary(Nectar.Lat))))
Bird.Output['NectarLat',9]<-Inflection.Point(coef(Nectar.Lat))
Bird.Output['NectarLat',10:11]<-uniroot.all(third.der,interval=c(0,62.50),b=coef(Nectar.Lat))
Bird.Output['NectarLat',12:13]<-uniroot.all(curvature.der,interval=c(0,62.50),b=coef(Nectar.Lat))
Bird.Output['NectarLat',14]<-sum(residuals(Nectar.Lat)^2)
Bird.Output['NectarLat',15]<-summary(Nectar.Lat)$sigma
Bird.Output['TrochLat',1:8]<-as.vector(t(coefficients(summary(Troch.Lat))))
Bird.Output['TrochLat',3+6]<-Inflection.Point(coef(Troch.Lat))
Bird.Output['TrochLat',4:5+6]<-uniroot.all(third.der,interval=c(0,62.50),b=coef(Troch.Lat))
Bird.Output['TrochLat',6:7+6]<-uniroot.all(curvature.der,interval=c(0,62.50),b=coef(Troch.Lat))
Bird.Output['TrochLat',14]<-sum(residuals(Troch.Lat)^2)
Bird.Output['TrochLat',15]<-summary(Troch.Lat)$sigma
Bird.Output['NectarElev',1:8]<-as.vector(t(coefficients(summary(Nectar.Elev))))
Bird.Output['NectarElev',3+6]<-Inflection.Point(coef(Nectar.Elev))
Bird.Output['NectarElev',4:5+6]<-uniroot.all(third.der,interval=c(0,62.50),b=coef(Nectar.Elev))
Bird.Output['NectarElev',6:7+6]<-uniroot.all(curvature.der,interval=c(0,62.50),b=coef(Nectar.Elev))
Bird.Output['NectarElev',14]<-sum(residuals(Nectar.Elev)^2)
Bird.Output['NectarElev',15]<-summary(Nectar.Elev)$sigma
Bird.Output['TrochElev',1:8]<-as.vector(t(coefficients(summary(Troch.Elev))))
Bird.Output['TrochElev',3+6]<-Inflection.Point(coef(Troch.Elev))
Bird.Output['TrochElev',4:5+6]<-uniroot.all(third.der,interval=c(0,62.50),b=coef(Troch.Elev))
Bird.Output['TrochElev',6:7+6]<-uniroot.all(curvature.der,interval=c(0,62.50),b=coef(Troch.Elev))
Bird.Output['TrochElev',14]<-sum(residuals(Troch.Elev)^2)
Bird.Output['TrochElev',15]<-summary(Troch.Elev)$sigma

write.csv(Bird.Output,'Full Bird Output 4.0.csv')
