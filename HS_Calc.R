expansion<-c(0.39,0.38,0.41,0.45,0.44,0.31,0.31,0.18,0.21,0.2,0.26,0.26,0.23
		 ,0.28,0.19,0.2,0.23,0.2,0.24,0.24,0.25,0.27,0.3,0.3,0.31,0.29,0.27
		 ,0.24,0.24,0.22,0.19,0.22)

percentage<-c(60.2,61.8,58.6,61.9,60.2,53.4,53.3,45.3,52.4,46.1,54.3,54.1,47.5
		  ,53,47.9,48,50.2,44.3,50.7,51.8,49.4,52.8,57.4,61.2,59.2,57.5,55.2
		  ,54.4,51.5,50.5,47.1,51.1)

length<-c(10,11,12,13)

percentage<-percentage/100

diameter<-expansion/percentage; radius<-diameter/2; area<-radius^2*pi; volume<-outer(area,length,FUN='*')

avg.rate<-13.8; max.rate<-17

avg.muL.per.sec<-volume*avg.rate; max.muL.per.sec<-volume*max.rate

summary(avg.muL.per.sec); summary(max.muL.per.sec)