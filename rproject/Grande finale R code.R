#CLEARS ALL VARIABLES FROM THE WORK SPACE
rm(list=ls())
 ls()

#IMPORTING INPUT FILES TO R
rawdata=read.csv("C:/Users/mosimi01/Desktop/Rinput/rawdata1.CSV",header=T,sep=",")
attach(rawdata)#allows all variables to be seen in the workspace

#FILTERING ACTIVE STORES

rawdata$Active<-ifelse(as.numeric(rawdata$Nov.14)>0,1,0)

rawdata=rawdata[rawdata$Active==1,]

rawdata$common<-rowSums(!is.na(rawdata[,c(12:24)]))
 
#IBD CONSTRUCTION
rawdata$IBD<-paste(rawdata$Urbanity.with.Key.City,rawdata$Type2)
 
#PART OF IBD CONSTRUCT IS DONE MANUALLY IN EXCEL

#EXPORTING RAW DATA TO EXCEL for further IBD construction

write.csv(rawdata,("C:/Users/mosimi01/Desktop/Routput/rawdata.csv"),row.names=FALSE,na="")

#CV CALCULATION

rawdata=read.csv("C:/Users/mosimi01/Desktop/Rinput/input(IBD).CSV",header=T,sep=",")

rawdata$mean<-apply(rawdata[,c(12:24)],1,mean,na.rm=T)

rawdata$sd<-apply(rawdata[,c(12:24)],1,sd,na.rm=T)

rawdata$CV<-rawdata$sd/rawdata$mean

#NB...STORE USABILITY IS DONE IN EXCEL
 #ACV&PCV CALCULATION

#FOR BI-MONTHLY DATA WE CALCULATE THE PCV BY MULYIPLYING BY 6 TO ANNUALIZE 


rawdata$ACV<-apply(rawdata[,c(24:13)],1,mean,na.rm=T)*12#includes all yearly sales of all categories even those which Nielsen doesnt audit

rawdata$PCV=(rawdata$mean)*12
rawdata$Ln=log(rawdata$PCV)

#OUTLIER DETECTION

index<-with(rawdata,order(IBD1,PCV))#SORTING DATA BY IBD THEN PCV
rawdata=rawdata[index,]

#LISTING ALL THE QUANTILES AT IBD LEVEL

Quartile=with(rawdata,aggregate(Ln,list(IBD1=IBD1),quantile))

quantile=data.frame(IBD1=Quartile[,1],Q1=as.numeric(Quartile[,2][,2]),Q3=as.numeric(Quartile[,2][,4]))

quantile$IQR=quantile$Q3-quantile$Q1

rawdata$Q1=quantile$Q1[match(rawdata$IBD1,quantile$IBD1)]

rawdata$Q3=quantile$Q3[match(rawdata$IBD1,quantile$IBD1)]

rawdata$IQR=quantile$IQR[match(rawdata$IBD1,quantile$IBD1)]

rawdata$LL=exp(rawdata$Q1-rawdata$IQR*1.5)

rawdata$UL=exp(rawdata$Q3+rawdata$IQR*1.5)

rawdata$PCV=round(rawdata$PCV,5)

rawdata$LL=round(rawdata$LL,5)

rawdata$UL=round(rawdata$UL,5)

rawdata$outlier1=ifelse(rawdata$PCV<rawdata$LL,1,0)

rawdata$outlier2=ifelse(rawdata$PCV>rawdata$UL,1,0)

rawdata$final_outlier=rawdata$outlier1+rawdata$outlier2

rawdata1=subset(rawdata,final_outlier==1)#outliers

rawdatafinal=subset(rawdata,final_outlier==0)#non-outliers

##XUNIVERSE,XFACTOR,ZFACTOR&BIAS CALCULATION

#SUMMARIZE VARIABLES LIKE A PIVOT TABLE IN EXCEL
#XUNIVERSE AT CELL LEVEL

library(plyr)

BB=ddply(rawdata,c("Cell.Name","IBD1"),summarise,Z.Universe=mean(Z.Universe))

C=ddply(rawdata,c("Cell.Name"),summarise,Zpanel=length(PCV))

D=ddply(rawdata,c("Cell.Name"),summarise,Xpanel=sum(PCV))

BB1=merge(BB,C)

BB2=merge(BB1,D)

E6=ddply(rawdatafinal,c("Cell.Name","IBD1"),summarise,Average=mean(PCV))
 
m=merge(BB2,E6)
 
m$X.Universe=(m$Average)*(m$Z.Universe)

m$ZF=m$Z.Universe/m$Zpanel

m$XF=m$X.Universe/m$Xpanel

#XUNIVERSE AT IBD LEVEL

E2=ddply(rawdatafinal,c("IBD1"),summarise,Average=mean(PCV))

m$AverageIBD=E2$Average[match(m$IBD1,E2$IBD1)]

m$X.UniverseIBD=m$AverageIBD*m$Z.Universe

m$Bias=m$XF/m$ZF

#EXPORTING R OUTPUT & SAVING IN EXCEL FORMAT(csv format)


COR=m

outliers=rawdata1

Non_outliers=rawdatafinal
usableCOR=rawdata
 
write.csv(usableCOR,("C:/Users/mosimi01/Desktop/Routput/usableCOR.csv"),row.names=FALSE,na="")#ACTUAL RAWDATA(WITH OUTLIERS)


write.csv(outliers,("C:/Users/mosimi01/Desktop/Routput/outliers.csv"),row.names=FALSE,na="")#OUTLIERS

 
write.csv(Non_outliers,("C:/Users/mosimi01/Desktop/Routput/Non_outliers.csv"),row.names=FALSE,na="")#Non-OUTLIERS

write.csv(COR,("C:/Users/mosimi01/Desktop/Routput/COR.csv"),row.names=FALSE,na="")#COR





