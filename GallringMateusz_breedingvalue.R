setwd("")

rm(list=ls())

source("~functionsGallring_breedingvalue.R")

#library(plyr)
library(ggplot2)
library(spatstat)
library(tidyverse)
library(gridExtra)

#läser indata
#OBS indata behöver ha följande variabler, din fil behöver följande kolumner (case sensitive) 
##Yta - namn, "character or numeric"), RAD - rad indikation  ("numeric"), PL - planta inom rad, ("numeric"), 
## BV - avelsvärde ("numeric"), D - diameter ("numeric"), kappa = border zone ("binary 0 - inte kappa or 1 - kappa)", 
## stickväg - binary (0 1))
head(indata)

indata<-read.csv("~GallYta1.csv", stringsAsFactors = FALSE) %>%
    mutate(Rad = RAD, Planta = PL, Ind_114_ebv =  BV, D19 = D) %>%
    filter(Kappa == 0)

head(indata)
#BA per stickväg
sum(pi*(indata$D19[indata$StickVag == 1 & indata$Kappa == 0]/2000)^2, na.rm = T)

#BAtot
sum(pi*(indata$D19[indata$Kappa == 0]/2000)^2, na.rm = T)
  
#uttag med 35% styrka - stickvägar
sum(pi*(indata$D19[indata$Kappa == 0]/2000)^2, na.rm = T)*0.35
sum(pi*(indata$D19[indata$Kappa == 0]/2000)^2, na.rm = T)*0.35 - sum(pi*(indata$D19[indata$StickVag == 1 & indata$Kappa == 0]/2000)^2, na.rm = T)

#att gallra %
(sum(pi*(indata$D19[indata$Kappa == 0]/2000)^2, na.rm = T)*0.35 - sum(pi*(indata$D19[indata$StickVag == 1& indata$Kappa == 0] /2000)^2, na.rm = T))/
(sum(pi*(indata$D19[indata$Kappa == 0]/2000)^2, na.rm = T) - sum(pi*(indata$D19[indata$StickVag == 1 & indata$Kappa == 0]/2000)^2, na.rm = T))




#sätter parametrar för analysen
  #förband för beräkning av koordinater
    forband<-1.8
  
  #Radie för konkurrensindex
    radius<-5

  #Gallringsparametrar
    #gegTarget<-25 #används inte i nuläget
    gprocTarget<-29.11 #gallringsstyrka (%)
    
#definierar koordinater för nettoytan, skapar träd-ID samt rensar data
  netto <- functionPrepareraData(indata,forband)

#gallra
  #++++++++++++++++++++++++++++++++++++++++++++
  #v?lj rangordningsmetod
    indexSelectionPositive<-TRUE #Om TRUE: lämnar träd med högt index, om FALSE: lämna träd med lågt index
    indexWeightDiameter<-TRUE #TRUE: träddiametern ingår i beräkningen av index tillsammans med Ind_114_ebv, FALSE: enbart Ind_114_ebv ingår i indexberäkningen
    indexDistanceMetod<-"klang" #två alternativ: "klang"=hanterar avstånd mellan träd enligt Klang (2000), "hegyi"=hanterar avstånd mellan träd enligt Hegyi's konkurrensindex
  #++++++++++++++++++++++++++++++++++++++++++++
    
  #gallra beståndet
gallringUt<-functionThinning(netto,gegTarget,gprocTarget,indexSelectionPositive,indexWeightDiameter,indexDistanceMetod)
  
#kolla konkurrenssituation
gallrIndex<-gallringUt %>% select(Yta,treeID,Ind_114_ebv,dcm,xKoord,yKoord,thinned) %>% mutate(cat="Index")
  
  #referens d?r gallringen enbart utf?rs efter h?gsta alt. l?gsta Ind_114_ebv-v?rde
gallrReferens<-netto %>% arrange(if(indexSelectionPositive == TRUE) -Ind_114_ebv else Ind_114_ebv) %>%
    mutate(d2=dcm^2,d2csum=cumsum(d2),d2sum=max(d2csum),d2limit=d2sum*(1-gprocTarget/100)) %>%
    mutate(thinned=ifelse(d2csum<d2limit,FALSE,TRUE)) %>%
    select(Yta,treeID,Ind_114_ebv,dcm,xKoord,yKoord,thinned) %>% 
    mutate(cat="Referens")
  
  #clark-evans index
    #best?nd efter gallring
      gallrIndexKvar <- gallrIndex %>% filter(!thinned)
      referensKvar <- gallrReferens %>% filter(!thinned)
      
    #berk?kna index
      minX <- min(gallrIndexKvar$xKoord)
      maxX <- max(gallrIndexKvar$xKoord)
      minY <- min(gallrIndexKvar$yKoord)
      maxY <- max(gallrIndexKvar$yKoord)
      
      clarkevans(ppp(gallrIndexKvar$xKoord,gallrIndexKvar$yKoord,c(minX, maxX),c(minY, maxY),correction=c("Donnelly","none","cdf")))  
      clarkevans(ppp(referensKvar$xKoord,referensKvar$yKoord,c(minX, maxX),c(minY, maxY), correction=c("Donnelly","none","cdf")))  
 
  
      
    #medel får avelsvärde
    gallrIndex %>% filter(!thinned) %>% summarise(Ind_114_ebv=mean(Ind_114_ebv))
    
    gallrReferens %>% filter(!thinned) %>%
      summarise(Ind_114_ebv=mean(Ind_114_ebv))
    
    
    ###före och efter gallring
    gallrIndex %>%  summarise(sr = mean(Ind_114_ebv))
    gallrIndex %>% group_by(thinned) %>% summarise(sr = mean(Ind_114_ebv), md = mean(dcm, na.rm = T))
      
    

  #kartor ?ver best?ndet efter gallringen enligt gallringsindex samt enbart efter f?r?dlingsindex (Ind_114_ebv)
    p1<-ggplot(gallrIndex,aes(x=xKoord,y=yKoord,size=dcm,group=thinned,fill=thinned)) + geom_point(shape=21) + 
      ggtitle("Urval baserat på index") + theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(breaks=c(FALSE,TRUE),values=c("black", "white"))
    p2<-ggplot(gallrReferens,aes(x=xKoord,y=yKoord,size=dcm,group=thinned,fill=thinned)) + geom_point(shape=21) + 
      ggtitle("Urval baserat enbart på Ind_114_ebv") + theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(breaks=c(FALSE,TRUE),values=c("black", "white")) 
    grid.arrange(p1,p2,ncol=2)
  
  #histogram f?r f?rdelningen av Ind_114_ebv och diameter efter gallring
    kvar<-rbind(gallrIndexKvar,referensKvar)
    p1<-ggplot(NULL) + geom_histogram(data=kvar,aes(Ind_114_ebv,group=cat,fill=cat),binwidth=0.1, colour="black", position="dodge")+ 
      ggtitle("Ind_114_ebv efter gallring") + theme(plot.title = element_text(hjust = 0.5))
    p2<-ggplot(NULL) + geom_histogram(data=kvar,aes(dcm,group=cat,fill=cat),binwidth=0.5, colour="black", position="dodge")+ 
      ggtitle("Diameter efter gallring") + theme(plot.title = element_text(hjust = 0.5))
    grid.arrange(p1,p2,ncol=2)
    