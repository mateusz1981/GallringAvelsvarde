functionPrepareraData <- function(indata,forband){
  utdata <- indata %>% 
    #omr?kning av koordinatsystem (v?nstra nedre h?rnet har koord (0,0)) samt ber?kning av ytsidornas l?ngd
      mutate(xKoord=(x-min(.$x))*forband+forband/2,yKoord=(y-min(.$y))*forband+forband/2) %>%
      mutate(xStart=min(.$xKoord)-forband/2,xSlut=max(.$xKoord)+forband/2,xSida=xSlut-xStart) %>%
      mutate(yStart=min(.$yKoord)-forband/2,ySlut=max(.$yKoord)+forband/2,ySida=ySlut-yStart) %>%
    #tilldelar tr?did
      mutate(treeID=paste(Yta,Rad,Planta,sep="_")) %>%
    #stryker träd utan för?dlingsv?rde samt v?rde f?r diameter
      filter(!is.na(Ind_114_ebv) & !is.na(D19) & D19!=0) %>%
      mutate(dcm=D19/10) %>%
    #väljer ut variabler
      select(Yta,treeID,Ind_114_ebv,dcm,xKoord,yKoord,xSida,ySida)
  
  return(utdata)
}

functionMirrorBruttoPlot <- function(netto){
  brutto<-netto[0,] #brutto_df ges samma mall som netto
  
  #speglar nettoytan i samtliga riktningar runt nettoytan
    for(xBrutto in c(-1,0,1)){
      for(yBrutto in c(-1,0,1)){
        brutto <- netto %>%
          mutate(xKoord=xKoord+xBrutto*xSida,yKoord=yKoord+yBrutto*ySida) %>%
          rbind(.,brutto)
      }
    }
    
  return(brutto)
}

functionThinning <- function(netto,gegTarget,gprocTarget,indexSelectionPositive,indexWeightDiameter,indexDistanceMetod){ 
  thinningEnd<-FALSE
  netto$thinned<-FALSE
  
  while (!thinningEnd) {
    #konstruera bruttoyta genom spegling av nettoytan
      brutto<-functionMirrorBruttoPlot(netto)
    
    #ber?kna konkurrensbidrag f?r att l?gga till ett visst tr?d
      for(i in 1:nrow(netto)) {
        #nollst?ller konkurrensparametrar
          netto$urvalIndex[i]<-0
        
        #ber?kna index f?r urval
          netto$urvalIndex[i]<-functionUrvalIndex(i,netto,brutto,radius,indexSelectionPositive,indexWeightDiameter,indexDistanceMetod)
      }
      
    #urval av gallringstr?d
      #rangordna tr?d efter urvalsindex i f?rsta hand och efter diameter i andra hand
        netto <- netto %>% arrange(-urvalIndex,dcm) %>% group_by(thinned) %>% mutate(rank = 1:n())
      
      #v?ljer ut tr?den med h?gst urvalindex f?r gallring
        netto <- netto %>% ungroup() %>% mutate(thinned=ifelse(rank==1 & thinned==FALSE,TRUE,thinned))
      
      #n?dbroms om tr?den skulle ta slut innan stopp-kriteriet ?r uppn?tt. F?rhindra evighetsloop
        if(nrow(netto[netto$thinned==FALSE,])==0) {
          message("Loop stoppad. Finns inga tr?d kvar att v?lja")
          return(netto)
          break
        } 
      
      x <- netto %>% ungroup %>% mutate(area=xSida*ySida/10000,ba=((pi*(dcm/100)^2)/4)/area) %>% summarise(gut=sum(ba[thinned==TRUE]),geg=sum(ba[thinned==FALSE]),gfg=sum(ba)) %>%
        mutate(gegTarget=geg,gprocTarget=gut/gfg*100)

      thinningEnd<-x$gprocTarget>=gprocTarget
  }
  
  return(netto)
}

functionUrvalIndex <- function(i,netto,brutto,radius,indexSelectionPositive,indexWeightDiameter,indexDistanceMetod){
  #urval av tr?d som ligger inom det aktuella tr?dets radie
    competitors <- functionFilterCompetitors(i,netto,brutto,radius)
  
  #om inget tr?d ligger inom radie: forts?tt med n?sta tr?d
    if(nrow(competitors)==0){
      ind<-0
      return(IndexHegyiInd114)
    }
  
  #ber?kna index
    #modifierat hegyi-index d?r diametern byts ut mot f?r?dlingsv?rdet. (ind+1) anv?nds f?r att undvika negativa v?rden
    if(indexSelectionPositive==TRUE){
      if(indexWeightDiameter==TRUE){
        indKvot<-((competitors$Ind_114_ebv+1)/(netto$Ind_114_ebv[i]+1))*((competitors$dcm+1)/(netto$dcm[i]+1))
      } else {
        indKvot<-(competitors$Ind_114_ebv+1)/(netto$Ind_114_ebv[i]+1)
      }
    } else {
      if(indexWeightDiameter==TRUE){
        indKvot<-((netto$Ind_114_ebv[i]+1)/(competitors$Ind_114_ebv+1))*((netto$dcm[i]+1)/(competitors$dcm+1))
      } else {
        indKvot<-(netto$Ind_114_ebv[i]+1)/(competitors$Ind_114_ebv+1)        
      }
    }
    
    if(indexDistanceMetod=="hegyi"){
      ind<-sum(indKvot*1/(competitors$dist+1))
    } else if (indexDistanceMetod=="klang"){
      ind<-sum((radius-competitors$dist)*indKvot)
    }


  return(ind)
}

functionFilterCompetitors<-function(i,netto,brutto,radius){
  
  competitors <- brutto %>% 
    mutate(dist=sqrt((netto$xKoord[i]-xKoord)^2 + (netto$yKoord[i]-yKoord)^2)) %>%
    filter(dist<=radius) %>% #tr?d inom en viss radie runt aktuellt tr?d
    filter(treeID!=netto$treeID[i]) %>% #aktuellt tr?d ska ej ing? bland konkurrenterna
    filter(thinned==FALSE) #inkludera enbart tr?d som inte har markerats som gallringstr?d
  
  return(competitors)
}
