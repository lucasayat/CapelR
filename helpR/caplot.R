### construction des fichiers references
options(stringsAsFactors=FALSE)
#  references<-function(){
   
#     library(xlsx)
#     options(stringsAsFactors=FALSE)
#     patauv<-read.xlsx("G:/Documents/AR/CapelR/ref/refan.xlsx", sheetName="patauv")
#     cfz1<-read.xlsx("G:/Documents/AR/CapelR/ref/refan.xlsx", sheetName="CFZ1")
#     cfz2<-read.xlsx("G:/Documents/AR/CapelR/ref/refan.xlsx", sheetName="CFZ2")
#     cvz1<-read.xlsx("G:/Documents/AR/CapelR/ref/refan.xlsx", sheetName="CVZ1")
#     four<-read.xlsx("G:/Documents/AR/CapelR/ref/refan.xlsx", sheetName="four")
#     conc<-read.xlsx("G:/Documents/AR/CapelR/ref/refan.xlsx", sheetName="conc")
#     eco<-read.xlsx("G:/Documents/AR/CapelR/ref/refan.xlsx", sheetName="eco")
#     pac<-read.xlsx("G:/Documents/AR/CapelR/ref/refan.xlsx", sheetName="PAC")
#      pateco<-read.xlsx("G:/Documents/AR/CapelR/ref/refan.xlsx", sheetName="pateco")
#     ref<-list(patauv,cfz1,cfz2,cvz1,four,conc,eco,pac,pateco)
#     saveRDS(ref,"G:/Documents/AR/CapelR/ref/ref") 
#      ref<-readRDS("G:/Documents/AR/CapelR/ref/ref") 

#      }

refan<-function(refods,refod){  
# library("readODS")
# filods<-"ref/refinosys.ods"
# refods<-read.ods(filods)
# refod<-readRDS("ref/refinosys")

for (i in 1:9)
{
 refod[[i]]<-refods[[i]][2:nrow(refods[[i]]),]

   stringToNumeric <- function(f) as.numeric(f)  
   a<-refod[[i]]
   b<-ifelse(i==1,3,2)
   cols <- c(b:ncol(a))
   a[cols] <- lapply(a[cols], stringToNumeric)
   refod[[i]]<-a
 colnames(refod[[i]])<-refods[[i]][1,]
}

saveRDS(refod,"ref/refperso")
ref<-readRDS("ref/refperso")

# p<-readRDS("data/capelinit")
# p[[2]]<-refod
# saveRDS(p,"data/capelinit")
# refod$an<-"2014"
# saveRDS(refod,"ref/refinosys")
}


zones<-function(surf,code){
   if (surf=="pat")
   {
     zon<-switch(code,
                 "auvergne"=1)
   }
   
   if (surf=="cf")
   {
   zon<-switch(code,
               "saisie"=2,
               "Zone1"=2,
               "Zone2"=3)
      
   }
   
   if (surf=="cv")
   {
     zon<-switch(code,
                 "saisie"=4,
                 "Centre"=4,
                 "Auvergne"=4)
     
   }
   return(zon)
   }




grafpat<- function(pat) {
  
 # pat<-c(30,10,75,20,41,30,16,40, 24,20,70,30)
#   

 par(bg = "transparent", mar = c(1, 4, 1, 0))
    VLp<-pat[1]
   surf_VLprint<-pat[2]
  UGBP<-pat[3]
   surf_ugbaprint<-pat[4]
   rec_print<-pat[5]
  patot<-pat[11]
  x <- matrix(c(surf_VLprint,rec_print, surf_ugbaprint))
  coul1 <- c("green4", "transparent", "green")
 
  bar1 <- barplot(x, col = coul1, xlim = c(1.5, 20), width =5, ylim = c(0,patot+10), 
                  cex.axis = 1, ylab = "ha", border = coul1)

  a1 <- surf_VLprint
  text(bar1, patot+5, labels = "1ere periode", cex =1.5, font = 2, col = "blue")
  text(bar1, a1-1, labels =paste(VLp, "vaches sur",surf_VLprint,"ha"), cex = 0.9, font = 2, col = "white")
  text(bar1, a1-3, labels = paste(round(surf_VLprint*100/VLp,0),"ares/VL"), cex = 0.9, font = 3, col = "white")
  text(bar1, a1+rec_print/2, labels = "Surface fauchee", cex = 0.9, font = 2, col = "green4")
   text(bar1, a1+rec_print/2-3, labels = paste(rec_print,"ha"), cex = 0.9, font = 2, col = "green4")
  text(bar1,patot-1,labels=paste(UGBP-VLp,"autres ugb sur",surf_ugbaprint,"ha"),cex = 0.9, col = "blue",font=2)
  text(bar1, patot-3, labels = paste(round(surf_ugbaprint*100/(UGBP-VLp)),"ares/UGB"), cex = 0.9, font = 3, col = "blue")
  
  VLete<-pat[6] 
  surf_VLete<-pat[7]
  surf_ugbaete<-pat[8]
  rec_ete<-pat[9]
  
  y <- matrix(c(surf_VLete,rec_ete, surf_ugbaete))
 coul2 <- c("green4", "transparent", "green")
  
  bar2 <- barplot(y, col = coul2, add = T, width = 6, space = 1, axes = F, border = "white")
  
  a2 <- surf_VLete
 text(bar2, patot+5, labels = "2eme periode", cex =1.5, font = 2, col = "blue")
 text(bar2, a2-1, labels =paste(VLete, "vaches sur",surf_VLete,"ha"), cex = 0.9, font = 2, col = "white")
 text(bar2, a2-3, labels = paste(round(surf_VLete*100/VLete,0),"ares/VL"), cex = 0.9, font = 3, col = "white")
 text(bar2, a2+rec_ete/2, labels = "Surface fauchee", cex = 0.9, font = 2, col = "green4")
 text(bar2, a2+rec_ete/2-3, labels = paste(rec_ete,"ha"), cex = 0.9, font = 2, col = "green4")
 text(bar2,patot-1,labels=paste(UGBP-VLete,"autres ugb sur",surf_ugbaete,"ha"),cex = 0.9, col = "blue",font=2)
 text(bar2, patot-3, labels = paste(round(surf_ugbaete*100/(UGBP-VLete)),"ares/UGB"), cex = 0.9, font = 3, col = "blue")
 
  patvlo<-pat[10]
  VLo<-pat[12]
  z <- matrix(c(patvlo,patot-patvlo))
 coul3 <-  c("green4","green")
 
 bar3 <- barplot(z, col = coul3, add = T, width = 7, space = 1.7, axes = F, border = "white")

 abline(h=(seq(0,round(patot/10,0)*10+10,10)), col="lightgray", lty="dotted")
 
 a3 <- patvlo
 text(bar3, patot+5, labels = "3eme periode", cex =1.5, font = 2, col = "blue")
 text(bar3, a3-1, labels =VLo , cex = 0.9, font = 2, col = "white")
 text(bar3, a3-3, labels = "vaches traites sur", cex = 0.9, font = 2, col = "white")
 text(bar3, a3-5, labels = paste(patvlo,"ha"), cex = 0.9, font = 2, col = "white")
 text(bar3, a3-7, labels = paste(round(patvlo*100/VLo,0),"ares/UGB"), cex = 0.9, font = 3, col = "white")
 text(bar3, patot-1, labels =(UGBP-VLo) , cex = 0.9, font = 2, col = "blue")
 text(bar3,patot-3,labels="autres ugb sur",cex = 0.9, font = 2,col="blue")
 text(bar3, patot-5, labels = paste(patot-patvlo,"ha"), cex = 0.9, font = 2,col="blue")
 text(bar3, patot-7, labels = paste(round((patot-patvlo)*100/(UGBP-VLo),0),"ares/UGB"), cex = 0.9, font = 3,col="blue")
 
}

################
grafsto<- function(rec1C,rec2C) {
  
  #rec1C<-c(15,2,6,8)
  
  par(bg = "transparent", mar = c(5, 4, 3, 0))
  
 # refou<-read.csv2("ref/Refourauv1.csv")
      
  HaFoinTard<-as.numeric(rec1C[1])
  HaFoinprec<-as.numeric(rec1C[2])
  HaEnr<-as.numeric(rec1C[3])
  HaEns<-as.numeric(rec1C[4])
  HaFoin2C<-as.numeric(rec2C)
 
   rech<-data.frame(four=c("Foin 1ereC - 2eme per.","Foin 1ereC - 1ere per.","Enrubannage","Ensilage"),
                    surfou=c(HaFoinTard,HaFoinprec,HaEnr,HaEns))
 
 rec<-HaFoinTard+ HaFoinprec+HaEnr+HaEns
 
  x <- matrix(c(HaFoinTard,HaFoinprec,HaEnr,HaEns))
 coul1 <- c("yellow", "yellowgreen","orchid", "pink","cyan")
  
  bar1 <- barplot(x, col = coul1, xlim = c(1, 12), width =6, ylim =c(0,rec+2), main="Surfaces de fauche 1ere et 2eme periode.",
                  cex.axis = 1, ylab = "ha", border = "yellow",names.arg="Printemps")
                  

 lab1<-c("")
 for(i in 1:nrow(rech))  
     {
  
   if(rech$surfou[i]>0){
       if(i==1) {  lab1[i]<- lab1[i]<-rech$four[i]
            }else{
                lab1[i]<-paste(rech$four[i],round(rech$surfou[i],1),"ha")
            }
        }else{
          lab1[i]<-"" 
        }}  
 
 text(x=bar1,y=cumsum(rech[,2])-rech[,2]/2,label = lab1,cex=1)
   
  y <- matrix(c(HaFoinTard,HaFoin2C))
  coul2 <- c("yellow", "cyan")
  
  bar2 <- barplot(y, col = coul2, add = T, width = 6, space = 1.2,names.arg="Ete", 
                  axes = F, border = "yellow")
 
 if(HaFoinTard>0){text(x=bar2,y=HaFoinTard/2,label = paste("Recolte 2eme periode ",round(HaFoinTard,1),"ha"),cex=1)}
 if(HaFoin2C>0) {text(x=bar2,y=HaFoinTard+HaFoin2C/2,label = paste("2emes coupes",round(HaFoin2C,1),"ha"),cex=1)}
  
  #abline(h=(seq(0,round(rec/10,0)*10+10,10)), col="lightgray", lty="dotted",lwd=2)
  
}

#############

stock_herbe<-function(recherbe){
  #stock_herbe(recherbe=c(TfoinTard,TfoinPre,Tenr,Tens,reg,Trecot)) 
  #recherbe<-c(25,10,9,7,8,10)
  par(bg = "transparent", mar = c(5, 4, 3, 2))
  
   FoinTard<-round(recherbe[1],1)
   FoinPrec<-round(recherbe[2],1)
   Enr<-round(recherbe[3],1)
   Ens<-round(recherbe[4],1)
   reg<-round(recherbe[5],1)
   Recot<-round(recherbe[6],1)
    
  stoh<-round(FoinTard+ FoinPrec+Enr+Ens+reg+Recot,0)
  
  x <- matrix(c(FoinTard,FoinPrec, Enr,Ens,reg,Recot))
  coul1 <- c("yellow", "yellowgreen","orchid", "pink","cyan","coral")
  
  bar<- barplot(x, col = coul1, xlim = c(1, 6), width =6, ylim =c(0,stoh+2), main="Stocks sur patures",
                  cex.axis = 1, ylab = "TMS", border = "yellow")
  mtext(paste(stoh,"TMS au total"),col="blue",font=2,cex=1)
  abline(h=(seq(0,round(stoh/10,0)*10+10,5)), col="lightgray")
  
  if(FoinTard>0){text(bar, FoinTard/2, labels = paste(FoinTard,"Tms"), cex =1, font = 2)} 
  if(FoinPrec>0){text(bar, FoinTard+FoinPrec/2, labels = paste(FoinPrec,"Tms"), cex =1, font = 2)} 
  if(Enr>0){text(bar, FoinTard+FoinPrec+Enr/2, labels = paste(Enr,"Tms"), cex =1, font = 2)} 
  if(Ens>0){text(bar, FoinTard+FoinPrec+Enr+Ens/2, labels = paste(Ens,"Tms"), cex =1, font = 2)}
  if(reg>0){text(bar, FoinTard+FoinPrec+Enr+Ens+reg/2, labels = paste(reg,"Tms"), cex =1, font = 2)}
  if(Recot>0){text(bar, FoinTard+FoinPrec+Enr+Ens++reg+Recot/2, labels = paste(Recot,"Tms"), cex =1, font = 2)}
  if(Recot>0){text(bar, FoinTard+FoinPrec+Enr+Ens++reg+Recot/1.5, labels = "Autres coupes :", cex =1, font = 2)}
}


###########

bestock<-function(besani,stocherbe,CF=FALSE,cufou,cfnom, AF=FALSE,achafou,nachafou,ventefou){
#   besani<-c(100,45,9)  
#   stocherbe<-69
  par(bg = "transparent", mar = c(5, 4, 3, 2))
  
  bestot<-besani[1]+besani[2]+besani[3]
  if(AF==TRUE){
    sto<-stocherbe+cufou[1]+cufou[2]+cufou[3]+cufou[4]+cufou[5]+achafou[1]+achafou[2]+achafou[3]
  }else{
    if(CF==TRUE){ sto<-stocherbe+cufou[1]+cufou[2]+cufou[3]+cufou[4]+cufou[5]
    }else{
    sto<-stocherbe
  }}
  
  
  maxy<-max(bestot,sto)
  x <- matrix(c(besani[1],besani[2], besani[3]))
  coul1 <- c("yellow", "yellowgreen","orchid")
  
  
  bar1<- barplot(x, col = coul1, xlim = c(1, 12), width =5,
                 ylim =c(0,maxy*1.2), main="",names.arg=paste("Besoins =",bestot,"Tms"),cex.names =1.5,
                cex.axis = 1, ylab = "TMS", border = "yellow")
  #mtext(paste(bestot,"TMS au total"),col="blue",font=2,cex=1.2)
 
  
   if(besani[1]>0){text(bar1, besani[1]/2, labels = paste("Vaches laitieres:",besani[1],"Tms"), cex =1, font = 2)} 
  if(besani[2]>0){text(bar1, besani[1]+besani[2]/2, labels = paste("Autres UGB paturants:",besani[2],"Tms"), cex =1, font = 2)} 
  if(besani[3]>0){text(bar1, besani[1]+besani[2]+besani[3]/2, labels = paste("Autres animaux:",besani[2],"Tms"), cex =1, font = 2)}
 
  if(CF==FALSE & AF==FALSE)
  {
  
  manque<-round(bestot-stocherbe,1)
  if(manque>=0){
    y<-matrix(c(stocherbe,manque))
    coul2<-c("green","transparent")
    bar2 <- barplot(y, col = coul2, add = T, width = 5,space = 1.5, names.arg=paste("Stocks =",round(stocherbe,0),"Tms"),cex.names =1.5,
                    axes = F, border = "white")
    text(bar2,stocherbe/2,labels=paste("Stocks herbe",stocherbe,"Tms"), cex =1, font = 2)
    text(bar2,stocherbe+manque/2,labels=paste("Manque",manque,"Tms"), cex =1, font = 2)
    
  }else{
    coul2<-c("green")
    bar2 <- barplot(stocherbe, col = coul2, add = T, width = 5,space = 1.5,names.arg=paste("Stocks =",round(stocherbe,0),"Tms"),cex.names =1.5,
                    axes = F, border = "white")
    text(bar2,stocherbe/2,labels=paste("Stocks herbe",stocherbe,"Tms"), cex =1, font = 2)
  
  }}
  
  if(CF==TRUE & AF==FALSE )
  {
      y<-matrix(c(stocherbe,cufou[1],cufou[2],cufou[3],cufou[4],cufou[5]))
      coul2<-c("green","salmon1","salmon2","salmon3","salmon4","orchid")
      
bar2 <- barplot(y, col = coul2, add = T, width = 5,space = 1.5, names.arg=paste("Stocks =",round(sto,0),"Tms"),cex.names =1.5,,
                      axes = F, border = "white")
     if(stocherbe>0) {text(bar2,stocherbe/2,labels=paste("Stocks herbe",stocherbe,"Tms"), cex =1, font = 2)}
    if(cufou[1]>0) {text(bar2,stocherbe+cufou[1]/2,labels=paste(cfnom[1],cufou[1],"Tms"), cex =1, font = 2)}
    if(cufou[2]>0) {text(bar2,stocherbe+cufou[1]+cufou[2]/2,labels=paste(cfnom[2],cufou[2],"Tms"), cex =1, font = 2)}
     if(cufou[3]>0) {text(bar2,stocherbe+cufou[1]+cufou[2]+cufou[3]/2,labels=paste(cfnom[3],cufou[3],"Tms"), cex =1, font = 2)}
    if(cufou[4]>0) {text(bar2,stocherbe+cufou[1]+cufou[2]+cufou[3]+cufou[4]/2,labels=paste(cfnom[4],cufou[4],"Tms"), cex =1, font = 2)}
if(cufou[5]>0) {text(bar2,stocherbe+cufou[1]+cufou[2]+cufou[3]+cufou[4]+cufou[5]/2,labels=paste(cfnom[5],cufou[5],"Tms"), cex =1, font = 2)}

  }

if(CF==TRUE & AF==TRUE )
{
  y<-matrix(c(stocherbe-ventefou,cufou[1],cufou[2],cufou[3],cufou[4],cufou[5],achafou[1],achafou[2],achafou[3]))
  coul2<-c("green","salmon1","salmon2","salmon3","salmon4","orchid","lightblue1","lightblue2","lightblue3")
  sto<-stocherbe-ventefou+cufou[1]+cufou[2]+cufou[3]+cufou[4]+cufou[5]+achafou[1]+achafou[2]+achafou[3]
  bar2 <- barplot(y, col = coul2, add = T, width = 5,space = 1.5, names.arg=paste("Stocks =",round(sto,0),"Tms"),cex.names =1.5,,
                  axes = F, border = "white")
  if(stocherbe>0) {text(bar2,(stocherbe-ventefou)/2,labels=paste("Stocks herbe",stocherbe,"Tms"), cex =1, font = 2)}
  if(cufou[1]>0) {text(bar2,stocherbe-ventefou+cufou[1]/2,labels=paste(cfnom[1],cufou[1],"Tms"), cex =1, font = 2)}
  if(cufou[2]>0) {text(bar2,stocherbe-ventefou+cufou[1]+cufou[2]/2,labels=paste(cfnom[2],cufou[2],"Tms"), cex =1, font = 2)}
  if(cufou[3]>0) {text(bar2,stocherbe-ventefou+cufou[1]+cufou[2]+cufou[3]/2,labels=paste(cfnom[3],cufou[3],"Tms"), cex =1, font = 2)}
  if(cufou[4]>0) {text(bar2,stocherbe-ventefou+cufou[1]+cufou[2]+cufou[3]+cufou[4]/2,labels=paste(cfnom[4],cufou[4],"Tms"), cex =1, font = 2)}
  if(cufou[5]>0) {text(bar2,stocherbe+cufou[1]+cufou[2]+cufou[3]+cufou[4]+cufou[5]/2,labels=paste(cfnom[5],cufou[5],"Tms"), cex =1, font = 2)}
  
  if(achafou[1]>0) {text(bar2,stocherbe-ventefou+cufou[1]+cufou[2]+cufou[3]+cufou[4]+cufou[5]+achafou[1]/2,
                         labels=paste(nachafou[1],achafou[1],"Tms"), cex =1, font = 2)}
  if(achafou[2]>0) {text(bar2,stocherbe-ventefou+cufou[1]+cufou[2]+cufou[3]+cufou[4]+cufou[5]+achafou[1]+achafou[2]/2,
                         labels=paste(nachafou[2],achafou[2],"Tms"), cex =1, font = 2)}
  if(achafou[3]>0) {text(bar2,stocherbe-ventefou+cufou[1]+cufou[2]+cufou[3]+cufou[4]+cufou[5]+achafou[1]+achafou[2]+achafou[3]/2,
                         labels=paste(nachafou[3],achafou[3],"Tms"), cex =1, font = 2)}
}
   

}

#####################plot conso et stocks herbe

cvrec<-function(){
  #   besani<-c(100,45,9)  
  #   stocherbe<-69
  par(bg = "transparent", mar = c(5, 4, 3, 2))
  
  bestot<-besani[1]+besani[2]+besani[3]
  x <- matrix(c(besani[1],besani[2], besani[3]))
  coul1 <- c("yellow", "yellowgreen","orchid")
  
  
  bar1<- barplot(x, col = coul1, xlim = c(1, 12), width =5,
                 ylim =c(0,bestot+2), main="",names.arg=paste("Besoins =",bestot,"Tms"),cex.names =1.5,
                 cex.axis = 1, ylab = "TMS", border = "yellow")
  #mtext(paste(bestot,"TMS au total"),col="blue",font=2,cex=1.2)
  
  
  if(besani[1]>0){text(bar1, besani[1]/2, labels = paste("Vaches laitieres:",besani[1],"Tms"), cex =1, font = 2)} 
  if(besani[2]>0){text(bar1, besani[1]+besani[2]/2, labels = paste("Autres UGB paturants:",besani[2],"Tms"), cex =1, font = 2)} 
  if(besani[3]>0){text(bar1, besani[1]+besani[2]+besani[3]/2, labels = paste("Autres animaux:",besani[2],"Tms"), cex =1, font = 2)}
  
  if(CF==FALSE)
  {
    manque<-bestot-stocherbe
    if(manque>=0){
      y<-matrix(c(stocherbe,manque))
      coul2<-c("green","cyan")
      bar2 <- barplot(y, col = coul2, add = T, width = 5,space = 1.5, names.arg=paste("Stocks =",stocherbe,"Tms"),cex.names =1.5,
                      axes = F, border = "white")
      text(bar2,stocherbe/2,labels=paste("Stocks herbe",stocherbe,"Tms"), cex =1, font = 2)
      text(bar2,stocherbe+manque/2,labels=paste("Manque",manque,"Tms"), cex =1, font = 2)
      
    }else{
      coul2<-c("green")
      bar2 <- barplot(stocherbe, col = coul2, add = T, width = 5,space = 1.5, names.arg="Stocks",
                      axes = F, border = "white")
      
    }}
  
  if(CF==TRUE)
  {
    y<-matrix(c(stocherbe,cufou[1],cufou[2],cufou[3],cufou[4]))
    coul2<-c("green","salmon1","salmon2","salmon3","salmon4")
    sto<-stocherbe+cufou[1]+cufou[2]+cufou[3]+cufou[4]
    bar2 <- barplot(y, col = coul2, add = T, width = 5,space = 1.5, names.arg=paste("Stocks =",sto,"Tms"),cex.names =1.5,,
                    axes = F, border = "white")
    if(stocherbe>0) {text(bar2,stocherbe/2,labels=paste("Stocks herbe",stocherbe,"Tms"), cex =1, font = 2)}
    if(cufou[1]>0) {text(bar2,stocherbe+cufou[1]/2,labels=paste(cfnom[1],cufou[1],"Tms"), cex =1, font = 2)}
    if(cufou[2]>0) {text(bar2,stocherbe+cufou[1]+cufou[2]/2,labels=paste(cfnom[2],cufou[2],"Tms"), cex =1, font = 2)}
    if(cufou[3]>0) {text(bar2,stocherbe+cufou[1]+cufou[2]+cufou[3]/2,labels=paste(cfnom[3],cufou[3],"Tms"), cex =1, font = 2)}
    if(cufou[4]>0) {text(bar2,stocherbe+cufou[1]+cufou[2]+cufou[3]+cufou[4]/2,labels=paste(cfnom[4],cufou[4],"Tms"), cex =1, font = 2)}
  }
  
}

###########################plot recolte cv


recv<-function(nomcul,surcul,recven,intrani,intravl){
 
#   nomcul<-c("a","b","c","d")
#   surcul<-c(12,5,8,6)
#   recven<-c(120,25,100,52)
#   intrani<-c(20,5,30,2)
#   intravl<-c(30,2,50,1)
  
  
  par(bg = "transparent", mar = c(5, 4, 7,2))
  
  rectot<-recven[1]+recven[2]+recven[3]+recven[4]+intrani[1]+intrani[2]+intrani[3]+intrani[4]+intravl[1]+intravl[2]+intravl[3]+intravl[4]
  recmax<-max(recven[1]+intrani[1]+intravl[1],
                recven[2]+intrani[2]+intravl[2],
                recven[3]+intrani[3]+intravl[3],     
                recven[4]+intrani[4]+intravl[4])
  editot<-recmax*0.1
  editrdt<-recmax*0.05
  x1 <- matrix(c(intravl[1],intrani[1], recven[1]))
  coul<- c("orchid", "yellowgreen","yellow")
  
  argcv<-c("")
  for(i in 1:4)
  {
    if (nomcul[i] == "saisie")
    {
      argcv[i]<-""
    }else{
      argcv[i]<-paste(nomcul[i],round(surcul[i],1),"ha")
    }
  }
  
  
  bar1<- barplot(x1, col = coul, xlim = c(1, 12), width =3,
                 ylim =c(0,recmax*1.15), main="",names.arg=argcv[1],cex.names =1,
                 cex.axis = 1, ylab = "Tonnes", border = "blue")
  
  if(intravl[1]>0){text(bar1, intravl[1]/2, labels = paste("VL",intravl[1],"T"), font = 1)} 
  if(intrani[1]>0){text(bar1, intravl[1]+intrani[1]/2, labels = paste("Autres UGB",intrani[1],"T"), font = 1)} 
  if(recven[1]>0){text(bar1, intravl[1]+intrani[1]+recven[1]/2, labels = paste("vendu",recven[1],"T"),font = 2)}
    rec<-recven[1]+intrani[1]+intravl[1]
  if(rec>0){
       text(bar1,rec+editot,labels=paste("Recolte",rec,"T"),font=2,col="red",cex=1.2);
            text(bar1,rec+editrdt,labels=paste(round(rec*10/surcul[1],0),"Qx/ha"),font=3,col="blue",cex=1) 
  }
  
  for(i in 2:4)
     {
     y<- matrix(c(intravl[i],intrani[i], recven[i]))
      bar <- barplot(y, col = coul, add = T, width =3,space = i-0.8, names.arg=argcv[i],cex.names =1,
                     axes = F, border = "blue") 
     if(intravl[i]>0){text(bar, intravl[i]/2, labels = paste("VL",intravl[i],"T"), font = 1)} 
     if(intrani[i]>0){text(bar, intravl[i]+intrani[i]/2, labels = paste("Autres UGB",intrani[i],"T"),  font = 1)} 
     if(recven[i]>0){text(bar, intravl[i]+intrani[i]+recven[i]/2, labels = paste("vendu",recven[i],"T"),  font = 2)} 
     rec[i]<-recven[i]+intrani[i]+intravl[i]
     if(rec[i]>0){text(bar,max(rec[i]+editot),labels=paste("Recolte",rec[i],"T"),font=2,col="red",cex=1.2);
                  text(bar,max(rec[i]+editrdt),labels=paste(round(rec[i]*10/surcul[i],0),"Qx/ha"),font=3,col="blue",cex=1) }

     }
   
    mtext(paste("Recolte totale:",rectot,"Tonnes"),col="red",cex=1.5,font=2)
   grid()
   
}


###### plot conso conc

consconc<-function(nbvl,nugb,nomcul,intravl,g_l,rdtlait,intrani,ncon,tconvl,tconani){
  
  # nbvl=60;nugb=25;nomcul<-c("a","b","c","d");intravl<-c(5,0,0,0);intrani<-c(0,0,3,2) ;
  #ncon<-c("e","f","g","h","i","j");tconvl<-c(0,0,0,0,0,0);tconani<-c(0,0,0,0,0,0)
  # g_l<-100;rdtlait<-8000
  #intravl<-c(25,0,0,0)
  par(bg = "transparent", mar = c(5, 4, 5, 2))  
  convl <- 1000*matrix(c(intravl,tconvl))/nbvl
  congvl<-round((sum(intravl)+sum(tconvl))*1000/nbvl,0)
  nconvl<-c(nomcul,ncon)
  coul1<- c(rep("yellow",length(intravl)),rep("cyan",length(tconvl)-1),"lavender")
   
  maxy<-max(congvl,(sum(intrani*1000)+sum(tconani*1000))/nugb)
  totconvl<-round(sum(tconvl)+sum(intravl),0)
  bar1<- barplot(convl, col = coul1, xlim = c(0.5, 6), width =2.8,
                 ylim =c(0,maxy+100), main="",names.arg=paste("Vaches laitieres",totconvl,"T"),cex.names =1.5,
                 cex.axis = 1, ylab = "Kg/UGB", border = "blue")
  objecon<-g_l*rdtlait/1000
  lines(c(0.5,3.4),c(objecon,objecon),col="red",lwd=3)
  text(3.7,objecon,col="red",label=paste(g_l,"g/l"))
 

  lines(c(0.5,3.4),c(congvl,congvl),col="blue",lwd=3)
  text(0.5,congvl*1.04,col="blue",label=paste(round(1000*congvl/rdtlait,0),"g/l"))
  
  lab1<-c("")
  for(i in 1:length(convl))
  {
    if(convl[i]>0) {lab1[i]<-paste(nconvl[i],round(convl[i],0),"Kg")
    }else{
      lab1[i]<-""
    }  
  }   
  text(x=bar1,y=cumsum(convl[,1])-convl[,1]/2,label = lab1,cex=1)
  
  conani <- 1000*matrix(c(intrani,tconani)/nugb)
  nconani<-c(nomcul,ncon)
  coul2<- c(rep("yellow",length(intrani)),rep("cyan",length(tconani)-1),"lavender") 
  totconani<-round(sum(tconani)+sum(intrani),0)
  bar2 <- barplot(conani, col = coul2, add = T, width =2.8,space = 1.3, 
                  names.arg=paste("Autres UGB",totconani,"T"),cex.names =1.5,axes = F, border = "blue")
  lab2<-c("")
  for(i in 1:length(conani))
  {
    if(conani[i]>0) {lab2[i]<-paste(nconani[i],round(conani[i],0),"Kg")
    }else{
      lab2[i]<-""
    }  
  }   
  text(x=bar2,y=cumsum(conani[,1])-conani[,1]/2,label = lab2,cex=1)
  
}


####### plot eco

barseul<-function(titre,vec,nvec){
  
  #vec<-c(100,20,50,60,20)
  #nvec<-c("Lait","Viande BL","Viande BV","Cultures","Aides")
  #titre<-"produit"
  
  par(bg = "transparent", mar = c(5, 4, 5, 2))    
  #coul<- c("cyan","plum","red","yellow","orange","lavender","yellowgreen") 
  coul<-brewer.pal(9,"Pastel1")
  sovec<-round(sum(vec),0)
  elem<-matrix(vec)
  
  bar<- barplot(elem, col = coul, ylim =c(0,sovec*1.1), main=paste(titre,sovec,"Euros"), ylab = "Euros", 
                border = "blue",cex.main=2)

  lab<-c("")
  for(i in 1:length(vec))
  {
    if(vec[i]>0) {lab[i]<-paste(nvec[i],round(elem[i],0),"Euros")
    }else{
      lab[i]<-""
    }  
  }   
  text(x=bar,y=cumsum(elem[,1])-elem[,1]/2,label = lab,cex=1.2)
  
  
}


reseco<-function(pblait,pbviabl,pbviabv,pbcul,aides,otreprod,chopani,chopsurf,chstruc,annu,msa){
  
  #pblait<-100;pbviabl<-20;pbviabv<-10;pbcul<-60;aides<-10;otreprod<-10;chopani<-50;chopsurf<-30;chstruc<-50;annu<-30;msa<-20
   
  par(bg = "transparent", mar = c(5, 4, 5, 2))  

  coul1<-brewer.pal(6,"Pastel1")
  produit<-pblait+pbviabl+pbviabv+pbcul+aides+otreprod
  prod<-matrix(c(pblait,pbviabl,pbviabv,pbcul,aides,otreprod))
  
  bar1<- barplot(prod, col = coul1, xlim = c(0.5, 6), width =1.8,axes=FALSE,
                 ylim =c(0,produit*1.1), main="", cex.axis = 1, ylab = "", border = "blue")
  
   text(x=bar1,y=produit*1.05,labels=paste("Produits",round(produit,0),"Euros"),cex=1.5,col="blue")
  
  nprod<-c("Lait","Viande BL","Viande BV","Cultures","Aides","Autres produits")
  lab1<-c("")
  for(i in 1:length(prod))
  {
    if(prod[i]>0) {lab1[i]<-paste(nprod[i],round(prod[i],0),"Euros")
    }else{
      lab1[i]<-""
    }  
  }   
  text(x=bar1,y=cumsum(prod[,1])-prod[,1]/2,labels = lab1,cex=1)
  

  EBE<-produit-chopani-chopsurf-chstruc-msa
  cha<- matrix(c(EBE,msa,chstruc,chopsurf,chopani))
  
  coul2<-brewer.pal(5,"Pastel2")

  bar2 <- barplot(cha, col = coul2, add = T, width =1.8,space = 1.3, main="",axes = F, border = "blue")
  
  text(x=bar2,y=produit*1.05,labels=paste("Charges",round(produit-EBE,0),"Euros"),cex=1.5,col="blue")

  lab2<-c("")
  ncha<-c("EBE","MSA","Charges fixes","Charges surfaces","Charges animales")
  for(i in 1:length(cha))
  {
    if(cha[i]>0) {lab2[i]<-paste(ncha[i],round(cha[i],0),"Euros")
    }else{
      lab2[i]<-""
    }  
  }   
  text(x=bar2,y=cumsum(cha[,1])-cha[,1]/2,label = lab2,cex=1)

  
    
    dispo<-produit-chopani-chopsurf-chstruc-annu-msa
    rev<- matrix(c(dispo,annu))
    
    coul3<- brewer.pal(3,"Set3")
    
    bar3<- barplot(rev, col = coul3, add = T, width =1.8,space = 2.4, axes = F, border = "blue")
    
    lab3<-c("")
    nrev<-c("Disponible","annuites")
    for(i in 1:length(rev))
    {
      if(rev[i]>0) {lab3[i]<-paste(nrev[i],round(rev[i],0),"Euros")
      }else{
        lab3[i]<-""
      }  
    }   
    text(x=bar3,y=cumsum(rev[,1])-rev[,1]/2,label = lab3,cex=1)
      
}

############## fonctions pour edition et choix des résultats

calculex<-function(p,w){
  # p<-readRDS("G:/Documents/AR/caperso/6m.rds")[[1]]
  # w= projets selectionnés pour l'édition
  projex<-p[[2]][w,2]
  
  umoex<-p[[3]][w,3:5]
  surfex<-p[[3]][w,c(6,7,29,8,9,10)]
  animex<-p[[3]][w,c(11,12,13,14,15,16,17)]
  ex<-cbind(umoex,surfex,animex)
  ex$laiprod<-round(ex$ex_vl*ex$ex_rdtvl/1000,0)
  ex<-as.data.frame(t(as.matrix(ex)))
  colnames(ex)<-projex
  rownames(ex)<-c("UMO exploitant","UMO salaries","UMO benevoles","SAU","STH","PT","Ensilable",
                  "Paturable VT","Dt non  recoltable","Nombre de vaches","rendement laitier","UGB",
                  "UGB estives","UGB non paturant","UGB lait","Vaches allaitantes","Lait produit")
  
  return(ex)
}


calcusurf<-function(p,w){
  # p<-readRDS("data/capelist")
  proj<-p[[2]][w,2]
  ex<-p[[3]][w,]
  pat<-p[[4]][w,]  
  rec<-p[[5]][w,]
  cf<-p[[7]][w,]
  cv<-p[[8]][w,]
  fert<-p[[9]][w,]
  
SAU<-ex$ex_sau  
surpat<-ex$ex_shpat
rec_print<-round(surpat-(ex$ex_ugb-ex$ex_eng-ex$ex_ugbes-(pat$pat_PCVTp/100)*ex$ex_vl)*pat$pat_SUGBAp/100-(ex$ex_vl*pat$pat_SVTp*pat$pat_PCVTp)/10000,1)
rec_ete<-round(surpat-(ex$ex_ugb-ex$ex_eng-ex$ex_ugbes-(pat$pat_PCVTe/100)*ex$ex_vl)*pat$pat_SUGBAe/100-(ex$ex_vl*pat$pat_SVTe*pat$pat_PCVTe)/10000,1) 

ha_ens<-round(rec_print*rec$rec_pcens/100,1)
t_ens<-round(ha_ens*rec$rec_rendens,0)
ha_enr<-round(rec_print*rec$rec_pcenr/100,1)
t_enr<-round(ha_enr*rec$rec_rendenr,0)
ha_foinp<-round(rec_print*rec$rec_pcfoinp/100,1)
t_foinp<-round(ha_foinp*rec$rec_rendfoinp,0)
ha_foint<-round(rec_print*(1-(rec$rec_pcens+rec$rec_pcenr+rec$rec_pcfoinp)/100),1)
t_foint<-round(ha_foint*rec$rec_rendfoint,0)
ha_reg<-round(rec_ete-ha_foint,1)
t_reg<-round(ha_reg*rec$rec_rendreg,0)
t_stoh<-t_ens+t_enr+t_foinp+t_foint+t_reg
t_foin<-t_foinp+t_foint+t_reg

cf1<-cf$cf_four1
ha_cf1<-round(cf$cf_surfour1,1)
t_cf1<-round(ha_cf1*cf$cf_rdtfour1,0)

cf2<-cf$cf_four2
ha_cf2<-round(cf$cf_surfour2,1)
t_cf2<-round(ha_cf2*cf$cf_rdtfour2,0)

cf3<-cf$cf_four3
ha_cf3<-round(cf$cf_surfour3,1)
t_cf3<-round(ha_cf3*cf$cf_rdtfour3,0)

cf4<-cf$cf_four4
ha_cf4<-round(cf$cf_surfour4,0)
t_cf4<-round(ha_cf4*cf$cf_rdtfour4,0)

ha_cf<-ha_cf1+ha_cf2+ha_cf3+ha_cf4
t_cf<-t_cf1+t_cf2+t_cf3+t_cf4
t_four<-t_stoh+t_cf

CV<-ex$ex_sau-ex$ex_shpat-ha_cf
cv1<-cv$cv_cv1
ha_cv1<-round(CV*cv$cv_pcv1/100,1)
T_cv1<-round(ha_cv1*cv$cv_rdtcv1/10,0)
Int_cv1<-round(cv$cv_t1ani+cv$cv_t1vl,0)

cv2<-cv$cv_cv2
ha_cv2<-round(CV*cv$cv_pcv2/100,1)
T_cv2<-round(ha_cv2*cv$cv_rdtcv2/10,0)
Int_cv2<-round(cv$cv_t2ani+cv$cv_t2vl,0)

cv3<-cv$cv_cv3
ha_cv3<-round(CV*cv$cv_pcv3/100,1)
T_cv3<-round(ha_cv3*cv$cv_rdtcv3/10,0)
Int_cv3<-round(cv$cv_t3ani+cv$cv_t3vl,0)

cv4<-cv$cv_cv4
ha_cv4<-round(CV*cv$cv_pcv4/100,1)
T_cv4<-round(ha_cv4*cv$cv_rdtcv4/10,0)
Int_cv4<-round(cv$cv_t4ani+cv$cv_t4vl,0)

Intrac<-cv<-Int_cv1+Int_cv2+Int_cv3+Int_cv4
SFP<-SAU-CV

NH<-fert$fert_NH
PH<-fert$fert_PH
KH<-fert$fert_KH
NCF<-fert$fert_NCF
PCF<-fert$fert_PCF
KCF<-fert$fert_KCF
NCV<-fert$fert_NCV
PCV<-fert$fert_PCV
KCV<-fert$fert_KCV

  surf<-cbind(SAU,CV,SFP,surpat,rec_print,rec_ete,ha_ens,t_ens,ha_enr,t_enr,ha_foinp,t_foinp,ha_foint,t_foint,ha_reg,t_reg,
              t_stoh,t_foin,cf1,ha_cf1,t_cf1,cf2,ha_cf2,t_cf2,cf3,ha_cf3,t_cf3,cf4,ha_cf4,t_cf4,ha_cf,t_cf,t_four,
              cv1,ha_cv1,T_cv1,Int_cv1,cv2,ha_cv2,T_cv2,Int_cv2,cv3,ha_cv3,T_cv3,Int_cv3,cv4,ha_cv4,T_cv4,Int_cv4,Intrac,
              NH,PH,KH,NCF,PCF,KCF,NCV,PCV,KCV)

   surf<-as.data.frame(t(as.matrix(surf)))
   colnames(surf)<-proj

  rownames(surf)<-c("SAU","Ha grandes cult.","SFP","STH","Recoltes printemps (ha)","Recoltes ete (ha)","Ha ensilage","TMS ensilage",
                  "Ha enrubanage","TMS enrubanage","Foin 1ereC-1erePer"," Foin 1ereC-1erePer","Foin 1ereC-2emePer","TMS 1ereC-2emePer",
                  "Ha regains","TMS regains","TMS stock d'herbe","Dt TMS foin","Cult. four1","cf1 Surface","cf1 TMS",
                  "Cult. four2","cf2 Surface","cf2 TMS","Cult. four3","cf3 Surface","cf3 TMS",
                  "Cult. four4","cf4 Surface","cf4 TMS","ha cult. four.","TMS cult. four.","TMS fourrages expl.",
                  "Cult 1","Cult 1 ha","Cult1 Tonnes","Cult1 T consommees",
                  "Cult 2","Cult 2 ha","Cult2 Tonnes","Cult2 T consommees","Cult 3","Cult3  ha","Cult 3 Tonnes","Cult3  T consommees",
                  "Cult 4","Cult 4 ha","Cult4 Tonnes","Cult4 T consommees","Grandes cultures consommees (T)",
                  "N/ha d'herbe","P/ha d'herbe","K/ha d'herbe",
                  "N/ha de cult. four.","P/ha de cult. four.","K/ha de cult. four.","N/ha de grande cult.","P/ha de grande cult.",
                  "K/ha de grande cult."
                  )  
  return(surf)
}


calcalim<-function(p,w){
  #p<-readRDS("CapelR/data/capelist")
  proj<-p[[2]][w,2]
  ex<-p[[3]][w,]
  bes<-p[[6]][w,]
  fourh<-p[[10]][w,]
  con<-p[[11]][w,]
    s<-calcusurf(p,w)

  qtivl<-round(4100+(0.33*1.033*ex$ex_rdtvl),0)
  rdtvl<-round(ex$ex_rdtvl,0)
  congl<-bes$bes_conan
  convl<-round(rdtvl*congl/1000,0)
  pconvl<-round(100*convl/qtivl,0)
  TMStoc<-as.vector(s[33,],mode="integer")
  TMStocf<-as.vector(s[32,],mode="integer")
  fourach1<-fourh$fourh_nhfour1
  tfourach1<-fourh$fourh_thfour1
  fourach2<-fourh$fourh_nhfour2
  tfourach2<-fourh$fourh_thfour2
  fourach3<-fourh$fourh_nhfour3
  tfourach3<-fourh$fourh_thfour3
   con1<-con$con_con1
  tcon1<-con$con_tcon1
  con2<-con$con_con2
  tcon2<-con$con_tcon2
  con3<-con$con_con3
  tcon3<-con$con_tcon3
  con4<-con$con_con4
  tcon4<-con$con_tcon4
  con5<-con$con_con5
  tcon5<-con$con_tcon5
  con6<-con$con_con6
  tcon6<-con$con_tcon6
  tfourach<-tfourach1+tfourach2+tfourach3
  tconach<-tcon1+tcon2+tcon3+tcon4+tcon5+tcon6
  tintrac<-as.vector(s[51,],mode="integer")
  besani<- round(qtivl*ex$ex_vl/1000+5.5*(ex$ex_ugb-ex$ex_vl),0)
  valoherbe<-round((besani-TMStocf-tfourach-tconach-tintrac)/ex$ex_shpat,1)
  
  
  alim<-cbind(qtivl,rdtvl,congl,convl,pconvl,TMStoc,TMStocf,fourach1,tfourach1,fourach2,tfourach2,fourach3,tfourach3,
    con1,tcon1,con2,tcon2,con3,tcon3,con4,tcon4,con5,tcon5,con6,tcon6,tfourach,tconach,tintrac
  ,besani,valoherbe)


  alim<-as.data.frame(t(as.matrix(alim)))
  colnames(alim)<-proj
  
  rownames(alim)<- c( "qtivl" ,    "rdtvl"  ,   "congl"  ,   "convl" ,    "pconvl"  ,  "TMStoc" ,   "TMStocf"  ,
   "fourach1" , "tfourach1", "fourach2", "tfourach2", "fourach3" , "tfourach3" ,"con1"  ,    "tcon1"  ,   "con2",    
  "tcon2"  ,   "con3"   ,   "tcon3"  ,   "con4"   ,  "tcon4"   ,  "con5"  ,    "tcon5" ,    "con6"  ,    "tcon6"   , 
  "tfourach",  "tconach",   "tintrac" ,  "besani" ,   "valoherbe")
  
  return(alim)
}


######## fonctions de calcul des résultats économiques


prod<-function(lait,via,cul,aid,otreprod){ 
#   lait<-data.frame(num=c(1,2,3),vola=c(150,200,300),pria=c(350,300,200),volb=c(50,20,0),prib=c(350,300,200),volc=c(0,50,0),pric=c(100,100,100),autoc=c(1,1,1))  
#   via<-data.frame(num=c(1,2,3),vbl=50,ugba=20,kgugba=300,priv=2)
#   cul<-data.frame(num=c(1,2,3),q1ven=c(500,500,500),q1aut=c(50,50,50),pricv1=c(16,16,16),
#   q2ven=c(500,500,500),q2aut=c(50,50,50),pricv2=c(16,16,16), q3ven=c(500,0,0),q3aut=c(50,20,0),
#   pricv3=c(16,16,16),q4ven=c(500,500,500),q4aut=c(50,50,50),pricv4=c(16,16,16))
#   aides<-data.frame(num=c(1,2,3),pil1=c(5000,12000,3000),pil2=c(6000,2000,15000))
  
  prodlait<-lait[,2]*lait[,3]+lait[,4]*lait[,5]+lait[,6]*lait[,7]
  prodviabl <-(lait[,2]+lait[,4]+lait[,6]+lait[,8])*via[,2]
  proviabv<-via[,3]*via[,4]*via[,5]
  procul<-vencul<-cul[,2]*cul[,4]+cul[,5]*cul[,7]+cul[,8]*cul[,10]+cul[,11]*cul[,13]
  autocul<-cul[,3]*cul[,4]+cul[,6]*cul[,7]+cul[,9]*cul[,10]+cul[,12]*cul[,13] 
  vencul<-procul-autocul

  pil1<-aid[,2]
  pil2<-aid[,3]
  otraid<-aid[,4]
  otreprod<-otreprod
  return(list(prodlait,prodviabl,proviabv,vencul,autocul,pil1,pil2,otraid,otreprod))
}


chopani<-function(ugbha,conach,conaut,fourach,chopani){
  #     ugbha<-data.frame(num=c(1,2,3),ugb=c(70,20,58),ha=c(70,100,93))
  #    conach<-data.frame(num=c(1,2,3),tcon1=c(20,30,10),pricon1=c(150,200,300),tcon2=c(10,15,2),pricon2=c(150,200,300),
  #            tcon3=c(20,30,10),pricon3=c(150,200,300),tcon4=c(20,30,10),pricon4=c(150,200,300),tcon5=c(20,30,10),pricon5=c(150,200,300),
  #             tcon6=c(20,30,10),pricon6=c(150,200,300))
  #   conaut<-data.frame(num=c(1,2,3),tanicv1=c(20,30,10),tvlcv1=c(20,30,10),pricv1=c(150,200,300),
  #                 tanicv2=c(20,30,10),tvlcv2=c(20,30,10),pricv2=c(150,200,300),tanicv3=c(20,30,10),
  #                 tvlcv3=c(20,30,10),pricv3=c(150,200,300),tanicv4=c(20,30,10),tvlcv4=c(20,30,10),pricv4=c(150,200,300))
  #    fourach<-data.frame(num=c(1,2,3),tfour1=c(10,3,0),pfour1=c(100,150,50),tfour2=c(10,3,0),pfour2=c(100,150,50),
  #                        tfour3=c(10,3,0),pfour3=c(100,150,50))
  #    chopani<-data.frame(num=c(1,2,3),cmvugb=c(45,35,75),litugb=c(20,5,1),fel=c(100,100,100),veto=c(65,70,85),autani=c(50,20,15))
  
  
  achacon<-conach[,2]*conach[,3]+conach[,4]*conach[,5]+conach[,6]*conach[,7]+conach[,8]*conach[,9]+conach[,10]*conach[,11]+conach[,12]*conach[,13]
  autocon<-(conaut[,2]+conaut[,3])*conaut[,4]+(conaut[,5]+conaut[,6])*conaut[,7]+(conaut[,8]+conaut[,9])*conaut[,10]+(conaut[,11]+conaut[,12])*conaut[,13]
  achafou<-fourach[,2]*fourach[,3]+fourach[,4]*fourach[,5]+fourach[,6]*fourach[,7]
  cmv<-ugbha[,2]*chopani[,2]
  lit<-ugbha[,2]*chopani[,3]
  fel<-ugbha[,2]*chopani[,4]
  veto<-ugbha[,2]*chopani[,5]
  auchani<-ugbha[,2]*chopani[,6]
  
  chop<-list(achacon,autocon,cmv,achafou,lit,fel,veto,auchani)
  return(chop)
  
}

chosurf<-function(surf,fert,prunit,surfcf,refsemcf,rephytocf,refrecf,refdivcf,surfcv,refsemcv,rephytocv,refrecv,refdivcv){
  
  #   surf<-data.frame(num=c(1,2,3),ha=c(70,100,93),herbe=c(50,10,30))
  #       fert<-data.frame(num=c(1,2,3),NH=c(100,100,50),PH=c(10,10,10),KH=c(10,20,50),NCF=c(100,100,100),PCF=c(50,50,50),KCF=c(10,10,10),
  #                         NCV=c(100,100,100),PCV=c(50,20,30),KCV=c(10,50,30))
  #       prunit<-c(N=1000,P=1000,K=700)/1000
  # #     
  #       surfcf<-data.frame(cf1=c(10,20,30),cf2=c(1,3,5),cf3=c(5,6,0),cf4=c(0,0,0),cf5=c(0,0,0))
  #       refsemcf<-data.frame(cf1=c(10,20,30),cf2=c(1,3,5),cf3=c(5,6,0),cf4=c(0,0,0),cf5=c(0,0,0))
  #       rephytocf<-data.frame(cf1=c(10,20,30),cf2=c(1,3,5),cf3=c(5,6,0),cf4=c(0,0,0),cf5=c(0,0,0))
  #       refrecf<-data.frame(cf1=c(10,20,30),cf2=c(1,3,5),cf3=c(5,6,0),cf4=c(0,0,0),cf5=c(0,0,0))
  #       refdivcf<-data.frame(cf1=c(10,20,30),cf2=c(1,3,5),cf3=c(5,6,0),cf4=c(0,0,0),cf5=c(0,0,0))
  #       
  #      surfcv<-data.frame(cv1=c(10,20,30),cv2=c(1,3,5),cv3=c(5,6,0),cv4=c(0,0,0))
  #      refsemcv<-data.frame(cv1=c(10,20,30),cv2=c(1,3,5),cv3=c(5,6,0),cv4=c(0,0,0))
  #      rephytocv<-data.frame(cv1=c(10,20,30),cv2=c(1,3,5),cv3=c(5,6,0),cv4=c(0,0,0))
  #      refrecv<-data.frame(cv1=c(10,20,30),cv2=c(1,3,5),cv3=c(5,6,0),cv4=c(0,0,0))
  #      refdivcv<-data.frame(cv1=c(10,20,30),cv2=c(1,3,5),cv3=c(5,6,0),cv4=c(0,0,0))
  
  fertiherbe<- surf[,3]*(fert[,2]*prunit[1]+fert[,3]*prunit[2]+fert[,4]*prunit[3]) 
  ferticf<-rowSums(surfcf)*(fert[,5]*prunit[1]+fert[,6]*prunit[2]+fert[,7]*prunit[3])
  ferticv<-(surf[,2]-rowSums(surfcf)-surf[,3])*(fert[,8]*prunit[1]+fert[,9]*prunit[2]+fert[,10]*prunit[3])
  
  semcf<-rowSums(surfcf*refsemcf)
  phytocf<-rowSums(surfcf*rephytocf)
  recf<-rowSums(surfcf*refrecf)
  divcf<-rowSums(surfcf*refdivcf)
  
  semcv<- rowSums(surfcv*refsemcv)
  phytocv<-rowSums(surfcv*rephytocv)
  recv<-rowSums(surfcv*refrecv)
  divcv<-rowSums(surfcv*refdivcv)
  ############ pour l'herbe reprendre dans référentiel ha PP et ha PT et charges sem,phyto puis recolte ha ens enr foin...t 1/3 ? demander 
  
  #ajouter les charges surfaces
  chosurf<-list(fertiherbe,ferticf,ferticv,semcf,semcv,phytocf,phytocv,divcf,divcv,recf,recv)
  return(chosurf)
  
}

##############################################

calprod<-function(p,w){
  #p<-readRDS("G:/Documents/AR/CapelR/data/capelist")
  proj<-p[[2]][w,2]
  ex<-p[[3]][w,]
  cv<-p[[8]][w,]
  pro<-p[[12]][w,]
  s<-calcusurf(p,w)
 limA<-pro$pro_limA
 limB<-pro$pro_limB
    volait<-ex$ex_vl*ex$ex_rdtvl/1000-pro$pro_autoc
 
  volA<-0
   volB<-0
   volC<-0
 
    volA<-ifelse(volait<=limA,volait,limA)

    volB<-ifelse(limB<limA,0,ifelse(volait>limA & volait<=limB,volait-limA,ifelse(volait>limB,limB,0)))
    volC<-ifelse(limB<limA,0,ifelse(volait>limB,volait-limB,0))

    lait<-data.frame(num=pro$num,vola=volA,pria=pro$pro_priA,volb=volB,prib=pro$pro_priB,volc=volC,pric=pro$pro_priC)
     
   # via<-data.frame(num=pro$num,viabl=pro$pro_viabl,ugba=ex$ex_ugb-ex$ex_ugbl,kgvv=pro$pro_kgvv,priv=pro$pro_priv)
    
  cul<-data.frame(num=pro$num,q1pro=as.vector(s[36,],mode="integer"),q1aut=as.vector(s[37,],mode="integer"),pricv1=cv$cv_pri1,
                q2pro=as.vector(s[40,],mode="integer"),q2aut=as.vector(s[41,],mode="integer"),pricv2=cv$cv_pri2,   
                q3pro=as.vector(s[44,],mode="integer"),q3aut=as.vector(s[45,],mode="integer"),pricv3=cv$cv_pri3,
                q4pro=as.vector(s[48,],mode="integer"),q4aut=as.vector(s[49,],mode="integer"),pricv4=cv$cv_pri4)  
   
  prodlait<-c(lait[,2]*lait[,3]+lait[,4]*lait[,5]+lait[,6]*lait[,7])
  prodviabl <-(volait+pro$pro_autoc)*pro$pro_viabl
  prodviabv<-(ex$ex_ugb-ex$ex_ugbl)*pro$pro_kgvv*pro$pro_priv
  procul<-vencul<-cul[,2]*cul[,4]+cul[,5]*cul[,7]+cul[,8]*cul[,10]+cul[,11]*cul[,13]
  autocul<-cul[,3]*cul[,4]+cul[,6]*cul[,7]+cul[,9]*cul[,10]+cul[,12]*cul[,13] 
  vencul<-procul-autocul
#   if(pro$pro_primvl[1]=="Montagne")
#   {p1vl<-ifelse(ex$ex_vl<pro$pro_pardpb*40,60*pro$pro_pardpb*ex$ex_vl,pro$pro_pardpb*40*60)
#    }else{
#     p1vl<-ifelse(ex$ex_vl<pro$pro_pardpb*40,30*pro$pro_pardpb*ex$ex_vl,pro$pro_pardpb*40*30)} ## pour la plaine
  p1vl<-0
  ###### on ne tient pas compte des parts PAC pour le pil1
  pil1<-ex$ex_sau*pro$pro_dpb+p1vl
  pil2<-ifelse(ex$ex_sau<pro$pro_parichn*25,ex$ex_sau*pro$pro_ichn1,ifelse(ex$ex_sau<50*pro$pro_parichn,
                                25*pro$pro_parichn*pro$pro_ichn1+(ex$ex_sau-25*pro$pro_parichn)*pro$pro_ichn2,
                                25*pro$pro_parichn*pro$pro_ichn1+25*pro$pro_parichn*pro$pro_ichn2))
  
  otraid<-pro$pro_otraid
  otprod<-pro$pro_otprod

  protot<-prodlait+prodviabl+prodviabv+vencul+autocul+pil1+pil2+otraid+otprod
  viatot<-prodviabl+prodviabv
  cultot<-vencul+autocul
  totaid<-pil1+pil2+otraid
  ecopro<-as.data.frame(cbind(protot,prodlait,viatot,prodviabl,
                              prodviabv,cultot,vencul,autocul,totaid,pil1,pil2,otraid,otprod))
  
  num2int <- function(f) as.integer(f)
  cols <- c(1:length(ecopro))
  ecopro[cols] <- lapply(ecopro[cols], num2int)
  
  ecopro<-as.data.frame(t(as.matrix(ecopro))) 
  colnames(ecopro)<-ecopro[1,]
  colnames(ecopro)<-proj
  
  rownames(ecopro)<- c( "Produit total","Produit lait","Viande","Produit viande BL","Produit viande BV",
                    "Produit cultures", "Ventes cultures", "Autoconso cultures",
                    "Aides","1er Pilier","2me pilier","Autres aides","Autres produits")
  
  return(ecopro)
}


calchop<-function(p,w){
  # p<-readRDS("G:/Documents/AR/caperso/callumv.rds")[[1]]
  proj<-p[[2]][w,2]
  ex<-p[[3]][w,]
  v<-p[[8]][w,]
  c<-p[[11]][w,]
  f<-p[[10]][w,]
  ch<-p[[13]][w,]
  fert<-p[[9]][w,]
  VL<-ex$ex_vl
  UGB<-ex$ex_ugb
  SAU<-ex$ex_sau
  
  achacon<-c$con_tcon1*c$con_pcon1+c$con_tcon2*c$con_pcon2+c$con_tcon3*c$con_pcon3+c$con_tcon4*c$con_pcon4+c$con_tcon5*c$con_pcon5+c$con_tcon6*c$con_pcon6
  autocon<-(v$cv_t1ani+v$cv_t1vl)*v$cv_pri1+(v$cv_t2ani+v$cv_t2vl)*v$cv_pri2+(v$cv_t3ani+v$cv_t3vl)*v$cv_pri3+(v$cv_t4ani+v$cv_t4vl)*v$cv_pri4
  achafou<-f$fourh_thfour1*f$fourh_phfour1+f$fourh_thfour2*f$fourh_phfour2+f$fourh_thfour3*f$fourh_phfour3
  cmv<-ex$ex_ugb*ch$chop_cmv+(c$con_minvl*VL+c$con_minugb*(UGB-VL))*c$con_pricmv
  lit<-ex$ex_ugb*ch$chop_lit
  fel<-ex$ex_ugb*ch$chop_fel
  veto<-ex$ex_ugb*ch$chop_veto
  divani<-ex$ex_ugb*ch$chop_divel
  ochani<-ch$chop_ochani
  chani<-achacon+autocon+achafou+cmv+lit+fel+veto+divani+ochani 
  
  fertih<-fert$fert_HaH*(fert$fert_NH*ch$chop_priN+fert$fert_PH*ch$chop_priP+fert$fert_KH*ch$chop_priK)/1000
  ferticf<-fert$fert_HaCf*(fert$fert_NCF*ch$chop_priN+fert$fert_PCF*ch$chop_priP+fert$fert_KCF*ch$chop_priK)/1000
  ferticv<-fert$fert_HaCv*(fert$fert_NCV*ch$chop_priN+fert$fert_PCV*ch$chop_priP+fert$fert_KCV*ch$chop_priK)/1000
  fertil<-fertih+ferticf+ferticv
  
  sem<-ch$chop_semha*SAU
  phyt<-ch$chop_phytha*SAU
  divsurf<-ch$chop_divha*SAU
  otchas<-ch$chop_otchas
  
  chosurf<-fertil+sem+phyt+divsurf+otchas
  chotot<-chani+chosurf
  

  
  ecochop<-as.data.frame(cbind(chotot,chani,achacon,autocon,achafou,cmv,lit,fel,veto,divani,ochani,
                               chosurf,fertil,sem,phyt,divsurf,otchas))
  
  num2int <- function(f) as.integer(f)
  cols <- c(1:length(ecochop))
  ecochop[cols] <- lapply(ecochop[cols], num2int)
  
  
 
  ecochop<-as.data.frame(t(as.matrix(ecochop))) 
  #colnames(ecochop)<-ecochop[1,]
  colnames(ecochop)<-proj
  
  rownames(ecochop)<- c( "Charges operationnelles","Charges animales","Achat de concentre", "Intra-consommation concentre", "Achat de fourrages",
                        "CMV",     "litiere",     "Frais d'elevage",     "Frais veto",   "Divers charges animales" , "Autres charges animales" ,
                        "Charges surface","Fertilisation","Semences","Phytos","Divers surfaces","Autres charges surfaces")
  
  return(ecochop)
}





calfix<-function(p,w){
  #p<-readRDS("CapelR/data/capelist")
  proj<-p[[2]][w,2]
  ex<-p[[3]][w,]
  fert<-p[[9]][w,]
  fix<-p[[14]][w,]
  UGB<-ex$ex_ugb
  SAU<-ex$ex_sau
  
  rec<-fert$fert_HaH*fix$fix_rechah+fert$fert_HaCf*fix$fix_rechacf+fert$fert_HaCv*fix$fix_rechacv
  sal<-fix$fix_sal
  ferm<-fix$fix_ferm
  carb<-fix$fix_carb*SAU
  entremat<-fix$fix_entremat*SAU
  entrebat<-fix$fix_entrebat*UGB
  tdep<-fix$fix_tdep*SAU
  ass<-fix$fix_assu
  impt<-fix$fix_impt
  eau<-fix$fix_eau*UGB
  edfgdf<-fix$fix_edfgdf*UGB
  fraig<-fix$fix_fraig
  divfix<-fix$fix_div*UGB
  fixot<-fix$fix_otrefix
  
  fixtot<-rec+sal+ferm+carb+entremat+entrebat+tdep+ass+impt+eau+edfgdf+fraig+divfix+fixot
    
  ecofix<-as.data.frame(cbind(fixtot,rec,sal,ferm,carb,entremat,entrebat,tdep,ass,impt,eau,edfgdf,fraig,divfix,fixot))
  
  num2int <- function(f) as.integer(f)
  cols <- c(1:length(ecofix))
  ecofix[cols] <- lapply(ecofix[cols], num2int)
  
  
  
  ecofix<-as.data.frame(t(as.matrix(ecofix))) 
  colnames(ecofix)<-ecofix[1,]
  colnames(ecofix)<-proj
  
  rownames(ecofix)<-c("Charges de structure","Recoltes (travaux 1/3).","Salaires","Fermages","Carburant-lubrifiants","Entretien du materiel",
                      "Entretien des batiment","Transport et deplacements","Assurances","Impots et taxes","Eau",
                      "Electricite","Frais de gestion","Divers","Autres charges fixes")
  
  
  return(ecofix)
}


calcures<-function(p,w){
 # p<-readRDS("G:/Documents/AR/caperso/callumv2.rds")[[1]]
  proj<-p[[2]][w,2]
  ex<-p[[3]][w,]
  fert<-p[[9]][w,]
  fix<-p[[14]][w,]
  UGB<-ex$ex_ugb
  SAU<-ex$ex_sau
  rev<-p[[15]][w,]
  
 prod<-as.vector(calprod(p,w)[1,],mode="integer")
 chop<-as.vector(calchop(p,w)[1,],mode="integer")
 fix<-as.vector(calfix(p,w)[1,],mode="integer")
 msa<-rev$rev_msa
 EBE<-prod-chop-fix-msa
 EBE_UMOex<-EBE/ex$ex_umoex
 EBE_PB<-EBE/prod
 oldanu<-rev$rev_oldannu
 newanu<-rev$rev_newanu
 annu<-oldanu+newanu
 dispo<-EBE-annu
 dispoUMOE<-(EBE-annu)/ex$ex_umoe
 
 revenu<-as.data.frame(cbind(msa,EBE,EBE_UMOex,EBE_PB,oldanu,newanu,annu,dispo,dispoUMOE))
 
 num2int <- function(f) as.integer(f)
 cols <- c(1:length(revenu))
 revenu[cols] <- lapply(revenu[cols], num2int)
  
 revenu<-as.data.frame(t(as.matrix(revenu))) 
 colnames(revenu)<-revenu[1,]
 colnames(revenu)<-proj
 
 rownames(revenu)<-c("MSA","EBE","EBE par UMO expl.","EBE/PB","Annuites anciennes","Annuites nouvelles","Annuites","disponible","Disponible par UMO expl.")
 
 return(revenu)
}

#################### graphiques travail

totrav<-function(vl,ugb,veaux,hapat,hacul,sau,traite,tralim,curapa,travo,trapat,tstroup,tsfou,tscul,tradiv,queltradiv)
{
  #   vl=50;ugb=100;veaux=45;hapat=50;hacul=70;sau=150;traite=15;tralim=10;curapa=2;travo=15;trapat=1;tstroup=0.1;tsfou=0.5;tscul=0.5
  hour<-8
  par(bg = "transparent", mar =c(5, 2 ,2, 0))
  traitan<-vl*traite
  aliman<-ugb*tralim
  curan<-ugb*curapa
  van<-veaux*travo
  patan<-hapat*trapat
  troupan<-ugb*tstroup*hour
  fouran<-ugb*tsfou*hour
  culan<-hacul*tscul*hour
  
  travan <- matrix(c(traitan,aliman,curan,van,patan,troupan,fouran,culan,tradiv))
  totrav<-sum(travan[,1])
  coul1<-brewer.pal(9,"Pastel1")
  #coul<-c("cyan","tan","plum","snow","green","salmon","tan1","yellow")
  bar1<- barplot(travan, col = coul1, main="",
                 names.arg=paste("Travail annuel",round(totrav,0),"H/an"),cex.names =1,
                 cex.axis = 1, ylab = "h/an", border = "white")
  
  
  labtradiv<-ifelse(tradiv==0,"",ifelse(queltradiv !="",queltradiv,"Divers"))
  lib<-c("Traite","Alimentation","Curage, paillage","Veaux","Paturage",
         "TS troupeau","TS fourrages","TS gr. cultures",labtradiv)
  
  lab1<-c("")
  for(i in 1:5)
  {
    if(travan[i]>0) {lab1[i]<-paste(lib[i],round(travan[i]/313,1),"h/jour")
    }else{
      lab1[i]<-""
    }  
  }
for(i in 6:length(travan))
{
  if(travan[i]>0) {lab1[i]<-paste(lib[i],round(travan[i]/hour,0),"Jours")
  }else{
    lab1[i]<-""
  }  
}
  
  text(x=bar1,y=cumsum(travan[,1])-travan[,1]/2,label = lab1,cex=1,font=2)
  
  
}

## graph TA
reparta<-function(vl,ugb,veaux,hapat,hacul,sau,traite,tralim,curapa,travo,
                  trapat,tstroup,tsfou,tscul,entrep,salar,benev,umoex)
  {
#   
#   vl=50;ugb=100;veaux=45;hapat=50;hacul=70;sau=150;traite=15;tralim=10;curapa=2;travo=15;trapat=1;tstroup=0.1;tsfou=0.5;tscul=0.5
#   entrep=500;salar=1800;benev=200;umoex=1
  hour<-8
  par(bg = "transparent", mar = c(5, 0, 2, 0))
  traitan<-vl*traite
  aliman<-ugb*tralim
  curan<-ugb*curapa
  van<-veaux*travo
  patan<-hapat*trapat
  troupan<-ugb*tstroup*hour
  fouran<-ugb*tsfou*hour
  culan<-hacul*tscul*hour
  
  travan <- matrix(c(traitan,aliman,curan,van,patan))
  totrav<-sum(travan[,1])
  coul1<-brewer.pal(9,"Pastel1")
  #coul<-c("cyan","tan","plum","snow","green","salmon","tan1","yellow")
  bar1<- barplot(travan, col = coul1, main="",xlim = c(1, 25), width =5,axes=FALSE,
                names.arg=paste("Travail",round(totrav,0),"H/an"),cex.names =1,
                 cex.axis = 1, ylab = "h/an", border = "white")
    
  
  
  lib<-c("Traite","Alimentation","Curage, paillage","Veaux","Paturage")
  
  lab1<-c("")
  for(i in 1:length(travan))
  {
    if(travan[i]>0) {lab1[i]<-paste(lib[i],round(travan[i],0),"Heures")
    }else{
      lab1[i]<-""
    }  
  }   
  text(x=bar1,y=cumsum(travan[,1])-travan[,1]/2,label = lab1,cex=1)

 travex<- sum(travan[,1])-entrep-salar-benev
  rep<-matrix(rev(c(entrep,salar,benev,travex)))
  coul2<-brewer.pal(4,"Accent")
  
  bar2 <- barplot(rep, col = coul2, add = T, width = 5,space = 1.5, names.arg="Repartition",
                  axes = F, border = "white")
 lib2<-rev(c("Entreprise","Salaries","Benevoles","Exploitant(e)s")) 
 
  lab2<-c("")
  for(i in 1:length(rep))
  {
    if(rep[i]>0) {lab2[i]<-paste(lib2[i],round(rep[i],0),"H/an")
    }else{
      lab2[i]<-""
    }  
  }   
  text(x=bar2,y=cumsum(rep[,1])-rep[,1]/2,label = lab2,cex=1)

 abline(h=seq(0,round(totrav/10,0)*10,500), col="lightgray", lty="dotted")
 
 TA_UGB<-round((traitan+aliman+curan+van+patan)/ugb,0)
 TS_SAU<-round((troupan+fouran+culan)/sau/hour,2)
 
 deb<-20
 text(deb,9*totrav/10,label=paste("TA/UGB",TA_UGB,"heures"),cex=1.5,col="blue")
 #text(deb,8*totrav/10,label=paste("TS/ha",TS_SAU,"jour(s)"),cex=1.5,col="blue")
 text(deb,3*totrav/10,label=paste("Travail exploitant",round(travex/umoex,0),"heures/an/UMO expl"),cex=1.5,col="blue")
 text(deb,2*totrav/10,label=paste("Travail exploitant",round(travex/365/umoex,1),"heures/jour/UMO expl"),cex=1.5,col="blue")
}



### repar TS
reparts<-function(vl,ugb,veaux,hapat,hacul,sau,traite,tralim,curapa,travo,
                  trapat,tstroup,tsfou,tscul,entrep,salar,benev,umoex)
{
  #   
  #   vl=50;ugb=100;veaux=45;hapat=50;hacul=70;sau=150;traite=15;tralim=10;curapa=2;travo=15;trapat=1;tstroup=0.1;tsfou=0.5;tscul=0.5
  #   entrep=500;salar=1800;benev=200;umoex=1
  hour<-8
  par(bg = "transparent", mar = c(5, 0, 2, 0))
  traitan<-vl*traite
  aliman<-ugb*tralim
  curan<-ugb*curapa
  van<-veaux*travo
  patan<-hapat*trapat
  troupan<-ugb*tstroup*hour
  fouran<-ugb*tsfou*hour
  culan<-hacul*tscul*hour
  
  travan <- matrix(c(troupan,fouran,culan))
  totrav<-sum(travan[,1])
  coul1<-brewer.pal(9,"Pastel1")
  #coul<-c("cyan","tan","plum","snow","green","salmon","tan1","yellow")
  bar1<- barplot(travan, col = coul1, main="",xlim = c(1, 25), width =5,axes=FALSE,
                 names.arg=paste("Travail",round(totrav,0),"H/an"),cex.names =1,
                 cex.axis = 1, ylab = "h/an", border = "white")
  
  
  
  lib<-c( "TS troupeau","TS fourrages","TS Gr. cult.")
  
  lab1<-c("")
  for(i in 1:length(travan))
  {
    if(travan[i]>0) {lab1[i]<-paste(lib[i],round(travan[i],0),"Heures")
    }else{
      lab1[i]<-""
    }  
  }   
  text(x=bar1,y=cumsum(travan[,1])-travan[,1]/2,label = lab1,cex=1)
  
  travex<- sum(travan[,1])-entrep-salar-benev
  rep<-matrix(rev(c(entrep,salar,benev,travex)))
  coul2<-brewer.pal(4,"Accent")
  
  bar2 <- barplot(rep, col = coul2, add = T, width = 5,space = 1.5, names.arg="Repartition",
                  axes = F, border = "white")
  lib2<-rev(c("Entreprise","Salaries","Benevoles","Exploitant(e)s")) 
  
  lab2<-c("")
  for(i in 1:length(rep))
  {
    if(rep[i]>0) {lab2[i]<-paste(lib2[i],round(rep[i],0),"H/an")
    }else{
      lab2[i]<-""
    }  
  }   
  text(x=bar2,y=cumsum(rep[,1])-rep[,1]/2,label = lab2,cex=1)
  
  abline(h=seq(0,round(totrav/10,0)*10,500), col="lightgray", lty="dotted")
  
  TA_UGB<-round((traitan+aliman+curan+van+patan)/ugb,0)
  TS_SAU<-round((troupan+fouran+culan)/sau/hour,2)
  deb<-20
  text(deb,8*totrav/10,label=paste("TS/ha",TS_SAU,"jour(s)"),cex=1.5,col="blue")
  text(deb,3*totrav/10,label=paste("Travail de saison exploitant",round(travex/umoex,0),"heures/UMO expl"),cex=1.5,col="blue")
  text(deb,2*totrav/10,label=paste("Travail de saison exploitant",round(travex/umoex/8,1),"jours/UMO expl"),cex=1.5,col="blue")
}


repartot<-function(vl,ugb,veaux,hapat,hacul,sau,traite,tralim,curapa,travo,trapat,tstroup,tsfou,tscul,entrep,salar,benev,umoex,tradiv,queltradiv)
{
  #   
  #   vl=50;ugb=100;veaux=45;hapat=50;hacul=70;sau=150;traite=15;tralim=10;curapa=2;travo=15;trapat=1;tstroup=0.1;tsfou=0.5;tscul=0.5
  #   entrep=500;salar=1800;benev=200;umoex=1
  hour<-8
  par(bg = "transparent", mar = c(5, 0, 2, 0))
  traitan<-vl*traite
  aliman<-ugb*tralim
  curan<-ugb*curapa
  van<-veaux*travo
  patan<-hapat*trapat
  troupan<-ugb*tstroup*hour
  fouran<-ugb*tsfou*hour
  culan<-hacul*tscul*hour
  
  travan <- matrix(c(traitan,aliman,curan,van,patan,troupan,fouran,culan,tradiv))
  totrav<-sum(travan[,1])
  coul1<-brewer.pal(9,"Pastel1")
  #coul<-c("cyan","tan","plum","snow","green","salmon","tan1","yellow")
  bar1<- barplot(travan, col = coul1, main="",xlim = c(1, 25), width =5,axes=FALSE,
                 names.arg=paste("Travail",round(totrav,0),"H/an"),cex.names =1,
                 cex.axis = 1, ylab = "h/an", border = "white")
  
  labtradiv<-ifelse(tradiv==0,"",ifelse(queltradiv !="",queltradiv,"Divers"))
  
  lib<-c("Traite","Alimentation","Curage, paillage","Veaux","Paturage",
         "TS troupeau","TS fourrages","TS Gr. cult.",labtradiv)
  
  lab1<-c("")
  for(i in 1:length(travan))
  {
    if(travan[i]>0) {lab1[i]<-paste(lib[i],round(travan[i],0),"Heures")
    }else{
      lab1[i]<-""
    }  
  }   
  text(x=bar1,y=cumsum(travan[,1])-travan[,1]/2,label = lab1,cex=1)
  
  travex<- sum(travan[,1])-entrep-salar-benev
  rep<-matrix(rev(c(entrep,salar,benev,travex)))
  coul2<-brewer.pal(4,"Accent")
  
  bar2 <- barplot(rep, col = coul2, add = T, width = 5,space = 1.5, names.arg="Repartition",
                  axes = F, border = "white")
  lib2<-rev(c("Entreprise","Salaries","Benevoles","Exploitant(e)s")) 
  
  lab2<-c("")
  for(i in 1:length(rep))
  {
    if(rep[i]>0) {lab2[i]<-paste(lib2[i],round(rep[i],0),"H/an")
    }else{
      lab2[i]<-""
    }  
  }   
  text(x=bar2,y=cumsum(rep[,1])-rep[,1]/2,label = lab2,cex=1)
  
  abline(h=seq(0,round(totrav/10,0)*10,500), col="lightgray", lty="dotted")
  
  TA_UGB<-round((traitan+aliman+curan+van+patan)/ugb,0)
  TS_SAU<-round((troupan+fouran+culan)/sau/hour,2)
  TDC<-(365-52)*8-travex/umoex
    
  deb<-20
  text(deb,9*totrav/10,label=paste("TA/UGB",TA_UGB,"heures"),cex=1.5,col="blue")
  text(deb,8*totrav/10,label=paste("TS/ha",TS_SAU,"jour(s)"),cex=1.5,col="blue")
  text(deb,3*totrav/10,label=paste("Travail exploitant",round(travex/umoex,0),"heures/UMO expl"),cex=1.5,col="blue")
  text(deb,2*totrav/10,label=paste("Temps Disponible Calcule =",round(TDC,0),"heures/UMO expl"),cex=1.5,col="blue")
}



######### matrice de risques volatilité prix1/prix2
# 
# varisk <-function(EBE,prod1,num1,vol1,pri1,vap1,np1,prod2,num2,vol2,pri2,vap2,np2){
#  # RES<-50000 ;prod1="Prix du lait";num1=1;vol1<-500 ;pri1=350;vap1<-10 ;np1<-3 ;prod2="Prix de l'aliment";num2=3;vol2<- -100 ;pri2=350;vap2<-50;np2<-2
#   
#   ncol<-2*np1+1
#   nlin<-2*np2+1
#   mat<-data.frame(t(matrix(0,ncol,nlin)))
#   
#   nom1<-""
#   j<-1
#   secol<-seq(from=-vap1*np1,to=vap1*np1,by=vap1)
#   for(i in secol )
#   {
#     nom1[j]<-ifelse(i==0,paste(prod1,"moyen du projet",pri1+i),paste(prod1,pri1+i))
#     j<-j+1 }
#   colnames(mat)<-nom1
#   
#   nom2<-""
#   j<-1
#   seclin<-seq(from=-vap2*np2,to=vap2*np2,by=vap2)
#   for(i in seclin)
#   {
#     nom2[j]<-ifelse(i==0,paste(prod2,"moyen du projet",pri2+i),paste(prod2,pri2+i))
#     j<-j+1 }
#   rownames(mat)<-nom2
# 
#   
#   for(i in 1:ncol)
#   {
#     for(j in 1:nlin)
#     {
#       x<-ifelse(num1 <3,1,-1)
#       y<-ifelse(num2 <3 ,-1,1)
#       mat[j,i]<-round(EBE + (secol[i]*vol1*x-seclin[j]*vol2*y)*0.75,0)
#     }
#   }
#   
# 
# return(mat)
# 
# }

######################matrice prix/volume

variski <-function(RES,pri,varpri,npri,vol,varvol,nvol){
  #RES<-0 ;pri=350;varpri=20;npri=2;vol=700;varvol=0.01;nvol=4 
  
  ncol<-2*npri+1
  nlin<-2*nvol+1
  mat<-data.frame(t(matrix(0,ncol,nlin)))
  
  nom1<-""
  j<-1
  secol<-seq(from=-varpri*npri,to=varpri*npri,by=varpri)
  
  for(i in secol )
  {
    nom1[j]<-ifelse(i==0,paste("Prix moyen du projet",pri+i),paste("Prix ",pri+i))
    j<-j+1 }
  colnames(mat)<-nom1

  nom2<-""
  j<-1
  
  seclin<-seq(from=-varvol*nvol,to=varvol*nvol,by=varvol)
  
  for(i in seclin)
  {
    nom2[j]<-ifelse(i==0,paste("Volume du projet",round(vol*(1+i))),paste("Volume",round(vol*(1+i),0)))
    j<-j+1 }
  
  rownames(mat)<-nom2
  
  
  for(i in 1:ncol)
  {
    for(j in 1:nlin)
    {   
      mat[j,i]<-round(RES + (secol[i]*vol)+seclin[j]*vol*(pri+secol[i]),0)  
    }
  }
  
  
  return(mat)
  
}





