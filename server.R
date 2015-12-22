
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

#### CapelR version 7 du 14/12/2015
#  rm(list=ls())
library(shiny)
options(shiny.port = 8000)
library(rmarkdown)
library(xtable)


#### CAPELR version 0.0.2 du 25/06/2015

#   setwd("G:/caperso")
#   setwd("G:/Documents/AR/CapelR") 


#      setwd("~/Documents/AR/CapelR/")  
#     shiny::runApp("G:/Documents/AR/CapelR",port=8000)
#     shiny::runApp("~/Documents/AR/CapelR")
#   p<-readRDS("data/capelist")
#  p<- readRDS("~/caperso/capel76M.rds")[[1]]
#   saveRDS(p,"6M_2013.rds")
#  q<-readRDS("~/Documents/AR/CapelR/data/capelinit")
# q<-readRDS("~/Documents/AR/caperso/test_2015-12-17.rds")
#  saveRDS(q,"~/Documents/AR/CapelR/data/capelinit" )
# p<-list(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],p[[8]],p[[9]],p[[10]],p[[11]],p[[12]],p[[13]],p[[14]],p[[15]])
#  rm(list=ls())
  coninit<-as.data.frame(matrix("",15,3))
  colnames(coninit)<-c("Synthèse",".............Forces.............",".............Faiblesses.............")
  coninit[,1]<-c("Potentiel humain","Environnement","Milieu","Fertilisation","Vaches laitières",rep(" ",10))
saveRDS(list(coninit),"data/conatou") 

source("helpR/caplot.R")
warn<-1
options(stringsAsFactors=FALSE)
p<-readRDS("data/capelinit")
                                 
capelist<-p[[1]]
saveRDS(capelist,"data/capelist")

#refemb<-p[[2]]
if(!is.null(p[[3]]))
{
  rotations<-p[[3]]  
  saveRDS(rotations,"data/rotations") 
}

zoncf<-2
zoncv<-4

vers<-"7"

ref<-readRDS("ref/refinosys")

######### supprimer dans tmp les fichiers de plus de 100 jours
diractu<-paste0(dirname(getwd()),"/CapelR/tmp")
 finf <- file.info( list.files(diractu,full.names=TRUE))
filinf<-finf[difftime(Sys.time(), finf[,"mtime"], units = "days") > 100 , 1:4]
if(nrow((filinf)))
{
    filist<-rownames(filinf)
    print(paste(filist,"a été supprimé du dossier /temp","\n"))
    unlink(filist)  
} 
###########
atoucon<-as.data.frame(matrix("",5,3))
colnames(atoucon)<-c("Rubriques","Atouts","Contraintes")
atoucon[,1]<-c("Potentiel humain","Environnement","Milieu","Fertilisation","Vaches laitières")
#############
shinyServer(function(input, output, clientData, session) {
  
  
output$version<-renderText({
  version<-paste("Réalisé par Jean-Luc REUILLON Institut de l'élevage, avec l'aide des chambres d'agriculture du Centre et de l'Allier,
                 et la contribution financière du CASDAR MAAF. Logiciel sous licence GPLV3. Version :",vers)
  
})
  
  #############
  #updateRadioButtons(session,"refinoperso",selected=capelist[[1]]$refino)
  ######## loading new file
  newfarm<-reactive ({ 
    input$donold
    newfarm<-readRDS("data/capelist")
   
    if(!is.null(input$donold))
    {
      inFile <- input$donold
      if (is.null(inFile))  
        return(NULL)   
      capefarm<-readRDS(inFile$datapath)
      newfarm<-capefarm[[1]]

      if(length(capefarm)>3){
        saveRDS(capefarm[[3]],"data/rotations")                          
      }
      if(length(capefarm)>4){
        saveRDS(capefarm[[4]],"data/conatou")                           
      }
      
      ############ maj pour les nouvelles versions
      if (length(capefarm$version)==0)
       {
        newfarm[[4]]$pat_PCVTp<- newfarm[[4]]$pat_PCVTp*100
        newfarm[[4]]$pat_PCVTe<- newfarm[[4]]$pat_PCVTe*100
        newfarm[[4]]$pat_PCVTo<- newfarm[[4]]$pat_PCVTo*100
        newfarm[[5]]$rec_pcens<- newfarm[[5]]$rec_pcens*100
        newfarm[[5]]$rec_pcenr<- newfarm[[5]]$rec_pcenr*100
        newfarm[[5]]$rec_pcfoinp<- newfarm[[5]]$rec_pcfoinp*100
        
       }else{
         v<-capefarm$version
         if(nchar(v)>1){v<-substr(v,nchar(v),nchar(v))}
         if(as.integer(v)<2 ){newfarm[[15]]$rev_cominv<-"Investissements"  
                              }
         if(as.integer(v) < 4 ){newfarm[[16]]$tra_tradiv<-0
                              newfarm[[16]]$tra_queltradiv<-""
                              }
         if(as.integer(v) < 5 ){newfarm[[3]]$ex_PT<-0
                                newfarm[[3]]$ex_lat<-0
                                newfarm[[3]]$ex_tougbl<-1.4
                                newfarm[[3]]$ex_tougb<-1
                              }
         if(as.integer(v) < 6 ){newfarm[[5]]$rec_typerecot <-"Préciser."
                                newfarm[[5]]$rec_msrecot <-0
                           }
         
       }
      if(as.integer(v) < 7 ){newfarm[[2]]$descriproj <-""
                             newfarm[[3]]$ex_shrec <-0
                             newfarm[[4]]$pat_compat <-""
                             newfarm[[5]]$rec_comrech <-""
                             newfarm[[6]]$bes_combesani <-""
                             newfarm[[7]]$cf_comcf <-""
                             newfarm[[8]]$cv_comcv<-""
                            newfarm[[9]]$fert_comferti <-"" 
                            newfarm[[11]]$con_pricmv <-0
                            newfarm[[15]]$rev_comatrix <-""
         }         
       }
      
    return(newfarm)
   })

 
  refemb<-reactive ({ 
    input$donold
    ref<-p[[2]]
    if(!is.null(input$donold))
    {
      inFile <- input$donold
      if (is.null(inFile))  
        {return(NULL)
         }else{
        capefarm<-readRDS(inFile$datapath)
        ref<-capefarm[[2]]
        fichepat<-ref[[1]]$Code
     updateSelectInput(session,"refou",choices=fichepat)
      
    }}
    return(ref)
  })
  
version_du_fichier<-reactive ({ 
  input$donold
  if(!is.null(input$donold))
  {
    inFile <- input$donold
    if (is.null(inFile))  
    {return(NULL)
    }else{
      capefarm<-readRDS(inFile$datapath)
      version<-capefarm$version 
    }
  }else{
    version<-p$version
  }
  return(version)
})

  
  
  
observe({
  newfarm()
  if(!is.null(newfarm()))
  saveRDS(newfarm(),"data/capelist")
  farm<-newfarm()[[2]]
  updateSelectizeInput(session,"selproj",choices=farm$projid)
  #updateRadioButtons(session,"refinoperso",selected=newfarm()[[1]]$refino)  
})
###################################
capelist <-  reactiveFileReader(100, session, "data/capelist",readRDS)
rot <-  reactiveFileReader(100, session, "data/rotations",readRDS)
conatou <-reactiveFileReader(100, session, "data/conatou",readRDS)
##########################################################################
output$projex<-output$proref<-output$propat<-output$projsh<-output$projet<-output$probes<-output$projachafou<-output$projcon<-output$projeco<-output$projchop<-output$projrev<-output$projconatou<-renderText({
  text<-input$desproj
})

output$projrap<-renderText({
text<-paste("Edition du projet",input$desproj)
})


## Add an alert to the previously created alert anchor:

observe({
  
  input$datereal
  class<-"danger"
  invalidateLater(5000, session)
  
   w<-runif(1)
  warn<-as.integer(w*10)
  
  if(warn%%2==0)
  {
    text<-"Penser à appliquer les changements." 
    showshinyalert(session,id="a1",HTMLtext=text,styleclass=class)
  }else{
    text<-"Penser à sauvegarder le dossier." 
    showshinyalert(session,id="a1",HTMLtext=text,styleclass=class)
  }
    
})


observe({
  input$selproj
  maxid<-max(capelist()[[2]]$num)
  session$sendCustomMessage(type='max',maxid) 
})

observe({
  input$selproj
  projet<-input$selproj
  session$sendCustomMessage(type='proj',projet) 
})
############ minimum 

seuil<-reactive({
  sh<-ifelse(input$SH<=0,1,input$SH)
  return(c(sh)) 
})

output$notasurf<-renderText({
  notabene<-""
  notabene[1]<-ifelse(input$SAU<=0,"La SAU ne peut être nulle!","")
  notabene[2]<-ifelse(input$SH<=0,"La surface en herbe ne peut être nulle!","")
  notabene[3]<-ifelse(input$SH>input$SAU,"La surface en herbe ne peut être plus grande que la SAU!","")
  
  return(paste(notabene[1],notabene[2],notabene[3]))
})
########## 
output$notani<-renderText({
  notani<-""
  notani[1]<-ifelse(input$UGBL<input$VL,"Le nombre de VL  ne peut être inférieur aux UGB lait","")
  notani[2]<-ifelse(input$UGB<input$UGBL,"Les UGB lait ne peuvent être inférieures aux UGB totales","")
  notani[3]<-ifelse(input$UGB<=0,"Il n'y a pas d'UGB ????","")
  
  return(paste(notani[1],notani[2],notani[3]))
})

output$spelait1<-renderText({
 
  text1<-paste(round(input$UGBL/input$VL,2),"UGB lait par vache laitière.")

  return(text1)
  
})

output$spelait2<-renderText({
  
  text2<- paste(round(input$UGB/input$UGBL,2),"UGB par UGB lait")

  return(text2)
  
})


output$spelait3<-renderText({

  text3<- paste(round(input$VL*input$RDT/(input$SH+input$HaFour1+input$HaFour2+input$HaFour3+input$HaFour4+input$Hader),0),
                "litres de lait par ha SFP") 
  return(text3)
  
})

output$speva<-renderText({
  UGBV<-input$UGB-input$UGBL
  VA<-input$VA
  if(UGBV>0)
  {
    text<-paste(UGBV,"autres UGB que bovins lait","\n",round(VA/UGBV,2),"vache allaitante par autres UGB que le lait") 
  }else{
    text<-"Pas d'autres UGB que laitiers"
  }
  
  return(text)
  
})

output$MO<-renderText({
  MO<-paste(input$TUMOE+input$TUMOS,"UMO dont",input$TUMOS,"salariés")
  return(MO)  
})

output$recap<-renderTable({
  SFP<-input$SH+input$HaFour1+input$HaFour2+input$HaFour3+input$HaFour4+input$Hader
  if(SFP>0){
    recap<-data.frame("SAU"=input$SAU,"SFP"=SFP,
                      "UGB"=input$UGB,"Dt UGB lait"=input$UGBL,"VL"=input$VL,"Rdt lait"=input$RDT,"Lait.an L"=input$VL*input$RDT,
                      "Lait.ha SFP"=round(input$VL*input$RDT/(input$SH+input$HaFour1+input$HaFour2+input$HaFour3+input$HaFour4+input$Hader),0),
                      "UGB.haSFP"=round(input$UGB/SFP))
    recap<-t(recap)
    colnames(recap)<-c(input$selproj)
    return(recap) 
  }else{
    recap<-NULL
  }
},digits=0
)


######### changement de projet

observe({
  input$selproj
  ref<-refemb()
  if(!is.null(input$selproj))
  { 
    projid<-capelist()[[2]]
    w<-which(projid$projid==input$selproj)
    
############ feuille HOME    
    updateTextInput(session,"desproj","Description",value=projid$desc[w])
  updateTextInput(session,"DescriProj",value=projid$descriproj[w])
    updateTextInput(session,"tec",value=projid$tec[w])
    updateDateInput(session,"datereal",value=projid$real[w])
    updateTextInput(session,"ferme",value=capelist()[[1]]$nomex)


######### feuille exploitation 
    projex<-capelist()[[3]]
    updateNumericInput(session,"TUMOE",value=projex$ex_umoex[w])
    updateNumericInput(session,"TUMOS",value=projex$ex_umosal[w])
    updateNumericInput(session,"TUMOB",value=projex$ex_umoben[w])
    updateNumericInput(session,"SAU",value=projex$ex_sau[w])
    updateNumericInput(session,"LAB",value=projex$ex_lab[w])
    updateNumericInput(session,"SH",value=projex$ex_shpat[w],min=1)
    updateNumericInput(session,"PT",value=projex$ex_PT[w],min=0)
    updateNumericInput(session,"SHnorec",value=projex$ex_shrec[w],min=0)
    updateNumericInput(session,"SHens",value=projex$ex_shens[w])
    updateNumericInput(session,"SHpatvl",value=projex$ex_shpatvl[w])
    updateNumericInput(session,"SHvlnorec",value=projex$ex_shvlnorec[w])
    updateNumericInput(session,"VL",value=projex$ex_vl[w])
    updateNumericInput(session,"RDT",value=projex$ex_rdtvl[w])
    updateNumericInput(session,"UGB",value=projex$ex_ugb[w])
     updateNumericInput(session,"UGBES",value=projex$ex_ugbes[w])
     updateNumericInput(session,"ENG",value=projex$ex_eng[w])
   updateNumericInput(session,"UGBL",value=projex$ex_ugbl[w])
   updateNumericInput(session,"VA",value=projex$ex_va[w])
    updateNumericInput(session,"UGB_UGBL",value=projex$ex_tougb[w])
   updateNumericInput(session,"UGBL_VL",value=projex$ex_tougbl[w])
   updateTextInput(session,"logani",value=projex$ex_logani[w])
   updateTextInput(session,"stofou",value=projex$ex_stofou[w])
  updateTextInput(session,"instraite",value=projex$ex_traite[w])
   updateTextInput(session,"surfex",value=projex$ex_surfex[w])
   updateTextInput(session,"herbipat",value=projex$ex_herbipat[w])
updateTextInput(session,"herbihs",value=projex$ex_herbihs[w])
updateTextInput(session,"hs",value=projex$ex_hs[w])
updateTextInput(session,"SdQ",value=projex$ex_SdQ[w])
updateTextInput(session,"maxilog",value=projex$ex_maxilog[w])
updateTextInput(session,"maxitraite",value=projex$ex_maxitraite[w])
updateTextInput(session,"maxiw",value=projex$ex_maxitrav[w])
####################paturage
    pat<-capelist()[[4]]

    updateNumericInput(session,"PC_vt_print",value=pat$pat_PCVTp[w])
    updateNumericInput(session,"Surf_vt_prin",value=pat$pat_SVTp[w])
    updateNumericInput(session,"Surf_ugba_prin",value=pat$pat_SUGBAp[w])
    updateNumericInput(session,"PC_vt_ete",value=pat$pat_PCVTe[w])
   updateNumericInput(session,"Surf_vt_ete",value=pat$pat_SVTe[w])
   updateNumericInput(session,"Surf_ugba_ete",value=pat$pat_SUGBAe[w])
  updateNumericInput(session,"PC_vt_oto",value=pat$pat_PCVTo[w])
  updateNumericInput(session,"Surf_vt_oto",value=pat$pat_SVTo[w])
  updateTextInput(session,"ComPat",value=pat$pat_compat[w])

 ############# recolte d'herbe
  rech<-capelist()[[5]]
updateSelectInput(session,"refou",selected=rech$rec_refou[w])
updateNumericInput(session,"PC_ens",value=rech$rec_pcens[w])
updateNumericInput(session,"RDT_ens",value=rech$rec_rendens[w])
updateNumericInput(session,"PC_enr",value=rech$rec_pcenr[w])
updateNumericInput(session,"RDT_enr",value=rech$rec_rendenr[w])
updateNumericInput(session,"PC_foinprec",value=rech$rec_pcfoinp[w])
updateNumericInput(session,"RDT_foinprec",value=rech$rec_rendfoinp[w])
updateNumericInput(session,"RDT_fointard",value=rech$rec_rendfoint[w])   
updateNumericInput(session,"RDT_2C",value=rech$rec_rendreg[w])
updateTextInput(session,"typerecot",value=rech$rec_typerecot[w])
updateNumericInput(session,"msrecot",value=rech$rec_msrecot[w])
updateTextInput(session,"ComRecherbe",value=rech$rec_comrech[w])

########################
bes<-capelist()[[6]]
updateNumericInput(session,"jours_print",value=bes$bes_jprint[w])
updateNumericInput(session,"kgms_print",value=bes$bes_kgprint[w])
updateNumericInput(session,"jours_ete",value=bes$bes_jete[w])
updateNumericInput(session,"kgms_ete",value=bes$bes_kgete[w])
updateNumericInput(session,"jours_aut",value=bes$bes_joto[w])
updateNumericInput(session,"kgms_aut",value=bes$bes_kgoto[w])
updateNumericInput(session,"kgms_hiv",value=bes$bes_kghiv[w])
updateNumericInput(session,"CONVL",value=bes$bes_conan[w])
updateNumericInput(session,"Tms_ugbp",value=bes$bes_tugbp[w])
updateNumericInput(session,"Tms_ugbe",value=bes$bes_tugbe[w])
updateNumericInput(session,"PC_pertes",value=bes$bes_pertes[w])
updateTextInput(session,"ComBesani",value=bes$bes_combesani[w])
##############

cf<-capelist()[[7]]

#zoncf<-zones("cf",cf$cf_zone[w])
refcf<-ref[[zoncf]]$Four
#updateSelectInput(session,"fouzon",selected=cf$cf_zone[w])
updateSelectInput(session,"four1",selected=cf$cf_four1[w],choices=refcf)
updateNumericInput(session,"HaFour1",value=cf$cf_surfour1[w])
updateNumericInput(session,"RdtFour1",value=cf$cf_rdtfour1[w])
updateSelectInput(session,"four2",selected=cf$cf_four2[w],choices=refcf)
updateNumericInput(session,"HaFour2",value=cf$cf_surfour2[w])
updateNumericInput(session,"RdtFour2",value=cf$cf_rdtfour2[w])
updateSelectInput(session,"four3",selected=cf$cf_four3[w],choices=refcf)
updateNumericInput(session,"HaFour3",value=cf$cf_surfour3[w])
updateNumericInput(session,"RdtFour3",value=cf$cf_rdtfour3[w])
updateSelectInput(session,"four4",selected=cf$cf_four4[w],choices=refcf)
updateNumericInput(session,"HaFour4",value=cf$cf_surfour4[w])
updateNumericInput(session,"RdtFour4",value=cf$cf_rdtfour4[w])
updateSelectInput(session,"der",selected=cf$cf_der[w],choices=refcf)
updateNumericInput(session,"Hader",value=cf$cf_surfder[w])
updateNumericInput(session,"Rdtder",value=cf$cf_rdtder[w])
updateTextInput(session,"ComCF",value=cf$cf_comcf[w])
##################
cv<-capelist()[[8]]
#updateSelectInput(session,"cvzon",selected="saisie")
#zoncv<-zones("cv",cv$cv_zone[w])
zoncv<-4
refcv<-ref[[zoncv]][,1]
updateSelectInput(session,"cv1",selected=cv$cv_cv1[w],choices=refcv)
updateNumericInput(session,"PCv1",value=cv$cv_pcv1[w])
updateNumericInput(session,"RdtCv1",value=cv$cv_rdtcv1[w])
updateNumericInput(session,"T1ani",value=cv$cv_t1ani[w])
updateNumericInput(session,"T1vl",value=cv$cv_t1vl[w])
updateSelectInput(session,"cv2",selected=cv$cv_cv2[w],choices=refcv)
updateNumericInput(session,"PCv2",value=cv$cv_pcv2[w])
updateNumericInput(session,"RdtCv2",value=cv$cv_rdtcv2[w])
updateNumericInput(session,"T2ani",value=cv$cv_t2ani[w])
updateNumericInput(session,"T2vl",value=cv$cv_t2vl[w])
updateSelectInput(session,"cv3",selected=cv$cv_cv3[w],choices=refcv)
updateNumericInput(session,"PCv3",value=cv$cv_pcv3[w])
updateNumericInput(session,"RdtCv3",value=cv$cv_rdtcv3[w])
updateNumericInput(session,"T3ani",value=cv$cv_t3ani[w])
updateNumericInput(session,"T3vl",value=cv$cv_t3vl[w])
updateSelectInput(session,"cv4",selected=cv$cv_cv4[w],choices=refcv)
updateNumericInput(session,"PCv4",value=cv$cv_pcv4[w])
updateNumericInput(session,"RdtCv4",value=cv$cv_rdtcv4[w])
updateNumericInput(session,"T4ani",value=cv$cv_t4ani[w])
updateNumericInput(session,"T4vl",value=cv$cv_t4vl[w])
updateNumericInput(session,"PriCv1",value=cv$cv_pri1[w])
updateNumericInput(session,"PriCv2",value=cv$cv_pri2[w])
updateNumericInput(session,"PriCv3",value=cv$cv_pri3[w])
updateNumericInput(session,"PriCv4",value=cv$cv_pri4[w])
updateTextInput(session,"ComCV",value=cv$cv_comcv[w])
###############
fert<-capelist()[[9]]
updateNumericInput(session,"NH",value=fert$fert_NH[w])
updateNumericInput(session,"PH",value=fert$fert_PH[w])
updateNumericInput(session,"KH",value=fert$fert_KH[w])
updateNumericInput(session,"NCF",value=fert$fert_NCF[w])
updateNumericInput(session,"PCF",value=fert$fert_PCF[w])
updateNumericInput(session,"KCF",value=fert$fert_KCF[w])
updateNumericInput(session,"NCV",value=fert$fert_NCV[w])
updateNumericInput(session,"PCV",value=fert$fert_PCV[w])
updateNumericInput(session,"KCV",value=fert$fert_KCV[w])
updateTextInput(session,"ComFerti",value=fert$fert_comferti[w])
#######################
fourh<-capelist()[[10]]

updateSelectInput(session,"achafour1",selected=fourh$fourh_nhfour1[w])
updateNumericInput(session,"Tachafour1",value=fourh$fourh_thfour1[w])
updateNumericInput(session,"Pachafour1",value=fourh$fourh_phfour1[w])
updateSelectInput(session,"achafour2",selected=fourh$fourh_nhfour2[w])
updateNumericInput(session,"Tachafour2",value=fourh$fourh_thfour2[w])
updateNumericInput(session,"Pachafour2",value=fourh$fourh_phfour2[w])
updateSelectInput(session,"achafour3",selected=fourh$fourh_nhfour3[w])
updateNumericInput(session,"Tachafour3",value=fourh$fourh_thfour3[w])
updateNumericInput(session,"Pachafour3",value=fourh$fourh_phfour3[w])
updateNumericInput(session,"Tventefour1",value=fourh$fourh_tfouvendu[w])
updateNumericInput(session,"Pventefour1",value=fourh$fourh_pfouvendu[w])
updateCheckboxInput(session,"refoupri1",value=fourh$fourh_mopri1[w])
updateCheckboxInput(session,"refoupri2",value=fourh$fourh_mopri2[w])
updateCheckboxInput(session,"refoupri3",value=fourh$fourh_mopri3[w])
#################
con<-capelist()[[11]]
updateNumericInput(session,"minvl",value=con$con_minvl[w])
updateNumericInput(session,"minugb",value=con$con_minugb[w])
updateNumericInput(session,"pricmv",value=con$con_pricmv[w])
updateSelectInput(session,"con1",selected=con$con_con1[w])
updateNumericInput(session,"Tcon1",value=con$con_tcon1[w])
updateNumericInput(session,"Tconvl1",value=con$con_tconvl1[w])
updateNumericInput(session,"Pcon1",value=con$con_pcon1[w])  
updateSelectInput(session,"con2",selected=con$con_con2[w])
updateNumericInput(session,"Tcon2",value=con$con_tcon2[w])
updateNumericInput(session,"Tconvl2",value=con$con_tconvl2[w])
updateNumericInput(session,"Pcon2",value=con$con_pcon2[w]) 
updateSelectInput(session,"con3",selected=con$con_con3[w])
updateNumericInput(session,"Tcon3",value=con$con_tcon3[w])
updateNumericInput(session,"Tconvl3",value=con$con_tconvl3[w])
updateNumericInput(session,"Pcon3",value=con$con_pcon3[w])
updateSelectInput(session,"con4",selected=con$con_con4[w])
updateNumericInput(session,"Tcon4",value=con$con_tcon4[w])
updateNumericInput(session,"Tconvl4",value=con$con_tconvl4[w])
updateNumericInput(session,"Pcon4",value=con$con_pcon4[w])
updateSelectInput(session,"con5",selected=con$con_con5[w])
updateNumericInput(session,"Tcon5",value=con$con_tcon5[w])
updateNumericInput(session,"Tconvl5",value=con$con_tconvl5[w])
updateNumericInput(session,"Pcon5",value=con$con_pcon5[w])
updateSelectInput(session,"con6",selected=con$con_con6[w])
updateNumericInput(session,"Tcon6",value=con$con_tcon6[w])
updateNumericInput(session,"Tconvl6",value=con$con_tconvl6[w])
updateNumericInput(session,"Pcon6",value=con$con_pcon6[w]) 

updateCheckboxInput(session,"refcon1",value=con$con_priref1[w])
updateCheckboxInput(session,"refcon2",value=con$con_priref2[w])
updateCheckboxInput(session,"refcon3",value=con$con_priref3[w])
updateCheckboxInput(session,"refcon4",value=con$con_priref4[w])
updateCheckboxInput(session,"refcon5",value=con$con_priref5[w])
updateCheckboxInput(session,"refcon6",value=con$con_priref6[w])
#################
prod<-capelist()[[12]]

if(!is.null(prod$syseco[w])){ updateRadioButtons(session,"ficheco",
                    choices=c("Lait_spe"="SPP",
                             "Lait_CV"="PEL_SP",
                             "Lait_viande"="SPP_BV",
                             "Montagne de l'Est "="SPM_EST",
                             "Autres montagne herbagère"="SPM_MC",
                             "Montagne avec maïs"="SPM_M"),selected=prod$syseco[w],inline=FALSE)}

updateNumericInput(session,"autoc",value=prod$pro_autoc[w])
updateNumericInput(session,"limA",value=prod$pro_limA[w])
updateNumericInput(session,"priA",value=prod$pro_priA[w])
updateNumericInput(session,"limB",value=prod$pro_limB[w])
updateNumericInput(session,"priB",value=prod$pro_priB[w])  
updateNumericInput(session,"limC",value=prod$pro_limC[w])
updateNumericInput(session,"priC",value=prod$pro_priC[w]) 
updateNumericInput(session,"viabl",value=prod$pro_viabl[w])
updateNumericInput(session,"kgv_ugb",value=prod$pro_kgvv[w])
updateNumericInput(session,"priv",value=prod$pro_priv[w])
updateNumericInput(session,"ParPac",value=prod$pro_pardpb[w])
updateNumericInput(session,"DPB",value=prod$pro_dpb[w])
updateSelectInput(session,"PrimeVL",selected=con$con_primevl[w])
updateNumericInput(session,"ParIchn",value=prod$pro_parichn[w])
updateNumericInput(session,"ICHN1",value=prod$pro_ichn1[w])
updateNumericInput(session,"ICHN2",value=prod$pro_ichn2[w])
updateNumericInput(session,"otraid",value=prod$pro_otraid[w])
updateNumericInput(session,"otreprod",value=prod$pro_otprod[w])
updateNumericInput(session,"memotprod",value=prod$pro_memotprod[w])
############### 
chop<-capelist()[[13]] 
#updateCheckboxInput(session,"refani",value=chop$chop_refani[w]) 
updateNumericInput(session,"cmv",value=chop$chop_cmv[w])
updateNumericInput(session,"lit",value=chop$chop_lit[w])
updateNumericInput(session,"veto",value=chop$chop_veto[w])
updateNumericInput(session,"fel",value=chop$chop_fel[w])
updateNumericInput(session,"divel",value=chop$chop_divel[w])
updateNumericInput(session,"otrechani",value=chop$chop_ochani[w])
updateTextInput(session,"memotani",value=chop$chop_memochani[w])
updateCheckboxInput(session,"referti",value=chop$chop_refert[w]) 
updateNumericInput(session,"priN",value=chop$chop_priN[w])
updateNumericInput(session,"priP",value=chop$chop_priP[w])
updateNumericInput(session,"priK",value=chop$chop_priK[w])
#updateCheckboxInput(session,"refinsurf",value=chop$chop_resurf[w]) 
updateNumericInput(session,"sem",value=chop$chop_semha[w])
updateNumericInput(session,"phyt",value=chop$chop_phytha[w])
updateNumericInput(session,"surdiv",value=chop$chop_divha[w])
updateNumericInput(session,"otrechasurf",value=chop$chop_otchas[w])
updateTextInput(session,"memochas",value=chop$chop_memochas[w])
######## 
fix<-capelist()[[14]] 
updateCheckboxInput(session,"refrec",value=fix$fix_refrec[w]) 
updateNumericInput(session,"recens",value=fix$fix_recens[w])  
updateNumericInput(session,"recenr",value=fix$fix_recenr[w])
updateNumericInput(session,"recfoin",value=fix$fix_recfoin[w])
updateNumericInput(session,"sal",value=fix$fix_sal[w])
updateNumericInput(session,"ferm",value=fix$fix_ferm[w])
#updateCheckboxInput(session,"refstruc",value=fix$fix_refstruc[w])
updateNumericInput(session,"carb",value=fix$fix_carb[w])
updateNumericInput(session,"entremat",value=fix$fix_entremat[w])
updateNumericInput(session,"entrebat",value=fix$fix_entrebat[w])
updateNumericInput(session,"tdep",value=fix$fix_tdep[w])
updateNumericInput(session,"assu",value=fix$fix_assu[w])
updateNumericInput(session,"impt",value=fix$fix_impt[w])
updateNumericInput(session,"eau",value=fix$fix_eau[w])
updateNumericInput(session,"edfgdf",value=fix$fix_edfgdf[w])
updateNumericInput(session,"fraig",value=fix$fix_fraig[w])
updateNumericInput(session,"div",value=fix$fix_div[w])
updateNumericInput(session,"otrefix",value=fix$fix_otrefix[w])
updateTextInput(session,"memofix",value=fix$fix_memofix[w])
#updateCheckboxInput(session,"modifrec",value=fix$fix_okrec[w])
updateNumericInput(session,"rechah",value=fix$fix_rechah[w])
updateNumericInput(session,"rechacf",value=fix$fix_rechacf[w])
updateNumericInput(session,"rechacv",value=fix$fix_rechacv[w])
############
r<-capelist()[[15]] 
updateNumericInput(session,"oldannu",value=r$rev_oldannu[w])
updateNumericInput(session,"msa",value=r$rev_msa[w])
#updateCheckboxInput(session,"automsa",value=r$rev_automsa[w])
updateNumericInput(session,"ppautof",value=r$rev_ppautof[w])
updateNumericInput(session,"secautof",value=r$rev_secautof[w])
#updateCheckboxInput(session,"autosec",value=r$rev_autosec[w])
updateNumericInput(session,"montinv",value=r$rev_montinv[w])
updateNumericInput(session,"duran",value=r$rev_duran[w])
updateNumericInput(session,"taux",value=r$rev_taux[w])
updateTextInput(session,"cominv",value=r$rev_cominv[w])

updateSelectInput(session,"varisk",selected=r$rev_varisk[w])
updateNumericInput(session,"varpri",value=r$rev_varpri[w])
updateNumericInput(session,"iterpri",value=r$rev_iterpri[w])
updateNumericInput(session,"varvol",value=r$rev_varvol[w])
updateNumericInput(session,"itervol",value=r$rev_itervol[w])
updateSelectInput(session,"result",selected=r$rev_result[w])
updateTextInput(session,"CoMatrix",value=r$rev_comatrix[w])
 ########## 
res<-capelist()[[1]]
updateCheckboxGroupInput(session,"capel_ex",selected=res$datex[[1]])
updateCheckboxGroupInput(session,"capel_surf",selected=res$surf[[1]])
updateCheckboxGroupInput(session,"capel_alim",selected=res$alim[[1]])
updateCheckboxGroupInput(session,"capel_prod",selected=res$prod[[1]])

########## travail
tra<-capelist()[[16]]
updateNumericInput(session,"traite",value=tra$tra_traite[w])
updateNumericInput(session,"tralim",value=tra$tra_alim[w])
updateNumericInput(session,"curapa",value=tra$tra_curapa[w])
updateNumericInput(session,"curapa",value=tra$tra_curapa[w])
 updateNumericInput(session,"travo",value=tra$tra_vo[w])
 updateNumericInput(session,"trapat",value=tra$tra_pat[w])
 updateNumericInput(session,"troupeau",value=tra$tra_troup[w])
updateNumericInput(session,"trafou",value=tra$tra_four[w])
updateNumericInput(session,"tracul",value=tra$tra_cul[w])
updateTextInput(session,"comast",value=tra$tra_comast[w])
updateTextInput(session,"comts",value=tra$tra_comts[w])
updateNumericInput(session,"TAdel",value=tra$tra_TAdel[w])
updateNumericInput(session,"TAsal",value=tra$tra_TAsal[w])
updateNumericInput(session,"TAben",value=tra$tra_TAben[w])
updateNumericInput(session,"TSdel",value=tra$tra_TSdel[w])
updateNumericInput(session,"TSal",value=tra$tra_TSal[w])
updateNumericInput(session,"TSben",value=tra$tra_TSben[w])
updateTextInput(session,"comrepart",value=tra$tra_comrepart[w])
updateNumericInput(session,"tradiv",value=tra$tra_tradiv[w])
updateTextInput(session,"queltradiv",value=tra$tra_queltradiv[w])
}
})


####remplissage avec référentiels
# observe({
#   cfzon<-input$fouzon
#   ref<-refemb()
#   cf<-capelist()[[7]]
# 
#     projid<-capelist()[[2]]
#     w<-which(projid$projid==input$selproj)
#    f1<-cf$cf_four1[w]
#    
#   zon<-zones("cf",input$fouzon)
#    w1<- which(ref[[zon]][,1]==f1)
#   libf1<-ifelse(length(w1>0,f1,"saisie"))
#   
#   if(cfzon!="saisie")
#   {
#     cfref<-as.character(ref[[zon]][,1])
#     updateSelectInput(session,"four1",label=paste("Fourrage 1 dans la",cfzon),choices=cfref)
#     updateSelectInput(session,"four2",label=paste("Fourrage 2 dans la",cfzon),choices=cfref) 
#     updateSelectInput(session,"four3",label=paste("Fourrage 3 dans la",cfzon),choices=cfref) 
#     updateSelectInput(session,"four4",label=paste("Fourrage 4 dans la",cfzon),choices=cfref)
#     
#   }
# })

observeEvent(input$applito,{
updateNumericInput(session,"UGBL",value=input$VL*input$UGBL_VL)

updateNumericInput(session,"UGB",value=input$VL*input$UGBL_VL*input$UGB_UGBL)
})




observe({
   # cfzon<-input$fouzon
   ref<-refemb()
#   zon<-zones("cf",input$fouzon)
     zon<-zoncf

      culfou<-capelist()[[7]]
      projid<-capelist()[[2]]
      w<-which(projid$projid==input$selproj)
  
      f1<-culfou$cf_four1[w]
      w1<- which(ref[[zon]]$Four == f1)
      libf1<-ifelse(length(w1)>0,f1,"saisie")
     
  f2<-culfou$cf_four2[w]
  w2<- which(ref[[zon]]$Four == f2)
  libf2<-ifelse(length(w2)>0,f2,"saisie")
  
  f3<-culfou$cf_four3[w]
  w3<- which(ref[[zon]]$Four == f3)
  libf3<-ifelse(length(w3)>0,f3,"saisie")
  
  f4<-culfou$cf_four4[w]
  w4<- which(ref[[zon]]$Four == f4)
  libf4<-ifelse(length(w4)>0,f4,"saisie")
  
  #if(cfzon!="saisie")
  #{
    cfref<-as.character(ref[[zon]][,1])
    updateSelectInput(session,"four1",choices=cfref,selected=libf1)
    updateSelectInput(session,"four2",choices=cfref,selected=libf2) 
    updateSelectInput(session,"four3",choices=cfref,selected=libf3) 
    updateSelectInput(session,"four4",choices=cfref,selected=libf4)
    
  #}
})


observeEvent (input$utilicf, {
  input$four1
  input$four2
  input$four3
  input$four4
  ref<-refemb()
  #zon<-zones("cf",isolate(input$fouzon))
  zon<-zoncf
 # if(input$fouzon!="saisie")
  #{
    cf<-ref[[zon]]
    cf$Four<-as.character(cf$Four)
   w1<- which(cf$Four==input$four1)
    val1<-ifelse(length(w1)>0,cf$Rdt[w1],0)
    updateNumericInput(session,"RdtFour1",value=cf$Rdt[w1])
   
   w2<- which(cf$Four==input$four2)
   val2<-ifelse(length(w2)>0,cf$Rdt[w2],0) 
   updateNumericInput(session,"RdtFour2",value=val2)
   
   w3<- which(cf$Four==input$four3)
   val3<-ifelse(length(w3)>0,cf$Rdt[w3],0) 
   updateNumericInput(session,"RdtFour3",value=val3)
   
   w4<- which(cf$Four==input$four4)
   val4<-ifelse(length(w4)>0,cf$Rdt[w4],0) 
   updateNumericInput(session,"RdtFour4",value=val4)
  
  #}
  
})

#### choix des grandes cultures
observe({
  #input$cvzon
  #cvzon<-input$cvzon
  ref<-refemb()
#   zon<-zones("cv",input$cvzon)
  zon<-zoncv
#   if(cvzon!="saisie")
#   {

         grancul <-capelist()[[8]]
          projid<-capelist()[[2]]
         w<-which(projid$projid==input$selproj)

          cv1<-grancul$cv_cv1[w]
          w1<- which(ref[[zon]][,1] == cv1)
          libcv1<-ifelse(length(w1)>0,cv1,"saisie")

        cv2<-grancul$cv_cv2[w]
         w2<- which(ref[[zon]][,1] == cv2)
         libcv2<-ifelse(length(w2)>0,cv2,"saisie")

          cv3<-grancul$cv_cv3[w]
          w3<- which(ref[[zon]][,1] == cv3)
          libcv3<-ifelse(length(w3)>0,cv3,"saisie")  

            cv4<-grancul$cv_cv4[w]
            w4<- which(ref[[zon]][,1] == cv4)
            libcv4<-ifelse(length(w4)>0,cv4,"saisie")


    cvref<-as.character(ref[[zon]][,1])
    updateSelectInput(session,"cv1",choices=cvref,selected=libcv1)
    updateSelectInput(session,"cv2",choices=cvref,selected=libcv2) 
    updateSelectInput(session,"cv3",choices=cvref,selected=libcv3) 
    updateSelectInput(session,"cv4",choices=cvref,selected=libcv4)
    
 # }
    
})

observeEvent(input$utilicv,{

  ref<-refemb()
  #zoncv<-zones("cv",input$cvzon)
  zon<-zoncv
  grancul<-ref[[zon]]
  
  grancul$CV<-as.character(grancul$CV)
  
  w1<- which(grancul$CV==input$cv1)
  updateNumericInput(session,"RdtCv1",value=grancul$Rdt[w1])
  updateNumericInput(session,"PriCv1",value=grancul$privente[w1])
  
  w2<- which(grancul$CV==input$cv2)
  updateNumericInput(session,"RdtCv2",value=grancul$Rdt[w2])
  updateNumericInput(session,"PriCv2",value=grancul$privente[w2])
 
  w3<- which(grancul$CV==input$cv3)
  updateNumericInput(session,"RdtCv3",value=grancul$Rdt[w3])
  updateNumericInput(session,"PriCv3",value=grancul$privente[w3])
  
  w4<- which(grancul$CV==input$cv4)
  updateNumericInput(session,"RdtCv4",value=grancul$Rdt[w4])
  updateNumericInput(session,"PriCv4",value=grancul$privente[w4])
  
  
})

observe({
  input$achafour1
  input$refoupri1
  ref<-refemb()
  
  if(input$refoupri1==TRUE)
  {
    f<-which(ref[[5]]$four==input$achafour1)
    updateNumericInput(session,"Pachafour1",value=ref[[5]][f,5])    
  }
  
  input$achafour2
  input$refoupri2
  
  if(input$refoupri2==TRUE)
  {
    f<-which(ref[[5]]$four==input$achafour2)
    updateNumericInput(session,"Pachafour2",value=ref[[5]][f,5])    
  }
  
  input$achafour3
  input$refoupri3
  
  if(input$refoupri3==TRUE)
  {
    f<-which(ref[[5]]$four==input$achafour3)
    updateNumericInput(session,"Pachafour3",value=ref[[5]][f,5])    
  }
  
})


observe({
  ref<-refemb()
  input$con1;input$refcon1  
  if(input$refcon1==TRUE){a<-which(ref[[6]]$conc==input$con1);updateNumericInput(session,"Pcon1",value=ref[[6]][a,5])}
  input$con2;input$refcon2  
  if(input$refcon2==TRUE){b<-which(ref[[6]]$conc==input$con2);updateNumericInput(session,"Pcon2",value=ref[[6]][b,5])}
  input$con3;input$refcon3  
  if(input$refcon3==TRUE){c<-which(ref[[6]]$conc==input$con3);updateNumericInput(session,"Pcon3",value=ref[[6]][c,5])}
  input$con4;input$refcon4  
  if(input$refcon4==TRUE){d<-which(ref[[6]]$conc==input$con4);updateNumericInput(session,"Pcon4",value=ref[[6]][d,5])}
  input$con5;input$refcon5 
  if(input$refcon5==TRUE){e<-which(ref[[6]]$conc==input$con5);updateNumericInput(session,"Pcon5",value=ref[[6]][e,5])}
  input$con6;input$refcon6  
  if(input$refcon6==TRUE){f<-which(ref[[6]]$conc==input$con6);updateNumericInput(session,"Pcon6",value=ref[[6]][f,5])}
    
})

observe({
  input$referti 
  ref<-refemb()
  if(input$referti==TRUE)
    {
    a<-which(colnames(ref[[7]])==input$ficheco)
    updateNumericInput(session,"priN",value=ref[[7]][8,a])
    updateNumericInput(session,"priP",value=ref[[7]][9,a])
    updateNumericInput(session,"priK",value=ref[[7]][10,a])
  }
})

observe({
  input$refrec 
  ref<-refemb()
  if(input$refrec==TRUE)
  {
    a<-which(colnames(ref[[7]])==input$ficheco)
    updateNumericInput(session,"recens",value=ref[[7]][11,a])
    updateNumericInput(session,"recenr",value=ref[[7]][12,a])
    updateNumericInput(session,"recfoin",value=ref[[7]][13,a])
      
  }
})


observeEvent(input$refani,{
    chopis<-chopi()
    chopi<-chopani(chopis[[1]],chopis[[2]],chopis[[3]],chopis[[4]],chopis[[5]])
    UGB<-input$UGB
    cmvugb<-round(chopi[[3]]/UGB,1)
    litugb<-round(chopi[[5]]/UGB,1)
    felugb<-round(chopi[[6]]/UGB,1)
    vetugb<-round(chopi[[7]]/UGB,1)
    divugb<- round(chopi[[8]]/UGB,1)   
    
    updateNumericInput(session,"cmv",value=cmvugb)
    updateNumericInput(session,"lit",value=litugb)
    updateNumericInput(session,"veto",value=felugb)
    updateNumericInput(session,"fel",value=vetugb)
    updateNumericInput(session,"divel",value=divugb)
 
})



observeEvent(input$refinsurf,{
  ref<-refemb()
  SH<-seuil()[1]

    chopos<-chopo()
    chas<-chosurf(chopos[[1]],chopos[[2]],chopos[[3]],chopos[[4]],chopos[[5]],chopos[[6]],chopos[[7]],chopos[[8]],chopos[[9]],chopos[[10]],
                  chopos[[11]],chopos[[12]],chopos[[13]])
    semiherbe<-(SH-input$PT)*ref[[9]]$Sem[1]+input$PT*ref[[9]]$Sem[2]
    phytherbe<-(SH-input$PT)*ref[[9]]$Phyt[1]+input$PT*ref[[9]]$Phyt[2]
    divherbe<-(SH-input$PT)*ref[[9]]$Div[1]+input$PT*ref[[9]]$Div[2]
    SAU<-input$SAU
   semha<-round((chas[[4]]+chas[[5]]+semiherbe)/SAU,1)
    phytha<-round((chas[[6]]+chas[[7]]+phytherbe)/SAU,1)
    divha<-round((chas[[8]]+chas[[9]]+divherbe)/SAU,1)
   updateNumericInput(session,"sem",value=semha)
   updateNumericInput(session,"phyt",value=phytha)
   updateNumericInput(session,"surdiv",value=divha)

})


observeEvent(input$modifrec,{

    charec<-charec()
    reh<-round(ifelse(charec[2,1]>0,charec[1,1]/charec[2,1],0),0)
    ref<-round(ifelse(charec[2,2]>0,charec[1,2]/charec[2,2],0),0)
    rev<-round(ifelse(charec[2,3]>0,charec[1,3]/charec[2,3],0),0)
    updateNumericInput(session,"rechah",value=reh) 
    updateNumericInput(session,"rechacf",value=ref)
    updateNumericInput(session,"rechacv",value=rev)  

})



observeEvent(input$refstruc,{
  ref<-refemb()

    a<-which(colnames(ref[[7]])==input$ficheco)
    updateNumericInput(session,"carb",value=ref[[7]][14,a])
    updateNumericInput(session,"entremat",value=ref[[7]][15,a])
    updateNumericInput(session,"entrebat",value=ref[[7]][16,a])
    updateNumericInput(session,"tdep",value=ref[[7]][17,a])
    updateNumericInput(session,"assu",value=ref[[7]][18,a])
    updateNumericInput(session,"impt",value=ref[[7]][19,a])
    updateNumericInput(session,"eau",value=ref[[7]][20,a])
    updateNumericInput(session,"edfgdf",value=ref[[7]][21,a])
    updateNumericInput(session,"fraig",value=ref[[7]][22,a])
    updateNumericInput(session,"divt",value=ref[[7]][23,a])

})




######

output$protab<-output$protabedit<-renderTable({
  input$ad
  tab<-capelist()[[2]][,c(1:5)]
  tab$real<-format(tab$real,"%d %b %Y")
  colnames(tab)<-c("Num","Identifiant","Titre","Par: ","Le : ")
 tab$Num<-as.integer(tab$Num)
  return(tab)
},include.rownames = FALSE
)




############ validation ajout sup
observe ({
  input$ad 
  if(!is.null(input$ad))
  {
    if(input$ad == "on")
    {        
        if(!is.null(input$adproj))
        {
        projum<-as.integer(input$adproj)
        
        if(projum>=0)
        {
          capel<-isolate(capelist())
          proj<-capel[[2]]
          
          w<-which(proj$num==projum)
          if(w>0)
          {
        
      num<-max(as.integer(proj$num))+1
        projid<-data.frame(num=num,projid=paste0("scenario_",num),
        desc<-paste(input$adesc,"(<-scen",projum,")"),tec=proj$tec[w],real=format(Sys.Date(),"%Y/%m/%d"),descriproj="")
        ajoutid<-projid
        colnames(ajoutid)<-colnames(proj)
        
         q<-rbind(proj,ajoutid)
         
          capel[[2]]<-q
          q$num<-as.integer(q$num)
        
          l<-length(capel)
          for(i in 3:l)
          {
            cap<-capel[[i]]
            p<-rbind(cap[3:length(cap)],cap[w,3:length(cap)])
            capel[[i]] <-cbind(q[,1:2],p) 
          }
  
           saveRDS(capel,"data/capelist")
            z<-capel[[2]]
            n<-which(z$num==1)
           updateSelectInput(session, "selproj",choices=z$projid,selected=z$projid[n])  
         rot<-isolate(rot())
         rot[[num]]<-rot[[w]]
         saveRDS(rot,"data/rotations")
      conatou<-isolate(conatou())
      conatou[[num]]<-conatou[[w]]
      saveRDS(conatou,"data/conatou")   
         }          
    }}}}
  
})

observe({
  input$supelim

  if(!is.null(input$supelim))
  {
    elim<-as.integer(input$supelim) 
     if(elim>1)
     {
      capel<-isolate(capelist())  
      projid<-capel[[2]]
      numel<-as.integer(which(projid$num==elim))
      if(numel>0)
      {
      l<-length(capel)

      for(i in 2:l)
      {
      q<-capel[[i]][-numel,]
      capel[[i]]<-q
      }
   
      saveRDS(capel,"data/capelist")
      
    updateSelectInput(session, "selproj",choices=q$projid)
    
    rot<-isolate(rot())
    rot[[numel]]<-NULL
    saveRDS(rot,"data/rotations")
    
    conatou<-isolate(conatou())
    conatou[[numel]]<-NULL
    saveRDS(conatou,"data/conatou")

    }}}
  
})



########## enregistrements du projet en cours
  
    saveproj<-function(numproj)
      {
      SH<-seuil()[1]
    capel<-isolate(capelist())       
 
    capel[[1]]$nomex<-input$ferme
    #capel[[1]]$refino<-input$refinoperso
    proj<-capel[[2]]
     #numproj<-which(proj$projid==proj)
    
     proj$desc[numproj]<-input$desproj
     proj$tec[numproj]<-input$tec
     proj$real[numproj]<-as.Date(input$datereal)
      proj$descriproj[numproj]<-input$DescriProj
       capel[[2]]<-proj
        
    proj<-capel[[3]]
   # numproj<-which(proj$projid==input$selproj)    
    proj$ex_umoex[numproj]<-input$TUMOE
    proj$ex_umosal[numproj]<-input$TUMOS
    proj$ex_umoben[numproj]<-input$TUMOB
    proj$ex_sau[numproj]<-input$SAU
    proj$ex_lab[numproj]<-input$LAB
    proj$ex_shpat[numproj]<-SH
    proj$ex_PT[numproj]<-input$PT
    proj$ex_shrec[numproj]<-input$SHnorec
    proj$ex_shens[numproj]<-input$SHens
    proj$ex_shpatvl[numproj]<-input$SHpatvl
    proj$ex_shvlnorec[numproj]<-input$SHvlnorec
    proj$ex_vl[numproj]<-input$VL
    proj$ex_rdtvl[numproj]<-input$RDT
    proj$ex_ugb[numproj]<-input$UGB
    proj$ex_ugbes[numproj]<-input$UGBES
    proj$ex_eng[numproj]<-input$ENG
    proj$ex_ugbl[numproj]<-input$UGBL
    proj$ex_va[numproj]<-input$VA
   proj$ex_tougb[numproj]<-input$UGB_UGBL
   proj$ex_tougbl[numproj]<-input$UGBL_VL
   proj$ex_logani[numproj]<-input$logani
   proj$ex_stofou[numproj]<-input$stofou
   proj$ex_traite[numproj]<-input$instraite
   proj$ex_surfex[numproj]<-input$surfex
   proj$ex_herbipat[numproj]<-input$herbipat
   proj$ex_herbihs[numproj]<-input$herbihs
   proj$ex_hs[numproj]<-input$hs
   proj$ex_SdQ[numproj]<-input$SdQ
   proj$ex_maxilog[numproj]<-input$maxilog
   proj$ex_maxitraite[numproj]<-input$maxitraite
   proj$ex_maxitrav[numproj]<-input$maxiw
   
    capel[[3]]<-proj
       
    proj<-capel[[4]]
   # numproj<-which(proj$projid==input$selproj) 
    proj$pat_PCVTp[numproj]<-input$PC_vt_print
    proj$pat_SVTp[numproj]<-input$Surf_vt_prin
    proj$pat_SUGBAp[numproj]<-input$Surf_ugba_prin
    proj$pat_PCVTe[numproj]<-input$PC_vt_ete
    proj$pat_SVTe[numproj]<-input$Surf_vt_ete
    proj$pat_SUGBAe[numproj]<-input$Surf_ugba_ete
    proj$pat_PCVTo[numproj]<-input$PC_vt_oto
    proj$pat_SVTo[numproj]<-input$Surf_vt_oto
   proj$pat_compat[numproj]<-input$ComPat
    capel[[4]]<-proj
      
    proj<-capel[[5]]
    #numproj<-which(proj$projid==input$selproj)    
    proj$rec_refou[numproj]<-input$refou
    proj$rec_pcens[numproj]<-input$PC_ens
    proj$rec_rendens[numproj]<-input$RDT_ens
    proj$rec_pcenr[numproj]<-input$PC_enr
    proj$rec_rendenr[numproj]<-input$RDT_enr
    proj$rec_pcfoinp[numproj]<-input$PC_foinprec
    proj$rec_rendfoinp[numproj]<-input$RDT_foinprec
    proj$rec_rendfoint[numproj]<-input$RDT_fointard
    proj$rec_rendreg[numproj]<-input$RDT_2C
   proj$rec_typerecot[numproj]<-input$typerecot
   proj$rec_msrecot[numproj]<-input$msrecot
   proj$rec_comrech[numproj]<-input$ComRecHerbe
    capel[[5]]<-proj
    
    proj<-capel[[6]]
   # numproj<-which(proj$projid==input$selproj) 
    
    proj$bes_jprint[numproj]<-input$jours_print
    proj$bes_kgprint[numproj]<-input$kgms_print
    proj$bes_jete[numproj]<-input$jours_ete
    proj$bes_kgete[numproj]<-input$kgms_ete
    proj$bes_joto[numproj]<-input$jours_aut
    proj$bes_kgoto[numproj]<-input$kgms_aut
    proj$bes_kghiv[numproj]<-input$kgms_hiv
    proj$bes_conan[numproj]<-input$CONVL
    proj$bes_tugbp[numproj]<-input$Tms_ugbp
    proj$bes_tugbe[numproj]<-input$Tms_ugbe
    proj$bes_pertes[numproj]<-input$PC_pertes
    proj$bes_combesani[numproj]<-input$ComBesani
    capel[[6]]<-proj
    
    proj<-capel[[7]]
    #numproj<-which(proj$projid==input$selproj)   
    #proj$cf_zone[numproj]<-""
    proj$cf_four1[numproj]<-input$four1
    proj$cf_surfour1[numproj]<-input$HaFour1
    proj$cf_rdtfour1[numproj]<-input$RdtFour1
    proj$cf_four2[numproj]<-input$four2
    proj$cf_surfour2[numproj]<-input$HaFour2
    proj$cf_rdtfour2[numproj]<-input$RdtFour2
    proj$cf_four3[numproj]<-input$four3
    proj$cf_surfour3[numproj]<-input$HaFour3
    proj$cf_rdtfour3[numproj]<-input$RdtFour3
    proj$cf_four4[numproj]<-input$four4
    proj$cf_surfour4[numproj]<-input$HaFour4
    proj$cf_rdtfour4[numproj]<-input$RdtFour4  
   proj$cf_der[numproj]<-input$der
   proj$cf_surfder[numproj]<-input$Hader
   proj$cf_rdtder[numproj]<-input$Rdtder
   proj$cf_comcf[numproj]<-input$ComCF
    capel[[7]]<-proj
    
    proj<-capel[[8]]
    #numproj<-which(proj$projid==input$selproj)     
    #proj$cv_zone[numproj]<-""
    proj$cv_cv1[numproj]<-input$cv1
    proj$cv_pcv1[numproj]<-input$PCv1
    proj$cv_rdtcv1[numproj]<-input$RdtCv1
    proj$cv_t1ani[numproj]<-input$T1ani
    proj$cv_t1vl[numproj]<-input$T1vl
    proj$cv_pri1[numproj]<-input$PriCv1    
    proj$cv_cv2[numproj]<-input$cv2
    proj$cv_pcv2[numproj]<-input$PCv2
    proj$cv_rdtcv2[numproj]<-input$RdtCv2
    proj$cv_t2ani[numproj]<-input$T2ani
    proj$cv_t2vl[numproj]<-input$T2vl
    proj$cv_pri2[numproj]<-input$PriCv2    
    proj$cv_cv3[numproj]<-input$cv3
    proj$cv_pcv3[numproj]<-input$PCv3
    proj$cv_rdtcv3[numproj]<-input$RdtCv3
    proj$cv_t3ani[numproj]<-input$T3ani
    proj$cv_t3vl[numproj]<-input$T3vl
    proj$cv_pri3[numproj]<-input$PriCv3   
    proj$cv_cv4[numproj]<-input$cv4
    proj$cv_pcv4[numproj]<-input$PCv4
    proj$cv_rdtcv4[numproj]<-input$RdtCv4
    proj$cv_t4ani[numproj]<-input$T4ani
    proj$cv_t4vl[numproj]<-input$T4vl
    proj$cv_pri4[numproj]<-input$PriCv4  
    proj$cv_comcv[numproj]<-input$ComCV
    capel[[8]]<-proj
    
   proj<-capel[[10]]
  # numproj<-which(proj$projid==input$selproj) 
   
   proj$fourh_nhfour1[numproj]<-input$achafour1
   proj$fourh_thfour1[numproj]<-input$Tachafour1
   proj$fourh_phfour1[numproj]<-input$Pachafour1
   proj$fourh_nhfour2[numproj]<-input$achafour2
   proj$fourh_thfour2[numproj]<-input$Tachafour2
   proj$fourh_phfour2[numproj]<-input$Pachafour2
   proj$fourh_nhfour3[numproj]<-input$achafour3
   proj$fourh_thfour3[numproj]<-input$Tachafour3
   proj$fourh_phfour3[numproj]<-input$Pachafour3
   proj$fourh_mopri1[numproj]<-input$refoupri1
   proj$fourh_mopri2[numproj]<-input$refoupri2
   proj$fourh_mopri3[numproj]<-input$refoupri3
   proj$fourh_tfouvendu[numproj]<-input$Tventefour1
   proj$fourh_pfouvendu[numproj]<-input$Pventefour1
   
   capel[[10]]<-proj
   
   proj<-capel[[11]]
   # numproj<-which(proj$projid==input$selproj) 
   proj$con_minvl[numproj]<-input$minvl
   proj$con_minugb[numproj]<-input$minugb
  proj$con_pricmv[numproj]<-input$pricmv
   proj$con_con1[numproj]<-input$con1
   proj$con_tcon1[numproj]<-input$Tcon1
   proj$con_tconvl1[numproj]<-input$Tconvl1
   proj$con_pcon1[numproj]<-input$Pcon1
   proj$con_con2[numproj]<-input$con2
   proj$con_tcon2[numproj]<-input$Tcon2
   proj$con_tconvl2[numproj]<-input$Tconvl2
   proj$con_pcon2[numproj]<-input$Pcon2
   proj$con_con3[numproj]<-input$con3
   proj$con_tcon3[numproj]<-input$Tcon3
   proj$con_tconvl3[numproj]<-input$Tconvl3
   proj$con_pcon3[numproj]<-input$Pcon3
   proj$con_con4[numproj]<-input$con4
   proj$con_tcon4[numproj]<-input$Tcon4
   proj$con_tconvl4[numproj]<-input$Tconvl4
   proj$con_pcon4[numproj]<-input$Pcon4
   proj$con_con5[numproj]<-input$con5
   proj$con_tcon5[numproj]<-input$Tcon5
   proj$con_tconvl5[numproj]<-input$Tconvl5
   proj$con_pcon5[numproj]<-input$Pcon5
   proj$con_con6[numproj]<-input$con6
   proj$con_tcon6[numproj]<-input$Tcon6
   proj$con_tconvl6[numproj]<-input$Tconvl6
   proj$con_pcon6[numproj]<-input$Pcon6
   proj$con_priref1[numproj]<-input$refcon1
   proj$con_priref2[numproj]<-input$refcon2
   proj$con_priref3[numproj]<-input$refcon3
   proj$con_priref4[numproj]<-input$refcon4
   proj$con_priref5[numproj]<-input$refcon5
   proj$con_priref6[numproj]<-input$refcon6
 
  
   capel[[11]]<-proj
   
   proj<-capel[[12]]
   proj$syseco[numproj]<-input$ficheco
   proj$pro_autoc[numproj]<-input$autoc
   proj$pro_limA[numproj]<-input$limA
   proj$pro_priA[numproj]<-input$priA
   proj$pro_limB[numproj]<-input$limB
   proj$pro_priB[numproj]<-input$priB
   proj$pro_limC[numproj]<-input$limC
   proj$pro_priC[numproj]<-input$priC
   proj$pro_viabl[numproj]<-input$viabl
   proj$pro_kgvv[numproj]<-input$kgv_ugb
   proj$pro_priv[numproj]<-input$priv
   proj$pro_pardpb[numproj]<-input$ParPac
   proj$pro_dpb[numproj]<-input$DPB
   proj$pro_primvl[numproj]<-input$PrimeVL
   proj$pro_parichn[numproj]<-input$ParIchn
   proj$pro_ichn1[numproj]<-input$ICHN1
   proj$pro_ichn2[numproj]<-input$ICHN2
   proj$pro_otraid[numproj]<-input$otraid
   proj$pro_otprod[numproj]<-input$otreprod
   proj$pro_memotprod[numproj]<-input$memotprod
   capel[[12]]<-proj
   
   proj<-capel[[13]]
   #numproj<-which(proj$projid==input$selproj) 
   #proj$chop_refani[numproj]<-input$refani
   proj$chop_cmv[numproj]<-input$cmv
   proj$chop_lit[numproj]<-input$lit
   proj$chop_veto[numproj]<-input$veto
   proj$chop_fel[numproj]<-input$fel
   proj$chop_divel[numproj]<-input$divel
   proj$chop_ochani[numproj]<-input$otrechani
   proj$chop_memochani[numproj]<-input$memotani
   proj$chop_refert[numproj]<-input$referti
   proj$chop_priN[numproj]<-input$priN
   proj$chop_priP[numproj]<-input$priP
   proj$chop_priK[numproj]<-input$priK
  # proj$chop_resurf[numproj]<-input$refinsurf
   proj$chop_semha[numproj]<-input$sem
   proj$chop_phytha[numproj]<-input$phyt
   proj$chop_divha[numproj]<-input$surdiv
   proj$chop_otchas[numproj]<-input$otrechasurf
   proj$chop_memochas[numproj]<-input$memochas  
   capel[[13]]<-proj
   
   proj<-capel[[14]]
   # numproj<-which(proj$projid==input$selproj) 
   proj$fix_refrec[numproj]<-input$refrec
   proj$fix_recens[numproj]<-input$recens
   proj$fix_recenr[numproj]<-input$recenr
   proj$fix_recfoin[numproj]<-input$recfoin  
   #proj$fix_okrec[numproj]<-input$modifrec 
   proj$fix_rechah[numproj]<-input$rechah
   proj$fix_rechacf[numproj]<-input$rechacf
   proj$fix_rechacv[numproj]<-input$rechacv 
   proj$fix_sal[numproj]<-input$sal
   proj$fix_ferm[numproj]<-input$ferm
   #proj$fix_refstruc[numproj]<-input$refstruc
   proj$fix_carb[numproj]<-input$carb
   proj$fix_entremat[numproj]<-input$entremat
   proj$fix_entrebat[numproj]<-input$entrebat
   proj$fix_tdep[numproj]<-input$tdep
   proj$fix_assu[numproj]<-input$assu
   proj$fix_impt[numproj]<-input$impt
   proj$fix_eau[numproj]<-input$eau
   proj$fix_edfgdf[numproj]<-input$edfgdf
   proj$fix_fraig[numproj]<-input$fraig
   proj$fix_div[numproj]<-input$div
   proj$fix_otrefix[numproj]<-input$otrefix
   proj$fix_memofix[numproj]<-input$memofix
   capel[[14]]<-proj
   
   proj<-capel[[15]]
   #numproj<-which(proj$projid==input$selproj) 
   proj$rev_oldannu[numproj]<-input$oldannu
   proj$rev_msa[numproj]<-input$msa
   #proj$rev_automsa[numproj]<-FALSE
   proj$rev_ppautof[numproj]<-input$ppautof
   proj$rev_secautof[numproj]<-input$secautof
   #proj$rev_autosec[numproj]<-FALSE
   proj$rev_montinv[numproj]<-input$montinv
   proj$rev_duran[numproj]<-input$duran
   proj$rev_taux[numproj]<-input$taux
   proj$rev_cominv[numproj]<-input$cominv
   proj$rev_varisk[numproj]<-input$varisk
  proj$rev_varpri[numproj]<-input$varpri
  proj$rev_var1[numproj]<-input$varElem1
  proj$rev_iterpri[numproj]<-input$iterpri
  proj$rev_varvol[numproj]<-input$varvol
  proj$rev_itervol[numproj]<-input$itervol
  proj$rev_result[numproj]<-input$result
  proj$rev_comatrix[numproj]<-input$CoMatrix
   c<-input$montinv
   t<-input$duran
   i<-input$taux/100 
   proj$rev_newanu[numproj]<-c*i/(1-(1+i)^-t) 
   capel[[15]]<-proj
  
  
   proj<-capel[[16]]
  proj$tra_traite[numproj]<-input$traite
  proj$tra_alim[numproj]<-input$tralim
  proj$tra_curapa[numproj]<-input$curapa
  proj$tra_vo[numproj]<-input$travo
  proj$tra_pat[numproj]<-input$trapat
  proj$tra_troup[numproj]<-input$troupeau
  proj$tra_four[numproj]<-input$trafou
  proj$tra_cul[numproj]<-input$tracul
  proj$tra_comast[numproj]<-input$comast
  proj$tra_comts[numproj]<-input$comts
  proj$tra_tradiv[numproj]<-input$tradiv
  proj$tra_queltradiv[numproj]<-input$queltradiv
  
  proj$tra_TAdel[numproj]<-input$TAdel
  proj$tra_TAsal[numproj]<-input$TAsal
  proj$tra_TAben[numproj]<-input$TAben
  
  proj$tra_TSdel[numproj]<-input$TSdel
  proj$tra_TSal[numproj]<-input$TSal
  proj$tra_TSben[numproj]<-input$TSben
  
  proj$tra_comrepart[numproj]<-input$comrepart
  
  capel[[16]]<-proj
     
    proj<-capel[[9]]

    fertil<-fertil()

    proj$fert_HaH[numproj]<-as.numeric(fertil[1,2]) 

      
    proj$fert_NH[numproj]<-ifelse(is.na(input$NH),0,input$NH)
    proj$fert_PH[numproj]<-ifelse(is.na(input$PH),0,input$PH)
    proj$fert_KH[numproj]<-ifelse(is.na(input$KH),0,input$KH)


    capel[[9]]<-proj
     
    proj<-capel[[9]]

    proj$fert_HaCf[numproj]<-as.numeric(fertil[7,2])
    

      proj$fert_NCF[numproj]<-ifelse(is.na(input$NCF),0,input$NCF)
      proj$fert_PCF[numproj]<-ifelse(is.na(input$PCF),0,input$PCF)
      proj$fert_KCF[numproj]<-ifelse(is.na(input$KCF),0,input$KCF)
  
    capel[[9]]<-proj
      
    proj<-capel[[9]]
   
    fertil<-fertil()
    
    proj$fert_HaCv[numproj]<-as.numeric(fertil[12,2]) 
    

      proj$fert_NCV[numproj]<-ifelse(is.na(input$NCV),0,input$NCV)
      proj$fert_PCV[numproj]<-ifelse(is.na(input$PCV),0,input$PCV)
      proj$fert_KCV[numproj]<-ifelse(is.na(input$KCV),0,input$KCV)
      proj$fert_comferti[numproj]<-input$ComFerti
    capel[[9]]<-proj
    
    saveRDS(capel,"data/capelist")
  
  
    }

observe ({
  
  input$val_pro
  if(!is.null(input$val_pro)){ if(input$val_pro == "on"){ 
    capel<-isolate(capelist())       
    proj<-capel[[2]]
    numproj<-which(proj$projid==input$selproj)
    saveproj(numproj)
   
    #################
     if(!is.null(hot()))
     {
       rot<-rot()
       hot<-hot()
       rot[[numproj]]<-hot
       saveRDS(rot,"data/rotations")
     }
    
    if(!is.null(hot2()))
    {
      conatou<-conatou()
      hot2<-hot2()
      conatou[[numproj]]<-hot2
      saveRDS(conatou,"data/conatou")
    }
    
    
    capel<-readRDS( "data/capelist")
    ref<-refemb()
    rot<-readRDS("data/rotations")
    conatou<-readRDS("data/conatou")
    
    temprecup<-list(capel,ref,rot,conatou)
    temprecup$version<-vers
    
    time<-format(Sys.Date(), "%A%d%B%Y")
    recup<-paste0("tmp/",input$ferme,"_",time,".rds")
    saveRDS(temprecup,recup)
    
    capel[[1]]$datex<-list(input$capel_ex) 
    capel[[1]]$surf<-list(input$capel_surf) 
    capel[[1]]$alim<-list(input$capel_alim) 
    capel[[1]]$prod<-list(input$capel_prod)
                          
    saveRDS(capel,"data/capelist")
    
  }}
  })



################ fin application projet
############## fichier temp des surfaces
 surfex<-reactive({ 
    capelist()
    SH<-seuil()[1]
   surf<-data.frame(matrix("",5,2))
  
  colnames(surf)<-c("lib","val")
  VLpmax<-input$SHpatvl*100/input$Surf_vt_prin
  VLpos<-input$VL*input$PC_vt_print/100
  VLp<-round(min(VLpmax,VLpos),0)
  surf_VLprint<-round((VLp*input$Surf_vt_prin)/100,1)
  UGBpat<-round(input$UGB-input$UGBES-input$ENG,0)
  surf_ugbaprint<-round(((UGBpat-VLp)*input$Surf_ugba_prin)/100,1)
  rec_print<-round(SH-surf_VLprint-surf_ugbaprint,1)
  
  VLemax<-input$SHpatvl*100/input$Surf_vt_ete
  VLepos<-input$VL*input$PC_vt_ete/100
  VLe<-round(min(VLemax,VLepos),0)
  surf_VLete<-round((VLe*input$Surf_vt_ete)/100,1)
  surf_ugbaete<-round(((UGBpat-VLe)*input$Surf_ugba_ete)/100,1)
  rec_ete<-round(SH-surf_VLete-surf_ugbaete,1)
  
  VLomax<-input$SHpatvl*100/input$Surf_vt_oto
  VLopos<-input$VL*input$PC_vt_oto/100
  VLo<-round(min(VLomax,VLopos),0)
  patvlo<-round((VLo*input$Surf_vt_oto)/100,1)
  
  
  surf[1,]<-c("VLp",VLp)
  surf[2,]<-c("surf_VLprint",surf_VLprint)
  surf[3,]<-c("UGBpat",UGBpat)
  surf[4,]<-c("surf_ugbaprint",surf_ugbaprint)
  surf[5,]<-c("rec_print",rec_print)
 
  surf[6,]<-c("VLe",VLe)
  surf[7,]<-c("surf_VLete",surf_VLete)
  surf[8,]<-c("surf_ugbaete",surf_ugbaete)
  surf[9,]<-c("rec_ete",rec_ete)
  
  surf[10,]<-c("patvlo",patvlo)
  surf[11,]<-c("patot",SH)
  surf[12,]<-c("VLo",VLo)
  
  surf[13,]<-c("Four1",input$four1)
  surf[14,]<-c("HaFour1",input$HaFour1)
  surf[15,]<-c("RdtFour1",input$RdtFour1)
  
  surf[16,]<-c("Four2",input$four2)
  surf[17,]<-c("HaFour2",input$HaFour2)
  surf[18,]<-c("RdtFour2",input$RdtFour2)
  
  surf[19,]<-c("Four3",input$four3)
  surf[20,]<-c("HaFour3",input$HaFour3)
  surf[21,]<-c("RdtFour3",input$RdtFour3)
  
  surf[22,]<-c("Four4",input$four4)
  surf[23,]<-c("HaFour4",input$HaFour4)
  surf[24,]<-c("RdtFour4",input$RdtFour4)
  
  surf[25,]<-c("CV1",input$cv1)
  surf[26,]<-c("PCv1",input$PCv1)
  surf[27,]<-c("RdtCv1",input$RdtCv1)
  surf[28,]<-c("T1ani",input$T1ani)
  surf[29,]<-c("T1vl",input$T1vl)
  
  surf[30,]<-c("CV2",input$cv2)
  surf[31,]<-c("PCv2",input$PCv2)
  surf[32,]<-c("RdtCv2",input$RdtCv2)
  surf[33,]<-c("T2ani",input$T2ani)
  surf[34,]<-c("T2vl",input$T2vl)
  
  surf[35,]<-c("CV3",input$cv3)
  surf[36,]<-c("PCv3",input$PCv3)
  surf[37,]<-c("RdtCv3",input$RdtCv3)
  surf[38,]<-c("T3ani",input$T3ani)
  surf[39,]<-c("T3vl",input$T3vl)
  
  surf[40,]<-c("CV4",input$cv4)
  surf[41,]<-c("PCv4",input$PCv4)
  surf[42,]<-c("RdtCv4",input$RdtCv4)
  surf[43,]<-c("T4ani",input$T4ani)
  surf[44,]<-c("T4vl",input$T4vl)
 
  surf[45,]<-c("PriCv1",input$PriCv1)
  surf[46,]<-c("PriCv2",input$PriCv2)
  surf[47,]<-c("PriCv3",input$PriCv3)
  surf[48,]<-c("PriCv4",input$PriCv4)
  
  PC1C<-(input$PC_ens+input$PC_enr+input$PC_foinprec)/100
  surf[49,]<-c("PCFoinTardif",1-PC1C)
  surf[50,]<-c("haFoinTardif",(1-PC1C)*rec_print)
  surf[51,]<-c("hader",input$Hader)
  surf[52,]<-c("Rdtder",input$Rdtder)
  
  return(surf)
  
})

#output$tabsurfex<-renderText({
#  tab<-revenu()
#  return(tab)
#})


output$plot_pat <- renderPlot({
  s<-suppressWarnings(as.numeric(surfex()[,2]))
  
  if(s[2]>input$SHvlnorec)
 {
    patvlnorec<-input$SHvlnorec
  }else{
    patvlnorec<-s[2]
  }
  
 
 if(s[7]>input$SHvlnorec)
 {
   patvlnorecete<-input$SHvlnorec
 }else{
   patvlnorecete<-s[7]
 }
 
 validate(
   need(input$SHnorec < s[4]+patvlnorec ,paste("La surface de pâturage des animaux en 1ère période sur les surfaces non récoltables de",patvlnorec+s[4],
                                               "ha est inférieure à la surface de",input$SHnorec,"ha  déclarée dans les contraintes des surfaces de l'exploitation.", 
                                               "Augmenter la surface de paturage de la 1ère période.")),
   
   need(input$SHnorec < patvlnorecete+ s[8],paste("La surface de paturage des animaux",patvlnorecete+s[8],
                                 "ha en 2ème période est inférieure à la surface d'herbe non récoltable de",
                                 input$SHnorec,"ha  déclarée dans les contraintes des surfaces de l'exploitation.", 
                                 "Augmenter la surface de paturage de la 2ème période."))
 )
 
  pat<-c(s[1],s[2],s[3],s[4],s[5],s[6],s[7],s[8], s[9],s[10],s[11],s[12])
   return(grafpat(pat))
})

############## tableau choix pat et reference

output$fichrefou<-output$ficheherbe<-output$infoherbe<-renderText({
  input$selproj 
  if(!is.null(input$selproj)) 
    stoh<-capelist()[[5]]
  w<-which(stoh$projid==input$selproj)
  fiche<-stoh$rec_refou[w]
  if (fiche == "saisie") {text<-"Pas de fiche utilisée pour le projet actuel."
  }else{
    text<-paste("La fiche actuelle du projet est :",fiche)
  }
 
  return(text)
})

output$fichecf<-renderText({
  input$selproj 
  if(!is.null(input$selproj)) 
    cap<-capelist()[[7]]
  w<-which(cap$projid==input$selproj)  
  text<- cap$cf_zone[w]
  
  return(text)
})

output$fichecv<-renderText({
  input$selproj 
  if(!is.null(input$selproj)) 
    cap<-capelist()[[8]]
  w<-which(cap$projid==input$selproj)  
  text<- cap$cv_zone[w]
  text<-ifelse(text=="","saisie",text)
  return(text)
})


output$SurfHerbe<-renderTable({
  
  input$selproj  
  input$refou
  
  comp<-data.frame(matrix("",5,5))
  colnames(comp)<-c("Surfaces","Projet(ares/UGB)","Fiche (ares/UGB)","Projet (Ha)","Fiche (ha)")
  s<-suppressWarnings(as.numeric(surfex()[,2]))
  VLp<-s[1]; surf_VLprint<-s[2];UGBpat<-s[3];surf_ugbaprint<-s[4];rec_print<-s[5] 
  VLe<-s[6]; surf_VLete<-s[7] ; surf_ugbaete<-s[8]; rec_ete<-s[9]
  patvlo<-s[10];patot<-s[11];VLo<-s[12] 
  surf_print<-surf_VLprint+surf_ugbaprint
  surf_ete<-surf_VLete+surf_ugbaete
  HaFoinTard<-(1-(input$PC_ens+input$PC_enr+input$PC_foinprec)/100)*rec_print
  rec2C<-rec_ete-HaFoinTard
  
  stoh<-capelist()[[5]]
  refou<-refemb()[[1]]
  t<-which(stoh$projid==input$selproj)
  w<-which(refou$Code==stoh$rec_refou[t]) 
  if(length(w)>0)
  {
  comp[1,]<-c("Surfaces paturage printemps",100*surf_print/UGBpat,refou$Pat1Ares[w],surf_print, UGBpat*refou$Pat1Ares[w]/100)
  comp[2,]<-c("1eres coupes",100*rec_print/UGBpat, patot*refou$Pc1c[w]/UGBpat, rec_print,patot*refou$Pc1c[w]/100)
  comp[3,]<-c("Surfaces paturage ete",100*surf_ete/UGBpat,refou$Pat2Ares[w],surf_ete, UGBpat*refou$Pat2Ares[w]/100)
  comp[4,]<-c("2emes coupes",100*rec2C/UGBpat, patot*refou$Pc1c[w]*refou$Pc2c[w]/UGBpat/100, rec2C,patot*refou$Pc1c[w]*refou$Pc2c[w]/10000)
  comp[5,]<-c("Paturage automne", patot*100/UGBpat,refou$PatotAres[w], patot,UGBpat*refou$PatotAres[w]/100)  

  }else{
    comp[1,]<-c("Surfaces paturage printemps",100*surf_print/UGBpat,"",surf_print, "")
    comp[2,]<-c("1eres coupes",100*rec_print/UGBpat, "", rec_print,"")
    comp[3,]<-c("Surfaces paturage ete",100*surf_ete/UGBpat,"",surf_ete, "")
    comp[4,]<-c("2emes coupes",100*rec2C/UGBpat,"", rec2C,"")
    comp[5,]<-c("Paturage automne", patot*100/UGBpat,"", patot,"")
  }
 for (i in 2:5)
 {
   comp[,i]<-as.numeric(comp[,i]) 
 }
 return(comp)
  
  },digits=1,include.rownames = FALSE,align="rrrlrl"
  )
#########
observeEvent(input$utilipat,{
  if(input$refou!="saisie" ){

    fifou<-input$refou
   # ficheref<-zones("pat","auvergne")
   # refpat<-refemb()[[ficheref]]
    refpat<-refemb()[[1]]
    w<-which(refpat$Code==fifou) 
    
    updateNumericInput(session,"PC_ens",value=refpat$PcEns[w])
    updateNumericInput(session,"RDT_ens",value=refpat$RendEns[w])
    updateNumericInput(session,"PC_enr",value=refpat$PcEnr[w])
    updateNumericInput(session,"RDT_enr",value=refpat$RendEnr[w])
    updateNumericInput(session,"PC_foinprec",value=refpat$PcFoinPrec[w])
    updateNumericInput(session,"RDT_foinprec",value=refpat$RendFoinPrec[w])
    updateNumericInput(session,"RDT_fointard",value=refpat$RendFoinTard[w]) 
    updateNumericInput(session,"RDT_2C",value=refpat$RendFoin2C[w])
    
  }
})


fointard<-reactive({  
  
  s<-1-(input$PC_ens-input$PC_enr-input$PC_foinprec)/100
  return(s)
})

output$PC1C<-renderPrint({
  s<-suppressWarnings(as.numeric(surfex()[,2]))
  PC1C<-round(s[49]*100,0)
  rec_ete<-s[9]
  rec_print<-s[5] 
  HaFoinTard<-round(100*(1-(input$PC_ens+input$PC_enr+input$PC_foinprec)/100)*rec_print,0) 
  PCreg<-round((rec_ete-HaFoinTard/100)*100/s[5],0)
  FoinTardif<-paste(PC1C,"% foin 1ère coupe", "\n","récolté sur ", "\n","la 2ème période", "\n",PCreg, "% de 2èmes","\n","coupes/1ères coupes.")
  cat(FoinTardif)
  })

output$RecHerbe<-renderPlot({
  
  surfex()
  input$refou
  s<-suppressWarnings(as.numeric(surfex()[,2]))
  rec_print<-s[5] 
  rec_ete<-s[9]

    HaFoinTard<-(1-(input$PC_ens+input$PC_enr+input$PC_foinprec)/100)*rec_print

  rec1<-c(HaFoinTard,input$PC_foinprec/100*rec_print,input$PC_enr/100*rec_print,
          input$PC_ens/100*rec_print )
 
  validate(
    need(HaFoinTard>=0,"% foin tardif négatif! Revoir le % des autres fourrages"),
    need(rec_ete-HaFoinTard >= 0,"% foin tardif > au récoltable été! Revoir le % des autres fourrages") 
    )
 
  #grafsto(rec1C=rec1,rec2C=rec_ete-HaFoinTard)
  if(HaFoinTard>=0 & rec_ete-HaFoinTard>=0){
    grafsto(rec1C=rec1,rec2C=rec_ete-HaFoinTard)
  }else{
    plot.new()
    text(0,0.7,labels="Foin tardif négatif!",pos=4,col="red",cex=2)
    text(0,0.5,labels="ou > au récoltabe été!",pos=4,col="red",cex=2)
    text(0,0.3,labels="Revoir les % ",pos=4,col="red",cex=2)
    text(0,0.1,labels="des autres fourrages",pos=4,col="red",cex=2)
  }
  
  
})
  
output$Stocherbe<-renderPlot({
  input$refou

  s<-suppressWarnings(as.numeric(surfex()[,2]))
  rec_print<-s[5] 
  rec_ete<-s[9]  
 Tens<-input$PC_ens*rec_print*input$RDT_ens/100
  Tenr<-input$PC_enr*rec_print*input$RDT_enr/100
  TfoinPre<-input$PC_foinprec*rec_print*input$RDT_foinprec/100
   reg<-(rec_ete-(1-(input$PC_ens+input$PC_enr+input$PC_foinprec)/100)*rec_print)*input$RDT_2C
  TfoinTard<-(1-(input$PC_ens+input$PC_enr+input$PC_foinprec)/100)*rec_print*input$RDT_fointard  
  Trecot<-input$msrecot
 
  if(reg>=0)
  {
    stock_herbe(recherbe=c(TfoinTard,TfoinPre,Tenr,Tens,reg,Trecot)) 
  }else{  
 plot.new()
 text(0,0.7,labels="Regain",pos=4,col="red",cex=1.5)
 text(0,0.5,labels="négatif",pos=4,col="red",cex=1.5)
 text(0,0.3,labels="revoir %.",pos=4,col="red",cex=1.5)
  }
  
  })  

#####besoins

bestoc<-reactive({
  bes<-as.data.frame(matrix("",7,3))
  jpat<-input$jours_print+input$jours_ete+input$jours_aut
  stopat<-input$jours_print*input$kgms_print+input$jours_ete*input$kgms_ete+input$jours_aut*input$kgms_aut
  jhiv<-365-jpat
  
 
   tmsi<-function(rdt,pv,cvl){return((2921+0.722*rdt+3.57*pv-0.04*cvl-3254*0.9-0.1975*rdt*0.9)/1000)}
  pertes<-input$PC_pertes
  tmsa<-function(rdt){ return((4.1+0.35*rdt*1.033/1000))}
  fourvl<-round(tmsa(input$RDT)-input$RDT*input$CONVL/1000000,1)
  
  #bes[1,]<-c("Consommation totale ingérée par vache par an",round(tmsi(input$RDT,650,input$CONVL*input$RDT/1000000),1),"TMS/an")
  bes[1,]<-c("Consommation totale par vache par an",round(tmsa(input$RDT),1),"TMS/an")
 
  bes[2,]<-c("Consommation de concentré annuelle par vache",round(input$RDT*input$CONVL/1000000,1),"TMS/an")
  bes[3,]<-c("Proportion de concentré dans la ration des vaches",round(input$RDT*input$CONVL/(10*(4100+0.33*1.033*input$RDT)),0),"% de la ration annuelle")
  bes[4,]<-c("Consommation de fourrage par vache et par an",fourvl,"TMS/an")
  bes[5,]<-c("Consommation de stocks fourrager par vache en hiver",round(jhiv*input$kgms_hiv/1000,1),"TMS/an")
  bes[6,]<-c("Consommation de stocks fourrager par vache au paturage",round(stopat/1000,1),"TMS/an")
  bes[7,]<-c("Consommation d'herbe paturée par vache",round(fourvl-jhiv*input$kgms_hiv/1000-stopat/1000,1),"TMS/an")
 
  stohivl<-(365-jpat)*input$kgms_hiv
  stovl<-round((stopat+stohivl)/1000,1)
  besrec<-stovl*input$VL+input$Tms_ugbp*(input$UGB-input$VL-input$ENG)+input$Tms_ugbe*input$ENG

  bes[8,]<-c("Besoin pour les vaches",stovl*input$VL,"TMS")
  bes[9,]<-c("Besoin autres ugb paturant",round(input$Tms_ugbp*(input$UGB-input$VL-input$ENG),0),"TMS")
  bes[10,]<-c("Besoins UGB engraissement",round(input$Tms_ugbe*input$ENG,0),"TMS")
  bes[11,]<-c("Besoin total de fourrages récoltés",round(besrec,0),"TMS")
  
  s<-suppressWarnings(as.numeric(surfex()[,2]))
  rec_print<-s[5] 
  rec_ete<-s[9] 
  lost<-(1-input$PC_pertes/100)
  Tens<-input$PC_ens*rec_print*input$RDT_ens*lost/100
  Tenr<-input$PC_enr*rec_print*input$RDT_enr*lost/100
  TfoinPre<-input$PC_foinprec*rec_print*input$RDT_foinprec*lost/100
  reg<-(rec_ete-(1-(input$PC_ens+input$PC_enr+input$PC_foinprec)/100)*rec_print)*input$RDT_2C*lost
  TfoinTard<-(1-(input$PC_ens+input$PC_enr+input$PC_foinprec)/100)*rec_print*input$RDT_fointard *lost
  Trecot<-input$msrecot*lost
  stoherbe<-Tens+Tenr+TfoinPre+reg+TfoinTard+Trecot
  
  bes[12,]<-c("Rappel des stocks sur pature",round(stoherbe,0),"TMS")
  bes[13,]<-c("Reste à couvrir par des achats ou d'autres fourrages",round(besrec-stoherbe,0),"TMS")
  bes[14,]<-c("Rendement laitier",input$RDT,"L/vl/an")
  bes[15,]<-c("Consommation de concentré des vaches en g/L",input$CONVL,"g/litre")
  return(bes)
  
  
})


output$besvl<-renderTable({
  bes<-bestoc()[c(1,14,15,2:7),]
  
  colnames(bes)<-c("Alimentation annuelle des vaches laitières.","Quantité","unité")
 return(bes)
  
},digits=1,include.rownames = FALSE,align="rrrl"
)


output$bestot<-renderPlot({
  bes<-as.numeric(bestoc()[8:13,2])

 besani<-c(bes[1],bes[2],bes[3])
 
  bestock(besani,bes[5],CF=FALSE)
})


output$cufou<-renderPlot({
  bes<-as.numeric(bestoc()[8:13,2]) 
  besani<-c(bes[1],bes[2],bes[3])
  lost<-(1-input$PC_pertes/100)
  cufou<-c(input$HaFour1*input$RdtFour1,input$HaFour2*input$RdtFour2,input$HaFour3*input$RdtFour3,
           input$HaFour4*input$RdtFour4,input$Hader*input$Rdtder)*lost
  
  cfnom<-c(input$four1,input$four2,input$four3,input$four4,input$der)
    bestock(besani,bes[5],CF=TRUE,cufou=cufou,cfnom=cfnom)
})


output$gcu<-renderPlot({  
  SAU<-input$SAU
  SH<-seuil()[1]
  s<-suppressWarnings(as.numeric(surfex()[,2]))
  pat<-round(s[2]+s[4],1)
  stoh<-round(SH-pat,1)
  
  hacf<-s[14]+s[17]+s[20]+s[23]
  
  CV<-round(SAU-SH-hacf,1)
  validate(
    need(CV>=0,"dans ce projet la surface en cultures de vente est négative!!, revoir les surfaces.")
    )
  hacv<-c(s[26]*CV/100,s[31]*CV/100,s[36]*CV/100,s[41]*CV/100)
  totcv<-(s[26]+s[31]+s[36]+s[41])*CV/100
  reste<-round(SAU-SH-hacf-totcv,1)
  
  if(reste>=0)  
  {
   surf <- matrix(c(totcv,reste,pat, stoh, hacf))
  coul<-c("yellow","red","green","yellowgreen","orange")
  par(bg="transparent")
  labs=c(paste("Grandes cultures",totcv,"ha"),paste("Reste",reste,"ha à affecter"),
         paste("Paturage",pat,"ha"),paste("Stocks d'herbe",stoh,"ha"),paste("Cult. four.",hacf,"ha"))
  pie(surf,labels=labs,col=coul,init.angle=180,clockwise = FALSE)
  
  }else{
    plot.new()
    text(0,0.7,labels="Trop de cultures",pos=4,col="red",cex=2)
    text(0,0.5,labels="de vente",pos=4,col="red",cex=2) 
  }
  
})

output$procul<-renderPlot({
  surfex()
  SH<-seuil()[1]
  s<-suppressWarnings(as.numeric(surfex()[,2]))
  hacf<-s[14]+s[17]+s[20]+s[23] 
  
  CV<-input$SAU-SH-hacf 
  validate(
    need(CV>=0,"dans ce projet la surface en cultures de vente est négative!!, revoir les surfaces.")
  )
  cvnom<-c(input$cv1,input$cv2,input$cv3,input$cv4)
  hacv<-c(s[26]*CV/100,s[31]*CV/100,s[36]*CV/100,s[41]*CV/100)
  prodcult<-c(s[26]*s[27]*CV/1000,s[31]*s[32]*CV/1000,s[36]*s[37]*CV/1000,s[41]*s[42]*CV/1000)
  distrani<-c(s[28],s[33],s[38],s[43])
  distrvl<-c(s[29],s[34],s[39],s[44])
  
recv(nomcul=cvnom,surcul=hacv,recven=round(prodcult-distrani-distrvl,0),intrani=distrani,intravl=distrvl) 

  
})




output$assol<-renderPlot({

  SAU<-input$SAU
  SH<-seuil()[1]
  s<-suppressWarnings(as.numeric(surfex()[,2]))
  pat<-s[2]+s[4]
  stoh<-round(SH-pat,1)

  cfnom<-c(input$four1,input$four2,input$four3,input$four4)
  hacf<-c(s[14],s[17],s[20],s[23])
  totcf<-round(s[14]+s[17]+s[20]+s[23],1)
  CV<-SAU-SH-totcf
  lab<-SAU-input$LAB
  validate(
    need(CV>=0,"Dans ce projet la surface en cultures de vente est négative!!, revoir les surfaces."),
    need(lab-CV>=0,paste("La surface en cultures",CV,"ha, est supérieure à la surface labourable déclarée de ",lab,
         "ha, dans les contraintes de surface de l'exploitation. Rétablir la cohérence avant de poursuivre."))
  )
  cvnom<-c(input$cv1,input$cv2,input$cv3,input$cv4)
  hacv<-c(s[26]*CV/100,s[31]*CV/100,s[36]*CV/100,s[41]*CV/100)
  totcv<-round((s[26]+s[31]+s[36]+s[41])*CV/100,1)
  manque <-round(SAU-SH-totcf-totcv,1)
 
  parsurf <- data.frame(labs=c("paturage","paturage et stocks",cfnom,cvnom,"manque"),
                        ha=c(pat,stoh,hacf,hacv,manque))
  par(bg="transparent")
    labs<-c("")
   nf<-sum(hacf!=0)
   nv<-sum(hacv!=0)
  if(manque>=0){

  for(i in 1:nrow(parsurf)) { 
  if(parsurf[i,2]>0) {
    labs[i]<-paste(parsurf[i,1],parsurf[i,2],"ha")
    #if (i<8) {nf=nf+1}else{nv=nv+1}
  }else{
    labs[i]<-""
  }
  }
   
 #coul<-brewer.pal(3,"Set2")
  coul<-c("green","yellowgreen",rep("orange",4),rep("yellow",4),"red")
  lp<-pie3D(parsurf$ha, main="",theta=1,labelcex=1.2,
          col=coul,explode=0.1,
          labels=labs,radius=1,labelrad=1.7)
  }else{
    plot.new()
    text(0,0.7,labels="Plus de surfaces",pos=4,col="red",cex=5)
    text(0,0.5,labels="que la SAU",pos=4,col="red",cex=5)    
  }
  
 
})


##########  rotations

nouvrot<-reactive({
    SAU<-input$SAU
    SH<-seuil()[1]
    s<-suppressWarnings(as.numeric(surfex()[,2]))
    pat<-s[2]+s[4]
    stoh<-SH-pat
    
    cfnom<-c(input$four1,input$four2,input$four3,input$four4)
    hacf<-c(s[14],s[17],s[20],s[23])
    totcf<-round(s[14]+s[17]+s[20]+s[23],1)
    CV<-SAU-SH-totcf
    validate(
      need(CV>=0,"dans ce projet la surface en cultures de vente est négative!!, revoir les surfaces.")
    )
    cvnom<-c(input$cv1,input$cv2,input$cv3,input$cv4)
    hacv<-c(s[26]*CV/100,s[31]*CV/100,s[36]*CV/100,s[41]*CV/100)
    totcv<-round((s[26]+s[31]+s[36]+s[41])*CV/100,1)
    manque <-round(SAU-SH-totcf-totcv,1)
    

      tab<-data.frame("Culture"=c("Pature","Pature et stock",cfnom,cvnom),
                      "AnPlace"=c(4,4,rep(1,length(cfnom)),rep(1,length(cvnom))),
                      "HaProjet"=c(pat,stoh,hacf,hacv),
                      "HaEnRotation"=c(0,0,rep(0,length(cfnom)),rep(0,length(cvnom))),
                      "Sole1"=c(0,0,rep(0,length(cfnom)),rep(0,length(cvnom))),
                      "Sole2"=c(0,0,rep(0,length(cfnom)),rep(0,length(cvnom))),
                      "Sole3"=c(0,0,rep(0,length(cfnom)),rep(0,length(cvnom))))
   
  return(tab)
  
})



tabrot<-reactive({  
  input$selproj
  #input$val_pro
  input$donold
   input$dup
   input$sup
 nouvas<- input$nouvrot
  projid<-isolate(capelist()[[2]])
  w<-which(projid$projid==input$selproj)
  table<-isolate(rot()[[w]])
      if(!is.null(nouvas))
      {
        if(nouvas==TRUE)
        {
          nouvrot<-nouvrot()[,1:3]
          tab<-table[,4:7]
          table<-cbind(nouvrot,tab)
          rot<-rot()
          rot[[w]]<-table
          saveRDS(rot,"data/rotations") 
          
        }
      }

  string2num <- function(f) round(as.numeric(f),1)
  cols <- c(2:6)
  table[cols] <- lapply(table[cols], string2num)

  return(table)
  
})


##############shinySky hotable

output$hotable1 <- renderHotable({

  tab<-tabrot()
  return(tab)
}, readOnly = FALSE
)

hotabrot<- reactive({
  hot.to.df(input$hotable1) # this will convert your input into a data.frame
})

hot<-reactive({
  hot<-hotabrot()
  string2num <- function(f) round(as.numeric(f),1)
  cols <- c(2:7)
  hot[cols] <- lapply(hot[cols], string2num)
  return(hot)
})

# output$verif<-renderTable({
#   input$val_pro
#   tab<-hot()
#   return(tab)
# })

################


output$tabrot<-renderTable({ 
  
  table<-hot()
  if(!is.null(table)){
   table<-table[,c(1,2,4,5,6,7)]
   hassol<-sol1()[[1]][,4]
   table$Sold1<-hassol-sol1()[[1]][,8]
   table$Sold2<-hassol-sol1()[[1]][,8]-sol2()[[1]][,8]
   table$Sold3<-hassol-sol1()[[1]][,8]-sol2()[[1]][,8]-sol3()[[1]][,8]
   
   table$Sole1<-sol1()[[1]][,8]
   table$Sole2<-sol2()[[1]][,8]
   table$Sole3<-sol3()[[1]][,8]
   
   colnames(table)<-c("Culture",'Durée',"Ha","HaS1","HaS2","HaS3","R1",
                      "R2","R3" )
  rotad<-data.frame("Culture"="Totaux","Durée"=0,"Ha"=sum(table[,3]),
                    "HaS1"=sum(table[,4]),"HaS2"=sum(table[,5]),"HaS3"=sum(table[,6]),
                    "R1"=sum(table[,7]), "R2"=sum(table[,8]),"R3"=sum(table[,9]))
  separot<-rep("",9)
   table<-rbind(table,separot,rotad)
  secol<-""
  table$S1<-""
  table$S2<-""
  table$S3<-""
  table<-table[,c(1,2,3,10,4,7,11,5,8,12,6,9)]
   table[table==0]<-""
  
    return(table)
  }else{
    return(NULL)
  }
},include.rownames = FALSE,digits=0
)


##### calcul des soles
 parasol<-function(solex)({
   
 
  sol<-solex
  if(length(sol$subsol>0))
  {
  string2num <- function(f) round(as.numeric(f),1)
  cols <- c(2:7)
  sol[cols] <- lapply(sol[cols], string2num)
  
  sol<-sol[order(sol$subsol,decreasing=TRUE),]
  sol$AnPlace[which(sol$subsol==0,)]<-0
  
  
  sol$a<-1  
  z<-100
  
  sola<-subset(sol,sol$subsol>0 & sol$HaEnRotation>0)
  
  if( nrow(sola) >1)
  {   
   sola$a[1]<-sola$subsol[1]*z
    gcdsol<-sola$a[1]
   
    for(i in 2:nrow(sola))
   {
    sola$subsol[i]<-min(sola$subsol[1]*sola$AnPlace[i]/sola$AnPlace[1],sola$HaEnRotation[i])
    sola$a[i]<-z*sola$subsol[i]
    gcdsol[i]<-gcd(gcdsol[i-1],sola$a[i]) 
   }
  }else{
    gcdsol<-1
    i<-1
  }
  
  surface_sole<-gcdsol[i]/z
  surtot<-sum(sola[,8])
  totan<-sum(sola[,2])
  nbsoles<-surtot/surface_sole
  sola<-sola[,1:8]
  sola$reste<-sola$HaEnRotation-sola$subsol
   solex$reste<-0
    w<-match(sola$Culture,solex$Culture)
    solex[w,]<-sola
  parasol<-list(solex,surtot,nbsoles,surface_sole,totan)
  return(parasol)
  }else{
    return(NULL)
  }
})


gcd <- function(x,y) {
  r <- x%%y;
  return(ifelse(r, gcd(y, r), y))
}
### rotation1
sol1<-reactive({
  input$val_pro
  solex<-hot()  
  solex$subsol<-round(as.numeric(solex$Sole1),1)
  sol<-parasol(solex) 
  return(sol)
})

output$tabsole1<-renderTable({
   sol<-sol1()[[1]]
   sol<-subset(sol,sol$subsol>0)
   sol<-sol[,c(1,2,8)]
   adtot<-data.frame("Culture"="totaux","AnPlace"=sum(sol[,2]),
                    "subsol"=sum(sol[,3]))
   separot<-rep("",3)
    sol<-rbind(sol,separot,adtot)
  colnames(sol)<-c("Cultures","Durée (ans)","Ha 1ère rotation")
  return(sol)
},include.rownames = FALSE,digits=1,align="rrcc"
)


output$textsole1<-renderText({
  sol<-sol1()
  surtot<-sol[[2]]
  nbsoles<-sol[[3]]
  surface_sole<-sol[[4]]
  totan<-sol[[5]]
  if(length(surface_sole)>0)
  {
    text<-paste("La 1ère rotation se fait sur",surtot,"ha au total, et est composée de",nbsoles,"soles de",surface_sole,
                "ha chacune. La rotation totale se déroule sur",totan,"années")
  }else{
    text<-"Rotation impossible ! essayer autre chose."
  }

  return(text)
})
##### rotation2
sol2<-reactive({
  input$val_pro
  solex<-sol1()[[1]]
  solex$HaEnRotation<-solex[,4]-solex[,8]
  solex$subsol<-round(as.numeric(solex$Sole2),1)
  sol<-parasol(solex)
  return(sol)
})

output$tabsole2<-renderTable({
  sol<-sol2()[[1]]
  sol<-subset(sol,sol$subsol>0)
  sol<-sol[,c(1,2,8)]
  adtot<-data.frame("Culture"="totaux","AnPlace"=sum(sol[,2]),"subsol"=sum(sol[,3]))
  separot<-rep("",3)
  sol<-rbind(sol,separot,adtot)
  colnames(sol)<-c("Cultures","Durée (ans)","Ha 2ème rotation")
  return(sol)
},include.rownames = FALSE,digits=1,align="rrcc"
)


output$textsole2<-renderText({
  sol<-sol2()
  surtot<-sol[[2]]
  nbsoles<-sol[[3]]
  surface_sole<-sol[[4]]
  totan<-sol[[5]]
  if(length(surface_sole)>0)
  {
    text<-paste("La 1ère rotation se fait sur",surtot,"ha au total, et est composée de",nbsoles,"soles de",surface_sole,
                "ha chacune. La rotation totale se déroule sur",totan,"années")
  }else{
    text<-"Rotation impossible ! essayer autre chose."
  }
  return(text)
})

## rotation3
sol3<-reactive({
  input$val_pro
  solex1<-sol1()[[1]]
  solex<-sol2()[[1]]
  #solex$HaEnRotation<-solex1[,4]-solex1[,8]-solex[,8]
  solex$HaEnRotation<-solex$reste
  solex$subsol<-round(as.numeric(solex$Sole3),1)
  sol<-parasol(solex)
  return(sol)
})

output$tabsole3<-renderTable({
  sol<-sol3()[[1]]
  sol<-subset(sol,sol$subsol>0)
  sol<-sol[,c(1,2,8)]
  adtot<-data.frame("Culture"="totaux","AnPlace"=sum(sol[,2]),"subsol"=sum(sol[,3]))
  separot<-rep("",3)
  sol<-rbind(sol,separot,adtot)
  colnames(sol)<-c("Cultures","Durée (ans)","Ha 3ème rotation")
  return(sol)
},include.rownames = FALSE,digits=1,align="rrcc"
)


output$textsole3<-renderText({
  sol<-sol3()
  surtot<-sol[[2]]
  nbsoles<-sol[[3]]
  surface_sole<-sol[[4]]
  totan<-sol[[5]]
  if(length(surface_sole)>0)
  {
    text<-paste("La 1ère rotation se fait sur",surtot,"ha au total, et est composée de",nbsoles,"soles de",surface_sole,
                "ha chacune. La rotation totale se déroule sur",totan,"années")
  }else{
    text<-"Rotation impossible ! essayer autre chose."
  }
  return(text)
})


############ fertilisation

fertil<-reactive({
  input$selproj
  capel<-capelist()
  projid<-capel[[2]]
   line<-which(projid$projid==input$selproj)
  ref<-refemb()
  fertil<-data.frame(matrix("",12,8))
  colnames(fertil)<-c("surf","ha","Nha","Pha","Kha","Ntot","Ptot","Ktot")
  ############ ferti herbe
  
  ficherbe<-capel[[5]][line,3]
  SH<-seuil()[1]
  refh<-ref[[1]]

    h<-which(as.character(refh[,1])==ficherbe)
    fertil[1,]<-c("Herbe",SH,refh$NH[h],refh$PH[h],refh$KH[h],refh$NH[h]*SH,refh$PH[h]*SH,refh$KH[h]*SH)
  
  s<-suppressWarnings(as.numeric(surfex()[,2]))
  ########### ferti cf
  zon_cf<-capel[[7]][line,3]
  zoncf<-zones("cf",zon_cf)
  refcf<-ref[[zoncf]]
  cfnom<-c(input$four1,input$four2,input$four3,input$four4,input$der)
  w<-0 
  for (n in 1:5)
  {
   w[n]<-which(refcf$Four==cfnom[n])
  }
  hacf<-c(s[14],s[17],s[20],s[23],s[51])
  for (i in 1:5)
  {
    if(cfnom[i]=="saisie") {fertil[i+1,]<-rep("",8)}else{
    fertil[i+1,]<-c(cfnom[i],hacf[i],refcf$N[w[i]],refcf$P[w[i]],refcf$K[w[i]],
                  refcf$N[w[i]]*hacf[i],refcf$P[w[i]]*hacf[i],refcf$K[w[i]]*hacf[i])
  }
  }
  f<-fertil[2:6,]
  fertil[7,]<-c("cultures fourragères",0,0,0,0,0,0,0)
  if(sum(as.numeric(f$ha),na.rm=true)>0){ 
  fertil[7,]<-c("Total CF",sum(as.numeric(f$ha),na.rm=TRUE),
           round(sum(as.numeric(f$ha)*as.numeric(f$Nha),na.rm=TRUE)/sum(as.numeric(f$ha),na.rm=TRUE),0),
           round(sum(as.numeric(f$ha)*as.numeric(f$Pha),na.rm=TRUE)/sum(as.numeric(f$ha),na.rm=TRUE),0),
           round(sum(as.numeric(f$ha)*as.numeric(f$Kha),na.rm=TRUE)/sum(as.numeric(f$ha),na.rm=TRUE),0),
           sum(as.numeric(f$Ntot),na.rm=TRUE),
           sum(as.numeric(f$Ptot),na.rm=TRUE),
           sum(as.numeric(f$Ktot),na.rm=TRUE))
  }
  totcf<-round(s[14]+s[17]+s[20]+s[23]+s[51],1)
  totsf<-totcf-s[51]
  
  ########## ferti cv
   zone_cv <-capel[[8]][line,3] 
   #zoncv<-zones("cv",zone_cv)
  zoncv<-4
  refcv<-ref[[zoncv]]
  
  CV<-input$SAU-seuil()[1]-totsf
  
  cvnom<-c(input$cv1,input$cv2,input$cv3,input$cv4)
  hacv<-c(s[26]*CV/100,s[31]*CV/100,s[36]*CV/100,s[41]*CV/100)
  totcv<-round((s[26]+s[31]+s[36]+s[41])*CV/100,1)
  z<-0 
  for (n in 1:4)
  {
    z[n]<-which(refcv$CV==cvnom[n])
  }
  for (i in 1:4)
  {
    if(cvnom[i]=="saisie") {fertil[i+7,]<-rep("",8)}else{
      fertil[i+7,]<-c(cvnom[i],hacv[i],refcv$N[z[i]],refcv$P[z[i]],refcv$K[z[i]],
                      refcv$N[z[i]]*hacv[i],refcv$P[z[i]]*hacv[i],refcv$K[z[i]]*hacv[i])
  }}
  
  v<-fertil[8:11,]
  fertil[12,]<-c("cultures de vente",0,0,0,0,0,0,0)
  if(sum(as.numeric(v$ha),na.rm=true)>0){
    fertil[12,]<-c("Total CV",sum(as.numeric(v$ha),na.rm=TRUE),
                   round(sum(as.numeric(v$ha)*as.numeric(v$Nha),na.rm=TRUE)/sum(as.numeric(v$ha),na.rm=TRUE),0),
                   round(sum(as.numeric(v$ha)*as.numeric(v$Pha),na.rm=TRUE)/sum(as.numeric(v$ha),na.rm=TRUE),0),
                   round(sum(as.numeric(v$ha)*as.numeric(v$Kha),na.rm=TRUE)/sum(as.numeric(v$ha),na.rm=TRUE),0),
                   sum(as.numeric(v$Ntot),na.rm=TRUE),
                   sum(as.numeric(v$Ptot),na.rm=TRUE),
                   sum(as.numeric(v$Ktot),na.rm=TRUE))
               }

  

  return(fertil)
})
 
 output$debug<-renderTable({
   p<-capelist()
   w<-listedit()
   recap<-calcusurf(p,w)
   if(length(recap)>1){recap<-recap[rownames(recap) %in% input$capel_surf,]} 
   return(recap) 
 })

output$fertih<-renderTable({
  a<-fertil()[1,]
  num2int <- function(f) as.integer(f)
  cols <- c(2:8)
  a[cols] <- lapply(a[cols], num2int)
  return(a)
},include.rownames = FALSE,digits=0
)

observeEvent(input$modifertil,{
  fertil<-fertil()
  updateNumericInput(session,"NH",value=as.numeric(fertil[1,3]))
  updateNumericInput(session,"PH",value=as.numeric(fertil[1,4]))
  updateNumericInput(session,"KH",value=as.numeric(fertil[1,5])) 
  
  updateNumericInput(session,"NCF",value=as.numeric(fertil[7,3]))
  updateNumericInput(session,"PCF",value=as.numeric(fertil[7,4]))
  updateNumericInput(session,"KCF",value=as.numeric(fertil[7,5])) 
  
  updateNumericInput(session,"NCV",value=as.numeric(fertil[12,3]))
  updateNumericInput(session,"PCV",value=as.numeric(fertil[12,4]))
  updateNumericInput(session,"KCV",value=as.numeric(fertil[12,5])) 

})


output$ferticf<-renderTable({
  a<-fertil()[2:7,]
  num2int <- function(f) as.integer(f)
  cols <- c(2:8)
  a[cols] <- lapply(a[cols], num2int)
  return(a)
},include.rownames = FALSE,digits=0
)

output$ferticv<-renderTable({
   a<- fertil()[8:12,]
  num2int <- function(f) as.integer(f)
  cols <- c(2:8)
  a[cols] <- lapply(a[cols], num2int)
  return(a)
},include.rownames = FALSE,digits=0
)






############ fin  fertil

output$achafou<-renderPlot({
  bes<-as.numeric(bestoc()[8:13,2]) 
  besani<-c(bes[1],bes[2],bes[3])
  cufou<-c(input$HaFour1*input$RdtFour1,input$HaFour2*input$RdtFour2,input$HaFour3*input$RdtFour3,
           input$HaFour4*input$RdtFour4,input$Hader*input$Rdtder)
  cfnom<-c(input$four1,input$four2,input$four3,input$four4,input$der)
  
  achafou<-c(input$Tachafour1,input$Tachafour2,input$Tachafour3)
  nachafou<-c(input$achafour1,input$achafour2,input$achafour3)
    ventefou<-input$Tventefour1
  bestock(besani,bes[5],CF=TRUE,cufou=cufou,cfnom=cfnom,AF=TRUE,achafou,nachafou,ventefou=ventefou)
})


output$consconc<-renderPlot({  
  
  surf<-surfex()[,2]
  nomcul<-c(surf[25],surf[30],surf[35],surf[40])
  intravl<-as.numeric(c(surf[29],surf[34],surf[39],surf[44]))
  intrani<-as.numeric(c(surf[28],surf[33],surf[38],surf[43]))
  ncon<-c(input$con1,input$con2,input$con3,input$con4,input$con5,input$con6,"CMV")
  tconvl<-c(input$Tconvl1,input$Tconvl2,input$Tconvl3,input$Tconvl4,input$Tconvl5,input$Tconvl6,input$minvl*input$VL/1000)
  tconani<-c(input$Tcon1-input$Tconvl1,input$Tcon2-input$Tconvl2,input$Tcon3-input$Tconvl3,
             input$Tcon4-input$Tconvl4,input$Tcon5-input$Tconvl5,input$Tcon6-input$Tconvl6,input$minugb*(input$UGB-input$VL)/1000)
  
  consconc(nbvl=input$VL,nugb=input$UGB-input$VL,nomcul=nomcul,
           intravl=intravl,g_l=input$CONVL,rdtlait=input$RDT,
           intrani=intrani,ncon=ncon,tconvl=tconvl,tconani=tconani)
})

################ typo eco
output$ecosys<-renderPlot({
  taille <- 1.2
  par(bg = "transparent", mar = c(0, 0, 0, 0), bty = "n")
    
  plot(1, 1, xlim = c(-1, 3), ylim = c(0, 3), pch = 19, cex = 0, col = "blue", xlab = "", 
       ylab = "", font.lab = 4, cex.lab = taille * 0.8, cex.axis = 1, main = "", axes = F)
  
  grid(nx = 4, ny = 3, col = "plum", lty = "dotted", lwd = 1)
  sepx1 <- 1
  sepx2 <- 2
  sepy <- 1
  gros<-1
  rect(0, 0, sepx1, sepy, col = "red", border = NA)
  rect(0, sepy, sepx1, 1.9, col = "cyan", border = NA)
  # rect(sepx2,0,3,sepy,col='orange',border=NA)
  rect(sepx1, sepy, sepx2, 1.9, col = "green", border = NA)
  rect(sepx1, 0, 3, sepy, col = "plum", border = NA)
  rect(sepx2, sepy, 3, 1.9, col = "yellow", border = NA)
  
  text(-1, 3, labels = "Cultures de vente", cex = 1.5, col = "blue", pos = 4)
  
  text(0, 2.7, labels = "Cult. vente <40ha  ", cex = gros, col = "blue", font = 2, pos = 4)
  text(0, 2.5, labels = "ou moins de", cex = gros, col = "blue", font = 2, pos = 4)
  text(0, 2.3, labels = "de 30% de la SAU", cex = gros, col = "blue", font = 2, pos = 4)
  
  text(sepx1, 3, labels = "Cult. vente > 40ha et ", cex = gros, col = "blue", font = 2, pos = 4)
  text(sepx1, 2.8, labels = "plus de 30% de la SAU", cex = gros, col = "blue", font = 2, pos = 4)
  text(sepx1, 2.6, labels = "et plus de 5000 litres ", cex = gros, col = "blue", font = 2, pos = 4)
  text(sepx1, 2.4, labels = "de lait par ha ", cex = gros, col = "blue", font = 2, pos = 4)
  text(sepx1, 2.2, labels = "de cult. de vente", cex = gros, col = "blue", font = 2, pos = 4)
  
  m <- 0.1
  text(sepx2 + m, 3, labels = "Cult. vente > 40 ha et", cex = gros, col = "blue", font = 2, 
       pos = 4)
  text(sepx2 + m, 2.8, labels = "plus de 30% de la SAU", cex = gros, col = "blue", font = 2, pos = 4)
  text(sepx2 + m, 2.6, labels = "et moins de 5000 litres", cex = gros, col = "blue", font = 2, 
       pos = 4)
  text(sepx2 + m, 2.4, labels = "de lait par ha", cex = gros, col = "blue", font = 2, 
       pos = 4)
  text(sepx2 + m, 2.2, labels = "de cult. de vente", cex = gros, col = "blue", font = 2, 
       pos = 4)
  
  
  text(-1, 2, labels = "Viande", cex = 1.5, col = "red", pos = 4)
  text(-1.1, 0.7, labels = "Plus de 5VA ou ", cex = gros, col = "red", font = 2, pos = 4)
  text(-1.2, 0.5, labels = "plus de 0.2JB/VL", cex = gros, col = "red", font = 2, pos = 4)
  
  text(-1.1, 1.6, labels = "Moins de 5VA et ", cex = gros, col = "red", font = 2, pos = 4)
  text(-1.2, 1.4, labels = "moins de 0.2JB/VL", cex = gros, col = "red", font = 2, pos = 4)
  
  text(0.1, 1.7, labels = "Lait_spec", pos = 4)
  text(1.1, 1.7, labels = "Lait_CV", pos = 4)
  text(2.1, 1.7, labels = "CV_lait", pos = 4)
  
  text(0.1, 0.7, labels = "Lait_viande", pos = 4)
  text(2, 0.7, labels = "Lait_viande_CV")
  # text(2,0.7,labels='CV_lait_viande',pos=4)
  
  
  s<-suppressWarnings(as.numeric(surfex()[,2]))
  sfp<-s[14]+s[17]+s[20]+s[23]
  cer <- input$SAU-sfp-seuil()[1]
  pcer<-100*cer/input$SAU
  laicer <- ifelse(cer>0,input$VL*input$RDT/cer, 10000)
  jbvl <- input$ENG/input$VL
  va <- input$VA
  
  x <- ifelse(cer > 40, ifelse(pcer > 30, ifelse(laicer > 5000, 1.5, 2.5), 0.5), 0.5)
  y <- ifelse(va < 5, ifelse(jbvl < 0.2, 1.5, 0.5), 0.5)
  
  points(x, y, pch = 19, cex = 3, col = "blue")
  
  
})

output$ficheco<-renderText({
  input$ficheco
})

output$rappel<-renderText({
  text<-paste(input$UGB-input$UGBL,"autres UGB que le lait dont",input$ENG,"UGB hors sols")
  return(text)
})
##############rassemblement données eco
prods<-reactive({
  ref<-refemb()
  laivendu<-input$VL*input$RDT/1000-input$autoc
  lait<-c(0,0,0)
  lait[1]<-ifelse(laivendu<=input$limA,laivendu,input$limA)
  if (laivendu>input$limA) 
    {
  lait[2]<-ifelse(laivendu <=input$limB,laivendu-input$limA,ifelse(laivendu>input$limB,input$limB-input$limA,0))
   
      }
   if(laivendu>input$limB)
   {
     lait[3]<-laivendu-input$limB 
   }
  
  if(input$limB<input$limA) {lait[2]<-lait[3]<-0}
  
  prolait<-data.frame(num=1,volA=lait[1],priA=input$priA,volB=lait[2],priB=input$priB,volC=lait[3],priC=input$priC,autoc=input$autoc)
  
  via<-data.frame(num=1,vbl=input$viabl,ugba=input$UGB-input$UGBL,kgugba=input$kgv_ugb,priv=input$priv)
  
  s<-suppressWarnings(as.numeric(surfex()[,2]))
  sfp<-s[14]+s[17]+s[20]+s[23]+seuil()[1]
  cer <- input$SAU-sfp
  recv1<-cer*s[26]*s[27]/1000;recv2<-cer*s[31]*s[32]/1000;recv3<-cer*s[36]*s[37]/1000;recv4<-cer*s[41]*s[42]/1000
  autoc1<-(s[28]+s[29]);autoc2<-(s[33]+s[34]);autoc3<-(s[38]+s[39]);autoc4<-(s[43]+s[44])
  
  cul<-data.frame(num=1,q1ven=recv1,q1aut=autoc1,pricv1=input$PriCv1,
                  q2ven=recv2,q2aut=autoc2,pricv2=input$PriCv2,
                  q3ven=recv3,q3aut=autoc3,pricv3=input$PriCv3,
                  q4ven=recv4,q4aut=autoc4,pricv4=input$PriCv4)
  
  if(input$SAU<input$ParIchn*25){ICHN<-input$SAU*input$ICHN1}
  if(input$SAU>=input$ParIchn*25 & input$SAU<input$ParIchn*50){ICHN<-25*input$ParIchn*input$ICHN1+(input$SAU-25*input$ParIchn)*input$ICHN2}
  if(input$SAU>=input$ParIchn*50){ICHN<-25*input$ParIchn*input$ICHN1+25*input$ParIchn*input$ICHN2}
  vlmont<-ref[[8]][3,2]
  vlplain<-ref[[8]][4,2]
  if(input$PrimeVL=="Montagne"){p1vl<-ifelse(input$VL<input$ParPac*40,vlmont*input$ParPac*input$VL,input$ParPac*40*vlmont)}  
  if(input$PrimeVL =="Plaine"){p1vl<-ifelse(input$VL<input$ParPac*40,vlplain*input$ParPac*input$VL,input$ParPac*40*vlplain)}  
  ######### on ne tient pas compte des patys PAC pour le pil1
  aides<-data.frame(num=1,pil1=input$SAU*input$DPB+p1vl,pil2=ICHN,otraid=input$otraid)
  otreprod<-input$otreprod
  prods<-list(prolait,via,cul,aides,otreprod)  
  return(prods)
  
})

chopi<-reactive({
  ref<-refemb()
  ugbha<-data.frame(num=1,ugb=input$UGB,ha=input$SAU)
  conach<-data.frame(num=1,tcon1=input$Tcon1,pricon1=input$Pcon1,tcon2=input$Tcon2,pricon2=input$Pcon2,
                     tcon3=input$Tcon3,pricon3=input$Pcon3,tcon4=input$Tcon4,pricon4=input$Pcon4,
                     tcon5=input$Tcon5,pricon5=input$Pcon5,tcon6=input$Tcon6,pricon6=input$Pcon6)
  conaut<-data.frame(num=1,tanicv1=input$T1ani,tvlcv1=input$T1vl,pricv1=input$PriCv1,
                     tanicv2=input$T2ani,tvlcv2=input$T2vl,pricv2=input$PriCv2,
                     tanicv3=input$T3ani,tvlcv3=input$T3vl,pricv3=input$PriCv3,
                     tanicv4=input$T4ani,tvlcv4=input$T4vl,pricv4=input$PriCv4)
  fourach<-data.frame(num=1,tfour1=input$Tachafour1,pfour1=input$Pachafour1,tfour2=input$Tachafour2,pfour2=input$Pachafour2,
                      tfour3=input$Tachafour3,pfour3=input$Pachafour3)
 
  refeco<-ref[[7]][,which(colnames(ref[[7]])==input$ficheco)]
  chopani<-data.frame(num=1,cmvugb=refeco[3],litugb=refeco[6],fel=refeco[4],veto=refeco[5],autani=refeco[7])  
   cmvalim <-(input$minvl*input$VL+input$minugb*(input$UGB-input$VL))*input$pricmv
  chopi<-list(ugbha,conach,conaut,fourach,chopani,cmvalim)
  return(chopi)
})


chopo<-reactive({
     
     input$selproj
    ref<-refemb()
     #### construction de liste de fichiers pour tableau et calcul avec chosurf() de caplot
     surf<-data.frame(num=1,ha=input$SAU,herbe=seuil()[1]) 
     #####  fichiers ferti
                cap<-capelist()[[9]]
               w<-which(cap$projid==input$selproj) 
     fert<-data.frame(num=1,NH=cap[w,5],PH=cap[w,6],KH=cap[w,7],NCF=cap[w,10],PCF=cap[w,11],KCF=cap[w,12],
                      NCV=cap[w,15],PCV=cap[w,16],KCV=cap[w,17])
      prunit<-c(N=input$priN,P=input$priP,K=input$priK)/1000
     
     ######### fichier chop des cultures fourragères
          capcf<-capelist()[[7]]
         f<-which(capcf$projid==input$selproj)
        surfcf<-data.frame(cf1=capcf[f,5],cf2=capcf[f,8],cf3=capcf[f,11],cf4=capcf[f,14],cf5=capcf[f,17])
        surfcf_horsder<-data.frame(cf1=capcf[f,5],cf2=capcf[f,8],cf3=capcf[f,11],cf4=capcf[f,14])
                #cfzon<-input$fouzon
                #zoncf<-zones("cf",input$fouzon) 
                # zoncf<-2
                refcf<-ref[[zoncf]]
                 
    liscf<-c(which(refcf$Four==capcf[f,4]),which(refcf$Four==capcf[f,7]),which(refcf$Four==capcf[f,10]),
             which(refcf$Four==capcf[f,13]),which(refcf$Four==capcf[f,16]))
     
     refsemcf<-data.frame(cf1=refcf$Sem[liscf[1]],cf2=refcf$Sem[liscf[2]],cf3=refcf$Sem[liscf[3]],cf4=refcf$Sem[liscf[4]],cf5=refcf$Sem[liscf[5]])
     rephytocf<-data.frame(cf1=refcf$Phyt[liscf[1]],cf2=refcf$Phyt[liscf[2]],cf3=refcf$Phyt[liscf[3]],cf4=refcf$Phyt[liscf[4]],cf5=refcf$Phyt[liscf[5]])
    refrecf<-data.frame(cf1=refcf$Rec[liscf[1]],cf2=refcf$Rec[liscf[2]],cf3=refcf$Rec[liscf[3]],cf4=refcf$Rec[liscf[4]],cf5=refcf$Rec[liscf[5]]) 
     refdivcf<-data.frame(cf1=refcf$Div[liscf[1]],cf2=refcf$Div[liscf[2]],cf3=refcf$Div[liscf[3]],cf4=refcf$Div[liscf[4]],cf5=refcf$Div[liscf[5]] ) 
     
     ######### fichier chop des cultures de vente
           capcv<-capelist()[[8]]
           v<-which(capcv$projid==input$selproj)
          CV<-input$SAU-seuil()[1]-sum(surfcf_horsder)
     surfcv<-data.frame(cv1=capcv[v,5]*CV/100,cv2=capcv[f,10]*CV/100,cv3=capcv[f,15]*CV/100,cv4=capcv[f,20]*CV/100)    
                #input$cvzon
                #cvzon<-input$cvzon
                #zoncv<-zones("cv",input$cvzon)
               zoncv<-4
             refcv<-ref[[zoncv]]
             liscv<-c(which(refcv$CV==capcv[v,4]),which(refcv$CV==capcv[v,9]),which(refcv$CV==capcv[v,14]),which(refcv$CV==capcv[v,19]))
      refsemcv<-data.frame(cv1=refcv$Sem[liscv[1]],cv2=refcv$Sem[liscv[2]],cv3=refcv$Sem[liscv[3]],cv4=refcv$Sem[liscv[4]])
     rephytocv<-data.frame(cv1=refcv$Phyt[liscv[1]],cf2=refcv$Phyt[liscv[2]],cv3=refcv$Phyt[liscv[3]],cv4=refcv$Phyt[liscv[4]])
     refrecv<-data.frame(cv1=refcv$Rec[liscv[1]],cv2=refcv$Rec[liscv[2]],cv3=refcv$Rec[liscv[3]],cf4=refcv$Rec[liscv[4]]) 
     refdivcv<-data.frame(cv1=refcv$Div[liscv[1]],cv2=refcv$Div[liscv[2]],cv3=refcv$Div[liscv[3]],cf4=refcv$Div[liscv[4]]) 
   
    
    semiherbe<-(seuil()[1]-input$PT)*ref[[9]]$Sem[1]+input$PT*ref[[9]]$Sem[2]
    phytherbe<-(seuil()[1]-input$PT)*ref[[9]]$Phyt[1]+input$PT*ref[[9]]$Phyt[2]
    divherbe<-(seuil()[1]-input$PT)*ref[[9]]$Div[1]+input$PT*ref[[9]]$Div[2]
    
   return(list(surf,fert,prunit,surfcf,refsemcf,rephytocf,refrecf,refdivcf,surfcv,refsemcv,
               rephytocv,refrecv,refdivcv,semiherbe,phytherbe,divherbe))
})


charec<-reactive({
  chopos<-chopo()
  chas<-chosurf(chopos[[1]],chopos[[2]],chopos[[3]],chopos[[4]],chopos[[5]],chopos[[6]],chopos[[7]],chopos[[8]],chopos[[9]],chopos[[10]],
                chopos[[11]],chopos[[12]],chopos[[13]])
  
  charec<-data.frame(t(matrix(0,4,1)))
  colnames(charec)<-c("Surface en herbe","Culture fourragères","Grandes cultures","Total")
  ##### calcul herbe
  s<-suppressWarnings(as.numeric(surfex()[,2]))
  rec_print<-s[5] 
  rec_ete<-s[9]
  haEns<-rec_print*input$PC_ens/100
  haEnr<-rec_print*input$PC_enr/100
  haFoinprec<-rec_print*input$PC_foinprec/100
  haFointard<-(1-(input$PC_ens+input$PC_enr+input$PC_foinprec)/100)*rec_print
  haReg<-rec_ete-haFointard
  
  recherbe<-haEns*input$recens+haEnr*input$recenr+(haFoinprec+haFointard+haReg)*input$recfoin
  ########
  charec[1,]<-c(recherbe,chas[[10]],chas[[11]],recherbe+chas[[10]]+chas[[11]])
  charec[2,]<-c(seuil()[1],sum(chopos[[4]]),sum(chopos[[9]]),input$SAU)
  recha<-ifelse(seuil()[1]>0,recherbe/seuil()[1])
  recfha<-ifelse(sum(chopos[[4]])>0,chas[[10]]/sum(chopos[[4]]),0)
  recvha<-ifelse(sum(chopos[[9]])>0,chas[[11]]/sum(chopos[[9]]),0)
  recsau<-(recherbe+chas[[10]]+chas[[11]])/input$SAU
  charec[3,]<-c(recha,recfha,recvha,recsau)
  return(charec)
})



chafix<-reactive({ 
  char<-charec()
  rec<-char[2,1]*input$rechah+char[2,2]*input$rechacf+char[2,3]*input$rechacv  
  sal<-input$sal
  ferm<-input$ferm
  SAU<-input$SAU
  UGB<-input$UGB
  carb<-input$carb*SAU
  entremat<-input$entremat*SAU
  entrebat<-input$entrebat*UGB
  tdep<-input$tdep*SAU
  assu<-input$assu
  impt<-input$impt
  eau<-input$eau*UGB
  edfgdf<-input$edfgdf*UGB
  fraig<-input$fraig
  div<-input$div*UGB
  otrefix<-input$otrefix
  chafix<-c(sal,ferm,rec,carb,entremat,entrebat,tdep,assu,impt,eau,edfgdf,fraig,div,otrefix)
  return(chafix)
})

revenu<-reactive({ 
prods<-prods()
prod<-prod(prods[[1]],prods[[2]],prods[[3]],prods[[4]],prods[[5]]) 
produit<-prod[[1]]
ref<-refemb()
chopis<-chopi()
chopi<-chopani(chopis[[1]],chopis[[2]],chopis[[3]],chopis[[4]],chopis[[5]])
cmvalim<-chopis[[6]]
UGB<-input$UGB
chopani<-chopi[[1]]+chopi[[2]]+chopi[[4]]+UGB*(input$cmv+input$lit+input$fel+input$veto+input$divel)+cmvalim+input$otrechani

chopos<-chopo()
chopo<-chosurf(chopos[[1]],chopos[[2]],chopos[[3]],chopos[[4]],chopos[[5]],chopos[[6]],chopos[[7]],chopos[[8]],chopos[[9]],chopos[[10]],
               chopos[[11]],chopos[[12]],chopos[[13]])
SAU<-input$SAU
chasurf<-chopo[[1]]+chopo[[2]]+chopo[[3]]+SAU*(input$sem+input$phyt+input$surdiv)+input$otrechasurf

chafix<-sum(chafix())

annu<-input$oldannu

return(c(prod[[1]], prod[[2]], prod[[3]],  prod[[4]]+prod[[5]],
         prod[[6]]+prod[[7]]+prod[[8]]  ,prod[[9]],  chopani, chasurf, chafix, annu))

})


observeEvent(input$automsa,{
  r<-revenu() 
  pblait=r[1];pbviabl=r[2];pbviabv=r[3];pbcul=r[4];aides=r[5];autresprod=r[6];chopani=r[7];chopsurf=r[8];chstruc=r[9];annu=r[10]
  pb<-as.integer(pblait+pbviabl+pbviabv+pbcul+aides+autresprod)
  msacal<-as.integer(0.25*(pblait+pbviabl+pbviabv+pbcul+aides+autresprod-chopani-chopsurf-chstruc-annu))  
  updateNumericInput(session,"msa",value=max(msacal,3500*input$TUMOE))
  })

observeEvent(input$autosec,{
  r<-revenu() 
  pblait=r[1];pbviabl=r[2];pbviabv=r[3];pbcul=r[4];aides=r[5];autresprod=r[6];chopani=r[7];chopsurf=r[8];chstruc=r[9];annu=r[10]
  pb<-as.integer(pblait+pbviabl+pbviabv+pbcul+aides+autresprod)
    updateNumericInput(session,"secautof",value=round(0.05*pb,0))
  
})

############# plots eco
output$prod<-renderPlot({  

  prods<-prods()
  prod<-prod(prods[[1]],prods[[2]],prods[[3]],prods[[4]],prods[[5]])   
  nvec<-c("Lait","Viande BL","viande BV","Vente cultures","autoc. concentrés","Aides 1er pilier",
          "Aides 2eme pilier","Autres aides","Autres produits")
barseul(titre="Produit",vec=c(prod[[1]],input$VL*input$RDT*input$viabl/1000,prod[[3]], prod[[4]],prod[[5]],prod[[6]],
                              prod[[7]],prod[[8]],prod[[9]]),nvec=nvec)
 
})

output$chop<-renderPlot({
  chopis<-chopi()
  chopi<-chopani(chopis[[1]],chopis[[2]],chopis[[3]],chopis[[4]],chopis[[5]])
  chopos<-chopo()
  
  chopo<-chosurf(chopos[[1]],chopos[[2]],chopos[[3]],chopos[[4]],chopos[[5]],chopos[[6]],chopos[[7]],chopos[[8]],chopos[[9]],chopos[[10]],
                                              chopos[[11]],chopos[[12]],chopos[[13]])
  UGB<-input$UGB
  SAU<-input$SAU
  CMV<-round(input$cmv*UGB+(input$minvl*input$VL+input$minugb*(input$UGB-input$VL))*input$pricmv,0)
  LIT<-round(input$lit*UGB,0)
  FEL<-round(input$fel*UGB,0)
  VETO<-round(input$veto*UGB,0)
  DIVEL<-round(input$divel*UGB,0)
  SEM<-round(input$sem*SAU,0)
  PHYT<-round(input$phyt*SAU,0)
  SURDIV<-round(input$surdiv*SAU,0)
             
  nvec<-c("Achat concentré","Concentré intra-cons.","CMV et AMV","Achat fourrages","Litière","Frais d'élevage","Frais véto","Autres Fel","autres charges animales",
           "Engrais","semences","Traitements","Divers","autres charges surfaces")
  barseul(titre="Charges opérationnelles",
          vec=c(chopi[[1]],chopi[[2]],CMV,chopi[[4]],LIT,FEL,VETO,DIVEL,input$otrechani,
                (chopo[[1]]+chopo[[2]]+chopo[[3]]),SEM,PHYT,SURDIV,input$otrechasurf),nvec=nvec)
})


  output$chop1000L<-renderTable({
    chopis<-chopi()
    chopi<-chopani(chopis[[1]],chopis[[2]],chopis[[3]],chopis[[4]],chopis[[5]])
    chopos<-chopo()
    
    chopo<-chosurf(chopos[[1]],chopos[[2]],chopos[[3]],chopos[[4]],chopos[[5]],chopos[[6]],chopos[[7]],chopos[[8]],chopos[[9]],chopos[[10]],
                   chopos[[11]],chopos[[12]],chopos[[13]])
    UGB<-input$UGB
    SAU<-input$SAU
    CMV<-round(input$cmv*UGB+(input$minvl*input$VL+input$minugb*(input$UGB-input$VL))*input$pricmv,0)
    LIT<-round(input$lit*UGB,0)
    FEL<-round(input$fel*UGB,0)
    VETO<-round(input$veto*UGB,0)
    DIVEL<-round(input$divel*UGB,0)
    SEM<-round(input$sem*SAU,0)
    PHYT<-round(input$phyt*SAU,0)
    SURDIV<-round(input$surdiv*SAU,0)
    nvec<-c("Concentrés","dont achat de concentré","dont concentré intra-cons.","dont CMV et AMV","Achat fourrages","Litière","Frais d'élevage","Frais véto","Autres Fel","autres charges animales",
            "Engrais","semences","Traitements","Divers","autres charges surfaces")
    vec=c(chopi[[1]]+chopi[[2]]+CMV,chopi[[1]],chopi[[2]],CMV,chopi[[4]],LIT,FEL,VETO,DIVEL,input$otrechani,
          (chopo[[1]]+chopo[[2]]+chopo[[3]]),SEM,PHYT,SURDIV,input$otrechasurf)
    laivendu<-input$VL*input$RDT/1000-input$autoc
    tab<-data.frame(t(matrix("",3,length(vec))))
    for(i in 1:length(nvec))
    {
      tab[i,]<-c(nvec[i],round(vec[i]/laivendu,0),"Euros/1000L")
    }
    colnames(tab)<-c("Charges opérationnelles"," par 1000L","")
    return(tab)
  },include.rownames = FALSE,align="rrrl"
  )


output$struc<-renderPlot({  
 
  chafix<-chafix()
  nvec<-c("Salaires","Fermages","Travaux par tiers des surfaces","Carburants lubrifiants","Entretien du matériel","Entretien des bâtiments",
          "Transport et déplacements","Assurances","Impôts et taxes","Eau","EDF GDF","Frais de gestion","Divers","Autres charges fixes")
  
  barseul(titre="Charges fixes",vec=chafix,nvec=nvec)
})

output$struc1000L<-renderTable({  
  
  chafix<-chafix()
  nvec<-c("Salaires","Fermages","Travaux par tiers des surfaces","Carburants lubrifiants","Entretien du matériel","Entretien des bâtiments",
          "Transport et déplacements","Assurances","Impôts et taxes","Eau","EDF GDF","Frais de gestion","Divers","Autres charges fixes")
  
  laivendu<-input$VL*input$RDT/1000-input$autoc
  tab<-data.frame(t(matrix("",3,length(nvec))))
  for(i in 1:length(nvec))
  {
    tab[i,]<-c(nvec[i],round(chafix[i]/laivendu,0),"Euros/1000L")
  }
  colnames(tab)<-c("Charges structurelles"," par 1000L","")
  return(tab)
  
  
},include.rownames = FALSE,align="rrrl"
)



output$rev<-renderText({ 
  r<-revenu()
  return(r)
})


output$reseco<-renderPlot({
  r<-revenu()
  reseco(pblait=r[1],pbviabl=r[2],pbviabv=r[3],pbcul=r[4],aides=r[5],otreprod=r[6],
         chopani=r[7],chopsurf=r[8],chstruc=r[9],annu=r[10]+newannu(),msa=input$msa)
  
#   return(c(prod[[1]],  prod[[2]],   prod[[3]]    ,prod[[4]]+prod[[5]],
#            prod[[6]]+prod[[7]]+prod[[8]]   ,prod[[9]],   chopani,  chasurf,   chafix,annu)) 

#prods<-list(prolait,via,cul,aides,otreprod)
# nvec<-c("Lait","Viande BL","viande BV","Vente cultures","autoc. concentrés","Aides 1er pilier",
#         "Aides 2eme pilier","Autres aides","Autres produits")
#  return(list(prodlait,prodviabl,proviabv,vencul,autocul,pil1,pil2,otraid,otreprod))

})

output$dispumo<-renderText({
  r<-revenu()
  dispo<-sum(r[1:6])-sum(r[7:10])-input$msa
  return(paste("Revenu disponible pour privé et autof. UMO exploitant avant investissement :",round(dispo/input$TUMOE,0)," Euros"))
})


output$cren<-renderText({
  r<-revenu()
  dispo<-sum(r[1:6])-sum(r[7:10])-input$msa
  return(paste("Capacité de remboursement d'annuités nouvelles :",round(dispo-input$ppautof*input$TUMOE-input$secautof,0)," Euros"))
})



newannu<-reactive({
  c<-input$montinv
  t<-input$duran
  i<-input$taux/100
  a<-c*i/(1-(1+i)^-t)
  return(a)
})

output$newannu<-renderText({
  a<-newannu()
  return(paste("Annuité nouvelle :",round(a,0),"Euros")) 
})

output$newdispumo<-renderText({
  r<-revenu()
  dispo<-sum(r[1:6])-sum(r[7:10])-input$msa
  a<-newannu()
  newdispo=(dispo-a)/input$TUMOE
  return(paste("Revenu disponible par UMO exploitant après investissement :",round(newdispo,0),"Euros")) 
})

output$perminv<-renderText({
  r<-revenu()
  dispo<-sum(r[1:6])-sum(r[7:10])-input$msa
  cren<-round(dispo-input$ppautof*input$TUMOE-input$secautof,0)
  c<-input$montinv
  t<-input$duran
  i<-input$taux/100
  inv<-cren*(1-(1+i)^-t)/i
  return(paste("capacité d'investissement aux conditions définies :",round(inv,0),"Euros")) 
})

############ matrice de risque



lait<-reactive({
  r<-revenu()
  #EBE<-sum(r[1:5])-sum(r[6:8])-input$msa
  laivendu<-input$VL*input$RDT/1000-input$autoc
  prilait<-round(r[1]/laivendu,0)
  lait<-c(laivendu,prilait)
  return(lait)
})

output$infolait<-renderText({
  lait<-lait()
  t<-paste(round(lait[1],0)," T de lait vendu à",round(lait[2],0)," Euros/T en moyenne annuelle.")
  return(t)  
})


cul<-reactive({  
  cul<-prods()[[3]]
  #saveRDS(cul,"G:/cul.rds")
  cuvol<-cul[1,2]+cul[1,5]+cul[1,8]+cul[1,11]
  if(cuvol>0){
    cupri<-(cul[1,2]*cul[1,4]+cul[1,5]*cul[1,7]+cul[1,8]*cul[1,10]+cul[1,11]*cul[1,13])/cuvol 
  }else{
    cupri<-0
  }
  cv<-c(cuvol,cupri)
  return(cv)
})


con<-reactive({
  input$Tcon1;input$Tcon2;input$T1vl;input$T1ani
  conach<-c(input$Tcon1,input$Tcon2,input$Tcon3,input$Tcon4,input$Tcon5,input$Tcon6)
  prixcon<-c(input$Pcon1,input$Pcon2,input$Pcon3,input$Pcon4,input$Pcon5,input$Pcon6)
  pricon<-round(sum(conach*prixcon)/sum(conach),0)
  conautovl<-c(input$T1vl,input$T2vl,input$T3vl,input$T4vl)
  conautani<-c(input$T1ani,input$T2ani,input$T3ani,input$T4ani)
  con<-c(sum(conach),pricon,sum(conautovl),sum(conautani))
  return(con)
})

output$infocon<-renderText({
  con<-con()
 # cmv<-(input$minvl*input$VL+input$minugb*(input$UGB-input$VL))/1000
  t<-paste(round(con[1]+con[3]+con[4],1)," T de concentré consommées par an, dont :")
  return(t)  
})

output$infoconachat<-renderText({
   con<-con()
  # cmv<-(input$minvl*input$VL+input$minugb*(input$UGB-input$VL))/1000
   pricon<-ifelse(con[2]!="NaN",round(con[2],0),0)
  t<-paste(round(con[1],1)," T de concentré acheté à",pricon," Euros/T en moyenne annuelle.")
  return(t)  
})

output$infoconauto<-renderText({
  con<-con()
  t<-paste(round(con[3]+con[4],1)," T de concentré produit dont ",round(con[3],1)," T pour les vaches, et ",round(con[4],1)," T pour les autres animaux.")
  return(t)  
})

output$varisq<-renderTable({
    r<-revenu()
  
  if(input$result==0)
  { res<-0
  }else{
    newan<-newannu()
    res<-sum(r[1:6])-sum(r[7:10])-input$msa-input$ppautof*input$TUMOE-newan
  }


   
 if(input$varisk==1)
 {
   vari<-lait()
   pri<-round(vari[2],0)
   vol<-vari[1] 
 }

 if(input$varisk==2)
 {
   vari<-cul()
   pri<-round(as.numeric(vari[2]),0)
   vol<-vari[1]
   validate(need(vol>0,"Pas de vente de cultures"))
 }
 
 
 if(input$varisk==3)
 {
   vari<-con()
   pri<-round(vari[2],0)
   vol<-vari[1]
   validate(need(vol>0,"Pas d'achats de concentré"))
 }
 
 
 varpri<-input$varpri
 npri<-input$iterpri

 varvol<-input$varvol/100
 nvol<-input$itervol
 
 
  variski(RES=res,pri=pri,varpri=varpri,npri=npri,vol=vol,varvol=varvol,nvol=nvol) 
  
},digits=0
)

var<-reactive({
varisk<-switch(input$varisk,
               "1" ="Lait",
               "2" ="Cultures de vente",
               "3" ="Aliments achetés")
               return(varisk)        
})

output$vares<-renderText({
  
  return(paste("Variation du prix et volume",var()))
  
})


############# tables eco
output$tabani<-renderTable({
  chopani<-chopi()
  chopeco<-chopani(chopani[[1]],chopani[[2]],chopani[[3]],chopani[[4]],chopani[[5]])
  achacon<-chopani[[2]]
  Tachacon=achacon[,2]+achacon[,4]+achacon[,6]+achacon[,8]+achacon[,10]+achacon[,12]
  pricon<-ifelse(Tachacon>0,chopeco[[1]]/Tachacon,0)
  intracon<-chopani[[3]]
  Tintracon<-intracon[,2]+intracon[,3]+intracon[,5]+intracon[,6]+intracon[,8]+intracon[,9]+intracon[,11]+intracon[,12]
  achafou<-chopani[[4]]
  Tachafou<-achafou[2]+achafou[4]+achafou[6]
   prifou<-ifelse(Tachafou>0,chopeco[[4]]/Tachafou,0)
  #cmv<-(input$minvl*input$VL+input$minugb*(input$UGB-input$VL))*input$pricmv/1000
 cmv<-chopani[[6]]
  tabani<-data.frame(t(matrix("",3,6)))
  tabani[1,]<-c("Quantité de concentré acheté",round(Tachacon,0),"Tonnes")
  tabani[2,]<-c("Prix moyen du concentré acheté",round(pricon,0),"Euros/T")
  tabani[3,]<-c("Quantité de concentré intra-consommé",round(Tintracon,0),"Tonnes")
  tabani[4,]<-c("Quantité de fourrages achetés",round(Tachafou,0),"Tonnes")
  tabani[5,]<-c("Prix moyen du fourrage acheté",as.integer(prifou),"Euros/T")
  tabani[6,]<-c("charge CMV alimentaires",as.integer(cmv/input$UGB),"Euros/UGB")
  
  return(tabani) 
},include.rownames = FALSE,include.colnames = FALSE
)

output$chopsurf<-renderTable({
  chopos<-chopo()
  ref<-refemb()
  chas<-chosurf(chopos[[1]],chopos[[2]],chopos[[3]],chopos[[4]],chopos[[5]],chopos[[6]],chopos[[7]],chopos[[8]],chopos[[9]],chopos[[10]],
                 chopos[[11]],chopos[[12]],chopos[[13]])
  semiherbe<-(seuil()[1]-input$PT)*ref[[9]]$Sem[1]+input$PT*ref[[9]]$Sem[2]
  phytherbe<-(seuil()[1]-input$PT)*ref[[9]]$Phyt[1]+input$PT*ref[[9]]$Phyt[2]
  divherbe<-(seuil()[1]-input$PT)*ref[[9]]$Div[1]+input$PT*ref[[9]]$Div[2]
  chops<-data.frame(t(matrix(c("",0,0,0,0),5,1)))
  colnames(chops)<-c("Charge surface","Surface en herbe","Culture fourragères","Grandes cultures","Total")
  chops[1,]<-c("Engrais",round(chas[[1]],0),round(chas[[2]],0),round(chas[[3]],0),round(chas[[1]]+chas[[2]]+chas[[3]],0))
  chops[2,]<-c("Semences",semiherbe,round(chas[[4]],0),round(chas[[5]],0),round(chas[[4]]+chas[[5]]+semiherbe,0))
  chops[3,]<-c("Phytos",phytherbe,round(chas[[6]],0),round(chas[[7]],0),round(chas[[6]]+chas[[7]]+phytherbe,0))
  chops[4,]<-c("Divers",divherbe,round(chas[[8]],0),round(chas[[9]],0),round(chas[[8]]+chas[[9]]+divherbe,0))
  chops[5,]<-c("Totaux",round(chas[[1]],0),round(chas[[2]]+chas[[4]]+chas[[6]]+chas[[8]],0),
               round(chas[[3]]+chas[[5]]+chas[[7]]+chas[[9]],0),
               round(chas[[1]]+chas[[2]]+chas[[3]]+chas[[4]]+chas[[5]]+chas[[6]]+chas[[7]]+chas[[8]]+chas[[9]],0))
  return(chops) 
},include.rownames = FALSE,align="rrcccc"
)

output$charec<-renderTable({
  charec<-charec()
  rownames(charec)<-c("Cout annuel (Euros)","Surfaces (ha)","Cout moyen par ha")
  return(charec)
},align="rcccc",digits=0
)


########### travail
output$travaref<-renderUI({
  
 lar<-paste0(input$lar*2,"%")
  action<-switch(input$refw,
                 "Traite"=img(src="travaref/traite.png",width=lar),
                 "Alimentation"=img(src="travaref/alisto.png",width=lar),
                 "Curage-paillage"=img(src="travaref/curapa.png",width=lar),
                 "Veaux"=img(src="travaref/travo.png",width=lar),
                 "Paturage"=img(src="travaref/patrav.png",width=lar),
                  "TS troupeau"=img(src="travaref/tstroupo.png",width=lar),
                  "TS fourrages"=img(src="travaref/tsfour.png",width=lar),
                  "TS grandes cultures"=img(src="travaref/tscul.png",width=lar)
                 )
  return(action)
})

trav<-reactive({
  vl<-input$VL
  ugb<-input$UGB
  veaux<-vl*0.9
  sau<-input$SAU
  hapat<-seuil()[1]
  s<-suppressWarnings(as.numeric(surfex()[,2]))
  sfp<-s[14]+s[17]+s[20]+s[23]
  hacul <- input$SAU-sfp-seuil()[1]
  traite<-input$traite
  tralim<-input$tralim
  curapa<-input$curapa
  travo<-input$travo
  trapat<-input$trapat
  tstroup<-input$troupeau
  tsfou<-input$trafou
  tscul<-input$tracul
  TAdel<-input$TAdel
  TAsal<-input$TAsal
  TAben<-input$TAben
  TSdel<-input$TSdel
  TSal<-input$TSal
  TSben<-input$TSben
  umoex<-input$TUMOE
  tradiv<-input$tradiv
  queltradiv<-input$queltradiv
  
  trav<-data.frame(vl=vl,ugb=ugb,veaux=veaux,paturage=hapat,cultures=hacul,sau=sau,traite=traite,tralim=tralim,
                   curapa=curapa,travo=travo,trapat=trapat,tstroup=tstroup,tsfou=tsfou,tscul=tscul,
                   TAdel=TAdel,TAsal=TAsal,TAben=TAben,TSdel=TSdel,TSal=TSal,TSben=TSben,umoex=umoex,tradiv=tradiv,queltradiv=queltradiv)
  return(trav)
})

output$totrav<-renderPlot({  
  t<-trav()
  
  totrav(t[1,1],t[1,2],t[1,3],t[1,4],t[1,5],t[1,6],t[1,7],t[1,8],t[1,9],t[1,10,],t[1,11],t[1,12],t[1,13],t[1,14],t[1,22],t[1,23])
  
  
  #totrav<-function(vl,ugb,veaux,hapat,hacul,sau,traite,tralim,curapa,travo,trapat,tstroup,tsfou,tscul,tradiv)
})



output$TA<-renderPlot({  
  t<-trav()
  
  reparta(t[1,1],t[1,2],t[1,3],t[1,4],t[1,5],t[1,6],t[1,7],t[1,8],
          t[1,9],t[1,10,],t[1,11],t[1,12],t[1,13],t[1,14],t[1,15],t[1,16],t[1,17],t[1,21])
  
  #(vl,ugb,veaux,hapat,hacul,sau,traite,tralim,curapa,travo,trapat,tstroup,tsfou,tscul,entrep,salar,benev,umoex)
})

output$TS<-renderPlot({  
  t<-trav()
  
  reparts(t[1,1],t[1,2],t[1,3],t[1,4],t[1,5],t[1,6],t[1,7],t[1,8],
         t[1,9],t[1,10,],t[1,11],t[1,12],t[1,13],t[1,14],t[1,18],t[1,19],t[1,20],t[1,21])
  
  #(vl,ugb,veaux,hapat,hacul,sau,traite,tralim,curapa,travo,trapat,tstroup,tsfou,tscul,entrep,salar,benev,umoex)
})


output$repartot<-renderPlot({  
  t<-trav()
  
  repartot(t[1,1],t[1,2],t[1,3],t[1,4],t[1,5],t[1,6],t[1,7],t[1,8],
          t[1,9],t[1,10,],t[1,11],t[1,12],t[1,13],t[1,14],
         t[1,15]+t[1,18],t[1,16]+t[1,19],t[1,17]+t[1,20],t[1,21],t[1,22],t[1,23])
  
  #(vl,ugb,veaux,hapat,hacul,sau,traite,tralim,curapa,travo,trapat,tstroup,tsfou,tscul,entrep,salar,benev,umoex,tradiv)
})


output$recatra<-renderTable({
  p<-trav()
   
  return(p)
  
})
############## OJO tabatoucon

##############shinySky hotable

output$hotable2 <- renderHotable({
  conatou<-conatou()
  projid<-capelist()[[2]]
  w<-which(projid$projid==input$selproj)
  if(length(conatou) >=w )
  {
      return(conatou[[w]]) 
    }else{
    return(coninit)
  }  
}, readOnly = FALSE
)

hotaconatou<- reactive({
  hot.to.df(input$hotable2) # this will convert your input into a data.frame
})

hot2<-reactive({
  hot2<-hotaconatou()
  return(hot2)
})

output$hotest<-renderTable({
  return(hot2())
})




######### tableau recap des données 
################# reports
output$projlist<-renderUI({
  cap<-capelist()[[2]]
  projlist<-cap$projid
  checkboxGroupInput("projedit",label="Choix des projets à éditer",choices=projlist,selected=projlist)
})


listedit<-reactive({
  p<-capelist()[[2]]
  if(!is.null(input$projedit))
  {
    w<-match(input$projedit,p$projid)
  }else{
    w<-p$num
  }
 
  return(w)
})


output$scenagraf<-renderUI({
  cap<-capelist()[[2]]
  n<-length(cap$projid)
  projlist<-cap$projid
  if(n>1)
  {
    wellPanel(
    downloadButton("downloadGraf","Editions de graphiques au format PDF"), 
    radioButtons("scena1",label="Choix du 1er projet",choices=projlist),
    radioButtons("scena2",label="Choix du 2ème projet",choices=projlist)
   )
  }else{
    return(NULL)
  }
  
})



output$recapex<-renderTable({
  p<-capelist()
  w<-listedit()
  recap<-calculex(p,w)
 if(length(recap)>1){
   choixex<-input$capel_ex
   recap<-recap[rownames(recap) %in% choixex,] 
 }
  return(recap)  

})



output$recapsurf<-renderTable({
  p<-capelist()
  w<-listedit()
  recap<-calcusurf(p,w)
  if(length(recap)>1){recap<-recap[rownames(recap) %in% input$capel_surf,]}
 
  return(recap)  
})



output$recapalim<-renderTable({
  p<-capelist()
  w<-listedit()
  recap<-calcalim(p,w)
  
  if(length(recap)>1){recap<-recap[rownames(recap) %in% input$capel_alim,]}
  
  return(recap)  
})



output$recaprod<-renderTable({
  p<-capelist()
  w<-listedit()
  recaprod<-calprod(p,w)
  recachop<-calchop(p,w)
  recafix<-calfix(p,w)
  recapres<-calcures(p,w)
  recap<-rbind(recaprod,recachop,recafix,recapres)
  #recap<-recapres
  if(length(recap)>1){recap<-recap[rownames(recap) %in% input$capel_prod,]} 
  return(recap)  
}
)


####### tableaux référentiel
output$ref1<-renderDataTable({
  ref<-refemb() 
  a<-ref[[1]]
  num2int <- function(f) as.integer(f)
  cols <- c(5,6,7,19,20,21)
  a[cols] <- lapply(a[cols], num2int)
  return(a)
}
)

output$ref2<-renderDataTable({
  ref<-refemb()
  a<-ref[[2]]
  num2int <- function(f) as.integer(f)
  cols <- c(3,4,5,7,11,12,13,14)
  a[cols] <- lapply(a[cols], num2int)
  return(a)
})

output$ref3<-renderDataTable({
  ref<-refemb()
  a<-ref[[3]]
  num2int <- function(f) as.integer(f)
  cols <- c(3,4,5,7,11,12,13,14)
  a[cols] <- lapply(a[cols], num2int)
  return(a)
})

output$ref4<-renderDataTable({
  ref<-refemb()
  a<-ref[[4]]
  num2int <- function(f) as.integer(f)
  cols <- c(2:10)
  a[cols] <- lapply(a[cols], num2int)
  return(a)
})

output$ref5<-renderDataTable({
 ref<-refemb()
  a<-ref[[5]]
  num2int <- function(f) as.integer(f)
  cols <- c(2,4,5)
  a[cols] <- lapply(a[cols], num2int)
  return(a)
})

output$ref6<-renderDataTable({
  ref<-refemb()
  a<-ref[[6]]
  num2int <- function(f) as.integer(f)
  cols <- c(2,4,5)
  a[cols] <- lapply(a[cols], num2int)
  return(a)
})

output$ref7<-renderDataTable({
  ref<-refemb()
  a<-ref[[7]]
  num2int <- function(f) as.integer(f)
  cols <- c(2:7)
  a[cols] <- lapply(a[cols], num2int)
  return(a)
})

output$ref8<-renderDataTable({
  ref<-refemb()
  a<-ref[[8]]
#   num2int <- function(f) as.integer(f)
#   cols <- c(2:7)
#   a[cols] <- lapply(a[cols], num2int)
  return(a)
})

output$ref9<-renderDataTable({
  ref<-refemb()
  a<-ref[[9]]
  #   num2int <- function(f) as.integer(f)
  #   cols <- c(2:7)
  #   a[cols] <- lapply(a[cols], num2int)
  return(a)
})


############# referentiel

output$refui<-renderUI({
  if(is.null(input$donold))
  {
  wellPanel(
    h4("Choix du référentiel du dossier"),
    em("Attention: Ce référentiel sera attaché au projet et ne pourra plus être changé par la suite!",
       style="color:blue;font-weight:bold;"),
    checkboxInput('refinoperso',label= em("Cocher pour utiliser un référentiel personnel, 
                                           sauvegarder le dossier puis le reprendre. "),value=FALSE),
    
    fileInput("refactu",label=em("Le référentiel personnel doit auparavant avoir été réalisé au format .ods et téléchargé ici:"),accept=".ods"))
  }else{
    farm<-newfarm()
    refutilise<-farm[[1]]
    refino<-refutilise$refino
    if(refino == FALSE)
      {h5("Utilisation du référentiel Inosys.")
    }else{
      h5("Utilisation d'un référentiel personnel.")}
    
  }
})


ref<-reactive({
  quelref<-input$refinoperso
  if(quelref ==FALSE)
  { ref<-readRDS("ref/refinosys")
  }else{
    ref<-readRDS("ref/refperso")
  }
  return(ref)
})

###### modif referentiel

observe({
  input$refactu
  if(!is.null(input$refactu))
  {
    inFile <- input$refactu
    
    if (is.null(inFile)) 
      return(NULL)
    library("readODS")
    refperso<-read.ods(inFile$datapath)
    refod<-readRDS("ref/refinosys")
    refan(refperso,refod)  
    
  }
  
})
############ save files


output$resave<- downloadHandler(
  filename = function() { paste0(input$ferme,'.csv') },
  content = function(file) {
    p<-capelist()
    recapex<-calculex(p)
    if(length(recapex)>1){recapex<-recapex[rownames(recapex) %in% input$capel_ex,]}
    recasurf<-calcusurf(p)
    if(length(recasurf)>1){recasurf<-recasurf[rownames(recasurf) %in% input$capel_surf,]}
    recalim<-calcalim(p)
    if(length(recalim)>1){recalim<-recalim[rownames(recalim) %in% input$capel_alim,]}
    recaprod<-calprod(p)
    recachop<-calchop(p)
    recafix<-calfix(p)
    recapres<-calcures(p)
    receco<-rbind(recaprod,recachop,recafix,recapres)
    if(length(receco)>1){receco<-receco[rownames(receco) %in% input$capel_prod,]}
    
    recap<-rbind(recapex,recasurf,recalim,receco)
    write.csv(recap,file)
    
  })



output$capsave<- downloadHandler(

   filename = function() { paste0(input$ferme,"_",Sys.Date(),'.rds') },
   content = function(file) {      
     capelist<-isolate(capelist()) 
     capelist[[1]]$nomex<-input$ferme
     
     if(input$refinoperso == TRUE) 
     {capelist[[1]]$refino<-input$refinoperso
      ref<-ref<- readRDS("ref/refperso")
     }else{
       ref<-refemb()
     }
     
     rot<-isolate(rot())
     con<-isolate(conatou())
     
    fichier<-list(capelist,ref,rot,con)
    fichier$version<-vers
    
    saveRDS(fichier,file)
    
   })


kelref<-reactive({
  if(input$kelref == FALSE) {ref<- readRDS("ref/refinosys")}else{ref<-readRDS("ref/refperso")}
  return(ref)
})

output$newref<- downloadHandler(
  filename = function() { paste0(input$ferme,"_",format(Sys.time(), "%A.%d%B%Y.%Hh%M"),'.rds') },
  content = function(file) {
   
    capelist<-isolate(capelist()) 
    capelist[[1]]$nomex<-input$ferme
    capelist[[1]]$refino<-input$kelref
    ref<-kelref()
    rot<-isolate(rot())
    con<-isolate(conatou())
    
    fichier<-list(capelist,ref,rot,con)
     fichier$version<-vers
    saveRDS(fichier,file)
    
  })



####### edit
# regFormula <- reactive({
#   as.formula(paste('mpg ~', input$x))
# })

output$downloadReport <- downloadHandler(
  filename = function() {
    paste('Edition', sep = '.', switch(
      input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  },
  
  content = function(file) {
    src <- normalizePath('out/rep.Rmd')
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'rep.Rmd')
    
   
    out <- render('rep.Rmd', switch(
      input$format,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()
    ))
    file.rename(out, file)
  }
)


output$raproj <- downloadHandler(
  filename = function() {
    paste('Edition', sep = '.', switch(
      input$format1, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  },
  
  content = function(file) {
    src <- normalizePath('out/raproj.Rmd')
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'raproj.Rmd')
    
    
    out <- render('raproj.Rmd', switch(
      input$format1,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()
    ))
    file.rename(out, file)
  }
)





output$downloadGraf <- downloadHandler(
  filename = function() {
   'Edition.pdf'   
  },
  
  content = function(file) {
    src <- normalizePath('out/repgraf.Rmd')
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'repgraf.Rmd')
    
    out <- render('repgraf.Rmd', pdf_document())
    file.rename(out, file)
  }
)

output$version_fichier<-renderText({
  version<-version_du_fichier()
  text<-paste("Le fichier en cours a été sauvegardé avec la version",version,"de capelR")
  return(text)
})



################fin session

    })



 
