
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

library("shiny")
library("readODS")
library('shinythemes')
library("shinysky")
library("RColorBrewer")
library("knitr")
library("plotrix")


options(stringsAsFactors=FALSE)
capel<-readRDS("data/capelist")[[2]]
sec<-5
ref<-readRDS("ref/refinosys")
resex<-c("UMO exploitant","UMO salaries","UMO benevoles","SAU","STH","PT","Ensilable",
         "Paturable VT","Dt non  recoltable","Nombre de vaches","rendement laitier","UGB","UGB estives","UGB non paturant","UGB lait","Vaches allaitantes","Lait produit")
resurf<-c("SAU","Ha grandes cult.","SFP","STH","Recoltes printemps (ha)","Recoltes ete (ha)","Ha ensilage","TMS ensilage",
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

resalim<- c("qtivl" ,    "rdtvl"  ,   "congl"  ,   "convl" ,    "pconvl"  ,  "TMStoc" ,   "TMStocf"  ,
                    "fourach1" , "tfourach1", "fourach2", "tfourach2", "fourach3" , "tfourach3" ,"con1"  ,    "tcon1"  ,   "con2",    
                    "tcon2"  ,   "con3"   ,   "tcon3"  ,   "con4"   ,  "tcon4"   ,  "con5"  ,    "tcon5" ,    "con6"  ,    "tcon6"   , 
                    "tfourach",  "tconach",   "tintrac" ,  "besani" ,   "valoherbe")
reseco<- c( "Produit total","Produit lait","Viande","Produit viande BL","Produit viande BV",
                      "Produit cultures", "Ventes cultures", "Autoconso cultures","Aides","1er Pilier","2me pilier",
                   "Autres aides","Autres produits", "Charges operationnelles","Charges animales","Achat de concentre", "Intra-consommation concentre", "Achat de fourrages",
            "CMV",     "litiere",     "Frais d'elevage",     "Frais veto",   "Divers charges animales" , "Autres charges animales" ,
            "Charges surface","Fertilisation","Semences","Phytos","Divers surfaces","Autres charges surfaces",
            "Charges de structure","Recoltes","Salaires","Fermages","Carburant-lubrifiants","Entretien du materiel",
            "Entretien des batiment","Transport et deplacements","Assurances","Impots et taxes","Eau",
            "Electricite","Frais de gestion","Divers","Autres charges fixes","MSA",
            "EBE","EBE par UMO expl.","EBE/PB","Annuites anciennes","Annuites nouvelles","Annuites","disponible","Disponible par UMO expl."
            )

show.pages<-c('paturage', 'fauche', 'cultures fourrageres','cultures','alimentation','revenu',"matrice de risques","travail","forces et faiblesses")

shinyUI(
  fluidPage(
   fluidRow(column(3,tags$input(type = "button",id = "validall" ,value="Appliquer les changements au scénario :")),
            column(3,h5(textOutput("projet"),style="color:BlueViolet;font-weight:bold;")),
            column(3,tags$input(type = "button",id = "info_projet" ,value="Description du scenario en cours")),     
            column(3,shinyalert(id="a1", click.hide = TRUE, auto.close.after = sec))),
                 
        navbarPage(selectizeInput("selproj","",choices=capel$projid,width='100px'),
                    
                    collapsible=TRUE,position = "static-top",footer =em(textOutput("version")),
                    theme = shinytheme("cerulean"),        
             tabPanel( h3(" Tableau de bord",style=("font-weight:bold;")),     
                             
             headerPanel(                         
                         tags$head(                            
                           tags$title(""),                                                
                          includeCSS("www/style.css"),
                          includeCSS("www/alert/sweet-alert.css"),
                           includeScript("www/alert/sweet-alert.js"),
                          includeScript("www/js/modif.js")

                         )),            
                                  
           fluidRow(
                
                 column(1,   
                      
                        tags$img(id="reso",src="logoreso.png", width="120px"),br(),br(),
                        tags$img(src="logoIE.png", width="100px"),br(),br(),
                         tags$img(src="logoCA.png", width="100px")

                          ), 
            
                    column(8,                         
                         
                    h2("Aide à la Décision en Système Laitier",style="color:blue;font-weight:bold;",align="center"),
                 wellPanel(
                fluidRow(
                column(4, 
                      h4(textInput("ferme",label="Exploitation :",value="KINEXISTEPA"), align="left"),

                      h5("Titre du scénario"),  
                      HTML('<textarea id="desproj" rows="2" style="width:100%"></textarea>'),
                      h5("Description du scénario"),  
                      HTML('<textarea id="DescriProj" rows="5" style="width:100%"></textarea>'),
                      
                       textInput("tec",label="Réalisé par:",value=""),
                     dateInput(inputId="datereal",label="le :", format="dd-mm-yyyy",language="fr",value=Sys.Date())),

                column(8,
                       h4("Scénarios étudiés:",align="center"),
                       tableOutput("protab"),
                      # textOutput("verif"),
                              tags$input(type = "button",id = "dup" ,value="Dupliquer un scénario",
                                         class="btn action-button btn-large btn-dup"),
                             
                              tags$input(type = "button",id = "sup" ,value="Supprimer un scénario",
                                         class="btn action-button btn-large btn-sup")
                            
                       ))
             
#      tags$script(HTML("var header = $('.navbar > .container');
#  header.append('<div style=\"position:absolute;bottom:0px;left:120px;margin:0;padding:3px 3px\"><button type=\"button\" id=\"info_projet\">Description du projet.</button> </div>');"
#                       ))
     )),
        
            column(3,
              
               wellPanel(
        
            #actionButton("recup","Reprendre le dossier précédent"),br(),br(),
               fileInput('donold', "Reprendre un dossier sauvegardé",accept='RDS')), 
              wellPanel(
               downloadButton("capsave","Sauvegarde du dossier")),
            
            uiOutput("refui"),
          
      busyIndicator("Chargement des données.")
      
            ))),    
        
     tabPanel(h3("Exploitation"),
              fluidRow(
                column(2,
                       wellPanel(style="height:560px;background-color: #EDEDED;",
                                 tags$img(src="exploit.jpg", width="150px"),
                       h3(textOutput("projex"),style="color:blue",align="left"),
                       textOutput("MO"),
                       tableOutput("recap")
                       )), 
                column(10,
               h4("Caractéristiques de l'exploitation",align="left"),               
              fluidRow(
            tabsetPanel(type = "pills",
              tabPanel("Eleveurs",
              fluidRow(
                column(3,
            h5("Main d'oeuvre de l'exploitation :"),      
            h6(numericInput("TUMOE","Nombre d'UMO exploitants",value=1,min=0),align="right"),
             h6(numericInput("TUMOS","UMO salariés",value=0,min=0),align="right"),
             h6(numericInput("TUMOB","UMU bénévoles",value=0,min=0),align="right")),
            column(2),
               column(6, 
                h5("Logement des animaux"),
                tags$textarea(id="logani", rows="2", style="width:500px;", ""),
                h5("Stockage des fourrages"),
                tags$textarea(id="stofou", rows="2", style="width:500px;", ""),
                h5("Installation de traite"),
                tags$textarea(id="instraite", rows="2", style="width:500px;", "")
                 ))),
               tabPanel("Surfaces",
                        fluidRow(
                          column(2,
                      h5("Surfaces du scénario",align="center"),   
             h6(numericInput("SAU","SAU (ha) ",value = 1,min=0),align="right"),
             h6(numericInput("LAB","Dont no labourable (ha) ",value = 0,min=0),align="right"),
             h6(numericInput("PT","Dt en prairies temporaires",value = 1,min=0),align="right")),

              column(3,
                     h2(textOutput("notasurf"),style="color:red;"),
                     h5("Surfaces en herbe entrant dans le paturage (ha)",align="center"),
                     h6(numericInput("SH","Surface en patures et patures+récoltes",value = 1,min=1),align="right"),
                     h6(numericInput("SHnorec","Surfaces en herbe non récoltables",value = 1,min=0),align="right"),
                     br(),
                     h6(numericInput("SHens","Surface ensilable - facultatif.",value = 1,min=0),align="right")),
                      
                column(3,
                       h5("Pâturage des vaches traites (ha)",align="center"), 
                     h6(numericInput("SHpatvl","Surface paturable par les vaches traites",value = 1,min=0),align="right"),
                     h6(numericInput("SHvlnorec","dont non récoltable",value = 1,min=0),align="right")),   
              column(4,
                     h5("Commentaires sur les surfaces de l'exploitation"),
                     tags$textarea(id="surfex", rows="5", style="width:300px;", ""))
               )),                   

               tabPanel("Animaux", 
                        fluidRow(
                          column(4,  
           h6(numericInput("UGB", "Nbre d'UGB annuel",  value=1,min=0),align="right"),
           h6(numericInput("UGBES", "Dont UGB estivés", value=0,min=0),align="right"),   
           h6(numericInput("ENG", "Dont UGB hors sols",  value=0,min=0),align="right"),
           h6(numericInput("UGBL", "Nbre d'UGB lait",  value=1,min=0),align="right"),
          h6(numericInput("VL", "Dont nombre de VL", value=1,min=0),align="right"),
          h6(numericInput("RDT", "Rendement laitier (litres)",  value=1,min=0), align="right"),
          h6(numericInput("VA", "Nbre de vaches allaitantes",  value=0,min=0), align="right"),
          h6("__________________________________________________________________"),
          h6(numericInput("UGB_UGBL", "Nbre d'UGB par UGBL",value=1.0,min=1.0,step=0.1), align="right"),
          h6(numericInput("UGBL_VL", "Nbre d'UGBL par VL",value=1.4,min=1.0,step=0.1), align="right")
          
          ),  
           column(3,
                  h2(textOutput("notani"),style="color:red;"),    
                  wellPanel(
                  h5("Indicateurs troupeau laitier"),
                  textOutput("spelait1"),
                  textOutput("spelait2"),
                  textOutput("spelait3")
                  
                  ),
                  wellPanel(
                    h5("Indicateurs troupeau allaitant"),
                    textOutput("speva")),
                
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          actionButton("applito","Appliquer ces taux au scénario.", class="btn btn-dup")
           ),
           column(5,
                  
                  h6("Autres animaux herbivores que laitiers "),
          tags$textarea(id="herbipat", rows="2", style="width:500px;", ""),
                  h6("Animaux herbivores non paturants: "),
          tags$textarea(id="herbihs", rows="2", style="width:500px;", " "),
                   h6("Hors-sol : "),
          tags$textarea(id="hs", rows="2", style="width:500px;", "")
          ))),

          tabPanel("Modules+",
                  tabsetPanel(type="pills", 
                              

           tabPanel("Autres infos",
                    fluidRow(
                      column(4,
                             wellPanel(style="height:100px;width:300px;background-color:#6CA6CD;",
                                selectInput("SdQ",label="Signe de qualité",
                             choices=c("Aucun","AOP","AB"),selected="Aucun"))         
                             ),
                      column(8, 
                    h5("Nombre de vache maximum pouvant être logées dans ce projet:"),
                    tags$textarea(id="maxilog", rows="2", style="width:500px;", " "),
                    h5("Nombre de vache maximum pouvant être traites dans ce projet:"),
                    tags$textarea(id="maxitraite", rows="2", style="width:500px;", " "),
                    h5("Nombre de vache maximum avec la contrainte travail :"),
                    tags$textarea(id="maxiw", rows="2", style="width:500px;", " ")
                    ))),
           tabPanel("Grilles cheptel",
                     #tableOutput('debug'),
                    h2("A FAIRE?"),
                    tags$img(src="grilleBL.PNG"),                 
                    tags$img(src="grilleBV.PNG"))))        
          ))))),

  tabPanel(h3("Surfaces"),
           
      tabsetPanel(type = "pills",
                           
    tabPanel("Pâturage",             
                     
            
             fluidRow(
               h4("Conduite des surfaces en herbe paturables",align="left"),
               column(3,     
                      wellPanel(        
                        h5("Paturage printemps:",style="color:blue",
                       numericInput("PC_vt_print",label="% des vaches traites",min=0,max=100,value=100,step=5),
                        numericInput("Surf_vt_prin","Ares pat. par VT",value=0,min=0),
                        numericInput("Surf_ugba_prin","Ares pat. autres par UGB",value=0,min=0),align="right")
                      ),
                      
                      wellPanel(
                        h5("Paturage automne:",style="color:blue",
                        numericInput("PC_vt_oto","% des vaches traites",min=0,max=100,value=100,step=5),
                        numericInput("Surf_vt_oto","Ares pat. par VT",value=0,min=0)),align="right")
                        ),
               column(3,
                      wellPanel(
                        h5("Paturage été:",style="color:blue",
                        numericInput("PC_vt_ete","% des vaches traites",min=0,max=100,value=100,step=5),
                        numericInput("Surf_vt_ete","Ares pat. par VT",value=0,min=0),
                        numericInput("Surf_ugba_ete","Ares pat. autres UGB",value=0,min=0)),align="right"),
                       h4("* VT = vache traite")
                      ),
               column(6,     
                      plotOutput("plot_pat"),
                      h5("Commentaires paturage :"),
                      tags$textarea(id="ComPat", rows="2",style="width:100%;", "")
                      
                      )),
                      h5("Tableau de synthèse des surfaces paturées et vérification des cohérences si une fiche
                        est utilisée pour les récoltes d'herbe:",textOutput("infoherbe"),style="color:blue"),
                       tableOutput("SurfHerbe")
                 ),   
    
    tabPanel("Récoltes sur les surfaces paturées.",
           fluidRow( 
             column(2,
                    h6("Selectionner une fiche Récolte d'herbe:"),
            
             em(selectInput("refou","",choices=c(as.character(ref[[1]]$Code)))),
             
             actionButton("utilipat","Appliquer la fiche", class="btn action-button btn-large btn-dup"),
             br(),br(),
                    h5(textOutput("ficheherbe"),style="color:green")),
             
             column(2, 
                         
               
                  h3("1ère période:",style="color:red;"),     
                  h6("Renseigner les % de chacun des types de récolte ci-dessous sur la surface totale fauchée sur la 1ère période, ainsi que les rendements en TMS/ha",style="color:blue;"),
                  wellPanel(
                  numericInput("PC_ens",h5("% d'ensilage"),min=0,max=100,value=0,step=5),
                  numericInput("RDT_ens","TMS/ha :",value=0,min=0)),
                  wellPanel(
                  numericInput("PC_enr",h5("% d'enrubannage"),min=0,max=100,value=0,step=5),
                  numericInput("RDT_enr","TMS/ha",value=0,min=0)),
                  wellPanel(
                  numericInput("PC_foinprec",h5("% foin 1C-1P*."),min=0,max=100,value=0,step=5),
                  numericInput("RDT_foinprec","TMS/ha",value=0,min=0)),
                  em("*foin 1C-1P = Foin récolté en 1ère coupe sur la 1ère période",style="color:blue;")
                         ),
                  
                   column(2,
                          h3("2ème période:",style="color:red;"), 
                       wellPanel(                  
                         h5(verbatimTextOutput("PC1C"),style="color:red;font-size:20px;"),
                         numericInput("RDT_fointard","TMS/ha",value=0,min=0)
                         ),
                        wellPanel(
                          
                          h5("2emes coupes:"),    
                          numericInput("RDT_2C","TMS/ha",value=0,min=0)),
                       h3("3ème période ou autres:",style="color:red;"), 
                       wellPanel(
      
                         tags$textarea(id="typerecot", rows="1", style="width:100%;", "Préciser le type de récolte ici."),
                         numericInput("msrecot","TMS/an.",value=0,min=0))
                       
                        ),
                column(6,
                       h5("Gestion des récoltes d'herbe sur les surfaces paturées",align="center"), 
                     fluidRow(  
                       column(8,
                              
                     plotOutput("RecHerbe",height="500px"),
                     h5("Commentaires récoltes herbagères"),
                     tags$textarea(id="ComRecHerbe", rows="2", style="width:80%;", "")
                     ),
                       column(3,
                     plotOutput("Stocherbe",height="500px")))))),
    
    tabPanel("Besoins des animaux",
             h5("Besoins des animaux en fourrages récoltés",align="center"),                     
             fluidRow(
               column(3,
    #h5("Fourrages et concentrés distribués vaches laitières",style="color:blue"), 
    
wellPanel(fluidRow(column(6,numericInput("jours_print","Nombre de jours printemps",value=60,min=0)),
          column(6,numericInput("kgms_print","KgMs four. distribués/vl/jour",value=0,min=0)))),

wellPanel(fluidRow(column(6,numericInput("jours_ete","Nombre de jours période été",value=60,min=0)),
         column(6,numericInput("kgms_ete","KgMs four. distribués/vl/jour",value=0,min=0)))),

wellPanel(fluidRow(column(6,numericInput("jours_aut","Nombre de jours automne",value=60,min=0)),
         column(6,numericInput("kgms_aut","KgMs four. distribués/vl/jour",value=0,min=0)))),

wellPanel(numericInput("kgms_hiv","KgMs/vl/jour four. distribué en hiver",value=0,min=0))),
column(3,
wellPanel(
h6(numericInput("CONVL"," Conc. +CMV (g/l) distribué aux VL/an", value=200,min=0),align="left")),
wellPanel(
h6("Fourrages distibué aux autres UGB paturants (TMS/ugb/an)",style="color:blue",
numericInput("Tms_ugbp","",value=3,min=0),align="right"),
h5("Fourrages distribués aux UGB hors-sols",style="color:blue",
numericInput("Tms_ugbe","",value=5,min=0,step=0.1),align="right")),
wellPanel(
h5("Pertes et refus des fourrage stockés (%)",style="color:blue",numericInput("PC_pertes","",value=5,min=0),align="right")
                      )),
              column(6,
       plotOutput("bestot",height="500px"),
       h5("Commentaires besoin des animaux :"),
       tags$textarea(id="ComBesani", rows="2", style="width:100%;", "")
       
       ))),
                
    tabPanel("Surfaces fourragères uniquement récoltées.",
         fluidRow(
          actionButton("utilicf","Appliquer les rendements du référentiel.", 
                       class="btn action-button btn-large btn-dup")),
             fluidRow( 
               column(3,
                      wellPanel(
          
             h5(selectInput("four1","Fourrages 1",choices=as.character(ref[[2]][,1]))), 
            h5(numericInput("HaFour1","Surface (ha)",value=0,min=0),align="right"),
               h5(numericInput("RdtFour1","Rendt. (TMS/ha)",value=0,min=0),align="right")
                   ),
                     wellPanel(
               h5(selectInput("four3","Fourrages 3",choices=as.character(ref[[2]][,1]))),
               h6(numericInput("HaFour3","Surface (ha)",value=0,min=0),align="right"),
               h6(numericInput("RdtFour3","Rendt. (TMS/ha)",value=0,min=0),align="right"))
                 ),
               column(3,
                     wellPanel(
                    h5(selectInput("four2","Fourrages 2",choices=as.character(ref[[2]][,1]))),
                     h6(numericInput("HaFour2","Surface (ha)",value=0,min=0),align="right"),
                     h6(numericInput("RdtFour2","Rendt. (TMS/ha)",value=0,min=0),align="right") 
                     ),
                  
                     wellPanel(
                             h5(selectInput("four4","Fourrages 4",choices=as.character(ref[[2]][,1]))),
                               h6(numericInput("HaFour4","Surface (ha)",value=0,min=0),align="right"),
                               h6(numericInput("RdtFour4","Rendt. (TMS/ha)",value=0,min=0),align="right")       
                               
                               ),
                 wellPanel(
                h5(selectInput("der","Dérobées",choices=as.character(ref[[2]][,1]))),
                h6(numericInput("Hader","Surface (ha)",value=0,min=0),align="right"),
                h6(numericInput("Rdtder","Rendt. (TMS/ha)",value=0,min=0),align="right"))),    
              
                 column(6,
                    plotOutput("cufou",height="500px"),
                    h5("Commentaires fourrages stocks:"),
                    tags$textarea(id="ComCF", rows="2", style="width:100%;", "") 
                  ))),
    
    tabPanel("Grandes cultures",

             fluidRow(
               actionButton("utilicv","Appliquer les rendements et prix du référentiel.", 
                            class="btn action-button btn-large btn-dup")),
               
             fluidRow(
               column(4,
                      wellPanel(
                                h5(selectInput("cv1","",choices=as.character(ref[[4]][,1]))),
                      fluidRow(column(4,numericInput("PCv1","% surf. GCU",value=0,min=0,max=100)),
                                column(4,numericInput("RdtCv1","Rend. Qx/ha",value=0,min=0)),
                                 column(4,numericInput("PriCv1","Prix Euros/T",value=0,min=0))
                                 ),                                
                      fluidRow(column(6,numericInput("T1vl","Distribué aux VL (T/an)",value=0,min=0)),
                                column(6,numericInput("T1ani"," + autres UGB (T/an)",value=0,min=0))
                                )),

                      wellPanel(
                                h5(selectInput("cv2","",choices=as.character(ref[[4]][,1]))),
                                fluidRow(column(4,numericInput("PCv2","% surf. GCU",value=0,min=0,max=100)),
                                         column(4,numericInput("RdtCv2","Rend. Qx/ha",value=0,min=0)),
                                         column(4,numericInput("PriCv2","Prix Euros/T",value=0,min=0))
                                ),                                
                                fluidRow(column(6,numericInput("T2vl","Distribué aux VL (T/an)",value=0,min=0)),
                                         column(6,numericInput("T2ani"," + autres UGB (T/an)",value=0,min=0))
                                )),
        
                      wellPanel(
                                h5(selectInput("cv3","",choices=as.character(ref[[4]][,1]))),
                                fluidRow(column(4,numericInput("PCv3","% surf. GCU",value=0,min=0,max=100)),
                                         column(4,numericInput("RdtCv3","Rend. Qx/ha",value=0,min=0)),
                                         column(4,numericInput("PriCv3","Prix Euros/T",value=0,min=0))
                                ),                                
                                fluidRow(column(6,numericInput("T3vl","Distribué aux VL (T/an)",value=0,min=0)),
                                         column(6,numericInput("T3ani"," + autres UGB (T/an)",value=0,min=0))
                                )),

                      wellPanel(
                                h5(selectInput("cv4","",choices=as.character(ref[[4]][,1]))),
                                fluidRow(column(4,numericInput("PCv4","% surf. GCU",value=0,min=0,max=100)),
                                         column(4,numericInput("RdtCv4","Rend. Qx/ha",value=0,min=0)),
                                         column(4,numericInput("PriCv4","Prix Euros/T",value=0,min=0))
                                ),                                
                                fluidRow(column(6,numericInput("T4vl","Distribué aux VL (T/an)",value=0,min=0)),
                                         column(6,numericInput("T4ani"," + autres UGB (T/an)",value=0,min=0))
                                ))),
                        
                        column(8,
                     plotOutput("assol",width="800px"),
             
                     plotOutput("procul",height="500px"),
                     h5("Commentaires cultures de vente :"),
                     tags$textarea(id="ComCV", rows="2", style="width:100%;", "")
                     )
               )),

     # tabPanel("Assolement"
              # ,plotOutput("assol",height="600px",width="1000px")
              # ),
tabPanel("Fertilisation",
         fluidRow(
           column(6,
                  h5("Référentie fertilisation minérale pour sur les surfaces paturées (herbe):",style="color:green"),
                  tableOutput("fertih"),
                  h5("Référentiel fertilisation minérale pour les cultures fourragères (CF):",style="color:green"),
                  tableOutput("ferticf"),
                  h5("Référentiel fertilisation minérale pour les grandes cultures (CV):",style="color:green"),
                  tableOutput("ferticv")
                  
                  ),
           column(4,
             actionButton("modifertil","Appliquer le référentiel pour la fertilisation. ",
                          class="btn btn-lg btn-dup"),br(),br(),
             h4("Dernière ligne des tableaux de gauche."),
              h4("Sinon renseigner manuellement les cases ci-dessous."),
             br(),
             wellPanel(fluidRow(column(4,numericInput("NH","N/ha herbe",value=0,min=0)),
                                 column(4,numericInput("PH","P/ha herbe",value=0,min=0)),
                                  column(4,numericInput("KH","K/ha herbe",value=0,min=0)))),
             wellPanel(fluidRow(column(4,numericInput("NCF","N/ha de CF",value=0,min=0)),
                                 column(4,numericInput("PCF","P/ha  de CF",value=0,min=0)),
                                column(4,numericInput("KCF","K/ha de CF",value=0,min=0)))),
             wellPanel(fluidRow(column(4,numericInput("NCV","N/ha de CV",value=0,min=0)),
                                column(4,numericInput("PCV","P/ha  de CV",value=0,min=0)),
                                column(4,numericInput("KCV","K/ha de CV",value=0,min=0)))),
             h5("Commentaires fertilisation :"),
             tags$textarea(id="ComFerti", rows="2", style="width:100%;", "") 
             )),
           column(2)),


tabPanel("Modules+",
         tabsetPanel(type="pills",   
                     tabPanel("Rotations",
         fluidRow(column(7,
                         tags$input(type = "button",id = "refairot",value="Importer un nouvel assolement",
                                    class="btn action-button btn-large btn-warning"),
                         br(),br(),
                         hotable("hotable1"),
                         h5("Mettre le nombre d'ha sur la tête d'assolement et 1 sur les autres cultures de la rotation.",
                            style=('color:red;font-weight:bold;')), br(),
                         h4("Tableau de bord des rotations."),
                         tableOutput("tabrot")),
                  #tableOutput("verif")),
                  column(5,
                         h4("1ère rotation:"),  
                         textOutput("textsole1"),tableOutput("tabsole1"),
                         h4("2ème rotation"),
                         textOutput("textsole2"),tableOutput("tabsole2"),
                         h4("3ème rotation"),
                         textOutput("textsole3"),tableOutput("tabsole3"),
                         h5("Commentaires rotations"),
                         tags$textarea(id="ComRot", rows="2", style="width:100%;", "")
                         )))))

              
           )
,busyIndicator("Chargement des données.") 

), 

     tabPanel(h3("Alimentation"),
      
              tabsetPanel(type="pills",

                tabPanel("Fourrages",
                         
                         fluidRow(
                           #column(2,
                            # h5(tags$input(type = "button",id = "valachafou" ,value="Enregistrer"), align="left")),                    
                         
                           column(6),
                             #h3(textOutput("projachafou"),style="color:blue")),
                           column(6,
                                  h5("Achat-vente de fourrages",align="left"))),
                         fluidRow(
                           column(6, 
                               h5("Achats de fourrages",style="color:blue"),
                                 
                               fluidRow(
                                 column(4,
                                        wellPanel(   
                                        selectInput("achafour1",h5(""),choices=ref[[5]][,1]),
                                        numericInput("Tachafour1","Quantité TMS",value=0,min=0),
                                        numericInput("Pachafour1","à Euros /TMS",value=0,min=0),
                                        checkboxInput("refoupri1","Prix du référentiel"))
                                        ),
                               column(4,
                                      wellPanel( 
                                      selectInput("achafour2",h5(""),choices=ref[[5]][,1]),
                                      numericInput("Tachafour2","Quantité TMS",value=0,min=0),
                                      numericInput("Pachafour2","à Euros /TMS",value=0,min=0),
                                      checkboxInput("refoupri2","Prix du référentiel"))
                                      ),
                               column(4,
                                      wellPanel(  
                                      selectInput("achafour3",h5(""),choices=ref[[5]][,1]),
                                      numericInput("Tachafour3","Quantité TMS",value=0,min=0),
                                      numericInput("Pachafour3","à Euros /TMS",value=0,min=0),
                                      checkboxInput("refoupri3","Prix du référentiel"))
                                      )),      

                             h5("Vente de fourrages",style="color:blue"),
                             wellPanel(  
                               numericInput("Tventefour1","Quantité de fourrages vendu par an (TMS):",value=0,min=0),
                               numericInput("Pventefour1","Prix de vente (Euros/TMS):",value=0,min=0))),
                            column(6,
                         plotOutput("achafou",height="500px")
                             ))),
                tabPanel("Concentrés",
                         fluidRow(                  
                           column(6),
                                 # h3(textOutput("projcon"),style="color:blue")),
                           column(6,
                                  h5("Consommation de concentrés",align="left"))),
                         fluidRow(
                           column(6,
                                  h5("Compléments minéraux et vitamines",style="color:blue"),
                                  fluidRow(column(4,
                                  numericInput("minvl","Kg de cmv/an/vl:",value=0,min=0)
                                  ),
                                  column(4,numericInput("minugb","Autres ugb",value=0,min=0)
                                  ), 
                                  column(4,numericInput("pricmv","Prix du cmv (Euros/kg)",value=0,min=0))
                                  ),
                                  h5("Achats de concentrés en tonnes par an:",style="color:blue"),
                                  fluidRow(
                                    column(4,
                                           wellPanel(   
                                             selectInput("con1",h5(""),choices=ref[[6]][,1],selected="cereales_achats"),
                                             h6(numericInput("Tcon1","Tonnes/an",value=0,min=0)),
                                             h6(numericInput("Tconvl1","Dt VL (T)",value=0,min=0)),
                                             h6(numericInput("Pcon1","Euros/T",value=0,min=0)),
                                             checkboxInput("refcon1","Prix du référentiel"))
                                             ),
                                    column(4,
                                           wellPanel( 
                                             selectInput("con2",h5(""),choices=ref[[6]][,1],selected="conc_equilibre"),
                                             h6(numericInput("Tcon2","Tonnes/an",value=0,min=0)),
                                             h6(numericInput("Tconvl2","Dt VL (T)",value=0,min=0)),
                                             h6(numericInput("Pcon2","Euros/T",value=0,min=0)),
                                             checkboxInput("refcon2","Prix du référentiel"))
                                             ),
                                    column(4,
                                           wellPanel(  
                                             selectInput("con3",h5(""),choices=ref[[6]][,1],selected="correcteur_azote"),
                                             h6(numericInput("Tcon3","Tonnes/an",value=0,min=0)),
                                             h6(numericInput("Tconvl3","Dt VL (T)",value=0,min=0)),
                                             h6(numericInput("Pcon3","Euros/T",value=0,min=0)),
                                             checkboxInput("refcon3","Prix du référentiel"))     
                                           )),

                                  fluidRow(
                                    column(4,
                                           wellPanel(   
                                             selectInput("con4",h5(""),choices=ref[[6]][,1],selected="saisie"),
                                             h6(numericInput("Tcon4","Tonnes/an",value=0,min=0)),
                                             h6(numericInput("Tconvl4","Dt VL (T)",value=0,min=0)),
                                             h6(numericInput("Pcon4","Euros/T",value=0,min=0)),
                                             checkboxInput("refcon4","Prix référentiel"))
                                             ),
                                    column(4,
                                           wellPanel( 
                                             selectInput("con5",h5(""),choices=ref[[6]][,1],selected="saisie"),
                                             h6(numericInput("Tcon5","Tonnes/an",value=0,min=0)),
                                             h6(numericInput("Tconvl5","Dt VL (T)",value=0,min=0)),
                                             h6(numericInput("Pcon5","Euros/T",value=0,min=0)),
                                             checkboxInput("refcon5","Prix référentiel"))
                                             ),
                                    column(4,
                                           wellPanel(  
                                             selectInput("con6",h5(""),choices=ref[[6]][,1],selected="saisie"),
                                             h6(numericInput("Tcon6","Tonnes/an",value=0,min=0)),
                                             h6(numericInput("Tconvl6","Dt VL (T)",value=0,min=0)),
                                             h6(numericInput("Pcon6","Euros/T",value=0,min=0)),
                                             checkboxInput("refcon6","Prix référentiel"))
                                             ))),
                           column(6,
                                  textOutput("infocon"),
                                  textOutput("infoconachat"),
                                  textOutput("infoconauto"),
                                  plotOutput("consconc",height="700px")))),
                
                tabPanel("Récapitulatif",
                         tableOutput("besvl")))
,busyIndicator("Chargement des données.") 
),
                                            
                 
   
tabPanel(h3("Economie"),
         fluidRow(
           column(2,
                  wellPanel(style="height:1000px;background-color:#EDEDED;",    
                            h3(textOutput("projeco"),style="color:blue;"),
                            h5("Fiche utilisée dans ce projet, pour les références économiques:",style="color:blue"),
                            h4(textOutput("ficheco"),style="color:green"),
                             h5("Rappels:",textOutput("rappel"),style="color:blue"))),                            
                    
           column(10, 
         tabsetPanel(type="pills",
           tabPanel("References système",
          fluidRow(column(3, br(),br(), wellPanel(style="height:300px;background-color:cornsilk1;", 
          radioButtons ("ficheco",label=h4("Choix du système d'exploitation:"),choices=c("Lait_spe"="SPP",
                                                                                           "Lait_CV"="PEL_SP",
                                                                                           "Lait_viande"="SPP_BV",
                                                                                          "Montagne de l'Est "="SPM_EST",
                                                                                           "Autres montagne herbagère"="SPM_MC",
                                                                                           "Montagne avec maïs"="SPM_M")
                        ,inline=FALSE))),
                       column(9,br(),br(), em("A partir des renseignements structures, classement automatique dans le graphique ci-dessous"),
                        plotOutput("ecosys",width="700px",height="600px")))), 

            tabPanel("Produits",                    
                   fluidRow(
                     column(8,       
                    h4("Produit lait"),
                    wellPanel(
                      h6(numericInput("autoc","Lait consommé sur la ferme (1000L)",min=0,value=0),align="left"), 
                    fluidRow(column(4,numericInput("limA","LaitA plafond (1000L)",value=300,min=0),
                    numericInput("priA","PrixA (Euros/1000L)",value=300,min=0)),
                    column(4,numericInput("limB","LaitB plafond (1000L)",value=1000,min=0),
                           numericInput("priB","Prix B (Euros/1000L)",value=200,min=0)),
                    column(4,br(),br(),numericInput("priC","Prix C (Euros/1000L)",value=100,min=0))),
                    textOutput("infolait")),   
                    h4("Produit viande"),
                      wellPanel(
                    fluidRow(column(4, numericInput("viabl","Viande issue de l'atelier lait (Euros/1000L)",min=0,value=50)),
                      column(4,numericInput("kgv_ugb","Autres viandes (Kilos vifs/UGB)",min=0,value=300)),
                       column(4,numericInput("priv","Prix autres viandes (Euros/kg vif)",min=0,value=2)
                       ))),
                    h4("Produit culture",
                       wellPanel(h5("Pour les détails ou modifier ---> onglet Grandes cultures dans les surfaces."))),
                    h4("Aides",
                       wellPanel(
                      h5("Aides du 1er pilier:"),
                      fluidRow(column(4, numericInput("ParPac","Parts PAC",min=0,value=1)),   
                      column(4,numericInput("DPB","DPB (Euros/ha)",min=0,value=270)),
                       column(4,radioButtons ("PrimeVL",label="Prime VL",choices=c("Montagne","Plaine"),selected="Montagne",inline=TRUE))),
                      h5("Aides du 2eme pilier:"),
                      fluidRow(column(4, numericInput("ParIchn","Parts ICHN",min=0,value=0)),
                        column(4,numericInput("ICHN1","ICHN1 (Euros/ha)",min=0,value=250)),
                        column(4,numericInput("ICHN2","ICHN2 (Euros/ha)",min=0,value=120)),
                        numericInput("otraid","Autres aides (Euros/an)",min=0,value=0))),
                      wellPanel(
                      numericInput("otreprod","Autres produits (Euros/an)",min=0,value=0),
                      h5("Remarques sur les produits du projet:"),
                      HTML('<textarea id="memotprod" rows="2" cols="60"></textarea>')))),
                    
                    column(4,
                           plotOutput("prod",height="1000px")))),

           tabPanel("Charges variables",

                    fluidRow(
                      column(6,        
                            h4("Charges animales"),
                    wellPanel(
                      tableOutput("tabani"),
                      #checkboxInput("refani","Accepter le référentiel pour les charges suivantes:"),
              fluidRow(column(12, actionButton("refani","Appliquer le référentiel. ",class="btn btn-lg btn-dup"))), 
                      h5( em("* Additifs en plus des cmv comptés dans l'alimentation"),
                        numericInput("cmv","AMV* (Euros/UGB)",min=0,value=0),
                        
                         numericInput("lit","Litière (Euros/UGB)",min=0,value=0),
                         numericInput("veto","Frais veto (Euros/UGB)",min=0,value=0),
                         numericInput("fel","Frais d'elevage (Euros/UGB)",min=0,value=0),
                         numericInput("divel","Divers elevage(Euros/ugb)",min=0,value=0),          
                      numericInput("otrechani","Autres charges animales (Euros/an)",min=0,value=0),align="right"),
                      h5("Remarques sur les charges animales du projet:"),
                      HTML('<textarea id="memotani" rows="2" cols="60">memo autres charges animales</textarea>')),
                        h4("Charges surfaces"),
                     wellPanel(
                       fluidRow(column(3,checkboxInput("referti","Prix engrais du référentiel")),
                               column(3,numericInput("priN","Prix N/T",min=0,value=0)),
                                 column(3,numericInput("priP","Prix P/T",min=0,value=0)),
                                 column(3,numericInput("priK","Prix K/T",min=0,value=0))), 
                       h4("Détail des charges variables surfaces (Euros/an"),
                       tableOutput("chopsurf"),
                       #checkboxInput("refinsurf","Accepter le référentiel (tableau) pour les charges suivantes:"),
                       fluidRow(column(12, actionButton("refinsurf","Appliquer le référentiel. ",class="btn btn-lg btn-dup"))),           
                       h5(numericInput("sem","semences (Euros/ha SAU)",min=0,value=0),
                          numericInput("phyt","Phytos (Euros/ha SAU)",min=0,value=0),
                          numericInput("surdiv","Frais divers surfaces (Euros/ha SAU)",min=0,value=0),    
                       numericInput("otrechasurf","Autres charges surfaces (Euros/an)",min=0,value=0),align="right"),
                       h5("Remarques sur les charges surfaces du projet:"),
                       HTML('<textarea id="memochas" rows="2" cols="60">memo autres charges surfaces</textarea>'))
                          ),
                     
                    column(6,
                           plotOutput("chop",height="800px"),
                           fluidRow(column(2),
                                    column(10,
                           tableOutput("chop1000L")))
                           ))),

          tabPanel("Charges fixes",
                   
                   fluidRow(
                     column(6,        
                            h4("Travaux par tiers des surfaces"),
                            wellPanel(
                              h5("Surfaces en herbe:"),
                              fluidRow(column(3,checkboxInput("refrec","Prix référentiel (Euros/ha)")),
                                column(3,numericInput("recens","Ensilage",min=0,value=0)),
                                column(3,numericInput("recenr","Enrubannage",min=0,value=0)),
                                column(3,numericInput("recfoin","Foin",min=0,value=0))),
                              h5("Calculs avec les éléments économiques du référentiel surfaces :"),
                            tableOutput("charec"),
                         #checkboxInput("modifrec","Cocher pour accepter, sinon le rentrer manuellement puis valider.",value=FALSE),
              
                         fluidRow(column(12, actionButton("modifrec","Appliquer les résultats du tableau. ",class="btn btn-lg btn-dup"))), 
                         
                         
                         fluidRow(column(4,numericInput("rechah",label="Euros/ha d'herbe total",value=0,min=0)),
                                     column(4,numericInput("rechacf",label="Euros/ha de cult. four.",value=0,min=0)),
                                     column(4,numericInput("rechacv",label="Euros/ha de gdes cult.",value=0,min=0))) 
                            
                                 ),            

                         h4("Autres charges structurelles"),
                            wellPanel(
                              fluidRow(column(4, numericInput("sal","Salaires (Euros/an)",min=0,value=0)),
                                      column(4, numericInput("ferm","Fermages (Euros/an)",min=0,value=0))   
                                       ),
                              
                            #checkboxInput("refstruc","Utiliser le référentiel pour les charges suivantes:"),
                fluidRow(column(12, actionButton("refstruc","Appliquer le référentiel pour les charges suivantes: ",class="btn btn-lg btn-dup"))),        
                            h5(numericInput("carb","Carburants et lubrifiants (Euros/ha SAU)",min=0,value=0),
                            numericInput("entremat","Entretien et petit matériel (Euros/ha SAU)",min=0,value=0),
                            numericInput("entrebat","Entretien batiments (Euros/UGB)",min=0,value=0),
                            numericInput("tdep","Transports et déplacements (Euros/ha SAU)",min=0,value=0),
                            numericInput("assu","Assurances (Euros/an)",min=0,value=0),
                            numericInput("impt","Impots et taxes (Euros)",min=0,value=0),
                            numericInput("eau","Eau (Euros/UGB)",min=0,value=0),
                            numericInput("edfgdf","EDF GDF (Euros/UGB)",min=0,value=0),
                            numericInput("fraig","Frais de gestion (Euros/an)",min=0,value=0),
                            numericInput("div","Autres charges de struct. diverses (Euros/UGB)",min=0,value=0),
                            align="right")),
                           wellPanel(
                             numericInput("otrefix","Autres charges fixes (Euros/an)",min=0,value=0),
                             h5("Remarques sur les charges fixes du projet:"),
                             HTML('<textarea id="memofix" rows="2" cols="60"></textarea>'))),
                     column(6,plotOutput("struc",height="800px"),
                            fluidRow(column(2),
                                     column(10,
                                            tableOutput("struc1000L")))
                                    ))),
          
          
           tabPanel("Revenu et CREN",
                  
                    fluidRow(column(4,
                                    wellPanel(            
                       numericInput("oldannu","Annuités (Euros/an)",min=0,max=100,step=1,value=10),
                         numericInput("msa","MSA",min=0,value=0),
                  actionButton("automsa","Calcul automatique MSA",events = c("dblclick"),
                               class="btn action-button btn-large btn-dup")), 
                         #checkboxInput("automsa","Calcul automatique.",value=FALSE),
                  
                  wellPanel(
                       h4(textOutput("dispumo"),style="color:blue"),br(),
                       numericInput("ppautof","Besoins pour privé par UMO expl.",value=20000),br(),
                       numericInput("secautof","Autofinancement et sécurité (Euros)",min=0,value=0),
                  actionButton("autosec","Calcul automatique sécurité", class="btn action-button btn-large btn-dup")), 
                       #checkboxInput("autosec","Calcul automatique.",value=TRUE)),
                 
                    wellPanel(
                       h4(textOutput("cren"),style="color:blue"),br(),
                       h5(numericInput("montinv","Investissement (Euros)",min=0,value=0),
                       numericInput("duran","Durée (années)",min=0,value=10),
                       numericInput("taux","taux (%)",min=0,value=2),align="right"),br(),
                       h5(textOutput("perminv"),style="color:green"),
                       h5(textOutput("newannu"),style="color:blue"),
                       h4(textOutput("newdispumo"),style="color:blue")
                                    )),
                             column(8,
                   
                    plotOutput("reseco",height="800px"),
                    
      HTML('<textarea id="cominv" rows="5" cols="100">Commentaires Investissements du scénario</textarea>') 
                    
                    ))),

                tabPanel("Matrice de risques",
                  fluidRow(column(3,

                           h4(selectInput("varisk",label="Variable de risque (prix et volume)",
                                          choices=c("Lait"=1,
                                                    "Cultures de vente"=2,
                                                    "Aliments achetés"=3
                                                    ),selected="1"),style="color:blue;background-color:yellow;"),
                            h4(numericInput("varpri",label="Variations du prix en Euros",value=10,min=1),
                            numericInput("iterpri",label="Nbre d'itérations du prix",value=3,min=1),align="right"),
      
                           h4(numericInput("varvol",label="Variation du volume en %)",value=1,min=1),
                              numericInput("itervol",label="Nbre d'itérations du volume",value=3,min=1),align="right"),
                           h4(selectInput("result","Variations sur quelle base?",choices=c("A partir de 0"=0,"Disponible-PP"=1)),
                              style="color:white;background-color:cyan;")),
                         column(9,
                                h3(textOutput("vares"),style="color:blue"),
                                tableOutput("varisq"),
                                h5("Commentaires matrice de risques :"),
                                tags$textarea(id="CoMatrix", rows="2", style="width:100%;", "")
                                )))


                    )))
         ,busyIndicator("Chargement des données.") 
         ),

        tabPanel(h3("Travail"),
                   tabsetPanel(type="pills",
                   tabPanel("Calcul du temps de travail",
                       fluidRow(
                     column(3,
                                  wellPanel(
                                  h4("Travail d'astreinte :",align="right"),
                                  
                                  h6(numericInput("traite","Traite (h/VL/an)",min=0,20),
                                     numericInput("tralim","Alimentation (h/UGB/an)",min=0,10),
                                     numericInput("curapa","Curage paillage (h/UGB/an)",min=0,2),
                                     numericInput("travo","soins aux veaux (h/veau/an)",min=0,15),
                                     numericInput("trapat","Pâturage (h/UGB/an)",min=0,0),align="right")),
                                    wellPanel(
                                  h4("Travail de saison :",align="right"),
                                  h5(numericInput("troupeau","TS troupeau (j/UGB/an)",min=0,0.3),
                                     numericInput("trafou","TS fourrage (j/UGB/an)",min=0,0.5),
                                     numericInput("tracul","TS culture (j/haCV/an)",min=0,0.5),align="right")),
                                wellPanel(
                                  h5(textInput("queltradiv","Autres travaux (préciser)",value="")),   
                              h5(numericInput("tradiv","Temps (heures/an)",min=0,0),align="right"))                  
                            ),
                           
           
           column(2,
                  plotOutput("totrav",height="800px")),  
           column(1,
                  radioButtons("refw","Référentiel travail",
                               c("Traite","Alimentation","Curage-paillage",
                                 "Veaux","Paturage","TS troupeau","TS fourrages","TS grandes cultures"))),
           
           column(5,      
                  
                  sliderInput("lar","Dimension de l'image",min=20,max=100,step=1,post="%",value=50),
                  uiOutput("travaref")
                  )
           
                 )),
         
         tabPanel("Répartition du travail d'astreinte",
                  
                  fluidRow(
                    column(3,
                           wellPanel(
                           h4("Répartition annuelle du travail d'astreinte:",align="right"),
                           h6(numericInput("TAdel","Délégation (heures/an)",min=0,0),
                              numericInput("TAsal","Travail salarié (heures/an)",min=0,0),
                              numericInput("TAben","Bénévolat (heures/an)",min=0,0),align="right")),
                           br(),br(),
                           h4("Commentaires sur le travail d'astreinte",align="right"),
                           HTML('<textarea id="comast" rows="5" cols="45">Commentaires TA:</textarea>')),
                    column(1),
                    column(8,
                           plotOutput("TA",width="900px",height="600px"))
                         )),
         
         tabPanel("Répartition du travail de saison",
                  
                  fluidRow(
                    column(3,
                           wellPanel(
                           h4("Répartition annuelle du travail de saison",align="right"),
                           h6(numericInput("TSdel","Délégation (heures/an)",min=0,0),
                              numericInput("TSal","Travail salarié (heures/an)",min=0,0),
                              numericInput("TSben","Bénévolat (heures/an)",min=0,0),align="right")),
                           br(),br(),
                           h4("Commentaires sur le travail de saison",align="right"),
                           HTML('<textarea id="comts" rows="5" cols="45">Commentaires TS</textarea>')),
                    column(1),
                    column(8,
                           plotOutput("TS",width="900px",height="600px"))
                    
                            )),
         
         
         tabPanel("Répartition du travail total",
                  fluidRow(
                    column(3, 
                       h4("Commentaires généraux sur  travail dans ce projet:"),   
              HTML('<textarea id="comrepart" rows="15" cols="45">Commentaires sur la répartition du travail:</textarea>')),
                  column(1),  
               column(8,
                 plotOutput("repartot",width="900px",height="600px"))
                             )),

         tabPanel("Références travail nationales",
          #tableOutput("recatra"),
          fluidRow(column(6,
          tags$img(src="travaref/refwTA.jpg", width="600px")),
              column(6,
              tags$img(src="travaref/refwTS.jpg", width="600px")))))       
   
           
          ), 
    
    tabPanel(h3("Editions"),
             tabsetPanel(type="pills",
                         
                         tabPanel("Editions de rapports",
                                  fluidRow(
                                    column(4,
                                           wellPanel(
                                             h4("Format d'édition:"),
                                             radioButtons('format1', " ", c('PDF', 'HTML', 'Word'), inline = TRUE),
                                             br(),
                                             h4("Décocher les pages à ne pas éditer dans la liste suivante:",
                                             checkboxGroupInput('show_page', " ", show.pages,
                                                                selected=show.pages,
                                                                inline = TRUE)),
                                             downloadButton("raproj",textOutput('projrap'))
                                             )),  
                                           
                                    column(8,
                                           h2(textOutput('projconatou'),style="text-align:center;"),
                                           hotable("hotable2")))),        
                         
              tabPanel("Cocher les scénarios à éditer",
                       fluidRow(
                         column(3,
                            wellPanel(              
                              uiOutput("projlist"))),
                                ## textOutput("test"))),
                         column(1),
                         column(7,
                                tableOutput("protabedit")    
                                )
                         )),
   
               tabPanel("Comparaison des résultats",
                        tabsetPanel(type="tabs", 
                            tabPanel(h5("Exploitation"),
                  fluidRow(column(2,
                   #h5(tags$input(type = "button",id = "data_ex" ,value="Enregistrer la selection :"), align="left"), 
                    #actionButton("data_ex","Garder", class="btn action-button btn-large btn-dup"),
                                  wellPanel(checkboxGroupInput("capel_ex",label="",choices=resex,selected=resex))),
                           column(10,tableOutput("recapex")))),
                  
                            tabPanel(h5("Surfaces"),
                              fluidRow(column(2,
                        #h5(tags$input(type = "button",id = "data_surf" ,value="Enregistrer la selection :"), align="left"),                     
                        #actionButton("data_surf","Garder", class="btn action-button btn-large btn-dup"),   
                        wellPanel(checkboxGroupInput("capel_surf",label="",choices=resurf,selected=resurf))),
                               column(6,tableOutput("recapsurf"))
                               #column(4,textOutput("tabsurfex"))
                           )),
                  
                          tabPanel(h5("Alimentation"),
                                   fluidRow(column(2,
                           #h5(tags$input(type = "button",id = "data_alim" ,value="Enregistrer la selection :"), align="left"),                 
                           #actionButton("data_alim","Garder", class="btn action-button btn-large btn-dup"), 
                           wellPanel(checkboxGroupInput("capel_alim",label="",choices=resalim,selected=resalim))),
                            column(10,tableOutput("recapalim")))),

                           tabPanel(h5("Economie"),
                                    fluidRow(column(2,
                                #h5(tags$input(type = "button",id = "data_eco" ,value="Enregistrer la selection :"), align="left"),                 
                                #actionButton("data_eco","Garder", class="btn action-button btn-large btn-dup"),  
                                wellPanel(
                                  checkboxGroupInput("capel_prod",label="",choices=reseco,selected=reseco))),
                                column(10,tableOutput("recaprod")))),
  
                    tabPanel(h5("Edition des comparatifs"),
                             fluidRow(
                               column(3,
                                      wellPanel(
                                        downloadButton("downloadReport","Editions des tableaux au format:"), 
                                        radioButtons('format', " ", c('PDF', 'HTML', 'Word'),
                                                     inline = TRUE)
                                      )),

                               column(4,
                                      wellPanel(
                                        h5("Sauvegarder dans un fichier .ods  toutes les données sélectionnées dans l'onglet 
                               ->Comparaison des résultats:",style="color:blue;font-weight:bold;"),
                                        downloadButton( "resave" ,"Tableaux des données du dossier."))),
                               column(4,
                                        uiOutput("scenagraf"))    
                    )))))),    

                                        
               
              tabPanel(h3("Référentiel"),
                       tabsetPanel(type="pills", 
                                   
                         tabPanel("Fiches fourragères",          
                                              fluidRow(
                                                column(6,
                                                       tags$img(src="cartefou.png",width="500px")),
                                                column(6,
                                                       tags$img(src="zonefou.png",width="700px")))                   
                               ),
                         
                        tabPanel("Fiches paturage et récoltes d'herbe",
                       dataTableOutput("ref1")),
                       tabPanel("Surfaces fourragères à stock exclusif",
                                dataTableOutput("ref2")),
                       #tabPanel("Cultures fourragères zone2",
                        #        dataTableOutput("ref3")),
                       tabPanel("Cultures de vente",
                                dataTableOutput("ref4")),
                       tabPanel("Achats de fourrage",
                                dataTableOutput("ref5")),        
                       tabPanel("Achats de concentrés",
                                dataTableOutput("ref6")),
                       tabPanel("Economie",
                                dataTableOutput("ref7")),
                       tabPanel("PAC",
                                dataTableOutput("ref8")),
                       tabPanel("PP-PT",
                                dataTableOutput("ref9")))),
                       
                     tabPanel("Outil",
                     textOutput("version_fichier"),
                     h4("Embarquer un nouveau référentiel dans le dossier",align="center"),
                     fluidRow(column(1,
                                     icon("umbrella", class="fa  fa-5x")),
                    
                              column(6,        
                              wellPanel(
                      
                      
                        h3("Attention opération à risque!. Après avoir sauvegardé le fichier, le recharger.
                                      Il faudra éventuellement revoir tous les champs 
                                    dans le fichier sauvegardé suite à cette opération,
                                     puis les appliquer projet par projet !.",style="color:blue;"))),
                          column(4,
                                              
                    selectInput("kelref","Quel nouveau référentiel?",choices=c("Inosys"=FALSE,"Personnel"=TRUE)),
                    icon("cog", class="fa fa-refresh fa-spin fa-3x"),
                     downloadButton("newref","Sauvegarde du dossier avec le nouveau référentiel")
                     )))
             


  
)))
