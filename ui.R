library(shiny)  
library(shinythemes)
library(shinycssloaders)
require(shinyBS)
require(shinyWidgets)
#devtools::install_github("dreamRs/shinyWidgets")

# Ce script définit l'interface utilisateur pour l'appli TypoChooseR
shinyUI(fluidPage( theme=shinytheme("united"),
             navbarPage(title=div( img(src="CHAIRE-DECISIONNELLE-ILLUSTRATIONPetitTEXTE.jpg", height = 36,  # titre avec le logo
                                       width = 36),"typoChooseR",align="top"),       # PREMIER PANNEAU, pour explorer et CHOISIR LES VARIABLES ACTIVES à l'aide d'une ACM
             
              ######################################
              ## Premier Onglet, Choix des variables actives et de la gestiond es données manquantes
              ########################################
             tabPanel("Choix des variables actives",
             sidebarPanel(width=3,
               
                  fileInput("fichier",
                 label=h4("Veuillez choisir un fichier pour votre jeu de données qualitatives",
                          tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                 bsButton("q1", label = "", icon = icon("question"), style = "info", size = "extra-small")
                 ),
                 accept=c(
                     "text/csv",
                     "text/comma-separated-values,text/plain",
                    ".csv")),
                  bsPopover(id = "q1", title = "InputType",
                            content = "Seuls les fichiers csv avec un séparateur de champ point-virgule sont pour le moment pris en charge",
                            placement = "right", 
                           options = list(container = "body")
                 ),
                sliderInput("percIndiv1","Pourcentage echantillon pour représentation de l'ACM",
                            min=0.1,max=100,value=25,step=0.5),
                withSpinner(uiOutput("varChoice")),
                actionButton("reset","Toutes/aucune variable(s)"),
                actionButton("goChoix","Appliquez les choix")
                ),
              mainPanel(
                h5("Certains graphiques ne peuvent être affichés que si l'application est ouverte dans un navigateur"),
                h6("(et non dans la fenêtre Rstudio)"),
                        actionButton("goChoix2","Appliquez les choix"),
                        h4(textOutput("messageNA")),
                        conditionalPanel(condition="output.alerteNA1>0",
                                    selectInput("naMethod", "Choisir la méthode de prise en compte des NA",
                                                list("","Convertir en \"Inconnu\"","Supprimer"))),
                        
                        tags$div(id='la'),
                        withSpinner(plotlyOutput("MCAplot")),
                        withSpinner(plotlyOutput("cramerPlot"))
                                                )
             ),
             
             ######################################
             # DEUXIÈME Onglet, pour explorer les méthodos et choisir la méthode de typologie selon critères stats
             ######################################
             
          tabPanel(                        
               "Choix de la méthode de classification",
  
            # Titre principal de panneau 
            headerPanel("TypoChooseR","Choix de la typologie à retenir"),
  
            # Sélection des paramètres pour la création de la typo
            sidebarPanel(
                width=3,
                #fileInput("fichier", "Veuillez choisir un fichier pour votre jeu de données qualitatives",
                #  accept=c(
                #    "text/csv",
                #    "text/comma-separated-values,text/plain",
                #    ".csv")), 
                sliderInput("percIndiv","Pourcentage echantillon pour exploration",min=0.2,max=100,value=2,step=0.5),
               
                selectInput("methode","Méthode de Partitionnement :",
                            choices=c("Kmeans+CAH"="mixte","Modèle de melange"="mixAll","K-means seulement"="kmeans"),selected="kmeans"),
                ###actionButton("fixer", "Fixer le nombre maxi de composantes à la dimensionnalité du fichier"),
                sliderInput("nbGroupes","Nombre de groupes",min=2,max=15,value=4,step=1),
                withSpinner(uiOutput("ui1")),
                withSpinner(uiOutput("ui2")),
    
                h4("Explorer le nombre de composantes"),
                actionButton("imageMap","Comparer les partitions"),
                h5("Attention ! Comparaison = temps de calcul qui augmente avec la taille de l'échantillon"),
                withSpinner(uiOutput("nbCompMax")),
                h5("Limité aux composantes de valeur propre >5% de la première valeur propre"),
                #sliderInput("nbCompMax","Nombre de composantes Max à explorer",min=2,max=20,value=4,step=1),
              
                h3("Basculer vers la recherche du nombre de groupes"),
                h4("(à nombre de composantes fixé)",
                   tags$style(type="text/css", "#q3 {vertical-align: top;}"),
                   bsButton("q3", label ="", icon=icon("question"),style="info", size="extra-small")
                ),
                bsPopover(id="q3", title="NbGroupes",
                          content="Echantillon grand=> Résultat fiable MAIS procédure longue. Interprétation difficile : Explorer avec un groupe de plus ou moins que proposé par le critère",
                          placement = "right", 
                          options = list(container = "body")),
                checkboxInput("compareGroup","Basculer"),
                sliderInput("groupMax","Nombre maximum de groupes à explorer",min=5,max=15,value=8)
          ),
         
               mainPanel(
                h4(textOutput("dim1")),
                h4(textOutput("dim2")),
                conditionalPanel(condition="!input.compareGroup",
                    sliderInput("cos2","A partir de quelle valeur du cos2 représenter les modalités ?", 
                                min=0, max=1,value=0.1 )),
                withSpinner(plotOutput("plane1Output")),
                verbatimTextOutput("echantSearchGroup"),
                withSpinner(plotOutput("imageMapOutput"))
                )
            ),
          ######################################
          #### Troisième onglet, exploration des groupes (répartition des variables par groupe)
          ######################################
      tabPanel( "Exploration de la Typo Choisie",   
           sidebarPanel(
                  width=4,
           h3("Caractérisation des groupes retenus", 
              tags$style(type = "text/css", "#q2 {vertical-align: top;}"),
              bsButton("q2", label = "", icon = icon("question"), style = "info", size = "extra-small")
           ),
           bsPopover(id = "q2", title = "Unchoose",
                     content = "Pour déselectionner une variable, cliquer sur son nom et appuyer sur la touche del ou suppr de votre clavier",
                     placement = "right", 
                     options = list(container = "body")
           ),
           uiOutput("varPlot"),
           
           h3("Vous pouvez entrer un libellé pour les groupes"),
           uiOutput("libelles"),
           #h3("Exporter l'échantillon segmenté"),
           #h4("Cette fonctionnalité n'est pour le moment implémentée que pour un usage local de l'application"),
           #actionButton("exporter","Exporter"),
           h3("Télécharger le fichier partitionné au format csv"),
           downloadButton('downloadData', 'Télécharger')
           ),
           mainPanel(
             h3("Effectifs des groupes retenus"),
             dataTableOutput("groupesEff"),
             h3("Répartition des quatre variables les plus liées aux groupes"),
             plotOutput("barPlots"),
             h3("Répartition par groupe de la(les) variable(s) choisie(s) par l'utilisateur"),
             plotOutput("barPlot2")
           )
           )
      
        )
      )
    
)