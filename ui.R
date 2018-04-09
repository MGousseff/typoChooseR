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
                #  fileInput("fichier", "Veuillez choisir un fichier pour votre jeu de données qualitatives",
                #            accept=c(
                #              "text/csv",
                #              "text/comma-separated-values,text/plain",
                #              ".csv")), 
                #          tags$div(id = 'ici'),
      
                          
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
                actionButton("reset","Toute/aucune variable(s)"),  
                actionButton("goChoix","Appliquez les choix")
                      ),
              mainPanel(
                        h4(textOutput("messageNA")),
                        
                        conditionalPanel(condition="output.alerteNA1>0",
                                    selectInput("naMethod", "Choisir la méthode de prise en compte des NA",
                                                list("","Convertir en \"Inconnu\"","Supprimer"))),
                        
                        tags$div(id='la'),
                        withSpinner(plotOutput("MCAplot")),
                        withSpinner(plotOutput("cramerPlot"))
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
                sliderInput("percIndiv","Pourcentage echantillon pour exploration",min=0.1,max=100,value=1,step=0.5),
               
                selectInput("methode","Méthode de Partitionnement :",
                            choices=c("Kmeans+CAH"="mixte","Modèle de melange"="mixAll","K-means seulement"="kmeans"),selected="kmeans"),
                ###actionButton("fixer", "Fixer le nombre maxi de composantes à la dimensionnalité du fichier"),
                sliderInput("nbGroupes","Nombre de groupes",min=2,max=10,value=4,step=1),
                h4("Variation de l'inertie intra"),
                h5("selon le nombre de groupe"),
                h5("à nombre de composantes fixé"),
                checkboxInput("compareGroup","Explorer"),
                    #sliderInput("nbComp","Nombre d'axes ACM",min=2,max=20,value=4,step=1),
    
                withSpinner(uiOutput("ui1")),
                withSpinner(uiOutput("ui2")),
    
                h4("Explorer le nombre de composantes"),
                actionButton("imageMap","Comparer les partitions"),
                h4("Attention ! Comparaison = temps de calcul qui augmente avec la taille de l'échantillon")
                ),
         
               mainPanel(
                h4(textOutput("dim1")),
                h4(textOutput("dim2")),
                sliderInput("cos2","A partir de quelle valeur du cos2 représenter les modalités ?", min=0, max=1,value=0.3 ),
                withSpinner(plotOutput("plane1Output")),
               # conditionalPanel(condition='input.compareGroup%%2==0',),
                #plotOutput("criteria"),
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
           bsPopover(id = "q2", title = "Unchoose",
                     content = "Pour déselectionner une variable, cliquer sur son nom et appuyer sur la touche del ou suppr de votre clavier",
                     placement = "right", 
                     options = list(container = "body")
           ),
           h3("Vous pouvez entrer un libellé pour les groupes"),
           uiOutput("libelles"),
           #h3("Exporter l'échantillon segmenté"),
           #h4("Cette fonctionnalité n'est pour le moment implémentée que pour un usage local de l'application"),
           #actionButton("exporter","Exporter"),
           h3("Télécharger le fichier partitionné au format csv"),
           downloadButton('downloadData', 'Télécharger')
           ),
           mainPanel(
             h3("Répartition des quatre variables les plus liées aux groupes"),
             plotOutput("barPlots"),
             h3("Répartition par groupe de la variable choisie par l'utilisateur"),
             plotOutput("barPlot2")
           )
           )
      
        )
      )
    
)