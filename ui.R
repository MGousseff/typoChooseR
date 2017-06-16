library(shiny)  
library(shinythemes)
library(shinycssloaders)

mycss <- "
#plot-container {
  position: relative;
}
#loading-spinner {
  position: absolute;
  left: 50%;
  top: 50%;
  z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
}
"


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
                sliderInput("percIndiv1","Pourcentage echantillon pour représentation de l'ACM",min=0.1,max=100,value=25,step=0.5),
                withSpinner(uiOutput("varChoice")) 
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
                sliderInput("nbGroupes","Nombre de groupes",min=2,max=10,value=4,step=1),
                h4("Explorer nombre de groupes"),
                h5("Pour un nombre de composantes donné"),
                checkboxInput("compareGroup","Explorer"),
                    #sliderInput("nbComp","Nombre d'axes ACM",min=2,max=20,value=4,step=1),
    
                withSpinner(uiOutput("ui1")),
                withSpinner(uiOutput("ui2")),
    
                ###actionButton("fixer", "Fixer le nombre maxi de composantes à la dimensionnalité du fichier"),
                selectInput("methode","Méthode de Partitionnement :",
                  choices=c("Kmeans+CAH"="mixte","Modèle de melange"="mixAll","K-means seulement"="kmeans"),selected="kmeans"),
                h4("Explorer le nombre de composantes"),
                actionButton("imageMap","Comparer les partitions"),
                h4("Attention ! Comparaison = temps de calcul qui augmente avec la taille de l'échantillon")
                ),
         
               mainPanel(
                h3(textOutput("dim")),
                withSpinner(plotOutput("plane1Output")),
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
           h3("Caractérisation des groupes retenus"),
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
             h3("Répartition des quatre variables les plus liées aux groupes"),
             plotOutput("barPlots"),
             h3("Répartition par groupe de la variable choisie par l'utilisateur"),
             plotOutput("barPlot2")
           )
           )
      
        )
      )
    
)