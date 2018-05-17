# Chargement des packages nécessaires
#

require(FactoMineR) # Utilisé pour les ACM
require(mclust) #pour les CAH
require(fields)
require(vcd) # pour calcul des cramers
require(MixAll)#pour le modèle de mélange
require(clusterCrit) # pour les calculs de critère (silhouette et pseudo F)
require(ggplot2) # pour des graphes avancés
require(ggrepel)
require(reshape2) # pour la fonction melt
#require(cowplot) # Pour l'affichage de plusieurs ggplot en mode grille
require(gridExtra)
require(shinyBS)
require(DT)

options(shiny.maxRequestSize=120*1024^2) 

# Fonction pour gérer les données manquante par conversion à une modalité "spécifique"Inconnu"
naConvert<-function(x){
  levelsX<-c(levels(x),"Inconnu")
  x<-factor(x,levels=levelsX)
  x[is.na(x)]<-"Inconnu"
  x<-factor(x)
  x
}

# fonction de classif Kmeans+CAH Pour la procédure rencontrée parfois en entreprise (modif possible pour modifier le nombre de centres, si besoin)
kmeansCAH<-function(data,nbGroupes){
  data.kmeans<-kmeans(x=data, centers=round(nrow(as.matrix(data))*0.05,digits=0), iter.max = 100, nstart = 90,        # le as.matrix gère le cas où data est réduit à une colonne
                    algorithm = "MacQueen")
  data.centers<-data.kmeans$centers
  data.klusters<-data.kmeans$cluster
  centers.dist<-dist(data.centers)
  centers.hclust<-hclust(centers.dist,method="ward.D2")
  centers.class<-cutree(centers.hclust,k=nbGroupes)
  data.classefinale<-centers.class[data.kmeans$cluster]
  data.classefinale<-data.classefinale
  data.classefinale
}


shinyServer(function(input,output,session){
  
  
  # # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
  # # ## # #
  # # ## # # INSTRUCTIONs RELATIVES AU CHOIX ET DE LA TAILLE D'ECHANTILLON
  # # ## # #
  
  
  #### Récupération du jeu de données utilisateur avant prise en compte des éventuelles données manquantes
  df1<- reactive({
    infile<-input$fichier
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    dftemp<-read.table(infile$datapath,sep=";",quote = "\"",header=T)
    dftemp<-data.frame(sapply(dftemp,as.factor))
    dftemp
  })
  
    
    
    alerteNA<-reactive({sum(is.na(df1()))})       ### Détection de données manquantes dans le fichier, sera utilisée en local
    
    output$alerteNA1<-reactive({sum(is.na(df1()))})     ### Ce réactive fait l'objet d'un output, pour servir de condition 
                                                        ### à l'affichage du conditionalPanel de choix du mode de gestion des NA 
    
    outputOptions(output, "alerteNA1", suspendWhenHidden = FALSE)   # Comme ce alerteNA1 n'est pas utilisé par l'ui il faut préciser qu'elle existe
                                                                    # pour que la condtion du conditionnalPanel le repère !!!
    
    output$messageNA<-renderText({  # Affichage du nombre de données manquantes du fichier
      if (alerteNA()>0){paste("Ce jeu de données contient",sum(is.na(df1()))," données manquantes, veuillez choisir la façon de les prendre en compte")}
        else{""}
    })
    
    
  
    df<-reactive({              #### Création du jeu de données prenant en compte les NA s'il y en a et qui sert de base à l'analyse
    if(alerteNA()==0){dftemp<-df1()} else{
    if(input$naMethod=="Convertir en \"Inconnu\""){dftemp<-data.frame(lapply(df1(),naConvert))}else{if(input$naMethod=="Supprimer"){dftemp<-na.omit(df1())} else dftemp<-df1()}  
    }
    dftemp
  }) 
  
  # Création de l'échantillon en variables initiales de taille choisie
  
  dfEchantPreChoice<-reactive({         #### Création d'un sous échantillon, pour ne pas geler les ordinateurs qui n'auraient pas la puissance requise 
                                        #### Pourrait être supprimé en version serveur si serveur assez puissant
    echant<-sample(1:nrow(df()),round(input$percIndiv1/100*nrow(df()),digits=0))
    dfEchantPreChoice<-df()[echant,]
    print(paste("dfEchant a ",nrow(dfEchantPreChoice)," lignes"))
    dfEchantPreChoice
  })
  
  # Affichage de la taille d'échantillon
  
  texte1<-reactive({                                       # invite à charger le jeu de données, ou à choisir quelle proportion on en prend une fois qu'il est chargé
    if(is.null(df())){"Choisissez votre jeu de données"}
    else{nombre<-round(input$percIndiv/100*nrow(df()),digits=0)
    nombre2<-ncol(dfEchant())
    paste("On échantillonne ",nombre,"lignes, soit ", input$percIndiv, 
          "pourcent du jeu de données.")}
  })
  texte2<-reactive({
    if(is.null(df())){" "}
    else{
    nombre2<-ncol(dfEchant())
    nombre3<-sum(unlist(lapply(dfEchant(),nlevels)))
    paste("La sélection comporte ",nombre3," catégories issues de ",nombre2," variables.")} 
  })
  
  output$dim1<-renderText({texte1()})
  output$dim2<-renderText({texte2()})
  
  
  
  
  # # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
  # # ## # #
  # # ## # # INSTRUCTIONs RELATIVES AU PREMIER ONGLET POUR LE CHOIX DES VARIABLES ACTIVES
  # # ## # #
  # # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
  
  output$varChoice <- renderUI({              #### Sélection des variables à conserver comme actives dans l'analyse
    infile<-input$fichier 
    if (is.null(infile)){h3("Veuillez charger un fichier")}
    else {checkboxGroupInput("activeVar", "Variables à prendre en compte pour le partitionnement",
                             choices=names(df()), selected=names(df()))
              }
  })
  
  observeEvent(input$reset,{  # Gestion du cochage décochage de toutes les variables à la fois
    print(as.numeric(input$reset))
    if (as.numeric(input$reset) > 0) {
      if (as.numeric(input$reset) %% 2 == 0){
        updateCheckboxGroupInput(session,"activeVar",selected=names(df()))
      }
    else{
      updateCheckboxGroupInput(session,"activeVar",selected=character(0))
    }
    }
    })
  
  dfEchant1<-eventReactive(input$goChoix|input$goChoix2,ignoreInit=T,{                     # jeu de données avec la taille d'échantillon et les variables sélectionnées
    dfEchant1<-dfEchantPreChoice()[,input$activeVar]
    print(input$activeVar)
    dfEchant1
  })
  
  # Réalisation de l'ACM sur l'échantillon choisi
  
  dfMCA1<-eventReactive(input$goChoix|input$goChoix2,ignoreInit = F,{      #Réalisation de l'ACM avec MCA de factoMineR'
    if (alerteNA()==0 | input$naMethod=="Convertir en \"Inconnu\"" | input$naMethod=="Supprimer"){
    dfMCA1<-MCA(dfEchant1(),graph=F,ncp=sum(sapply(dfEchant1(),nlevels)))
    dfMCA1}
  })
  
  output$MCAplot<-renderPlot({          # Premier plan factoriel de l'ACM du point de vue modalités
    if (alerteNA()==0 | input$naMethod=="Convertir en \"Inconnu\"" | input$naMethod=="Supprimer"){
    if (!is.null(input$fichier)){
      
      # représentation du premier plan factoriel de l'ACM
      cats<-apply(dfEchant1(), 2, function(x) nlevels(as.factor(x)))       # Pour les labels des catégories
      dfMCAvars<-data.frame(dfMCA1()$var$coord, Variable = rep(names(cats), cats))      #création du df pour ggplot
     ggplot(data=dfMCAvars,                                                            # création du ggplot du premier plan facto
             aes(x = Dim.1, y = Dim.2, label = rownames(dfMCAvars))) +
        geom_hline(yintercept = 0, colour = "gray70") +
        geom_vline(xintercept = 0, colour = "gray70") +
        geom_text(aes(colour=Variable)) +
        ggtitle("Modalités des variables retenues sur le premier plan factoriel")
    
      }
    } else {par(xaxt="n",yaxt="n",bty="n");plot(1,1,type="n",ann=FALSE);text(1,1,"Veuillez choisir le mode de gestion \n des données manquantes",cex=2)}
  })
  
  
  output$cramerPlot<-renderPlot({
    if (alerteNA()==0 | input$naMethod=="Convertir en \"Inconnu\"" | input$naMethod=="Supprimer"){
      if (!is.null(input$fichier)){
        
        ####### Plot des cramers entre variables
        
        require(vcd)
      df3<-dfEchant1()
      getCramers2<-Vectorize(function(i,j){               ### Fonction permettant de calculer les cramer par paires de variables 
        cramer<-assocstats(table(df3[,i],df3[,j]))$cramer
        cramer}
      )
      
      x<-1:ncol(df3) ; names(x)<-names(df3)             # création des indices des variables
      y<-1:ncol(df3) ; names(y)<-names(df3)
      cramerMat<-outer(x,y,getCramers2)                 # calcul des V de Cramer par paires
      
      #### avec ggplot 2
      
      meltedCramerMat<-melt(cramerMat,varnames=c("variable1","variable2"),value.name="Cramer")  # mise en forme pour ggplot
      
      ggplot(data = meltedCramerMat, aes(variable1, variable2, fill = Cramer))+                 # construction du ggplot
        geom_tile(color = "white")+
        scale_fill_gradient2(low = "white", mid="cyan", high = "blue",  
                             midpoint = 0.5, limit = c(0,1), space = "Lab",
                             name="Cramer") +
        theme_minimal()+ 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                         size = 10, hjust = 1))+
        ggtitle("Liens entre les variables retenues \n mesuré par le V de Cramer")+
        coord_fixed()
    }}
  })
  
  
  
  # # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ##   # # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
  # # ## # #
  # # ## # # INSTRUCTIONs RELATIVES AU DEUXIEME ONGLET POUR LE CHOIX D'UNE TYPO AVEC CRITÈRES STATS
  # # ## # #
  # # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## 
  

  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # # ## # # Les sliders d'input sont créés en fonction de la dimensionnalité du jeu de fichier d'entrée. 
  # # ## # #
  # # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## 
  
  dfEchant<-reactive({  # Taille de l'échantillon pour explorer les différentes partitions
    echant<-sample(1:nrow(df()),round(input$percIndiv/100*nrow(df()),digits=0))
    dfEchant<-df()[echant,input$activeVar]
    print(input$activeVar)
    print(str(dfEchant))
    dfEchant
  })
  
  dfMCA<-reactive({
    dfMCA<-MCA(dfEchant(),graph=F,ncp=sum(sapply(dfEchant(),nlevels)))
    print("etape dfMCA")
    dfMCA
    
  })
  
   # ajout d'un slider dont le max dépend de la dim du fichier
  output$ui1 <- renderUI({
    infile<-input$fichier 
    if (is.null(infile)){sliderInput("n", "Nombre de composantes à jour", 2,10,4)
      }
    else{sliderInput("nbComp", "Nombre de composantes à retenir", 2,length(dfMCA()$eig[,1]),4,step=1)
      }
  })
  output$ui2<- renderUI({
    infile<-input$fichier 
    if (is.null(infile)){sliderInput("factPlan","Plan Factoriel supplémentaire à représenter",value=2,min=2,max=5)}
    else{sliderInput("factPlan","Plan Factoriel supplémentaire à représenter",value=2,min=2,max=round(length(dfMCA()$eig[,1])/2),step=1)}
  })
  
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # Sorties = réalisation des typos à afficher, selon la méthode, le nombre de composantes et de groupes choisis. 
  # # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## 
  
  sorties<-reactive({
        if(input$methode=="mixte"){    #clustering avec la méthode kmeans nbrx puis CAH
     
      data<-dfMCA()$ind$coord[,1:input$nbComp]
      data.classefinale<-kmeansCAH(data,input$nbGroupes)
      data.classefinale
      }
    
    if(input$methode=="kmeans"){             #clustering avec des Kmeans seulement
      data<-dfMCA()$ind$coord[,1:input$nbComp]
      data.classefinale<-kmeans(data,centers=input$nbGroupes,iter.max = 100, nstart = 90,
                                algorithm = "Hartigan-Wong")$cluster
      data.classefinale
      }
                                            #classif modèles de mélange de distribs
    if(input$methode=="mixAll"){data.classefinale<-clusterCategorical(dfEchant(),nbCluster=input$nbGroupes)@zi+1
    data.classefinale} 
    
    values<-list(clusters=data.classefinale,comp=dfMCA()$ind$coord)
    values
    
      })
  
  
  
  
  
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # Affichage des graphiques
  
  
  output$plane1Output<-renderPlot({
    infile<-input$fichier 
    if (!is.null(infile)){
      if(input$compareGroup==F){
    planIndex<-c(input$factPlan*2-1,input$factPlan*2)
    planAbs<-input$factPlan*2-1
    planOrd<-input$factPlan*2
    
    ModVar_1_2<-function(mod){names(dfMCA()$call$X)[apply(dfMCA()$call$X,2,function(x){mod%in%x})]}
    
    ## Données pour le premier plan factoriel
      #indivs
    inds_1_2<-data.frame(dim1=sorties()$comp[,1],dim2=sorties()$comp[,2],
                         classe=as.factor(sorties()$clusters))
    
    validate(need(sum(apply(dfMCA()$var$cos2[,1:2],1,mean)==Inf)<1,
      "Trop peu de données pour calculer les cos2, merci d'augmenter la taille de votre échantillon."))
    
      #catégories des variables, filtrées par le cos2 ### A FAIRE : prendre le cos2 moyen sur le plan
    print("les cos2 moyens sur le premier plan")
    print(sort(apply(dfMCA()$var$cos2[,1:2],1,mean)))
    mods_1_2<-data.frame(dim1=dfMCA()$var$coord[apply(dfMCA()$var$cos2[,1:2],1,mean)>input$cos2,1],
                         dim2=dfMCA()$var$coord[apply(dfMCA()$var$cos2[,1:2],1,mean)>input$cos2,2])
   
    ### RATTACHEMENT D'UNE MODALITE A SA VARIABLE NON RESOLU
    modVarNames_1_2<-tapply(row.names(mods_1_2),row.names(mods_1_2),ModVar_1_2)
    
    mods_1_2$vars<-as.factor(modVarNames_1_2)
   
    ## Pour les plans supérieurs
    planAbsc<-input$factPlan*2-1
    planOrd<-input$factPlan*2
    
        #Eviter les cos2 infinis
    validate(need(sum(apply(dfMCA()$var$cos2[,planAbsc:planOrd],1,mean)==Inf)<1,
                  "Trop peu de données pour calculer les cos2, merci d'augmenter la taille de votre échantillon."))
    
    # Sélection des données représentées sur le plan factoriel choisi par l'utilisateur
    modsSup<-data.frame(absc=dfMCA()$var$coord[apply(dfMCA()$var$cos2[,c(planAbsc,planOrd)],1,mean)>input$cos2,planAbsc],
                        ord=dfMCA()$var$coord[apply(dfMCA()$var$cos2[,c(planAbsc,planOrd)],1,mean)>input$cos2,planOrd])
    
    indsSup<-data.frame(absc=sorties()$comp[,planAbsc],ord=sorties()$comp[,planOrd],
                        classe=as.factor(sorties()$clusters))
    
    
      temp<-dfMCA()$call$X
      ModVar<-function(chaine){names(temp)[apply(temp,2,function(x){chaine%in%x})]} # Quelle honte, modifier ce code !
      print(names(temp)[apply(temp,2,function(x){row.names(modsSup)[1]%in%x})])
      modVarNames<-tapply(row.names(modsSup),row.names(modsSup),ModVar)
      modsSup$vars<-as.factor(modVarNames)
  
    indPlot1<-ggplot(inds_1_2,aes(dim1,dim2,color=classe))+geom_point()+
            #labs(title="Individus sur le premier plan factoriel",group="classe")+
            theme_classic()+stat_ellipse(geom="polygon",alpha=0.2,aes(fill=classe))+
      stat_ellipse(aes(dim1,dim2,group=classe),linetype=2,color="black")
    
    
    indPlotSup<-ggplot(indsSup,aes(absc,ord,color=classe))+geom_point()+
      labs(x=paste("dim",planAbsc),y=paste("dim",planOrd),title=paste("Individus sur le ",input$factPlan,"ième plan factoriel"))+
            theme_classic()+stat_ellipse(geom="polygon",alpha=0.2,aes(fill=classe))+
      stat_ellipse(aes(absc,ord,group=classe),linetype=2,color="black")
    
    modPlot1<-ggplot(mods_1_2,aes(dim1,dim2,label=rownames(mods_1_2),
                                  color=vars))+
      geom_point()+scale_color_brewer(palette="Set1",guide=F)+
      geom_text_repel()+theme_classic() +
      labs(title="Modalités les mieux projetées sur le premier plan factoriel")

    modPlotSup<-ggplot(modsSup,aes(absc,ord,label=rownames(modsSup),color=vars))+geom_point()+
      scale_color_brewer(palette="Set1",guide=FALSE)+geom_text_repel()+
      labs(title=paste("Modalités les mieux projetées sur le ",input$factPlan,"ème plan factoriel"), 
           x=paste("dim",planAbsc),
           y=paste("dim",planOrd)
           )+
      theme_classic()
    
    grid.arrange(indPlot1,modPlot1,indPlotSup,modPlotSup)
      }
    ##########################################################################################
    #########    # Exploration du nombre de groupes
    ##########################################################################################
    
      
      
      else if(input$compareGroup==T&input$methode=="kmeans"){
      input$percIndiv
      data<-isolate(dfMCA()$ind$coord)[
        sample(1:nrow(isolate(dfMCA()$ind$coord)),size=min(nrow(isolate(dfMCA()$ind$coord)),5000)),
        1:input$nbComp]
    
      gCritcompare<-function(j){
        print(j)
        intCriteria(data,kmeans(data,centers=j,iter.max = 100, nstart = 40,
                                                         algorithm = "Hartigan-Wong")$cluster,crit="Silhouette")}
      groupSeq<-3:input$groupMax
    
      plot( groupSeq,unlist(tapply(X=groupSeq,INDEX=groupSeq,FUN=gCritcompare,simplify=TRUE)),type='l',
         ylab="Silhouette",xlab="Nb Groupes",
         main="Préférer un nombre de groupes avec un critère silhouette proche de 1")
    }
    
    if(input$compareGroup==T&input$methode=="mixte"){
      input$percIndiv
      data<-isolate(dfMCA()$ind$coord)[sample(1:nrow(isolate(dfMCA()$ind$coord)),
                                              size=min(nrow(isolate(dfMCA()$ind$coord)),5000)),
                                       1:isolate(input$nbComp)]
      gCritcompare<-function(j){ # finalement critère retenu = silhouette, mais je ne renomme pas la variable
        print(j)
        intCriteria(data,kmeansCAH(data,nbGroupes=j),crit="Silhouette")}
      groupSeq<-3:input$groupMax
      
      plot(groupSeq,unlist(tapply(X=groupSeq,INDEX=groupSeq,FUN=gCritcompare,simplify=TRUE)),type='l',
           ylab="Silhouette",xlab="Nb Groupes",
           main="Préférer un nombre de groupes avec un critère silhouette proche de 1")
    }
    
    if(input$compareGroup==T&input$methode=="mixAll"){
      input$percIndiv
      data<-isolate(dfEchant()[sample(1:nrow(isolate(dfEchant())),size=min(nrow(isolate(dfEchant())),5000)),])
      groupSeq<-3:input$groupMax
      critere<-NULL
      for (j in 3:input$groupMax){
      critere<-c(critere,clusterCategorical(isolate(dfEchant()),nbCluster=j,criterion="BIC")@criterion)
      #data.classefinale
      }
      
      plot(groupSeq,critere,type="l",xlab="Nombre de groupes",ylab="Critère BIC",main="Préférez un nombre de groupes avec un critère BIC petit")
    
    }
    }
    })
  
   output$echantSearchGroup<-renderText({
     if(input$compareGroup==T){
     paste("Si votre puissance de calcul est suffisante, augmentez la taille de l'échantillon pour une meilleure détection du nombre de groupes optimal.",
           "\n","\n",
           "Cependant, on réalise une typologie pour chaque nombre de groupe. Pour limiter le temps de calcul, la taille des échantillons utilisés est donc limitée à 5000 individus.",
           "\n","\n",
           "Note 1 : Le nombre optimal n'est qu'indicatif, il peut-être utile de regarder les typologies réalisées avec un groupe de plus ou de moins que l'optimum sur l'échantillon.",
            "\n","\n",
            "Note 2 : le critère silhouette doit être maximisé, le critère BIC doit être minimisé. ",
           "\n","\n",
           "Note 3 : la méthode Kmeans+CAH n'est pas recommandée, car elle est plus instable que les autres méthodes, il peut être prudent d'effectuer plusieurs fois la recherche de groupe",
           "\n","\n",
           " Note 4 : Les modèles de mélange ont parfois tendance à conserver un nombre plus grand de groupes, explorer avec un ou deux groupes de moins."
            )
    } 
   })
  
  #########   ############ #########   ############ #########   ############ #########   ############ #########   ############ 
  ## Matrice des comparaisons de partitions avec Rand Index
  #########   ############ #########   ############ #########   ############ #########   ############ #########   ############ 
  #########   ############ #########   ############ #########   ############ #########   ############ #########   ############ 
  
  
  heatmapData<-eventReactive(input$imageMap,{                            # C'est le bouton d'action qui provoque la réalisation de la carte des comparaisons
      infile<-isolate(input$fichier)                                     # NOTE : les isolate peuvent être supprimés grâce à l'encapsulation dans event reactive. 
      
      if (!is.null(infile)){
      if(isolate(input$compareGroup)==F&isolate(input$methode)=="mixte"){ 
        #dimMax<-length(isolate(dfMCA())$eig[,1])
        dimMax<-sum(isolate(dfMCA())$eig[,1]>=0.05*isolate(dfMCA())$eig[1,1])
        print(dimMax)
        data<-isolate(dfMCA())$ind$coord 
        fkmeansCAH<-function(i){                    # création d'une fonction prenant l'indice en entrée afin d'utiliser outer pour la création de la heatmap
          print(i)
          kmeansCAH(data=data[,1:i],nbGroupes=isolate(input$nbGroupes))
        }
        x<-1:dimMax
        classesFinales<--matrix(unlist(tapply(X=x,INDEX=x,FUN=fkmeansCAH,simplify=TRUE)),ncol=dimMax) 
        rand.matrice<-matrix(nrow=dimMax,ncol=dimMax) # matrice des rand, à remplir
        randf<-Vectorize(function(i,j){adjustedRandIndex(x=classesFinales[,i],y=classesFinales[,j])})
        rand.matrice<-as.matrix(outer(x,x,randf))
        rand.matrice}
        else if(isolate(input$compareGroup)==F&isolate(input$methode)=="kmeans"){               # on fait des K-means avec toutes les composantes et on les compare
        #dimMax<-length(isolate(dfMCA())$eig[,1])
        dimMax<-sum(isolate(dfMCA())$eig[,1]>=0.05*isolate(dfMCA())$eig[1,1])
        print(dimMax)
        data<-isolate(dfMCA()$ind$coord)
        fkmeans<-function(i){
          print(i)
          kmeans(x=data[,1:i],centers=isolate(input$nbGroupes),iter.max = 100, nstart = 90,
                 algorithm = "Hartigan-Wong")$cluster
        }
        x<-1:dimMax
        classesFinales<-matrix(unlist(tapply(X=x,INDEX=x,FUN=fkmeans,simplify=TRUE)),ncol=dimMax) 
        randf<-Vectorize(function(i,j){adjustedRandIndex(x=classesFinales[,i],y=classesFinales[,j])})
        rand.matrice<-outer(x,x,randf)
        rand.matrice} 
      } else if(isolate(input$compareGroup)==F&isolate(input$methode)=="MixAll"){rand.matrice<-1}
      rand.matrice
    }
  )
  
  
  output$imageMapOutput<-renderPlot({
    #input$imageMap
    
    if(input$compareGroup==F&(input$methode=="mixte"|input$methode=="kmeans")){
      #dimMax<-length(isolate(dfMCA())$eig[,1])
      dimMax<-sum(isolate(dfMCA())$eig[,1]>=0.05*isolate(dfMCA())$eig[1,1])
      print(paste("dimMax le nombre de dimensions est de ",dimMax))
      par(mfrow=c(1,2))
      print(heatmapData())
      image(heatmapData(),x=1:dimMax,y=1:dimMax,col=rev(heat.colors(40)),
                 main=paste("Indice de Rand pour comparer les partitions à ","\n",isolate(input$nbGroupes), " groupes"),
                 xlab="nombre d'axes retenus pour le calcul des distances", ylab="Nombre d'axes retenus pour le calcul des distances")
      plot(dfMCA()$eig[,1],main="Valeurs Propres de l'ACM",ylab="Valeurs propres axes ACM") # on met les valeurs propres à côté
    } else {
      plot(0,0,type="n",axes=F,yaxt='n',xaxt='n',ann=FALSE)
            text(0,0,
                 "Le modèle de mélange prend en compte toutes l'information, 
                 pas de choix de nombre de composantes à faire",
                 yaxt='n',xaxt='n',ann=FALSE,cex=1.6)}
  })
  

  
  # # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
  # # ## # #
  # # ## # # INSTRUCTIONs RELATIVES AU TROISIEME ONGLET POUR L'ANALYSE DES GROUPES OBTENUS et l'export
  # # ## # #
  # # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## 
  
  output$groupesEff<-renderDataTable({
    
    prov1<-table(sorties()$clusters,deparse.level=0)
    prov2<-prov1/sum(prov1)*100
    prov<-rbind(as.integer(prov1),round(prov2,2))
    colnames(prov)<-paste("groupe",1:length(prov1),sep=" ")
    rownames(prov)<-c("effectifs","pourcentages")
    prov<-DT::datatable(prov,rownames=TRUE, options = list(dom = 't'))
    prov
  })
  
  
  
  require(vcd)
  
  output$varPlot <- renderUI({
    infile<-input$fichier 
    if (is.null(infile)){h3("Veuillez charger un fichier")}
    else {selectInput("varPlot", "Sélectionner jusqu'à quatre autres variable à représenter : ", choices=names(df()),selected=names(dfEchant())[1],
                      multiple=T)
    }
  })
  
  
  
  getCramers<-function(x,i,y){
    cramer<-assocstats(table(x[,i],y))$cramer
    cramer
  }
  #tapply(X=x,INDEX=indices,FUN=getCramers,x=df,y=clusters)
  
  varCor<-reactive({
    indVar<-1:dim(dfEchant())[2]
    clusters<-sorties()$clusters
    cramersTests<-tapply(X=indVar,INDEX=indVar,FUN=getCramers,x=dfEchant(),y=clusters) 
    indiceProv<-as.numeric(unlist(dimnames(sort(cramersTests,decreasing=T)[1:4])))
    indiceProv
  })
    
  output$barPlots<-renderPlot({   # boxplot des 4 variables les plus liées au groupe, en fonction du groupe
    groupes<-sorties()$clusters
    
    for(j in 1:4){
      tableProv<-table(dfEchant()[,varCor()[j]],groupes)
      Vcram<-assocstats(tableProv)$cramer
      tableProv<-tableProv/apply(tableProv,1,sum)
      varName<-names(dfEchant())[varCor()[j]] 
      nam<-paste("plot",j,sep="")
      print(as.data.frame(tableProv))
      largeur<-as.numeric(table(groupes)/length(groupes))
      assign(nam,ggplot(data=as.data.frame(tableProv),aes(x=groupes,y=Freq,fill=Var1)) + 
               geom_bar(position="fill",stat="identity")+ 
               labs(title=varName,subtitle = paste("V Cramer groupe/",varName, round(Vcram,2)))+
                              guides(fill=guide_legend(title=varName)))
      }
    
    grid.arrange(plot1,plot2,plot3,plot4, nrow=2,ncol=2)
    
        })
  
  output$barPlot2<-renderPlot({
        validate(need(
      ((length(input$varPlot)<5)&(length(input$varPlot)>0)),
      "Choisissez de 1 à 4 variables à représenter en fonction des groupes"))
    
    groupes<-sorties()$clusters
    
    for(j in 1:length(input$varPlot)){
      print("nombre de lignes")
        print(nrow(df()[rownames(dfEchant()),input$varPlot[j]]))
       print(nrow(groupes))
        tableProv<-table(df()[rownames(dfEchant()),input$varPlot[j]],groupes)
        varProv<-df()[rownames(dfEchant()),input$varPlot[j]]
        Vcram<-assocstats(tableProv)$cramer
        tableProv<-tableProv/apply(tableProv,1,sum)
        print("tableprov")
        print(tableProv)
        #tableDF<-as.data.frame(tableProv)
        #print(tableDF)
        varName<-names(dfEchantPreChoice()[,input$varPlot])[j]
        nam<-paste("plot",j,sep="")
        assign(nam,ggplot(data=as.data.frame(tableProv),aes(x=groupes,y=Freq,fill=Var1)) + # Var1 vient de la sortie de table
                 geom_bar(position="fill",stat="identity")+ 
                 labs(title=varName,subtitle = paste("V Cramer groupe/",varName, round(Vcram,2)))+
                 guides(fill=guide_legend(title=varName)))
      }
    
    #Solution honteuse pour gérer le nombre de variables représentées
    if(length(input$varPlot)==1){grid.arrange(plot1,nrow=1,ncol=2)} 
      else if (length(input$varPlot)==2){grid.arrange(plot1,plot2,nrow=1,ncol=2)} 
        else if (length(input$varPlot)==3){grid.arrange(plot1,plot2,plot3,nrow=2,ncol=2)}
          else{grid.arrange(plot1,plot2,plot3,plot4,nrow=2,ncol=2)}
        
    
  })

  output$libelles<-renderUI({ 
    output<-tagList()
    effectifs<-table(sorties()$clusters)
    for (i in seq_along(unique(sorties()$clusters))){
     
     groupe<-paste("groupe",i,"(",effectifs[i],"individus )")
     print(groupe)
     output[[i]]<-textInput(groupe,paste("Libelle ",groupe),value="saisissez un libellé")
    }
    output
  })
  
  
  observeEvent(input$exporter,{
    
    libelles<-NULL
    for (i in 1:input$nbGroupes){
    print(paste("groupe",i))
      libelles[i]<-input[[paste("groupe",i)]]
    }
    print(libelles)
    
    
    clusteredDF<-cbind(dfEchant(),sorties()$clusters,libelles[sorties()$clusters])
    #print(head(clusteredDF))
    #print("j'aimerais exporter")
    write.table(clusteredDF,"clusteredDF",quote=F,sep=";",col.names=T,row.names=T)
    print("fait")
  })

  #######################################################
  ###
  ### télécharger le fichier partitionné
  #######################################################
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$fichier, "clustered.csv", sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- ";"
      libelles<-NULL
      for (i in 1:input$nbGroupes){
        print(paste("groupe",i))
        libelles[i]<-input[[paste("groupe",i)]]
      }
      # Write to a file specified by the 'file' argument
      data<-data.frame(cbind(dfEchant(),sorties()$clusters,libelles[sorties()$clusters]))
                       names(data)<-c(names(dfEchant()),"groupID","groupLabel")
      write.table(data, file, sep = sep,row.names = FALSE)
    }
  )
  
    
    })


#pour le lancer shiny::runApp("/data/A1_CreditAgricole/TypoACMPropre/typoChoice")

