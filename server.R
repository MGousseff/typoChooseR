# Chargement des packages nécessaires
require(FactoMineR) # Utilisé pour les ACM
require(mclust) #pour les CAH
require(fields)
require(vcd) # pour calcul des cramers
require(MixAll)#pour le modèle de mélange
require(clusterCrit) # pour le pseudo F de Calinsky Harabasz
require(ggplot2) # pour des graphes avancés
require(reshape2) # pour la fonction melt
#require(cowplot) # Pour l'affichage de plusieurs ggplot en mode grille
require(gridExtra)

options(shiny.maxRequestSize=60*1024^2) 

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


shinyServer(function(input,output){
  
  
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
                                                                    # pour que la condtiob du conditionnalPanel le repère !!!
    
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
  
  texte<-reactive({                                       # invite à charger le jeu de données, ou à choisir quelle proportion on en prend une fois qu'il est chargé
    if(is.null(df())){"Choisissez votre jeu de données"}
    else{nombre<-round(input$percIndiv/100*nrow(df()),digits=0)
    nombre2<-ncol(dfEchantPreChoice())
    paste("On échantillonne ",nombre,"lignes, soit ", input$percIndiv, 
          "pourcent du jeu de données qui a ",nombre2," variables")}
  })
  
  output$dim<-renderText({texte()})
  
  
  
  
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
  
  
  dfEchant1<-reactive({                     # jeu de données avec la taille d'échantillon et les variables sélectionnées
    dfEchant1<-dfEchantPreChoice()[,input$activeVar]
    print(input$activeVar)
    dfEchant1
  })
  
  # Réalisation de l'ACM sur l'échantillon choisi
  
  dfMCA1<-reactive({      #Réalisation de l'ACM avec MCA de factoMineR'
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
        ggtitle("Liens entre les variables retenus \n mesuré par le V de Cramer")+
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
    if (is.null(infile)){sliderInput("factPlan","Plan Factoriel supplémentaire à représenter",value=3,min=3,max=5)}
    else{sliderInput("factPlan","Plan Factoriel supplémentaire à représenter",value=3,min=3,max=round(length(dfMCA()$eig[,1])/2),step=1)}
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
    planIndex<-c(input$factPlan*2-1,input$factPlan*2)
    par(mfrow=c(2,2))
    plot(sorties()$comp[,1:2],main="Classes sur le premier plan fact.",col=sorties()$clusters) 
    plot(sorties()$comp[,3:4],main="Classes sur le deuxième plan fact.",col=sorties()$clusters)
    plot(sorties()$comp[,planIndex],main=paste("Classes sur le plan fact. ", isolate(input$factPlan)),col=sorties()$clusters)
    
    
    #########    # Comparaison des pseudo F selon nb groupes à nb composantes donné
    if(input$compareGroup==T&input$methode=="kmeans"){
    data<-isolate(dfMCA()$ind$coord)[sample(1:nrow(isolate(dfMCA()$ind$coord)),size=min(nrow(isolate(dfMCA()$ind$coord)),5000)),]
    gPseudoFcompare<-function(j){
      print(j)
      intCriteria(data,kmeans(data,centers=j,iter.max = 100, nstart = 90,
                                                         algorithm = "Hartigan-Wong")$cluster,crit="Calinski_Harabasz")}
    groupSeq<-2:15
    plot(unlist(tapply(X=groupSeq,INDEX=groupSeq,FUN=gPseudoFcompare,simplify=TRUE)),type='l',
         ylab="Pseudo F",xlab="Nb Groupes",
         main=paste("Pas de critère scalable","\n","pour le moment","\n", "fait avec un échantillon de taille 5000"))
    }
    
    if(input$compareGroup==T&input$methode=="mixte"){
      data<-isolate(dfMCA()$ind$coord)[sample(1:nrow(isolate(dfMCA()$ind$coord)),size=min(nrow(isolate(dfMCA()$ind$coord)),5000)),]
      gPseudoFcompare<-function(j){
        print(j)
        intCriteria(data,kmeansCAH(data,nbGroupes=j),crit="Calinski_Harabasz")}
      groupSeq<-2:15
      plot(unlist(tapply(X=groupSeq,INDEX=groupSeq,FUN=gPseudoFcompare,simplify=TRUE)),type='l',
           ylab="Pseudo F",xlab="Nb Groupes",
           main=paste("Pas de critère scalable","\n","pour le moment","\n", "fait avec un échantillon de taille 5000"))
    }
    }
    })
  

  #output$criteria<-renderPlot({
   # infile<-input$fichier 
  #  if (!is.null(infile)){
  #  plot(dfMCA()$eig[[1]],main="Valeurs Propres de l'ACM")}
  # })
  
  
  #########   ############ #########   ############ #########   ############ #########   ############ #########   ############ 
  ## Matrice des comparaisons de partitions avec Rand Index
  #########   ############ #########   ############ #########   ############ #########   ############ #########   ############ 
  #########   ############ #########   ############ #########   ############ #########   ############ #########   ############ 
  
  
  heatmapData<-eventReactive(input$imageMap,{                            # C'est le bouton d'action qui provoque la réalisation de la carte des comparaisons
      infile<-isolate(input$fichier)                                     # NOTE : les isolate peuvent être supprimés grâce à l'encapsulation dans event reactive. 
      
      if (!is.null(infile)){
      if(isolate(input$methode)=="mixte"){ 
        #dimMax<-length(isolate(dfMCA())$eig[,1])
        dimMax<-sum(isolate(dfMCA())$eig[,1]>=0.05*isolate(dfMCA())$eig[1,1])
        print(dimMax)
        data<-dfMCA()$ind$coord 
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
      if(isolate(input$methode)=="kmeans"){               # on fait des K-means avec toutes les composantes et on les compare
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
      }
      rand.matrice
    }
  )
  
  
  output$imageMapOutput<-renderPlot({
    input$imageMap
    if(input$methode=="mixAll"){                   # Dans le cas du modèle de mélange, la carte 'na pas de sens, on prend toute l'info)
      plot(1,2,xlab="",ylab="",type="n",xaxt="n",yaxt="n",main="Modèles de mélange")
      text(1,2,"totalité de l'info toujours prise en compte")}
    if(input$methode=="mixte"|input$methode=="kmeans"){
      #dimMax<-length(isolate(dfMCA())$eig[,1])
      dimMax<-sum(isolate(dfMCA())$eig[,1]>=0.05*isolate(dfMCA())$eig[1,1])
      print(paste("dimMax le nombre de dimensions est de ",dimMax))
      par(mfrow=c(1,2))
      print(heatmapData())
      image(heatmapData(),x=1:dimMax,y=1:dimMax,col=rev(heat.colors(40)),
                 main=paste("Indice de Rand pour comparer les partitions à ","\n",isolate(input$nbGroupes), " groupes"),
                 xlab="nombre d'axes retenus pour le calcul des distances", ylab="Nombre d'axes retenus pour le calcul des distances")
      plot(isolate(dfMCA()$eig[[1]]),main="Valeurs Propres de l'ACM",ylab="Valeurs propres axes ACM") # on met les valeurs propres à côté
    }
  })
  

  # # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # #
  # # ## # #
  # # ## # # INSTRUCTIONs RELATIVES AU TROISIEME ONGLET POUR L'ANALYSE DES GROUPES OBTENUS et l'export
  # # ## # #
  # # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## 
  require(vcd)
  
  output$varPlot <- renderUI({
    infile<-input$fichier 
    if (is.null(infile)){h3("Veuillez charger un fichier")}
    else {selectInput("varPlot", "Représenter la variable : ", choices=names(df()),selected=names(dfEchant())[1],
                      multiple=F)
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
    
  output$barPlots<-renderPlot({
    par(mfrow=c(2,2))
    groupes<-sorties()$clusters
    tableProv<-table(dfEchant()[,1],groupes)
    ### L'usage de cowplot::plot_grid est de faire l'équivalent de par(mfrow=) pour du ggplot2
    
    for(j in 1:4){
      tableProv<-table(dfEchant()[,varCor()[j]],groupes)
      tableProv<-tableProv/apply(tableProv,1,sum)
      tableDF<-as.data.frame(tableProv)
      #print(tableDF)
      varName<-print(names(dfEchant())[varCor()[j]])
      nam<-paste("plot",j,sep="")
      assign(nam,ggplot(data=as.data.frame(tableProv),aes(x=groupes,y=Freq,fill=Var1)) + geom_bar(position="fill",stat="identity")
             + guides(fill=guide_legend(title=varName)))
               #barplot(tableProv,
      #      col = rainbow(5),
       #     legend=rownames(tableProv),
        #    xlab="groupes")
      }
    print(ls()) 
    grid.arrange(plot1,plot2,plot3,plot4, nrow=2,ncol=2)
    
        })
  output$barPlot2<-renderPlot({
    print(input$varPlot)
    groupes<-sorties()$clusters
    tableProv<-table(df()[rownames(dfEchant()),input$varPlot],groupes)
    print("test")
    
    tableProv<-tableProv/apply(tableProv,1,sum)
    tableProv<-as.data.frame(tableProv)
    print(tableProv)
    ggplot(data=as.data.frame(tableProv),aes(x=groupes,y=Freq,fill=Var1)) + geom_bar(position="fill",stat="identity")+ggtitle(paste("Répartition des modalités de la variable ",input$varPlot, " par groupe"))
  })

  output$libelles<-renderUI({ 
    output<-tagList()
    for (i in seq_along(unique(sorties()$clusters))){
     
     groupe<-paste("groupe",i)
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

  
    
    })


#pour le lancer shiny::runApp("/data/A1_CreditAgricole/TypoACMPropre/typoChoice")

