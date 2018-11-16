a_installer<-c("FactoMineR","mclust","fields","vcd","MixAll","clusterCrit","ggplot2","ggrepel","reshape2","gridExtra","shinyBS",
               "DT","shiny","shinythemes","shinycssloaders","shinyBS","shinyWidgets","devtools")

for ( i in a_installer){
  message(paste("Recherche de ",i))
  if (!requireNamespace(i)){
    message(paste("     installation de",i))
    install.packages(i,checkBuilt=T,dependencies = T)}
  }

install.packages("devtools",checkBuilt=T)
require(devtools)
devtools::install_github("dreamRs/shinyWidgets")
