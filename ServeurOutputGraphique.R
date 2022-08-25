#outputs de création des différents graphiques faisables par l'application


#permet d'afficher le graphique spécifié dans la liste de choix
#!!! pour que cela fonctionne il faut que le nom du output de création de chque graphique corresponde au noom associé dans la liste de proposition créé sur le script "ServeurInputGraphique" : output$affichage_choix_graph
output$affichage_graphique <- renderUI({
  plotOutput(input$choix_graph)
})



#graphiques univariés pour années unique sur données quantitatives

#donnée utilisés pour ces graphiques lorsque année unique avec plus de 10 individus
data_annee_unique <- reactive({
  data()[data()[[input$temporellevar]] == input$choix_annee,]
})


#code création du boxplot
#!!!Si plus de 10 individus par année, on demande bien de préciser l'année via un slider et fait le graphique uniquement sur cette année-là!!!
#!!!sinon, on fait un graphique en ne prennant pas en compte l'année, soit un graph représentant la distribution de notre indicateur dans la base de données!!!
output$boxplot <- renderPlot({
  if (nb_individus > 10) {
    boxplot(data_annee_unique()[[input$selectedvar[1]]], horizontal = TRUE, xlab = input$selectedvar[1])
  } else {
    boxplot(data()[[input$selectedvar[1]]], horizontal = TRUE, xlab = input$selectedvar[1])
  }
})


#code création du beeswarm
output$beeswarm <- renderPlot({
  if (nb_individus > 10) {
    beeswarm(data_annee_unique()[[input$selectedvar[1]]], horizontal = TRUE, xlab = input$selectedvar[1])
  } else {
    beeswarm(data()[[input$selectedvar[1]]], horizontal = TRUE,  xlab = input$selectedvar[1])
  }
})


#code création du diagramme en barre
#A REFAIRE MIEUX / AMELIORER (exemple : affichage des noms de territoires correspondant aux plus hautes valeurs)
output$barchart <- renderPlot({
  if (nb_individus > 10) {
    data_boxplot <- data_annee_unique()[order(data_annee_unique()[[input$selectedvar[1]]],decreasing = TRUE),]
    barplot(data_boxplot[[input$selectedvar[1]]], names = data_boxplot$dep_resid, xlab = input$territoirevar, ylab = input$selectedvar[1],)
  } else {
    data_boxplot <- data()[order(data()[[input$selectedvar[1]]],decreasing = TRUE),]
    data_boxplot$combine <- paste(data_boxplot$dep_resid, data_boxplot[[input$temporellevar]], sep = " ")
    barplot(data_boxplot[[input$selectedvar[1]]], names = data_boxplot$combine, xlab = input$territoirevar, ylab = input$selectedvar[1],)
  }
})


#code création de l'histogramme
output$histogramme <- renderPlot({
  if (nb_individus > 10) {
    hist(data_annee_unique()[[input$selectedvar[1]]], xlab = input$selectedvar[1], main ="")
  } else {
    hist(data()[[input$selectedvar[1]]], xlab = input$selectedvar[1], main ="")
  }
})



#graphiques univariés pour série temporelle sur données quantitatives
#!!!il faudrait empécher la création du graphique si il y a moins de 10 individus (pas assez de valeurs), c'est une amélioration qui n'a pas encore été géré!!!

#lorsque "Série temporelle" est sélectioné, permet de ne sélectionner que les années souhaitées pour les données utilisées dans les graphiques
data_serie_temp <- reactive({
  data()[data()[[input$temporellevar]] %in% input$multi_annee,]
})


#code création du boxplot sur différentes années
output$boxplot2 <- renderPlot({
  boxplot(data_serie_temp()[[input$selectedvar[1]]] ~ data_serie_temp()[[input$temporellevar]], data = data_serie_temp(), xlab = "Annee", ylab = input$selectedvar[1])
})


#code création du beeswarm sur différentes années
output$beeswarm2 <- renderPlot({
  beeswarm(data_serie_temp()[[input$selectedvar[1]]] ~ data_serie_temp()[[input$temporellevar]], data = data_serie_temp(), xlab = "Annee", ylab = input$selectedvar[1])
})


#code création des courbes (par année et sur individus séléctionnées)
output$courbes <- renderPlot({
  tri_territoire <- data_serie_temp()[data_serie_temp()$dep_resid %in% input$choix_geo,]
  #creation d'une matrice où chaque colonne correspond à l'une des régions choisis
  mtx <- pivot_wider(tri_territoire[c("Annee","dep_resid",input$selectedvar[1])], names_from = dep_resid, values_from = input$selectedvar[1])
  mtx <- mtx[,-1]
  #utilise chaque colonne de la matrice pour créer une ligne/courbe dans le graphique
  try(matplot(x = unique(data_serie_temp()[[input$temporellevar]]), y = mtx, type = "b", xlab = "Année", ylab = input$selectedvar[1], pch=1, col = 1:length(input$choix_geo)))
  try(legend("topright", legend = unique(tri_territoire$dep_resid), col=1:length(input$choix_geo), pch=1))
})



#graphiques bivariés pour années unique sur données quantitatives


#code création du nuage de point
output$nuage <- renderPlot({
  if (nb_individus > 10) {
    plot(x = data_annee_unique()[[input$selectedvar[1]]], y = data_annee_unique()[[input$selectedvar[2]]], xlab = input$selectedvar[1], ylab = input$selectedvar[2])
  } else {
    plot(x=data()[[input$selectedvar[1]]], y=data()[[input$selectedvar[2]]], xlab = input$selectedvar[1], ylab = input$selectedvar[2])
  }
})


#code création du scatter plot
output$scatter <- renderPlot({
  if (nb_individus > 10) {
    plot <- ggplot(data_annee_unique(), aes(x=data_annee_unique()[[input$selectedvar[1]]], y=data_annee_unique()[[input$selectedvar[2]]]))+
      geom_point() +
      theme(legend.position="none") +
      xlab(input$selectedvar[1]) + ylab(input$selectedvar[2])
  } else {
    plot <- ggplot(data(), aes(x=data()[[input$selectedvar[1]]], y=data()[[input$selectedvar[2]]]))+
      geom_point()+
      theme(legend.position="none") +
      xlab(input$selectedvar[1]) + ylab(input$selectedvar[2])
  }
  ggMarginal(plot, type="histogram")
})



#graphiques bivariés pour série temporelle sur données quantitatives

#code création du nuage de points
output$nuage1 <- renderPlot({
  ggplot(data_serie_temp(), aes(x = data_serie_temp()[[input$selectedvar[1]]], y=data_serie_temp()[[input$selectedvar[2]]], col = factor(data_serie_temp()[[input$temporellevar]]))) +
    geom_point() +
    xlab(input$selectedvar[1]) + ylab(input$selectedvar[2])
})


#code création du nuage de points par individus
output$nuage2 <- renderPlot({
  tri_territoire <- data_serie_temp()[data_serie_temp()$dep_resid %in% input$choix_geo,]
  ggplot(tri_territoire, aes(x = tri_territoire[[input$selectedvar[1]]], y=tri_territoire[[input$selectedvar[2]]], col = factor(tri_territoire[[input$temporellevar]]))) +
    geom_point(size=4) + geom_line(aes(group = tri_territoire$dep_resid),color = "black", size=0.5) +
    xlab(input$selectedvar[1]) + ylab(input$selectedvar[2])
})
