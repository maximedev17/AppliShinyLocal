#Différents outputs nécessaires pour la page graphique 
#Sauf code de création des différents graphiques qui est dans le script "ServeurOutputGraphique"


#Choix d'un ou deux indicateurs/variables à visualiser graphiquement 
output$varchoice_graph <- renderUI({
  tagList(
    pickerInput(inputId = 'selectedvar',
                label = "Variables a visualiser : ",
                choices = nom_indicateurs(),
                multiple = TRUE,
                options = list(`max-options` = 2,`max-options-text` = "Vous ne pouvez pas visualiser plus de 2 variables en même temps !", `live-search` = TRUE)),
    #Permet aussi de préciser le type de variable si l'attribution par défault est incorrecte
    conditionalPanel(condition="input.selectedvar!=''",
                     uiOutput("vartype_graph"))
    )
  })
  

#Une fois les variable à visualiser sélectionnées, création d'une nouvelle liste déroulante (input) permettant à l'utilisateur de modifier le type par défault de la variable, au besoin
output$vartype_graph <- renderUI({
  #en créant la liste ainsi, un variable importé comme "integer" correspondant à du quantitative stock prendra la première valeure de la liste par défault, soit bien "Quantitative"
  list_type <- c("Quantitative" = "numeric",
                 "Qualitative nominale" = "character",
                 "Qualitative ordinale" = "chr2")
  #input$selectedvar[1] <- nom de la première variable sélectionnée
  #input$selectedvar[2] <- nom de la seconde variable sélectionnée
  tagList(
    if (input$selectedvar[1] != ""){
      selectInput(inputId="var_x",
                  label=paste0("type de la variable : ",input$selectedvar[1]),
                  choices=list_type, selected = is(data()[[input$selectedvar[1]]])[1])
    },
    if (length(input$selectedvar) == 2){
      selectInput(inputId="var_y",
                  label=paste0("type de la variable : ",input$selectedvar[2]),
                  choices=list_type, selected = is(data()[[input$selectedvar[2]]])[1])
    },
    br(),
    #On demande aussi à l'utilisateur comment il veut visualiser les indicateurs, année par année ou comparaison de différentes années dans un même graphique
    wellPanel(
      selectInput('temps','Comment utiliser la variable Année ?',choices=c("Année unique","Série temporelle"))
    )
  )
})


#cas année unique

#slider permettant à l'utilisateur de séléctionner une année unique pour laquelle il veut visualiser les données
output$sliderAnnee2 <- renderUI({
  #si moins de 10 observations par années, une visualisation graphique par année n'est plus très utile (choix fait, le 10 peut être modifié) 
  if (nb_individus > 10){
    sliderTextInput("choix_annee","Choisi l'année :", choices = unique(data()[[input$temporellevar]]), animate = TRUE)
  } else {
    h4("Pas assez de données pour faire une représentation par année, le graphique ci-dessus est donc pour toute années confondues")
  }
})


#cas série temporelle

#slider permettant de renseigner une période d'intérré lorsque l'on veut comparer un indicateur sur plusieurs année
output$sliderTemp <- renderUI({
  sliderTextInput("choix_periode","Choisi la période :",choices = unique(data()[[input$temporellevar]]), selected = c(min(unique(data()[[input$temporellevar]])),max(unique(data()[[input$temporellevar]]))))
})


#liste déroulante avec les différentes années pour lesquelles les données sont disponible dans la période rensaignée précédemment
#permet notamment de comparer l'indicateur séléctionné sur des années spécifiques, éloignées temporellement
output$precisionAnnee <- renderUI({
  periode <- unique(data()[[input$temporellevar]])
  periode <- periode[periode >= input$choix_periode[1] & periode <= input$choix_periode[2]]
  pickerInput(inputId="multi_annee",
              label=paste0("préciser les années d'intérêt : "),
              choices=periode,
              selected=periode,
              #maximum d'années à visualiser sur un même graphique limité à 6 (choix pouvant être modifié ci-dessous)
              options = list(`actions-box` = TRUE,`max-options` = 6,`max-options-text` = "Vous ne pouvez pas sélectionner plus de 6 années !", `live-search` = TRUE),
              multiple = TRUE)
})


#Liste déroulante avec les différents graphiques visualisables
#Les graphiques proposés à l'utilisateur dépendent du type de variables et de visualisation voulu ("Année unique" ou "Série temporelle)
output$affichage_choix_graph <- renderUI({
  if (input$temps == "Année unique" & length(input$selectedvar) == 1){
    list_graph <-c("Boîte à moustache" = "boxplot", "Beeswarm" = "beeswarm", "Diagramme en barre" = "barchart", "Histogramme" = "histogramme")
  }
  if (input$temps == "Série temporelle" & length(input$selectedvar) == 1){
    list_graph <-c("Boîte à moustache" = "boxplot2", "Beeswarm" = "beeswarm2", "Courbes" = "courbes")
  }
  if (input$temps == "Année unique" & length(input$selectedvar) == 2){
    list_graph <-c("Nuage de points" = "nuage", "Scatter plot" = "scatter")
  }
  if (input$temps == "Série temporelle" & length(input$selectedvar) == 2){
    list_graph <-c("Nuage de points" = "nuage1", "Nuage de point par individu" = "nuage2")
  }
  tagList(
    column(4, wellPanel(
      selectInput(inputId="choix_graph",
                  label="Choix du graphique : ",
                  choices= list_graph)
    )),
    #permet à l'utilisateur, pour certain graphiques spécifier, de préciser les territoires géographiques d'intérêt
    conditionalPanel(condition="input.choix_graph=='courbes' || input.choix_graph=='nuage2'",
                     column(8, wellPanel(
                       pickerInput(inputId="choix_geo",
                                   label=paste0("préciser les territoires d'intérêt : "),
                                   choices=unique(data()$dep_resid),
                                   selected=unique(data()$dep_resid)[1:3],
                                   options = list(`max-options` = 6,`max-options-text` = "Vous ne pouvez sélectionner que 6 territoires !", `live-search` = TRUE),
                                   multiple = TRUE))
                       #,
                       #c'est donc ici que l'on pourrait implémenter différentes options spécifiques à différents graphiques (exemple : couleur, taille...)
                       #conditionalPanel(condition="input.choix_graph!='courbes'",
                       #                p("Options à coder"))
                     ))
  )
})


#Gérer téléchargement des graphiques !!!!!
#output$downLoadPlot <- downloadHandler(
#  filename = function() {
#    paste("plot-", Sys.Date(), ".png", sep="")
#  },
#  content = function(file) {
#    ggsave(affichage_graphique())
#  }
#)