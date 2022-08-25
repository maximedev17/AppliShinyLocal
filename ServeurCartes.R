#partie serveur concernant la page Carte


#Permet à l'utilisateur de choisir la variable qu'il souhaite visualiser sur la carte
output$varchoice_maps <- renderUI({
  data()
  tagList(
    selectInput(inputId="selectedvarm",
                label="Variables à visualiser : ",
                choices=nom_indicateurs()),
    #si la variable à visualiser est spécifiée, on permet à l'utilisateur de corriger le type de cette variable trouvé par défault
    conditionalPanel(condition="input.selectedvarm!=''",
                     uiOutput("vartype_maps")
    )
  )
})


#Attribut par défaut un type à la variable visualisée et permet à l'utilisateur de corriger ce type au besoin
#L'attribution par défault se fait en fonction du type attribuer par R aux variables lors du téléchargement des données
#Exemple, si le type de la variable importée est numeric, on lui attribura par défault le type "Qualitative ratio"
output$vartype_maps <- renderUI({
  list_type <- c("Quantitative stock" = "integer",
                 "Quantitative ratio" = "numeric",
                 "Qualitative nominale" = "character",
                 "Qualitative ordinale" = "ordinale")
  selectInput(inputId="vartype",
              label=paste0("type de la variable : ",input$selectedvarm),
              choices=list_type, selected = is(data()[[input$selectedvarm]])[1])
})


#l'appel de cet output est aussi bloqué partie UI, c'est ici qu'il faudrait coder les options que l'utilisateur pourrait choisir pour le graphique (exemple, palette de couleur)
#output$affichage_choix_maps <- renderUI({
#  "options à ajouter"
#})


#création du fond de carte avec frontières département (pas encore appelé, juste créé)
dep <- reactive({
  st_read("geodata.gpkg", layer = 'dep')
})


#création du fond de carte avec frontières pays
pays <- reactive({
  st_read("geodata.gpkg", layer = 'pays')
})


#création et affichage de la carte
output$affichage_maps <- renderPlot({
  #Choix du type de carte en fonction du type de variable
  if (input$vartype == "integer"){
    maptype = "prop"
  } else if (input$vartype == "numeric"){
    maptype = "choro"
  } else {
    maptype = "typo"
  }
  
  #liaison de ma table de données et celle avec les informations nécessaires pour créer les forme des département (map)
  dep_avec_donnees <- merge(dep(), data()[data()[[input$temporellevar]] == input$choix_annee_maps,], by.x = "INSEE_DEP", by.y = input$territoirevar, all.x = TRUE)

  #créer une visualisation
  mf_theme("default")
  #centrer sur les departement
  mf_init(dep())
  #ajouter le fond des pays
  mf_map(pays(), add = T)
  #rajouter les contour des département (utile lorsque stock)
  mf_map(dep(), add = T)
  # tracer les indicateurs sur la carte 
  mf_map(dep_avec_donnees, var= input$selectedvarm, type = maptype, leg_pos = c(-1999.10521069368, 6625761.00876683),leg_title_cex = 1, leg_val_cex = 0.8, add = T)
  #rajouter certaines options
  mf_scale(100)
  mf_credits(txt = "(C) EuroGeographics pour les limites administratives",
             col = "black", bg = "white")
}, height = 800)
#La ligne précédante précise la hauteur du graphique rendu (représenatnt la carte)


#création du slider pour choisir l'année de représentation sur la carte
output$sliderAnneeMaps <- renderUI({
  if (nb_individus > 10){
    sliderTextInput("choix_annee_maps","Choisi l'année :", choices = unique(data()[[input$temporellevar]]), animate = TRUE)
  } else {
    h4("Pas assez de données pour faire une représentation par année")
  }
})