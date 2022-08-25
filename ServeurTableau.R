
#partie serveur ne concernant que la page Tableau de l'application

#Pour les variables de la base de données étudiée carractérisant l'année et le niveau géographique, permet de trier la table/database selon les modalités
output$varchoice_table <- renderUI({
  #tagList permet de renvoyer plusieurs Outputs avec un seul render
  tagList(
      pickerInput(inputId=input$territoirevar,
                  label=paste0("choix des modalités pour la variable : ",input$territoirevar),
                  choices=unique(data()[,input$territoirevar]),
                  options = list(`actions-box` = TRUE, `live-search` = TRUE),
                  multiple = TRUE),
      pickerInput(inputId=input$temporellevar,
                  label=paste0("choix des modalités pour la variable : ",input$temporellevar),
                  choices=unique(data()[,input$temporellevar]),
                  options = list(`actions-box` = TRUE, `live-search` = TRUE),
                  multiple = TRUE)
  )
}
)


# tri la table affichée selon les modalitées sélectionnées
output$table <- renderDataTable({
  data_conditionnel <- data()
  if (is.null(input[[input$territoirevar]]) == FALSE) {
    data_conditionnel <- data_conditionnel[data_conditionnel[[input$territoirevar]] %in% input[[input$territoirevar]],]
  }
  if (is.null(input[[input$temporellevar]]) == FALSE) {
    data_conditionnel <- data_conditionnel[data_conditionnel[[input$temporellevar]] %in% input[[input$temporellevar]],]
  }
  #creation de la base de données filtré que l'on va pouvoir importer par la suite grace au Download bouton situé sous la table affichée
  filter_data <<- data_conditionnel
  datatable(data_conditionnel, options = list(lengthMenu = c(20,30,50),orderClasses = TRUE))
  #!!! il y a aussi la possibilité de coder facilement pour ne montrer que les colonnes/variables voulut mais à voir si c'est nécessaire/utile!!!
})


#output pour le bouton de téléchargement des données sélectionnées
output$downLoadFilter <- downloadHandler(
  filename = function() {
    paste('Filtered data-', Sys.Date(), '.csv', sep = '')
  },
  content = function(file){
    write.csv(filter_data,file)
  }
)