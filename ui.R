#
# La partie UI, ce script, défini l'interface utilisateur de l'application Shiny
# Vous pouvez lancer l'application en cliquant sur le bouton 'Run App' en haut à droite.
#
# Pour plus d'information sur la création de l'interface, vous pouvez visiter le site suivant :
#
#    http://shiny.rstudio.com/
#

library(shiny)

#Définition de la partie UI
#CHAQUE APPEL A UN OUTPUT RENVOIE A UN OBJET CREE DANS LA PARTIE SERVEUR !!! Il faut donc aller la chercher pour plus d'informationss
shinyUI(fluidPage(
  theme = "cerulean",
  sidebarLayout(
    
    #création du sidebar
    sidebarPanel(
      #le premier wellPanel (sert à encadrer les objets créés à l'intérieur) est affiché dès le lancement de l'application
      #il concerne le choix et l'import de la base de donnée
      #une fois le bouton valider sélectionné, stocké dans la variable réactive data() à voir partie serveur, un dataframe est créé sur R avec les données choisis et tout le reste de l'application (tableau, graphiques, cartes) s'appuie sur ce dataframe
      wellPanel(
        fileInput('target_upload', 'Choose file to upload',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    '.csv'
                  )),
        #Sert à préciser le format du fichier csv utilisé
        radioButtons("separator","Separateur: ",choices = c(";",",",":"), selected=";",inline=TRUE),
        radioButtons("decimale","Decimale: ",choices = c(".",","), selected=".",inline=TRUE),
        radioButtons("encod","Encodage: ",choices = c("UTF-8","latin1"), selected="UTF-8",inline=TRUE),
        #afficherassociation prend la valeur 'Oui' seulement si le téléchargement du fichier csv est un succès et donc qu'un dataframe utilisable (data()) a été stocké
        conditionalPanel(condition="output.afficherassociation == 'Oui'",
                         #si cette condition est vérifiée, on affiche la suite, demandant à l'utilisateur de spécifier la variable de son fichier csv caractérisant l'Année et celle caractérisant le niveau géographique
                         uiOutput("association")
        )
      ),
      
      
      
      
      #afficher prend la valeur 'Oui' uniquement si l'utilisateur à précisé la variable caractérisant l'Année et celle caractérisant le niveau géographique
      conditionalPanel(condition="output.afficher == 'Oui'",
                       #si cette condition est vérifiée, on affiche le reste de l'application
                       br(),
                       wellPanel(
                         #dans un nouveau wellPanel, j'affiche des inputs différents selon les information dont j'ai besoin dans la page sélectionnée
                         conditionalPanel(condition="input.windows=='Tableau'",
                                          h4("Vous pouvez préciser les modalités vous intéressant parmi les variables de cette base de données"),
                                          br(),
                                          uiOutput("varchoice_table")
                         ),
                         conditionalPanel(condition="input.windows=='Graphique'",
                                          h4("Précisez les informations que vous voulez visualiser"),
                                          br(),
                                          uiOutput("varchoice_graph")
                         ),
                         conditionalPanel(condition="input.windows=='Carte'",
                                          h4("Précisez la variable à visualiser"),
                                          br(),
                                          uiOutput("varchoice_maps")
                         )
                       )  
      )),
    
    
    
    
    #cette partie concerne l'affichage des résultats dans le mainPanel
    mainPanel(
      #tout comme une partie du sidebarPanel, le mainPanel n'est afficher que lorsque afficher prend la valeur 'Oui' (voir ligne 44)
      conditionalPanel(condition="output.afficher == 'Oui'",
                       #On affichera différents outputs selon la page (/le panel) séléctionnée
                       tabsetPanel(id = "windows",
                                   tabPanel("Tableau de données", value = "Tableau",
                                            br(),
                                            #h4("Table"),
                                            #permet de créer un 'scroll' si le tableau a trop de colonne, soit de faire défiler
                                            fluidRow(style='overflow-x: scroll',
                                              dataTableOutput("table")
                                            ),
                                            #permet de centrer le bouton de téléchargement des données
                                            column(12, align="center", id="buttons",
                                                   downloadButton('downLoadFilter',"Telecharger les donnees filtrees")
                                            )
                                   ),
                                   tabPanel("Graphique", value = "Graphique", 
                                            conditionalPanel(condition="input.selectedvar==''",
                                                             h2("Selectionner une variable dans la liste a gauche intitulee : choix des variables a visualiser.")
                                            ),
                                            conditionalPanel(condition="input.selectedvar!=''",
                                                             fluidRow(
                                                               uiOutput("affichage_choix_graph")
                                                             ),
                                                             fluidRow(
                                                               #la ligne commenté ci-dessous permettrait de modifier la grosseur du graphique affiché (choix a été fait de ne pas l'utiliser)
                                                               #style='height:400px',
                                                               uiOutput("affichage_graphique")
                                                             ),
                                                             conditionalPanel(condition="input.temps == 'Année unique'",
                                                                              fluidRow(column(12, align="center",uiOutput("sliderAnnee2")))
                                                             ),
                                                             conditionalPanel(condition="input.temps == 'Série temporelle'",
                                                                              fluidRow(column(6, align="center", uiOutput("sliderTemp")),
                                                                                       column(6, align="center", uiOutput("precisionAnnee")))
                                                             )
                                                             #A IMPLEMENTER, puis pareil dans partie carte
                                                             #,
                                                             #column(12, align="center", id="buttons",
                                                             #      downloadButton('downLoadPlot',"Download the Plot")
                                                             #)
                                            )
                                   ),
                                   tabPanel("Map", value = "Carte",
                                            #la sélection d'un premier indicateur est fait automatiquement dans la liste de "variable a visualiser", c'est un choix discutable et modifiable au besoin
                                              conditionalPanel(condition="input.selectedvarm!=''",
                                                               #le output est aussi commenté sur le script "serveurCartes" ; l'idée est que c'est ici que l'on rajouterait la possibilitée à l'utilisateur de choisir des options sur l'affichage du graphique
                                                               #fluidRow(
                                                               #   uiOutput("affichage_choix_maps")
                                                               #),
                                                               fluidRow(class = "myrow1",
                                                                 #préciser class au fluidRow permet de modifier sa taille et autres paramètres. Il faut pour cela créer cette classe et préciser ces attributs (voir plus bas, l'emplacement est important, ligne 133)
                                                                 plotOutput("affichage_maps")),
                                                               fluidRow(
                                                                 column(12, align="center",uiOutput("sliderAnneeMaps")))
                                                               )
                                   )
                       )
      )
    )
  ),tags$head(tags$style("
      .myrow1{height:800px;}"))
  ))


#CHAQUE APPEL A UN OUTPUT RENVOIE A UN OBJET CREE DANS LA PARTIE SERVEUR !!! Il faut donc aller la chercher pour plus d'informationss
