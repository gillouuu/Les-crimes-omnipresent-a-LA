source(file = "global.R")
source(file = "Packages.R") #lien vers mes autres fichiers nécessaires au fonctionnement


ui <- fluidPage( #création d'une fluidPage
  useShinyjs(),  # Chargement de shinyjs
  div(
    h1("Les crimes à LA en fonction des victimes", align = "center",style = "text-decoration: underline;"),
    br(),
    br(), #Mise en place du titre avec des sauts de lignes pour la lisibilité
    
    # Mise en place de 3 widgets pour la sélection de la victime
    sidebarLayout(
      sidebarPanel( #partie interactive 
        h3("Sélectionnez la victime"),
        h5("Sélectionnez le sexe"),
        checkboxGroupInput("sexe", "", choices = c("Homme"="M","Femme"="F","Autre"="X","Non spécifié"="H")),
        br(),
        br(),
        h5("Sélectionnez l'âge"),
        sliderInput("age","",0,120,c(25,75)),
        br(),
        br(),
        h5("Sélectionnez l'ethnie"),
        checkboxGroupInput("ethnie","", choices = c("Blancs"="W","Noirs"="B","Hispaniques"="H","Autres"="O","Non spécifié"="X")),
      ),
      mainPanel( #partie affichage
        tabsetPanel(
          id = "onglets",  # Identifiant pour les onglets
          type = "tabs",  # Onglets horizontaux
          
          tabPanel("Statistiques",icon = icon("percent"), #onglet un contenant deux calculs et une image 
                   br(),
                   textOutput("totalCrimesText"),
                   br(),
                   textOutput("percentageText"),
                   br(),
                   style = "text-align: center;",
                   img(src = "LA.png", height = 500, width = 500, align = "center")
          ),
          

          tabPanel("Graphique des crimes", icon = icon("gun"),# onglet 2 contenant un graphique
                   br(),
                   br(),
                   br(),
                   plotOutput("crimeBarChart")
          ),
          

          tabPanel("Graphique des zones",icon = icon("chart-area"), # onglet 3 contenant un graphique et une heatmap
                   br(),
                   br(),
                   br(),
                   br(),
                   plotOutput("crimeAreaChart"),
                   br(),
                   br(),
                   leafletOutput("heatmap"),
                   br(),
                   br()
                   
          ),
          tabPanel("Répartition des mois", icon = icon("calendar"), # onglet 4 contenant deux graphiques
                   br(),
                   br(),
                   plotOutput("crimeYearChart"),
                   p(em("L'année 2023 représente moins de crimes car les données s'arrêtent avant la fin de l'année")),
                   br(),
                   br(),
                   plotOutput("crimeMonthChart")
          ),
        )
      )
    )
  )
)


# partie server, donc invisible dans l'app, qui retrace les calculs
server <- function(input, output, session) {
  
# création d'un filtre basé sur la sélection faite dans la partie interactive
  filteredData <- reactive({
    subset_data <- subset(data,
                          data$`Vict.Sex` %in% input$sexe &
                            data$`Vict.Age` >= input$age[1] & data$`Vict.Age` <= input$age[2] &
                            data$Vict.Descent %in% input$ethnie)
    
    
# calcul du ombre de crimes grâce au nombre d'occurence
    crime_counts <- table(subset_data$`Crm.Cd.Desc`)
    
# Sélection des 15 types de crime les plus fréquents par soucis de lisibilité
    top_crimes <- names(sort(crime_counts, decreasing = TRUE)[1:15])
    
# Filtrer les données uniquement pour les 15 types de crime les plus fréquents
    subset_data <- subset_data[subset_data$`Crm.Cd.Desc` %in% top_crimes, ]
    
    return(subset_data)
  })
  # création d'un calcul et d'une phrase pour son affichage
  output$totalCrimesText <- renderText({
    totalCrimes <- nrow(filteredData())
    paste("Nombre total de crimes commis sur la victime en question :", totalCrimes)
  })
  # création d'un calcul et d'une phrase pour son affichage
  output$percentageText <- renderText({
    totalCrimes <- nrow(filteredData())
    totalRowsFiltered <- nrow(data[data$`Vict.Age` != -2 & data$`Vict.Sex` != "H",])
    percentage <- (totalCrimes / 543934) * 100
    paste("Pourcentage des crimes sélectionnés par rapport au total :", round(percentage, 2), "%")
  })
  
  # création d'un graphique à barres 
  output$crimeBarChart <- renderPlot({
    ggplot(filteredData(), aes(x = `Crm.Cd.Desc`, fill = `Crm.Cd.Desc`)) +
      geom_bar() +
      labs(title = "Répartition des crimes", x = "Type de Crime", y = "Nombre de Crimes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      coord_cartesian(ylim = c(0, max(table(filteredData()$`Crm.Cd.Desc`)) * 1.2))  # agrandir le graphique pour la lisibilité
  }, height = 600)
  
  
  # Nouvelle fonction réactive pour les données filtrées par zone
  filteredDataByArea <- reactive({
    subset_data <- subset(data,
                          data$`Vict.Sex` %in% input$sexe &
                            data$`Vict.Age` >= input$age[1] & data$`Vict.Age` <= input$age[2] &
                            data$Vict.Descent %in% input$ethnie)
    
    return(subset_data)
  })
  
  # création d'un graphique à barres
  output$crimeAreaChart <- renderPlot({
    ggplot(filteredDataByArea(), aes(x = `AREA.NAME`, fill =`AREA.NAME`)) +
      geom_bar(position = "stack") +
      labs(title = "Répartition des crimes par zone", x = "Zone", y = "Nombre de Crimes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Incliner les étiquettes sous les barres
  })
  
  
  # création d'un graphique à barres  
  output$crimeYearChart <- renderPlot({
    ggplot(filteredData(),aes(x = year, fill = year)) +
      geom_bar() +
      labs(title = "Répartition des crimes en fonction des années ", x = "année", y = "Nombre de Crimes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
  })
  # création d'un graphique à barres  
  output$crimeMonthChart <- renderPlot({
    ggplot(filteredData(),aes(x = month, fill = `month`)) +
      geom_bar() +
      labs(title = "Répartition des crimes en fonction des mois ", x = "mois", y = "Nombre de Crimes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
  })
  # création d'une heatmap 
  center_lat_h <- reactive({median(filteredData()$LAT)})
  center_lon_h <- reactive({median(filteredData()$LON)})# choisir le lieu de zoom par défaut(LA)
  output$heatmap <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles()%>% 
      addTiles() %>%
      addHeatmap(
        lat = ~LAT,
        lng = ~LON,
        blur = 20,
        max = 0.2,
        radius = 10
      )    %>%
      setView(lat = center_lat_h(), lng = center_lon_h(), zoom = 9)
  })
  
  
  
}


shinyApp(ui = ui, server = server) # définition de l'App sans quoi rien ne marche 




