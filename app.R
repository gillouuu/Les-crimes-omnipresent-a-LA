source(file = "global.R")
source(file = "Packages.R")


ui <- fluidPage(
  useShinyjs(),  # Chargement de shinyjs
  div(
    h1("Les crimes à LA en fonction des victimes", align = "center",style = "text-decoration: underline;"),
    br(),
    br(),
    
    # Widget pour la sélection de la victime
    sidebarLayout(
      sidebarPanel(
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
      mainPanel(
        # Onglets principaux
        tabsetPanel(
          id = "onglets",  # Identifiant pour les onglets
          type = "tabs",  # Onglets horizontaux
          
          # Onglet 1 : Statistiques (Nombre de crimes et pourcentage)
          tabPanel("Statistiques",icon = icon("percent"),
                   br(),
                   textOutput("totalCrimesText"),
                   br(),
                   textOutput("percentageText"),
                   br(),
                   style = "text-align: center;",
                   img(src = "LA.png", height = 500, width = 500, align = "center")
          ),
          
          # Onglet 2 : Graphique à barres
          tabPanel("Graphique des crimes", icon = icon("gun"),
                   br(),
                   br(),
                   br(),
                   plotOutput("crimeBarChart")
          ),
          
          # Onglet 3 : Graphique par Zone
          tabPanel("Graphique des zones",icon = icon("chart-area"),
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
          tabPanel("Répartition des mois", icon = icon("calendar"),
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


# server.R
server <- function(input, output, session) {
  
  # Résultats basés sur la sélection
  filteredData <- reactive({
    subset_data <- subset(data,
                          data$`Vict.Sex` %in% input$sexe &
                            data$`Vict.Age` >= input$age[1] & data$`Vict.Age` <= input$age[2] &
                            data$Vict.Descent %in% input$ethnie)
    
    
    # Comptage des occurrences de chaque type de crime
    crime_counts <- table(subset_data$`Crm.Cd.Desc`)
    
    # Sélection des 15 types de crime les plus fréquents
    top_crimes <- names(sort(crime_counts, decreasing = TRUE)[1:15])
    
    # Filtrer les données uniquement pour les 15 types de crime les plus fréquents
    subset_data <- subset_data[subset_data$`Crm.Cd.Desc` %in% top_crimes, ]
    
    return(subset_data)
  })
  
  output$totalCrimesText <- renderText({
    totalCrimes <- nrow(filteredData())
    paste("Nombre total de crimes commis sur la victime en question :", totalCrimes)
  })
  
  output$percentageText <- renderText({
    totalCrimes <- nrow(filteredData())
    totalRowsFiltered <- nrow(data[data$`Vict.Age` != -2 & data$`Vict.Sex` != "H",])
    percentage <- (totalCrimes / 543934) * 100
    paste("Pourcentage des crimes sélectionnés par rapport au total :", round(percentage, 2), "%")
  })
  
  # Code pour créer le graphique à barres
  output$crimeBarChart <- renderPlot({
    ggplot(filteredData(), aes(x = `Crm.Cd.Desc`, fill = `Crm.Cd.Desc`)) +
      geom_bar() +
      labs(title = "Répartition des crimes", x = "Type de Crime", y = "Nombre de Crimes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      coord_cartesian(ylim = c(0, max(table(filteredData()$`Crm.Cd.Desc`)) * 1.2))  # Ajuster la plage y
  }, height = 600)
  
  
  # Nouvelle fonction réactive pour les données filtrées par zone
  filteredDataByArea <- reactive({
    subset_data <- subset(data,
                          data$`Vict.Sex` %in% input$sexe &
                            data$`Vict.Age` >= input$age[1] & data$`Vict.Age` <= input$age[2] &
                            data$Vict.Descent %in% input$ethnie)
    
    return(subset_data)
  })
  
  # Nouvelle sortie de graphique pour la répartition des crimes par zone
  output$crimeAreaChart <- renderPlot({
    ggplot(filteredDataByArea(), aes(x = `AREA.NAME`, fill =`AREA.NAME`)) +
      geom_bar(position = "stack") +
      labs(title = "Répartition des crimes par zone", x = "Zone", y = "Nombre de Crimes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Incliner les étiquettes sous les barres
  })
  
  
  
  output$crimeYearChart <- renderPlot({
    ggplot(filteredData(),aes(x = year, fill = year)) +
      geom_bar() +
      labs(title = "Répartition des crimes en fonction des années ", x = "année", y = "Nombre de Crimes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
  })
  
  output$crimeMonthChart <- renderPlot({
    ggplot(filteredData(),aes(x = month, fill = `month`)) +
      geom_bar() +
      labs(title = "Répartition des crimes en fonction des mois ", x = "mois", y = "Nombre de Crimes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
  })
  
  center_lat_h <- reactive({median(filteredData()$LAT)})
  center_lon_h <- reactive({median(filteredData()$LON)})
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

# shinyApp
shinyApp(ui = ui, server = server)




