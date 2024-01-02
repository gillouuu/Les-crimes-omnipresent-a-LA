source(file = "global.R")
source(file = "Packages.R")
library(shiny)

# global.R
library(ggplot2)

# Packages.R
# Assurez-vous d'avoir toutes les bibliothèques nécessaires installées et chargées ici

# ui.R
ui <- fluidPage(
  useShinyjs(),  # Chargement de shinyjs
  div(
    h1("Les crimes à LA en fonction des victimes", align = "center"),
    sidebarLayout(
      sidebarPanel(width = 3,
                   h3("Sélectionnez la victime"),
                   h5("Sélectionnez le sexe"),
                   checkboxGroupInput("sexe", "", choices = c("Homme"="M","Femme"="F","Autre"="X")),
                   br(),
                   br(),
                   h5("Sélectionnez l'âge"),
                   sliderInput("age","",0,120,c(25,75)),
                   br(),
                   br(),
                   h5("Sélectionnez l'ethnie"),
                   checkboxGroupInput("ethnie","", choices = c("Latinos","Blancs","Noirs","Asiatiques","Autres","Non spécifié")),
      ),
      
      mainPanel(
        h4("Nombre total de crimes commis sur la victime en question:"),
        # Utilisation d'un KPI
        infoBox("Total de crimes", value = textOutput("totalCrimesText")),
        textOutput("selectedFilters"),
        textOutput("uniqueSexValues"),
        textOutput("filteredDataCount"),
        textOutput("percentageText"),
        # Ajout de l'emplacement pour le graphique à barres
        plotOutput("crimeBarChart")
      )
    )
  )
)

# server.R
library(shiny)

server <- function(input, output) {
  filteredData <- reactive({
    subset_data <- subset(data,
                          data$`Vict.Sex` %in% input$sexe &
                            data$`Vict.Age` >= input$age[1] & data$`Vict.Age` <= input$age[2])
    
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
    paste(totalCrimes)
  })
  
  output$percentageText <- renderText({
    totalCrimes <- nrow(filteredData())
    totalRowsFiltered <- nrow(data[data$`Vict.Age` != -2 & data$`Vict.Sex` != "H",])
    percentage <- (totalCrimes / 735648) * 100
    paste("Pourcentage des crimes sélectionnés par rapport au total :", round(percentage, 2), "%")
  })
  
  # Code pour créer le graphique à barres
  output$crimeBarChart <- renderPlot({
    ggplot(filteredData(), aes(x = `Crm.Cd.Desc`)) +
      geom_bar() +
      labs(title = "Répartition des crimes", x = "Type de Crime", y = "Nombre de Crimes")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Incliner les étiquettes sous les barres
  })
}

# shinyApp
shinyApp(ui = ui, server = server)



