source(file = "global.R")
source(file = "Packages.R")

ui <- fluidPage(
  div(
    h1("Les crimes à LA en fonction des victimes", align = "center"),
    sidebarLayout(
      sidebarPanel(width = 3,
                   h3("Sélectionnez la victime"),
                   h5("Sélectionnez le sexe"),
                   checkboxGroupInput("sexe", "", choices = c("Homme","Femme")),
                   br(),
                   br(),
                   h5("Sélectionnez l'âge"),
                   sliderInput("age","",0,100,c(25,75)),
                   br(),
                   br(),
                   h5("Sélectionnez l'ethnie"),
                   checkboxGroupInput("ethnie","", choices = c("Latinos","Blancs","Noirs","Asiatiques","Autres","Non spécifié")),
                   
      ),
      
      mainPanel(
        h4("Nombre total de crimes commis sur la victime en question:"),
        textOutput("totalCrimesText")
      )
    )
  )
)

server <- function(input, output) {
  output$totalCrimesText <- renderText({
    # Filtrer le jeu de données en fonction des filtres sélectionnés
    filteredData <- subset(data,
                           "Vict Sex" %in% input$sexe &
                             "Vict Age" >= input$age[1] & "Vict Age" <= input$age[2])
    
    
    # Calculer le nombre total de crimes
    totalCrimes <- nrow(filteredData)
    
    # Retourner le texte à afficher
    paste(totalCrimes)
  })
}

shinyApp(ui = ui, server = server)
