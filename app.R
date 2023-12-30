source(file = "global.R")
source(file = "Packages.R")

ui <- fluidPage(
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
                   sliderInput("age","",0,100,c(25,75)),
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
        textOutput("percentageText")
      )
    )
  )
)

server <- function(input, output) {
  filteredData <- reactive({
    subset(data,
           data$`Vict.Sex` %in% input$sexe &
             data$`Vict.Age` >= input$age[1] & data$`Vict.Age` <= input$age[2] &
             data$`Vict.Age` != -2 &
             data$`Vict.Sex` != "H")
  })
  
  output$totalCrimesText <- renderText({
    totalCrimes <- nrow(filteredData())
    paste(totalCrimes)
  })
  
 
  output$percentageText <- renderText({
    totalCrimes <- nrow(filteredData())
    totalRowsFiltered <- nrow(data[data$`Vict.Age` != -2 & data$`Vict.Sex` != "H",])
    percentage <- (totalCrimes / totalRowsFiltered) * 100
    paste("Pourcentage des crimes sélectionnés par rapport au total :", round(percentage, 2), "%")
  })
}

shinyApp(ui = ui, server = server)
