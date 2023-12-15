source(file="global.R")
source(file = "Packages.R")


ui<-fluidPage(
  div(
    h1("Les crimes à LA en fonction des victimes",align="center"),
    sidebarLayout(
      sidebarPanel(width = 3,
                   h5("Sélectionnez la victime"),
                   checkboxGroupInput("box_group","", choices = c("Homme","Femme")),
                   h5("Sélectionnez l'âge de la victime"),
                   sliderInput("slider","",0,100,c(25,75)),
                   h5("Sélectionnez l'ethnie de la victime"),
                   checkboxGroupInput("box_group","", choices = c("Latinos","Blancs","Noirs","Asiatiques","Autres","Non spécifié")),
                   
      ),
      mainPanel(
        
      )
    )))













server <- function(input, output) {
  
}

shinyApp(ui=ui , server=server)

