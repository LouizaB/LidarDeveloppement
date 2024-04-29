library(shiny)
library(shinyWidgets)

# Define UI
ui <- fluidPage(
  titlePanel("Visualisation d'images avec différentes techniques"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("filtering", "Techniques de filtrage :", 
                  choices = c("HNT", "CHM", "TDM"), selected = "HNT"),
      selectInput("segmentation", "Techniques de segmentation de données :", 
                  choices = c("Segmentation daplont", "Segmentation silva", "CNN"), selected = "Segmentation daplont"),
      selectInput("normalization", "Techniques de normalisation :", 
                  choices = c("Homogène", "Aléatoire", "Elévation"), selected = "Homogène"),
      actionButton("update", "Mettre à jour")
    ),
    
    mainPanel(
      plotOutput("imagePlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  output$imagePlot <- renderPlot({
    # Récupérer les valeurs sélectionnées par l'utilisateur
    filtering <- input$filtering
    segmentation <- input$segmentation
    normalization <- input$normalization
    
    # Utiliser les valeurs sélectionnées pour afficher l'image correspondante
    # Ici, on suppose que vous avez déjà des fonctions ou des méthodes pour visualiser les images
    # Utilisez les valeurs de filtering, segmentation et normalization pour appeler ces fonctions/méthodes avec les paramètres appropriés
    # Par exemple :
    # image <- visualiser_image(filtering, segmentation, normalization)
    # plot(image)
    # Remplacez visualiser_image() par vos fonctions/méthodes réelles pour charger et visualiser les images
    # Assurez-vous que vos fonctions/méthodes prennent les options de filtrage, segmentation et normalisation comme paramètres
    
    # Pour cet exemple, nous affichons juste un message indiquant les options sélectionnées
    message <- paste("Technique de filtrage:", filtering, "\n",
                     "Technique de segmentation:", segmentation, "\n",
                     "Technique de normalisation:", normalization)
    cat(message)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
