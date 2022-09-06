# library(shiny)


shinyApp(
  # Define UI for application that draws a histogram
  ui <- fluidPage(
    # App title ----
    titlePanel("Voting Dashboard"),
    # Main panel for displaying outputs ----
    sidebarLayout(
      sidebarPanel(
        # ngens: number slider
        textInput(inputId = "space", 
                  label = "Enter the name of the Snapshot space you're interested in", 
                  value = "poh.eth"),
        # number of propositions to consider
        sliderInput(inputId = "n_props", label = "Number of Proposals", min = 1, max = 50,
                    value = 20),
        actionButton(inputId = "run",
                     label = "Run Analysis")
      ),
      mainPanel(
        textOutput(outputId = "partic")
      )
    )
  ),
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    observeEvent(eventExpr = { input$run }, handlerExpr = {
      space <- input$space
      
      participation <- getParticipation(vot_tab)
      
      output$partic <- renderText({participation})
    })
  },
  options = list(height = 600)
)