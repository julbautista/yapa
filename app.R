library(shiny)

ui <- navbarPage("2020 Election Simuations",
                 tabPanel("National",
                          mainPanel(
                            
                            dataTableOutput(outputId = "state_results"),
                            imageOutput(outputId = "ec_distribution"),
                            imageOutput(outputId = "state_vote_share")
                          )
                 ),
                 tabPanel("State",
                          # Sidebar layout with a input and output definitions ----
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              
                              # Input: Selector for choosing dataset ----
                              selectInput(inputId = "state",
                                          label = NULL,
                                          selected = "New York",
                                          choices = c(state.name, "District of Columbia"))
                            ),
                            
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              textOutput(outputId = "text"),
                              plotOutput(outputId = "state_simulations")
                            )
                          )
                 )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  simulations <- data.table::fread("results/state_simulations.csv")

  output$state_simulations <- renderPlot({
    
    simulations %>%
      filter(state == input$state) %>%
      ggplot() + 
      aes(x = value, fill = candidate, group = NULL, y = ..density..) +
      geom_histogram(position = "identity", alpha = 0.6, bins = 50) +
      scale_x_continuous(limits = c(0, 1),
                         breaks = seq(0, 1, 0.1),
                         labels = paste0(seq(0, 100, 10), "%")) +
      scale_fill_manual(values = c("blue", "grey", "red")) +
      scale_color_manual(values = c("blue", "grey", "red")) +
      geom_vline(aes(xintercept = mean, col = candidate), lty = 2) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_text(size = 9),
            legend.position = 'bottom',
            plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 9)) +
      labs(x = NULL, y = NULL, fill = NULL, col = NULL,
           title = paste(input$state)) +
      guides(col = FALSE)
    
  })
  
  output$text <- renderText(paste("In", input$state, "this is what happens"))
  
  state_results <- data.table::fread("results/state_results.csv")
  output$state_results <- renderDataTable(state_results, 
                                          options = list(scrollX = TRUE, 
                                                         scrollY = "200px",
                                                         pageLength = 51))
  
  
  output$ec_distribution <- renderImage({
    
    # Return a list containing the filename
    list(src = "docs/ec_distributions.png",
         contentType = 'image/png',
         width = 815,
         height = 500,
         alt = "distribution of electoral college votes")
  }, deleteFile = FALSE)
  
  output$state_vote_share <- renderImage({
    
    # Return a list containing the filename
    list(src = "docs/state_distributions.png",
         contentType = 'image/png',
         width = 840,
         height = 800,
         alt = "Vote share by state")
  }, deleteFile = FALSE)
  
  
}


shinyApp(ui, server)

