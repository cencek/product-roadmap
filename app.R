#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(shiny)
library(ggplot2)
library(dplyr)

# Sample data
sample_data <- data.frame(
  product_name = c("Project A", "Project B", "Project C"),
  start_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
  end_date = as.Date(c("2023-01-15", "2023-02-28", "2023-04-15"))
)

sample_data$numeric_product <- as.numeric(as.factor(sample_data$product_name))

sample_data$mid_date <- sample_data$start_date + 
  (sample_data$end_date - sample_data$start_date)/2



ui <- fluidPage(
  titlePanel("Product Roadmap Timeline"),
  sidebarLayout(
    sidebarPanel(
      # Placeholder for any input elements you may want to add later
    ),
    mainPanel(
      plotOutput("timelinePlot")
    )
  )
)

server <- function(input, output) {
  output$timelinePlot <- renderPlot({
    ggplot(sample_data, aes(x = numeric_product, ymin = start_date, ymax = end_date)) +
      geom_rect(aes(xmin = numeric_product - 0.4, 
                    xmax = numeric_product + 0.4), fill = "blue") +
      geom_text(aes(y = mid_date, label = product_name), 
                hjust = 1.5, color = "white", fontface = "bold") + 
      scale_x_continuous(breaks = sample_data$numeric_product, 
                         labels = sample_data$product_name) +
      coord_flip() +
      labs(title = "Product Roadmap Timeline", x = "", y = "Date") +
      theme_minimal()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




