# Load necessary libraries
library(shiny)
library(ggplot2)

# Sample data
sample_data <- data.frame(
  product_name = c("Project A", "Project B", "Project C"),
  start_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
  end_date = as.Date(c("2023-01-15", "2023-02-28", "2023-04-15")),
  project_type = c("Feature add", "New release", "Technical debt")
)

# Add numeric version of product_name and mid_date for plotting
sample_data$numeric_product <- as.numeric(as.factor(sample_data$product_name))
sample_data$mid_date <- sample_data$start_date + 
  (sample_data$end_date - sample_data$start_date)/2

# UI for the Shiny app
ui <- fluidPage(
  titlePanel("Product Roadmap Timeline"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_type", "Select Project Type:", 
                  choices = c("All", unique(sample_data$project_type)))
    ),
    mainPanel(
      plotOutput("timelinePlot")
    )
  )
)

# Server logic for the Shiny app
server <- function(input, output) {
  output$timelinePlot <- renderPlot({
    filtered_data <- sample_data
    if (input$selected_type != "All") {
      filtered_data <- sample_data[sample_data$project_type == input$selected_type, ]
    }
    
    ggplot(filtered_data, aes(x = numeric_product, ymin = start_date, ymax = end_date)) +
      geom_rect(aes(xmin = numeric_product - 0.4, 
                    xmax = numeric_product + 0.4, fill = project_type)) +
      scale_x_continuous(breaks = filtered_data$numeric_product, 
                         labels = filtered_data$product_name) +
      coord_flip() +
      labs(title = "Product Roadmap Timeline", x = "", y = "Date") +
      theme_minimal() +
      scale_fill_manual(values = c("Feature add" = "blue", "New release" = "darkgreen", "Technical debt" = "purple"))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
