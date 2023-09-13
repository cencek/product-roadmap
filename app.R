# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)

# Sample data
sample_data <- data.frame(
  product_name = c("Project A", "Project B", "Project C"),
  start_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
  end_date = as.Date(c("2023-01-15", "2023-02-28", "2023-04-15")),
  design_start = as.Date(c("2022-12-01", "2023-01-15", "2023-02-20")),
  design_end = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
  release_date = as.Date(c("2023-01-20", "2023-03-05", "2023-04-18")),
  project_type = c("Feature add", "New release", "Technical debt")
)

sample_data$release_text <- paste("Release date:", sample_data$release_date)
sample_data$numeric_product <- as.numeric(as.factor(sample_data$product_name))

# UI for the Shiny app
ui <- fluidPage(
  titlePanel("Product Roadmap Timeline"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_type", "Select Project Type:", 
                  choices = c("All", unique(sample_data$project_type)))
    ),
    mainPanel(
      plotlyOutput("timelinePlot")
    )
  )
)

# Server logic for the Shiny app
server <- function(input, output) {
  output$timelinePlot <- renderPlotly({
    filtered_data <- sample_data
    if (input$selected_type != "All") {
      filtered_data <- sample_data[sample_data$project_type == input$selected_type, ]
    }
    
    p <- ggplot(filtered_data, aes(x = numeric_product, fill = project_type, text = product_name)) +
      geom_rect(aes(ymin = design_start, ymax = design_end, xmin = numeric_product - 0.4, xmax = numeric_product + 0.4), alpha = 0.3) +
      geom_rect(aes(ymin = start_date, ymax = end_date, xmin = numeric_product - 0.4, xmax = numeric_product + 0.4)) +
      geom_point(aes(y = release_date, text = release_text), color = "black") +
      scale_x_continuous(breaks = filtered_data$numeric_product, 
                         labels = filtered_data$product_name) +
      coord_flip() +
      labs(title = "Product Roadmap Timeline", x = "", y = "Date") +
      theme_minimal() +
      scale_fill_manual(values = c("Feature add" = "blue", "New release" = "green", "Technical debt" = "red"))
    
    ggplotly(p, tooltip = "text")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
