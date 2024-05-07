install.packages(c("shiny", "shinydashboard", "DT", "tidyverse", "plotly", "httr"), repos = "http://cran.us.r-project.org")

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)
library(httr)

# get data via web api
api_url <- "https://data.medicaid.gov/api/1/metastore/schemas/dataset/items/6165f45b-ca93-5bb5-9d06-db29c692a360"
response <- GET(api_url)
data <- read_csv(content(response)$distribution[[1]]$downloadURL)

# data transformations
data <- data %>%
  mutate(`Report Date` = as.Date(`Report Date`, format = "%m/%d/%Y")) %>%
  filter(`Report Date` >= as.Date("2018-01-01")) %>%
  arrange(`State Name`, `Report Date`) %>%
  group_by(`State Name`) %>%
  mutate(Enrollment_Change = `Total Medicaid and CHIP Enrollment` - lag(`Total Medicaid and CHIP Enrollment`))

# frontend ui
ui <- fluidPage(
  titlePanel("Medicaid and CHIP Enrollment Rate of Change"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state_select", "Select State:", choices = unique(data$`State Name`)),
      width = 3
    ),
    mainPanel(
      plotlyOutput("enrollment_change_plot"),
      DTOutput("enrollment_table")
    )
  )
)

# backend server 
server <- function(input, output) {
  # reactive expression to filter data based on state selection
  state_data <- reactive({
    data %>%
      filter(`State Name` == input$state_select)
  })
  
  # plot for enrollment rate of change
  output$enrollment_change_plot <- renderPlotly({
    p <- ggplot(state_data(), aes(x = `Report Date`, y = Enrollment_Change, group = 1)) +
      geom_line(color = "turquoise") +
      geom_point(color = "darkgray", size = 2) +
      labs(title = paste("Rate of Change in Enrollment for", input$state_select),
           x = "Report Date", y = "Rate of Change in Enrollment") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "darkblue", size = 20, face = "bold"),
        axis.title = element_text(color = "darkred"),
        axis.text = element_text(color = "darkgreen")
      )
    ggplotly(p)
  })
  
  # data table to show detailed view
  output$enrollment_table <- renderDT({
    state_data() %>%
      select(`Report Date`, `Total Medicaid and CHIP Enrollment`, Enrollment_Change)
  }, options = list(pageLength = 5))
}

# run it!
shinyApp(ui, server)