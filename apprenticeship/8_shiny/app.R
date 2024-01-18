library(shiny)
library(tidyverse)
library(DT)

# load and clean data
data <- read_csv("https://raw.githubusercontent.com/ucrdatacenter/projects/main/apprenticeship/1_intro/surveys.csv") |> 
  drop_na(hindfoot_length, weight)

species_counts <- data |> 
  count(year, plot_type, genus, species)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Species occurrences"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      p("Choose the characteristics of the displayed data:"),
      sliderInput("years", # input ID
                  "Year range", # displayed label
                  min = min(data$year), # minimum value
                  max = max(data$year), # maximum value
                  value = c(min(data$year), max(data$year)) # initial value: two elements create a range
      ),
      checkboxGroupInput("plot_type", # input ID
                         "Plot type", # displayed label
                         choices = unique(data$plot_type), # choices
                         selected = unique(data$plot_type) # initial selection
      ),
      radioButtons("variable", # input ID
                   "Displayed variable", # displayed label
                   choiceNames = c("Hindfoot length", "Animal weight"), # displayed choices
                   choiceValues = c("hindfoot_length", "weight"), # values returned by input
      ),
      numericInput("bins", # input ID
                   "Number of bins:",  # displayed label
                   30 # initial value
      ),
      uiOutput("facet_ui") # UI element defined in server
    ),
    mainPanel(
      h3("Histogram of chosen variable"),
      plotOutput("plot"),
      textOutput("summary"),
      hr(),
      h3("Species counts"),
      dataTableOutput("counts")
    )
  )
)

# Define server logic required to create outputs
server <- function(input, output) {
  
  output$facet_ui <- renderUI({
    # display facet option only if there is more than one facet
    if (length(input$plot_type) > 1) {
      # UI element definition
      checkboxInput("facet", # input ID
                    "Facet by plot type?", # displayed label
                    FALSE # initial value
      )
    }
  })
  
  data_filtered <- reactive({
    data |> 
      filter(year >= input$years[1], year <= input$years[2],
             plot_type %in% input$plot_type) |> 
      rename("value" = input$variable) |> 
      select(year, value, genus, species, plot_type)
  })
  
  output$plot <- renderPlot({
    p <- data_filtered() |>
      ggplot(aes(value)) +
      geom_histogram(bins = input$bins) +
      theme_light()
    
    if (input$facet) p <- p + facet_wrap(~plot_type)
    
    p
  })
  
  output$summary <- renderText({
    paste0("Sample mean of ", input$variable, ":\n", mean(data_filtered()$value))
  })
  
  output$counts <- renderDataTable({
    species_counts |>
      filter(year >= input$years[1], year <= input$years[2],
             plot_type %in% input$plot_type) |>
      group_by(genus, species) |>
      summarize(n = sum(n))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
