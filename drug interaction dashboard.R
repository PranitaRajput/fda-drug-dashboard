install.packages("purr")
install.packages("ggplot2")
install.packages("DT")
install.packages("shiny")
# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(DT)
library(shiny)
url <- "https://api.fda.gov/drug/event.json?limit=1000"

#make the API request
response <- GET(url)

# Parse the JSON into a list
data_raw <- content(response, as = "parsed", type = "application/json")

# View structure
str(data_raw, max.level = 2)

# Extract the results part from the API response
events <- data_raw$results


# Safely extract fields of interest
cleaned_events <- map_df(events, function(x) {
  tibble(
    safetyreportid = x$safetyreportid %||% NA,
    receivedate = x$receivedate %||% NA,
    serious = x$serious %||% NA,
    patient_drug = if (!is.null(x$patient$drug)) paste0(map_chr(x$patient$drug, ~ .x$medicinalproduct), collapse = ", ") else NA,
    patient_reaction = if (!is.null(x$patient$reaction)) paste0(map_chr(x$patient$reaction, ~ .x$reactionmeddrapt), collapse = ", ") else NA
  )
})

# Preview cleaned data
head(cleaned_events)

# UI
ui <- fluidPage(
  titlePanel("FDA Drug Event Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("drug", "Select Drug:", choices = NULL, selected = NULL),
      checkboxGroupInput("serious", "Seriousness Level:",
                         choices = c("1" = "1", "2" = "2"), selected = c("1", "2"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Reactions Plot", plotOutput("reactionPlot")),
        tabPanel("Event Table", DTOutput("eventTable"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Replace this with your cleaned dataframe
  df <- cleaned_events
  
  # Convert to proper date
  df$receivedate <- as.Date(df$receivedate, format = "%Y%m%d")
  
  # Update drug choices
  observe({
    drug_list <- sort(unique(unlist(strsplit(df$patient_drug, ", "))))
    updateSelectInput(session, "drug", choices = drug_list)
  })
  
  # Reactive data based on filters
  filtered_data <- reactive({
    df %>%
      filter(grepl(input$drug, patient_drug, ignore.case = TRUE),
             serious %in% input$serious)
  })
  
  # Plot Output
  output$reactionPlot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)
    
    reactions <- strsplit(data$patient_reaction, ", ")
    reaction_df <- data.frame(reaction = unlist(reactions))
    
    top_reactions <- reaction_df %>%
      group_by(reaction) %>%
      tally(sort = TRUE) %>%
      top_n(10)
    
    ggplot(top_reactions, aes(x = reorder(reaction, n), y = n)) +
      geom_col(fill = "tomato") +
      coord_flip() +
      labs(title = paste("Top Reactions for", input$drug), x = "Reaction", y = "Count")
  })
  
  # Table Output
  output$eventTable <- renderDT({
    data <- filtered_data()
    datatable(data, options = list(pageLength = 10), rownames = FALSE)
  })
}

# Run the app
shinyApp(ui = ui, server = server)