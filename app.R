#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


# Open the packages you need ----------------------------------------------

library(shiny)
library(tidyverse)
library(here)
library(ggthemes)
theme_set(theme_fivethirtyeight())


# Read in data and prep for application -----------------------------------

## read in the data
dt <- read.csv(
  here(
    "_data",
    "dashboard_data.csv"
  )
)

## get raw variable names
  # the raw names
vars <- names(dt)

  # clean up the raw variable names
clean_names <- vars |>
  str_replace_all("_", " ") |>
  str_to_title()

  # index the raw names by cleaned names
names(vars) <- clean_names

  # keep only valid numerical values for x-axis
xvars <- vars[
  -c(1, 6)
]

  # keep only valid numerical values for y-axis (excluding start year)
yvars <- vars[
  -c(1, 2, 6)
]


# Define user interface ---------------------------------------------------

ui <- pageWithSidebar(
  headerPanel('Correlates of War Size Data Explorer'),
  sidebarPanel(
    selectInput(
      'xcol', 
      'X Variable', 
      names(xvars)
    ),
    selectInput(
      'ycol', 
      'Y Variable', 
      names(yvars), 
    ),
    selectInput(
      'hostlev', 
      'Hostility Level', 
      unique(dt$hostility_level),
      multiple = T,
      selected = unique(dt$hostility_level)
    ),
    selectInput(
      'facethostlev',
      'Facet by Hostility Level?',
      c("No", "Yes")
    ),
    selectInput(
      'logx',
      'X Log-10 Scale?',
      c("No", "Yes")
    ),
    selectInput(
      'logy',
      'Y Log-10 Scale?',
      c("No", "Yes")
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Data Visualization",
        plotOutput('plot1')
      ),
      tabPanel(
        title = "Metadata",
        tableOutput('dataset')
      )
    )
  )
)

# Define server logic -----------------------------------------------------

server <- function(input, output) {

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    
    # convert var names back to original format
    vars_to_keep <- c(input$xcol, input$ycol) |>
      str_to_lower() |>
      str_replace_all(" ", "_")
    
    # keep only the selected variables + hostility_level
    sdt <- dt[, c(vars_to_keep, "hostility_level")] |>
      filter(hostility_level %in% input$hostlev)
    
    # standardize the variable names
    names(sdt) <- c("xvar", "yvar", "hostlev")
    
    # return the updated dataset
    sdt
  })
  
  # Create the data visualization
  output$plot1 <- renderPlot({
    
    # make the base plot
    ggplot(selectedData()) +
      aes(
        x = xvar,
        y = yvar
      ) +
      geom_point(
        color = "gray"
      ) +
      geom_smooth(
        method = "gam",
        se = F
      ) -> base_plot
    
    # facet by hostility level?
    if(length(input$hostlev) == 2 & input$facethostlev == "Yes") {
      base_plot +
        facet_wrap(
          ~ hostlev,
          scales = "free",
          ncol = 2
        ) -> base_plot
    }
    
    # apply log-10 to x-axis?
    if(input$logx == "Yes") {
      base_plot +
        scale_x_log10() -> base_plot
    }
    
    # apply log-10 to y-axis?
    if(input$logy == "Yes") {
      base_plot +
        scale_y_log10() -> base_plot
    }
    
    # return the plot with some final finishes
    base_plot +
      labs(
        title = paste0(
          input$ycol, " Shown by ", input$xcol
        ),
        caption = "@MDWilliamsPhD"
      ) +
      theme(
        strip.text = element_text(
          size = 16
        ),
        axis.text = element_text(
          size = 12
        )
      )
  })
  
  # Create a metadata table
  output$dataset <- renderTable({
    tribble(
      ~ Variable, ~ Definition,
      "Year Started", "The calendar year a conflict began",
      "Years Duration", "The number of calendar years durring which a conflict lasted",
      "Minimum Fatalities", "The minimum estimate of battle-related fatalities",
      "Maximum Fatalities", "The maximum estimate of battle-related fatalities",
      "Hostility Level", "Whether a conflict represented only use of force or an all-out war",
      "Total Belligerents", "Number of countries involved by conflict end",
      "Belligerent Population", "Combined population of countries involved at conflict start",
      "Belligerent Military Spending", "Combined military spending of countries involved at conflict start",
      "Belligerent Troops", "Combined military personnel of countries involved at conflict start",
      "Mean Belligerent Democracy", "Average Polity 2 score of countries involved at conflict start"
    )
  })
}


# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
