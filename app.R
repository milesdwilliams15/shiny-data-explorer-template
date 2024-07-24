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


# Define user interface ---------------------------------------------------

## read in the data
dt <- read.csv(
  here(
    "_data",
    "dashboard_data.csv"
  )
)

## variable names
  # the raw names
vars <- names(dt)

  # cleaned up names
clean_names <- vars |>
  str_replace_all("_", " ") |>
  str_to_title()

  # vars indexed by clean_names
names(vars) <- clean_names

  # valid numerical values for x-axis
xvars <- vars[
  -c(1, 6)
]

  # valid numerical values for y-axis
yvars <- vars[
  -c(1, 2, 6)
]

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
    plotOutput('plot1')
  )
)

# Define server logic -----------------------------------------------------

server <- function(input, output) {

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    
    vars_to_keep <- c(input$xcol, input$ycol) |>
      str_to_lower() |>
      str_replace_all(" ", "_")
    
    sdt <- dt[, c(vars_to_keep, "hostility_level")] |>
      filter(hostility_level %in% input$hostlev)
    
    names(sdt) <- c("xvar", "yvar", "hostlev")
    
    sdt
  })
  
  output$plot1 <- renderPlot({
    
    ## make the base plot
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
    
    ## facet by hostility level?
    if(length(input$hostlev) == 2 & input$facethostlev == "Yes") {
      base_plot +
        facet_wrap(
          ~ hostlev,
          scales = "free",
          ncol = 2
        ) -> base_plot
    }
    
    ## apply log-10 to x-axis?
    if(input$logx == "Yes") {
      base_plot +
        scale_x_log10() -> base_plot
    }
    
    ## apply log-10 to y-axis?
    if(input$logy == "Yes") {
      base_plot +
        scale_y_log10() -> base_plot
    }
    
    ## return the plot
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
}


# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
