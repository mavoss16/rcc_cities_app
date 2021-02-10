## DEPENDENCIES ----------------------------------------------------------------------------------------
library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(sf)
library(leaflet)
source("rcc_data.R")
source("rcc_modules.R")

## SET THEME -------------------------------------------------------------------------------------------
bs_theme_new(bootswatch = "cosmo")
bs_theme_accent_colors(primary = "#373a3c", secondary = "#e46c0a", 
                       success = "#2aa6dc", info = "#3daf6f", warning = "#e86656", danger = "#efba38")
bs_theme_add_variables(`enable-rounded` = FALSE)


## UI --------------------------------------------------------------------------------------------------
ui <- fluidPage(
  bootstrap(),
  
  ## |_header ------------------------------------------------------------------------------------------
  tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),
  tags$head(tags$style(HTML(" .sidebar { font-size: 40%; } "))),
  
  ## |_logo --------------------------------------------------------------------------------------------           
  fluidRow(width = 12, align = "center",
    img(src = "capitals.jpg", class = "topimage", width = "98%", height = "600", style = "display: block; margin-left: auto; margin-right: auto;")
  ),
  ## |_navigation bar ---------------------------------------------------------------------------------- 
  navbarPage(title = NULL,
             
    ## |__capitals tab ---------------------------------------------------------------------------------            
    tabPanel("Community Capitals",
      fluidRow(style = "margin: 6px", width = 12,
        column(4, 
          br(), br(),
          img(src = "comm-caps.jpg",  width = "98%", style = "display: block; margin-left: auto; margin-right: auto;")
        ),
        column(8, 
          br(),
          h2(strong("The Community Capitals Framework")),
          br(),
          h4("The Community Capitals Framework is based on the idea that communities have assets. 
            These assets, or capitals, refer to the resources that support positive individual change (i.e. economic mobility) and community resilience. 
            Utility of the framework is the emphasis on assets, rather than deficits or needs. ")
        )
      )
    ),
    ## |__financial tab --------------------------------------------------------------------------------            
    tabPanel("Financial", 
      fluidRow(style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(widths = c(3, 9),
          tabPanel(title = "Maps and Indicators", capital_module_ui("Financial", "Financial")),
          tabPanel(title = "Data, Measures, and Methods", sources_ui("Financial"))
        )
      )
    ),
    ## |__human tab ------------------------------------------------------------------------------------            
    tabPanel("Human",
      fluidRow(style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(widths = c(3, 9),
          tabPanel(title = "Maps and Indicators", capital_module_ui("Human", "Human")),
          tabPanel(title = "Data, Measures, and Methods", sources_ui("Human"))
        )
      )
    ),
    ## |__social tab -----------------------------------------------------------------------------------            
    tabPanel("Social",
      fluidRow(style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(widths = c(3, 9),
          tabPanel(title = "Maps and Indicators", capital_module_ui("Social", "Social")),
          tabPanel(title = "Data, Measures, and Methods", sources_ui("Social"))
        )
      )
    ),
    ## |__built tab ------------------------------------------------------------------------------------            
    tabPanel("Built",
      fluidRow(style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(widths = c(3, 9),
          tabPanel(title = "Maps and Indicators", capital_module_ui("Built", "Built")),
          tabPanel(title = "Data, Measures, and Methods", sources_ui("Built"))
        )
      )
    ),
    ## |__ natural tab ---------------------------------------------------------------------------------            
    tabPanel("Natural",
      fluidRow(style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(widths = c(3, 9),
          tabPanel(title = "Maps and Indicators", capital_module_ui("Natural", "Natural")),
          tabPanel(title = "Data, Measures, and Methods", sources_ui("Natural"))
        )
      )
    ),
    ## |__political tab --------------------------------------------------------------------------------
    tabPanel("Political"),
    ## |__cultural tab ---------------------------------------------------------------------------------            
    tabPanel("Cultural")
  ## |_end navigation bar ------------------------------------------------------------------------------            
  ),    
  
  ## |_footer ------------------------------------------------------------------------------------------
  hr(),
  fluidRow(style = "margin: 20px", width = 12,
    column(12, align = 'center',
      tags$small(em( 'Iowa State University Public Science Collaborative')),
      p(tags$small(em('Last updated: February 2020')))
    )
)
  )




## SERVER ----------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  res_fin <- capital_module_server("Financial")
  res_soc <- capital_module_server("Social") 
  res_hum <- capital_module_server("Human") 
  res_nat <- capital_module_server("Natural")
  res_built <- capital_module_server("Built")
}


## APP -------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)