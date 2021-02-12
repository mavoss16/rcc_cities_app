## DEPENDENCIES ----------------------------------------------------------------------------------------
library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(reactable)
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
    img(src = "reaching_out.jpg", class = "topimage", width = "98%", height = "600", style = "display: block; margin-left: auto; margin-right: auto;")
    # Could use width = "1070" to keep from stretching
  ),
  fluidRow(width = 12, align = "left",
           h1(strong("Iowa's Recovery Ready Community Network")),
           style = "margin-left: 6px; margin-right: 6px; background-color: #373A3C; color: white"
  ),
  # titlePanel(
  #   title = "Iowa's Recovery Ready Community Network"
  # ),
  ## |_navigation bar ---------------------------------------------------------------------------------- 
  navbarPage(title = NULL,
             
    ## |__RRCI tab ---------------------------------------------------------------------------------            
    tabPanel("Recovery Ready Community Index",
      fluidRow(style = "margin-left: 6px; margin-right: 6px", width = 12,
        column(4, 
          br(), br(),
          img(src = "comm-caps.jpg",  width = "98%", style = "display: block; margin-left: auto; margin-right: auto;")
        ),
        column(8, 
          br(),
          h2(strong("The Community Capitals Framework")),
          br(),
          h4("The Community Capitals Framework is based on the idea that communities have assets. 
            These assets, or categorys, refer to the resources that support positive individual change (i.e. economic mobility) and community resilience. 
            Utility of the framework is the emphasis on assets, rather than deficits or needs. ")
        )
      ),
      fluidRow(style = "margin: 20px", width = 12,
               category_module_ui("rrci", "rrci"))
    ),
    
    
    ## |__recovery_resources tab --------------------------------------------------------------------------------            
    tabPanel("Recovery Resources", 
      fluidRow(style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(widths = c(3, 9),
          tabPanel(title = "Maps and Indicators", category_module_ui("recovery_resources", "recovery_resources")),
          tabPanel(title = "Data Table", table_module_ui("recovery_resources", "recovery_resources")),
          tabPanel(title = "Data, Measures, and Methods", sources_ui("recovery_resources"))
        )
      )
    ),
    ## |__primary_care_resources tab ------------------------------------------------------------------------------------            
    tabPanel("Primary Care Resources",
      fluidRow(style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(widths = c(3, 9),
          tabPanel(title = "Maps and Indicators", category_module_ui("primary_care_resources", "primary_care_resources")),
          tabPanel(title = "Data Table", table_module_ui("primary_care_resources", "primary_care_resources")),
          tabPanel(title = "Data, Measures, and Methods", sources_ui("primary_care_resources"))
        )
      )
    ),
    ## |__social_and_economic_resources tab -----------------------------------------------------------------------------------            
    tabPanel("Social and Economic Resources",
      fluidRow(style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(widths = c(3, 9),
          tabPanel(title = "Maps and Indicators", category_module_ui("social_and_economic_resources", "social_and_economic_resources")),
          tabPanel(title = "Data Table", table_module_ui("social_and_economic_resources", "social_and_economic_resources")),
          tabPanel(title = "Data, Measures, and Methods", sources_ui("social_and_economic_resources"))
        )
      )
    ),
    ## |__demographic_characteristics tab ------------------------------------------------------------------------------------            
    tabPanel("Demographic Characteristics",
      fluidRow(style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(widths = c(3, 9),
          tabPanel(title = "Maps and Indicators", category_module_ui("demographic_characteristics", "demographic_characteristics")),
          tabPanel(title = "Data Table", table_module_ui("demographic_characteristics", "demographic_characteristics")),
          tabPanel(title = "Data, Measures, and Methods", sources_ui("demographic_characteristics"))
        )
      )
    ),
    ## |__ community_characteristics tab ---------------------------------------------------------------------------------            
    tabPanel("Community Characteristics",
      fluidRow(style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(widths = c(3, 9),
          tabPanel(title = "Maps and Indicators", category_module_ui("community_characteristics", "community_characteristics")),
          tabPanel(title = "Data Table", table_module_ui("community_characteristics", "community_characteristics")),
          tabPanel(title = "Data, Measures, and Methods", sources_ui("community_characteristics"))
        )
      )
    ),
    ## |__Key Terms tab --------------------------------------------------------------------------------
    tabPanel("Key Terms")
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
  res_rrci <- category_module_server("rrci")
  res_reco <- category_module_server("recovery_resources")
  res_reco_table <- table_module_server("recovery_resources")
  res_soc_econ <- category_module_server("social_and_economic_resources")
  res_soc_econ_table <- table_module_server("social_and_economic_resources")
  res_prim_care <- category_module_server("primary_care_resources")
  res_prim_care_table <- table_module_server("primary_care_resources")
  res_comm <- category_module_server("community_characteristics")
  res_comm_table <- table_module_server("community_characteristics")
  res_demo <- category_module_server("demographic_characteristics")
  res_demo_table <- table_module_server("demographic_characteristics")
}


## APP -------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)