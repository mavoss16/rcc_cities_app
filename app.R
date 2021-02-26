## DEPENDENCIES ----------------------------------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(sf)
library(leaflet)
library(reactable)
source("data.R")
source("modules.R")

## SET THEME -------------------------------------------------------------------------------------------
bs_theme_new(bootswatch = "cosmo")
bs_theme_accent_colors(
  primary = "#373a3c",
  secondary = "#e46c0a",
  success = "#2aa6dc",
  info = "#3daf6f",
  warning = "#e86656",
  danger = "#efba38"
)
bs_theme_add_variables(`enable-rounded` = FALSE)


## UI --------------------------------------------------------------------------------------------------
ui = fluidPage(
  bootstrap(),
  
  ## |_header ------------------------------------------------------------------------------------------
  tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),
  tags$head(tags$style(HTML(
    " .sidebar { font-size: 40%; } "
  ))),
  
  ## |_logo --------------------------------------------------------------------------------------------
  fluidRow(
    width = 12,
    align = "center",
    style = "background-color: #373A3C; margin-left: auto; margin-right: auto",
    img(
      src = "reaching_out.jpg",
      class = "topimage",
      width = "1070px",
      height = "600",
      style = "display: block; margin-left: auto; margin-right: auto;"
    )
  ),
  fluidRow(
    width = 12,
    align = "left",
    h1(strong(tags$a(href = "https://mjvoss.shinyapps.io/rcc_cities/", "Iowa's Recovery Ready Community Network", style = "color: white"))),
    style = "background-color: #373A3C; color: white; margin-left: auto; margin-right: auto"
  ),
  # titlePanel(
  #   title = "Iowa's Recovery Ready Community Network"
  # ),
  ## |_navigation bar ----------------------------------------------------------------------------------
  navbarPage(
    title = NULL,
    windowTitle = "Iowa's Recovery Ready Community Network",
    
    ## |__Home tab---------------------------------------------------------------------------------------
    tabPanel(
      "Home",
      fluidRow(
        style = "margin-left: 6px; margin-right: 6px",
        home_ui("home")
      )
    ),
    
    ## |__What is Recovery? tab --------------------------------------------------------------------------------
    tabPanel(
      "What is Recovery?",
      fluidRow(
        style = "margin-left: 6px; margin-right: 6px",
        what_is_recovery_ui("what_is_recovery")
      )
    ),
    
    ## |__RRCI tab ---------------------------------------------------------------------------------
    tabPanel(
      "Recovery Ready Community Index",
      fluidRow(
        style = "margin-left: 6px; margin-right: 6px",
        rrci_ui("rrci")
      )
    ),
    
    ## |__recovery_resources tab --------------------------------------------------------------------------------
    tabPanel(
      "Recovery Resources",
      fluidRow(
        style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(
          widths = c(2, 10),
          tabPanel(
            title = "Map",
            category_module_ui("recovery_resources", "recovery_resources")
          ),
          tabPanel(
            title = "Data Table",
            table_module_ui("recovery_resources", "recovery_resources")
          ),
          tabPanel(
            title = "Data Sources", 
            source_table_module_ui("recovery_resources", "recovery_resources")
          )
        )
      )
    ),
    
    ## |__health_resources tab ------------------------------------------------------------------------------------
    tabPanel(
      "Health Resources",
      fluidRow(
        style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(
          widths = c(2, 10),
          tabPanel(
            title = "Map",
            category_module_ui("health_resources", "health_resources")
          ),
          tabPanel(
            title = "Data Table",
            table_module_ui("health_resources", "health_resources")
          ),
          tabPanel(
            title = "Data Sources", 
            source_table_module_ui("health_resources", "health_resources")
          )
        )
      )
    ),
    
    ## |__other_resources tab -----------------------------------------------------------------------------------
    tabPanel(
      "Other Resources",
      fluidRow(
        style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(
          widths = c(2, 10),
          tabPanel(
            title = "Map",
            category_module_ui(
              "other_community_resources",
              "other_community_resources"
            )
          ),
          tabPanel(
            title = "Data Table",
            table_module_ui(
              "other_community_resources",
              "other_community_resources"
            )
          ),
          tabPanel(
            title = "Data Sources", 
            source_table_module_ui(
              "other_community_resources", 
              "other_community_resources"
            )
          )
        )
      )
    ),
    
    ## |__demographic_characteristics tab ------------------------------------------------------------------------------------
    # tabPanel(
    #   "Demographic Characteristics",
    #   fluidRow(
    #     style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
    #     navlistPanel(
    #       widths = c(2, 10),
    #       tabPanel(
    #         title = "Map",
    #         category_module_ui(
    #           "demographic_characteristics",
    #           "demographic_characteristics"
    #         )
    #       ),
    #       tabPanel(
    #         title = "Data Table",
    #         table_module_ui(
    #           "demographic_characteristics",
    #           "demographic_characteristics"
    #         )
    #       ),
    #       tabPanel(title = "Data Sources", sources_ui("demographic_characteristics"))
    #     )
    #   )
    # ),
    
    ## |__community_characteristics tab ---------------------------------------------------------------------------------
    tabPanel(
      "Community Characteristics",
      fluidRow(
        style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(
          widths = c(2, 10),
          tabPanel(
            title = "Map",
            category_module_ui("community_characteristics", "community_characteristics")
          ),
          tabPanel(
            title = "Data Table",
            table_module_ui("community_characteristics", "community_characteristics")
          ),
          tabPanel(
            title = "Data Sources", 
            source_table_module_ui("community_characteristics", "community_characteristics")
          )
        )
      )
    ),
    
    ## |__Identifying Recovery Resources in Iowa tab --------------------------------------------------------------------------------
    # tabPanel(
    #   "Identifying Recovery Resources in Iowa",
    #   fluidRow(
    #     style = "margin-left: 6px; margin-right: 6px",
    #     identifying_recovery_resources_ui("identifying_recovery_resources")
    #   )
    # ),
    
    ## |__Key Terms tab --------------------------------------------------------------------------------
    tabPanel(
      "Key Terms",
      fluidRow(
        style = "margin-left: 6px; margin-right: 6px",
        key_terms_ui("key_terms")
      )
    )
    ## |_end navigation bar ------------------------------------------------------------------------------
  ),
  
  ## |_footer ------------------------------------------------------------------------------------------
  hr(),
  fluidRow(
    style = "margin: 20px",
    width = 12,
    column(12, align = 'center',
           tags$small(
             em('Iowa State University Public Science Collaborative')
           ),
           p(tags$small(
             em('Last updated: February 2020')
           )))
  )
)




## SERVER ----------------------------------------------------------------------------------------------
server = function(input, output, session) {
  res_rrci = category_module_server("rrci")
  res_reco = category_module_server("recovery_resources")
  res_reco_table = table_module_server("recovery_resources")
  res_reco_source_table = source_table_module_server("recovery_resources")
  res_other = category_module_server("other_community_resources")
  res_other_table = table_module_server("other_community_resources")
  res_other_source_table = source_table_module_server("other_community_resources")
  res_health = category_module_server("health_resources")
  res_health_table = table_module_server("health_resources")
  res_health_source_table = source_table_module_server("health_resources")
  res_comm = category_module_server("community_characteristics")
  res_comm_table = table_module_server("community_characteristics")
  res_comm_source_table = source_table_module_server("community_characteristics")
  #res_demo = category_module_server("demographic_characteristics")
  #res_demo_table = table_module_server("demographic_characteristics")
}


## APP -------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)