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
    img(
      src = "reaching_out.jpg",
      class = "topimage",
      width = "98%",
      height = "600",
      style = "display: block; margin-left: auto; margin-right: auto;"
    )
    # Could use width = "1070" to keep from stretching
  ),
  fluidRow(
    width = 12,
    align = "left",
    h1(strong("Iowa's Recovery Ready Community Network")),
    style = "background-color: #373A3C; color: white; margin-left: auto; margin-right: auto"
  ),
  # titlePanel(
  #   title = "Iowa's Recovery Ready Community Network"
  # ),
  ## |_navigation bar ----------------------------------------------------------------------------------
  navbarPage(
    title = NULL,
    windowTitle = "Iowa's Recovery Ready Community Network",
    
    ## |__RRCI tab ---------------------------------------------------------------------------------
    tabPanel(
      "Recovery Ready Community Index",
      fluidRow(
        style = "margin-left: 6px; margin-right: 6px",
        width = 12,
        column(
          12,
          br(),
          h2(strong("Recovery Ready Communities")),
          p(
            "Recovery Community Centers (RCCs) are a low-cost, member driven, voluntarist, locally managed, and community-based 
            intervention aimed at supporting sustainable recovery for people with substance use disorder, or SUD. 
            RCCs link people to existing community recovery resources and services and promote a vibrant recovery culture by offering 
            a physical community center where people in recovery can visit, engage with others in recovery, and learn about 
            support services and health resources. This approach is validated by academic studies and is promoted by the SUD community 
            and public health officials. However, finding the right communities for RCC development in Iowa has proven difficult: 
            Iowa is one of just five states in the U.S. that has yet to adopt the recovery community model."
          ),
          p(
            "Which Iowa communities are best positioned to support a Recovery Community Center? 
            To answer this question, we reviewed scientific literature on substance use recovery and engaged key 
            stakeholders who work directly with the SUD population in Iowa to understand what kinds of communities 
            are most conducive to SUD recovery. Based on what we learned, we identified 17 unique community-based resources 
            associated with successful RCC development and collected nearly 16,000 resource data points across almost all of 
            Iowa’s 944 cities and towns. These efforts culminated in the development of a novel Recovery Ready Community Index (RRCI), 
            a way to measure the breadth and depth of local recovery infrastructure and the size and strength of the 
            local substance use recovery culture. (For a visual overview of the results by the four recovery domains, see Figures 4 and 5). 
            We then analyzed index results to identify the thirty highest value, ‘Recovery Ready’ communities in Iowa based on this index. 
            These results are visualized in Figure 1. The size of the circle for each community reflects the community’s overall population."
          ),
          p(),
          h4("Breadth of Recovery Resources"),
          p(
            "Breadth of recovery resources was measured by counting the number of different types of recovery resources in 
            each county and community. With 17 categories of recovery resources, this index ranged from 0-17. A town with a 
            score of zero indicates that we were unable to locate any of the resources and infrastructure listed in Table 3 
            (e.g. no hospitals nor parks nor treatment centers). Cities with a score of 17 have one or more of each of the 
            recovery resource categories for which we had data. Six cities, including Sioux City, Mason City, Fort Dodge, 
            Dubuque, Iowa City, and Des Moines, had at least one instance of each of the 17 recovery resources we measured."
          ),
          p(
            "The importance of measuring resource breadth was documented in the scientific literature and was a consistent 
            theme in conversations with experts, who noted that providing multiple pathways to recovery is critical to successful, 
            sustainable recovery. The reason for this is because the recovery process is a personal journey, inextricably interwoven 
            with a person’s own, distinctive biography. Places with a wealth of recovery support resources are better able to meet 
            the diverse and unique needs of their recovery population. Put differently, RCCs in resource rich communities can better 
            serve their members by providing access to a wider variety of resources. This ensures that each RCC member has the 
            particular resources they need, when they need them, as they progress through their personal recovery journey (See Figure 4, panel a)."
          ),
          h4("Depth of Recovery Resources"),
          p(
            ""
          ),
          h4("Size of Recovery Culture"),
          p(
            ""
          ),
          h4("Strength (Vibrancy) of Recovery Culture"),
          p(
            ""
          ),
          h4("Recovery Ready Community Index"),
          p(
            ""
          )
        )
      ),
      fluidRow(
        style = "margin-left: 6px; margin-right: 6px",
        width = 12,
        column(
          4,
          br(),
          br(),
          img(src = "comm-caps.jpg",  width = "98%", style = "display: block; margin-left: auto; margin-right: auto;")
        ),
        column(
          8,
          br(),
          h2(strong("The Community Capitals Framework")),
          br(),
          h4("The Community Capitals Framework is based on the idea that communities have assets.
            These assets, or categories, refer to the resources that support positive individual change (i.e. economic mobility) and community resilience.
            Utility of the framework is the emphasis on assets, rather than deficits or needs. ")
        )
      ),
      fluidRow(
        style = "margin: 20px",
        width = 12,
        column(10, offset = 1,
               category_module_ui("rrci", "rrci"))
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
            title = "Maps and Indicators",
            category_module_ui("recovery_resources", "recovery_resources")
          ),
          tabPanel(
            title = "Data Table",
            table_module_ui("recovery_resources", "recovery_resources")
          ),
          tabPanel(title = "Data Methods", sources_ui("recovery_resources"))
        )
      )
    ),
    ## |__primary_care_resources tab ------------------------------------------------------------------------------------
    tabPanel(
      "Primary Care Resources",
      fluidRow(
        style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(
          widths = c(2, 10),
          tabPanel(
            title = "Maps and Indicators",
            category_module_ui("primary_care_resources", "primary_care_resources")
          ),
          tabPanel(
            title = "Data Table",
            table_module_ui("primary_care_resources", "primary_care_resources")
          ),
          tabPanel(title = "Data Methods", sources_ui("primary_care_resources"))
        )
      )
    ),
    ## |__social_and_economic_resources tab -----------------------------------------------------------------------------------
    tabPanel(
      "Social and Economic Resources",
      fluidRow(
        style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(
          widths = c(2, 10),
          tabPanel(
            title = "Maps and Indicators",
            category_module_ui(
              "social_and_economic_resources",
              "social_and_economic_resources"
            )
          ),
          tabPanel(
            title = "Data Table",
            table_module_ui(
              "social_and_economic_resources",
              "social_and_economic_resources"
            )
          ),
          tabPanel(title = "Data Methods", sources_ui("social_and_economic_resources"))
        )
      )
    ),
    ## |__demographic_characteristics tab ------------------------------------------------------------------------------------
    tabPanel(
      "Demographic Characteristics",
      fluidRow(
        style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(
          widths = c(2, 10),
          tabPanel(
            title = "Maps and Indicators",
            category_module_ui(
              "demographic_characteristics",
              "demographic_characteristics"
            )
          ),
          tabPanel(
            title = "Data Table",
            table_module_ui(
              "demographic_characteristics",
              "demographic_characteristics"
            )
          ),
          tabPanel(title = "Data Methods", sources_ui("demographic_characteristics"))
        )
      )
    ),
    ## |__ community_characteristics tab ---------------------------------------------------------------------------------
    tabPanel(
      "Community Characteristics",
      fluidRow(
        style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
        navlistPanel(
          widths = c(2, 10),
          tabPanel(
            title = "Maps and Indicators",
            category_module_ui("community_characteristics", "community_characteristics")
          ),
          tabPanel(
            title = "Data Table",
            table_module_ui("community_characteristics", "community_characteristics")
          ),
          tabPanel(title = "Data Methods", sources_ui("community_characteristics"))
        )
      )
    ),
    ## |__Key Terms tab --------------------------------------------------------------------------------
    tabPanel(
      "Key Terms",
      fluidRow(
        style = "margin-left: 6px; margin-right: 6px",
        width = 12,
        column(12,
               h2(
                 strong("Recovery Ready Community Key Terms")
               ),
               br(),
               h5("Placeholder Text"),
               p("Placeholder"))
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
  res_soc_econ = category_module_server("social_and_economic_resources")
  res_soc_econ_table = table_module_server("social_and_economic_resources")
  res_prim_care = category_module_server("primary_care_resources")
  res_prim_care_table = table_module_server("primary_care_resources")
  res_comm = category_module_server("community_characteristics")
  res_comm_table = table_module_server("community_characteristics")
  res_demo = category_module_server("demographic_characteristics")
  res_demo_table = table_module_server("demographic_characteristics")
}


## APP -------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)