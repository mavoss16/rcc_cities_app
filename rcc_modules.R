## CREATE MODULES

## MAKE MAP FUNCTION -----------------------------------------------------------------------------------
make_map <- function(selection) {
  # browser()
  # find variable that is to be mapped
  selected_var <- unique(rcc_labels_data$count_name[rcc_labels_data$drop_down_name == selection])
  fill_col_label <- rcc_labels_data$drop_down_name[rcc_labels_data$drop_down_name == selection]
  fill_col <- rcc_city_data %>% pull(selected_var)
  orig_col <- fill_col
  
  # create palette to be used
  pal <- colorBin(c("#f1d581", "#b6c45c", "#7db257", "#4c9c53", "#34834b", "#146c37"), domain = fill_col, bins = 5, pretty = FALSE)
  
  # mutate data for map bubbles
  fill_col = (fill_col - min(fill_col))/(max(fill_col) - min(fill_col))
  fill_col = (fill_col * 10) + 1
  
  # make map
  rcc_city_data = rcc_city_data %>%
    mutate(value = fill_col,
           popup = stringr::str_c("<strong>", city, "</strong>",
                                  "<br/>",
                                  fill_col_label, ": ", round(orig_col, 2)) %>%
             purrr::map(htmltools::HTML))
  
  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = rcc_city_data, lng = ~lng, lat = ~lat,
                     radius = ~value, fillOpacity = 0.5,
                     stroke = TRUE, weight = 2, opacity = 0.5,
                     label = ~popup, 
                     labelOptions = labelOptions(direction = "bottom",
                                                 style = list(
                                                   "font-size" = "12px",
                                                   "border-color" = "rgba(0,0,0,0.5)",
                                                   direction = "auto"
                                                 )))
}

## MAKE TABLE FUNCTION --------------------------------------
make_table <- function(category){
  print(category)
  category = stringr::str_replace_all(tolower(category), " ", "_")
  print(category)
  selected_var <- unique(rcc_labels_data$count_name[rcc_labels_data$category == category])
  var_pretty_names <- unique(rcc_labels_data$drop_down_name[rcc_labels_data$category == category])
  
  table_data = rcc_city_data %>% select(all_of(c("city", "county", "rrci_rank", selected_var)))
  names(table_data) = c("City", "County", "RRCI Rank", var_pretty_names)
  
  #green_pal = function(x) rgb(colorRampPalette(c("#ffffff", "#71CA97"))(x))

  reactable(table_data, pagination = FALSE,
            defaultColDef = colDef(style = function(value, index, name){
              if(name != "City" && name != "County"){
                if(name == "RRCI Rank"){
                  green_pal = colorNumeric(colorRamp(c("#ffffff", "#71CA97")), domain = c(min(table_data[,name]), max(table_data[,name])), reverse = TRUE)
                }
                else{
                  green_pal = colorNumeric(colorRamp(c("#ffffff", "#71CA97")), domain = c(min(table_data[,name]), max(table_data[,name])))
                }
                list(background = green_pal(value))
              }
              else{
                list(background = "#ffffff")
              }
            }
            )
            )
}



## DEFINE UI & SERVER FOR SELECTIONS -------------------------------------------------------------------
variable_select_ui <- function(id, category) {
  ns <- NS(id)
  
  # define choices for variable selection
  variable_options <- as.list(unique(rcc_labels_data$drop_down_name[rcc_labels_data$category == category]))
  #subdomain_options <- as.list(unique(rcc_labels_data$sub_domain[rcc_labels_data$category == category]))
  
  # assemble UI elements
  tagList(
    h4(strong("Selector"), align = "left"),
    p(),
    selectInput(ns("which_variable"), "Variable", choices = variable_options),
    p(),
    #selectInput(ns("which_subdomain"), "Sub Domain", choices = subdomain_options),
    #p()
  )
}

variable_select_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      ## update input$subdomain 
      # Is this because the subdomain options changed based on the domain? I think yes
      # observe({
      #   variable_options <- unique(rcc_labels_data$sub_domain[rcc_labels_data$domain == stringr::str_replace_all(input$which_domain, " ", "_")])
      #   updateSelectInput(session, "which_subdomain", choices = variable_options)
      # })
      
      return(
        list(
          variable = reactive({ input$which_variable })#,
          # subdomain = reactive({ input$which_subdomain })
        )
      )
    }
  )
}

## DEFINE UI & SERVER FOR MAPS -------------------------------------------------------------------------
map_ui <- function(id) {
  ns <- NS(id)
  
  # assemble UI elements
  tagList(
    h4(textOutput(ns("var"))),
    #h6(textOutput(ns("lab"))),
    
    leafletOutput(ns("map"), width = "1000px")
    # Maybe use if statement to adjust for first page
  )
}

map_server <- function(id, selections) {
  moduleServer(
    id,
    function(input, output, session) {
      map <- reactive({
        select_var <- selections$variable()
        p <- make_map(select_var)
        return(p)
      })
      
      var <- reactive({
        var <- selections$variable()
        return(var)
      })
      
      # Label to go above the map. Would need to add to labels sheet
      # lab <- reactive({
      #   lab <- unique(rcc_labels_data$drop_down_name[rcc_labels_data$sub_domain == selections$subdomain()])
      #   return(lab)
      # })
      
      output$map <- renderLeaflet({ map() })
      output$var <- renderText({ var() })
      #output$lab <- renderText({ lab() })
    }
  )
}

## DEFINE UI and SERVER FOR TABLE -------------------------------------------------------------
table_ui <- function(id){
  ns <- NS(id)
  table_title = stringr::str_replace_all(id, "_", " ") %>% stringr::str_replace_all("-table", " Data Table") %>% tools::toTitleCase()
  
  # assemble UI elements
  tagList(
    h4(strong(table_title), align = "left"),
    p(),
    #h5(textOutput(ns("var"))),
    #h6(textOutput(ns("lab"))),
    reactableOutput(ns("table"), width = "120%")
  )
}

table_server <- function(id, category) {
  moduleServer(
    id,
    function(input, output, session) {
      
      table = reactive({
        return(make_table(category))
      })
      
      # var <- reactive({
      #   var <- selections$variable()
      #   return(var)
      # })
      
      # Label to go above the map. Would need to add to labels sheet
      # lab <- reactive({
      #   lab <- unique(rcc_labels_data$drop_down_name[rcc_labels_data$sub_domain == selections$subdomain()])
      #   return(lab)
      # })
      
      output$table <- renderReactable({ table() })
      #output$var <- renderText({ var() })
      #output$lab <- renderText({ lab() })
    }
  )
}


## TABLE MODULE UI & SERVER -------------------------------------
table_module_ui <- function(id, category){
  ns <- NS(id)
  tagList(
    fluidRow(style = "margin: 6px", width = 12,
             column(12, align = "left",
                    table_ui(ns("table"))
                    )
             )
  )
}

table_module_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      tables <- table_server("table", id)
    }
  )
}

## DEFINE UI & SERVER FOR EACH CATEGORY -----------------------------------------------------------------
category_module_ui <- function(id, category) {
  ns <- NS(id)
  
  category_title = stringr::str_replace_all(category, "_", " ") %>% tools::toTitleCase()
  
  tagList(
    ## row for text
    fluidRow(style = "margin: 6px", width = 12,
             column(12, align = "left",
                    h3(strong(category_title)), 
                    p(), 
                    p(strong("Select a variable using the map selector."),
                      "The resulting maps will display the 30 Iowa cities with bubbles that correspond to the amount of the selected variable.",
                      "Scroll over the bubbles to see the data and the city names.",
                      "Select the Data Table tab on the left to see the values for all 30 cities, or the Data Methods tab to learn where we found the data."))
    ),
    ## row for selector & map
    fluidRow(style = "margin: 6px",
             column(width = 3,
                    variable_select_ui(ns("selections"), category)
             ),
             column(width = 9,
                    map_ui(ns("map"))
             )
    )
  )
}

category_module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      selections <- variable_select_server("selections")
      maps <- map_server("map", selections = selections)
    }
  )
}



## MAKE MODULES FOR SOURCES TAB ------------------------------------------------------------------------
## (INCOMPLETE)
## Honestly, this is a mess and I did not have the time to think of a better solution.
## Ideally, this should probably become a function.
sources_ui <- function(id){
  ns <- NS(id)
  
  if(id == "Financial"){
    tagList(
      fluidRow(style = "margin: 6px", width = 12,
               column(12, align = "left",
                      h3(strong("Data, Measures, and Methods"))
               )
      ),
      fluidRow(style = "margin: 6px", width = 12,
               column(6, 
                      wellPanel(strong('Commerce'), 
                                p(),
                                em('Description.'),
                                br(), 
                                p("Does the community have an active business community to provide necessary goods and services to its citizens and create new job opportunities?"),
                                p(),
                                em('How We Measure Commerce.'),
                                br('We calculate the commerce index using information on:'),
                                tags$li('Number of businesses per 10k people'),
                                tags$li('Number of new businesses per 10k people'),
                                tags$li('Percent county in agriculture acres'),
                                tags$li('Land value per acre'),
                                tags$li("Net income per farm operation"),
                                tags$li("Percent emp in ag, forestry, or mining"),
                                br(),
                                p("Preliminary analyses examined distributional characteristics for each indicator item. 
              To reduce potential influences due to extreme scores or differences in indicator variability within the domain, 
              each indicator was split into five groups determined by quintile rankings. 
              Negative indicators for which high scores in the raw scale reflected low levels of the domain were reversed after 
              quintile ranking to ensure that lower/higher scores on all indicators reflected the same end of the domain scale. 
              The resulting measures then indexed counties in the lowest 20%, between 20%-40%, 40%-60%, 60%-80%, and the highest 
              20% of all counties. Quintile ranks on each of the indictors were averaged to compute the total domain score such 
              that county placement in higher quintiles indicates higher levels of the Commerce composite index. "),
                                p(),
                                em('Data sources.'),
                                tags$li('County Business Patterns, 2016 and 2018'),
                                tags$li('American Community Survey 2014/18 (5-year) estimates.')
                      )
               ),                         
               column(6, 
                      wellPanel(strong('Financial Well Being'), 
                                p(),
                                em('Description.'),
                                br(), 
                                p("Do people live in a community with balanced financial status?"),
                                p(),
                                em('How We Measure Financial Well Being.'),
                                br('We calculate financial well being using information on:'),
                                tags$li('Gini Index:income inequality per county'),
                                tags$li('Percent 18-64 in poverty during last 12 mos, 2018'),
                                tags$li('Percent HH recieve public assist during past 12 mos, 2018'),
                                tags$li('Percent HH with supplemental sec income past 12 mos, 2018'),
                                tags$li('Median HH income past 12 mos inflation adjusted, 2018'),
                                tags$li('Percent less than 4 year degree'),
                                tags$li('Mean credit score'),
                                br(),
                                p('Preliminary analyses examined distributional characteristics for each indicator item. 
              To reduce potential influences due to extreme scores or differences in indicator variability within the domain, 
              each indicator was split into five groups determined by quintile rankings. Negative indicators for which high scores 
              in the raw scale reflected low levels of the domain were reversed after quintile ranking to ensure that lower/higher 
              scores on all indicators reflected the same end of the domain scale. The resulting measures then indexed counties in 
              the lowest 20%, between 20%-40%, 40%-60%, 60%-80%, and the highest 20% of all counties. Quintile ranks on each of the 
              indictors were averaged to compute the total domain score such that county placement in higher quintiles indicates higher 
              levels of the Commerce composite index. '),
                                p(),
                                em('Data sources.'),
                                tags$li('American Community Survey 2014/18 (5-year) estimates.'),
                                tags$li('American Community Survey 2018 (1-year) estimates.'),
                                tags$li('American Community Survey 2014/18 (5-year) estimates.'),
                                tags$li("Onboard Informatics' dataset")
                      )
               )
      ),
      fluidRow(style = "margin: 6px", width = 12,
               column(6, 
                      wellPanel(strong('Employment Index'), 
                                p(),
                                em('Description.'),
                                br(), 
                                p('Are there employment opportunities available that will foster upward economic mobility?'),
                                p(),
                                em('How We Measure the Employment Index.'),
                                br('We calculate the employment index using information on:'),
                                tags$li('Average commute time per county'),
                                tags$li('Jan-Mar 2020 monthly unemployment rate'),
                                tags$li('Apr - May 2020 monthly unemployment minus the pre_covid rate'),
                                tags$li('Percent of people over 16 that are in the labor force'),
                                br(),
                                p('Preliminary analyses examined distributional characteristics for each indicator item. To reduce potential 
                influences due to extreme scores or differences in indicator variability within the domain, each indicator 
                was split into five groups determined by quintile rankings. Negative indicators for which high scores in the 
                raw scale reflected low levels of the domain were reversed after quintile ranking to ensure that lower/higher 
                scores on all indicators reflected the same end of the domain scale. The resulting measures then indexed counties 
                in the lowest 20%, between 20%-40%, 40%-60%, 60%-80%, and the highest 20% of all counties. Quintile ranks on each of 
                the indictors were averaged to compute the total domain score such that county placement in higher quintiles indicates 
                higher levels of the Commerce composite index. '),
                                p(),
                                em('Data sources.'),
                                tags$li('American Community Survey 2014/18 (5-year) estimates.'),
                                tags$li('Local Area Unemployment Statistics (LAUS)')
                      )
               ),
               column(6, 
                      wellPanel(strong('Economic Diversification Index'), 
                                p(),
                                em('Description.'),
                                br(), 
                                p('To what extent are communities dependent on single companies, organizations, or industries for the employment, investment, and opportunities available to them?'),
                                p(),
                                em('How We Measure the Economic Diversification Index.'),
                                br('We calculate the economic diversification index using information on:'),
                                tags$li('HHI Diversity composite index of 2 items, quintiles'),
                                tags$li('Distribution of Employment by Company/Industry, 1-10,000'),
                                tags$li('Distribution of Income Generated by Industry, 1-10,000'),
                                br(),
                                p('Preliminary analyses examined distributional characteristics for each indicator item. To reduce potential 
                influences due to extreme scores or differences in indicator variability within the domain, each indicator 
                was split into five groups determined by quintile rankings. Negative indicators for which high scores in the 
                raw scale reflected low levels of the domain were reversed after quintile ranking to ensure that lower/higher 
                scores on all indicators reflected the same end of the domain scale. The resulting measures then indexed counties 
                in the lowest 20%, between 20%-40%, 40%-60%, 60%-80%, and the highest 20% of all counties. Quintile ranks on each of 
                the indictors were averaged to compute the total domain score such that county placement in higher quintiles indicates 
                higher levels of the Commerce composite index. '),
                                p(),
                                em('Data sources.'),
                                tags$li('American Community Survey 2014/18 (5-year) estimates.')                                                              ))
      )
    )
  } else if(id == "Built"){
    tagList(
      fluidRow(style = "margin: 6px", width = 12, 
               column(12, align = "left", 
                      h3(strong("Data, Measures, and Methods"))
               )
      ),
      fluidRow(style = "margin: 6px", width = 12,
               column(4, 
                      wellPanel(strong('Remote Work Accessibility'), 
                                p(),
                                em('Description.'),
                                br(), tags$b('The remote work relative accessibility measure highlights counties where residents may have difficulty working remotely if instructed to do so.'),
                                ('It conceptualizes four telecommunication infrastructure and employment characteristics as potential barriers, providing
              a relative ranking of county telework preparedness.'),
                                p(),
                                em('How We Measure Remote Work Accessibility.'),
                                br('We calculate remote work relative accessibility using information on percent:'),
                                tags$li('Households with no broadband internet subscription.'),
                                tags$li('Persons in labor force with no computer available.'),
                                tags$li('Persons who are not currently working remotely and are employed in telework unfriendly occupations
              (service, natural, construction, maintenance, production, transportation, material moving, and military specific occupations).'),
                                tags$li('Persons who are not currently working remotely and are employed in telework unfriendly industries 
              (construction, manufacturing, wholesale, retail, transportation and warehousing, utilities, and government, including armed forces).'),
                                br('We compute quintile cut-offs for each indicator. County placement in a higher quintile indicates lower relative accessibility.
              We assign descriptive labels for county relative accessibility based on whether they placed in 4th or 5th quintile a certain number of times. 
              The more times a county places in the 4th or 5th quintile on relevant indicators, the lower its relative remote work accessibility:'),
                                tags$li('Very high: 0 indicators.'),
                                tags$li('High: 1 indicator.'),
                                tags$li('Medium: 2 indicators.'), 
                                tags$li('Low: 3 indicators.'),
                                tags$li('Very low: all 4 indicators.'),
                                p(),
                                em('Data source.'),
                                p(a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target = "_blank"), "2014/18 (5-year) estimates.")
                      )
               ),                         
               column(4, 
                      wellPanel(strong('Remote Education Accessibility'), 
                                p(),
                                em('Description.'),
                                br(), tags$b('The remote education relative accessibility measure highlights counties where K-12 students may have difficulty participating in online education.'), 
                                ('It considers telecommunication infastructure and K-12 enrollment in providing a relative ranking of county K-12 remote education preparedness.'),
                                p(),
                                em('How We Measure Remote Education Accessibility.'),
                                br('We calculate remote education relative accessibility using information on percent:'),
                                tags$li('Households with no internet access subscription.'),
                                tags$li('Population under age 18 without a computer.'),
                                tags$li('Population enrolled in K-12.'),
                                br('We compute quintile cut-offs for each indicator. County placement in a higher quintile indicates lower relative accessibility.
              We assign descriptive labels for county relative accessibility based on whether they placed in 4th or 5th quintile a certain number of times.
              The more times a county places in the 4th or 5th quintile on relevant indicators, the lower its relative remote education accessibility:'),
                                tags$li('High: 0 indicators.'),
                                tags$li('Medium: 1 indicator.'),
                                tags$li('Low: 2 indicators.'),
                                tags$li('Very low: all 3 indicators.'),
                                p(),
                                em('Data source.'),
                                p(a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target = "_blank"), "2014/18 (5-year) estimates.")
                      )
               ),
               column(4,
                      wellPanel(strong('Telemental Health Accessibility'), 
                                p(),
                                em('Description.'),
                                br(), tags$b('The telemental health relative accessibility measure highlights counties where high need for mental health services is coupled with barriers to access.'), 
                                ('It considers telecommunication infastructure, health insurance, in-person provider availability, and mental health status
              in providing a relative ranking of county K-12 telemental health accessibility.'),
                                p(),
                                em('How We Measure Telemental Health Accessibility.'),
                                br('We calculate telemental health relative accessibility using information on:'),
                                tags$li('Percent households without internet access.'),
                                tags$li('Percent households with no computer.'),
                                tags$li('Average number of poor mental health days in past month.'),
                                tags$li('Number of mental health providers per 100,000 population (reverse-coded).'),
                                tags$li('Percent population under age 65 without health insurance.'),
                                br('We compute quintile cut-offs for each indicator. County placement in a higher quintile indicates lower relative accessibility.
              We assign descriptive labels for county relative accessibility based on whether they placed in 4th or 5th quintile a certain number of times.
              The more times a county places in the 4th or 5th quintile on relevant indicators, the lower its relative telemental health accessibility:'),
                                tags$li('Very high: 0 indicators.'),
                                tags$li('High: 1 indicator.'),
                                tags$li('Medium: 2 or 3 indicators.'),
                                tags$li('Low: 4 indicators.'),
                                tags$li('Very low: all 5 indicators'),
                                p(),
                                em('Data source.'),
                                p(a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target = "_blank"), "2014/18 (5-year) estimates and",
                                  a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources', 'County Health Rankings', target = "_blank"), "2019.")
                      )
               )
      )
    )
  } else {
    tagList(
      fluidRow(style = "margin: 6px", width = 12,
               column(12, align = "left",
                      h3(strong("Data, Measures, and Methods"))
               )
      ),
      fluidRow(style = "margin: 6px", width = 12,
               column(4, 
                      wellPanel(strong('Domain 1'),
                                p(),
                                em('Description.'),
                                br(), 
                                br(),
                                p(),
                                em('How We Measure ...'),
                                br('We calculate ... using information on:'),
                                tags$li('Percent households ...'),
                                tags$li('Percent households ...'),
                                br('We compute quintile cut-offs for each indicator.... :'),
                                tags$li('Very high: 0 indicators.'),
                                tags$li('High: 1 indicator.'),
                                tags$li('Medium: 2 or 3 indicators.'),
                                tags$li('Low: 4 indicators.'),
                                tags$li('Very low: all 5 indicators'),
                                p(),
                                em('Data source.'),
                                p()
                      )
               ),
               column(4, 
                      wellPanel(strong('Domain 2'),
                                p(),
                                em('Description.'),
                                br(), 
                                br(),
                                p(),
                                em('How We Measure ...'),
                                br('We calculate ... using information on:'),
                                tags$li('Percent households ...'),
                                tags$li('Percent households ...'),
                                br('We compute quintile cut-offs for each indicator.... :'),
                                tags$li('Very high: 0 indicators.'),
                                tags$li('High: 1 indicator.'),
                                tags$li('Medium: 2 or 3 indicators.'),
                                tags$li('Low: 4 indicators.'),
                                tags$li('Very low: all 5 indicators'),
                                p(),
                                em('Data source.'),
                                p()
                      )
               ),
               column(4, 
                      wellPanel(strong('Domain 3'),
                                p(),
                                em('Description.'),
                                br(), 
                                br(),
                                p(),
                                em('How We Measure ...'),
                                br('We calculate ... using information on:'),
                                tags$li('Percent households ...'),
                                tags$li('Percent households ...'),
                                br('We compute quintile cut-offs for each indicator.... :'),
                                tags$li('Very high: 0 indicators.'),
                                tags$li('High: 1 indicator.'),
                                tags$li('Medium: 2 or 3 indicators.'),
                                tags$li('Low: 4 indicators.'),
                                tags$li('Very low: all 5 indicators'),
                                p(),
                                em('Data source.'),
                                p()
                      )
               )
      )
    )
  }
}


