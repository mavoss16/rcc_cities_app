## CREATE MODULES

## MAKE MAP FUNCTION -----------------------------------------------------------------------------------
make_map = function(selection) {
  # browser()
  # find variable that is to be mapped
  selected_var = unique(rcc_labels_data$count_name[rcc_labels_data$drop_down_name == selection])
  fill_col_label = rcc_labels_data$drop_down_name[rcc_labels_data$drop_down_name == selection]
  fill_col = rcc_city_data %>% pull(selected_var)
  orig_col = fill_col
  
  # create palette to be used
  pal = colorBin(
    c(
      "#f1d581",
      "#b6c45c",
      "#7db257",
      "#4c9c53",
      "#34834b",
      "#146c37"
    ),
    domain = fill_col,
    bins = 5,
    pretty = FALSE
  )
  
  # mutate data for map bubbles
  fill_col = (fill_col - min(fill_col)) / (max(fill_col) - min(fill_col))
  fill_col = (fill_col * 10) + 1
  
  # make map
  rcc_city_data = rcc_city_data %>%
    mutate(
      value = fill_col,
      popup = stringr::str_c(
        "<strong>",
        city,
        "</strong>",
        "<br/>",
        fill_col_label,
        ": ",
        round(orig_col, 2)
      ) %>%
        purrr::map(htmltools::HTML)
    )
  
  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      data = rcc_city_data,
      lng = ~ lng,
      lat = ~ lat,
      radius = ~ value,
      fillOpacity = 0.5,
      stroke = TRUE,
      weight = 2,
      opacity = 0.5,
      label = ~ popup,
      labelOptions = labelOptions(
        direction = "bottom",
        style = list(
          "font-size" = "12px",
          "border-color" = "rgba(0,0,0,0.5)",
          direction = "auto"
        )
      )
    )
}

## MAKE TABLE FUNCTION --------------------------------------
make_table = function(category) {
  print(category)
  category = stringr::str_replace_all(tolower(category), " ", "_")
  print(category)
  selected_var = unique(rcc_labels_data$count_name[rcc_labels_data$category == category])
  var_pretty_names = unique(rcc_labels_data$drop_down_name[rcc_labels_data$category == category])
  
  table_data = rcc_city_data %>% select(all_of(c(
    "city", "county", "rrci_rank", selected_var
  )))
  names(table_data) = c("City", "County", "RRCI Rank", var_pretty_names)
  
  #green_pal = function(x) rgb(colorRampPalette(c("#ffffff", "#71CA97"))(x))
  
  reactable(
    table_data,
    pagination = FALSE,
    defaultColDef = colDef(
      format = colFormat(separators = TRUE),
      style = function(value, index, name) {
        if (name != "City" && name != "County") {
          if (name == "RRCI Rank") {
            green_pal = colorNumeric(colorRamp(c("#ffffff", "#71CA97")),
                                     domain = c(min(table_data[, name]), max(table_data[, name])),
                                     reverse = TRUE)
          }
          else{
            green_pal = colorNumeric(colorRamp(c("#ffffff", "#71CA97")), domain = c(min(table_data[, name]), max(table_data[, name])))
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
variable_select_ui = function(id, category) {
  ns = NS(id)
  
  # define choices for variable selection
  variable_options = as.list(unique(rcc_labels_data$drop_down_name[rcc_labels_data$category == category]))
  #subdomain_options = as.list(unique(rcc_labels_data$sub_domain[rcc_labels_data$category == category]))
  
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

variable_select_server = function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns = session$ns
                 ## update input$subdomain
                 # Is this because the subdomain options changed based on the domain? I think yes
                 # observe({
                 #   variable_options = unique(rcc_labels_data$sub_domain[rcc_labels_data$domain == stringr::str_replace_all(input$which_domain, " ", "_")])
                 #   updateSelectInput(session, "which_subdomain", choices = variable_options)
                 # })
                 
                 return(
                   list(variable = reactive({
                     input$which_variable
                   })))#,
                   # subdomain = reactive({ input$which_subdomain })))
               })
}

## DEFINE UI & SERVER FOR MAPS -------------------------------------------------------------------------
map_ui = function(id) {
  ns = NS(id)
  
  # assemble UI elements
  tagList(
    h4(textOutput(ns("var"))),
    #h6(textOutput(ns("lab"))),
    
    leafletOutput(ns("map"), width = "1150px", height = "500px")
    # Maybe use if statement to adjust for first page)
  )
}

map_server = function(id, selections) {
  moduleServer(id,
               function(input, output, session) {
                 map = reactive({
                   select_var = selections$variable()
                   p = make_map(select_var)
                   return(p)
                 })
                 
                 var = reactive({
                   var = selections$variable()
                   return(var)
                 })
                 
                 # Label to go above the map. Would need to add to labels sheet
                 # lab = reactive({
                 #   lab = unique(rcc_labels_data$drop_down_name[rcc_labels_data$sub_domain == selections$subdomain()])
                 #   return(lab)
                 # })
                 
                 output$map = renderLeaflet({
                   map()
                 })
                 output$var = renderText({
                   var()
                 })
                 #output$lab = renderText({ lab() })
               })
}

## DEFINE UI and SERVER FOR TABLE -------------------------------------------------------------
table_ui = function(id) {
  ns = NS(id)
  table_title = stringr::str_replace_all(id, "_", " ") %>% stringr::str_replace_all("-table", " Data Table") %>% tools::toTitleCase()
  
  # assemble UI elements
  tagList(
    h4(strong(table_title), align = "left"),
    p(
      "The table rows are initially sorted by RRCI rank. Click on a column name to sort the rows by that column's values."
    ),
    #h5(textOutput(ns("var"))),
    #h6(textOutput(ns("lab"))),
    reactableOutput(ns("table"), width = "1250px")
  )
}

table_server = function(id, category) {
  moduleServer(id,
               function(input, output, session) {
                 table = reactive({
                   return(make_table(category))
                 })
                 
                 # var = reactive({
                 #   var = selections$variable()
                 #   return(var)
                 # })
                 
                 # Label to go above the map. Would need to add to labels sheet
                 # lab = reactive({
                 #   lab = unique(rcc_labels_data$drop_down_name[rcc_labels_data$sub_domain == selections$subdomain()])
                 #   return(lab)
                 # })
                 
                 output$table = renderReactable({
                   table()
                 })
                 #output$var = renderText({ var() })
                 #output$lab = renderText({ lab() })
               })
}


## TABLE MODULE UI & SERVER -------------------------------------
table_module_ui = function(id, category) {
  ns = NS(id)
  tagList(fluidRow(
    style = "margin: 6px",
    width = 12,
    column(12, align = "left",
           table_ui(ns("table")))
  ))
}

table_module_server = function(id) {
  moduleServer(id,
               function(input, output, session) {
                 tables = table_server("table", id)
               })
}

## DEFINE UI & SERVER FOR EACH CATEGORY -----------------------------------------------------------------
category_module_ui = function(id, category) {
  ns = NS(id)
  
  text = "The resulting maps will display the 30 Iowa cities considered for RCCs, with bubble sizes that correspond to the amount of the selected
  variable. Scroll over the bubbles to see the data and city names. Select the Data Table on the left to see the values for all 30 cities,
  or the Data Methods tab to learn where we found the data."
  
  category_title = stringr::str_replace_all(category, "_", " ") %>% tools::toTitleCase()
  if (category_title == "Rrci") {
    category_title = "Recovery Ready Community Index"
    text = ""
  }
  
  
  
  tagList(
    ## row for text
    fluidRow(
      style = "margin: 6px",
      width = 12,
      column(12, align = "left",
             h3(strong(category_title)),
             p(),
             p(
               strong("Select a variable using the map selector."), text
             ))
    ),
    ## row for selector & map
    fluidRow(
      style = "margin: 6px",
      column(width = 2,
             variable_select_ui(ns("selections"), category)),
      column(width = 10,
             map_ui(ns("map")))
    )
  )
}

category_module_server = function(id) {
  moduleServer(id,
               function(input, output, session) {
                 selections = variable_select_server("selections")
                 maps = map_server("map", selections = selections)
               })
}



## MAKE MODULES FOR SOURCES TAB ------------------------------------------------------------------------
## (INCOMPLETE)
## Honestly, this is a mess and I did not have the time to think of a better solution.
## Ideally, this should probably become a function.
sources_ui = function(id) {
  ns = NS(id)
  
  title = stringr::str_replace_all(id, "_", " ") %>% tools::toTitleCase()
  
  if (id == "recovery_resources") {
    tagList(
      fluidRow(
        style = "margin: 6px",
        width = 12,
        column(
          12,
          align = "left",
          h3(strong(title, " Data Methods")),
          p(
            "Click on the sources to open links (where possible) from which the data was collected."
          )
        )
      ),
      fluidRow(
        style = "margin: 6px",
        width = 12,
        column(
          6,
          wellPanel(
            strong('Mutual Aid Meetings'),
            p(),
            em('Source(s):'),
            br(),
            tags$a(href = "https://www.aa-iowa.org/meetings/", "Alcoholics Anonymous - Iowa"),
            br(),
            tags$a(href = "https://www.na-iowa.org/meetings/", "Narcotics Anonymous - Iowa"),
            br(),
            tags$a(href = "https://adultchildren.org/mtsearch", "Adult Children of Alcoholics"),
            br(),
            tags$a(href = "https://al-anon.org/al-anon-meetings/find-an-alateen-meeting/", "Al-Anon"),
            br(),
            tags$a(href = "http://draonline.qwknetllc.com/meetings_dra/usa/iowa.html", "Dual Recovery Anonymous"),
            br(),
            tags$a(href = "https://www.nar-anon.org/", "Nar-Anon"),
            br(),
            tags$a(href = "https://www.smartrecoverytest.org/local/full-meeting-list-download/", "SMART"),
            br(),
            tags$a(href = "https://locator.crgroups.info/", "Celebrate Recovery"),
            br(),
            tags$a(href = "https://www.facebook.com/crushofiowa/", "CRUSH"),
            br(),
            tags$a(href = "https://refugerecovery.org/meetings?tsml-day=any&tsml-region=iowa", "Refuge Recovery"),
            br(),
            tags$a(href = "https://www.pillsanonymous.org/meetings/find-a-meeting/", "Pills Anonymous"),
            br(),
            p(),
            em("Methods:"),
            br(),
            p("Counted the number of mutual aid meetings by city.")
          )
        ),
        column(
          6,
          wellPanel(
            strong('Mutual Aid Meetings Per 10,000'),
            p(),
            em('Source(s):'),
            br(),
            p("Mutual Aid Meetings Data"),
            tags$a(
              href = "https://www.census.gov/programs-surveys/acs",
              "American Community Survey 2014/18 (5-year) Population Estimates"
            ),
            br(),
            p(),
            em("Methods:"),
            br(),
            p(
              "Counted the number of mutual aid meetings by city, divided by the ACS 2014/18 population estimate, and multiplied by 10,000."
            )
          )
        ),
        fluidRow(
          style = "margin: 6px",
          width = 12,
          column(6,
                 wellPanel(
                   strong('Peer Support Providers'),
                   p(),
                   em('Source(s):'),
                   br(),
                   p(
                     "Todd Lange, Recovery & Resilience Coordinator within AmeriCorp"
                   ),
                   br(),
                   p(),
                   em("Methods:"),
                   br(),
                   p("Counted the number of peer support providers by city.")
                 )),
          column(6,
                 wellPanel(
                   strong('Drug Drop Off Locations'),
                   p(),
                   em('Source(s):'),
                   br(),
                   tags$a(href = "https://iowa.maps.arcgis.com/apps/webappviewer/index.html?id=5377c6", "Iowa Office of Drug Control Policy"),
                 )),
          fluidRow(
            style = "margin: 6px",
            width = 12,
            column(4,
                   wellPanel(
                     strong('SUD and Problem Gambling Treatment Locations'),
                     p(),
                     em('Source(s):'),
                     br(),
                     tags$a(href = "https://idph.iowa.gov/Portals/1/userfiles/166/Licensure/All%", "Iowa Department of Public Health"),
                   )),
            column(
              4,
              wellPanel(
                strong('Recovery Housing'),
                p(),
                em('Source(s):'),
                br(),
                tags$a(href = "https://www.alltreatment.com/ia/accredited/", "AllTreatment.com"),
                br(),
                tags$a(href = "https://www.transitionalhousing.org/state/Iowa", "TransitionalHousing.org"),
                br(),
                tags$a(href = "https://www.womensoberhousing.com/state/iowa.html", "WomenSoberHousing.com"),
                br(),
                tags$a(href = "https://www.addicted.org/iowa-long-term-drug-rehab.html", "Addicted.org"),
                br(),
                tags$a(href = "https://www.recovery.org/browse/Iowa/", "Recovery.org"),
                br(),
                tags$a(href = "https://www.drug-rehabs.org/Iowa-drug-rehab-alcohol-rehabs-program.htm", "Drug-rehabs.org")
              )
            ),
            column(4,
                   wellPanel(
                     strong('Medically Assisted Treatment Locations'),
                     p(),
                     em('Source(s):'),
                     br(),
                     tags$a(href = "https://idph.iowa.gov/mat", "Iowa Department of Public")
                   ))
          )
        )
      )
    )
  } else if (id == "Built") {
    tagList(
      fluidRow(
        style = "margin: 6px",
        width = 12,
        column(12, align = "left",
               h3(strong(" Data Methods")))
      ),
      fluidRow(
        style = "margin: 6px",
        width = 12,
        column(
          4,
          wellPanel(
            strong('Remote Work Accessibility'),
            p(),
            em('Source(s):'),
            br(),
            tags$b(
              'The remote work relative accessibility measure highlights counties where residents may have difficulty working remotely if instructed to do so.'
            ),
            (
              'It conceptualizes four telecommunication infrastructure and employment characteristics as potential barriers, providing
              a relative ranking of county telework preparedness.'
            ),
            p(),
            em('How We Measure Remote Work Accessibility.'),
            br(
              'We calculate remote work relative accessibility using information on percent:'
            ),
            tags$li('Households with no broadband internet subscription.'),
            tags$li('Persons in labor force with no computer available.'),
            tags$li(
              'Persons who are not currently working remotely and are employed in telework unfriendly occupations
              (service, natural, construction, maintenance, production, transportation, material moving, and military specific occupations).'
            ),
            tags$li(
              'Persons who are not currently working remotely and are employed in telework unfriendly industries
              (construction, manufacturing, wholesale, retail, transportation and warehousing, utilities, and government, including armed forces).'
            ),
            br(
              'We compute quintile cut-offs for each indicator. County placement in a higher quintile indicates lower relative accessibility.
              We assign descriptive labels for county relative accessibility based on whether they placed in 4th or 5th quintile a certain number of times.
              The more times a county places in the 4th or 5th quintile on relevant indicators, the lower its relative remote work accessibility:'
            ),
            tags$li('Very high: 0 indicators.'),
            tags$li('High: 1 indicator.'),
            tags$li('Medium: 2 indicators.'),
            tags$li('Low: 3 indicators.'),
            tags$li('Very low: all 4 indicators.'),
            p(),
            em('Data source.'),
            p(
              a(
                href = 'https://www.census.gov/programs-surveys/acs',
                'American Community Survey',
                target = "_blank"
              ),
              "2014/18 (5-year) estimates."
            )
          )
        ),
        column(
          4,
          wellPanel(
            strong('Remote Education Accessibility'),
            p(),
            em('Source(s):'),
            br(),
            tags$b(
              'The remote education relative accessibility measure highlights counties where K-12 students may have difficulty participating in online education.'
            ),
            (
              'It considers telecommunication infastructure and K-12 enrollment in providing a relative ranking of county K-12 remote education preparedness.'
            ),
            p(),
            em('How We Measure Remote Education Accessibility.'),
            br(
              'We calculate remote education relative accessibility using information on percent:'
            ),
            tags$li('Households with no internet access subscription.'),
            tags$li('Population under age 18 without a computer.'),
            tags$li('Population enrolled in K-12.'),
            br(
              'We compute quintile cut-offs for each indicator. County placement in a higher quintile indicates lower relative accessibility.
              We assign descriptive labels for county relative accessibility based on whether they placed in 4th or 5th quintile a certain number of times.
              The more times a county places in the 4th or 5th quintile on relevant indicators, the lower its relative remote education accessibility:'
            ),
            tags$li('High: 0 indicators.'),
            tags$li('Medium: 1 indicator.'),
            tags$li('Low: 2 indicators.'),
            tags$li('Very low: all 3 indicators.'),
            p(),
            em('Data source.'),
            p(
              a(
                href = 'https://www.census.gov/programs-surveys/acs',
                'American Community Survey',
                target = "_blank"
              ),
              "2014/18 (5-year) estimates."
            )
          )
        ),
        column(
          4,
          wellPanel(
            strong('Telemental Health Accessibility'),
            p(),
            em('Source(s):'),
            br(),
            tags$b(
              'The telemental health relative accessibility measure highlights counties where high need for mental health services is coupled with barriers to access.'
            ),
            (
              'It considers telecommunication infastructure, health insurance, in-person provider availability, and mental health status
              in providing a relative ranking of county K-12 telemental health accessibility.'
            ),
            p(),
            em('How We Measure Telemental Health Accessibility.'),
            br(
              'We calculate telemental health relative accessibility using information on:'
            ),
            tags$li('Percent households without internet access.'),
            tags$li('Percent households with no computer.'),
            tags$li('Average number of poor mental health days in past month.'),
            tags$li(
              'Number of mental health providers per 100,000 population (reverse-coded).'
            ),
            tags$li('Percent population under age 65 without health insurance.'),
            br(
              'We compute quintile cut-offs for each indicator. County placement in a higher quintile indicates lower relative accessibility.
              We assign descriptive labels for county relative accessibility based on whether they placed in 4th or 5th quintile a certain number of times.
              The more times a county places in the 4th or 5th quintile on relevant indicators, the lower its relative telemental health accessibility:'
            ),
            tags$li('Very high: 0 indicators.'),
            tags$li('High: 1 indicator.'),
            tags$li('Medium: 2 or 3 indicators.'),
            tags$li('Low: 4 indicators.'),
            tags$li('Very low: all 5 indicators'),
            p(),
            em('Data source.'),
            p(
              a(
                href = 'https://www.census.gov/programs-surveys/acs',
                'American Community Survey',
                target = "_blank"
              ),
              "2014/18 (5-year) estimates and",
              a(
                href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources',
                'County Health Rankings',
                target = "_blank"
              ),
              "2019."
            )
          )
        )
      )
    )
  } else {
    tagList(
      fluidRow(
        style = "margin: 6px",
        width = 12,
        column(12, align = "left",
               h3(strong(" Data Methods")))
      ),
      fluidRow(
        style = "margin: 6px",
        width = 12,
        column(
          4,
          wellPanel(
            strong('Domain 1'),
            p(),
            em('Source(s):'),
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
        column(
          4,
          wellPanel(
            strong('Domain 2'),
            p(),
            em('Source(s):'),
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
        column(
          4,
          wellPanel(
            strong('Domain 3'),
            p(),
            em('Source(s):'),
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
