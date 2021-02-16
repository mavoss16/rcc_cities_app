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
  
  ## Recovery Resources
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
      ## First row of boxes (2 boxes)
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
        ## Second row of boxes (2 boxes)
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
          ## 3rd row of boxes (3 boxes)
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
                   )
            )
          )
        )
      )
    )
    ## Primary Care Resources
  } else if (id == "primary_care_resources") {
    tagList(
      fluidRow(
        style = "margin: 6px",
        width = 12,
        column(12, align = "left",
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
            strong('Hospitals'),
            p(),
            em('Source(s):'),
            br(),
            tags$a(href = "https://en.wikipedia.org/wiki/List_of_hospitals_in_Iowa", "Wikipedia list of Iowa hospitals"),
            br(),
            p(),
            em("Methods:"),
            br(),
            p("Counted the number of hospitals by city.")
          )
        ),
        column(
          6,
          wellPanel(
            strong('Mental Health Centers'),
            p(),
            em('Source(s):'),
            br(),
            tags$a(href = "https://dhs.iowa.gov/sites/default/files/MHDDAccreditedProviders_32.pdf", "Iowa Department of Human Services"),
            br(),
            p(),
            em("Methods:"),
            br(),
            p("Counted the number of mental health centers by city.")
          )
        ),
        ## Second row of boxes (2 boxes)
        fluidRow(
          style = "margin: 6px",
          width = 12,
          column(6,
                 wellPanel(
                   strong('Rural Clinics'),
                   p(),
                   em('Source(s):'),
                   br(),
                   tags$a(href = "https://iarhc.org/find-a-rural-health-clinic?view=map", "Iowa Association of Rural Health Clinics"),
                   br(),
                   p(),
                   em("Methods:"),
                   br(),
                   p("Counted the number of rural clinics by city.")
                 )),
          column(6,
                 wellPanel(width = "500px",
                   strong('Veterans Affairs Health Centers'),
                   p(),
                   em('Source(s):'),
                   br(),
                   tags$a(href = "https://www.va.gov/directory/guide/state.asp?STATE=IA&dnum=ALL", "U.S. Department of Veterans Affairs"),
                 )
          )
        )
      )
    )
    ## Social and Economic Resources
  } else if(id == "social_and_economic_resources"){
    tagList(
      fluidRow(
        style = "margin: 6px",
        width = 12,
        column(12, align = "left",
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
          4,
          wellPanel(
            strong('Law Enforcement Offices'),
            p(),
            em('Source(s):'),
            br(),
            tags$a(href = "https://hifld-geoplatform.opendata.arcgis.com/datasets/local-law-enforcement-locations", "Homeland Infrastructure Foundation-Level Data")
          )
        ),
        column(
          4,
          wellPanel(
            strong('Colleges'),
            p(),
            em('Source(s):'),
            br(),
            tags$a(href = "https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Iowa", "Wikipedia list of Iowa colleges and universities")
          )
        ),
        column(
          4,
          wellPanel(
            strong('Childcare Providers'),
            p(),
            em('Source(s):'),
            br(),
            tags$a(href = "http://ccmis.dhs.state.ia.us/ClientPortal/ProviderLocator.aspx", "Iowa Department of Human Services")
          )
        )
      ),
      fluidRow(
        style = "margin: 6px",
        width = 12,
        column(
          3,
          wellPanel(
            strong('Places of Worship'),
            p(),
            em('Source(s):'),
            br(),
            tags$a(href = "https://data.iowa.gov/Physical-Geography/Iowa-Church-Buildings/juvk-dad9", "USGS Geographic Names Information System")
          )
        ),
        column(
          3,
          wellPanel(
            strong('Workforce Development Offices'),
            p(),
            em('Source(s):'),
            br(),
            tags$a(href = "https://www.iowaworkforcedevelopment.gov/contact", "Iowa Workforce Development")
          )
        ),
        column(
          3,
          wellPanel(
            strong('Libraries'),
            p(),
            em('Source(s):'),
            br(),
            tags$a(href = "https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey", 
                   "Institute of Museum and Library Services, Public Libraries Survey")
          )
        ),
        column(
          3,
          wellPanel(
            strong('Parks'),
            p(),
            em('Source(s):'),
            br(),
            tags$a(href = "https://www.mycountyparks.com/County/Default.aspx", "MyCountyParks.com")
          )
        )
      )
    )
    ## Demographic Characteristics
  } else if(id == "demographic_characteristics"){
    tagList(
      fluidRow(
        style = "margin: 6px",
        width = 12,
        column(12, align = "left",
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
          4,
          wellPanel(
            strong('Domain 1'),
            p(),
            em('Source(s):'),
            br(),
          )
        ),
        column(
          4,
          wellPanel(
            strong('Domain 2'),
            p(),
            em('Source(s):'),
            br(),
          )
        ),
        column(
          4,
          wellPanel(
            strong('Domain 3'),
            p(),
            em('Source(s):'),
            br(),
          )
        )
      )
    )
    ## Community Characteristics
  } else if(id == "community_characteristics"){
    tagList(
      fluidRow(
        style = "margin: 6px",
        width = 12,
        column(12, align = "left",
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
          4,
          wellPanel(
            strong('Domain 1'),
            p(),
            em('Source(s):'),
            br(),
          )
        ),
        column(
          4,
          wellPanel(
            strong('Domain 2'),
            p(),
            em('Source(s):'),
            br(),
          )
        ),
        column(
          4,
          wellPanel(
            strong('Domain 3'),
            p(),
            em('Source(s):'),
            br(),
          )
        )
      )
    )
  }
}
