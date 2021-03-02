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
  if(selection == "Recovery Ready Community Score" || selection == "Depth Ranking"){
    fill_col = fill_col - 1
    fill_col = (fill_col * 10 * -1) + 1
  }
  else{
    fill_col = (fill_col * 10) + 1
  }
  
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
  category = stringr::str_replace_all(tolower(category), " ", "_")
  selected_var = unique(rcc_labels_data$count_name[rcc_labels_data$category == category])
  var_pretty_names = unique(rcc_labels_data$drop_down_name[rcc_labels_data$category == category])
  
  table_data = rcc_city_data %>% select(all_of(c("city", "county", "rrci_rank", selected_var)))
  
  names(table_data) = c("city", "county", "rrci_rank", var_pretty_names)
  
  max_width = ifelse(category == "community_characteristics", 85, 100)
  font_size = ifelse(category == "community_characteristics", "10px", "13px")
  city_width = ifelse(category == "community_characteristics", 85, 100)
  county_width = ifelse(category == "community_characteristics", 90, 110)
  
  reactable(
    table_data,
    pagination = FALSE,
    compact = TRUE,
    style = list(fontSize = font_size),
    columns = list(
      city = colDef(
        name = "City",
        width = city_width
      ),
      county = colDef(
        name = "County",
        width = county_width
      ),
      rrci_rank = colDef(
        name = "RRCI Rank",
        width = 70,
        style = function(value, index, name){
          green_pal = colorNumeric(colorRamp(c("#ffffff", "#71CA97")),
                                   domain = c(min(table_data[, name]), max(table_data[, name])),
                                   reverse = TRUE)
          list(background = green_pal(value))
        }
      )
    ),
    defaultColDef = colDef(
      format = colFormat(separators = TRUE),
      maxWidth = max_width,
      style = function(value, index, name) {
        if (name != "city" && name != "county" && name != "rrci") {
          green_pal = colorNumeric(colorRamp(c("#ffffff", "#71CA97")), domain = c(min(table_data[, name]), max(table_data[, name])))
          list(background = green_pal(value))
        }
        else{
          list(background = "#ffffff")
        }
      }
    )
  )
}



## MAKE SOURCE TABLE FUNCTION --------------------------------------
make_source_table = function(category) {
  category = stringr::str_replace_all(tolower(category), " ", "_")
  sources = rcc_labels_data[rcc_labels_data$category == category,]

  sources = sources %>% select(drop_down_name, data_source, collection_method, variable_description)
  #names(sources) = c("Variable", "Data Source", "Collection Method", "Description")

  reactable(
    sources,
    pagination = FALSE,
    compact = FALSE,
    columns = list(
      drop_down_name = colDef(
        name = "Variable",
        width = 150
      ),
      data_source = colDef(
        name = "Data Source",
        width = 250
      ),
      collection_method = colDef(
        name = "Collection Method",
        width = 110
      ),
      variable_description = colDef(
        name = "Description",
        width = 250
      )
    )
  )
}



## DEFINE UI & SERVER FOR SELECTIONS -------------------------------------------------------------------
variable_select_ui = function(id, category) {
  ns = NS(id)
  
  # define choices for variable selection
  variable_options = as.list(unique(rcc_labels_data$drop_down_name[rcc_labels_data$category == category]))

  # assemble UI elements
  tagList(
    h4(strong("Selector"), align = "left"),
    p(),
    selectInput(ns("which_variable"), "Resource", choices = variable_options),
    p()
  )
}

variable_select_server = function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns = session$ns
                 
                 return(
                   list(variable = reactive({
                     input$which_variable
                   })))
               })
}

## DEFINE UI & SERVER FOR MAPS -------------------------------------------------------------------------
map_ui = function(id) {
  ns = NS(id)
  if(id == "rrci-map"){
    width_amount = "1000px"
  }
  else{
    width_amount = "95%"
  }
  # assemble UI elements
  tagList(
    h4(textOutput(ns("var"))),
    #h6(textOutput(ns("lab"))),
    
    #leafletOutput(ns("map"), width = "1150px", height = "500px")
    leafletOutput(ns("map"), width = width_amount)
    # Maybe use if statement to adjust for first page
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
    h3(strong(table_title), align = "left"),
    p(
      "The table rows are initially sorted by RRCI rank. Click on a column name to sort the rows by that column's values."
    ),
    #h5(textOutput(ns("var"))),
    #h6(textOutput(ns("lab"))),
    reactableOutput(ns("table"), width = "105%")
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


## TABLE PAGE MODULE UI & SERVER -------------------------------------
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
  or the Data Sources tab to learn where we found the data."
  
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



## DEFINE UI and SERVER FOR SOURCE TABLE -------------------------------------------------------------
source_table_ui = function(id) {
  ns = NS(id)
  table_title = stringr::str_replace_all(id, "_", " ") %>% stringr::str_replace_all("-source table", " Data Sources Table") %>% tools::toTitleCase()
  
  # assemble UI elements
  tagList(
    h3(strong(table_title), align = "left"),
    #h5(textOutput(ns("var"))),
    #h6(textOutput(ns("lab"))),
    p(
      "This table gives information about each of the variables in this category. There is also a list underneath the table with links to each of the data sources"
    ),
    reactableOutput(ns("source_table"), width = "105%")
  )
}

source_table_server = function(id, category) {
  moduleServer(id,
               function(input, output, session) {
                 source_table = reactive({
                   return(make_source_table(category))
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
                 
                 output$source_table = renderReactable({
                   source_table()
                 })
                 #output$var = renderText({ var() })
                 #output$lab = renderText({ lab() })
               })
}


## SOURCE TABLE PAGE MODULE UI & SERVER -------------------------------------
source_table_module_ui = function(id, category) {
  ns = NS(id)
  tagList(
    fluidRow(
      style = "margin: 6px",
      width = 12,
      column(
        width = 12, 
        align = "left",
        source_table_ui(ns("source_table"))
      )
    ),
    fluidRow(
      style = "margin: 6px",
      width = 12,
      column(
        width = 12,
        aligh = "left",
        sources_ui(category)
      )
    )
  )
}

source_table_module_server = function(id) {
  moduleServer(id,
               function(input, output, session) {
                 source_tables = source_table_server("source_table", id)
               })
}

## MAKE MODULE FOR SOURCES LINKS LIST ------------------------------------------------------------------------
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
          h3(strong(title, " Website Links")),
          p(
            "Click on the links below to see the sources of the data."
          ),
          tags$a(href = "https://www.aa-iowa.org/meetings/", "Alcoholics Anonymous - Iowa", target = "_blank"),
          br(),
          tags$a(href = "https://www.na-iowa.org/meetings/", "Narcotics Anonymous - Iowa", target = "_blank"),
          br(),
          tags$a(href = "https://adultchildren.org/mtsearch", "Adult Children of Alcoholics", target = "_blank"),
          br(),
          tags$a(href = "https://al-anon.org/al-anon-meetings/find-an-alateen-meeting/", "Al-Anon", target = "_blank"),
          br(),
          tags$a(href = "https://draonline.qwknetllc.com/meetings_dra/usa/iowa.html", "Dual Recovery Anonymous", target = "_blank"),
          br(),
          tags$a(href = "https://www.nar-anon.org/", "Nar-Anon", target = "_blank"),
          br(),
          tags$a(href = "https://www.smartrecoverytest.org/local/full-meeting-list-download/", "SMART", target = "_blank"),
          br(),
          tags$a(href = "https://locator.crgroups.info/", "Celebrate Recovery", target = "_blank"),
          br(),
          tags$a(href = "https://www.facebook.com/crushofiowa/", "CRUSH", target = "_blank"),
          br(),
          tags$a(href = "https://refugerecovery.org/meetings?tsml-day=any&tsml-region=iowa", "Refuge Recovery", target = "_blank"),
          br(),
          tags$a(href = "https://www.pillsanonymous.org/meetings/find-a-meeting/", "Pills Anonymous", target = "_blank"),
          br(),
          tags$a(href = "https://www.census.gov/programs-surveys/acs", "American Community Survey 2014/18 (5-year) Population Estimates", target = "_blank"),
          br(),
          tags$a(href = "https://odcp.iowa.gov/rxtakebacks", "Iowa Office of Drug Control Policy", target = "_blank"),
          br(),
          tags$a(href = "https://idph.iowa.gov/Portals/1/userfiles/166/Licensure/All%20Licensed%20Substance%20Use%20Disorder%20-%20Problem%20Gambling%20Program%27s%20List.pdf", "Iowa Department of Public Health - SUD and Problem Gambling Treatment Locations"),
          br(),
          tags$a(href = "https://www.alltreatment.com/ia/accredited/", "AllTreatment.com", target = "_blank"),
          br(),
          tags$a(href = "https://www.transitionalhousing.org/state/Iowa", "TransitionalHousing.org", target = "_blank"),
          br(),
          tags$a(href = "https://www.womensoberhousing.com/state/iowa.html", "WomenSoberHousing.com", target = "_blank"),
          br(),
          tags$a(href = "https://www.addicted.org/iowa-long-term-drug-rehab.html", "Addicted.org", target = "_blank"),
          br(),
          tags$a(href = "https://www.recovery.org/browse/Iowa/", "Recovery.org", target = "_blank"),
          br(),
          tags$a(href = "https://www.drug-rehabs.org/Iowa-drug-rehab-alcohol-rehabs-program.html", "Drug-rehabs.org", target = "_blank"),
          br(),
          tags$a(href = "https://idph.iowa.gov/mat", "Iowa Department of Public Health - Medication Assisted Treatment", target = "_blank")
        )
      )
    )
    ## Health Resources
  } else if (id == "health_resources") {
    tagList(
      fluidRow(
        style = "margin: 6px",
        width = 12,
        column(
          12,
          align = "left",
          h3(strong(title, " Website Links")),
          p(
            "Click on the links below to see the sources of the data."
          ),
          tags$a(href = "https://en.wikipedia.org/wiki/List_of_hospitals_in_Iowa", "Wikipedia list of Iowa hospitals", target = "_blank"),
          br(),
          tags$a(href = "https://dhs.iowa.gov/sites/default/files/MHDDAccreditedProviders_32.pdf", "Iowa Department of Human Services", target = "_blank"),
          br(),
          tags$a(href = "https://iarhc.org/find-a-rural-health-clinic?view=map", "Iowa Association of Rural Health Clinics", target = "_blank"),
          br(),
          tags$a(href = "https://www.va.gov/directory/guide/state.asp?STATE=IA&dnum=ALL", "U.S. Department of Veterans Affairs", target = "_blank")
        )
      )
    )
    ## Other Resources
  } else if(id == "other_community_resources"){
    tagList(
      fluidRow(
        style = "margin: 6px",
        width = 12,
        column(
          12,
          align = "left",
          h3(strong(title, " Website Links")),
          p(
            "Click on the links below to see the sources of the data."
          ),
          tags$a(href = "https://hifld-geoplatform.opendata.arcgis.com/datasets/local-law-enforcement-locations", "Homeland Infrastructure Foundation-Level Data", target = "_blank"),
          br(),
          tags$a(href = "https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Iowa", "Wikipedia list of Iowa colleges and universities", target = "_blank"),
          br(),
          tags$a(href = "https://ccmis.dhs.state.ia.us/ClientPortal/ProviderLocator.aspx", "Iowa Department of Human Services - Childcare Providers", target = "_blank"),
          br(),
          tags$a(href = "https://data.iowa.gov/Physical-Geography/Iowa-Church-Buildings/juvk-dad9", "USGS Geographic Names Information System", target = "_blank"),
          br(),
          tags$a(href = "https://www.iowaworkforcedevelopment.gov/contact", "Iowa Workforce Development", target = "_blank"),
          br(),
          tags$a(href = "https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey", "Institute of Museum and Library Services, Public Libraries Survey", target = "_blank"),
          br(),
          tags$a(href = "https://www.mycountyparks.com/County/Default.aspx", "MyCountyParks.com", target = "_blank")
        )
      )
    )
    ## Community Characteristics
  } else if(id == "community_characteristics"){
    tagList(
      fluidRow(
        style = "margin: 6px",
        width = 12,
        column(
          12,
          align = "left",
          h3(strong(title, " Website Links")),
          p(
            "Click on the link below to see the sources of the data. Please inquire if you would like specific ACS table numbers."
          ),
          tags$a(href = "https://www.census.gov/programs-surveys/acs", "American Community Survey 2015/19 (5-year) Population Estimates", target = "_blank"),
        )
      )
    )
  }
}


## MAKE MODULE FOR WHAT IS RECOVERY? --------------------------------------------------------------------
what_is_recovery_ui = function(id) {
  ns = NS(id)
  
  tagList(
    fluidRow(
      style = "margin: 6px",
      width = 12,
      column(
        width = 10,
        offset = 1,
        h2(strong("What is Recovery?")),
        p(
          "Though substance use recovery is an evolving concept that has been defined in a number of ways over the years, 
          an emerging consensus is that recovery is a voluntary path toward improved personal wellbeing coupled with a diminished 
          risk of substance use relapse (see Appendix Table C1 for list of recovery definitions). As noted by Bill White (2007):"
        ),
        tags$blockquote(style = "margin-left: 40px; margin-right: 40px",
          "Recovery is the experience through which individuals, families, and communities impacted by severe alcohol and 
          other drug (AOD) problems utilize internal and external resources to voluntarily resolve these problems, 
          heal the wounds inflicted by AOD related problems, actively manage their continued vulnerability to such problems, 
          and develop a healthy, productive, and meaningful life.",
          cite = "Bill White (2007)"
        ),
        p(
          "In White's view, resources help individuals, families, and communities in two ways. First, they help solve alcohol and 
          other drug-related problems (for example, addiction, unemployment, housing instability, family separation) and second, 
          they support health, productivity, and meaning in post treatment life. The appeal of this view is that it is explicit and 
          direct in its recognition that recovery involves not only internal resources such as mental, emotional, and genetic factors, 
          but also external ones. In thinking about where to focus federal and state resources to develop a network of community-based 
          recovery centers in Iowa, a key question is, \"What kinds of external resources matter most for sustained SUD recovery?\""
        ),
        p(
          "If we knew which resources were most helpful to long-run recovery, we could target towns and cities with large stocks of 
          'recovery resources' to grow Recovery Community Centers. To answer this question, we reviewed the scientific literature 
          on substance use recovery, giving special attention to theories of SUD recovery and frameworks that involved external resources. 
          We also reviewed focus group data collected by Iowa's HIPWUD Board to understand how Iowans who use drugs 
          define recovery (HIPWUD, 2020). More broadly, we looked for models of recovery that included the community in which recovery 
          happens and the wider ecological factors, such as access to nature and connection to faith communities, 
          that constitute external recovery resources."
        ),
        p(
          "The road to long-term recovery is challenging and oftentimes characterized by sequences of relapse, treatment, incarceration, 
          and short-term abstinence, each which can be more extreme and harmful for people with few resources 
          (Laudet and White, 2008; Kelly et al., 2020). The scientific literature indicates that the majority of treatment services 
          rely on an acute care model of interventions, which results in a \"revolving door effect\" characterized by multiple acute care episodes 
          (Grove-Paul et al., 20, p.6). Conversely, the disease management paradigm is viewed as more a holistic approach to recovery that 
          not only addresses problem substance use behaviors, but also the myriad of other needs of an individual in recovery. 
          Common needs of people with SUD include vocational training, employment services, housing assistance, pro-social support and 
          connections to the local community, building (or rebuilding) family and friendship networks, and perhaps most importantly, 
          a sense of purpose and meaning in life that agrees with the values, beliefs and motivations of pro-recovery behaviors 
          of people who use drugs. In our review of the recovery literature, we identified the Recovery Oriented Systems of Care (ROSC) 
          theoretical approach as an especially promising framework to guide recovery efforts in Iowa, owing to its holistic visioning of 
          SUD recovery options and its flexibility in supporting a variety of demographic, cultural, and socioeconomic subgroups who 
          constitute the recovery communities of Iowa."
        ),
        p(
          "ROSC shifts the recovery process in the direction of a collaboration between public health workers, clinical care providers, 
          the local community, and the recovery population by linking them within a system of care and support. 
          The innovation of the ROSC model is its recognition that service delivery should incorporate clinical treatment into a 
          long-term recovery capital framework to address the proximate problem of substance use addiction and the myriad of other 
          needs of an individual in recovery. Recovery capital refer to substance use recovery that is self-motivated, durable, 
          and calibrated to the resources in the community and available to a person in recovery. 
          Growing an individual's recovery capital is viewed as critical for sustained recovery. 
          ROSC encourages an individualized and self-directed approach to recovery that builds on the strengths and 
          resilience of individuals, families, and communities to chart a course toward sustainable recovery from substance use related problems. 
          ROSC empowers individuals by providing them with the information, tools, resources, life-skills, and supports they need 
          for long-term recovery (McKay, 2016)."
        ),
        p(
          "The ROSC framework leverages the notion of 'recovery ready', with the assumption that communities should utilize 
          evidence-based prevention strategies to engage in early intervention and education of individuals and communities 
          about the dangers of substance use. This framework encourages communities to provide opportunities for individuals in, 
          or seeking, recovery to find housing, education, and employment, as well as access to the kinds of supportive environments 
          that facilitate long-term recovery and a higher state of individual wellbeing (Ashford et al., 2019). Two related theories, 
          the Recovery Ready Ecological Model (Ashford et al. 2019; Haberle et al. 2014; Matto 2004; Best et al. 2016), and 
          the Recovery Capital framework (Cloud and Granfield 2008; Cano et al. 2017; Laudet and White 2008; Groshkova et al. 2012; 
          Sanchez et al. 2019), point to the same general approach to sustainable, community-based recovery. Namely, leverage both the 
          formal and informal systems of care that exist within a community to systematically reintegrate people in recovery into social 
          and civic life (e.g. employment, family, church, volunteerism, active living). It is informal resources that represent the 
          bridge from short-term, resource intensive clinical care to sustainable, long-run recovery."
        ),
        p(
          "ROSC also suggests that a comprehensive continuum of care model, rather than just an acute model of care, 
          should be deployed if we want to increase the opportunity for successful recovery and decrease the economic and 
          health burden to families and the state. Because most individuals will engage in the process of recovery within 
          the communities where they live, the long-term support for individuals in recovery might be most beneficial when 
          presented within the local community. In this way, the ROSC model satisfies the requirement for providing the 
          long-term support to those in recovery by leveraging the community resources and systems of care that already 
          exist within the community (Ashford et al., 2019)."
        ),
        p(
          "Drawing on an analogy that aligns with Iowa's history of agriculture, communities are the soil in which substance 
          use and related health and social problems grow or fail to grow, and in which the mitigation of substance use problems 
          thrive or fail. Understanding which communities have the right soil composition to facilitate organic (readily available 
          and not contingent on resources external to the community) and sustainable growth is critical to statewide SUD recovery efforts."
        )
      )
    )
  )
}
  
## MAKE MODULE FOR IDENTIFYING RECOVERY RESOURCES --------------------------------------------------------------------
identifying_recovery_resources_ui = function(id) {
  ns = NS(id)
  
  tagList(
    fluidRow(
      style = "margin: 6px",
      width = 12,
      column(
        width = 10,
        offset = 1,
        h2(strong("Identifying Recovery Resources in Iowa")),
        p(
          "Based on our review of the literature, we identified a number of clinical and non-clinical community-based resources that 
          have been shown to positively impact SUD recovery. There is wide agreement, for example, that being close to a hospital or 
          health clinic is good for SUD recovery. In particular, medication assisted treatment (e.g. suboxone, methadone) is perhaps 
          the most critical clinical support necessary to prevent relapse among those with opioid use disorder (OUD). Ready access to 
          treatment services, including both inpatient and outpatient services, regular participation in a peer support group such as 
          Alcoholics Anonymous, Narcotics Anonymous, or Self-Management and Recovery Training also enhances recovery prospects. 
          An emerging consensus among recovery science scholars is that recovery coaches and peer mentors, especially those with lived 
          SUD experience, offer people in recovery invaluable emotional, social, and cultural support that lengthens recovery duration 
          and empowers individuals to build the resilience needed to sustain their recovery. More broadly, the literature suggests that 
          generalized, formal care provided by doctors, clinicians, and other certified health professionals is invaluable to SUD recovery. 
          Equally important are the formal and informal systems of care that are specific to people with particular substance use histories, 
          and the many other behavioral health risks arising from harmful chemical use."
        ),
        p(
          "Our review of the literature, especially the emerging consensus around the Recovery Capital framework and Recovery Ready 
          Ecological Model, suggest that non-clinical community resources are also important. For example, workforce development and 
          continuous training for service providers plays a critical role in helping those with a substance use disorder get back on 
          their feet and move toward financial independence. Churches offer a ready-made community that, under the right conditions, 
          provide people who use drugs a welcoming and supportive environment for recovery. Importantly, churches and other voluntarist, 
          service-oriented non-profits offer those in recovery a sense of purpose and meaningful life that is absent from much of the 
          clinical care toolkit, and yet essential to sustainable recovery. Communities that offer their residents easy and plentiful 
          access to nature (e.g. parks, walkable communities) and affordable access to cultural content such as books, movies, lectures 
          (e.g. libraries), and community activities such as farmers markets, festivals, and parades provide people in recovery with 
          multiple pathways to rejoin the community as active and welcomed members. Notably, each of these resources is either very low 
          cost or free, making them especially appealing resources because of their ability to be accessed by all members of the community. 
          Communities that offer many of these kinds of resources to their residents are well equipped to build the positive, recovery-oriented 
          culture critical to Recover Community Center success."
        ),
        p(
          "We organized two data discovery workshops with substance use experts in Iowa, including a workshop with recovery programming 
          leaders at the Bureau of Substance Abuse, and another with members of the Linkage to Care Advisory Board, a diverse group of 
          Iowans' with expertise and interest in minimizing the harms of SUD on Iowa's families and communities. These workshops allowed 
          our team to discuss SUD recovery, present the ROSC model, and solicit feedback concerning the kinds of community infrastructure 
          that should be considered when thinking about where to locate Recovery Community Centers in Iowa."
        ),
        p(
          "We also interviewed directors of Recovery Community Centers throughout the country to learn how best to establish RCCs in Iowa 
          (Dorius, Dorius, Talbert, Van Selous, Jahic, Bahe, & Young, 2020). Throughout these interviews, we watched for stories and 
          suggestions that would preference some types of resources over others and for particular places that might be particularly 
          well suited for RCC development."
        ),
        p(
          "Drawing on feedback received from workshop participants, national leaders, the academic literature, and our own expertise 
          and insights, our research team developed a list of 17 community resources that are widely understood to support and enhance 
          SUD recovery (see Table 2). We then located, acquired, and cleaned this extensive list of community-based recovery resources 
          in Iowa. See Appendix for data collection details. Our team's efforts produced the names, addresses, and geolocation information 
          for nearly 16,000 recovery resources throughout the state. We mapped these resources in several different ways. In the first step, 
          we summed all of the recovery resources in each city and county in Iowa (e.g. all hospitals, plus all treatment centers, plus all 
          colleges in each place), which we visualized in the data visualization program Tableau (version 2020.2.1). Figure 3 identifies 
          counties and cities with especially large numbers of total recovery resources. Each county is shaded by their overall number of 
          resources (the darker the shade the more resources), and circles represent the population size of communities within counties 
          (the bigger the circle the more people). According to our data, Polk County has at least 1,784 existing recovery resources, with 
          Des Moines laying claim to 1,034 of these resources. Woodbury County has at least 564 recovery resources, of which 405, or roughly 
          4 out of every 5 resources, is located in Sioux City. Audubon County has at least 59 total resources and Adams County has at least 
          39 total resources. This first step of analysis helped identify potential locations for Recovery Community Center engagement efforts, 
          but lacked a more nuanced approach to evaluating communities based on the type and quality of their total resources."
        )
      )
    )
  )
}

## MAKE MODULE FOR KEY TERMS TAB --------------------------------------------------------------------
key_terms_ui = function(id) {
  ns = NS(id)
  
  tagList(
    fluidRow(
      style = "margin: 6px",
      width = 12,
      column(
        width = 10,
        offset = 1,
        h2(strong("Key Terms")),
        p(
          strong("Acute care model"),
          " approaches substance use as a pathological, acute illness. It is characterized by crisis interventions, clinical assessments, 
          admission to treatments geared toward stabilization, with a focus on symptom suppression, short-term service-oriented relationships. 
          Acute care typically ends at discharge from treatment (termination of the service relationship) with the expectation of permanent 
          resolution of alcohol or other substance problems."
        ),
        p(
          strong("Comprehensive continuum of care model"),
          " model is a holistic approach to recovery that views long-term recovery as an often-circular process characterized by 
          sequences of relapse, treatment, incarceration, and short-term remission. This cyclic life trajectory is especially 
          prevalent among vulnerable populations (e.g. fewer resources)."
        ),
        p(
          strong("Opioid use disorder"),
          " pathological cycles of destructive opioid use characterized by loss of control of opioid use, risky opioid use, 
          impaired social functioning, tolerance, and withdrawal symptoms from opioids."
        ),
        p(
          strong("Recovery Capital framework"),
          " rests on the assumption that the recovery from substance use is more than the absence of substance use in an 
          otherwise unchanged life. This framework proposes that sustained recovery and prevention of relapse can be 
          fortified by mobilizing social, personal, environmental and cultural resources. The goal is physical, mental 
          and social wellbeing, enhanced quality of life, and meaningful life goals. Social position and the socio-economic 
          context of substance use effect the acquisition and accumulation of recovery resources (capital)."
        ),
        p(
          strong("Recovery Community Centers"),
          " are community-oriented, local organizations developed around the concept of social capital incubators. 
          The center links members of the recovery community to different support services and recovery resources near them. 
          Peer mentors facilitate the accrual of recovery capital by linking members to, for example, recovery coaching, 
          medication assisted treatment, employment or education linkages. Located in the heart of the community, Recovery 
          Community Centers often support mobilization efforts, peer support meetings, service and community outreach activities, 
          and destigmatize campaigns."
        ),
        p(
          strong("Recovery Oriented Systems of Care"),
          " is a strength-based framework that builds on strengths and resilience of individuals, families, and communities to 
          promote sustainable wellbeing, good health, and recovery from substance use problems. Recovery Oriented Systems of Care 
          (ROSC) support continuity of service and care by linking formal systems of care to existing community resources and 
          informal systems of care. ROSC emphasize sustained recovery management, a coordinated multi-system approach, with 
          flexibility to meet diverse and unique needs of individuals in, or seeking, recovery. ROSC assumes recovery is a 
          process along a continuum that requires the ongoing monitoring of individuals in or seeking recovery, involvement of 
          peers and allies for support, individualized and comprehensive services, continuity of care that is aligned with personal 
          belief systems, and commitment to peer-delivered recovery support services."
        ),
        p(
          strong("Recovery Ready Community Index"),
          " is a tool that assesses community recovery readiness by measuring the breadth and depth of existing community recovery 
          resources. Resources include formal and informal resources and clinical and non-clinical systems of care service providers, 
          such as hospitals, treatment centers, mutual aid and support groups, recovery coaches, churches, and parks. Assessment of 
          assets and the channeling of existing assets to support individuals in, or seeking, recovery can substantially enhance 
          communities’ efforts to effectively respond to substance use."
        ),
        p(
          strong("Recovery Ready Ecological Model"),
          " helps assess and identify elements found to be supportive of recovery as well as elements that might act as barriers 
          to successful recovery. This model proposes that communities and professional sectors collaborate to provide a holistic 
          infrastructure promotes sustained recovery."
        ),
        p(
          strong("Substance Use Disorder"),
          " is here defined as any use of alcohol or drugs that is compulsive and/or dangerous. It is characterized by impaired 
          social or physical control, risky use, sustained and heavy substance use despite experiencing the harmful consequences 
          of heavy use, and pharmacological criteria. Other symptoms include escalating use due to chemical tolerance and cravings 
          to use drugs despite negative consequences."
        ),
        p(
          strong("Sustained Recovery"),
          " is dynamic, intentional process of self-directed change through which individuals utilize internal and external resources 
          to voluntarily resolve these problems, heal the wounds inflicted by alcohol or other substance-related problems, actively 
          manage their vulnerability to such problems, strive to develop and maintain a healthy, productive, and meaningful life."
        )
      )
    )
  )
}


## MAKE RRCI MODULE -------------------------------------------------------------
rrci_ui = function(id){
  ns = NS(id)
  
  tagList(
    fluidRow(
      style = "margin-left: 6px; margin-right: 6px",
      width = 12,
      column(
        width = 10,
        offset = 1,
        h2(strong("Recovery Ready Communities")),
        br()
      )
    ),
    fluidRow(
      style = "margin-left: auto; margin-right: auto",
      width = 12,
      column(
        width = 10, 
        offset = 1,
        category_module_ui("rrci", "rrci")
      )
    ),
    fluidRow(
      style = "margin-left: 6px; margin-right: 6px",
      width = 12,
      column(
        width = 10,
        offset = 1,
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
            Iowa's 944 cities and towns. These efforts culminated in the development of a novel Recovery Ready Community Index (RRCI), 
            a way to measure the breadth and depth of local recovery infrastructure and the size and strength of the 
            local substance use recovery culture. (For a visual overview of the results by the four recovery domains, see Figures 4 and 5). 
            We then analyzed index results to identify the thirty highest value, 'Recovery Ready' communities in Iowa based on this index. 
            These results are visualized in Figure 1. The size of the circle for each community reflects the community's overall population."
        ),
        p(
          "Iowa's Recovery Ready Communities are located in every region of the state and include a diversity of cities from major 
          metropolitan areas, micropolitan areas, and communities of less than 10,000 residents. Each town had at least nine of the 17 
          types of recovery infrastructure assessed, which can be leveraged to enhance the chances of sustainable recovery. We recommend 
          that future work to develop Recovery Community Centers in Iowa target the communities identified in Figure 1 for detailed community 
          profiling, outreach, and engagement. We believe each of these communities can benefit from, and also be a benefit to, a 
          Recovery Community Center and its members. You can learn more about the RRCI in the detailed report titled \"The Recovery 
          Ready Community Index: A Public Health Assessment Tool\"."
        ),
        p(
          "We then classified the recovery resources into four sub-indexes (breadth, depth, size, and strength) that make up 
          the basis for our multi-dimensional Recovery Ready Community Index (RRCI)."
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
            with a person's own, distinctive biography. Places with a wealth of recovery support resources are better able to meet 
            the diverse and unique needs of their recovery population. Put differently, RCCs in resource rich communities can better 
            serve their members by providing access to a wider variety of resources. This ensures that each RCC member has the 
            particular resources they need, when they need them, as they progress through their personal recovery journey (See Figure 4, panel a)."
        ),
        h4("Depth of Recovery Resources"),
        p(
          "Depth of recovery resources was measured by first counting the total number of resources in each category and then ranking 
          cities accordingly. This produced 17 rankings-one for each resource-ranging from 1 (the top ranked city for that particular 
          resource) to n (the lowest ranked city for that particular resource, which conceptually can be as high as the number of cities 
          in Iowa, but in practice is usually lower because of ties and missing data).  We then averaged the 17 individual rankings to 
          create an overall ranking for each city or county. Cities and counties with a large number of resources in each category 
          received a low average ranking (e.g., if you were a top-ten ranked city on each of the indicators, your average score would 
          be 10 or less). Cities with fewer resources, especially those who faced resource scarcity over multiple categories, received 
          a higher average ranking (e.g., if a community was ranked 50th or higher on every category, their average score would be at 
          least 50).  Des Moines had the most favorable ranking on this index with a score of 6, meaning that Des Moines' average ranking 
          across all 17 resources was six, followed by Ames (16) and Dubuque (29)."
        ),
        p(
          "The goal of the measure was to identify communities with diverse stocks of each resource to help facilitate long-run, 
          sustainable recovery. When individuals need medical support, for example, communities with a larger number of clinics 
          and hospitals may be able to provide more rapid, customizable, and culturally-appropriate care. When communities have 
          a variety of options for each resource category, people can engage resources that make sense for their personal recovery 
          journey, such as those that match their transportation options (e.g., one is easier to reach by bus or by foot), those 
          are conducive to work or childcare schedules (e.g., open early, late, or on weekends), and those that align better with 
          their personal needs and wants (e.g., faith-based versus secular; peer support for alcohol versus opioids, et cetera)."
        ),
        h4("Size of Recovery Culture"),
        p(
          "Size of local recovery culture was estimated as the total number of weekly substance use disorder recovery meetings per week 
          in each city/county. Places with many weekly meetings were inferred to be places with a large recovery culture. 
          According to our data, Des Moines hosts about 213 weekly meetings, followed by Sioux City with 140 weekly meetings, 
          and Cedar Rapids with 109 weekly meetings, ranking these towns in the top three, respectively, for size of recovery culture."
        ),
        p(
          "One of the most valuable things we learned from our interviews with recovery community leaders in other states was the 
          importance of the local recovery culture. Places where people with SUD were welcomed to participate in civic life, 
          where SUD stigma was challenged, and where the recovery population had a habit of coming together to share their experiences, 
          support each other, and collaborate on finding resources to rebuild lives often badly damaged by SUD, were described as being 
          well-positioned for Recovery Community Centers. To identify which types of peer support meetings we should assess, we relied 
          on our literature review of support meetings types, scoured recovery-oriented message boards and online discussion forums that 
          discussed relevant terms and websites, and conducted an extensive internet search to identify a wide range of SUD oriented peer 
          support groups in Iowa. We then used web scraping techniques to pull information from all identified support group meeting 
          websites and create a dataset of key features including times, dates, and locations of every mutual aid and peer support meeting in Iowa."
        ),
        p(
          strong("Data Limitation:"),
          " Assessing the size of recovery culture produces two important limitations. 
          First, our assessment of culture does not tell us anything specific about the size of the recovery population accessing 
          the resources. For example, public records provide details on the number of meetings available but they do not indicate 
          the size of the meeting groups. It is possible that some places have many of meetings, but only a small number of participants. 
          Thus, we cannot directly infer size of recovery population but we can draw inferences about the intensity (engagement) of 
          the recovery community. Second, when we measure of size of local recovery culture this way, it advantages larger towns and 
          cities because places with a larger number of residents should, all else equal, have a larger number of weekly peer support 
          meetings and places with fewer residents should have, by the same logic, a smaller number of weekly meetings. This measure 
          does not identify the relative strength of these resources, such as communities that have more meetings than expected, given 
          the size of town. We address this limitation with the measure of strength."
        ),
        h4("Strength (Vibrancy) of Recovery Culture"),
        p(
          "The strength of the recovery community is measured as the difference between the observed number of weekly meetings and 
          the number of meetings expected, based upon the total population of each town. This number was expressed as a percentage 
          difference between observed and predicted number of weekly peer support meetings. According to this measure, the town of 
          Harlan has 515 percent more weekly meetings than expected. Sioux City had 175 percent more weekly meetings than expected, 
          and Des Moines had 6 percent fewer weekly meetings than expected, given the size of its population. By combining our 
          measures of size and strength of local recovery culture, we are able to treat small and large towns with greater equity, 
          keeping with IDPH's mission to deliver health services equitably to the people and communities of Iowa."
        ),
        h4("Recovery Ready Community Index"),
        p(
          "Once we had constructed each of the four, recovery ready sub-indexes, we created a summary measure, which we refer to as 
          the Recovery Ready Community Index, or RRCI. This index is the simple average of each town's ranking across the four sub-indexes, 
          including breadth and depth of local recovery resources and size and strength of local recovery culture. 
          A town that scored highly on all four indexes received a high RRCI score, while a place that scored low on many or all four 
          indexes received a low RRCI score. We report results of this work in Table 3, which lists the top 30 'recovery-ready' 
          communities in Iowa and details their scores on each of the sub-indexes, their RRCI score, and their total population size. 
          Sioux City, Mason City, Fort Dodge, Dubuque, and Ames fill out the top five recovery ready communities in Iowa, according to 
          our measure. We also created maps of each of the four subcomponents of the RRCI to allow for visualization of how each of the 
          30 target communities differ in the breadth and depth of their recovery resources/infrastructure and also in the size and 
          strength score on the index. Algona, for example, is a ranked 27th on the RRCI, but because it only has 9 of 17 possible 
          recovery resources, it received a lower breadth of recovery score (and a smaller circle on Figure 5 panel a). 
          Algona has a large number of weekly peer support meetings, relative to its population size, which is why the circle for 
          Algona is quite large in Figure 5 panel b."
        ),
        p(
          "A strength of this index is that it captures a number of important and theoretically sound dimensions of substance 
          use recovery in a relatively direct way with a single number and it does so in a way that does not disadvantage small towns. 
          In fact, by our measure, 13 of the top 30 recovery ready communities in Iowa have populations of less than 15,000 residents. 
          Based on our analysis, we suggest that IDPH could target any or all of these towns for RCC engagement. More broadly, we 
          believe that the recovery population living in these places represent a valuable, and perhaps untapped resource in the community. 
          By organizing the SUD recovery populations in these town around an RCC, host communities would have a simply way to coordinate 
          and collaborate with the recovery population on substance use prevention, treatment, and recovery initiative."
        ),
        h3("In Support of Health Equity"),
        p(
          "We strove to create a recovery ready community measure that did not unduly preference large cities over smaller ones. 
          The IDPH principle of health equity is a driving force behind efforts to limit place-based bias from community decision 
          criteria whenever possible. With health equity in mind, we population-weighted the strength scores to reflect the average 
          number of recovery meetings per person, per community, so they are comparable across towns and cities of all sizes, thus 
          reducing the likelihood that we will undercount the value of small towns as potential hosts for Recovery Community Centers."
        ),
        p(
          em(
            "Equity in Action: Prior to adjustment, only 7 of the 30 communities identified as 'Recovery Ready' 
            had populations of less than 15,000. After adjustment, 13 of the 30 communities selected were small towns."
          )
        )
      )
    )
  )
}
## MAKE HOME MODULE--------------------------------------------------------------------
home_ui = function(id){
  ns = NS(id)
  
  tagList(
    fluidRow(
      style = "margin-left: auto; margin-right: auto",
      width = 12,
      column(
        width = 10,
        offset = 1,
        h2(strong(em("Is Your Community Recovery Ready?"))),
        h5("Every community in Iowa has the potential to promote recovery and resilience among its citizens. We have partnered with the 
           Iowa Department of Public Health to better understand which Iowa communities are poised to join with national partners in building a 
           Recovery Community Center (RCC) network to encourage, empower, and support people in recovery."),
      )
    ),
    fluidRow(
      style = "margin-left: auto; margin-right: auto",
      width = 12,
      column(
        width = 6,
        offset = 1,
        h2(strong(em("Who are we?"))),
        h5(
          "The Public Science Collaborative is a university-based partnership of social and data scientists, clinicians, 
           computer scientists, and outreach specialists. We work together to understand and address the grand challenges 
           of our day by helping motivate data insights into action."
        )
      ),
      column(
        width = 5,
        img(src = "psc_logo.png", class = "topimage", width = "425", height = "150", style = "display: block; margin-left: auto; margin-right: auto;")
      )
    ),
    fluidRow(
      style = "margin-left: auto; margin-right: auto",
      width = 12,
      column(
        width = 10,
        offset = 1,
        h5("The content of this dashboard was created by PSC members",
           em("Shawn Dorius, Cassandra Dorius, Elizabeth Talbert, Kelsey Van Selous, Ilma Jahic, Masoud Nosrati, Darien Bahe, Emma Young, and Matthew Voss.")),
        h5("Dashboard and videos created by ", em("Matthew Voss", tags$a(href = "https://www.linkedin.com/in/voss-matthew/", ".", target = "_blank")))
      )
    ),
    fluidRow(
      style = "margin-left: auto; margin-right: auto",
      width = 12,
      column(
        width = 6,
        offset = 1,
        h2(strong(em("Our trusted partners:"))),
        h5(
          "This work was made possible by the help and support of our state collaborators, Monica Wilke-Brown and Kevin Gabbert 
          (Iowa Department of Public Health Bureau of Substance Abuse), who were instrumental in the design and implementation of this project."
        )
      ),
      column(
        width = 5,
        img(src = "idph_logo.png", class = "topimage", width = "395", height = "150", style = "display: block; margin-left: auto; margin-right: auto;")
      )
    ),
    fluidRow(
      style = "margin-left: auto; margin-right: auto",
      width = 12,
      column(
        width = 10,
        offset = 1,
        h5(
          "We also appreciate and acknowledge the support of the following groups and individuals:"
        ),
        h5(
          em(
            "Linkage to Care Advisory Board: "
          ),
          "Todd Lange, Steve Arndt, Susie Sher, Kathy Tryen, Tammy Noble, Mindi Tennapel, Jennifer Husmann, Sarah Fineran, 
          Katrina Carter, Julie Baker, Jennifer Nu, Jay Blakley, John Hallman, Daniel L Lewis, Christopher Vitek, 
          Gagandeep Kaur Lamba, Monica Wilke-Brown, Sarah Vannice, Toby Yak, Jen Pearson, Catherine Lillehoj, Elizabeth Mcchesney, 
          Patrick C McGovern, Liz Sweet, Moran Cein"
        ),
        h5(
          em(
            "Data Science for the Public Good Student & Faculty Support: "
          ),
          "Shawn Dorius, Cassandra Dorius, Heike Hofmann, Atefeh Rajabalizadeh, Kishor Sridhar, Jessie Bustin, Matthew Voss, 
          Joel Von Behren, Andrew Maloney, Grant Durbahn, Vikram Magal, Kelsey Van Selous, & Masoud Nosrati"
        )
      )
    ),
    fluidRow(
      style = "margin-left: auto; margin-right: auto",
      width = 12,
      column(
        width = 10,
        offset = 1,
        h2(strong(em("Learn more about our study from these reports:"))), 
        tags$a(
          href = "rcc_executive_report.pdf", 
          "CLICK HERE TO READ THE EXECUTIVE REPORT", 
          target = "_blank",
          style = "color: #BF0101"
        ),
        br(),
        tags$a(
          href = "rrci_report.pdf", 
          "CLICK HERE TO LEARN ABOUT IOWA'S RECOVERY INDEX", 
          target = "_blank",
          style = "color: #BF0101"
        ),
        br(),
        tags$a(
          href = "rcc_qualitative_report.pdf", 
          "CLICK HERE TO SEE ADVICE FROM NATIONAL LEADERS ABOUT BUILDING AN RCC IN IOWA", 
          target = "_blank",
          style = "color: #BF0101"
        ),
        br(),
        h2(strong(em("Watch these videos to learn about using the dashboard:")))
      ),
    ),
    fluidRow(
      style = "margin-left: auto; margin-right:auto",
      width = 12,
      column(
        width = 2,
        offset = 3,
        h4(strong("Dashboard Overview")),
        tags$video(src = "dashboard_overview.mp4", type = "video/mp4", controls = TRUE, width = "110%")
      ),
      column(
        width = 2,
        h4(strong("Maps")),
        tags$video(src = "dashboard_maps.mp4", type = "video/mp4", controls = TRUE, width = "110%")
      ),
      column(
        width = 2,
        h4(strong("Tables and Sources")),
        tags$video(src = "dashboard_tables.mp4", type = "video/mp4", controls = TRUE, width = "110%")
      )
    ),
    fluidRow(
      style = "margin-left: auto; margin-right: auto",
      width = 12,
      column(
        width = 10,
        offset = 1,
        h2(strong(em("Funding:"))),
        h5(
          "This dashboard and related publications were made possible by funding from the Iowa Department of Public Health 
          Substance Use Bureau, Centers for Disease Control, and Substance Use and Mental Health Services Administration. 
          Dorius, S., Dorius, C., & Talbert, E. Advancing Substance Use Recovery in Iowa. 1/10/2020-9/29/2020. (Subaward $260,000). 
          Its contents are solely the responsibility of the authors and do not necessarily represent the official views of the funding 
          agencies or sponsors."
        )
      )
    ),
    fluidRow(
      style = "margin-left: auto; margin-right: auto",
      width = 12,
      column(
        width = 12,
        #offset = 5,
        align = "center",
        div(
          img(src = "iowa_state_logo.jpg", class = "topimage", width = "200", height = "195", style = "display: block; margin-left: auto; margin-right: auto;"),
          style = "text-align: center;"
        )
      )
    )
  )
}