## DATA IMPORT ------------------------------------------------------------------------------------------
capitals_data <- read_csv("data/Capitals.csv") # data from ISU indicators teams
# data_med <- read_rds("~/Git/gates/rivanna_data/working/dashboard/data_med.Rds") # I do not know where to get this data
data_work <- read_rds("data/data_work.Rds") # data from beta gates app
data_edu <- read_rds("data/data_edu.Rds") # data from beta gates app

## MODIFY CAPITALS DATA ---------------------------------------------------------------------------------
# find empty rows and remove them
keep_rows <- names(capitals_data)[stringr::str_detect(names(capitals_data), "_ITEMS", negate = TRUE)]
capitals_data <- capitals_data %>% select(keep_rows) %>% janitor::clean_names()

# read in labels for drop down menus
capitals_label_data <- read_csv("data/Capitals-labels.csv", col_types = cols(Notes = col_skip())) %>% 
  janitor::clean_names()

# clean labels so they match column names of data
capitals_label_data <- capitals_label_data %>% 
  mutate(individual_items_var_name = tolower(individual_items_var_name),
         individual_items_var_name = stringr::str_remove(individual_items_var_name, "-")
  ) %>% 
  rename(domain = assets_var_name, 
         sub_domain = name_for_drop_down_menu,
         variable_names = individual_items_var_name)

# fix duplicate
capitals_label_data$sub_domain[26] <- "Poor Mental Health Days"


## CREATE BUILT LABELS ----------------------------------------------------------------------------------
labels_built <- data_frame(
  capital = "Built",
  domain = c(rep("Remote Education", 4), rep("Remote Work", 5), rep("Telemental Health", 6)),
  sub_domain = c("Remote Education Index", "Households without internet", "Youth without computer", "Population in K-12", 
                 "Remote Work Index", "Households without broadband", "Workers without computer", "Non-remote workers in remote unfriendly occupations", "Non-remote workers in remote unfriendly industries",
                 "Telemental Health Index", "Households without internet", "Households without computer", "Population uninsured", "Poor mental health days", "Mental health providers")
)

# make values match data from the other capitals :/
labels_built$domain <- stringr::str_replace_all(labels_built$domain, " ", "_")
labels_built$sub_domain <- stringr::str_replace_all(labels_built$sub_domain, " ", "_")
labels_built$variable_names <- labels_built$sub_domain
labels_built$variable_labels <- stringr::str_replace_all(labels_built$sub_domain, "_", " ")


data_built_work <- as_data_frame(data_work) %>% 
  filter(STATEFP == "19") %>% 
  rename("fips" = GEOID,
         "Remote_Work_Index" = accessibility) %>% 
  select(fips, `Remote_Work_Index`, "Households_without_broadband" = nointernetQuint,  "Workers_without_computer" = nocomputerQuint,
         "Non-remote_workers_in_remote_unfriendly_occupations" = occupQuint, "Non-remote_workers_in_remote_unfriendly_industries" = industrQuint)

data_built_edu <- as_data_frame(data_edu) %>% 
  filter(STATEFP == "19") %>% 
  rename("fips" = GEOID,
         "remote_education_index" = accessibility) %>% 
  select(fips, ink12Quint, "Remote_Education_Index" = remote_education_index, "Households_without_internet" = nointernetQuint,
         "Youth_without_computer" = nocomputerQuint, "Population_in_K-12" = ink12Quint)

# join the two built datasets together
data_built <- data_built_work %>% 
  left_join(data_built_edu, by = "fips") %>% 
  mutate(fips = as.double(fips),
         Remote_Education_Index = 5 - as.integer(Remote_Education_Index))

## JOIN DATA SETS & ADD MAP DATA ------------------------------------------------------------------------
# join built data with other data
capitals_data <- capitals_data %>% 
  left_join(data_built, by = "fips")

capitals_label_data <- rbind(capitals_label_data, labels_built)

# add in map data
capitals_map_data <- albersusa::counties_sf()  %>% 
  select(fips, geometry) %>% 
  mutate(fips = as.double(as.character(fips))) %>% 
  right_join(capitals_data, by = c("fips"))
