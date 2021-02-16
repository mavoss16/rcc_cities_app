
rcc_labels_data = read.csv("data/rcc_labels.csv")
rcc_city_data = read.csv("data/rcc_cities_csl_counts.csv")

rcc_city_data$meetings_per_10000_count = round(rcc_city_data$meetings_per_10000_count, 2)
rcc_city_data$avg_commute = round(rcc_city_data$avg_commute, 2)

pct_fix = function(x){
  y = x * 100
  y = round(y, digits = 2)
  return(y)
}

rcc_city_data$pct_under18 = rcc_city_data$pct_under18 %>% pct_fix()
rcc_city_data$pct_over65 = rcc_city_data$pct_over65 %>% pct_fix()
rcc_city_data$pct_white = rcc_city_data$pct_white %>% pct_fix()
rcc_city_data$pct_nonwhite = rcc_city_data$pct_nonwhite %>% pct_fix()
rcc_city_data$pct_poverty = rcc_city_data$pct_poverty %>% pct_fix()
rcc_city_data$pct_hs = rcc_city_data$pct_hs %>% pct_fix()
rcc_city_data$pct_bach = rcc_city_data$pct_bach %>% pct_fix()
rcc_city_data$pct_grad = rcc_city_data$pct_grad %>% pct_fix()
rcc_city_data$pct_snap = rcc_city_data$pct_snap %>% pct_fix()
rcc_city_data$pct_commute_over30 = rcc_city_data$pct_commute_over30 %>% pct_fix()
rcc_city_data$pct_internet = rcc_city_data$pct_internet %>% pct_fix()



# rcc_labels_data = rcc_labels_data %>% mutate_if(starts_with("pct_"), ~pct_fix())
