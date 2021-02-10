
rcc_labels_data = read.csv("data/rcc_labels.csv")
rcc_city_data = read.csv("data/rcc_cities_csl_counts.csv")

rcc_city_data$meetings_per_10000_count = round(rcc_city_data$meetings_per_10000_count, 2)