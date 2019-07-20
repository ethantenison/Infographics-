#libraries
library(janitor)
library(dplyr)
library(cartogram) 
library(mapproj)

#so numbers do not show as scientific notation 
options(scipen = 999)


#map file 
mymap <- readOGR("Texas_County_Boundaries_Detailed.shp")
mymap@data <- clean_names(mymap@data)


#voting results 
voting_results <- read.csv("senate_voting_results_2018.csv")
voting_results <- clean_names(voting_results)
voting_results$i_county <- as.character(voting_results$i_county)
voting_results$voted_for <- as.character(voting_results$voted_for)

#joining map and voting results 
mymap@data <- left_join(mymap@data, voting_results, by = c("cnty_nm" = "i_county"))
mymap@data <- arrange(mymap@data, cnty_nm)


#population per county **the csv was changed to eliminate "County, Texas" from the values of every county name 
pop <- read.csv("ACS_17_5YR_DP05_with_ann.csv")
names(pop) <- as.matrix(pop[1, ])
pop <- pop[-1, ]
pop <- clean_names(pop)
pop <- select(pop, geography, estimate_sex_and_age_total_population)
pop <- rename(pop, population = estimate_sex_and_age_total_population, county = geography)
pop$county <- as.character(pop$county)
pop$population <- as.numeric(as.character(pop$population))

#big join
mymap@data <- bind_cols(mymap@data, pop)
mymap@data <- select(mymap@data, county, gid, voted_for, population)
mymap@data$voted_for[is.na(mymap@data$voted_for)] <- "Cruz"

#mapping
texas_cartogram <- cartogram(mymap, "population", itermax=7)


texas_cartogram_df <- tidy(texas_cartogram) %>% left_join(. , texas_cartogram@data, by=c("id"="gid")) 
texas_df <- tidy(mymap) %>% left_join(. , mymap@data, by=c("id"="gid")) 

# And using the advices of chart #331 we can custom it to get a better result:
ggplot() +
  geom_polygon(data = texas_df, aes(fill = voted_for, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  labs( title = "2018 Texas Senate Election", subtitle="by county" ) +
  ylim(-35,35) +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#f5f5f4", color = NA), 
    panel.background = element_rect(fill = "#f5f5f4", color = NA), 
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.2, 0.26)
  ) +
  coord_map()

# You can do the same for afr_cartogram_df

ggplot() +
  geom_polygon(data = texas_cartogram_df, aes(fill = voted_for, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  labs( title = "2018 Texas Senate Election", subtitle="by county" ) +
  ylim(-35,35) +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#f5f5f4", color = NA), 
    panel.background = element_rect(fill = "#f5f5f4", color = NA), 
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.2, 0.26)
  ) +
  coord_map()

