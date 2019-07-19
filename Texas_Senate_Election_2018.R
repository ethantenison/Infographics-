#libraries
library(janitor)
library(dplyr)


#so numbers do not show as scientific notation 
options(scipen = 999)


#map file 
mymap <- st_read("Texas_County_Boundaries_Detailed.shp", stringsAsFactors = FALSE)
mymap <- clean_names(mymap)


#voting results 
voting_results <- read.csv("senate_voting_results_2018.csv")
voting_results <- clean_names(voting_results)
voting_results$i_county <- as.character(voting_results$i_county)
voting_results$voted_for <- as.character(voting_results$voted_for)

#joining map and voting results 
mymap_voting <- left_join(mymap, voting_results, by = c("cnty_nm" = "i_county"))
mymap_voting <- arrange(mymap_voting, cnty_nm)


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
mymap_voted_pop <- bind_cols(mymap_voting, pop)
mymap_voted_pop <- select(mymap_voted_pop, county, gid, voted_for, population, geometry)
mymap_voted_pop$voted_for[is.na(mymap_voted_pop$voted_for)] <- "Cruz"

#mapping
ggplot(mymap_voted_pop) + geom_sf(aes(fill= voted_for, geometry = geometry)) 