
########################################################################################################################
# process the population data
#####################################process the population data of landscan in 2000 to 2022
######################Complete on the linux server
library(sp)
library(sf)
library(ggplot2)
library(dplyr)
library(data.table)
shapefile <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
shapefile <- as(shapefile,"Spatial")
library(raster)
pop_2000 <- raster("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/landscan-global-2022.tif")
pop_2000
xmin <- -180
xmax <- 180
ymin <- -90
ymax <- 90
res_x <- 1.25
res_y <- 1
template_raster <- raster(extent(-180, 180, -90, 90), resolution=c(1.25, 1), crs=crs(pop_2000))
resolutions <- res(pop_2000)
res_x <- resolutions[1]
res_y <- resolutions[2]
factor_x <- 150
factor_y <- 120
print(paste("Factor X:", factor_x))
print(paste("Factor Y:", factor_y))
aggregated_raster <- aggregate(pop_2000, c(factor_x, factor_y), fun=sum, expand=TRUE, na.rm=TRUE)
plot(aggregated_raster, main="resampled population")

shapefile_sf <- st_as_sf(shapefile)
raster_df <- as.data.frame(aggregated_raster, xy=TRUE)
colnames(raster_df) <- c("lon", "lat", "population")
raster_df <- na.omit(raster_df)
head(raster_df)
ggplot() +
  geom_raster(data = raster_df, aes(x = lon, y = lat, fill = population), interpolate = TRUE) +
  geom_sf(data = shapefile_sf, fill = NA, color = "black") +
  scale_fill_viridis_c(option = "D") +
  labs(title = "World Population Distribution with Country Borders") +
  theme_minimal()

unique(raster_df$lon)
unique(raster_df$lat)
head(raster_df)
total <- sum(raster_df$population)
total
write.csv(raster_df,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2022.csv",row.names = FALSE)

library(data.table)
base_path <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/history/"
years <- 2000:2022
file_names <- paste0(base_path, "pop_", years, ".csv")
pop <- data.frame(lon = numeric(0), 
                  lat = numeric(0),
                  population = numeric(0),
                  year = numeric(0))
for (file_name in file_names){
  pop_y <- fread(file_name)
  pop_y$year <- as.numeric(gsub(".*pop_(\\d+).csv", "\\1", file_name))
  pop <- rbind(pop,pop_y)
}
write.csv(pop,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/history/pop_2000_2022.csv",row.names = FALSE)

# calculate the grid population in 1980 to 1999 with the proportion of population in 2000
#############The 1980-1999 population was allocated in proportion to the 2000 population
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/history/pop_2000_2022.csv")
unique(pop$lon)
unique(pop$lat)
# Adjust the lat column to the nearest 0.5 degree
pop$lat <- round(pop$lat * 2) / 2
# Round the fourth decimal place
pop$lon <- round(pop$lon, 4)
pop <- pop[pop$year==2000,]
library(dplyr)
# Calculate the total population for each year
pop_total_per_year <- pop %>%
  group_by(year) %>%
  summarise(total_population = sum(population, na.rm = TRUE))
pop_total_per_year
# Calculate the ratio of each lat and lon combination
pop_proportion <- pop %>%
  left_join(pop_total_per_year, by = "year") %>%
  mutate(proportion = population / total_population)
library(sf)
library(raster)
library(dplyr)
library(data.table)
library(ggplot2)
library(viridis)
library(Polychrome)
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)

coordinates(pop_proportion) <- ~lon+lat
pop_sf <- st_as_sf(pop_proportion, coords = c("lon", "lat"), crs = st_crs(world_new))
crs_radiation_sf <- st_crs(pop_sf)
crs_world_new <- st_crs(world_new)
# Set CRS for radiation_sf. Here, WGS 84 (EPSG:4326) is used as an example
st_crs(pop_sf) <- 4326
# convert to same
pop_sf_transformed <- st_transform(pop_sf, st_crs(world_new))
# Fixed the geometry of world_new
world_new <- st_make_valid(world_new)
# Spatial matching: Determine which country/region each radiation cell belongs to
matches <- st_intersects(pop_sf_transformed, world_new)
# Extract latitude and longitude information from the geometry column
coords <- st_coordinates(pop_sf)
pop_sf$lon <- coords[, "X"]
pop_sf$lat <- coords[, "Y"]
pop_proportion_by_country <- data.frame(country = character(0), 
                                        proportion = numeric(0), 
                                        lon = numeric(0), 
                                        lat = numeric(0))
for (i in seq_along(matches)) {
  if (length(matches[[i]]) > 0) {
    for (j in matches[[i]]) {
      pop_proportion_by_country <- rbind(pop_proportion_by_country, 
                                         data.frame(country = world_new$NAME_LONG[j], 
                                                    pop_proportion = pop_sf$proportion[i],
                                                    lon = pop_sf$lon[i], 
                                                    lat = pop_sf$lat[i]))
    }
  }
}
quantile(pop_proportion_by_country$pop_proportion)
write.csv(pop_proportion_by_country,file ="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/ratio/pop_proportion_2000.csv",row.names = FALSE)

#distribute the population of World Population Perspective to grid
library(data.table)
library(dplyr)
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/WPP2022_PopulationBySingleAgeSex_Medium_1950-2021.csv")
pop_country<- pop[pop$LocTypeName=="Country/Area",]
head(pop_country)
country_names_map <- unique(world_new$NAME_LONG)
country_names_map <- sort(country_names_map)
unique_pop <- setdiff(pop_country$Location, country_names_map)
unique_country_map <- setdiff(country_names_map, pop_country$Location)
pop_country$Location[pop_country$Location == "Dem. People's Republic of Korea"] <- "Dem. Rep. Korea"
pop_country$Location[pop_country$Location == "Viet Nam"] <- "Vietnam"
pop_country$Location[pop_country$Location == "Micronesia (Fed. States of)"] <- "Micronesia"
pop_country$Location[pop_country$Location == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
pop_country$Location[pop_country$Location == "Solomon Islands"] <- "Solomon Is."
pop_country$Location[pop_country$Location == "Lao People's Democratic Republic"] <- "Laos"
pop_country$Location[pop_country$Location == "Marshall Islands"] <- "Marshall Is."
pop_country$Location[pop_country$Location == "Brunei Darussalam"] <- "Brunei"
pop_country$Location[pop_country$Location == "Republic of Moldova"] <- "Moldova"
pop_country$Location[pop_country$Location == "Russian Federation"] <- "Russia"
pop_country$Location[pop_country$Location == "Antigua and Barbuda"] <- "Antigua and Barb."
pop_country$Location[pop_country$Location == "Dominican Republic"] <- "Dominican Rep."
pop_country$Location[pop_country$Location == "United States of America"] <- "United States"
pop_country$Location[pop_country$Location == "Bolivia (Plurinational State of)"] <- "Bolivia"
pop_country$Location[pop_country$Location == "Saint Vincent and the Grenadines"] <- "St. Vin. and Gren."
pop_country$Location[pop_country$Location == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
pop_country$Location[pop_country$Location == "Syrian Arab Republic"] <- "Syria"
pop_country$Location[pop_country$Location == "State of Palestine"] <- "Palestine"
pop_country$Location[pop_country$Location == "Congo"] <- "Republic of the Congo"
pop_country$Location[pop_country$Location == "Iran (Islamic Republic of)"] <- "Iran"
pop_country$Location[pop_country$Location == "Türkiye"] <- "Turkey"
pop_country$Location[pop_country$Location == "Equatorial Guinea"] <- "Eq. Guinea"
pop_country$Location[pop_country$Location == "Central African Republic"] <- "Central African Rep."
pop_country$Location[pop_country$Location == "United Republic of Tanzania"] <- "Tanzania"
pop_country$Location[pop_country$Location == "Eswatini"] <- "eSwatini"
pop_country$Location[pop_country$Location == "Cook Islands"] <- "Cook Is."
pop_country$Location[pop_country$Location == "United States Virgin Islands"] <- "U.S. Virgin Is."
pop_country$Location[pop_country$Location == "South Sudan"] <- "S. Sudan"
pop_country$Location[pop_country$Location == "Northern Mariana Islands"] <- "N. Mariana Is."
pop_country$Location[pop_country$Location == "Faroe Islands"] <- "Faeroe Islands"
pop_country$Location[pop_country$Location == "British Virgin Islands"] <- "British Virgin Is."
pop_country$Location[pop_country$Location == "Cayman Islands"] <- "Cayman Is."
pop_country$Location[pop_country$Location == "Saint Barthélemy"] <- "St-Barthélemy"
pop_country$Location[pop_country$Location == "Saint Martin (French part)"] <- "Saint-Martin"
pop_country$Location[pop_country$Location == "Sint Maarten (Dutch part)"] <- "Sint Maarten"
pop_country$Location[pop_country$Location == "Turks and Caicos Islands"] <- "Turks and Caicos Is."
pop_country$Location[pop_country$Location == "Falkland Islands (Malvinas)"] <- "Falkland Is."
pop_country$Location[pop_country$Location == "Saint Pierre and Miquelon"] <- "St. Pierre and Miquelon"
pop_country$Location[pop_country$Location == "French Polynesia"] <- "Fr. Polynesia"
pop_country$Location[pop_country$Location == "Western Sahara"] <- "W. Sahara"

country_names_map <- unique(world_new$NAME_LONG)
country_names_map <- sort(country_names_map)
unique_pop <- setdiff(pop_country$Location, country_names_map)
unique_country_map <- setdiff(country_names_map, pop_country$Location)

m_total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/gbd_1980_2021.csv")
unique_pop <- setdiff(pop_country$Location, m_total$location_name)
unique_m <- setdiff(m_total$location_name, pop_country$Location)
pop_country <- pop_country[,c("Location","Time","AgeGrp","PopMale","PopFemale","PopTotal")]
unique(pop_country$Location)

pop_country_sum <- pop_country %>%
  filter(Location %in% c("China", "China, Hong Kong SAR", "China, Macao SAR", "China, Taiwan Province of China")) %>%
  group_by(Time, AgeGrp) %>%
  summarise(
    PopMale = sum(PopMale),
    PopFemale = sum(PopFemale),
    PopTotal = sum(PopTotal)
  ) %>%
  ungroup()
pop_country_sum <- data.frame(pop_country_sum)
pop_country_sum$Location <- "China"
pop_country <- pop_country %>%
  filter(Location != "China" & Location != "China, Hong Kong SAR"& Location != "China, Macao SAR"& Location != "China, Taiwan Province of China")
pop_country <- rbind(pop_country,pop_country_sum)

pop_proportion <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/ratio/pop_proportion_2000.csv")
pop_proportion
unique(pop_proportion$country)
unique_pop <- setdiff(pop_country$Location, pop_proportion$country)
unique_proportion <- setdiff(pop_proportion$country, pop_country$Location)

###########when generate population per age, don't run this code
pop_country <- pop_country %>%
  mutate(AgeGrp = case_when(
    AgeGrp %in% c("1", "2", "3", "4") ~ "1-4 years",
    AgeGrp %in% c("5", "6", "7", "8", "9") ~ "5-9 years",
    AgeGrp %in% c("10", "11", "12", "13", "14") ~ "10-14 years",
    AgeGrp %in% c("15", "16", "17", "18", "19") ~ "15-19 years",
    AgeGrp %in% c("20", "21", "22", "23", "24") ~ "20-24 years",
    AgeGrp %in% c("25", "26", "27", "28", "29") ~ "25-29 years",
    AgeGrp %in% c("30", "31", "32", "33", "34") ~ "30-34 years",
    AgeGrp %in% c("35", "36", "37", "38", "39") ~ "35-39 years",
    AgeGrp %in% c("40", "41", "42", "43", "44") ~ "40-44 years",
    AgeGrp %in% c("45", "46", "47", "48", "49") ~ "45-49 years",
    AgeGrp %in% c("50", "51", "52", "53", "54") ~ "50-54 years",
    AgeGrp %in% c("55", "56", "57", "58", "59") ~ "55-59 years",
    AgeGrp %in% c("60", "61", "62", "63", "64") ~ "60-64 years",
    AgeGrp %in% c("65", "66", "67", "68", "69") ~ "65-69 years",
    AgeGrp %in% c("70", "71", "72", "73", "74") ~ "70-74 years",
    AgeGrp %in% c("75", "76", "77", "78", "79") ~ "75-79 years",
    AgeGrp %in% c("80", "81", "82", "83", "84") ~ "80-84 years",
    AgeGrp %in% c("85", "86", "87", "88", "89") ~ "85-89 years",
    AgeGrp %in% c("90", "91", "92", "93", "94") ~ "90-94 years",
    AgeGrp %in% c("95", "96", "97", "98", "99", "100+") ~ "95+ years",
  )) %>%
  # Group the data by Location, Time, and the new AgeGrp column, and then sum PopMale, PopFemale, and PopTotal
  group_by(Location, Time, AgeGrp) %>%
  summarise(
    PopMale = sum(PopMale),
    PopFemale = sum(PopFemale),
    PopTotal = sum(PopTotal)
  ) %>%
  ungroup()
pop_country <- data.frame(pop_country)
pop_country <- na.omit(pop_country)
colnames(pop_country)[which(names(pop_country) == "Time")] <- "year"
colnames(pop_country)[which(names(pop_country) == "Location")] <- "country"
pop_country <- pop_country[pop_country$year>=1980&pop_country$year<=1999,]
head(pop_country)
head(pop_proportion)
unique(pop_country$year)

total <- merge(pop_country,pop_proportion,by="country")
total
# Calculate the total female population for each year
female_pop_total_per_year <- pop_country %>%
  group_by(year) %>%
  summarise(total_female = sum(PopFemale, na.rm = TRUE))
female_pop_total_per_year
female_pop_total_per_year <- data.frame(female_pop_total_per_year)
# Calculate the total male population for each year
male_pop_total_per_year <- pop_country %>%
  group_by(year) %>%
  summarise(total_male = sum(PopMale, na.rm = TRUE))
male_pop_total_per_year
male_pop_total_per_year <- data.frame(male_pop_total_per_year)
# Calculate the total female population of each country
female_pop_total_per_country_per_year <- pop_country %>%
  group_by(country,year) %>%
  summarise(total_female_country = sum(PopFemale, na.rm = TRUE))
female_pop_total_per_country_per_year
female_pop_total_per_country_per_year <- data.frame(female_pop_total_per_country_per_year)
# Calculate the total male population of each country
male_pop_total_per_country_per_year <- pop_country %>%
  group_by(country,year) %>%
  summarise(total_male_country = sum(PopMale, na.rm = TRUE))
male_pop_total_per_country_per_year
male_pop_total_per_country_per_year <- data.frame(male_pop_total_per_country_per_year)
# Calculate the total female population of each age group in each country
female_pop_total_per_country_per_year_per_age <- pop_country %>%
  group_by(country,year,AgeGrp) %>%
  summarise(total_female_country_age = sum(PopFemale, na.rm = TRUE))
female_pop_total_per_country_per_year_per_age
female_pop_total_per_country_per_year_per_age <- data.frame(female_pop_total_per_country_per_year_per_age)
# Calculate the total male population of each age group in each country
male_pop_total_per_country_per_year_per_age <- pop_country %>%
  group_by(country,year,AgeGrp) %>%
  summarise(total_male_country_age = sum(PopMale, na.rm = TRUE))
male_pop_total_per_country_per_year_per_age
male_pop_total_per_country_per_year_per_age <- data.frame(male_pop_total_per_country_per_year_per_age)

total_ <- merge(total,female_pop_total_per_year,by="year")
total_ <- merge(total_,male_pop_total_per_year,by="year")
total_ <- merge(total_,female_pop_total_per_country_per_year,by=c("country","year"))
total_ <- merge(total_,male_pop_total_per_country_per_year,by=c("country","year"))
total_ <- merge(total_,female_pop_total_per_country_per_year_per_age,by=c("country","year","AgeGrp"))
total_ <- merge(total_,male_pop_total_per_country_per_year_per_age,by=c("country","year","AgeGrp"))
total_$age_proportion_female <- total_$total_female_country_age/total_$total_female_country
total_$age_proportion_male <- total_$total_male_country_age/total_$total_male_country

total_t <- total_[total_$year==1990 & total_$AgeGrp=="1-4 years",]
#total_t <- total_[total_$year==1990 & total_$AgeGrp=="0",]
sum(total_t$pop_proportion)
total_$pop_proportion_change <- total_$pop_proportion/sum(total_t$pop_proportion)

total_$popfemale_grid <- total_$total_female*1000*total_$pop_proportion_change*total_$age_proportion_female
total_$popmale_grid <- total_$total_male*1000*total_$pop_proportion_change*total_$age_proportion_male
total_test <- total_[total_$year==1991,]
sum(total_test$popfemale_grid)
fwrite(total_,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/grid population/pop_1980_1999_2000pro.csv",row.names = FALSE)

##########################calculate the population from 2000 to 2021
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/WPP2022_PopulationBySingleAgeSex_Medium_1950-2021.csv")
pop_country<- pop[pop$LocTypeName=="Country/Area",]
head(pop_country)
pop_country$Location[pop_country$Location == "Dem. People's Republic of Korea"] <- "Dem. Rep. Korea"
pop_country$Location[pop_country$Location == "Viet Nam"] <- "Vietnam"
pop_country$Location[pop_country$Location == "Micronesia (Fed. States of)"] <- "Micronesia"
pop_country$Location[pop_country$Location == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
pop_country$Location[pop_country$Location == "Solomon Islands"] <- "Solomon Is."
pop_country$Location[pop_country$Location == "Lao People's Democratic Republic"] <- "Laos"
pop_country$Location[pop_country$Location == "Marshall Islands"] <- "Marshall Is."
pop_country$Location[pop_country$Location == "Brunei Darussalam"] <- "Brunei"
pop_country$Location[pop_country$Location == "Republic of Moldova"] <- "Moldova"
pop_country$Location[pop_country$Location == "Russian Federation"] <- "Russia"
pop_country$Location[pop_country$Location == "Antigua and Barbuda"] <- "Antigua and Barb."
pop_country$Location[pop_country$Location == "Dominican Republic"] <- "Dominican Rep."
pop_country$Location[pop_country$Location == "United States of America"] <- "United States"
pop_country$Location[pop_country$Location == "Bolivia (Plurinational State of)"] <- "Bolivia"
pop_country$Location[pop_country$Location == "Saint Vincent and the Grenadines"] <- "St. Vin. and Gren."
pop_country$Location[pop_country$Location == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
pop_country$Location[pop_country$Location == "Syrian Arab Republic"] <- "Syria"
pop_country$Location[pop_country$Location == "State of Palestine"] <- "Palestine"
pop_country$Location[pop_country$Location == "Congo"] <- "Republic of the Congo"
pop_country$Location[pop_country$Location == "Iran (Islamic Republic of)"] <- "Iran"
pop_country$Location[pop_country$Location == "Türkiye"] <- "Turkey"
pop_country$Location[pop_country$Location == "Equatorial Guinea"] <- "Eq. Guinea"
pop_country$Location[pop_country$Location == "Central African Republic"] <- "Central African Rep."
pop_country$Location[pop_country$Location == "United Republic of Tanzania"] <- "Tanzania"
pop_country$Location[pop_country$Location == "Eswatini"] <- "eSwatini"
pop_country$Location[pop_country$Location == "Cook Islands"] <- "Cook Is."
pop_country$Location[pop_country$Location == "United States Virgin Islands"] <- "U.S. Virgin Is."
pop_country$Location[pop_country$Location == "South Sudan"] <- "S. Sudan"
pop_country$Location[pop_country$Location == "Northern Mariana Islands"] <- "N. Mariana Is."
pop_country$Location[pop_country$Location == "Faroe Islands"] <- "Faeroe Islands"
pop_country$Location[pop_country$Location == "British Virgin Islands"] <- "British Virgin Is."
pop_country$Location[pop_country$Location == "Cayman Islands"] <- "Cayman Is."
pop_country$Location[pop_country$Location == "Saint Barthélemy"] <- "St-Barthélemy"
pop_country$Location[pop_country$Location == "Saint Martin (French part)"] <- "Saint-Martin"
pop_country$Location[pop_country$Location == "Sint Maarten (Dutch part)"] <- "Sint Maarten"
pop_country$Location[pop_country$Location == "Turks and Caicos Islands"] <- "Turks and Caicos Is."
pop_country$Location[pop_country$Location == "Falkland Islands (Malvinas)"] <- "Falkland Is."
pop_country$Location[pop_country$Location == "Saint Pierre and Miquelon"] <- "St. Pierre and Miquelon"
pop_country$Location[pop_country$Location == "French Polynesia"] <- "Fr. Polynesia"
pop_country$Location[pop_country$Location == "Western Sahara"] <- "W. Sahara"

pop_country <- pop_country[,c("Location","Time","AgeGrp","PopMale","PopFemale","PopTotal")]
pop_country_sum <- pop_country %>%
  filter(Location %in% c("China", "China, Hong Kong SAR", "China, Macao SAR", "China, Taiwan Province of China")) %>% # 筛选出目标地区的行
  group_by(Time, AgeGrp) %>%
  summarise(
    PopMale = sum(PopMale),
    PopFemale = sum(PopFemale),
    PopTotal = sum(PopTotal)
  ) %>%
  ungroup()
pop_country_sum <- data.frame(pop_country_sum)
pop_country_sum$Location <- "China"
pop_country <- pop_country %>%
  filter(Location != "China" & Location != "China, Hong Kong SAR"& Location != "China, Macao SAR"& Location != "China, Taiwan Province of China")
pop_country <- rbind(pop_country,pop_country_sum)

###############The 1.25°*1° gridded population for 2000-2022 is obtained, now calculate the proportion of population in each grid every year from 2000-2022
library(dplyr)
library(data.table)
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/history/pop_2000_2022.csv")
pop_total_per_year <- pop %>%
  group_by(year) %>%
  summarise(total_population = sum(population, na.rm = TRUE))
pop_total_per_year
pop_proportion <- pop %>%
  left_join(pop_total_per_year, by = "year") %>%
  mutate(proportion = population / total_population)
average_proportion <- pop_proportion %>%
  group_by(lat, lon) %>%
  summarise(avg_proportion = mean(proportion, na.rm = TRUE))
average_proportion <- data.frame(average_proportion)
quantile(average_proportion$avg_proportion)
sum <-sum(average_proportion$avg_proportion)
print(average_proportion)
library(sf)
library(raster)
library(dplyr)
library(data.table)
library(ggplot2)
library(viridis)
library(Polychrome)
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)

coordinates(average_proportion) <- ~lon+lat
pop_sf <- st_as_sf(average_proportion, coords = c("lon", "lat"), crs = st_crs(world_new))
crs_radiation_sf <- st_crs(pop_sf)
crs_world_new <- st_crs(world_new)
st_crs(pop_sf) <- 4326
pop_sf_transformed <- st_transform(pop_sf, st_crs(world_new))
world_new <- st_make_valid(world_new)
matches <- st_intersects(pop_sf_transformed, world_new)
coords <- st_coordinates(pop_sf)
pop_sf$lon <- coords[, "X"]
pop_sf$lat <- coords[, "Y"]
pop_proportion_by_country <- data.frame(country = character(0), 
                                        avg_proportion = numeric(0), 
                                        lon = numeric(0), 
                                        lat = numeric(0))
for (i in seq_along(matches)) {
  if (length(matches[[i]]) > 0) {
    for (j in matches[[i]]) {
      pop_proportion_by_country <- rbind(pop_proportion_by_country, 
                                         data.frame(country = world_new$NAME_LONG[j], 
                                                    pop_proportion = pop_sf$avg_proportion[i],
                                                    lon = pop_sf$lon[i], 
                                                    lat = pop_sf$lat[i]))
    }
  }
}
write.csv(pop_proportion_by_country,file ="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/history/pop_proportion_2000_2022.csv",row.names = FALSE)

#######################distribute the WPP data to grid population in 2000-2021
pop_proportion <-fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/history/pop_proportion_2000_2022.csv")
pop_proportion
unique_pop <- setdiff(pop_country$Location, pop_proportion$country)
unique_proportion <- setdiff(pop_proportion$country, pop_country$Location)
###########when generate population per age, don't run this code
pop_country <- pop_country %>%
  mutate(AgeGrp = case_when(
    AgeGrp %in% c("1", "2", "3", "4") ~ "1-4 years",
    AgeGrp %in% c("5", "6", "7", "8", "9") ~ "5-9 years",
    AgeGrp %in% c("10", "11", "12", "13", "14") ~ "10-14 years",
    AgeGrp %in% c("15", "16", "17", "18", "19") ~ "15-19 years",
    AgeGrp %in% c("20", "21", "22", "23", "24") ~ "20-24 years",
    AgeGrp %in% c("25", "26", "27", "28", "29") ~ "25-29 years",
    AgeGrp %in% c("30", "31", "32", "33", "34") ~ "30-34 years",
    AgeGrp %in% c("35", "36", "37", "38", "39") ~ "35-39 years",
    AgeGrp %in% c("40", "41", "42", "43", "44") ~ "40-44 years",
    AgeGrp %in% c("45", "46", "47", "48", "49") ~ "45-49 years",
    AgeGrp %in% c("50", "51", "52", "53", "54") ~ "50-54 years",
    AgeGrp %in% c("55", "56", "57", "58", "59") ~ "55-59 years",
    AgeGrp %in% c("60", "61", "62", "63", "64") ~ "60-64 years",
    AgeGrp %in% c("65", "66", "67", "68", "69") ~ "65-69 years",
    AgeGrp %in% c("70", "71", "72", "73", "74") ~ "70-74 years",
    AgeGrp %in% c("75", "76", "77", "78", "79") ~ "75-79 years",
    AgeGrp %in% c("80", "81", "82", "83", "84") ~ "80-84 years",
    AgeGrp %in% c("85", "86", "87", "88", "89") ~ "85-89 years",
    AgeGrp %in% c("90", "91", "92", "93", "94") ~ "90-94 years",
    AgeGrp %in% c("95", "96", "97", "98", "99", "100+") ~ "95+ years",
  )) %>%
  group_by(Location, Time, AgeGrp) %>%
  summarise(
    PopMale = sum(PopMale),
    PopFemale = sum(PopFemale),
    PopTotal = sum(PopTotal)
  ) %>%
  ungroup()
pop_country <- data.frame(pop_country)
colnames(pop_country)[which(names(pop_country) == "Time")] <- "year"
colnames(pop_country)[which(names(pop_country) == "Location")] <- "country"
pop_country <- pop_country[pop_country$year>=2000&pop_country$year<=2021,]
head(pop_country)
head(pop_proportion)
total <- merge(pop_country,pop_proportion,by=c("country","year"))
total
unique(total$year)
pop_country_t <- pop_country[pop_country$year>=2000&pop_country$year<=2021,]
# Calculate the total female population for each year
female_pop_total_per_year <- pop_country_t %>%
  group_by(year) %>%
  summarise(total_female = sum(PopFemale, na.rm = TRUE))
female_pop_total_per_year
female_pop_total_per_year <- data.frame(female_pop_total_per_year)
# Calculate the total male population for each year
male_pop_total_per_year <- pop_country_t %>%
  group_by(year) %>%
  summarise(total_male = sum(PopMale, na.rm = TRUE))
male_pop_total_per_year
male_pop_total_per_year <- data.frame(male_pop_total_per_year)
# Calculate the total female population of each country
female_pop_total_per_country_per_year <- pop_country_t %>%
  group_by(country,year) %>%
  summarise(total_female_country = sum(PopFemale, na.rm = TRUE))
female_pop_total_per_country_per_year
female_pop_total_per_country_per_year <- data.frame(female_pop_total_per_country_per_year)
# Calculate the total male population of each country
male_pop_total_per_country_per_year <- pop_country_t %>%
  group_by(country,year) %>%
  summarise(total_male_country = sum(PopMale, na.rm = TRUE))
male_pop_total_per_country_per_year
male_pop_total_per_country_per_year <- data.frame(male_pop_total_per_country_per_year)
# Calculate the total female population of each age group in each country
female_pop_total_per_country_per_year_per_age <- pop_country_t %>%
  group_by(country,year,AgeGrp) %>%
  summarise(total_female_country_age = sum(PopFemale, na.rm = TRUE))
female_pop_total_per_country_per_year_per_age
female_pop_total_per_country_per_year_per_age <- data.frame(female_pop_total_per_country_per_year_per_age)
# Calculate the total male population of each age group in each country
male_pop_total_per_country_per_year_per_age <- pop_country_t %>%
  group_by(country,year,AgeGrp) %>%
  summarise(total_male_country_age = sum(PopMale, na.rm = TRUE))
male_pop_total_per_country_per_year_per_age
male_pop_total_per_country_per_year_per_age <- data.frame(male_pop_total_per_country_per_year_per_age)

total_ <- merge(total,female_pop_total_per_year,by="year")
total_ <- merge(total_,male_pop_total_per_year,by="year")
total_ <- merge(total_,female_pop_total_per_country_per_year,by=c("country","year"))
total_ <- merge(total_,male_pop_total_per_country_per_year,by=c("country","year"))
total_ <- merge(total_,female_pop_total_per_country_per_year_per_age,by=c("country","year","AgeGrp"))
total_ <- merge(total_,male_pop_total_per_country_per_year_per_age,by=c("country","year","AgeGrp"))
total_$age_proportion_female <- total_$total_female_country_age/total_$total_female_country
total_$age_proportion_male <- total_$total_male_country_age/total_$total_male_country

total_t <- total_[total_$AgeGrp=="1-4 years",]
#total_t <- total_[total_$AgeGrp=="0",]
total_sum <- total_t %>%
  group_by(year) %>%
  summarise(total_pop_proportion = sum(pop_proportion))
total_sum <- data.frame(total_sum)
total_ <- merge(total_,total_sum,by="year")
total_$pop_proportion_change <- total_$pop_proportion/total_$total_pop_proportion

total_$popfemale_grid <- total_$total_female*1000*total_$pop_proportion_change*total_$age_proportion_female
total_$popmale_grid <- total_$total_male*1000*total_$pop_proportion_change*total_$age_proportion_male

fwrite(total_,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid.csv",row.names = FALSE)

###########there are 2022, 2025, 2030, 2035, 2040... 2100 of population, linear interpolation of population proportion in the middle years
library(sf)
library(raster)
library(dplyr)
library(data.table)
library(ggplot2)
library(viridis)
library(Polychrome)
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)

library(data.table)
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2022.csv")
unique(pop$lon)
unique(pop$lat)
# Adjust the lat column to the nearest 0.5 degree
pop$lat <- round(pop$lat * 2) / 2
# Round the fourth decimal place
pop$lon <- round(pop$lon, 4)
pop <- pop[pop$year==2022,]
library(dplyr)
# Calculate the total population for each year
pop_total_per_year <- pop %>%
  group_by(year) %>%
  summarise(total_population = sum(population, na.rm = TRUE))
pop_total_per_year
# Calculate the ratio of each lat and lon combination
pop_proportion <- pop %>%
  left_join(pop_total_per_year, by = "year") %>%
  mutate(proportion = population / total_population)
pop_proportion <- pop_proportion[,c("lon","lat","proportion","year")]

coordinates(pop_proportion) <- ~lon+lat
pop_sf <- st_as_sf(pop_proportion, coords = c("lon", "lat"), crs = st_crs(world_new))
crs_radiation_sf <- st_crs(pop_sf)
crs_world_new <- st_crs(world_new)
# Set CRS for radiation_sf. Here, WGS 84 (EPSG:4326) is used
st_crs(pop_sf) <- 4326
# Convert to the same coordinate system
pop_sf_transformed <- st_transform(pop_sf, st_crs(world_new))
# Fixed the geometry of world new
world_new <- st_make_valid(world_new)
#plot(world_new)
# Spatial matching: Determine which country/region each radiation cell belongs to
matches <- st_intersects(pop_sf_transformed, world_new)
# Extract latitude and longitude information from the geometry column
coords <- st_coordinates(pop_sf)
# Add the latitude and longitude information to the radiation sf data box
pop_sf$lon <- coords[, "X"]
pop_sf$lat <- coords[, "Y"]
pop_proportion_by_country <- data.frame(country = character(0), 
                                        proportion = numeric(0), 
                                        lon = numeric(0), 
                                        lat = numeric(0)
)
for (i in seq_along(matches)) {
  if (length(matches[[i]]) > 0) {
    for (j in matches[[i]]) {
      pop_proportion_by_country <- rbind(pop_proportion_by_country, 
                                         data.frame(country = world_new$NAME_LONG[j], 
                                                    pop_proportion = pop_sf$proportion[i],
                                                    lon = pop_sf$lon[i], 
                                                    lat = pop_sf$lat[i])
      )
    }
  }
}
pop_proportion_by_country$year <- 2022

########process the population projection data
#library(rgdal)
library(sp)
library(sf)
library(ggplot2)
library(dplyr)
library(data.table)
library(raster)
shapefile <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
SSP2_2025 <- raster("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/SSP2/SPP2/SSP2_2100.tif")
SSP2_2025
xmin <- -180
xmax <- 180
ymin <- -90
ymax <- 90
res_x <- 1.25
res_y <- 1
template_raster <- raster(extent(-180, 180, -90, 90), resolution=c(1.25, 1), crs=crs(SSP2_2025))
resolutions <- res(SSP2_2025)
res_x <- resolutions[1]
res_y <- resolutions[2]
factor_x <- ceiling(1.25 / res_x)
factor_y <- ceiling(1.0 / res_y)
print(paste("Factor X:", factor_x))
print(paste("Factor Y:", factor_y))
aggregated_raster <- aggregate(SSP2_2025, c(factor_x, factor_y), fun=sum, expand=TRUE, na.rm=TRUE)
#plot(aggregated_raster, main="重采样后的人口分布")
shapefile_sf <- st_as_sf(shapefile)
raster_df <- as.data.frame(aggregated_raster, xy=TRUE)
colnames(raster_df) <- c("lon", "lat", "population")
raster_df <- na.omit(raster_df)
head(raster_df)
unique(raster_df$lon)
unique(raster_df$lat)
raster_df$lat <- round(raster_df$lat * 2) / 2
raster_df$lon <- round(raster_df$lon, 4)
head(raster_df)
write.csv(raster_df,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/SSP2/SPP2/SSP2_2100_grid_125_1.csv",row.names = FALSE)

#Linear interpolation is performed every 5 years
years <- c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)
total_proportion <- data.frame(country=character(0),proportion=numeric(0),lon=numeric(0),lat=numeric(0),year=numeric(0))
total_proportion <- rbind(pop_proportion_by_country,total_proportion)
base_path <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/SSP2/SPP2/"
file_names <- paste0(base_path, "SSP2_", years,"_grid_125_1", ".csv")
for (file_name in file_names){
  print(file_name)
  pop <- fread(file_name)
  pop$year <- as.numeric(gsub(".*SSP2_(\\d+)_grid_125_1.csv", "\\1", file_name))
  # Calculate the total population for each year
  pop_total_per_year <- pop%>%
    group_by(year) %>%
    summarise(total_population = sum(population, na.rm = TRUE))
  pop_total_per_year
  # Calculate the ratio of each lat and lon combination
  pop_proportion <- pop %>%
    left_join(pop_total_per_year, by = "year") %>%
    mutate(proportion = population / total_population)
  pop_proportion <- pop_proportion[,c("lon","lat","proportion","year")]
  coordinates(pop_proportion) <- ~lon+lat 
  pop_sf <- st_as_sf(pop_proportion, coords = c("lon", "lat"), crs = st_crs(world_new))
  crs_radiation_sf <- st_crs(pop_sf)
  crs_world_new <- st_crs(world_new)
  st_crs(pop_sf) <- 4326
  pop_sf_transformed <- st_transform(pop_sf, st_crs(world_new))
  world_new <- st_make_valid(world_new)
  matches <- st_intersects(pop_sf_transformed, world_new)
  coords <- st_coordinates(pop_sf)
  pop_sf$lon <- coords[, "X"]
  pop_sf$lat <- coords[, "Y"]
  pop_proportion_by_country <- data.frame(country = character(0), 
                                          proportion = numeric(0), 
                                          lon = numeric(0), 
                                          lat = numeric(0)
  )
  for (i in seq_along(matches)) {
    if (length(matches[[i]]) > 0) {
      for (j in matches[[i]]) {
        pop_proportion_by_country <- rbind(pop_proportion_by_country, 
                                           data.frame(country = world_new$NAME_LONG[j], 
                                                      pop_proportion = pop_sf$proportion[i],
                                                      lon = pop_sf$lon[i], 
                                                      lat = pop_sf$lat[i])
        )
      }
    }
  }
  pop_proportion_by_country$year <- as.numeric(gsub(".*SSP2_(\\d+)_grid_125_1.csv", "\\1", file_name))
  total_proportion <- rbind(total_proportion,pop_proportion_by_country)
}
total_proportion
unique(total_proportion$year)
setDT(total_proportion)

known_years <- c(2022, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)
# Calculate the gap year, that is, the year in which interpolation is required
gaps <- c()
for (i in 1:(length(known_years) - 1)) {
  current_gap <- seq(from = known_years[i], to = known_years[i+1], by = 1)[-1]
  gaps <- c(gaps, current_gap[-length(current_gap)])  # The end of a known year is not included
}
# Interpolate for each unique country, latitude, and longitude combination
interpolated_results <- total_proportion[, {
  # First check that there are enough data points to interpolate
  if (.N > 1 && sum(!is.na(pop_proportion)) > 1) {
    # Linear interpolation is performed using the approx function
    interpolated_values <- approx(x = year, y = pop_proportion, xout = gaps, method = "linear", rule = 2)$y
    # Create a data table containing all interpolated years and corresponding interpolated results
    list(year = gaps, pop_proportion = interpolated_values)
  } else {
    # If there are less than two data points, the NA value is returned
    list(year = gaps, pop_proportion = rep(as.double(NA), length(gaps)))
  }
}, by = .(country, lat, lon)]
unique(interpolated_results$year)
interpolated_results <- na.omit(interpolated_results)
interpolated_results <- interpolated_results[,c("country","pop_proportion","lon","lat","year")]
total_proportion <- rbind(total_proportion,interpolated_results)
unique(total_proportion$year)
write.csv(total_proportion,file ="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/ratio/pop_proportion_22_100.csv",row.names = FALSE)
total_proportion <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/ratio/pop_proportion_22_100.csv")
# Calculate the sum of the scale values for each year
proportion_per_year <- total_proportion %>%
  group_by(year) %>%
  summarise(proportion_sum = sum(pop_proportion, na.rm = TRUE))
print(proportion_per_year,n=30)
tail(proportion_per_year)

#####process the population data from 2022 to 2100
library(data.table)
library(dplyr)
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/WPP2022_PopulationBySingleAgeSex_Medium_2022-2100/WPP2022_PopulationBySingleAgeSex_Medium_2022-2100.csv")
pop_country<- pop[pop$LocTypeName=="Country/Area",]
head(pop_country)
pop_country$Location[pop_country$Location == "Dem. People's Republic of Korea"] <- "Dem. Rep. Korea"
pop_country$Location[pop_country$Location == "Viet Nam"] <- "Vietnam"
pop_country$Location[pop_country$Location == "Micronesia (Fed. States of)"] <- "Micronesia"
pop_country$Location[pop_country$Location == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
pop_country$Location[pop_country$Location == "Solomon Islands"] <- "Solomon Is."
pop_country$Location[pop_country$Location == "Lao People's Democratic Republic"] <- "Laos"
pop_country$Location[pop_country$Location == "Marshall Islands"] <- "Marshall Is."
pop_country$Location[pop_country$Location == "Brunei Darussalam"] <- "Brunei"
pop_country$Location[pop_country$Location == "Republic of Moldova"] <- "Moldova"
pop_country$Location[pop_country$Location == "Russian Federation"] <- "Russia"
pop_country$Location[pop_country$Location == "Antigua and Barbuda"] <- "Antigua and Barb."
pop_country$Location[pop_country$Location == "Dominican Republic"] <- "Dominican Rep."
pop_country$Location[pop_country$Location == "United States of America"] <- "United States"
pop_country$Location[pop_country$Location == "Bolivia (Plurinational State of)"] <- "Bolivia"
pop_country$Location[pop_country$Location == "Saint Vincent and the Grenadines"] <- "St. Vin. and Gren."
pop_country$Location[pop_country$Location == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
pop_country$Location[pop_country$Location == "Syrian Arab Republic"] <- "Syria"
pop_country$Location[pop_country$Location == "State of Palestine"] <- "Palestine"
pop_country$Location[pop_country$Location == "Congo"] <- "Republic of the Congo"
pop_country$Location[pop_country$Location == "Iran (Islamic Republic of)"] <- "Iran"
pop_country$Location[pop_country$Location == "Türkiye"] <- "Turkey"
pop_country$Location[pop_country$Location == "Equatorial Guinea"] <- "Eq. Guinea"
pop_country$Location[pop_country$Location == "Central African Republic"] <- "Central African Rep."
pop_country$Location[pop_country$Location == "United Republic of Tanzania"] <- "Tanzania"
pop_country$Location[pop_country$Location == "Eswatini"] <- "eSwatini"
pop_country$Location[pop_country$Location == "Cook Islands"] <- "Cook Is."
pop_country$Location[pop_country$Location == "United States Virgin Islands"] <- "U.S. Virgin Is."
pop_country$Location[pop_country$Location == "South Sudan"] <- "S. Sudan"
pop_country$Location[pop_country$Location == "Northern Mariana Islands"] <- "N. Mariana Is."
pop_country$Location[pop_country$Location == "Faroe Islands"] <- "Faeroe Islands"
pop_country$Location[pop_country$Location == "British Virgin Islands"] <- "British Virgin Is."
pop_country$Location[pop_country$Location == "Cayman Islands"] <- "Cayman Is."
pop_country$Location[pop_country$Location == "Saint Barthélemy"] <- "St-Barthélemy"
pop_country$Location[pop_country$Location == "Saint Martin (French part)"] <- "Saint-Martin"
pop_country$Location[pop_country$Location == "Sint Maarten (Dutch part)"] <- "Sint Maarten"
pop_country$Location[pop_country$Location == "Turks and Caicos Islands"] <- "Turks and Caicos Is."
pop_country$Location[pop_country$Location == "Falkland Islands (Malvinas)"] <- "Falkland Is."
pop_country$Location[pop_country$Location == "Saint Pierre and Miquelon"] <- "St. Pierre and Miquelon"
pop_country$Location[pop_country$Location == "French Polynesia"] <- "Fr. Polynesia"
pop_country$Location[pop_country$Location == "Western Sahara"] <- "W. Sahara"
country_names_map <- unique(world_new$NAME_LONG)
country_names_map <- sort(country_names_map)
unique_pop <- setdiff(pop_country$Location, country_names_map)
unique_country_map <- setdiff(country_names_map, pop_country$Location)
pop_country <- pop_country[,c("Location","Time","AgeGrp","PopMale","PopFemale","PopTotal")]
pop_country_sum <- pop_country %>%
  filter(Location %in% c("China", "China, Hong Kong SAR", "China, Macao SAR", "China, Taiwan Province of China")) %>% 
  group_by(Time, AgeGrp) %>% 
  summarise(
    PopMale = sum(PopMale),
    PopFemale = sum(PopFemale),
    PopTotal = sum(PopTotal)
  ) %>%
  ungroup()
pop_country_sum <- data.frame(pop_country_sum)
pop_country_sum$Location <- "China"
pop_country <- pop_country %>%
  filter(Location != "China" & Location != "China, Hong Kong SAR"& Location != "China, Macao SAR"& Location != "China, Taiwan Province of China")
pop_country <- rbind(pop_country,pop_country_sum)

# creat a new col of AgeGrp
pop_country <- pop_country %>%
  mutate(AgeGrp = case_when(
    AgeGrp %in% c("1", "2", "3", "4") ~ "1-4 years",
    AgeGrp %in% c("5", "6", "7", "8", "9") ~ "5-9 years",
    AgeGrp %in% c("10", "11", "12", "13", "14") ~ "10-14 years",
    AgeGrp %in% c("15", "16", "17", "18", "19") ~ "15-19 years",
    AgeGrp %in% c("20", "21", "22", "23", "24") ~ "20-24 years",
    AgeGrp %in% c("25", "26", "27", "28", "29") ~ "25-29 years",
    AgeGrp %in% c("30", "31", "32", "33", "34") ~ "30-34 years",
    AgeGrp %in% c("35", "36", "37", "38", "39") ~ "35-39 years",
    AgeGrp %in% c("40", "41", "42", "43", "44") ~ "40-44 years",
    AgeGrp %in% c("45", "46", "47", "48", "49") ~ "45-49 years",
    AgeGrp %in% c("50", "51", "52", "53", "54") ~ "50-54 years",
    AgeGrp %in% c("55", "56", "57", "58", "59") ~ "55-59 years",
    AgeGrp %in% c("60", "61", "62", "63", "64") ~ "60-64 years",
    AgeGrp %in% c("65", "66", "67", "68", "69") ~ "65-69 years",
    AgeGrp %in% c("70", "71", "72", "73", "74") ~ "70-74 years",
    AgeGrp %in% c("75", "76", "77", "78", "79") ~ "75-79 years",
    AgeGrp %in% c("80", "81", "82", "83", "84") ~ "80-84 years",
    AgeGrp %in% c("85", "86", "87", "88", "89") ~ "85-89 years",
    AgeGrp %in% c("90", "91", "92", "93", "94") ~ "90-94 years",
    AgeGrp %in% c("95", "96", "97", "98", "99", "100+") ~ "95+ years",
  )) %>%
  group_by(Location, Time, AgeGrp) %>%
  summarise(
    PopMale = sum(PopMale),
    PopFemale = sum(PopFemale),
    PopTotal = sum(PopTotal)
  ) %>%
  ungroup()
pop_country <- data.frame(pop_country)
colnames(pop_country)[which(names(pop_country) == "Time")] <- "year"
colnames(pop_country)[which(names(pop_country) == "Location")] <- "country"

pop_proportion <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/ratio/pop_proportion_22_100.csv")
unique(pop_country$year)
head(pop_proportion)
head(pop_country)
pop_country <- pop_country[,c("country","year","PopMale","PopFemale","AgeGrp")]
total <- merge(pop_country,pop_proportion,by=c("country","year"))
#total[total$country=="Afghanistan"&total$year==1990&total$AgeGrp=="0-4 years",]
total
unique(total$year)
pop_country_t <- pop_country[pop_country$year>=2022&pop_country$year<=2100,]
# Calculate the total female population for each year
female_pop_total_per_year <- pop_country_t %>%
  group_by(year) %>%
  summarise(total_female = sum(PopFemale, na.rm = TRUE))
female_pop_total_per_year
female_pop_total_per_year <- data.frame(female_pop_total_per_year)
# Calculate the total male population for each year
male_pop_total_per_year <- pop_country_t %>%
  group_by(year) %>%
  summarise(total_male = sum(PopMale, na.rm = TRUE))
male_pop_total_per_year
male_pop_total_per_year <- data.frame(male_pop_total_per_year)
# Calculate the total female population of each country
female_pop_total_per_country_per_year <- pop_country_t %>%
  group_by(country,year) %>%
  summarise(total_female_country = sum(PopFemale, na.rm = TRUE))
female_pop_total_per_country_per_year
female_pop_total_per_country_per_year <- data.frame(female_pop_total_per_country_per_year)
# Calculate the total male population of each country
male_pop_total_per_country_per_year <- pop_country_t %>%
  group_by(country,year) %>%
  summarise(total_male_country = sum(PopMale, na.rm = TRUE))
male_pop_total_per_country_per_year
male_pop_total_per_country_per_year <- data.frame(male_pop_total_per_country_per_year)
# Calculate the total female population of each age group in each country
female_pop_total_per_country_per_year_per_age <- pop_country_t %>%
  group_by(country,year,AgeGrp) %>%
  summarise(total_female_country_age = sum(PopFemale, na.rm = TRUE))
female_pop_total_per_country_per_year_per_age
female_pop_total_per_country_per_year_per_age <- data.frame(female_pop_total_per_country_per_year_per_age)
# Calculate the total male population of each age group in each country
male_pop_total_per_country_per_year_per_age <- pop_country_t %>%
  group_by(country,year,AgeGrp) %>%
  summarise(total_male_country_age = sum(PopMale, na.rm = TRUE))
male_pop_total_per_country_per_year_per_age
male_pop_total_per_country_per_year_per_age <- data.frame(male_pop_total_per_country_per_year_per_age)

total_ <- merge(total,female_pop_total_per_year,by="year")
total_ <- merge(total_,male_pop_total_per_year,by="year")
total_ <- merge(total_,female_pop_total_per_country_per_year,by=c("country","year"))
total_ <- merge(total_,male_pop_total_per_country_per_year,by=c("country","year"))
total_ <- merge(total_,female_pop_total_per_country_per_year_per_age,by=c("country","year","AgeGrp"))
total_ <- merge(total_,male_pop_total_per_country_per_year_per_age,by=c("country","year","AgeGrp"))
total_$age_proportion_female <- total_$total_female_country_age/total_$total_female_country
total_$age_proportion_male <- total_$total_male_country_age/total_$total_male_country

total_t <- total_[total_$AgeGrp=="1-4 years",]
total_sum <- total_t %>%
  group_by(year) %>%
  summarise(total_pop_proportion = sum(pop_proportion))
total_sum <- data.frame(total_sum)
total_ <- merge(total_,total_sum,by="year")
total_$pop_proportion_change <- total_$pop_proportion/total_$total_pop_proportion

total_$popfemale_grid <- total_$total_female*1000*total_$pop_proportion_change*total_$age_proportion_female
total_$popmale_grid <- total_$total_male*1000*total_$pop_proportion_change*total_$age_proportion_male

fwrite(total_,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2022_2100_grid.csv",row.names = FALSE)

