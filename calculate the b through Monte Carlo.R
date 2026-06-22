########################################process the gbd database of skin cancer
library(data.table)
m_1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/GBD 2021/IHME-GBD_2021_DATA-205ec450-1.csv")
m_2 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/GBD 2021/IHME-GBD_2021_DATA-205ec450-2.csv")
m_3 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/GBD 2021/IHME-GBD_2021_DATA-205ec450-3.csv")
m_total <- rbind(m_1,m_2,m_3)
unique(m_total$year)
m_total <- m_total[,c("measure_name","location_name","sex_name","age_name","cause_name","year","val","upper","lower")]
library(dplyr)
unique(m_total$location_name)
# filter location_name is "China" and "Taiwan (Province of China)"  and sum the data
m_total_sum <- m_total %>%
  filter(location_name %in% c("China", "Taiwan (Province of China)")) %>% 
  group_by(measure_name,sex_name, age_name, cause_name, year) %>% 
  summarise(
    val = sum(val),   
    upper = sum(upper), 
    lower = sum(lower)  
  ) %>%
  ungroup()
m_total_sum <- data.frame(m_total_sum)
m_total_sum$location_name <- "China"
m_total <- m_total %>%
  filter(location_name != "China" & location_name != "Taiwan (Province of China)")
m_total <- rbind(m_total,m_total_sum)

library(colorspace)
library(RColorBrewer)
library(sf)
library(data.table)
library(dplyr)
library(ggplot2)
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)
plot(world_new)
country_names_map <- unique(world_new$NAME_LONG)
country_names_map <- sort(country_names_map)

# unify the location name
unique_m <- setdiff(m_total$location_name, country_names_map)
unique_country_map <- setdiff(country_names_map, m_total$location_name)
m_total$location_name[m_total$location_name == "Saint Vincent and the Grenadines"] <- "St. Vin. and Gren."
m_total$location_name[m_total$location_name == "Viet Nam"] <- "Vietnam"
m_total$location_name[m_total$location_name == "South Sudan"] <- "S. Sudan"
m_total$location_name[m_total$location_name == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
m_total$location_name[m_total$location_name == "United States of America"] <- "United States"
m_total$location_name[m_total$location_name == "Central African Republic"] <- "Central African Rep."
m_total$location_name[m_total$location_name == "Lao People's Democratic Republic"] <- "Laos"
m_total$location_name[m_total$location_name == "Democratic People's Republic of Korea"] <- "Dem. Rep. Korea"
m_total$location_name[m_total$location_name == "Congo"] <- "Republic of the Congo"
m_total$location_name[m_total$location_name == "Syrian Arab Republic"] <- "Syria"
m_total$location_name[m_total$location_name == "Dominican Republic"] <- "Dominican Rep."
m_total$location_name[m_total$location_name == "Bolivia (Plurinational State of)"] <- "Bolivia"
m_total$location_name[m_total$location_name == "Marshall Islands"] <- "Marshall Is."
m_total$location_name[m_total$location_name == "Russian Federation"] <- "Russia"
m_total$location_name[m_total$location_name == "Micronesia (Federated States of)"] <- "Micronesia"
m_total$location_name[m_total$location_name == "Türkiye"] <- "Turkey"
m_total$location_name[m_total$location_name == "Antigua and Barbuda"] <- "Antigua and Barb."
m_total$location_name[m_total$location_name == "Equatorial Guinea"] <- "Eq. Guinea"
m_total$location_name[m_total$location_name == "United Republic of Tanzania"] <- "Tanzania"
m_total$location_name[m_total$location_name == "Republic of Moldova"] <- "Moldova"
m_total$location_name[m_total$location_name == "Eswatini"] <- "eSwatini"
m_total$location_name[m_total$location_name == "Brunei Darussalam"] <- "Brunei"
m_total$location_name[m_total$location_name == "Iran (Islamic Republic of)"] <- "Iran"
m_total$location_name[m_total$location_name == "Solomon Islands"] <- "Solomon Is."
m_total$location_name[m_total$location_name == "Northern Mariana Islands"] <- "N. Mariana Is."
m_total$location_name[m_total$location_name == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
m_total$location_name[m_total$location_name == "Cook Islands"] <- "Cook Is."
m_total$location_name[m_total$location_name == "United States Virgin Islands"] <- "U.S. Virgin Is."

unique(m_total$age_name)
unique(m_total$cause_name)
m_total$age_name[m_total$age_name == "80-84"] <- "80-84 years"
m_total$age_name[m_total$age_name == "85-89"] <- "85-89 years"
m_total$age_name[m_total$age_name == "90-94"] <- "90-94 years"
m_total$age_name[m_total$age_name == "<5 years"] <- "1-4 years"

unique(m_total$location_name)
write.csv(m_total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/gbd_1980_2021.csv",row.names = FALSE)

########################################process the gbd database of cataract
library(data.table)
library(dplyr)
file_path <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/GBD 2021/cataracts/"
m_total <- data.frame(measure_name=character(),
                       location_name=character(),
                       sex_name=character(),
                       age_name=character(),
                       cause_name=character(),
                       year=integer(),
                       val=numeric(),
                       upper=numeric(),
                       lower=numeric(),
                       stringsAsFactors = FALSE)
for (year in 1990:2021) {
  #year <- 1990
  file_name <- paste0(file_path, year, ".csv")
  data <- read.csv(file_name)
  data <- data[,c("measure_name","location_name","sex_name","age_name","cause_name","year","val","upper","lower")]
  data <- data[data$age_name!="12-23 months",]
  data <- data[data$age_name!="2-4 years",]
  m_total <- rbind(m_total,data)
}
m_total_test <- m_total[m_total$age_name=="5-9 years",]
unique(m_total_test$val)
unique(m_total$year)

sort(unique(m_total$location_name))
# filter location_name is "China" and "Taiwan (Province of China)"  and sum the data
m_total_sum <- m_total %>%
  filter(location_name %in% c("People's Republic of China", "Taiwan (Province of China)")) %>% 
  group_by(measure_name,sex_name, age_name, cause_name, year) %>% 
  summarise(
    val = sum(val),   
    upper = sum(upper), 
    lower = sum(lower)  
  ) %>%
  ungroup()
m_total_sum <- data.frame(m_total_sum)
m_total_sum$location_name <- "China"
m_total <- m_total %>%
  filter(location_name != "People's Republic of China" & location_name != "Taiwan (Province of China)")
m_total <- rbind(m_total,m_total_sum)

library(colorspace)
library(RColorBrewer)
library(sf)
library(data.table)
library(dplyr)
library(ggplot2)
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)
plot(world_new)
country_names_map <- unique(world_new$NAME_LONG)
country_names_map <- sort(country_names_map)
# unify the location name
unique_m <- setdiff(m_total$location_name, country_names_map)
unique_country_map <- setdiff(country_names_map, m_total$location_name)
m_total$location_name[m_total$location_name == "Saint Vincent and the Grenadines"] <- "St. Vin. and Gren."
m_total$location_name[m_total$location_name == "Socialist Republic of Viet Nam"] <- "Vietnam"
m_total$location_name[m_total$location_name == "United States of America"] <- "United States"
m_total$location_name[m_total$location_name == "Central African Republic"] <- "Central African Rep."
m_total$location_name[m_total$location_name == "Lao People's Democratic Republic"] <- "Laos"
m_total$location_name[m_total$location_name == "Democratic People's Republic of Korea"] <- "Dem. Rep. Korea"
m_total$location_name[m_total$location_name == "Congo"] <- "Republic of the Congo"
m_total$location_name[m_total$location_name == "Syrian Arab Republic"] <- "Syria"
m_total$location_name[m_total$location_name == "Dominican Republic"] <- "Dominican Rep."
m_total$location_name[m_total$location_name == "Russian Federation"] <- "Russia"
m_total$location_name[m_total$location_name == "Antigua and Barbuda"] <- "Antigua and Barb."
m_total$location_name[m_total$location_name == "United Republic of Tanzania"] <- "Tanzania"
m_total$location_name[m_total$location_name == "Republic of Moldova"] <- "Moldova"
m_total$location_name[m_total$location_name == "Brunei Darussalam"] <- "Brunei"
m_total$location_name[m_total$location_name == "Solomon Islands"] <- "Solomon Is."
m_total$location_name[m_total$location_name == "Northern Mariana Islands"] <- "N. Mariana Is."
m_total$location_name[m_total$location_name == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
m_total$location_name[m_total$location_name == "Cook Islands"] <- "Cook Is."
m_total$location_name[m_total$location_name == "Republic of Cuba"] <- "Cuba"
m_total$location_name[m_total$location_name == "Plurinational State of Bolivia"] <- "Bolivia"
m_total$location_name[m_total$location_name == "Republic of Maldives"] <- "Maldives"
m_total$location_name[m_total$location_name == "Independent State of Papua New Guinea"] <- "Papua New Guinea"
m_total$location_name[m_total$location_name == "Principality of Andorra"] <- "Andorra"
m_total$location_name[m_total$location_name == "Republic of Sierra Leone"] <- "Sierra Leone"
m_total$location_name[m_total$location_name == "State of Kuwait"] <- "Kuwait"
m_total$location_name[m_total$location_name == "Bolivarian Republic of Venezuela"] <- "Venezuela"
m_total$location_name[m_total$location_name == "Republic of Chile"] <- "Chile"
m_total$location_name[m_total$location_name == "Republic of Yemen"] <- "Yemen"
m_total$location_name[m_total$location_name == "Republic of Madagascar"] <- "Madagascar"
m_total$location_name[m_total$location_name == "Republic of Italy"] <- "Italy"
m_total$location_name[m_total$location_name == "Kingdom of Lesotho"] <- "Lesotho"
m_total$location_name[m_total$location_name == "Commonwealth of Dominica"] <- "Dominica"
m_total$location_name[m_total$location_name == "Independent State of Samoa"] <- "Samoa"
m_total$location_name[m_total$location_name == "Togolese Republic"] <- "Togo"
m_total$location_name[m_total$location_name == "Lebanese Republic"] <- "Lebanon"
m_total$location_name[m_total$location_name == "Republic of the Union of Myanmar"] <- "Myanmar"
m_total$location_name[m_total$location_name == "Republic of Tajikistan"] <- "Tajikistan"
m_total$location_name[m_total$location_name == "Republic of Poland"] <- "Poland"
m_total$location_name[m_total$location_name == "Republic of Ecuador"] <- "Ecuador"
m_total$location_name[m_total$location_name == "Republic of Austria"] <- "Austria"
m_total$location_name[m_total$location_name == "Republic of the Gambia"] <- "Gambia"
m_total$location_name[m_total$location_name == "Republic of Malawi"] <- "Malawi"
m_total$location_name[m_total$location_name == "Grand Duchy of Luxembourg"] <- "Luxembourg"
m_total$location_name[m_total$location_name == "Eastern Republic of Uruguay"] <- "Uruguay"
m_total$location_name[m_total$location_name == "Republic of the Philippines"] <- "Philippines"
m_total$location_name[m_total$location_name == "Republic of Namibia"] <- "Namibia"
m_total$location_name[m_total$location_name == "Republic of Malta"] <- "Malta"
m_total$location_name[m_total$location_name == "Republic of Ghana"] <- "Ghana"
m_total$location_name[m_total$location_name == "Republic of Cyprus"] <- "Cyprus"
m_total$location_name[m_total$location_name == "Republic of San Marino"] <- "San Marino"
m_total$location_name[m_total$location_name == "Republic of Serbia"] <- "Serbia"
m_total$location_name[m_total$location_name == "Republic of Peru"] <- "Peru"
m_total$location_name[m_total$location_name == "Kingdom of Belgium"] <- "Belgium"
m_total$location_name[m_total$location_name == "Republic of Mauritius"] <- "Mauritius"
m_total$location_name[m_total$location_name == "Federative Republic of Brazil"] <- "Brazil"
m_total$location_name[m_total$location_name == "Slovak Republic"] <- "Slovakia"
m_total$location_name[m_total$location_name == "Republic of Guinea"] <- "Guinea"
m_total$location_name[m_total$location_name == "Kingdom of Tonga"] <- "Tonga"
m_total$location_name[m_total$location_name == "Democratic Socialist Republic of Sri Lanka"] <- "Sri Lanka"
m_total$location_name[m_total$location_name == "Republic of South Africa"] <- "South Africa"
m_total$location_name[m_total$location_name == "People's Democratic Republic of Algeria"] <- "Algeria"
m_total$location_name[m_total$location_name == "Republic of Uzbekistan"] <- "Uzbekistan"
m_total$location_name[m_total$location_name == "State of Qatar"] <- "Qatar"
m_total$location_name[m_total$location_name == "Kingdom of Morocco"] <- "Morocco"
m_total$location_name[m_total$location_name == "Republic of Paraguay"] <- "Paraguay"
m_total$location_name[m_total$location_name == "Republic of Equatorial Guinea"] <- "Eq. Guinea"
m_total$location_name[m_total$location_name == "State of Libya"] <- "Libya"
m_total$location_name[m_total$location_name == "Islamic Republic of Afghanistan"] <- "Afghanistan"
m_total$location_name[m_total$location_name == "Kingdom of the Netherlands"] <- "Netherlands"
m_total$location_name[m_total$location_name == "Kingdom of Thailand"] <- "Thailand"
m_total$location_name[m_total$location_name == "Republic of Guyana"] <- "Guyana"
m_total$location_name[m_total$location_name == "Republic of Colombia"] <- "Colombia"
m_total$location_name[m_total$location_name == "People's Republic of Bangladesh"] <- "Bangladesh"
m_total$location_name[m_total$location_name == "Republic of Sudan"] <- "Sudan"
m_total$location_name[m_total$location_name == "Arab Republic of Egypt"] <- "Egypt"
m_total$location_name[m_total$location_name == "Republic of Haiti"] <- "Haiti"
m_total$location_name[m_total$location_name == "Kingdom of Eswatini"] <- "eSwatini"
m_total$location_name[m_total$location_name == "Republic of Guinea-Bissau"] <- "Guinea-Bissau"
m_total$location_name[m_total$location_name == "Republic of Liberia"] <- "Liberia"
m_total$location_name[m_total$location_name == "Republic of Mozambique"] <- "Mozambique"
m_total$location_name[m_total$location_name == "Kingdom of Denmark"] <- "Denmark"
m_total$location_name[m_total$location_name == "Gabonese Republic"] <- "Gabon"
m_total$location_name[m_total$location_name == "Kingdom of Norway"] <- "Norway"
m_total$location_name[m_total$location_name == "Democratic Republic of Timor-Leste"] <- "Timor-Leste"
m_total$location_name[m_total$location_name == "Republic of Vanuatu"] <- "Vanuatu"
m_total$location_name[m_total$location_name == "Republic of Rwanda"] <- "Rwanda"
m_total$location_name[m_total$location_name == "Republic of Zimbabwe"] <- "Zimbabwe"
m_total$location_name[m_total$location_name == "Republic of Costa Rica"] <- "Costa Rica"
m_total$location_name[m_total$location_name == "Republic of Albania"] <- "Albania"
m_total$location_name[m_total$location_name == "Sultanate of Oman"] <- "Oman"
m_total$location_name[m_total$location_name == "Republic of Slovenia"] <- "Slovenia"
m_total$location_name[m_total$location_name == "Republic of Finland"] <- "Finland"
m_total$location_name[m_total$location_name == "Republic of Croatia"] <- "Croatia"
m_total$location_name[m_total$location_name == "Portuguese Republic"] <- "Portugal"
m_total$location_name[m_total$location_name == "Union of the Comoros"] <- "Comoros"
m_total$location_name[m_total$location_name == "Kingdom of Bhutan"] <- "Bhutan"
m_total$location_name[m_total$location_name == "Islamic Republic of Mauritania"] <- "Mauritania"
m_total$location_name[m_total$location_name == "Republic of El Salvador"] <- "El Salvador"
m_total$location_name[m_total$location_name == "Republic of Trinidad and Tobago"] <- "Trinidad and Tobago"
m_total$location_name[m_total$location_name == "Republic of India"] <- "India"
m_total$location_name[m_total$location_name == "French Republic"] <- "France"
m_total$location_name[m_total$location_name == "Republic of Seychelles"] <- "Seychelles"
m_total$location_name[m_total$location_name == "Republic of Mali"] <- "Mali"
m_total$location_name[m_total$location_name == "Federal Republic of Somalia"] <- "Somalia"
m_total$location_name[m_total$location_name == "Republic of Burundi"] <- "Burundi"
m_total$location_name[m_total$location_name == "Kingdom of Spain"] <- "Spain"
m_total$location_name[m_total$location_name == "Republic of Indonesia"] <- "Indonesia"
m_total$location_name[m_total$location_name == "Principality of Monaco"] <- "Monaco"
m_total$location_name[m_total$location_name == "Republic of Benin"] <- "Benin"
m_total$location_name[m_total$location_name == "Republic of Guatemala"] <- "Guatemala"
m_total$location_name[m_total$location_name == "Republic of Azerbaijan"] <- "Azerbaijan"
m_total$location_name[m_total$location_name == "Republic of Fiji"] <- "Fiji"
m_total$location_name[m_total$location_name == "Kingdom of Cambodia"] <- "Cambodia"
m_total$location_name[m_total$location_name == "Federal Republic of Germany"] <- "Germany"
m_total$location_name[m_total$location_name == "Republic of Armenia"] <- "Armenia"
m_total$location_name[m_total$location_name == "Republic of Bulgaria"] <- "Bulgaria"
m_total$location_name[m_total$location_name == "Republic of Singapore"] <- "Singapore"
m_total$location_name[m_total$location_name == "Kingdom of Sweden"] <- "Sweden"
m_total$location_name[m_total$location_name == "Republic of Belarus"] <- "Belarus"
m_total$location_name[m_total$location_name == "Republic of Estonia"] <- "Estonia"
m_total$location_name[m_total$location_name == "Kingdom of Bahrain"] <- "Bahrain"
m_total$location_name[m_total$location_name == "Republic of the Niger"] <- "Niger"
m_total$location_name[m_total$location_name == "Republic of Honduras"] <- "Honduras"
m_total$location_name[m_total$location_name == "Federal Democratic Republic of Nepal"] <- "Nepal"
m_total$location_name[m_total$location_name == "Swiss Confederation"] <- "Switzerland"
m_total$location_name[m_total$location_name == "Islamic Republic of Pakistan"] <- "Pakistan"
m_total$location_name[m_total$location_name == "Hellenic Republic"] <- "Greece"
m_total$location_name[m_total$location_name == "Republic of Uganda"] <- "Uganda"
m_total$location_name[m_total$location_name == "Republic of Djibouti"] <- "Djibouti"
m_total$location_name[m_total$location_name == "Commonwealth of the Bahamas"] <- "Bahamas"
m_total$location_name[m_total$location_name == "Republic of Nauru"] <- "Nauru"
m_total$location_name[m_total$location_name == "Republic of Iceland"] <- "Iceland"
m_total$location_name[m_total$location_name == "Republic of Palau"] <- "Palau"
m_total$location_name[m_total$location_name == "Republic of South Sudan"] <- "S. Sudan"
m_total$location_name[m_total$location_name == "Republic of Kiribati"] <- "Kiribati"
m_total$location_name[m_total$location_name == "Kingdom of Saudi Arabia"] <- "Saudi Arabia"
m_total$location_name[m_total$location_name == "Republic of Suriname"] <- "Suriname"
m_total$location_name[m_total$location_name == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
m_total$location_name[m_total$location_name == "Republic of Tunisia"] <- "Tunisia"
m_total$location_name[m_total$location_name == "Federal Republic of Nigeria"] <- "Nigeria"
m_total$location_name[m_total$location_name == "Republic of Latvia"] <- "Latvia"
m_total$location_name[m_total$location_name == "Republic of Cameroon"] <- "Cameroon"
m_total$location_name[m_total$location_name == "Republic of Angola"] <- "Angola"
m_total$location_name[m_total$location_name == "Republic of Niue"] <- "Niue"
m_total$location_name[m_total$location_name == "Republic of Zambia"] <- "Zambia"
m_total$location_name[m_total$location_name == "Republic of the Marshall Islands"] <- "Marshall Is."
m_total$location_name[m_total$location_name == "Democratic Republic of Sao Tome and Principe"] <- "Sao Tome and Principe"
m_total$location_name[m_total$location_name == "Republic of Kazakhstan"] <- "Kazakhstan"
m_total$location_name[m_total$location_name == "Republic of Iraq"] <- "Iraq"
m_total$location_name[m_total$location_name == "Republic of Nicaragua"] <- "Nicaragua"
m_total$location_name[m_total$location_name == "United Mexican States"] <- "Mexico"
m_total$location_name[m_total$location_name == "State of Eritrea"] <- "Eritrea"
m_total$location_name[m_total$location_name == "Federal Democratic Republic of Ethiopia"] <- "Ethiopia"
m_total$location_name[m_total$location_name == "Argentine Republic"] <- "Argentina"
m_total$location_name[m_total$location_name == "Republic of Turkey"] <- "Turkey"
m_total$location_name[m_total$location_name == "Republic of Cabo Verde"] <- "Cabo Verde"
m_total$location_name[m_total$location_name == "Republic of Lithuania"] <- "Lithuania"
m_total$location_name[m_total$location_name == "Federated States of Micronesia"] <- "Micronesia"
m_total$location_name[m_total$location_name == "State of Israel"] <- "Israel"
m_total$location_name[m_total$location_name == "Republic of Chad"] <- "Chad"
m_total$location_name[m_total$location_name == "Islamic Republic of Iran"] <- "Iran"
m_total$location_name[m_total$location_name == "Republic of Senegal"] <- "Senegal"
m_total$location_name[m_total$location_name == "Republic of Panama"] <- "Panama"
m_total$location_name[m_total$location_name == "Republic of Kenya"] <- "Kenya"
m_total$location_name[m_total$location_name == "Kyrgyz Republic"] <- "Kyrgyzstan"
m_total$location_name[m_total$location_name == "Hashemite Kingdom of Jordan"] <- "Jordan"
m_total$location_name[m_total$location_name == "Republic of Côte d'Ivoire"] <- "Côte d'Ivoire"
m_total$location_name[m_total$location_name == "Republic of Botswana"] <- "Botswana"
m_total$location_name[m_total$location_name == "Czech Republic"] <- "Czechia"
m_total$location_name[m_total$location_name == "United States Virgin Islands"] <- "U.S. Virgin Is."

unique(m_total$age_name)
unique(m_total$cause_name)
m_total$age_name[m_total$age_name == "80-84"] <- "80-84 years"
m_total$age_name[m_total$age_name == "85-89"] <- "85-89 years"
m_total$age_name[m_total$age_name == "90-94"] <- "90-94 years"

fwrite(m_total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/gbd_cataract_1990_2021.csv",row.names = FALSE)

############################Perform Monte Carlo sampling for incidence data, for three types of skin cancers
library(dplyr)
library(data.table)
library(triangle)
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1)
pop$lat <- round(pop$lat * 2) / 2
# Round the fourth decimal place
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
pop_long
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex

gbd <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/gbd_1980_2021.csv")
unique(gbd$cause_name)
gbd_ <- gbd[gbd$measure_name=="Incidence"&gbd$cause_name=="Non-melanoma skin cancer (basal-cell carcinoma)"&gbd$age_name!="All ages",]
colnames(gbd_)[which(names(gbd_) == "location_name")] <- "country"
total <- merge(pop_long,gbd_, by=c("country","year","age_name","sex_name"))
total <- data.frame(total)
total <- total[total$val!=0,]
total$incidence_val <- total$val/total$pop_country_age_sex
total$incidence_permillion_val <- total$incidence_val*10^6
total$incidence_lower <- total$lower/total$pop_country_age_sex
total$incidence_permillion_lower <- total$incidence_lower*10^6
total$incidence_upper <- total$upper/total$pop_country_age_sex
total$incidence_permillion_upper <- total$incidence_upper*10^6
total_ <- total %>%
  select(country,year, sex_name,age_name, incidence_permillion_val,incidence_permillion_lower,incidence_permillion_upper) %>%
  distinct()
# Define a normal distribution sampling function, with faster approach to avoid negative values
normal_sample <- function(mean, sd, n) {
  # Generate samples in batches and filter out negative values directly
  samples <- rnorm(n, mean = mean, sd = sd)
  # Filter out the negative values (retain valid samples only)
  valid_samples <- samples[samples >= 0]
  # If not enough valid samples, regenerate only the invalid ones
  while (length(valid_samples) < n) {
    additional_samples <- rnorm(n - length(valid_samples), mean = mean, sd = sd)
    valid_samples <- c(valid_samples, additional_samples[additional_samples >= 0])
  }
  # Return exactly n valid samples
  return(valid_samples[1:n])
}

for (i in 1990:2021) {
  total_y <- total_[total_$year == i,]
  print(i)
  results <- total_y %>%
    rowwise() %>%
    do({
      print(paste("Processing:", .$country, .$year, .$sex_name, .$age_name))
      lower <- .$incidence_permillion_lower
      mode <- .$incidence_permillion_val
      upper <- .$incidence_permillion_upper
      print(paste("lower:", lower, "mode:", mode, "upper:", upper))
      # Check for invalid values (Inf or NA) and skip if found
      if (!is.na(lower) && !is.na(mode) && !is.na(upper) && lower != Inf && mode != Inf && upper != Inf && lower <= mode && mode <= upper) {
        mean <- mode
        sd <- (upper - lower) / 4  # Estimate SD from range
        # Ensure SD is positive
        if (sd > 0) {
          tryCatch({
            samples <- normal_sample(mean, sd, 1000)
          }, error = function(e) {
            print(paste("Error in normal_sample:", e$message))
            samples <- rep(NA, 1000)
          })
        } else {
          print("Invalid standard deviation")
          samples <- rep(NA, 1000)
        }
      } else {
        print("Invalid parameters for normal distribution (Inf or NA values)")
        samples <- rep(NA, 1000)
      }
      data.frame(
        country = .$country,
        year = .$year,
        sex_name = .$sex_name,
        age_name = .$age_name,
        incidence_permillion_sample = samples,
        simulations = 1:1000
      )
    })
  head(results)
  results <- data.frame(results)
  fwrite(results, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence/bcc_incidence_mtkl_1000_%d.csv", i), row.names = FALSE)
}

#Merge all files into one
file_directory <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence/"
file_names <- list.files(file_directory, pattern = "^bcc_incidence_mtkl_1000_199[0-9].csv$|^bcc_incidence_mtkl_1000_20[0-2][0-9].csv$", full.names = TRUE)
data_list <- list()
for (file_name in file_names) {
  print(file_name)
  data <- fread(file_name)
  data_list[[length(data_list) + 1]] <- data
}
sum_total <- rbindlist(data_list)
head(sum_total)
fwrite(sum_total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence/bcc_incidence_mtkl_1000.csv",row.names = FALSE)

############################Perform Monte Carlo sampling for radiation data, for MM and BCC
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
no_cores <- 55
registerDoParallel(cores = no_cores)
ra_annual <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual.csv")
ra_annual$sum_y_mj <- ra_annual$sum_y * 3600 / 10^6
unique_coords <- unique(ra_annual[, .(lon, lat)])
unique_coords <- as.data.frame(unique_coords)
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra_annual <- merge(ra_annual,country,by=c("lon","lat"))
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro_perage.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid_perage.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1)
pop$lat <- round(pop$lat * 2) / 2
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex

#function to get the age groups
get_age_group <- function(age) {
  if (age <= 4) {
    "1-4 years"
  } else if (age >= 95) {
    "95+ years"
  } else {
    lower_bound <- 5 * ((age - 5) %/% 5) + 5
    upper_bound <- lower_bound + 4
    age_range <- paste(lower_bound, upper_bound, sep="-")
    paste(age_range, "years", sep=" ")
  }
}

#function to calculate the radiation in current year
calculate_dose_a <- function(a, Dose) {
  Dose <- Dose[1:a]
  for (x in 0:(a - 1)) {
    dose_a <- Dose[x + 1]
  }
  return(dose_a)
}
# function to calculate the Y
calculate_Y_BCC_CM_weighted <- function(dose_a_weighted_total, c, d) {
  sum_val <- 0
  for (x in 1:(length(dose_a_weighted_total))) {
    phi_a <- sum(dose_a_weighted_total[1:(x)])
    sum_val <- sum_val + dose_a_weighted_total[x] *phi_a* ((length(dose_a_weighted_total) - x)^(d - c))
  }
  return(sum_val)
}

# Set the uncertainty ranges for c and d
c_mean <-0.6
c_sd <- 0.4 / 1.96
d_mean <- 4.7
d_sd <- 1 / 1.96
# Set the times of simulation
n_simulations <- 1000

#The year_ ranges from 1990 to 2021
for (year_ in 1995) {
  print(paste("Processing year:", year_))
  ra_annual_filtered <- ra_annual[ra_annual$year >= (year_ - 100) & ra_annual$year <= (year_ - 1), ]
  results <- foreach(coord = iter(unique_coords, by = 'row'), .combine = rbind, .packages = 'data.table') %dopar% {
    lon_ <- coord[["lon"]]
    lat_ <- coord[["lat"]]
    df_all <- ra_annual_filtered[lon == lon_ & lat == lat_, ]
    setorder(df_all, -year)
    Dose <- as.vector(df_all$sum_y_mj)
    df <- data.frame(lat = numeric(0), lon = numeric(0), age = numeric(0), dose_a = numeric(0), phi_a = numeric(0))
    for (age in 1:100) {
      dose_a <- calculate_dose_a(age, Dose)
      result <- data.frame(lat = lat_, lon = lon_, age = age, dose_a = dose_a)
      df <- rbind(df, result)
    }
    return(df)
  }
  results <- na.omit(results)
  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total <- merge(country,results,by=c("lat","lon"))
  total$year_history <- year_-total$age
  colnames(total)[which(names(total) == "age")] <- "age_name"
  class(pop_long$age_name)
  pop_long$age_name <- as.numeric(pop_long$age_name)
  pop_long <- na.omit(pop_long)
  total$year <- year_
  total <- merge(total,pop_long,by=c("lat","lon","year","age_name","country"))
  total$dose_a_weighted <- total$dose_a*total$pop_pro
  ra_per_country_per_age_per_sex <- total %>%
    group_by(country,year_history,age_name,sex_name) %>%
    summarise(dose_a_weighted_total = sum(dose_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex <- data.frame(ra_per_country_per_age_per_sex)
  total <- merge(total,ra_per_country_per_age_per_sex,by=c("country","year_history","age_name","sex_name"))
  total <- na.omit(total)
  # Filter the combinations in the total dataframe
  unique_combinations <- unique(total[, .(country, year, age_name, sex_name)])
  unique_combinations <- na.omit(unique_combinations)
  # Monte Carlo simulation and Y (uncorrected) calculation were performed
  simulation_results <- foreach(row = 1:nrow(unique_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- unique_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total[total$country == country_ & total$year == year_ & total$age_name <= age_name_ & total$sex_name == sex_name_, ]
    group <- group %>%
      select(country, year,year_history, age_name, sex_name, dose_a_weighted_total) %>%
      distinct()
    dose_a_weighted_total <- unlist(group$dose_a_weighted_total)
    simulations <- data.frame(simulation = 1:n_simulations, Y_BCC_CM = NA)
    for (sim in 1:n_simulations) {
      c_sim <- rnorm(1, c_mean, c_sd)
      d_sim <- rnorm(1, d_mean, d_sd)
      age <- as.numeric(age_name_)
      simulations$Y_BCC_CM[sim] <- calculate_Y_BCC_CM_weighted(dose_a_weighted_total, c_sim, d_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    simulations
  }
  simulation_results<- data.frame(simulation_results)
  colnames(simulation_results)[which(names(simulation_results) == "age_name")] <- "age"
  simulation_results_ <- simulation_results %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_BCC_CM = mean(Y_BCC_CM, na.rm = TRUE))
  fwrite(simulation_results_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/new_sum_m_mj_mtkl_1000_combined_%d.csv", year_), row.names = FALSE)
}
stopImplicitCluster()


############################Perform Monte Carlo sampling for radiation data, for SCC
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
no_cores <- 55
registerDoParallel(cores = no_cores)
ra_annual <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_NMSC.csv")
ra_annual$sum_y_mj <- ra_annual$sum_y * 3600 / 10^6
unique_coords <- unique(ra_annual[, .(lon, lat)])
unique_coords <- as.data.frame(unique_coords)
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra_annual <- merge(ra_annual,country,by=c("lon","lat"))
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro_perage.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid_perage.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1)
pop$lat <- round(pop$lat * 2) / 2
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex

get_age_group <- function(age) {
  if (age <= 4) {
    "1-4 years"
  } else if (age >= 95) {
    "95+ years"
  } else {
    lower_bound <- 5 * ((age - 5) %/% 5) + 5
    upper_bound <- lower_bound + 4
    age_range <- paste(lower_bound, upper_bound, sep="-")
    paste(age_range, "years", sep=" ")
  }
}

# function to calculate the age accumulated radiation
calculate_phi_a <- function(a, Dose) {
  Dose <- Dose[1:a]
  Dose <- rev(Dose)
  for (x in 0:(a - 1)) {
    phi_a <- sum(Dose[1:(x + 1)])
  }
  return(phi_a)
}

# function to calculate the Y
calculate_Y_SCC_weighted <- function(phi_a_weighted_total, age, c, d) {
  sum_val <- 0
  for (x in 0:(age - 1)) {
    sum_val <- (phi_a_weighted_total^(c)) * ((age)^(d-c))
  }
  return(sum_val)
}

# Set the uncertainty ranges for c and d
c_mean <-2.5
c_sd <- 0.7 / 1.96
d_mean <- 6.6
d_sd <- 0.4 / 1.96
# Set the times of simulation
n_simulations <- 1000

#The year_ ranges from 1990 to 2021
for (year_ in 1990) {
  print(paste("Processing year:", year_))
  ra_annual_filtered <- ra_annual[ra_annual$year >= (year_ - 100) & ra_annual$year <= (year_ - 1), ]
  results <- foreach(coord = iter(unique_coords, by = 'row'), .combine = rbind, .packages = 'data.table') %dopar% {
    print(coord)
    lon_ <- coord[["lon"]]
    lat_ <- coord[["lat"]]
    df_all <- ra_annual_filtered[lon == lon_ & lat == lat_, ]
    setorder(df_all, -year)
    Dose <- as.vector(df_all$sum_y_mj)
    df <- data.frame(lat = numeric(0), lon = numeric(0), age = numeric(0), phi_a = numeric(0))
    for (age in 1:100) {
      phi_a <- calculate_phi_a(age, Dose)
      result <- data.frame(lat = lat_, lon = lon_, age = age, phi_a = phi_a)
      df <- rbind(df, result)
    }
    return(df)
  }
  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total <- merge(country,results,by=c("lat","lon"))
  total$year <- year_
  colnames(total)[which(names(total) == "age")] <- "age_name"
  class(pop_long$age_name)
  pop_long$age_name <- as.numeric(pop_long$age_name)
  pop_long <- na.omit(pop_long)
  total <- merge(total,pop_long,by=c("lat","lon","year","age_name","country"))
  total$phi_a_weighted <- total$phi_a*total$pop_pro
  ra_per_country_per_age_per_sex <- total %>%
    group_by(country,year,age_name,sex_name) %>%
    summarise(phi_a_weighted_total = sum(phi_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex <- data.frame(ra_per_country_per_age_per_sex)
  total <- merge(total,ra_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
  total <- na.omit(total)
  # Filter the combinations in the total dataframe
  unique_combinations <- unique(total[, .(country, year, age_name, sex_name)])
  unique_combinations <- na.omit(unique_combinations)
  # Monte Carlo simulation and Y (uncorrected) calculation were performed
  simulation_results <- foreach(row = 1:nrow(unique_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- unique_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total[total$country == country_ & total$year == year_ & total$age_name == age_name_ & total$sex_name == sex_name_, ]
    phi_a_weighted_total <- group$phi_a_weighted_total[1]
    simulations <- data.frame(simulation = 1:n_simulations, Y_SCC = NA)
    for (sim in 1:n_simulations) {
      c_sim <- rnorm(1, c_mean, c_sd)
      d_sim <- rnorm(1, d_mean, d_sd)
      age <- as.numeric(age_name_)
      simulations$Y_SCC[sim] <- calculate_Y_SCC_weighted(phi_a_weighted_total, age, c_sim, d_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    simulations
  }
  simulation_results<- data.frame(simulation_results)
  colnames(simulation_results)[which(names(simulation_results) == "age_name")] <- "age"
  simulation_results_ <- simulation_results %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_SCC = mean(Y_SCC, na.rm = TRUE))
  fwrite(simulation_results_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/new_sum_scc_mj_mtkl_1000_combined_%d.csv", year_), row.names = FALSE)
}
stopImplicitCluster()


######################Merge into one large dataframe (country and year may need to be split due to large size of this dataframe)
library(data.table)
folder_path <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/"
combined_data <- data.table()
for (year in 1990:2021) {
  file_name <- paste0("new_sum_scc_mj_mtkl_1000_combined_", year, ".csv")
  file_path <- file.path(folder_path, file_name)
  year_data <- fread(file_path)
  combined_data <- rbindlist(list(combined_data, year_data), use.names = TRUE, fill = TRUE)
}
output_file <- file.path(folder_path, "ra_mtkl_scc_1000.csv")
fwrite(combined_data, output_file, row.names = FALSE)

#####################################################generate c and d
library(data.table)
#generate random 3000 c and d
#scc
c_mean <- 2.5
c_sd <- 0.7 / 1.96
d_mean <- 6.6
d_sd <- 0.4 / 1.96
#bcc
# c_mean <- 1.4
# c_sd <- 0.4 / 1.96
# d_mean <- 4.9
# d_sd <- 0.6 / 1.96
#m
# c_mean <- 0.6
# c_sd <- 0.4 / 1.96
# d_mean <- 4.7
# d_sd <- 1 / 1.96
# set up simulations
#n_simulations <- 3000
set.seed(123)
c_simulations <- rnorm(n_simulations, mean = c_mean, sd = c_sd)
d_simulations <- rnorm(n_simulations, mean = d_mean, sd = d_sd)
#merge c_simulations and d_simulations
simulation_df <- data.frame(
  simulation = 1:n_simulations,
  c = c_simulations,
  d = d_simulations
)
# 查看前几行数据
head(simulation_df)
fwrite(simulation_df, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_d_scc.csv"), row.names = FALSE)

#######################################################################calculate and store the b
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
inc <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence/scc_incidence_mtkl_1000.csv")
ra <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/ra_mtkl_scc_1000.csv")
colnames(inc)[which(names(inc) == "simulations")] <- "simulation"
total <- merge(inc,ra,by=c("year","country","age_name","sex_name","simulation"))
#total$b <- total$incidence_permillion_sample/total$average_Y_BCC_CM
total$b <- total$incidence_permillion_sample/total$average_Y_SCC
fwrite(total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/b_scc_1000.csv",row.names = FALSE)


library(data.table)
library(dplyr)
merged_data <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/b_scc_1000.csv")
#setDT(merged_data)
result <- merged_data[, {
  if (.N >= 1) {
    sampled_b <- sample(b, size = 3000, replace = TRUE)  # 仅从当前分组抽样
    data.table(
      b = sampled_b,
      simulation = 1:3000
    )
  } else {
    NULL  # 避免空分组报错
  }
}, by = .(country, age_name, sex_name)]
head(result)
fwrite(result,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/extract_b_scc_3000_new.csv",row.names = FALSE)



#################################################################################################for the mortality, all for 1990-2021
##########calculate the age-accumulated radiation
library(data.table)
merged_data <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_nocontrol.csv")
earliest_year <- min(merged_data$year)
latest_year <- max(merged_data$year)
setDT(merged_data)
# Define the year and age range in advance
years <- 1890:2100
max_age <- 100

# Cycle through each year
for (current_year in years) {
  #current_year <- 1891
  print(current_year)
  results <- data.table(lat = numeric(0), lon = numeric(0), sum_age = numeric(0), age = integer(0), year = integer(0))
  # Filter data for all relevant years
  relevant_years <- merged_data[year <= current_year & year >= max(1890, current_year - max_age + 1),]
  # Calculate the cumulative amount of radiation at each location and each age
  for (age in 1:min(max_age, current_year - 1889)) {
    year_of_age = current_year - age + 1
    sum_age_data <- relevant_years[year >= year_of_age, .(sum_age = sum(sum_y)), by = .(lat, lon)]
    sum_age_data[, `:=`(age = age, year = current_year)]
    results <- rbindlist(list(results, sum_age_data), use.names = TRUE, fill = TRUE)
  }
  setorder(results, lat, lon, -age)
  fwrite(results, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/age_sum_m_nocontrol_%d.csv", current_year), row.names = FALSE)
}

file_path <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/"
years <- 1980:2100
file_names <- paste0("age_sum_wmo_m_", years, ".csv")
full_paths <- file.path(file_path, file_names)
ra <- do.call(rbind, lapply(full_paths, read.csv))
head(ra)
# Function that returns the corresponding age group based on age
get_age_group <- function(age) {
  if (age <= 4) {
    return("1-4 years")
  } else if (age <= 9) {
    return("5-9 years")
  } else if (age <= 14) {
    return("10-14 years")
  } else if (age <= 19) {
    return("15-19 years")
  } else if (age <= 24) {
    return("20-24 years")
  } else if (age <= 29) {
    return("25-29 years")
  } else if (age <= 34) {
    return("30-34 years")
  } else if (age <= 39) {
    return("35-39 years")
  } else if (age <= 44) {
    return("40-44 years")
  } else if (age <= 49) {
    return("45-49 years")
  } else if (age <= 54) {
    return("50-54 years")
  } else if (age <= 59) {
    return("55-59 years")
  } else if (age <= 64) {
    return("60-64 years")
  } else if (age <= 69) {
    return("65-69 years")
  } else if (age <= 74) {
    return("70-74 years")
  } else if (age <= 79) {
    return("75-79 years")
  } else if (age <= 84) {
    return("80-84 years")
  } else if (age <= 89) {
    return("85-89 years")
  } else if (age <= 94) {
    return("90-94 years")
  } else {
    return("95+ years")
  }
}
ra[, age_name := sapply(age, get_age_group)]
ra_mean <- ra %>%
  group_by(lat,lon,year,age_name) %>%
  summarise(ra_mean = mean(sum_age, na.rm = TRUE))
ra_mean <- data.frame(ra_mean)
fwrite(ra_mean,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/radiation_m_1980_2100.csv")

############################Perform Monte Carlo sampling for mortality data, for two types of skin cancers, MM and SCC
#########################################c in the mortality
library(data.table)
#生成 c 和 d 的 3000 组随机组合
# 设置c和d的不确定性范围
#m_de_male
# c_mean <- 0.58
# c_sd <- 0.02 / 1.96

#m_de_female
# c_mean <- 0.5
# c_sd <- 0.02 / 1.96

#scc_de_male
# c_mean <- 0.71
# c_sd <- 0.03 / 1.96

#scc_de_female
c_mean <- 0.46
c_sd <- 0.03 / 1.96

# 设置模拟次数
n_simulations <- 3000
set.seed(123)  # 设置随机种子以保证可重复性
c_simulations <- rnorm(n_simulations, mean = c_mean, sd = c_sd)
# 将 c_simulations 和 d_simulations 组合为一个数据框
simulation_df <- data.frame(
  simulation = 1:n_simulations,
  c = c_simulations
)
# 查看前几行数据
head(simulation_df)
fwrite(simulation_df, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_scc_de_female.csv"), row.names = FALSE)


library(dplyr)
library(data.table)
library(triangle)
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1)
pop$lat <- round(pop$lat * 2) / 2
# Round the fourth decimal place
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
pop_long
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex

gbd <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/gbd_1980_2021.csv")
unique(gbd$cause_name)
gbd_ <- gbd[gbd$measure_name=="Deaths"&gbd$cause_name=="Malignant skin melanoma"&gbd$age_name!="All ages",]
colnames(gbd_)[which(names(gbd_) == "location_name")] <- "country"
total <- merge(pop_long,gbd_, by=c("country","year","age_name","sex_name"))
total <- data.frame(total)
total <- total[total$val!=0,]
total$incidence_val <- total$val/total$pop_country_age_sex
total$incidence_permillion_val <- total$incidence_val*10^6
total$incidence_lower <- total$lower/total$pop_country_age_sex
total$incidence_permillion_lower <- total$incidence_lower*10^6
total$incidence_upper <- total$upper/total$pop_country_age_sex
total$incidence_permillion_upper <- total$incidence_upper*10^6
total_ <- total %>%
  select(country,year, sex_name,age_name, incidence_permillion_val,incidence_permillion_lower,incidence_permillion_upper) %>%
  distinct()
# Define a normal distribution sampling function, with faster approach to avoid negative values
normal_sample <- function(mean, sd, n) {
  # Generate samples in batches and filter out negative values directly
  samples <- rnorm(n, mean = mean, sd = sd)
  # Filter out the negative values (retain valid samples only)
  valid_samples <- samples[samples >= 0]
  # If not enough valid samples, regenerate only the invalid ones
  while (length(valid_samples) < n) {
    additional_samples <- rnorm(n - length(valid_samples), mean = mean, sd = sd)
    valid_samples <- c(valid_samples, additional_samples[additional_samples >= 0])
  }
  # Return exactly n valid samples
  return(valid_samples[1:n])
}
for (i in 1990:2021) {
  total_y <- total_[total_$year == i,]
  print(i)
  results <- total_y %>%
    rowwise() %>%
    do({
      print(paste("Processing:", .$country, .$year, .$sex_name, .$age_name))
      lower <- .$incidence_permillion_lower
      mode <- .$incidence_permillion_val
      upper <- .$incidence_permillion_upper
      print(paste("lower:", lower, "mode:", mode, "upper:", upper))
      # Check for invalid values (Inf or NA) and skip if found
      if (!is.na(lower) && !is.na(mode) && !is.na(upper) && lower != Inf && mode != Inf && upper != Inf && lower <= mode && mode <= upper) {
        mean <- mode
        sd <- (upper - lower) / 4  # Estimate SD from range
        # Ensure SD is positive
        if (sd > 0) {
          tryCatch({
            samples <- normal_sample(mean, sd, 1000)
          }, error = function(e) {
            print(paste("Error in normal_sample:", e$message))
            samples <- rep(NA, 1000)
          })
        } else {
          print("Invalid standard deviation")
          samples <- rep(NA, 1000)
        }
      } else {
        print("Invalid parameters for normal distribution (Inf or NA values)")
        samples <- rep(NA, 1000)
      }
      data.frame(
        country = .$country,
        year = .$year,
        sex_name = .$sex_name,
        age_name = .$age_name,
        incidence_permillion_sample = samples,
        simulations = 1:1000
      )
    })
  head(results)
  results <- data.frame(results)
  fwrite(results, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence/m_death_mtkl_1000_%d.csv", i), row.names = FALSE)
}

#Merge all files into one
file_directory <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence/"
file_names <- list.files(file_directory, pattern = "^m_death_mtkl_1000_199[0-9].csv$|^m_death_mtkl_1000_20[0-2][0-9].csv$", full.names = TRUE)
data_list <- list()
for (file_name in file_names) {
  print(file_name)
  data <- fread(file_name)
  data_list[[length(data_list) + 1]] <- data
}
sum_total <- rbindlist(data_list)
head(sum_total)
fwrite(sum_total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence/m_death_mtkl_1000.csv",row.names = FALSE)


library(data.table)
library(dplyr)
ra <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/radiation_b_s_1980_2100.csv")
ra_90_21<-ra[ra$year>=1990&ra$year<=2021,]
ra_90_21$ra_mj <- ra_90_21$ra_mean*3600/10^6
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra_90_21 <- merge(ra_90_21,country,by=c("lon","lat"))
library(dplyr)
library(data.table)
library(triangle)
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid.csv")
pop2 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2022_2100_grid.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop2 <- pop2[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1,pop2)
pop$lat <- round(pop$lat * 2) / 2
# Round the fourth decimal place
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
pop_long
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex
ra_ <- merge(ra_90_21,pop_long,by=c("lon","lat","country","year","age_name"))
ra_$ra_mj_weighted <- ra_$ra_mj*ra_$pop_pro
ra_per_country_per_age_per_sex <- ra_ %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(ra_mj_weighted_total = sum(ra_mj_weighted, na.rm = TRUE))
ra_per_country_per_age_per_sex<- data.frame(ra_per_country_per_age_per_sex)


m <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence/scc_death_mtkl_1000.csv")
# 确保数据是data.table格式
setDT(ra_per_country_per_age_per_sex)
setDT(m)
# 合并数据
merged_data <- merge(ra_per_country_per_age_per_sex, m, by = c("country", "year", "sex_name", "age_name"))
c_male <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_scc_de_male.csv")
c_male$sex_name <- "Male"
c_female <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_scc_de_female.csv")
c_female$sex_name <- "Female"
c <- rbind(c_male,c_female)
colnames(c)[which(names(c) == "simulation")] <- "simulations"
merged_data <- merge(merged_data,c,by=c("simulations","sex_name"))
merged_data <- merged_data %>%
  mutate(ra_mj_weighted_total_powered = ra_mj_weighted_total ^ c)

# 计算列b
merged_data[, b := incidence_permillion_sample / ra_mj_weighted_total_powered]
fwrite(merged_data,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/b_scc_de_1000_new.csv",row.names = FALSE)

library(data.table)
library(dplyr)
merged_data <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/b_scc_de_1000_new.csv")
result <- merged_data[, {
  if (.N >= 1) {
    sampled_b <- sample(b, size = 1000, replace = TRUE)
    data.table(
      b = sampled_b,
      simulation = 1:1000
    )
  } else {
    NULL  # 避免空分组报错
  }
}, by = .(country, age_name, sex_name)]
fwrite(result,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/extract_b_scc_de_1000_new.csv",row.names = FALSE)



############################Perform Monte Carlo sampling for cataract data
library(dplyr)
library(data.table)
library(triangle)
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1)
pop$lat <- round(pop$lat * 2) / 2
# Round the fourth decimal place
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
pop_long
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex

gbd <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/gbd_cataract_1990_2021.csv")
unique(gbd$cause_name)
gbd_ <- gbd[gbd$age_name!="All ages",]
colnames(gbd_)[which(names(gbd_) == "location_name")] <- "country"
total <- merge(pop_long,gbd_, by=c("country","year","age_name","sex_name"))
total <- data.frame(total)
total <- total[total$val!=0,]
total$incidence_val <- total$val/total$pop_country_age_sex
total$incidence_permillion_val <- total$incidence_val*10^6
total$incidence_lower <- total$lower/total$pop_country_age_sex
total$incidence_permillion_lower <- total$incidence_lower*10^6
total$incidence_upper <- total$upper/total$pop_country_age_sex
total$incidence_permillion_upper <- total$incidence_upper*10^6
total_ <- total %>%
  select(country,year, sex_name,age_name, incidence_permillion_val,incidence_permillion_lower,incidence_permillion_upper) %>%
  distinct()

# Define a normal distribution sampling function, with faster approach to avoid negative values
normal_sample <- function(mean, sd, n) {
  # Generate samples in batches and filter out negative values directly
  samples <- rnorm(n, mean = mean, sd = sd)
  # Filter out the negative values (retain valid samples only)
  valid_samples <- samples[samples >= 0]
  # If not enough valid samples, regenerate only the invalid ones
  while (length(valid_samples) < n) {
    additional_samples <- rnorm(n - length(valid_samples), mean = mean, sd = sd)
    valid_samples <- c(valid_samples, additional_samples[additional_samples >= 0])
  }
  # Return exactly n valid samples
  return(valid_samples[1:n])
}
for (i in 1990:2021) {
  total_y <- total_[total_$year == i,]
  print(i)
  results <- total_y %>%
    rowwise() %>%
    do({
      print(paste("Processing:", .$country, .$year, .$sex_name, .$age_name))
      lower <- .$incidence_permillion_lower
      mode <- .$incidence_permillion_val
      upper <- .$incidence_permillion_upper
      print(paste("lower:", lower, "mode:", mode, "upper:", upper))
      # Check for invalid values (Inf or NA) and skip if found
      if (!is.na(lower) && !is.na(mode) && !is.na(upper) && lower != Inf && mode != Inf && upper != Inf && lower <= mode && mode <= upper) {
        mean <- mode
        sd <- (upper - lower) / 4  # Estimate SD from range
        # Ensure SD is positive
        if (sd > 0) {
          tryCatch({
            samples <- normal_sample(mean, sd, 1000)
          }, error = function(e) {
            print(paste("Error in normal_sample:", e$message))
            samples <- rep(NA, 1000)
          })
        } else {
          print("Invalid standard deviation")
          samples <- rep(NA, 1000)
        }
      } else {
        print("Invalid parameters for normal distribution (Inf or NA values)")
        samples <- rep(NA, 1000)
      }
      data.frame(
        country = .$country,
        year = .$year,
        sex_name = .$sex_name,
        age_name = .$age_name,
        incidence_permillion_sample = samples,
        simulations = 1:1000
      )
    })
  head(results)
  results <- data.frame(results)
  fwrite(results, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence/cataract_mtkl_1000_%d.csv", i), row.names = FALSE)
}

#Merge all files into one
file_directory <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence/"
file_names <- list.files(file_directory, pattern = "^cataract_mtkl_1000_199[0-9].csv$|^cataract_mtkl_1000_20[0-2][0-9].csv$", full.names = TRUE)
data_list <- list()
for (file_name in file_names) {
  print(file_name)
  data <- fread(file_name)
  data_list[[length(data_list) + 1]] <- data
}
sum_total <- rbindlist(data_list)
head(sum_total)
fwrite(sum_total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence/cataract_mtkl_1000.csv",row.names = FALSE)


##########calculate the age-accumulated radiation
library(data.table)
library(foreach)
library(doParallel)
no_cores <- 25
registerDoParallel(cores = no_cores)
merged_data <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_cataract_nocontrol.csv")
earliest_year <- min(merged_data$year)
latest_year <- max(merged_data$year)
setDT(merged_data)
# Define the year and age range in advance
years <- 1880:2100
max_age <- 100

# Cycle through each year
foreach (current_year = years) %dopar%{
  #current_year <- 1891
  print(current_year)
  results <- data.table(lat = numeric(0), lon = numeric(0), sum_age = numeric(0), age = integer(0), year = integer(0))
  # Filter data for all relevant years
  relevant_years <- merged_data[year <= current_year & year >= max(1880, current_year - max_age + 1),]
  # Calculate the cumulative amount of radiation at each location and each age
  for (age in 1:min(max_age, current_year - 1879)) {
    year_of_age = current_year - age + 1
    sum_age_data <- relevant_years[year >= year_of_age, .(sum_age = sum(sum_y)), by = .(lat, lon)]
    sum_age_data[, `:=`(age = age, year = current_year)]
    results <- rbindlist(list(results, sum_age_data), use.names = TRUE, fill = TRUE)
  }
  setorder(results, lat, lon, -age)
  fwrite(results, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/age_sum_cataract_nocontrol_%d.csv", current_year), row.names = FALSE)
}

file_path <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/"
years <- 1980:2100
file_names <- paste0("age_sum_cataract_nocontrol_", years, ".csv")
full_paths <- file.path(file_path, file_names)
ra <- data.frame()
for (i in seq_along(full_paths)) {
  cat("Processing year:", years[i], "\n")  # 打印当前处理的年份
  temp_data <- read.csv(full_paths[i])
  ra <- rbind(ra, temp_data)
}
head(ra)
# Function that returns the corresponding age group based on age
get_age_group <- function(age) {
  if (age <= 4) {
    return("1-4 years")
  } else if (age <= 9) {
    return("5-9 years")
  } else if (age <= 14) {
    return("10-14 years")
  } else if (age <= 19) {
    return("15-19 years")
  } else if (age <= 24) {
    return("20-24 years")
  } else if (age <= 29) {
    return("25-29 years")
  } else if (age <= 34) {
    return("30-34 years")
  } else if (age <= 39) {
    return("35-39 years")
  } else if (age <= 44) {
    return("40-44 years")
  } else if (age <= 49) {
    return("45-49 years")
  } else if (age <= 54) {
    return("50-54 years")
  } else if (age <= 59) {
    return("55-59 years")
  } else if (age <= 64) {
    return("60-64 years")
  } else if (age <= 69) {
    return("65-69 years")
  } else if (age <= 74) {
    return("70-74 years")
  } else if (age <= 79) {
    return("75-79 years")
  } else if (age <= 84) {
    return("80-84 years")
  } else if (age <= 89) {
    return("85-89 years")
  } else if (age <= 94) {
    return("90-94 years")
  } else {
    return("95+ years")
  }
}
setDT(ra)
ra[, age_name := sapply(age, get_age_group)]
ra_mean <- ra %>%
  group_by(lat,lon,year,age_name) %>%
  summarise(ra_mean = mean(sum_age, na.rm = TRUE))
ra_mean <- data.frame(ra_mean)
fwrite(ra_mean,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/radiation_cataract_1980_2100_nocontrol.csv")

#########################################c in the cataract
library(data.table)

#cataract
c_mean <- 0.17
c_sd <- 0.09 / 1.96

# set up simulations
n_simulations <- 3000
set.seed(123)  
c_simulations <- rnorm(n_simulations, mean = c_mean, sd = c_sd)
# merge c_simulations and d_simulations
simulation_df <- data.frame(
  simulation = 1:n_simulations,
  c = c_simulations
)
head(simulation_df)
fwrite(simulation_df, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_cataract.csv"), row.names = FALSE)


library(data.table)
library(dplyr)
ra <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/radiation_cataract_1980_2100.csv")
ra_90_21<-ra[ra$year>=1990&ra$year<=2021,]
ra_90_21$ra_mj <- ra_90_21$ra_mean*3600/10^6
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra_90_21 <- merge(ra_90_21,country,by=c("lon","lat"))
library(dplyr)
library(data.table)
library(triangle)
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid.csv")
pop2 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2022_2100_grid.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop2 <- pop2[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1,pop2)
pop$lat <- round(pop$lat * 2) / 2
# Round the fourth decimal place
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
pop_long
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex
ra_ <- merge(ra_90_21,pop_long,by=c("lon","lat","country","year","age_name"))
ra_$ra_mj_weighted <- ra_$ra_mj*ra_$pop_pro
ra_per_country_per_age_per_sex <- ra_ %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(ra_mj_weighted_total = sum(ra_mj_weighted, na.rm = TRUE))
ra_per_country_per_age_per_sex<- data.frame(ra_per_country_per_age_per_sex)

m <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence/cataract_mtkl_1000.csv")
setDT(ra_per_country_per_age_per_sex)
setDT(m)
# merge data
merged_data <- merge(ra_per_country_per_age_per_sex, m, by = c("country", "year", "sex_name", "age_name"))

c <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_cataract.csv")
colnames(c)[which(names(c) == "simulation")] <- "simulations"
merged_data <- merge(merged_data,c,by=c("simulations"))
merged_data <- merged_data %>%
  mutate(ra_mj_weighted_total_powered = ra_mj_weighted_total ^ c)
# 计算列b
merged_data[, b := incidence_permillion_sample / ra_mj_weighted_total_powered]
fwrite(merged_data,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/b_cataract_1000_new.csv",row.names = FALSE)

merged_data <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/b_cataract_1000.csv")
result <- merged_data[, {
  if (.N >= 1) {
    sampled_b <- sample(b, size = 3000, replace = TRUE) 
    data.table(
      b = sampled_b,
      simulation = 1:3000
    )
  } else {
    NULL
  }
}, by = .(country, age_name, sex_name)]
head(result)
fwrite(result,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/extract_b_cataract_3000_new.csv",row.names = FALSE)


###################################################################calculated the b of the optimal estimation
###incidence
library(dplyr)
library(data.table)
library(triangle)
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1)
pop$lat <- round(pop$lat * 2) / 2
# Round the fourth decimal place
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
pop_long
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex

gbd <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/gbd_1980_2021.csv")
unique(gbd$cause_name)
gbd_ <- gbd[gbd$measure_name=="Deaths"&gbd$cause_name=="Malignant skin melanoma"&gbd$age_name!="All ages",]
colnames(gbd_)[which(names(gbd_) == "location_name")] <- "country"
total <- merge(pop_long,gbd_, by=c("country","year","age_name","sex_name"))
total <- data.frame(total)
total <- total[total$val!=0,]
total$incidence_val <- total$val/total$pop_country_age_sex
total$incidence_permillion_val <- total$incidence_val*10^6
total$incidence_lower <- total$lower/total$pop_country_age_sex
total$incidence_permillion_lower <- total$incidence_lower*10^6
total$incidence_upper <- total$upper/total$pop_country_age_sex
total$incidence_permillion_upper <- total$incidence_upper*10^6
total_ <- total %>%
  select(country,year, sex_name,age_name, incidence_permillion_val,incidence_permillion_lower,incidence_permillion_upper) %>%
  distinct()
fwrite(total_,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal estimation/death_m.csv")

library(dplyr)
library(data.table)
library(triangle)
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1)
pop$lat <- round(pop$lat * 2) / 2
# Round the fourth decimal place
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
pop_long
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex

gbd <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/gbd_cataract_1990_2021.csv")
unique(gbd$cause_name)
gbd_ <- gbd[gbd$age_name!="All ages",]
colnames(gbd_)[which(names(gbd_) == "location_name")] <- "country"
total <- merge(pop_long,gbd_, by=c("country","year","age_name","sex_name"))
total <- data.frame(total)
total <- total[total$val!=0,]
total$incidence_val <- total$val/total$pop_country_age_sex
total$incidence_permillion_val <- total$incidence_val*10^6
total$incidence_lower <- total$lower/total$pop_country_age_sex
total$incidence_permillion_lower <- total$incidence_lower*10^6
total$incidence_upper <- total$upper/total$pop_country_age_sex
total$incidence_permillion_upper <- total$incidence_upper*10^6
total_ <- total %>%
  select(country,year, sex_name,age_name, incidence_permillion_val,incidence_permillion_lower,incidence_permillion_upper) %>%
  distinct()
fwrite(total_,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal estimation/daly_cataract.csv")

####radiation
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
no_cores <- 15
registerDoParallel(cores = no_cores)
ra_annual <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_NMSC.csv")
ra_annual$sum_y_mj <- ra_annual$sum_y * 3600 / 10^6
unique_coords <- unique(ra_annual[, .(lon, lat)])
unique_coords <- as.data.frame(unique_coords)
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra_annual <- merge(ra_annual,country,by=c("lon","lat"))
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro_perage.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid_perage.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1)
pop$lat <- round(pop$lat * 2) / 2
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex

#function to get the age groups
get_age_group <- function(age) {
  if (age <= 4) {
    "1-4 years"
  } else if (age >= 95) {
    "95+ years"
  } else {
    lower_bound <- 5 * ((age - 5) %/% 5) + 5
    upper_bound <- lower_bound + 4
    age_range <- paste(lower_bound, upper_bound, sep="-")
    paste(age_range, "years", sep=" ")
  }
}

#function to calculate the radiation in current year
calculate_dose_a <- function(a, Dose) {
  Dose <- Dose[1:a]
  for (x in 0:(a - 1)) {
    dose_a <- Dose[x + 1]
  }
  return(dose_a)
}
# function to calculate the Y
calculate_Y_BCC_CM_weighted <- function(dose_a_weighted_total, c, d) {
  sum_val <- 0
  for (x in 1:(length(dose_a_weighted_total))) {
    phi_a <- sum(dose_a_weighted_total[1:(x)])
    sum_val <- sum_val + dose_a_weighted_total[x] *phi_a* ((length(dose_a_weighted_total) - x)^(d - c))
  }
  return(sum_val)
}

# Set the uncertainty ranges for c and d
c_mean <-1.4
#c_sd <- 0.4 / 1.96
d_mean <- 4.9
#d_sd <- 1 / 1.96
# Set the times of simulation
n_simulations <- 1

#The year_ ranges from 1990 to 2021
for (year_ in 1990:2021) {
  #year_ <- 1990
  print(paste("Processing year:", year_))
  ra_annual_filtered <- ra_annual[ra_annual$year >= (year_ - 100) & ra_annual$year <= (year_ - 1), ]
  results <- foreach(coord = iter(unique_coords, by = 'row'), .combine = rbind, .packages = 'data.table') %dopar% {
    lon_ <- coord[["lon"]]
    lat_ <- coord[["lat"]]
    df_all <- ra_annual_filtered[lon == lon_ & lat == lat_, ]
    setorder(df_all, -year)
    Dose <- as.vector(df_all$sum_y_mj)
    df <- data.frame(lat = numeric(0), lon = numeric(0), age = numeric(0), dose_a = numeric(0), phi_a = numeric(0))
    for (age in 1:100) {
      dose_a <- calculate_dose_a(age, Dose)
      result <- data.frame(lat = lat_, lon = lon_, age = age, dose_a = dose_a)
      df <- rbind(df, result)
    }
    return(df)
  }
  results <- na.omit(results)
  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total <- merge(country,results,by=c("lat","lon"))
  total$year_history <- year_-total$age
  colnames(total)[which(names(total) == "age")] <- "age_name"
  class(pop_long$age_name)
  pop_long$age_name <- as.numeric(pop_long$age_name)
  pop_long <- na.omit(pop_long)
  total$year <- year_
  total <- merge(total,pop_long,by=c("lat","lon","year","age_name","country"))
  total$dose_a_weighted <- total$dose_a*total$pop_pro
  ra_per_country_per_age_per_sex <- total %>%
    group_by(country,year_history,age_name,sex_name) %>%
    summarise(dose_a_weighted_total = sum(dose_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex <- data.frame(ra_per_country_per_age_per_sex)
  total <- merge(total,ra_per_country_per_age_per_sex,by=c("country","year_history","age_name","sex_name"))
  total <- na.omit(total)
  # Filter the combinations in the total dataframe
  unique_combinations <- unique(total[, .(country, year, age_name, sex_name)])
  unique_combinations <- na.omit(unique_combinations)
  # Monte Carlo simulation and Y (uncorrected) calculation were performed
  simulation_results <- foreach(row = 1:nrow(unique_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- unique_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total[total$country == country_ & total$year == year_ & total$age_name <= age_name_ & total$sex_name == sex_name_, ]
    group <- group %>%
      select(country, year,year_history, age_name, sex_name, dose_a_weighted_total) %>%
      distinct()
    dose_a_weighted_total <- unlist(group$dose_a_weighted_total)
    simulations <- data.frame(simulation = 1:n_simulations, Y_BCC_CM = NA)
    for (sim in 1:n_simulations) {
      # c_sim <- rnorm(1, c_mean, c_sd)
      # d_sim <- rnorm(1, d_mean, d_sd)
      c_sim <- c_mean
      d_sim <- d_mean
      age <- as.numeric(age_name_)
      simulations$Y_BCC_CM[sim] <- calculate_Y_BCC_CM_weighted(dose_a_weighted_total, c_sim, d_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    simulations
  }
  simulation_results<- data.frame(simulation_results)
  colnames(simulation_results)[which(names(simulation_results) == "age_name")] <- "age"
  simulation_results_ <- simulation_results %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_BCC_CM = mean(Y_BCC_CM, na.rm = TRUE))
  fwrite(simulation_results_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal estimation/new_sum_bcc_mj_mtkl_1000_combined_%d.csv", year_), row.names = FALSE)
}
stopImplicitCluster()

#scc
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
no_cores <- 12
registerDoParallel(cores = no_cores)
ra_annual <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_NMSC.csv")
ra_annual$sum_y_mj <- ra_annual$sum_y * 3600 / 10^6
unique_coords <- unique(ra_annual[, .(lon, lat)])
unique_coords <- as.data.frame(unique_coords)
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra_annual <- merge(ra_annual,country,by=c("lon","lat"))
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro_perage.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid_perage.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1)
pop$lat <- round(pop$lat * 2) / 2
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex

get_age_group <- function(age) {
  if (age <= 4) {
    "1-4 years"
  } else if (age >= 95) {
    "95+ years"
  } else {
    lower_bound <- 5 * ((age - 5) %/% 5) + 5
    upper_bound <- lower_bound + 4
    age_range <- paste(lower_bound, upper_bound, sep="-")
    paste(age_range, "years", sep=" ")
  }
}

# function to calculate the age accumulated radiation
calculate_phi_a <- function(a, Dose) {
  Dose <- Dose[1:a]
  Dose <- rev(Dose)
  for (x in 0:(a - 1)) {
    phi_a <- sum(Dose[1:(x + 1)])
  }
  return(phi_a)
}

# function to calculate the Y
calculate_Y_SCC_weighted <- function(phi_a_weighted_total, age, c, d) {
  sum_val <- 0
  for (x in 0:(age - 1)) {
    sum_val <- (phi_a_weighted_total^(c)) * ((age)^(d-c))
  }
  return(sum_val)
}

# Set the uncertainty ranges for c and d
c_mean <-2.5
#c_sd <- 0.7 / 1.96
d_mean <- 6.6
#d_sd <- 0.4 / 1.96
# Set the times of simulation
n_simulations <- 1

#The year_ ranges from 1990 to 2021
for (year_ in 1999:2006) {
  print(paste("Processing year:", year_))
  ra_annual_filtered <- ra_annual[ra_annual$year >= (year_ - 100) & ra_annual$year <= (year_ - 1), ]
  results <- foreach(coord = iter(unique_coords, by = 'row'), .combine = rbind, .packages = 'data.table') %dopar% {
    #print(coord)
    lon_ <- coord[["lon"]]
    lat_ <- coord[["lat"]]
    df_all <- ra_annual_filtered[lon == lon_ & lat == lat_, ]
    setorder(df_all, -year)
    Dose <- as.vector(df_all$sum_y_mj)
    df <- data.frame(lat = numeric(0), lon = numeric(0), age = numeric(0), phi_a = numeric(0))
    for (age in 1:100) {
      phi_a <- calculate_phi_a(age, Dose)
      result <- data.frame(lat = lat_, lon = lon_, age = age, phi_a = phi_a)
      df <- rbind(df, result)
    }
    return(df)
  }
  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total <- merge(country,results,by=c("lat","lon"))
  total$year <- year_
  colnames(total)[which(names(total) == "age")] <- "age_name"
  class(pop_long$age_name)
  pop_long$age_name <- as.numeric(pop_long$age_name)
  pop_long <- na.omit(pop_long)
  total <- merge(total,pop_long,by=c("lat","lon","year","age_name","country"))
  total$phi_a_weighted <- total$phi_a*total$pop_pro
  ra_per_country_per_age_per_sex <- total %>%
    group_by(country,year,age_name,sex_name) %>%
    summarise(phi_a_weighted_total = sum(phi_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex <- data.frame(ra_per_country_per_age_per_sex)
  total <- merge(total,ra_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
  total <- na.omit(total)
  # Filter the combinations in the total dataframe
  unique_combinations <- unique(total[, .(country, year, age_name, sex_name)])
  unique_combinations <- na.omit(unique_combinations)
  # Monte Carlo simulation and Y (uncorrected) calculation were performed
  simulation_results <- foreach(row = 1:nrow(unique_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- unique_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total[total$country == country_ & total$year == year_ & total$age_name == age_name_ & total$sex_name == sex_name_, ]
    phi_a_weighted_total <- group$phi_a_weighted_total[1]
    simulations <- data.frame(simulation = 1:n_simulations, Y_SCC = NA)
    for (sim in 1:n_simulations) {
      # c_sim <- rnorm(1, c_mean, c_sd)
      # d_sim <- rnorm(1, d_mean, d_sd)
      c_sim <- c_mean
      d_sim <- d_mean
      age <- as.numeric(age_name_)
      simulations$Y_SCC[sim] <- calculate_Y_SCC_weighted(phi_a_weighted_total, age, c_sim, d_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    simulations
  }
  simulation_results<- data.frame(simulation_results)
  colnames(simulation_results)[which(names(simulation_results) == "age_name")] <- "age"
  simulation_results_ <- simulation_results %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_SCC = mean(Y_SCC, na.rm = TRUE))
  fwrite(simulation_results_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal estimation/new_sum_scc_mj_mtkl_1000_combined_%d.csv", year_), row.names = FALSE)
}
stopImplicitCluster()

######################Merge into one large dataframe (country and year may need to be split due to large size of this dataframe)
library(data.table)
folder_path <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal estimation/"
combined_data <- data.table()
for (year in 1990:2021) {
  file_name <- paste0("new_sum_scc_mj_mtkl_1000_combined_", year, ".csv")
  file_path <- file.path(folder_path, file_name)
  year_data <- fread(file_path)
  combined_data <- rbindlist(list(combined_data, year_data), use.names = TRUE, fill = TRUE)
}
output_file <- file.path(folder_path, "ra_mtkl_scc.csv")
fwrite(combined_data, output_file, row.names = FALSE)

##########calculate the b
library(data.table)
ra <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal estimation/ra_mtkl_scc.csv")
inc <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal estimation/incidence_scc.csv")
total <- merge(inc,ra,by=c("year","country","age_name","sex_name"))
#total$b <- total$incidence_permillion_val/total$average_Y_BCC_CM
total$b <- total$incidence_permillion_val/total$average_Y_SCC
setDT(total)
result <- total[, .(mean_b = mean(b, na.rm = TRUE)), by = .(country, age_name, sex_name)]
head(result)
fwrite(result,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal estimation/b_scc_oe.csv")

#############calculate the b for mortality and cataract
library(data.table)
library(dplyr)
ra <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/radiation_b_s_1980_2100.csv")
ra_90_21<-ra[ra$year>=1990&ra$year<=2021,]
ra_90_21$ra_mj <- ra_90_21$ra_mean*3600/10^6
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra_90_21 <- merge(ra_90_21,country,by=c("lon","lat"))
library(dplyr)
library(data.table)
library(triangle)
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid.csv")
pop2 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2022_2100_grid.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop2 <- pop2[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1,pop2)
pop$lat <- round(pop$lat * 2) / 2
# Round the fourth decimal place
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
pop_long
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex

ra_ <- merge(ra_90_21,pop_long,by=c("lon","lat","country","year","age_name"))
ra_$ra_mj_weighted <- ra_$ra_mj*ra_$pop_pro
ra_per_country_per_age_per_sex <- ra_ %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(ra_mj_weighted_total = sum(ra_mj_weighted, na.rm = TRUE))
ra_per_country_per_age_per_sex<- data.frame(ra_per_country_per_age_per_sex)

m <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/death_scc.csv")
setDT(ra_per_country_per_age_per_sex)
setDT(m)
merged_data <- merge(ra_per_country_per_age_per_sex, m, by = c("country", "year", "sex_name", "age_name"))
# Add a column c and assign a value based on the value of sex_name
merged_data <- merged_data %>%
  mutate(c = if_else(sex_name == "Male", 0.71, 0.46))
  #mutate(c = if_else(sex_name == "Male", 0.58, 0.5))
merged_data$ra_mj_weighted_total_powered <- merged_data$ra_mj_weighted_total^merged_data$c
# 计算列b
merged_data[, b := incidence_permillion_val / ra_mj_weighted_total_powered]
result <- merged_data[, .(mean_b = mean(b, na.rm = TRUE)), by = .(country, age_name, sex_name)]
fwrite(result,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/b_scc_de_oe.csv",row.names = FALSE)
#fwrite(result,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/b_m_de_oe.csv",row.names = FALSE)


library(data.table)
library(dplyr)
ra <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/radiation_cataract_1980_2100.csv")
ra_90_21<-ra[ra$year>=1990&ra$year<=2021,]
ra_90_21$ra_mj <- ra_90_21$ra_mean*3600/10^6
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra_90_21 <- merge(ra_90_21,country,by=c("lon","lat"))
library(dplyr)
library(data.table)
library(triangle)
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid.csv")
pop2 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2022_2100_grid.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop2 <- pop2[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1,pop2)
pop$lat <- round(pop$lat * 2) / 2
# Round the fourth decimal place
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
pop_long
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex
ra_ <- merge(ra_90_21,pop_long,by=c("lon","lat","country","year","age_name"))
ra_$ra_mj_weighted <- ra_$ra_mj*ra_$pop_pro
ra_per_country_per_age_per_sex <- ra_ %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(ra_mj_weighted_total = sum(ra_mj_weighted, na.rm = TRUE))
ra_per_country_per_age_per_sex<- data.frame(ra_per_country_per_age_per_sex)

m <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/daly_cataract.csv")
# 确保数据是data.table格式
setDT(ra_per_country_per_age_per_sex)
setDT(m)
# 合并数据
merged_data <- merge(ra_per_country_per_age_per_sex, m, by = c("country", "year", "sex_name", "age_name"))
merged_data$c <- 0.17
merged_data$ra_mj_weighted_total_powered <- merged_data$ra_mj_weighted_total^merged_data$c
# 计算列b
merged_data[, b := incidence_permillion_val / ra_mj_weighted_total_powered]
result <- merged_data[, .(mean_b = mean(b, na.rm = TRUE)), by = .(country, age_name, sex_name)]
fwrite(result,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/b_cataract_oe.csv",row.names = FALSE)