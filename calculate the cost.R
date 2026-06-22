################################process the life expectancy data
library(data.table)
l1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/IHME-GBD_2021_DATA-3476279a-1/IHME-GBD_2021_DATA-3476279a-1.csv")
l2 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/IHME-GBD_2021_DATA-3476279a-2/IHME-GBD_2021_DATA-3476279a-2.csv")
l3 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/IHME-GBD_2021_DATA-3476279a-3/IHME-GBD_2021_DATA-3476279a-3.csv")
m_total_1 <- rbind(l1,l2,l3)
unique(m_total_1$location_name)

library(data.table)
library(dplyr)
file_path <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/"
m_total <- data.frame(measure_id=numeric(),
                      measure_name=character(),
                      location_id=numeric(),
                      location_name=character(),
                      sex_id=numeric(),
                      sex_name=character(),
                      age_id=numeric(),
                      age_name=character(),
                      metric_id=numeric(),
                      metric_name=character(),
                      cause_name=character(),
                      year=integer(),
                      val=numeric(),
                      upper=numeric(),
                      lower=numeric(),
                      stringsAsFactors = FALSE)
for (year in 1980:1989) {
  #year <- 1980
  file_name <- paste0(file_path, year, ".csv")
  data <- read.csv(file_name)
  m_total <- rbind(m_total,data)
}
unique(m_total$year)
m_total <- rbind(m_total,m_total_1)

m_total$location_name[m_total$location_name == "Dem. People's Republic of Korea"] <- "Dem. Rep. Korea"
m_total$location_name[m_total$location_name == "Viet Nam"] <- "Vietnam"
m_total$location_name[m_total$location_name == "Micronesia (Fed. States of)"] <- "Micronesia"
m_total$location_name[m_total$location_name == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
m_total$location_name[m_total$location_name == "Solomon Islands"] <- "Solomon Is."
m_total$location_name[m_total$location_name == "Lao People's Democratic Republic"] <- "Laos"
m_total$location_name[m_total$location_name == "Marshall Islands"] <- "Marshall Is."
m_total$location_name[m_total$location_name == "Brunei Darussalam"] <- "Brunei"
m_total$location_name[m_total$location_name == "Republic of Moldova"] <- "Moldova"
m_total$location_name[m_total$location_name == "Russian Federation"] <- "Russia"
m_total$location_name[m_total$location_name == "Antigua and Barbuda"] <- "Antigua and Barb."
m_total$location_name[m_total$location_name == "Dominican Republic"] <- "Dominican Rep."
m_total$location_name[m_total$location_name == "United States of America"] <- "United States"
m_total$location_name[m_total$location_name == "Bolivia (Plurinational State of)"] <- "Bolivia"
m_total$location_name[m_total$location_name == "Saint Vincent and the Grenadines"] <- "St. Vin. and Gren."
m_total$location_name[m_total$location_name == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
m_total$location_name[m_total$location_name == "Syrian Arab Republic"] <- "Syria"
m_total$location_name[m_total$location_name == "State of Palestine"] <- "Palestine"
m_total$location_name[m_total$location_name == "Congo"] <- "Republic of the Congo"
m_total$location_name[m_total$location_name == "Iran (Islamic Republic of)"] <- "Iran"
m_total$location_name[m_total$location_name == "Türkiye"] <- "Turkey"
m_total$location_name[m_total$location_name == "Equatorial Guinea"] <- "Eq. Guinea"
m_total$location_name[m_total$location_name == "Central African Republic"] <- "Central African Rep."
m_total$location_name[m_total$location_name == "United Republic of Tanzania"] <- "Tanzania"
m_total$location_name[m_total$location_name == "Eswatini"] <- "eSwatini"
m_total$location_name[m_total$location_name == "Cook Islands"] <- "Cook Is."
m_total$location_name[m_total$location_name == "United States Virgin Islands"] <- "U.S. Virgin Is."
m_total$location_name[m_total$location_name == "South Sudan"] <- "S. Sudan"
m_total$location_name[m_total$location_name == "Northern Mariana Islands"] <- "N. Mariana Is."
m_total$location_name[m_total$location_name == "Faroe Islands"] <- "Faeroe Islands"
m_total$location_name[m_total$location_name == "British Virgin Islands"] <- "British Virgin Is."
m_total$location_name[m_total$location_name == "Cayman Islands"] <- "Cayman Is."
m_total$location_name[m_total$location_name == "Saint Barthélemy"] <- "St-Barthélemy"
m_total$location_name[m_total$location_name == "Saint Martin (French part)"] <- "Saint-Martin"
m_total$location_name[m_total$location_name == "Sint Maarten (Dutch part)"] <- "Sint Maarten"
m_total$location_name[m_total$location_name == "Turks and Caicos Islands"] <- "Turks and Caicos Is."
m_total$location_name[m_total$location_name == "Falkland Islands (Malvinas)"] <- "Falkland Is."
m_total$location_name[m_total$location_name == "Saint Pierre and Miquelon"] <- "St. Pierre and Miquelon"
m_total$location_name[m_total$location_name == "French Polynesia"] <- "Fr. Polynesia"
m_total$location_name[m_total$location_name == "Western Sahara"] <- "W. Sahara"
m_total$location_name[m_total$location_name == "Democratic People's Republic of Korea"] <- "Dem. Rep. Korea"
m_total$location_name[m_total$location_name == "Micronesia (Federated States of)"] <- "Micronesia"

m_de_wmo <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/m_de_results/m_de_wmo_1000_1980_2100.csv")
de_country <- unique(m_de_wmo$country)
m_total_location_name <- unique(m_total$location_name)
m_total_location_name <- sort(m_total_location_name)

unique_m <- setdiff(m_total_location_name, de_country)
unique_de <- setdiff(de_country, m_total_location_name)

pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid.csv")
pop2 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2022_2100_grid.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop2 <- pop2[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1,pop2)
# Adjust the lat column to the nearest 0.5 degree
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

unique_pop <- pop_long %>%
  select(country, year, age_name, sex_name, pop_country_age_sex) %>%
  distinct()
head(unique_pop)
unique_pop$pop_country_age_sex_permillion <- unique_pop$pop_country_age_sex/10^6

m_de_wmo <- merge(unique_pop,m_de_wmo, by=c("country","year","age_name","sex_name"))
m_de_wmo$number <- m_de_wmo$Y_value*m_de_wmo$pop_country_age_sex_permillion
m_total
unique(m_total$age_name)
m_total$age_name <- gsub("^80-84$", "80-84 years", m_total$age_name)
m_total$age_name <- gsub("^85-89$", "85-89 years", m_total$age_name)
m_total$age_name <- gsub("^90-94$", "90-94 years", m_total$age_name)
m_total[m_total$age_name=="6-11 months",]
m_total[m_total$age_name=="12-23 months",]
m_total[m_total$age_name=="2-4 years",]
unique(m_de_wmo$age_name)
setDT(m_total)
age_groups <- c("12-23 months", "2-4 years")
avg_data <- m_total[age_name %in% age_groups, .(
  val = mean(val, na.rm = TRUE),
  upper = mean(upper, na.rm = TRUE),
  lower = mean(lower, na.rm = TRUE)
), by = .(measure_id, measure_name, location_id, location_name, sex_id, sex_name, metric_id, metric_name, year)]
avg_data[, age_name := "1-4 years"]
avg_data
age_groups_to_remove <- c("12-23 months", "2-4 years")
m_total <- m_total[!age_name %in% age_groups_to_remove]
m_total <- m_total[,c("measure_id", "measure_name", "location_id", "location_name", "sex_id", "sex_name", "metric_id", "metric_name", "year", "val", "upper", "lower", "age_name")]
m_total <- rbind(m_total, avg_data)
m_total
unique(m_total$age_name)
unique(m_total$year)
fwrite(m_total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/life_expectancy_80_21.csv",row.names = FALSE)

library(data.table)
library(dplyr)
m_total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/life_expectancy_80_21.csv")
#set the data after 2021 to be with the same as 2021
data_2021 <- m_total[year == 2021]
new_years <- 2022:2100
new_data <- data_2021[rep(seq_len(nrow(data_2021)), each = length(new_years)), ]
new_data[, year := rep(new_years, times = nrow(data_2021))]
m_total <- rbind(m_total, new_data)
setorder(m_total, location_name, sex_name, age_name, year)
unique(m_total$year)
fwrite(m_total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/life_expectancy.csv",row.names = FALSE)

#cause the life expectancy data in GBD dataset has a uncertainty range, so need to perform Monte Carlo random sampling
library(data.table)
library(dplyr)
library(triangle)
m_total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/life_expectancy.csv")
m_total <- m_total[,c("location_name","sex_name","age_name","year","val","lower","upper")]
library(triangle)
m_total$age_name <- gsub("^80-84$", "80-84 years", m_total$age_name)
m_total$age_name <- gsub("^85-89$", "85-89 years", m_total$age_name)
m_total$age_name <- gsub("^90-94$", "90-94 years", m_total$age_name)
colnames(m_total)[colnames(m_total) == "location_name"] <- "country"
m_de_wmo <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/gbd_1980_2021.csv")
unique_country <- unique(m_de_wmo$country)
m_total <- m_total[m_total$country %in% unique_country, ]
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
no_cores <- 12
registerDoParallel(cores = no_cores)
# Define a normal distribution sampling function that avoids negative values by regeneration
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

foreach (i = 1980:2100) %dopar% {
  total_y <- m_total[m_total$year == i,]
  print(i)
  # Perform Monte Carlo sampling
  results <- total_y %>%
    rowwise() %>%
    do({
      # Print current combination info
      print(paste("Processing:", .$country, .$year, .$sex_name, .$age_name))
      mean <- .$val  # Use val as the mean
      z <- 1.96  # Z value for 95% confidence interval
      sd <- (.$upper - .$lower) / (2 * z)  # Calculate standard deviation from 95% CI
      # Print parameter info
      print(paste("mean:", mean, "sd:", sd))
      # Check parameter validity
      if (!is.na(mean) && !is.na(sd) && sd > 0) {
        # Attempt to generate normal distribution samples
        tryCatch({
          samples <- normal_sample(mean, sd, 3000)  # Call the updated sampling function
        }, error = function(e) {
          print(paste("Error in normal_sample:", e$message))
          samples <- rep(NA, 3000)
        })
      } else {
        print("Invalid parameters for normal distribution")
        samples <- rep(NA, 3000)
      }
      data.frame(
        country = .$country,
        year = .$year,
        sex_name = .$sex_name,
        age_name = .$age_name,
        life_ex_sample = samples,
        simulations = 1:3000
      )
    })
  # View results
  head(results)
  results <- data.frame(results)
  fwrite(results, file = sprintf(
    "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/life_ex_mtkl_3000_%d.csv", i),
    row.names = FALSE
  )
}



library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
no_cores <- 11
registerDoParallel(cores = no_cores)
# 进行蒙特卡洛模拟和Y的计算
results <- foreach(year_ = 2061:2080, .combine = rbind, .packages = c('dplyr')) %dopar% {
  #year_ <- 1980
  m <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/death_results/total_dea_control_%d.csv",year_))
  life <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/life_ex_mtkl_3000_%d.csv",year_))
  colnames(life)[which(names(life) == "simulations")] <- "simulation"
  total <- merge(m,life,by=c("year","country","age_name","sex_name","simulation"))
  total$dif_m <- total$number_value_m_nocontrol-total$number_value_m_wmo
  total$dif_scc <- total$number_value_scc_nocontrol-total$number_value_scc_wmo
  total$yll_m <- total$dif_m*total$life_ex_sample
  total$yll_scc <- total$dif_scc*total$life_ex_sample
  total <- total[,c("year","country","age_name","sex_name","simulation","life_ex_sample","dif_m","dif_scc","yll_m","yll_scc")]
  fwrite(total,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/yll_3000_origin_norandom_%d.csv",year_),row.names = FALSE)
  
}

############################################################################calculate the DALY and cost related to mortality
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
no_cores <- 11
registerDoParallel(cores = no_cores)
foreach(year_ = 2061:2100, .combine = rbind, .packages = c('dplyr')) %dopar% {
  #year_ <- 1980
  m_yll <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/yll_3000_origin_norandom_%d.csv",year_))
  m_yll$yld_m <- m_yll$yll_m/0.923*0.077
  m_yll$yld_scc <- m_yll$yll_scc/0.894*0.106
  m_yll$daly_m <- m_yll$yll_m+m_yll$yld_m
  m_yll$daly_scc <- m_yll$yll_scc+m_yll$yld_scc
  m_yll$daly = rowSums(m_yll[, c("daly_m", "daly_scc")], na.rm = TRUE)
  
  VSLY <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/VSLY_final.csv")
  colnames(VSLY)[which(names(VSLY) == "Country")] <- "country"
  VSLY$country[VSLY$country == "C?te d'Ivoire"] <- "Côte d'Ivoire"
  m_yll$country[m_yll$country == "Republic of the Congo"] <- "Congo"
  unique(VSLY$country)
  unique(m_yll$country)
  
  total <- merge(m_yll,VSLY,by=c("country"))
  total$cost_sc <- total$daly*total$VSLY
  total <- total[,c("year","country","age_name","sex_name","simulation","daly_m","daly_scc","daly","VSLY","cost_sc")]
  fwrite(total,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/vsly_cost_sc_3000_origin_norandom_%d.csv",year_),row.names = FALSE)
}


#########################################################calculate the DALY and cost related to cataract
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
no_cores <- 11
registerDoParallel(cores = no_cores)
foreach(year_ = 2061:2100, .combine = rbind, .packages = c('dplyr')) %dopar% {
  #year_ <- 1980
  m_yll <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cataract_total_results/cataract_total_control_%d.csv",year_))
  VSLY <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/VSLY_final.csv")
  colnames(VSLY)[which(names(VSLY) == "Country")] <- "country"
  VSLY$country[VSLY$country == "C?te d'Ivoire"] <- "Côte d'Ivoire"
  m_yll$country[m_yll$country == "Republic of the Congo"] <- "Congo"
  unique(VSLY$country)
  unique(m_yll$country)
  
  total <- merge(m_yll,VSLY,by=c("country"))
  total$cost_ca <- total$dif*total$VSLY
  total <- total[,c("year","country","age_name","sex_name","simulation","dif","VSLY","cost_ca")]
  fwrite(total,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/vsly_cost_ca_3000_origin_norandom_%d.csv",year_),row.names = FALSE)
}


###############################################direct costs avoided due to cataracts
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
no_cores <- 11
registerDoParallel(cores = no_cores)
foreach(year_ = 2061:2100, .combine = rbind, .packages = c('dplyr')) %dopar% {
  #year_ <- 1980
  m_yll <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cataract_total_results/cataract_total_control_%d.csv",year_))
  treat <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/cataract_treat.csv")
  treat$country[treat$country == "C?te d'Ivoire"] <- "Côte d'Ivoire"
  treat$country[treat$country == "Republic of the Congo"] <- "Congo"
  m_yll$country[m_yll$country == "Republic of the Congo"] <- "Congo"
  
  total <- merge(m_yll,treat,by=c("country"))
  unique(total$country)
  total$treat_ca <- total$dif*total$treat_2020
  total <- total[,c("year","country","age_name","sex_name","simulation","dif","treat_2020","treat_ca")]
  fwrite(total,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/treat_ca/treat_cost_ca_3000_origin_norandom_%d.csv",year_),row.names = FALSE)
}



############################################################################direct costs avoided due to skin cancer
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
no_cores <- 11
registerDoParallel(cores = no_cores)
foreach(year_ = 2061:2100, .combine = rbind, .packages = c('dplyr')) %dopar% {
  #year_ <- 1980
  m <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence_results/total_inc_control_%d.csv",year_))
  m$dif_m <- m$number_value_m_nocontrol-m$number_value_m_wmo
  m$dif_bcc <- m$number_value_bcc_nocontrol-m$number_value_bcc_wmo
  m$dif_scc <- m$number_value_scc_nocontrol-m$number_value_scc_wmo
  
  unit_cost <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/treat_cost/unit_cost_nouncertainty_final.csv")
  unit_cost <- unit_cost[unit_cost$type=="MM"]
  all_countries <- unique(m$country)
  colnames(unit_cost)[which(names(unit_cost) == "location")] <- "country"
  country_mm <- unique(unit_cost$country)
  filtered_countries <- all_countries[!all_countries %in% country_mm]
  world_data <- unit_cost %>% filter(country == "World")
  expanded_countries <- filtered_countries %>%
    lapply(function(country_name) {
      world_data %>%
        mutate(country = country_name)
    }) %>%
    bind_rows()
  unit_cost <- bind_rows(unit_cost, expanded_countries)
  print(unit_cost)
  unit_cost <- unit_cost[,c("country","mean_cost")]
  colnames(unit_cost)[which(names(unit_cost) == "mean_cost")] <- "mean_cost_m"
  total_cost <- merge(m,unit_cost,by=c("country"))
  total_cost$cost_m <- total_cost$dif_m*total_cost$mean_cost_m
  
  unit_cost <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/treat_cost/unit_cost_nouncertainty_final.csv")
  unit_cost <- unit_cost[unit_cost$type=="BCC and SCC"]
  colnames(unit_cost)[which(names(unit_cost) == "location")] <- "country"
  country_mm <- unique(unit_cost$country)
  filtered_countries <- all_countries[!all_countries %in% country_mm]
  world_data <- unit_cost %>% filter(country == "World")
  expanded_countries <- filtered_countries %>%
    lapply(function(country_name) {
      world_data %>%
        mutate(country = country_name)  
    }) %>%
    bind_rows()
  unit_cost <- bind_rows(unit_cost, expanded_countries)
  print(unit_cost)
  unit_cost <- unit_cost[,c("country","mean_cost")]
  colnames(unit_cost)[which(names(unit_cost) == "mean_cost")] <- "mean_cost_nm"
  total_cost <- merge(total_cost,unit_cost,by=c("country"))
  total_cost$cost_bcc <- total_cost$dif_bcc*total_cost$mean_cost_nm
  total_cost$cost_scc <- total_cost$dif_scc*total_cost$mean_cost_nm
  total_cost$cost_total = rowSums(total_cost[, c("cost_m", "cost_bcc", "cost_scc")], na.rm = TRUE)
  total_cost <- total_cost[,c("year","country","age_name","sex_name","simulation","cost_m","cost_bcc","cost_scc","cost_total")]
  
  fwrite(total_cost,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/treat_cost/treat_cost_3000_origin_norandom_%d.csv",year_),row.names = FALSE)
}



library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
no_cores <- 11
registerDoParallel(cores = no_cores)
foreach(year_ = 2061:2100, .combine = rbind, .packages = c('dplyr')) %dopar% {
  #year_ <- 1980
  treat <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/treat_cost/treat_cost_3000_origin_norandom_%d.csv",year_))
  treat_ca <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/treat_ca/treat_cost_ca_3000_origin_norandom_%d.csv",year_))
  vsly_sc <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/vsly_cost_sc_3000_origin_norandom_%d.csv",year_))
  vsly_ca <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/vsly_cost_ca_3000_origin_norandom_%d.csv",year_))
  treat_ca <- treat_ca[,c("year","country","age_name","sex_name","simulation","treat_ca")]
  treat <- treat[,c("year","country","age_name","sex_name","simulation","cost_total")]
  vsly_sc <- vsly_sc[,c("year","country","age_name","sex_name","simulation","cost_sc")]
  vsly_ca <- vsly_ca[,c("year","country","age_name","sex_name","simulation","cost_ca")]
  total <- merge(treat,vsly_sc,by=c("year","country","age_name","sex_name","simulation"),all.x=TRUE,all.y=TRUE)
  total <- merge(total,vsly_ca,by=c("year","country","age_name","sex_name","simulation"),all.x=TRUE,all.y=TRUE)
  total <- merge(total,treat_ca,by=c("year","country","age_name","sex_name","simulation"),all.x=TRUE,all.y=TRUE)
  colnames(total)[which(names(total) == "cost_total")] <- "cost_treat_total"
  total$cost_total = rowSums(total[, c("cost_treat_total","treat_ca", "cost_sc", "cost_ca")], na.rm = TRUE)
  fwrite(total,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/cost_total_3000_origin_norandom_%d.csv",year_),row.names = FALSE)
}


library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
cl <- makeCluster(12) 
registerDoParallel(cl)
years <- 1980:2100
input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/cost_total_3000_origin_norandom_"
output_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_norandom_"
foreach(year = years, .packages = c("data.table", "dplyr")) %dopar% {
  input_file <- paste0(input_path_template, year, ".csv")
  inc <- fread(input_file)
  result <- inc %>%
    group_by(year, simulation) %>%
    summarize(
      cost_sum = sum(cost_total, na.rm = TRUE) # 求和 dif
    )
  output_file <- paste0(output_path_template, year, ".csv")
  fwrite(result, file = output_file)
}
stopCluster(cl)


library(data.table)
years <- 1980:2100
input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_norandom_"
output_file <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_cost_norandom.csv"
year_sum_death <- data.table()
for (year in years) {
  input_file <- paste0(input_path_template, year, ".csv")
  if (file.exists(input_file)) {
    temp_data <- fread(input_file)
    year_sum_death <- rbind(year_sum_death, temp_data, fill = TRUE) # 合并数据
  } else {
    message(paste("File not found:", input_file))
  }
}
fwrite(year_sum_death, file = output_file)


year_sum <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_cost_norandom.csv")
library(dplyr)
result <- year_sum %>%
  group_by(simulation) %>%
  summarize(
    total_cost = sum(cost_sum, na.rm = TRUE),
  )
print(result)
quantiles <- quantile(result$total_cost, probs = c(0.025,0.5,0.975), na.rm = TRUE)
print(quantiles)




########################################################################calculate the optimal estimation of the cost
library(data.table)
library(dplyr)
life_ex <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/life_expectancy.csv")
life_ex <- life_ex[,c("location_name","sex_name","age_name","year","val","lower","upper")]
life_ex$age_name <- gsub("^80-84$", "80-84 years", life_ex$age_name)
life_ex$age_name <- gsub("^85-89$", "85-89 years", life_ex$age_name)
life_ex$age_name <- gsub("^90-94$", "90-94 years", life_ex$age_name)
unique(life_ex$age_name)
colnames(life_ex)[colnames(life_ex) == "location_name"] <- "country"
de <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_de_1980_2100.csv")
total <- merge(de,life_ex,by=c("year","country","age_name","sex_name"))
total$dif_m <- total$number_value_m_nocontrol-total$number_value_m_wmo
total$dif_scc <- total$number_value_scc_nocontrol-total$number_value_scc_wmo
total$yll_m <- total$dif_m*total$val
total$yll_scc <- total$dif_scc*total$val

total$yld_m <- total$yll_m/0.923*0.077
total$yld_scc <- total$yll_scc/0.894*0.106
total$daly_m <- total$yll_m+total$yld_m
total$daly_scc <- total$yll_scc+total$yld_scc
total$daly = rowSums(total[, c("daly_m", "daly_scc")], na.rm = TRUE)

VSLY <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/VSLY_final.csv")
colnames(VSLY)[which(names(VSLY) == "Country")] <- "country"
VSLY$country[VSLY$country == "C?te d'Ivoire"] <- "Côte d'Ivoire"
#total$country[total$country == "Republic of the Congo"] <- "Congo"
unique(VSLY$country)
unique(total$country)

total <- merge(total,VSLY,by=c("country"))
unique(total$country)
total$cost_sc <- total$daly*total$VSLY
total <- total[,c("year","country","age_name","sex_name","daly_m","daly_scc","daly","VSLY","cost_sc")]
fwrite(total,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/vsly_cost_sc_oe.csv"),row.names = FALSE)




library(data.table)
library(dplyr)
ca <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_cataract_daly_1980_2100.csv")

VSLY <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/VSLY_final.csv")
colnames(VSLY)[which(names(VSLY) == "Country")] <- "country"
VSLY$country[VSLY$country == "C?te d'Ivoire"] <- "Côte d'Ivoire"
#total$country[total$country == "Republic of the Congo"] <- "Congo"
unique(VSLY$country)

total <- merge(ca,VSLY,by=c("country"))
unique(total$country)
total$cost_ca <- total$dif*total$VSLY
total <- total[,c("year","country","age_name","sex_name","dif","VSLY","cost_ca")]
fwrite(total,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/vsly_cost_ca_oe.csv"),row.names = FALSE)


#########direct costs avoided due to cataracts
library(data.table)
library(dplyr)
ca <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_cataract_daly_1980_2100.csv")
treat <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cataract_treat.csv")
unique(ca$country)
treat$country[treat$country == "C?te d'Ivoire"] <- "Côte d'Ivoire"
treat$country[treat$country == "Republic of the Congo"] <- "Congo"
ca$country[ca$country == "Republic of the Congo"] <- "Congo"

total <- merge(ca,treat,by=c("country"))
unique(total$country)
total$treat_ca <- total$dif*total$treat_2020
total <- total[,c("year","country","age_name","sex_name","dif","treat_2020","treat_ca")]
fwrite(total,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/treat_ca_cost_oe.csv"),row.names = FALSE)


##############direct costs avoided due to cataracts
m <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_1980_2100.csv")
m$dif_m <- m$number_value_m_nocontrol-m$number_value_m_wmo
m$dif_bcc <- m$number_value_bcc_nocontrol-m$number_value_bcc_wmo
m$dif_scc <- m$number_value_scc_nocontrol-m$number_value_scc_wmo

unit_cost <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/unit_cost_nouncertainty_final.csv")
unit_cost <- unit_cost[unit_cost$type=="MM"]
all_countries <- unique(m$country)
colnames(unit_cost)[which(names(unit_cost) == "location")] <- "country"
country_mm <- unique(unit_cost$country)
filtered_countries <- all_countries[!all_countries %in% country_mm]
world_data <- unit_cost %>% filter(country == "World")
expanded_countries <- filtered_countries %>%
  lapply(function(country_name) {
    world_data %>%
      mutate(country = country_name) 
  }) %>%
  bind_rows()
unit_cost <- bind_rows(unit_cost, expanded_countries)
print(unit_cost)
unit_cost <- unit_cost[,c("country","mean_cost")]
colnames(unit_cost)[which(names(unit_cost) == "mean_cost")] <- "mean_cost_m"
total_cost <- merge(m,unit_cost,by=c("country"))
total_cost$cost_m <- total_cost$dif_m*total_cost$mean_cost_m

unit_cost <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/unit_cost_nouncertainty_final.csv")
unit_cost <- unit_cost[unit_cost$type=="BCC and SCC"]
colnames(unit_cost)[which(names(unit_cost) == "location")] <- "country"
country_mm <- unique(unit_cost$country)
filtered_countries <- all_countries[!all_countries %in% country_mm]
world_data <- unit_cost %>% filter(country == "World")
expanded_countries <- filtered_countries %>%
  lapply(function(country_name) {
    world_data %>%
      mutate(country = country_name)
  }) %>%
  bind_rows()
unit_cost <- bind_rows(unit_cost, expanded_countries)
print(unit_cost)
unit_cost <- unit_cost[,c("country","mean_cost")]
colnames(unit_cost)[which(names(unit_cost) == "mean_cost")] <- "mean_cost_nm"
total_cost <- merge(total_cost,unit_cost,by=c("country"))
total_cost$cost_bcc <- total_cost$dif_bcc*total_cost$mean_cost_nm
total_cost$cost_scc <- total_cost$dif_scc*total_cost$mean_cost_nm
total_cost$cost_total = rowSums(total_cost[, c("cost_m", "cost_bcc", "cost_scc")], na.rm = TRUE)
total_cost <- total_cost[,c("year","country","age_name","sex_name","cost_m","cost_bcc","cost_scc","cost_total")]

fwrite(total_cost,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/treat_cost_oe.csv"),row.names = FALSE)



#total costs
treat <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/treat_cost_oe.csv"))
vsly_sc <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/vsly_cost_sc_oe.csv"))
vsly_ca <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/vsly_cost_ca_oe.csv"))
treat_ca <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/treat_ca_cost_oe.csv"))
treat <- treat[,c("year","country","age_name","sex_name","cost_total")]
treat_ca <- treat_ca[,c("year","country","age_name","sex_name","treat_ca")]
vsly_sc <- vsly_sc[,c("year","country","age_name","sex_name","cost_sc")]
vsly_ca <- vsly_ca[,c("year","country","age_name","sex_name","cost_ca")]
total <- merge(treat,vsly_sc,by=c("year","country","age_name","sex_name"),all.x=TRUE,all.y=TRUE)
total <- merge(total,vsly_ca,by=c("year","country","age_name","sex_name"),all.x=TRUE,all.y=TRUE)
total <- merge(total,treat_ca,by=c("year","country","age_name","sex_name"),all.x=TRUE,all.y=TRUE)
colnames(total)[which(names(total) == "cost_total")] <- "cost_treat_total"
total$cost_total = rowSums(total[, c("cost_treat_total", "cost_sc", "cost_ca","treat_ca")], na.rm = TRUE)
fwrite(total,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv"),row.names = FALSE)
total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
sum(total$cost_total)
sum(total$treat_ca, na.rm = TRUE)
sum(total$cost_ca, na.rm = TRUE)
