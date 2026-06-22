##########################calculate the Y in any special year
##########################incidence
#MM
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro_perage.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid_perage.csv")
pop2 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2022_2100_grid_perage.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop2 <- pop2[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1)
pop <- rbind(pop,pop2)
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
for (year_ in 1980:2100) {
  pop_long_fil <- pop_long[pop_long$year <- year_]
  fwrite(pop_long_fil,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_long_%d.csv",year_), row.names = FALSE)
}

library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(broom)
library(purrr)
library(tidyr)
no_cores <- 35
registerDoParallel(cores = no_cores)

ra_annual <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual.csv")
ra_annual$sum_y_mj <- ra_annual$sum_y * 3600 / 10^6
unique_coords <- unique(ra_annual[, .(lon, lat)])
unique_coords <- as.data.frame(unique_coords)
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra_annual <- merge(ra_annual,country,by=c("lon","lat"))

for (year_ in 1980:2100) {
  results <- foreach(coord = iter(unique_coords, by = 'row'), .combine = rbind, .packages = 'data.table') %dopar% {
    print(coord)
    lon_ <- coord[["lon"]]
    lat_ <- coord[["lat"]]
    df_all <- ra_annual_filtered[lon == lon_ & lat == lat_, ]
    setorder(df_all, -year)
    Dose <- as.vector(df_all$sum_y_mj)
    df <- data.frame(lat = numeric(0), lon = numeric(0), age = numeric(0), dose_a = numeric(0))
    for (age in 1:100) {
      dose_a <- calculate_dose_a(age, Dose)
      result <- data.frame(lat = lat_, lon = lon_, age = age, dose_a = dose_a)
      df <- rbind(df, result)
    }
    return(df)
  }
  results <- na.omit(results)
  fwrite(results,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_m_wmo/results_%d.csv", year_), row.names = FALSE)
}



library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
no_cores <- 50
registerDoParallel(cores = no_cores)
library(dplyr)
library(data.table)

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

calculate_dose_a <- function(a, Dose) {
  Dose <- Dose[1:a]
  for (x in 0:(a - 1)) {
    dose_a <- Dose[x + 1]
  }
  return(dose_a)
}
calculate_Y_BCC_CM_weighted <- function(dose_a_weighted_total, c, d, b) {
  sum_val <- 0
  for (x in 1:(length(dose_a_weighted_total))) {
    phi_a <- sum(dose_a_weighted_total[1:(x)])
    sum_val <- sum_val + dose_a_weighted_total[x] *phi_a* ((length(dose_a_weighted_total) - x)^(d - c))
  }
  sum_val <- b*sum_val
  return(sum_val)
}

c_mean <- 0.6
c_sd <- 0.4 / 1.96
d_mean <- 4.7
d_sd <- 1 / 1.96
n_simulations <- 3000

country_list <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/extract_b_m_3000_new.csv")
countries <- unique(country_list$country)

for (year_ in 1980) {
  #year_ <- 1980
  print(paste("Processing year:", year_))
  results_list <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/extract_b_m_3000_new.csv")
  results <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_m_wmo/results_%d.csv",year_))
  pop_long <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_long_%d.csv",year_))
  results_no <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_m_nocontrol/results_%d.csv",year_))
  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total <- merge(country,results,by=c("lat","lon"))
  total$year <- year_
  total$year_history <- year_-total$age
  colnames(total)[which(names(total) == "age")] <- "age_name"
  total <- merge(total,pop_long,by=c("lat","lon","year","age_name","country"))
  total$dose_a_weighted <- total$dose_a*total$pop_pro
  #total$phi_a_weighted <- total$phi_a*total$pop_pro
  ra_per_country_per_age_per_sex <- total %>%
    group_by(country,year_history,age_name,sex_name) %>%
    summarise(dose_a_weighted_total = sum(dose_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex <- data.frame(ra_per_country_per_age_per_sex)
  total <- merge(total,ra_per_country_per_age_per_sex,by=c("country","year_history","age_name","sex_name"))
  total <- na.omit(total)
  # Use the filter function of dplyr to filter the total data frame
  total <- total %>%
    filter(country %in% countries)
  # Filter the combinations in the total data box
  unique_combinations <- unique(total[, .(country, year, age_name, sex_name)])
  unique_combinations <- na.omit(unique_combinations)
  unique_combinations <- unique_combinations %>%
    mutate(age_group = sapply(age_name, get_age_group))
  # Make sure the column names are consistent
  results_list <- results_list %>%
    rename(age_group = age_name)
  # Filter the unique values of country, age_group, sex_name in results_list
  unique_results <- results_list %>%
    distinct(country, age_group, sex_name)
  # Filter out the rows in unique_combinations that are consistent with unique_results
  filtered_combinations <- semi_join(unique_combinations, unique_results, by = c("country", "age_group", "sex_name"))
  
  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total_no <- merge(country,results_no,by=c("lat","lon"))
  total_no$year <- year_
  total_no$year_history <- year_-total_no$age
  colnames(total_no)[which(names(total_no) == "age")] <- "age_name"
  total_no <- merge(total_no,pop_long,by=c("lat","lon","year","age_name","country"))
  total_no$dose_a_weighted <- total_no$dose_a*total_no$pop_pro
  #total$phi_a_weighted <- total$phi_a*total$pop_pro
  ra_per_country_per_age_per_sex_no <- total_no %>%
    group_by(country,year_history,age_name,sex_name) %>%
    summarise(dose_a_weighted_total = sum(dose_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex_no <- data.frame(ra_per_country_per_age_per_sex_no)
  total_no <- merge(total_no,ra_per_country_per_age_per_sex_no,by=c("country","year_history","age_name","sex_name"))
  total_no <- na.omit(total_no)
  total_no <- total_no %>%
    filter(country %in% countries)
  
  simulation_results_no <- foreach(row = 1:nrow(filtered_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- filtered_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total_no[total_no$country == country_ & total_no$year == year_ & total_no$age_name <= age_name_ & total_no$sex_name == sex_name_, ]
    # Filter unique values
    group_ <- group %>%
      select(country, year,year_history, age_name, sex_name, dose_a_weighted_total) %>%
      distinct()
    dose_a_weighted_total <- unlist(group_$dose_a_weighted_total)
    age_group_ <- get_age_group(age_name_)
    filtered_data <- results_list %>%
      filter(country == country_, age_group == age_group_, sex_name == sex_name_)
    simulations <- data.frame(simulation = 1:n_simulations, Y_BCC_CM = NA)
    c_d <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_d_m.csv")
    for (sim in 1:n_simulations) {
      c_sim <- c_d %>% filter(simulation == sim) %>% pull(c)
      d_sim <- c_d %>% filter(simulation == sim) %>% pull(d)
      # Extract the b value corresponding to the current simulation from the filtered_data data frame
      b_sim <- filtered_data %>% filter(simulation == sim) %>% pull(b)
      age <- as.numeric(age_name_)
      simulations$Y_BCC_CM[sim] <- calculate_Y_BCC_CM_weighted(dose_a_weighted_total, c_sim, d_sim, b_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    return(simulations)
  }
  
  simulation_results <- foreach(row = 1:nrow(filtered_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- filtered_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total[total$country == country_ & total$year == year_ & total$age_name <= age_name_ & total$sex_name == sex_name_, ]
    # 筛选唯一值
    group_ <- group %>%
      select(country, year,year_history, age_name, sex_name, dose_a_weighted_total) %>%
      distinct()
    dose_a_weighted_total <- unlist(group_$dose_a_weighted_total)
    age_group_ <- get_age_group(age_name_)
    filtered_data <- results_list %>%
      filter(country == country_, age_group == age_group_, sex_name == sex_name_)
    simulations <- data.frame(simulation = 1:n_simulations, Y_BCC_CM = NA)
    c_d <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_d_m.csv")
    for (sim in 1:n_simulations) {
      c_sim <- c_d %>% filter(simulation == sim) %>% pull(c)
      d_sim <- c_d %>% filter(simulation == sim) %>% pull(d)
      b_sim <- filtered_data %>% filter(simulation == sim) %>% pull(b)
      age <- as.numeric(age_name_)
      simulations$Y_BCC_CM[sim] <- calculate_Y_BCC_CM_weighted(dose_a_weighted_total, c_sim, d_sim, b_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    return(simulations)
  }
  
  simulation_results<- data.frame(simulation_results)
  simulation_results <- na.omit(simulation_results)
  simulation_results_no<- data.frame(simulation_results_no)
  simulation_results_no <- na.omit(simulation_results_no)
  colnames(simulation_results_no)[which(names(simulation_results_no) == "Y_BCC_CM")] <- "Y_BCC_CM_nocontrol"
  
  colnames(simulation_results)[which(names(simulation_results) == "age_name")] <- "age"
  simulation_results_ <- simulation_results %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_BCC_CM = mean(Y_BCC_CM, na.rm = TRUE))
  
  colnames(simulation_results_no)[which(names(simulation_results_no) == "age_name")] <- "age"
  simulation_results_no_ <- simulation_results_no %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_BCC_CM_no = mean(Y_BCC_CM_nocontrol, na.rm = TRUE))
  
  fwrite(simulation_results_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/m_results/WMO/m_wmo_3000_origin_%d.csv", year_), row.names = FALSE)
  fwrite(simulation_results_no_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/m_results/Nocontrol/m_nocontrol_3000_origin_%d.csv", year_), row.names = FALSE)
  gc()
}
stopImplicitCluster()

##BCC
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(broom)
library(purrr)
library(tidyr)
no_cores <- 50
registerDoParallel(cores = no_cores)

ra_annual <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_NMSC_Nocontrol.csv")
ra_annual$sum_y_mj <- ra_annual$sum_y * 3600 / 10^6
unique_coords <- unique(ra_annual[, .(lon, lat)])
unique_coords <- as.data.frame(unique_coords)
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra_annual <- merge(ra_annual,country,by=c("lon","lat"))

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

calculate_dose_a <- function(a, Dose) {
  Dose <- Dose[1:a]
  for (x in 0:(a - 1)) {
    dose_a <- Dose[x + 1]
  }
  return(dose_a)
}

calculate_Y_BCC_CM_weighted <- function(dose_a_weighted_total, c, d, b) {
  sum_val <- 0
  for (x in 1:(length(dose_a_weighted_total))) {
    phi_a <- sum(dose_a_weighted_total[1:(x)])
    sum_val <- sum_val + dose_a_weighted_total[x] *phi_a* ((length(dose_a_weighted_total) - x)^(d - c))
  }
  sum_val <- b*sum_val
  return(sum_val)
}

c_mean <- 1.4
c_sd <- 0.4 / 1.96
d_mean <- 4.9
d_sd <- 0.6 / 1.96
n_simulations <- 3000

country_list <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/extract_b_scc_3000_new.csv")
countries <- unique(country_list$country)

for (year_ in 1980:2100) {
  results <- foreach(coord = iter(unique_coords, by = 'row'), .combine = rbind, .packages = 'data.table') %dopar% {
    print(coord)
    lon_ <- coord[["lon"]]
    lat_ <- coord[["lat"]]
    df_all <- ra_annual_filtered[lon == lon_ & lat == lat_, ]
    setorder(df_all, -year)
    Dose <- as.vector(df_all$sum_y_mj)
    df <- data.frame(lat = numeric(0), lon = numeric(0), age = numeric(0), dose_a = numeric(0))
    for (age in 1:100) {
      dose_a <- calculate_dose_a(age, Dose)
      result <- data.frame(lat = lat_, lon = lon_, age = age, dose_a = dose_a)
      df <- rbind(df, result)
    }
    return(df)
  }
  results <- na.omit(results)
  fwrite(results,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_b_s_wmo/results_%d.csv", year_), row.names = FALSE)
}

for (year_ in 2005) {
  #year_ <- 1980
  print(paste("Processing year:", year_))
  results_list <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/extract_b_bcc_3000_new.csv")
  results <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_b_s_wmo/results_%d.csv",year_))
  pop_long <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_long_%d.csv",year_))
  results_no <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results fo TUV/ra_annual_b_s_nocontrol/results_%d.csv",year_))
  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total <- merge(country,results,by=c("lat","lon"))
  total$year <- year_
  total$year_history <- year_-total$age
  colnames(total)[which(names(total) == "age")] <- "age_name"
  total <- merge(total,pop_long,by=c("lat","lon","year","age_name","country"))
  total$dose_a_weighted <- total$dose_a*total$pop_pro
  #total$phi_a_weighted <- total$phi_a*total$pop_pro
  ra_per_country_per_age_per_sex <- total %>%
    group_by(country,year_history,age_name,sex_name) %>%
    summarise(dose_a_weighted_total = sum(dose_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex <- data.frame(ra_per_country_per_age_per_sex)
  total <- merge(total,ra_per_country_per_age_per_sex,by=c("country","year_history","age_name","sex_name"))
  total <- na.omit(total)
  total <- total %>%
    filter(country %in% countries)
  unique_combinations <- unique(total[, .(country, year, age_name, sex_name)])
  unique_combinations <- na.omit(unique_combinations)
  unique_combinations <- unique_combinations %>%
    mutate(age_group = sapply(age_name, get_age_group))
  results_list <- results_list %>%
    rename(age_group = age_name)
  unique_results <- results_list %>%
    distinct(country, age_group, sex_name)
  filtered_combinations <- semi_join(unique_combinations, unique_results, by = c("country", "age_group", "sex_name"))
  
  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total_no <- merge(country,results_no,by=c("lat","lon"))
  total_no$year <- year_
  total_no$year_history <- year_-total_no$age
  colnames(total_no)[which(names(total_no) == "age")] <- "age_name"
  total_no <- merge(total_no,pop_long,by=c("lat","lon","year","age_name","country"))
  total_no$dose_a_weighted <- total_no$dose_a*total_no$pop_pro
  #total$phi_a_weighted <- total$phi_a*total$pop_pro
  ra_per_country_per_age_per_sex_no <- total_no %>%
    group_by(country,year_history,age_name,sex_name) %>%
    summarise(dose_a_weighted_total = sum(dose_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex_no <- data.frame(ra_per_country_per_age_per_sex_no)
  total_no <- merge(total_no,ra_per_country_per_age_per_sex_no,by=c("country","year_history","age_name","sex_name"))
  total_no <- na.omit(total_no)
  total_no <- total_no %>%
    filter(country %in% countries)
 
  simulation_results_no <- foreach(row = 1:nrow(filtered_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- filtered_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total_no[total_no$country == country_ & total_no$year == year_ & total_no$age_name <= age_name_ & total_no$sex_name == sex_name_, ]
    # 筛选唯一值
    group_ <- group %>%
      select(country, year,year_history, age_name, sex_name, dose_a_weighted_total) %>%
      distinct()
    dose_a_weighted_total <- unlist(group_$dose_a_weighted_total)
    age_group_ <- get_age_group(age_name_)
    filtered_data <- results_list %>%
      filter(country == country_, age_group == age_group_, sex_name == sex_name_)
    simulations <- data.frame(simulation = 1:n_simulations, Y_BCC_CM = NA)
    c_d <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_d_bcc.csv")
    for (sim in 1:n_simulations) {
      c_sim <- c_d %>% filter(simulation == sim) %>% pull(c)
      d_sim <- c_d %>% filter(simulation == sim) %>% pull(d)
      b_sim <- filtered_data %>% filter(simulation == sim) %>% pull(b)
      age <- as.numeric(age_name_)
      simulations$Y_BCC_CM[sim] <- calculate_Y_BCC_CM_weighted(dose_a_weighted_total, c_sim, d_sim, b_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    return(simulations)
  }
  
  simulation_results <- foreach(row = 1:nrow(filtered_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- filtered_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total[total$country == country_ & total$year == year_ & total$age_name <= age_name_ & total$sex_name == sex_name_, ]
    group_ <- group %>%
      select(country, year,year_history, age_name, sex_name, dose_a_weighted_total) %>%
      distinct()
    dose_a_weighted_total <- unlist(group_$dose_a_weighted_total)
    age_group_ <- get_age_group(age_name_)
    filtered_data <- results_list %>%
      filter(country == country_, age_group == age_group_, sex_name == sex_name_)
    simulations <- data.frame(simulation = 1:n_simulations, Y_BCC_CM = NA)
    c_d <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_d_bcc.csv")
    for (sim in 1:n_simulations) {
      c_sim <- c_d %>% filter(simulation == sim) %>% pull(c)
      d_sim <- c_d %>% filter(simulation == sim) %>% pull(d)
      b_sim <- filtered_data %>% filter(simulation == sim) %>% pull(b)
      age <- as.numeric(age_name_)
      simulations$Y_BCC_CM[sim] <- calculate_Y_BCC_CM_weighted(dose_a_weighted_total, c_sim, d_sim, b_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    return(simulations)
  }
  
  simulation_results<- data.frame(simulation_results)
  simulation_results <- na.omit(simulation_results)
  simulation_results_no<- data.frame(simulation_results_no)
  simulation_results_no <- na.omit(simulation_results_no)
  colnames(simulation_results_no)[which(names(simulation_results_no) == "Y_BCC_CM")] <- "Y_BCC_CM_nocontrol"
  
  colnames(simulation_results)[which(names(simulation_results) == "age_name")] <- "age"
  simulation_results_ <- simulation_results %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_BCC_CM = mean(Y_BCC_CM, na.rm = TRUE))
  
  colnames(simulation_results_no)[which(names(simulation_results_no) == "age_name")] <- "age"
  simulation_results_no_ <- simulation_results_no %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_BCC_CM_no = mean(Y_BCC_CM_nocontrol, na.rm = TRUE))
  
  fwrite(simulation_results_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/bcc_results/WMO/bcc_wmo_3000_origin_%d.csv", year_), row.names = FALSE)
  fwrite(simulation_results_no_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/bcc_results/Nocontrol/bcc_nocontrol_3000_origin_%d.csv", year_), row.names = FALSE)
  gc()
}
stopImplicitCluster()


##SCC
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(broom)
library(purrr)
library(tidyr)
no_cores <- 35
registerDoParallel(cores = no_cores)

ra_annual <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_NMSC_Nocontrol.csv")
ra_annual$sum_y_mj <- ra_annual$sum_y * 3600 / 10^6
unique_coords <- unique(ra_annual[, .(lon, lat)])
unique_coords <- as.data.frame(unique_coords)
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra_annual <- merge(ra_annual,country,by=c("lon","lat"))



library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
no_cores <- 50
registerDoParallel(cores = no_cores)
library(dplyr)
library(data.table)
#library(triangle)

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

calculate_phi_a <- function(a, Dose) {
  Dose <- Dose[1:a]
  Dose <- rev(Dose)
  for (x in 0:(a - 1)) {
    phi_a <- sum(Dose[1:(x + 1)])
  }
  return(phi_a)
}

calculate_Y_SCC_weighted <- function(phi_a_weighted_total, age, c, d,b) {
  sum_val <- 0
  for (x in 0:(age - 1)) {
    sum_val <- (phi_a_weighted_total^(c)) * ((age)^(d-c))
  }
  sum_val <- b*sum_val
  return(sum_val)
}

c_mean <- 2.5
c_sd <- 0.7 / 1.96
d_mean <- 6.6
d_sd <- 0.4 / 1.96
n_simulations <- 3000

country_list <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/extract_b_scc_3000_new.csv")
countries <- unique(country_list$country)

for (year_ in 2048) {
  #year_ <- 1980
  print(paste("Processing year:", year_))
  results_list <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/extract_b_scc_3000_new.csv")
  results <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/resuts of TUV/ra_annual_s_wmo/results_%d.csv",year_))
  pop_long <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_long_%d.csv",year_))
  results_no <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_s_nocontrol/results_%d.csv",year_))
  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total <- merge(country,results,by=c("lat","lon"))
  total$year <- year_
  total$year_history <- year_-total$age
  colnames(total)[which(names(total) == "age")] <- "age_name"
  total <- merge(total,pop_long,by=c("lat","lon","year","age_name","country"))
  #total$dose_a_weighted <- total$dose_a*total$pop_pro
  total$phi_a_weighted <- total$phi_a*total$pop_pro
  ra_per_country_per_age_per_sex <- total %>%
    group_by(country,year_history,age_name,sex_name) %>%
    summarise(phi_a_weighted_total = sum(phi_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex <- data.frame(ra_per_country_per_age_per_sex)
  total <- merge(total,ra_per_country_per_age_per_sex,by=c("country","year_history","age_name","sex_name"))
  total <- na.omit(total)
  total <- total %>%
    filter(country %in% countries)
  unique_combinations <- unique(total[, .(country, year, age_name, sex_name)])
  unique_combinations <- na.omit(unique_combinations)
  unique_combinations <- unique_combinations %>%
    mutate(age_group = sapply(age_name, get_age_group))
  results_list <- results_list %>%
    rename(age_group = age_name)
  unique_results <- results_list %>%
    distinct(country, age_group, sex_name)
  filtered_combinations <- semi_join(unique_combinations, unique_results, by = c("country", "age_group", "sex_name"))
  
  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total_no <- merge(country,results_no,by=c("lat","lon"))
  total_no$year <- year_
  total_no$year_history <- year_-total_no$age
  colnames(total_no)[which(names(total_no) == "age")] <- "age_name"
  total_no <- merge(total_no,pop_long,by=c("lat","lon","year","age_name","country"))
  #total_no$dose_a_weighted <- total_no$dose_a*total_no$pop_pro
  total_no$phi_a_weighted <- total_no$phi_a*total_no$pop_pro
  ra_per_country_per_age_per_sex_no <- total_no %>%
    group_by(country,year_history,age_name,sex_name) %>%
    summarise(phi_a_weighted_total = sum(phi_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex_no <- data.frame(ra_per_country_per_age_per_sex_no)
  total_no <- merge(total_no,ra_per_country_per_age_per_sex_no,by=c("country","year_history","age_name","sex_name"))
  total_no <- na.omit(total_no)
  total_no <- total_no %>%
    filter(country %in% countries)
  

  simulation_results_no <- foreach(row = 1:nrow(filtered_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- filtered_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total_no[total_no$country == country_ & total_no$year == year_ & total_no$age_name <= age_name_ & total_no$sex_name == sex_name_, ]
    # 筛选唯一值
    group_ <- group %>%
      select(country, year,year_history, age_name, sex_name, phi_a_weighted_total) %>%
      distinct()
    phi_a_weighted_total <- group$phi_a_weighted_total[1]
    #dose_a_weighted_total <- unlist(group_$dose_a_weighted_total)
    age_group_ <- get_age_group(age_name_)
    filtered_data <- results_list %>%
      filter(country == country_, age_group == age_group_, sex_name == sex_name_)
    simulations <- data.frame(simulation = 1:n_simulations, Y_SCC = NA)
    c_d <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_d_scc.csv")
    for (sim in 1:n_simulations) {
      c_sim <- c_d %>% filter(simulation == sim) %>% pull(c)
      d_sim <- c_d %>% filter(simulation == sim) %>% pull(d)
      b_sim <- filtered_data %>% filter(simulation == sim) %>% pull(b)
      age <- as.numeric(age_name_)
      simulations$Y_SCC[sim] <- calculate_Y_SCC_weighted(phi_a_weighted_total, age, c_sim, d_sim, b_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    return(simulations)
  }
  
  simulation_results <- foreach(row = 1:nrow(filtered_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- filtered_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total[total$country == country_ & total$year == year_ & total$age_name <= age_name_ & total$sex_name == sex_name_, ]
    group_ <- group %>%
      select(country, year,year_history, age_name, sex_name, phi_a_weighted_total) %>%
      distinct()
    phi_a_weighted_total <- group$phi_a_weighted_total[1]
    #dose_a_weighted_total <- unlist(group_$dose_a_weighted_total)
    age_group_ <- get_age_group(age_name_)
    filtered_data <- results_list %>%
      filter(country == country_, age_group == age_group_, sex_name == sex_name_)
    simulations <- data.frame(simulation = 1:n_simulations, Y_SCC = NA)
    c_d <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_d_scc.csv")
    for (sim in 1:n_simulations) {
      c_sim <- c_d %>% filter(simulation == sim) %>% pull(c)
      d_sim <- c_d %>% filter(simulation == sim) %>% pull(d)
      b_sim <- filtered_data %>% filter(simulation == sim) %>% pull(b)
      age <- as.numeric(age_name_)
      simulations$Y_SCC[sim] <- calculate_Y_SCC_weighted(phi_a_weighted_total, age, c_sim, d_sim, b_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    return(simulations)
  }
  
  simulation_results<- data.frame(simulation_results)
  simulation_results <- na.omit(simulation_results)
  simulation_results_no<- data.frame(simulation_results_no)
  simulation_results_no <- na.omit(simulation_results_no)
  colnames(simulation_results_no)[which(names(simulation_results_no) == "Y_SCC")] <- "Y_SCC_nocontrol"
  
  colnames(simulation_results)[which(names(simulation_results) == "age_name")] <- "age"
  simulation_results_ <- simulation_results %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_SCC = mean(Y_SCC, na.rm = TRUE))
  
  colnames(simulation_results_no)[which(names(simulation_results_no) == "age_name")] <- "age"
  simulation_results_no_ <- simulation_results_no %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_SCC_no = mean(Y_SCC_nocontrol, na.rm = TRUE))
  
  fwrite(simulation_results_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/scc_results/WMO/scc_wmo_3000_origin_%d.csv", year_), row.names = FALSE)
  fwrite(simulation_results_no_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/scc_results/Nocontrol/scc_nocontrol_3000_origin_%d.csv", year_), row.names = FALSE)
  gc()
}
stopImplicitCluster()

##############################################################################mortality
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

library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
no_cores <- 11
registerDoParallel(cores = no_cores)
ra <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/radiation_b_s_1980_2100_nocontrol.csv")
ra$ra_mj <- ra$ra_mean*3600/10^6
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra <- merge(ra,country,by=c("lon","lat"))
ra_ <- merge(ra, pop_long, by = c("lon", "lat", "country", "year", "age_name"))
ra_$ra_mj_weighted <- ra_$ra_mj * ra_$pop_pro
ra_per_country_per_age_per_sex <- ra_ %>%
  group_by(country, age_name, sex_name, year) %>%
  summarise(ra_mj_weighted_total = sum(ra_mj_weighted, na.rm = TRUE)) %>%
  ungroup()

c_male <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_scc_de_male.csv")
c_male$sex_name <- "Male"
c_female <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_scc_de_female.csv")
c_female$sex_name <- "Female"
c <- rbind(c_male,c_female)

b <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/extract_b_scc_de_3000_new.csv")
years <- 2021:2060
process_year <- function(year_) {
  b <- b[, .(country, age_name, sex_name, b, simulation)]
  ra_y <- ra_per_country_per_age_per_sex %>% filter(year == year_)
  total <- merge(ra_y,c,by=c("sex_name"))
  total <- total %>%
    mutate(ra_mj_weighted_total_powered = ra_mj_weighted_total ^ c)
  total <- merge(total, b, by = c("country", "age_name", "sex_name","simulation"), allow.cartesian = TRUE)
  total$Y <- total$ra_mj_weighted_total_powered * total$b
  fwrite(total,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/scc_de_results/Nocontrol/scc_de_nocontrol_3000_origin_%d.csv",year_),row.names = FALSE)
}
foreach(year_ = years) %dopar% {
  process_year(year_)
}

##############################################################################cataract
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

library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
no_cores <- 11
registerDoParallel(cores = no_cores)
ra <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/radiation_cataract_1980_2100.csv")
ra$ra_mj <- ra$ra_mean*3600/10^6
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra <- merge(ra,country,by=c("lon","lat"))
ra_ <- merge(ra, pop_long, by = c("lon", "lat", "country", "year", "age_name"))
ra_$ra_mj_weighted <- ra_$ra_mj * ra_$pop_pro
ra_per_country_per_age_per_sex <- ra_ %>%
  group_by(country, age_name, sex_name, year) %>%
  summarise(ra_mj_weighted_total = sum(ra_mj_weighted, na.rm = TRUE)) %>%
  ungroup()

c <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_cataract.csv")

b <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/b/extract_b_cataract_3000_new.csv")
years <- 1980:2020
process_year <- function(year_) {
  b <- b[, .(country, age_name, sex_name, b, simulation)]
  ra_y <- ra_per_country_per_age_per_sex %>% filter(year == year_)
  total <- merge(ra_y, b, by = c("country", "age_name", "sex_name"), allow.cartesian = TRUE)
  total <- merge(total,c,by=c("simulation"))
  total <- total %>%
    mutate(ra_mj_weighted_total_powered = ra_mj_weighted_total ^ c)
  total$Y <- total$ra_mj_weighted_total_powered * total$b
  fwrite(total,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cataract_results/WMO/cataract_wmo_3000_origin_%d.csv",year_),row.names = FALSE)
}
foreach(year_ = years) %dopar% {
  process_year(year_)
}


########################################calculated the optimal estimation
#m
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
no_cores <- 50
registerDoParallel(cores = no_cores)
library(dplyr)
library(data.table)

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

calculate_dose_a <- function(a, Dose) {
  Dose <- Dose[1:a]
  for (x in 0:(a - 1)) {
    dose_a <- Dose[x + 1]
  }
  return(dose_a)
}
calculate_Y_BCC_CM_weighted <- function(dose_a_weighted_total, c, d, b) {
  sum_val <- 0
  for (x in 1:(length(dose_a_weighted_total))) {
    phi_a <- sum(dose_a_weighted_total[1:(x)])
    sum_val <- sum_val + dose_a_weighted_total[x] *phi_a* ((length(dose_a_weighted_total) - x)^(d - c))
  }
  sum_val <- b*sum_val
  return(sum_val)
}

c_mean <- 0.6
# c_sd <- 0.4 / 1.96
d_mean <- 4.7
# d_sd <- 1 / 1.96
n_simulations <- 1

country_list <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/b_m_oe.csv")
countries <- unique(country_list$country)

for (year_ in 1980:2100) {
  #year_ <- 1980
  print(paste("Processing year:", year_))
  results_list <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/b_m_oe.csv")
  results <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_m_wmo/results_%d.csv",year_))
  pop_long <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_long_%d.csv",year_))
  results_no <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_m_nocontrol/results_%d.csv",year_))
  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total <- merge(country,results,by=c("lat","lon"))
  total$year <- year_
  total$year_history <- year_-total$age
  colnames(total)[which(names(total) == "age")] <- "age_name"
  total <- merge(total,pop_long,by=c("lat","lon","year","age_name","country"))
  total$dose_a_weighted <- total$dose_a*total$pop_pro
  #total$phi_a_weighted <- total$phi_a*total$pop_pro
  ra_per_country_per_age_per_sex <- total %>%
    group_by(country,year_history,age_name,sex_name) %>%
    summarise(dose_a_weighted_total = sum(dose_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex <- data.frame(ra_per_country_per_age_per_sex)
  total <- merge(total,ra_per_country_per_age_per_sex,by=c("country","year_history","age_name","sex_name"))
  total <- na.omit(total)
  total <- total %>%
    filter(country %in% countries)
  unique_combinations <- unique(total[, .(country, year, age_name, sex_name)])
  unique_combinations <- na.omit(unique_combinations)
  unique_combinations <- unique_combinations %>%
    mutate(age_group = sapply(age_name, get_age_group))
  results_list <- results_list %>%
    rename(age_group = age_name)
  unique_results <- results_list %>%
    distinct(country, age_group, sex_name)
  filtered_combinations <- semi_join(unique_combinations, unique_results, by = c("country", "age_group", "sex_name"))

  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total_no <- merge(country,results_no,by=c("lat","lon"))
  total_no$year <- year_
  total_no$year_history <- year_-total_no$age
  colnames(total_no)[which(names(total_no) == "age")] <- "age_name"
  total_no <- merge(total_no,pop_long,by=c("lat","lon","year","age_name","country"))
  total_no$dose_a_weighted <- total_no$dose_a*total_no$pop_pro
  #total$phi_a_weighted <- total$phi_a*total$pop_pro
  ra_per_country_per_age_per_sex_no <- total_no %>%
    group_by(country,year_history,age_name,sex_name) %>%
    summarise(dose_a_weighted_total = sum(dose_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex_no <- data.frame(ra_per_country_per_age_per_sex_no)
  total_no <- merge(total_no,ra_per_country_per_age_per_sex_no,by=c("country","year_history","age_name","sex_name"))
  total_no <- na.omit(total_no)
  total_no <- total_no %>%
    filter(country %in% countries)

  simulation_results_no <- foreach(row = 1:nrow(filtered_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- filtered_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    #print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total_no[total_no$country == country_ & total_no$year == year_ & total_no$age_name <= age_name_ & total_no$sex_name == sex_name_, ]
    group_ <- group %>%
      select(country, year,year_history, age_name, sex_name, dose_a_weighted_total) %>%
      distinct()
    dose_a_weighted_total <- unlist(group_$dose_a_weighted_total)
    age_group_ <- get_age_group(age_name_)
    filtered_data <- results_list %>%
      filter(country == country_, age_group == age_group_, sex_name == sex_name_)
    simulations <- data.frame(simulation = 1:n_simulations, Y_BCC_CM = NA)
    for (sim in 1:n_simulations) {
      c_sim <- c_mean
      d_sim <- d_mean
      b_sim <- filtered_data$mean_b
      age <- as.numeric(age_name_)
      simulations$Y_BCC_CM[sim] <- calculate_Y_BCC_CM_weighted(dose_a_weighted_total, c_sim, d_sim, b_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    return(simulations)
  }


  simulation_results <- foreach(row = 1:nrow(filtered_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- filtered_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    #print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total[total$country == country_ & total$year == year_ & total$age_name <= age_name_ & total$sex_name == sex_name_, ]
    group_ <- group %>%
      select(country, year,year_history, age_name, sex_name, dose_a_weighted_total) %>%
      distinct()
    dose_a_weighted_total <- unlist(group_$dose_a_weighted_total)
    age_group_ <- get_age_group(age_name_)
    filtered_data <- results_list %>%
      filter(country == country_, age_group == age_group_, sex_name == sex_name_)
    simulations <- data.frame(simulation = 1:n_simulations, Y_BCC_CM = NA)
    for (sim in 1:n_simulations) {
      c_sim <- c_mean
      d_sim <- d_mean

      b_sim <- filtered_data$mean_b
      age <- as.numeric(age_name_)
      simulations$Y_BCC_CM[sim] <- calculate_Y_BCC_CM_weighted(dose_a_weighted_total, c_sim, d_sim, b_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    return(simulations)
  }

  simulation_results<- data.frame(simulation_results)
  simulation_results <- na.omit(simulation_results)
  simulation_results_no<- data.frame(simulation_results_no)
  simulation_results_no <- na.omit(simulation_results_no)
  colnames(simulation_results_no)[which(names(simulation_results_no) == "Y_BCC_CM")] <- "Y_BCC_CM_nocontrol"

  colnames(simulation_results)[which(names(simulation_results) == "age_name")] <- "age"
  simulation_results_ <- simulation_results %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_BCC_CM = mean(Y_BCC_CM, na.rm = TRUE))

  colnames(simulation_results_no)[which(names(simulation_results_no) == "age_name")] <- "age"
  simulation_results_no_ <- simulation_results_no %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_BCC_CM_no = mean(Y_BCC_CM_nocontrol, na.rm = TRUE))

  fwrite(simulation_results_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/m_results/WMO/m_wmo_oe_%d.csv", year_), row.names = FALSE)
  fwrite(simulation_results_no_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/m_results/Nocontrol/m_nocontrol_oe_%d.csv", year_), row.names = FALSE)
  gc()
}
stopImplicitCluster()

#bcc
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(broom)
library(purrr)
library(tidyr)
no_cores <- 50
registerDoParallel(cores = no_cores)

ra_annual <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_NMSC_Nocontrol.csv")
ra_annual$sum_y_mj <- ra_annual$sum_y * 3600 / 10^6
unique_coords <- unique(ra_annual[, .(lon, lat)])
unique_coords <- as.data.frame(unique_coords)
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra_annual <- merge(ra_annual,country,by=c("lon","lat"))

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

calculate_dose_a <- function(a, Dose) {
  Dose <- Dose[1:a]
  for (x in 0:(a - 1)) {
    dose_a <- Dose[x + 1]
  }
  return(dose_a)
}

calculate_Y_BCC_CM_weighted <- function(dose_a_weighted_total, c, d, b) {
  sum_val <- 0
  for (x in 1:(length(dose_a_weighted_total))) {
    phi_a <- sum(dose_a_weighted_total[1:(x)])
    sum_val <- sum_val + dose_a_weighted_total[x] *phi_a* ((length(dose_a_weighted_total) - x)^(d - c))
  }
  sum_val <- b*sum_val
  return(sum_val)
}

c_mean <- 1.4
#c_sd <- 0.4 / 1.96
d_mean <- 4.9
#d_sd <- 0.6 / 1.96
n_simulations <- 1

country_list <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/b_bcc_oe.csv")
countries <- unique(country_list$country)

for (year_ in 1980:1990) {
  #year_ <- 1980
  print(paste("Processing year:", year_))
  results_list <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/b_bcc_oe.csv")
  results <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_b_s_wmo/results_%d.csv",year_))
  pop_long <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_long_%d.csv",year_))
  results_no <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_b_s_nocontrol/results_%d.csv",year_))
  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total <- merge(country,results,by=c("lat","lon"))
  total$year <- year_
  total$year_history <- year_-total$age
  colnames(total)[which(names(total) == "age")] <- "age_name"
  total <- merge(total,pop_long,by=c("lat","lon","year","age_name","country"))
  total$dose_a_weighted <- total$dose_a*total$pop_pro
  #total$phi_a_weighted <- total$phi_a*total$pop_pro
  ra_per_country_per_age_per_sex <- total %>%
    group_by(country,year_history,age_name,sex_name) %>%
    summarise(dose_a_weighted_total = sum(dose_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex <- data.frame(ra_per_country_per_age_per_sex)
  total <- merge(total,ra_per_country_per_age_per_sex,by=c("country","year_history","age_name","sex_name"))
  total <- na.omit(total)
  total <- total %>%
    filter(country %in% countries)
  unique_combinations <- unique(total[, .(country, year, age_name, sex_name)])
  unique_combinations <- na.omit(unique_combinations)
  unique_combinations <- unique_combinations %>%
    mutate(age_group = sapply(age_name, get_age_group))
  results_list <- results_list %>%
    rename(age_group = age_name)
  unique_results <- results_list %>%
    distinct(country, age_group, sex_name)
  filtered_combinations <- semi_join(unique_combinations, unique_results, by = c("country", "age_group", "sex_name"))

  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total_no <- merge(country,results_no,by=c("lat","lon"))
  total_no$year <- year_
  total_no$year_history <- year_-total_no$age
  colnames(total_no)[which(names(total_no) == "age")] <- "age_name"
  total_no <- merge(total_no,pop_long,by=c("lat","lon","year","age_name","country"))
  total_no$dose_a_weighted <- total_no$dose_a*total_no$pop_pro
  #total$phi_a_weighted <- total$phi_a*total$pop_pro
  ra_per_country_per_age_per_sex_no <- total_no %>%
    group_by(country,year_history,age_name,sex_name) %>%
    summarise(dose_a_weighted_total = sum(dose_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex_no <- data.frame(ra_per_country_per_age_per_sex_no)
  total_no <- merge(total_no,ra_per_country_per_age_per_sex_no,by=c("country","year_history","age_name","sex_name"))
  total_no <- na.omit(total_no)
  total_no <- total_no %>%
    filter(country %in% countries)

  simulation_results_no <- foreach(row = 1:nrow(filtered_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- filtered_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    #print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total_no[total_no$country == country_ & total_no$year == year_ & total_no$age_name <= age_name_ & total_no$sex_name == sex_name_, ]
    group_ <- group %>%
      select(country, year,year_history, age_name, sex_name, dose_a_weighted_total) %>%
      distinct()
    dose_a_weighted_total <- unlist(group_$dose_a_weighted_total)
    age_group_ <- get_age_group(age_name_)
    filtered_data <- results_list %>%
      filter(country == country_, age_group == age_group_, sex_name == sex_name_)
    simulations <- data.frame(simulation = 1:n_simulations, Y_BCC_CM = NA)
    for (sim in 1:n_simulations) {
      c_sim <- c_mean
      d_sim <- d_mean
      b_sim <- filtered_data$mean_b
      age <- as.numeric(age_name_)
      simulations$Y_BCC_CM[sim] <- calculate_Y_BCC_CM_weighted(dose_a_weighted_total, c_sim, d_sim, b_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    return(simulations)
  }

  simulation_results <- foreach(row = 1:nrow(filtered_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- filtered_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    #print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total[total$country == country_ & total$year == year_ & total$age_name <= age_name_ & total$sex_name == sex_name_, ]
    # 筛选唯一值
    group_ <- group %>%
      select(country, year,year_history, age_name, sex_name, dose_a_weighted_total) %>%
      distinct()
    dose_a_weighted_total <- unlist(group_$dose_a_weighted_total)
    age_group_ <- get_age_group(age_name_)
    filtered_data <- results_list %>%
      filter(country == country_, age_group == age_group_, sex_name == sex_name_)
    simulations <- data.frame(simulation = 1:n_simulations, Y_BCC_CM = NA)
    for (sim in 1:n_simulations) {
      c_sim <- c_mean
      d_sim <- d_mean
      b_sim <- filtered_data$mean_b
      age <- as.numeric(age_name_)
      simulations$Y_BCC_CM[sim] <- calculate_Y_BCC_CM_weighted(dose_a_weighted_total, c_sim, d_sim, b_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    return(simulations)
  }

  simulation_results<- data.frame(simulation_results)
  simulation_results <- na.omit(simulation_results)
  simulation_results_no<- data.frame(simulation_results_no)
  simulation_results_no <- na.omit(simulation_results_no)
  colnames(simulation_results_no)[which(names(simulation_results_no) == "Y_BCC_CM")] <- "Y_BCC_CM_nocontrol"

  colnames(simulation_results)[which(names(simulation_results) == "age_name")] <- "age"
  simulation_results_ <- simulation_results %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_BCC_CM = mean(Y_BCC_CM, na.rm = TRUE))

  colnames(simulation_results_no)[which(names(simulation_results_no) == "age_name")] <- "age"
  simulation_results_no_ <- simulation_results_no %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_BCC_CM_no = mean(Y_BCC_CM_nocontrol, na.rm = TRUE))

  fwrite(simulation_results_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/bcc_results/WMO/bcc_wmo_%d.csv", year_), row.names = FALSE)
  fwrite(simulation_results_no_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/bcc_results/Nocontrol/bcc_nocontrol_%d.csv", year_), row.names = FALSE)
  gc()
}
stopImplicitCluster()

#scc
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
no_cores <- 50
registerDoParallel(cores = no_cores)
library(dplyr)
library(data.table)
#library(triangle)

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

calculate_phi_a <- function(a, Dose) {
  Dose <- Dose[1:a]
  Dose <- rev(Dose)
  for (x in 0:(a - 1)) {
    phi_a <- sum(Dose[1:(x + 1)])
  }
  return(phi_a)
}

calculate_Y_SCC_weighted <- function(phi_a_weighted_total, age, c, d,b) {
  sum_val <- 0
  for (x in 0:(age - 1)) {
    sum_val <- (phi_a_weighted_total^(c)) * ((age)^(d-c))
  }
  sum_val <- b*sum_val
  return(sum_val)
}

c_mean <- 2.5
#c_sd <- 0.7 / 1.96
d_mean <- 6.6
#d_sd <- 0.4 / 1.96
n_simulations <- 1

country_list <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/b_scc_oe.csv")
countries <- unique(country_list$country)

for (year_ in 1980:1990) {
  #year_ <- 1980
  print(paste("Processing year:", year_))
  results_list <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/b_scc_oe.csv")
  results <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_s_wmo/results_%d.csv",year_))
  pop_long <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_long_%d.csv",year_))
  results_no <- fread(sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_s_nocontrol/results_%d.csv",year_))
  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total <- merge(country,results,by=c("lat","lon"))
  total$year <- year_
  total$year_history <- year_-total$age
  colnames(total)[which(names(total) == "age")] <- "age_name"
  total <- merge(total,pop_long,by=c("lat","lon","year","age_name","country"))
  #total$dose_a_weighted <- total$dose_a*total$pop_pro
  total$phi_a_weighted <- total$phi_a*total$pop_pro
  ra_per_country_per_age_per_sex <- total %>%
    group_by(country,year_history,age_name,sex_name) %>%
    summarise(phi_a_weighted_total = sum(phi_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex <- data.frame(ra_per_country_per_age_per_sex)
  total <- merge(total,ra_per_country_per_age_per_sex,by=c("country","year_history","age_name","sex_name"))
  total <- na.omit(total)
  total <- total %>%
    filter(country %in% countries)
  unique_combinations <- unique(total[, .(country, year, age_name, sex_name)])
  unique_combinations <- na.omit(unique_combinations)
  unique_combinations <- unique_combinations %>%
    mutate(age_group = sapply(age_name, get_age_group))
  results_list <- results_list %>%
    rename(age_group = age_name)
  unique_results <- results_list %>%
    distinct(country, age_group, sex_name)
  filtered_combinations <- semi_join(unique_combinations, unique_results, by = c("country", "age_group", "sex_name"))

  country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
  total_no <- merge(country,results_no,by=c("lat","lon"))
  total_no$year <- year_
  total_no$year_history <- year_-total_no$age
  colnames(total_no)[which(names(total_no) == "age")] <- "age_name"
  total_no <- merge(total_no,pop_long,by=c("lat","lon","year","age_name","country"))
  #total_no$dose_a_weighted <- total_no$dose_a*total_no$pop_pro
  total_no$phi_a_weighted <- total_no$phi_a*total_no$pop_pro
  ra_per_country_per_age_per_sex_no <- total_no %>%
    group_by(country,year_history,age_name,sex_name) %>%
    summarise(phi_a_weighted_total = sum(phi_a_weighted, na.rm = TRUE))
  ra_per_country_per_age_per_sex_no <- data.frame(ra_per_country_per_age_per_sex_no)
  total_no <- merge(total_no,ra_per_country_per_age_per_sex_no,by=c("country","year_history","age_name","sex_name"))
  total_no <- na.omit(total_no)
  total_no <- total_no %>%
    filter(country %in% countries)

  simulation_results_no <- foreach(row = 1:nrow(filtered_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- filtered_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    #print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total_no[total_no$country == country_ & total_no$year == year_ & total_no$age_name <= age_name_ & total_no$sex_name == sex_name_, ]
    group_ <- group %>%
      select(country, year,year_history, age_name, sex_name, phi_a_weighted_total) %>%
      distinct()
    phi_a_weighted_total <- group$phi_a_weighted_total[1]
    #dose_a_weighted_total <- unlist(group_$dose_a_weighted_total)
    age_group_ <- get_age_group(age_name_)
    filtered_data <- results_list %>%
      filter(country == country_, age_group == age_group_, sex_name == sex_name_)
    simulations <- data.frame(simulation = 1:n_simulations, Y_SCC = NA)
    #c_d <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/monte carlo/c_d_scc.csv")
    for (sim in 1:n_simulations) {
      c_sim <- c_mean
      d_sim <- d_mean
      b_sim <- filtered_data$mean_b
      age <- as.numeric(age_name_)
      simulations$Y_SCC[sim] <- calculate_Y_SCC_weighted(phi_a_weighted_total, age, c_sim, d_sim, b_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    return(simulations)
  }

  simulation_results <- foreach(row = 1:nrow(filtered_combinations), .combine = rbind, .packages = c('dplyr')) %dopar% {
    combination <- filtered_combinations[row, ]
    country_ <- combination$country
    year_ <- combination$year
    age_name_ <- combination$age_name
    sex_name_ <- combination$sex_name
    #print(paste("Processing: Country =", country_, ", Year =", year_, ", Age Name =", age_name_, ", Sex Name =", sex_name_))
    group <- total[total$country == country_ & total$year == year_ & total$age_name <= age_name_ & total$sex_name == sex_name_, ]
    # 筛选唯一值
    group_ <- group %>%
      select(country, year,year_history, age_name, sex_name, phi_a_weighted_total) %>%
      distinct()
    phi_a_weighted_total <- group$phi_a_weighted_total[1]
    #dose_a_weighted_total <- unlist(group_$dose_a_weighted_total)
    age_group_ <- get_age_group(age_name_)
    filtered_data <- results_list %>%
      filter(country == country_, age_group == age_group_, sex_name == sex_name_)
    simulations <- data.frame(simulation = 1:n_simulations, Y_SCC = NA)
    for (sim in 1:n_simulations) {
      c_sim <- c_mean
      d_sim <- d_mean
      b_sim <- filtered_data$mean_b
      age <- as.numeric(age_name_)
      simulations$Y_SCC[sim] <- calculate_Y_SCC_weighted(phi_a_weighted_total, age, c_sim, d_sim, b_sim)
    }
    simulations$country <- country_
    simulations$year <- year_
    simulations$age_name <- age_name_
    simulations$sex_name <- sex_name_
    return(simulations)
  }

  simulation_results<- data.frame(simulation_results)
  simulation_results <- na.omit(simulation_results)
  simulation_results_no<- data.frame(simulation_results_no)
  simulation_results_no <- na.omit(simulation_results_no)
  colnames(simulation_results_no)[which(names(simulation_results_no) == "Y_SCC")] <- "Y_SCC_nocontrol"

  colnames(simulation_results)[which(names(simulation_results) == "age_name")] <- "age"
  simulation_results_ <- simulation_results %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_SCC = mean(Y_SCC, na.rm = TRUE))

  colnames(simulation_results_no)[which(names(simulation_results_no) == "age_name")] <- "age"
  simulation_results_no_ <- simulation_results_no %>%
    mutate(age_name = sapply(age, get_age_group)) %>%
    group_by(year,country,age_name,sex_name,simulation) %>%
    summarise(average_Y_SCC_no = mean(Y_SCC_nocontrol, na.rm = TRUE))

  fwrite(simulation_results_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal estimation/scc_results/WMO/scc_wmo_%d.csv", year_), row.names = FALSE)
  fwrite(simulation_results_no_, file = sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal estimation/scc_results/Nocontrol/scc_nocontrol_%d.csv", year_), row.names = FALSE)
  gc()
}
stopImplicitCluster()

####for the mortality and cataract
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

library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
no_cores <- 12
registerDoParallel(cores = no_cores)
ra <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/radiation_b_s_1980_2100_nocontrol.csv")
ra$ra_mj <- ra$ra_mean*3600/10^6
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra <- merge(ra,country,by=c("lon","lat"))
ra_ <- merge(ra, pop_long, by = c("lon", "lat", "country", "year", "age_name"))
ra_$ra_mj_weighted <- ra_$ra_mj * ra_$pop_pro
ra_per_country_per_age_per_sex <- ra_ %>%
  group_by(country, age_name, sex_name, year) %>%
  summarise(ra_mj_weighted_total = sum(ra_mj_weighted, na.rm = TRUE)) %>%
  ungroup()

b <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/b_scc_de_oe.csv")
years <- 1980:2100
process_year <- function(year_) {
  b <- b[, .(country, age_name, sex_name, mean_b)]
  ra_y <- ra_per_country_per_age_per_sex %>% filter(year == year_)
  total <- merge(ra_y, b, by = c("country", "age_name", "sex_name"), allow.cartesian = TRUE)
  total <- total %>%
    mutate(c = if_else(sex_name == "Male", 0.71, 0.46))
    #mutate(c = if_else(sex_name == "Male", 0.58, 0.5))
  total$ra_mj_weighted_total_powered <- total$ra_mj_weighted_total^total$c
  total$Y <- total$ra_mj_weighted_total_powered * total$mean_b
  fwrite(total,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/scc_de_results/Nocontrol/scc_de_nocontrol_%d.csv",year_),row.names = FALSE)

}
foreach(year_ = years) %dopar% {
  process_year(year_)
}


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

library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(tidyr)
no_cores <- 12
registerDoParallel(cores = no_cores)
ra <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/radiation_cataract_1980_2100.csv")
ra$ra_mj <- ra$ra_mean*3600/10^6
country <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/country_lon_lat.csv")
ra <- merge(ra,country,by=c("lon","lat"))
ra_ <- merge(ra, pop_long, by = c("lon", "lat", "country", "year", "age_name"))
ra_$ra_mj_weighted <- ra_$ra_mj * ra_$pop_pro
ra_per_country_per_age_per_sex <- ra_ %>%
  group_by(country, age_name, sex_name, year) %>%
  summarise(ra_mj_weighted_total = sum(ra_mj_weighted, na.rm = TRUE)) %>%
  ungroup()

b <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/b_cataract_oe.csv")
years <- 1980:2100
process_year <- function(year_) {
  b <- b[, .(country, age_name, sex_name, mean_b)]
  ra_y <- ra_per_country_per_age_per_sex %>% filter(year == year_)
  total <- merge(ra_y, b, by = c("country", "age_name", "sex_name"), allow.cartesian = TRUE)
  total$c <- 0.17
  total$ra_mj_weighted_total_powered <- total$ra_mj_weighted_total^total$c
  total$Y <- total$ra_mj_weighted_total_powered * total$mean_b
  fwrite(total,file=sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cataract_results/WMO/cataract_wmo_%d.csv",year_),row.names = FALSE)
}
foreach(year_ = years) %dopar% {
  process_year(year_)
}

#################merge into a large dataframe
library(data.table)
folder_path <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/m_de_results/WMO/"
combined_data <- data.table()
for (year in 1980:2100) {
  file_name <- paste0("m_de_wmo_", year, ".csv")
  file_path <- file.path(folder_path, file_name)
  year_data <- fread(file_path)
  combined_data <- rbindlist(list(combined_data, year_data), use.names = TRUE, fill = TRUE)
}
output_file <- file.path(folder_path, "m_de_wmo.csv")
fwrite(combined_data, output_file, row.names = FALSE)

###############################################################################calculate the number
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

scc_nocontrol <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/m_results/Nocontrol/m_nocontrol.csv")
total <- merge(scc_nocontrol,pop_per_country_per_age_per_sex,by=c("year","country","age_name","sex_name"))
total$pop_country_age_sex_permillion <- total$pop_country_age_sex/(10^6)
total$number_value <- total$average_Y_BCC_CM_no*total$pop_country_age_sex_permillion
head(total)
fwrite(total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/m_results/Nocontrol/m_number_nocontrol_1980_2100_oe.csv",row.names = FALSE)

scc_wmo <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/m_results/WMO/m_wmo.csv")
total <- merge(scc_wmo,pop_per_country_per_age_per_sex,by=c("year","country","age_name","sex_name"))
total$pop_country_age_sex_permillion <- total$pop_country_age_sex/(10^6)
total$number_value <- total$average_Y_BCC_CM*total$pop_country_age_sex_permillion
head(total)
fwrite(total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/m_results/WMO/m_number_wmo_1980_2100_oe.csv",row.names = FALSE)

#mortality and cataract
scc_nocontrol <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/scc_de_results/Nocontrol/scc_de_nocontrol.csv")
total <- merge(scc_nocontrol,pop_per_country_per_age_per_sex,by=c("year","country","age_name","sex_name"))
total$pop_country_age_sex_permillion <- total$pop_country_age_sex/(10^6)
total$number_value <- total$Y*total$pop_country_age_sex_permillion
head(total)
fwrite(total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/scc_de_results/Nocontrol/scc_de_number_nocontrol_1980_2100_oe.csv",row.names = FALSE)

scc_wmo <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/scc_de_results/WMO/scc_de_wmo.csv")
total <- merge(scc_wmo,pop_per_country_per_age_per_sex,by=c("year","country","age_name","sex_name"))
total$pop_country_age_sex_permillion <- total$pop_country_age_sex/(10^6)
total$number_value <- total$Y*total$pop_country_age_sex_permillion
head(total)
fwrite(total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/scc_de_results/WMO/scc_de_number_wmo_1980_2100_oe.csv",row.names = FALSE)


scc_nocontrol <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cataract_results/Nocontrol/cataract_nocontrol.csv")
total <- merge(scc_nocontrol,pop_per_country_per_age_per_sex,by=c("year","country","age_name","sex_name"))
total$pop_country_age_sex_permillion <- total$pop_country_age_sex/(10^6)
total$daly_value <- total$Y*total$pop_country_age_sex_permillion
head(total)
fwrite(total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cataract_results/Nocontrol/cataract_daly_nocontrol_1980_2100_oe.csv",row.names = FALSE)

scc_wmo <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cataract_results/WMO/cataract_wmo.csv")
total <- merge(scc_wmo,pop_per_country_per_age_per_sex,by=c("year","country","age_name","sex_name"))
total$pop_country_age_sex_permillion <- total$pop_country_age_sex/(10^6)
total$daly_value <- total$Y*total$pop_country_age_sex_permillion
head(total)
fwrite(total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cataract_results/WMO/cataract_daly_wmo_1980_2100_oe.csv",row.names = FALSE)


#######################################################################calculate the number for the total of three kinds of cancers
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(data.table)
library(dplyr)
m_wmo <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/m_results/WMO/m_number_wmo_1980_2100_oe.csv")
m_nocontrol <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/m_results/Nocontrol/m_number_nocontrol_1980_2100_oe.csv")
m_wmo <- m_wmo[,c("year","country","age_name","sex_name","pop_country_age_sex_permillion","average_Y_BCC_CM","number_value")]
m_nocontrol <- m_nocontrol[,c("year","country","age_name","sex_name","pop_country_age_sex_permillion","average_Y_BCC_CM_no","number_value")]

bcc_wmo <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/bcc_results/WMO/bcc_number_wmo_1980_2100_oe.csv")
bcc_nocontrol <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/bcc_results/Nocontrol/bcc_number_nocontrol_1980_2100_oe.csv")
bcc_wmo <- bcc_wmo[,c("year","country","age_name","sex_name","pop_country_age_sex_permillion","average_Y_BCC_CM","number_value")]
bcc_nocontrol <- bcc_nocontrol[,c("year","country","age_name","sex_name","pop_country_age_sex_permillion","average_Y_BCC_CM_no","number_value")]

scc_wmo <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/scc_results/WMO/scc_number_wmo_1980_2100_oe.csv")
scc_nocontrol <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/scc_results/Nocontrol/scc_number_nocontrol_1980_2100_oe.csv")
scc_wmo <- scc_wmo[,c("year","country","age_name","sex_name","pop_country_age_sex_permillion","average_Y_SCC","number_value")]
scc_nocontrol <- scc_nocontrol[,c("year","country","age_name","sex_name","pop_country_age_sex_permillion","average_Y_SCC_no","number_value")]

m_wmo$scenario <- "m_wmo"
m_nocontrol$scenario <- "m_nocontrol"
bcc_wmo$scenario <- "bcc_wmo"
bcc_nocontrol$scenario <- "bcc_nocontrol"
scc_wmo$scenario <- "scc_wmo"
scc_nocontrol$scenario <- "scc_nocontrol"

colnames(m_wmo)[which(names(m_wmo) == "average_Y_BCC_CM")] <- "Y_value"
colnames(bcc_wmo)[which(names(bcc_wmo) == "average_Y_BCC_CM")] <- "Y_value"
colnames(scc_wmo)[which(names(scc_wmo) == "average_Y_SCC")] <- "Y_value"
colnames(m_nocontrol)[which(names(m_nocontrol) == "average_Y_BCC_CM_no")] <- "Y_value"
colnames(bcc_nocontrol)[which(names(bcc_nocontrol) == "average_Y_BCC_CM_no")] <- "Y_value"
colnames(scc_nocontrol)[which(names(scc_nocontrol) == "average_Y_SCC_no")] <- "Y_value"

library(data.table)
setDT(m_wmo)
m_wmo <- dcast(
  m_wmo,
  country + year + age_name + sex_name + pop_country_age_sex_permillion ~ scenario,
  value.var = c("Y_value","number_value")
)
head(m_wmo)
m_nocontrol <- dcast(
  m_nocontrol,
  country + year + age_name + sex_name + pop_country_age_sex_permillion ~ scenario,
  value.var = c("Y_value","number_value")
)
head(m_nocontrol)
bcc_wmo <- dcast(
  bcc_wmo,
  country + year + age_name + sex_name + pop_country_age_sex_permillion ~ scenario,
  value.var = c("Y_value","number_value")
)
head(bcc_wmo)
bcc_nocontrol <- dcast(
  bcc_nocontrol,
  country + year + age_name + sex_name + pop_country_age_sex_permillion ~ scenario,
  value.var = c("Y_value","number_value")
)
head(bcc_nocontrol)
scc_wmo <- dcast(
  scc_wmo,
  country + year + age_name + sex_name + pop_country_age_sex_permillion ~ scenario,
  value.var = c("Y_value","number_value")
)
head(scc_wmo)
scc_nocontrol <- dcast(
  scc_nocontrol,
  country + year + age_name + sex_name + pop_country_age_sex_permillion ~ scenario,
  value.var = c("Y_value","number_value")
)
head(scc_nocontrol)

total <- merge(m_wmo,m_nocontrol,by=c("country","year","age_name","sex_name","pop_country_age_sex_permillion"),allow.cartesian = TRUE)
total <- merge(total,bcc_wmo,by=c("country","year","age_name","sex_name","pop_country_age_sex_permillion"),all.x=TRUE,all.y=TRUE,allow.cartesian = TRUE)
total <- merge(total,bcc_nocontrol,by=c("country","year","age_name","sex_name","pop_country_age_sex_permillion"),all.x=TRUE,all.y=TRUE,allow.cartesian = TRUE)
total <- merge(total,scc_wmo,by=c("country","year","age_name","sex_name","pop_country_age_sex_permillion"),all.x=TRUE,all.y=TRUE,allow.cartesian = TRUE)
total <- merge(total,scc_nocontrol,by=c("country","year","age_name","sex_name","pop_country_age_sex_permillion"),all.x=TRUE,all.y=TRUE,allow.cartesian = TRUE)

total$number_val_wmo = rowSums(total[, c("number_value_m_wmo", "number_value_bcc_wmo", "number_value_scc_wmo")], na.rm = TRUE)
total$number_val_nocontrol = rowSums(total[, c("number_value_m_nocontrol", "number_value_bcc_nocontrol", "number_value_scc_nocontrol")], na.rm = TRUE)
total$dif <- total$number_val_nocontrol-total$number_val_wmo

fwrite(total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_1980_2100.csv",row.names = FALSE)


###################death
m_wmo <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/m_de_results/WMO/m_de_number_wmo_1980_2100_oe.csv")
m_nocontrol <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/m_de_results/Nocontrol/m_de_number_nocontrol_1980_2100_oe.csv")
m_wmo <- m_wmo[,c("year","country","age_name","sex_name","pop_country_age_sex_permillion","Y","number_value")]
m_nocontrol <- m_nocontrol[,c("year","country","age_name","sex_name","pop_country_age_sex_permillion","Y","number_value")]

scc_wmo <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/scc_de_results/WMO/scc_de_number_wmo_1980_2100_oe.csv")
scc_nocontrol <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/scc_de_results/Nocontrol/scc_de_number_nocontrol_1980_2100_oe.csv")
scc_wmo <- scc_wmo[,c("year","country","age_name","sex_name","pop_country_age_sex_permillion","Y","number_value")]
scc_nocontrol <- scc_nocontrol[,c("year","country","age_name","sex_name","pop_country_age_sex_permillion","Y","number_value")]

m_wmo$scenario <- "m_wmo"
m_nocontrol$scenario <- "m_nocontrol"
scc_wmo$scenario <- "scc_wmo"
scc_nocontrol$scenario <- "scc_nocontrol"

colnames(m_wmo)[which(names(m_wmo) == "Y")] <- "Y_value"
colnames(scc_wmo)[which(names(scc_wmo) == "Y")] <- "Y_value"
colnames(m_nocontrol)[which(names(m_nocontrol) == "Y")] <- "Y_value"
colnames(scc_nocontrol)[which(names(scc_nocontrol) == "Y")] <- "Y_value"

library(data.table)
setDT(m_wmo)
m_wmo <- dcast(
  m_wmo,
  country + year + age_name + sex_name + pop_country_age_sex_permillion ~ scenario,
  value.var = c("Y_value","number_value")
)
head(m_wmo)
m_nocontrol <- dcast(
  m_nocontrol,
  country + year + age_name + sex_name + pop_country_age_sex_permillion ~ scenario,
  value.var = c("Y_value","number_value")
)
head(m_nocontrol)
scc_wmo <- dcast(
  scc_wmo,
  country + year + age_name + sex_name + pop_country_age_sex_permillion ~ scenario,
  value.var = c("Y_value","number_value")
)
head(scc_wmo)
scc_nocontrol <- dcast(
  scc_nocontrol,
  country + year + age_name + sex_name + pop_country_age_sex_permillion ~ scenario,
  value.var = c("Y_value","number_value")
)
head(scc_nocontrol)

total <- merge(m_wmo,m_nocontrol,by=c("country","year","age_name","sex_name","pop_country_age_sex_permillion"),allow.cartesian = TRUE)
total <- merge(total,scc_wmo,by=c("country","year","age_name","sex_name","pop_country_age_sex_permillion"),all.x=TRUE,all.y=TRUE,allow.cartesian = TRUE)
total <- merge(total,scc_nocontrol,by=c("country","year","age_name","sex_name","pop_country_age_sex_permillion"),all.x=TRUE,all.y=TRUE,allow.cartesian = TRUE)

total$number_val_wmo = rowSums(total[, c("number_value_m_wmo", "number_value_scc_wmo")], na.rm = TRUE)
total$number_val_nocontrol = rowSums(total[, c("number_value_m_nocontrol", "number_value_scc_nocontrol")], na.rm = TRUE)
total$dif <- total$number_val_nocontrol-total$number_val_wmo

fwrite(total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_de_1980_2100.csv",row.names = FALSE)


###########cataract
m_wmo <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cataract_results/WMO/cataract_daly_wmo_1980_2100_oe.csv")
m_nocontrol <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cataract_results/Nocontrol/cataract_daly_nocontrol_1980_2100_oe.csv")
m_wmo <- m_wmo[,c("year","country","age_name","sex_name","pop_country_age_sex_permillion","Y","daly_value")]
m_nocontrol <- m_nocontrol[,c("year","country","age_name","sex_name","pop_country_age_sex_permillion","Y","daly_value")]
m_wmo$scenario <- "wmo"
m_nocontrol$scenario <- "nocontrol"

colnames(m_wmo)[which(names(m_wmo) == "Y")] <- "Y_value"
colnames(m_nocontrol)[which(names(m_nocontrol) == "Y")] <- "Y_value"
library(data.table)
setDT(m_wmo)
m_wmo <- dcast(
  m_wmo,
  country + year + age_name + sex_name + pop_country_age_sex_permillion ~ scenario,
  value.var = c("Y_value","daly_value")
)
head(m_wmo)
m_nocontrol <- dcast(
  m_nocontrol,
  country + year + age_name + sex_name + pop_country_age_sex_permillion ~ scenario,
  value.var = c("Y_value","daly_value")
)
total <- merge(m_wmo,m_nocontrol,by=c("country","year","age_name","sex_name","pop_country_age_sex_permillion"),allow.cartesian = TRUE)
total$dif <- total$daly_value_nocontrol-total$daly_value_wmo
fwrite(total,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_cataract_daly_1980_2100.csv",row.names = FALSE)



###############################################calculate the uncertainty range
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

m_wmo <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/m_results/WMO/m_wmo_3000_origin_1980.csv")
m_nocontrol <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/m_results/Nocontrol/m_nocontrol_3000_origin_1980.csv")
bcc_wmo <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/bcc_results/WMO/bcc_wmo_3000_origin_1980.csv")
bcc_nocontrol <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/bcc_results/Nocontrol/bcc_nocontrol_3000_origin_1980.csv")
scc_wmo <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/scc_results/WMO/scc_wmo_3000_origin_1980.csv")
scc_nocontrol <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/scc_results/Nocontrol/scc_nocontrol_3000_origin_1980.csv")

colnames(m_wmo)[which(names(m_wmo) == "average_Y_BCC_CM")] <- "Y_value_m"
colnames(m_nocontrol)[which(names(m_nocontrol) == "average_Y_BCC_CM_no")] <- "Y_value_m_no"
colnames(bcc_wmo)[which(names(bcc_wmo) == "average_Y_BCC_CM")] <- "Y_value_bcc"
colnames(bcc_nocontrol)[which(names(bcc_nocontrol) == "average_Y_BCC_CM_no")] <- "Y_value_bcc_no"
colnames(scc_wmo)[which(names(scc_wmo) == "average_Y_SCC")] <- "Y_value_scc"
colnames(scc_nocontrol)[which(names(scc_nocontrol) == "average_Y_SCC_no")] <- "Y_value_scc_no"

m_wmo <- merge(m_wmo,pop_per_country_per_age_per_sex,by=c("year","country","age_name","sex_name"))
m_wmo$pop_country_age_sex_permillion <- m_wmo$pop_country_age_sex/(10^6)
m_wmo$number_value_m_wmo <- m_wmo$Y_value_m*m_wmo$pop_country_age_sex_permillion

m_nocontrol <- merge(m_nocontrol,pop_per_country_per_age_per_sex,by=c("year","country","age_name","sex_name"))
m_nocontrol$pop_country_age_sex_permillion <- m_nocontrol$pop_country_age_sex/(10^6)
m_nocontrol$number_value_m_nocontrol <- m_nocontrol$Y_value_m_no*m_nocontrol$pop_country_age_sex_permillion

bcc_wmo <- merge(bcc_wmo,pop_per_country_per_age_per_sex,by=c("year","country","age_name","sex_name"))
bcc_wmo$pop_country_age_sex_permillion <- bcc_wmo$pop_country_age_sex/(10^6)
bcc_wmo$number_value_bcc_wmo <- bcc_wmo$Y_value_bcc*bcc_wmo$pop_country_age_sex_permillion

bcc_nocontrol <- merge(bcc_nocontrol,pop_per_country_per_age_per_sex,by=c("year","country","age_name","sex_name"))
bcc_nocontrol$pop_country_age_sex_permillion <- bcc_nocontrol$pop_country_age_sex/(10^6)
bcc_nocontrol$number_value_bcc_nocontrol <- bcc_nocontrol$Y_value_bcc_no*bcc_nocontrol$pop_country_age_sex_permillion

scc_wmo <- merge(scc_wmo,pop_per_country_per_age_per_sex,by=c("year","country","age_name","sex_name"))
scc_wmo$pop_country_age_sex_permillion <- scc_wmo$pop_country_age_sex/(10^6)
scc_wmo$number_value_scc_wmo <- scc_wmo$Y_value_scc*scc_wmo$pop_country_age_sex_permillion

scc_nocontrol <- merge(scc_nocontrol,pop_per_country_per_age_per_sex,by=c("year","country","age_name","sex_name"))
scc_nocontrol$pop_country_age_sex_permillion <- scc_nocontrol$pop_country_age_sex/(10^6)
scc_nocontrol$number_value_scc_nocontrol <- scc_nocontrol$Y_value_scc_no*scc_nocontrol$pop_country_age_sex_permillion

total <- merge(m_wmo,m_nocontrol,by=c("year","country","age_name","sex_name","pop_country_age_sex","pop_country_age_sex_permillion","simulation"),all.x=TRUE,all.y=TRUE,allow.cartesian = TRUE)
total <- merge(total,bcc_wmo,by=c("year","country","age_name","sex_name","pop_country_age_sex","pop_country_age_sex_permillion","simulation"),all.x=TRUE,all.y=TRUE,allow.cartesian = TRUE)
total <- merge(total,bcc_nocontrol,by=c("year","country","age_name","sex_name","pop_country_age_sex","pop_country_age_sex_permillion","simulation"),all.x=TRUE,all.y=TRUE,allow.cartesian = TRUE)
total <- merge(total,scc_wmo,by=c("year","country","age_name","sex_name","pop_country_age_sex","pop_country_age_sex_permillion","simulation"),all.x=TRUE,all.y=TRUE,allow.cartesian = TRUE)
total <- merge(total,scc_nocontrol,by=c("year","country","age_name","sex_name","pop_country_age_sex","pop_country_age_sex_permillion","simulation"),all.x=TRUE,all.y=TRUE,allow.cartesian = TRUE)

total$number_val_wmo = rowSums(total[, c("number_value_m_wmo", "number_value_bcc_wmo", "number_value_scc_wmo")], na.rm = TRUE)
total$number_val_nocontrol = rowSums(total[, c("number_value_m_nocontrol", "number_value_bcc_nocontrol", "number_value_scc_nocontrol")], na.rm = TRUE)
total$dif <- total$number_val_nocontrol-total$number_val_wmo

total <- total[,c("year","country","age_name","sex_name","simulation","number_value_m_wmo", "number_value_bcc_wmo", "number_value_scc_wmo","number_value_m_nocontrol", "number_value_bcc_nocontrol", "number_value_scc_nocontrol","number_val_wmo","number_val_nocontrol","dif")]


library(foreach)
library(doParallel)
num_cores <- 6
registerDoParallel(cores = num_cores)
years <- 1980:2100
all_totals <- foreach(year = years, .combine = rbind, .packages = c("data.table")) %dopar% {
  print(year)
  m_wmo <- fread(paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/m_results/WMO/m_wmo_3000_origin_new_", year, ".csv"))
  m_nocontrol <- fread(paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/m_results/Nocontrol/m_nocontrol_3000_origin_new_", year, ".csv"))
  bcc_wmo <- fread(paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/bcc_results/WMO/bcc_wmo_3000_origin_new_", year, ".csv"))
  bcc_nocontrol <- fread(paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/bcc_results/Nocontrol/bcc_nocontrol_3000_origin_new_", year, ".csv"))
  scc_wmo <- fread(paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/scc_results/WMO/scc_wmo_3000_origin_new_", year, ".csv"))
  scc_nocontrol <- fread(paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/scc_results/Nocontrol/scc_nocontrol_3000_origin_new_", year, ".csv"))
  
  colnames(m_wmo)[which(names(m_wmo) == "average_Y_BCC_CM")] <- "Y_value_m"
  colnames(m_nocontrol)[which(names(m_nocontrol) == "average_Y_BCC_CM_no")] <- "Y_value_m_no"
  colnames(bcc_wmo)[which(names(bcc_wmo) == "average_Y_BCC_CM")] <- "Y_value_bcc"
  colnames(bcc_nocontrol)[which(names(bcc_nocontrol) == "average_Y_BCC_CM_no")] <- "Y_value_bcc_no"
  colnames(scc_wmo)[which(names(scc_wmo) == "average_Y_SCC")] <- "Y_value_scc"
  colnames(scc_nocontrol)[which(names(scc_nocontrol) == "average_Y_SCC_no")] <- "Y_value_scc_no"
  
  ##Check if Y is greater than a million. If it is, replace it with a million
  m_wmo <- m_wmo %>%
    mutate(Y_value_m = ifelse(Y_value_m > 1000000, 1000000, Y_value_m))
  m_nocontrol <- m_nocontrol %>%
    mutate(Y_value_m_no = ifelse(Y_value_m_no > 1000000, 1000000, Y_value_m_no))
  bcc_wmo <- bcc_wmo %>%
    mutate(Y_value_bcc = ifelse(Y_value_bcc > 1000000, 1000000, Y_value_bcc))
  bcc_nocontrol <- bcc_nocontrol %>%
    mutate(Y_value_bcc_no = ifelse(Y_value_bcc_no > 1000000, 1000000, Y_value_bcc_no))
  scc_wmo <- scc_wmo %>%
    mutate(Y_value_scc = ifelse(Y_value_scc > 1000000, 1000000, Y_value_scc))
  scc_nocontrol <- scc_nocontrol %>%
    mutate(Y_value_scc_no = ifelse(Y_value_scc_no > 1000000, 1000000, Y_value_scc_no))
  
  # merge data
  m_wmo <- merge(m_wmo, pop_per_country_per_age_per_sex, by=c("year","country","age_name","sex_name"))
  m_nocontrol <- merge(m_nocontrol, pop_per_country_per_age_per_sex, by=c("year","country","age_name","sex_name"))
  bcc_wmo <- merge(bcc_wmo, pop_per_country_per_age_per_sex, by=c("year","country","age_name","sex_name"))
  bcc_nocontrol <- merge(bcc_nocontrol, pop_per_country_per_age_per_sex, by=c("year","country","age_name","sex_name"))
  scc_wmo <- merge(scc_wmo, pop_per_country_per_age_per_sex, by=c("year","country","age_name","sex_name"))
  scc_nocontrol <- merge(scc_nocontrol, pop_per_country_per_age_per_sex, by=c("year","country","age_name","sex_name"))
  m_wmo$pop_country_age_sex_permillion <- m_wmo$pop_country_age_sex / (10^6)
  m_wmo$number_value_m_wmo <- m_wmo$Y_value_m * m_wmo$pop_country_age_sex_permillion
  m_nocontrol$pop_country_age_sex_permillion <- m_nocontrol$pop_country_age_sex / (10^6)
  m_nocontrol$number_value_m_nocontrol <- m_nocontrol$Y_value_m_no * m_nocontrol$pop_country_age_sex_permillion
  bcc_wmo$pop_country_age_sex_permillion <- bcc_wmo$pop_country_age_sex / (10^6)
  bcc_wmo$number_value_bcc_wmo <- bcc_wmo$Y_value_bcc * bcc_wmo$pop_country_age_sex_permillion
  bcc_nocontrol$pop_country_age_sex_permillion <- bcc_nocontrol$pop_country_age_sex / (10^6)
  bcc_nocontrol$number_value_bcc_nocontrol <- bcc_nocontrol$Y_value_bcc_no * bcc_nocontrol$pop_country_age_sex_permillion
  scc_wmo$pop_country_age_sex_permillion <- scc_wmo$pop_country_age_sex / (10^6)
  scc_wmo$number_value_scc_wmo <- scc_wmo$Y_value_scc * scc_wmo$pop_country_age_sex_permillion
  scc_nocontrol$pop_country_age_sex_permillion <- scc_nocontrol$pop_country_age_sex / (10^6)
  scc_nocontrol$number_value_scc_nocontrol <- scc_nocontrol$Y_value_scc_no * scc_nocontrol$pop_country_age_sex_permillion
  # merge all data
  total <- merge(m_wmo, m_nocontrol, by=c("year", "country", "age_name", "sex_name", "pop_country_age_sex", "pop_country_age_sex_permillion", "simulation"), all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE)
  total <- merge(total, bcc_wmo, by=c("year", "country", "age_name", "sex_name", "pop_country_age_sex", "pop_country_age_sex_permillion", "simulation"), all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE)
  total <- merge(total, bcc_nocontrol, by=c("year", "country", "age_name", "sex_name", "pop_country_age_sex", "pop_country_age_sex_permillion", "simulation"), all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE)
  total <- merge(total, scc_wmo, by=c("year", "country", "age_name", "sex_name", "pop_country_age_sex", "pop_country_age_sex_permillion", "simulation"), all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE)
  total <- merge(total, scc_nocontrol, by=c("year", "country", "age_name", "sex_name", "pop_country_age_sex", "pop_country_age_sex_permillion", "simulation"), all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE)
  total$number_val_wmo <- rowSums(total[, c("number_value_m_wmo", "number_value_bcc_wmo", "number_value_scc_wmo")], na.rm=TRUE)
  total$number_val_nocontrol <- rowSums(total[, c("number_value_m_nocontrol", "number_value_bcc_nocontrol", "number_value_scc_nocontrol")], na.rm=TRUE)
  total$dif <- total$number_val_nocontrol - total$number_val_wmo
  total <- total[, c("year", "country", "age_name", "sex_name", "pop_country_age_sex", "simulation", 
                     "number_value_m_wmo", "number_value_bcc_wmo", "number_value_scc_wmo",
                     "number_value_m_nocontrol", "number_value_bcc_nocontrol", "number_value_scc_nocontrol", 
                     "number_val_wmo", "number_val_nocontrol", "dif")]
  fwrite(total, paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence_results/total_inc_", year, ".csv"))
  return(total)
}
fwrite(all_totals, "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence_results/final_total_inc_all_years.csv")

#####################################death
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

library(foreach)
library(doParallel)
num_cores <- 10
registerDoParallel(cores = num_cores)
years <- 2091:2100
all_totals <- foreach(year = years, .combine = rbind, .packages = c("data.table")) %dopar% {
  print(year)
  m_wmo <- fread(paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/m_de_results/WMO/m_de_wmo_3000_origin_", year, ".csv"))
  m_nocontrol <- fread(paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/m_de_results/Nocontrol/m_de_nocontrol_3000_origin_", year, ".csv"))
  scc_wmo <- fread(paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/scc_de_results/WMO/scc_de_wmo_3000_origin_", year, ".csv"))
  scc_nocontrol <- fread(paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/scc_de_results/Nocontrol/scc_de_nocontrol_3000_origin_", year, ".csv"))
  
  colnames(m_wmo)[which(names(m_wmo) == "Y")] <- "Y_value_m"
  colnames(m_nocontrol)[which(names(m_nocontrol) == "Y")] <- "Y_value_m_no"
  colnames(scc_wmo)[which(names(scc_wmo) == "Y")] <- "Y_value_scc"
  colnames(scc_nocontrol)[which(names(scc_nocontrol) == "Y")] <- "Y_value_scc_no"
  
  m_wmo <- m_wmo %>%
    mutate(Y_value_m = ifelse(Y_value_m > 1000000, 1000000, Y_value_m))
  m_nocontrol <- m_nocontrol %>%
    mutate(Y_value_m_no = ifelse(Y_value_m_no > 1000000, 1000000, Y_value_m_no))
  scc_wmo <- scc_wmo %>%
    mutate(Y_value_scc = ifelse(Y_value_scc > 1000000, 1000000, Y_value_scc))
  scc_nocontrol <- scc_nocontrol %>%
    mutate(Y_value_scc_no = ifelse(Y_value_scc_no > 1000000, 1000000, Y_value_scc_no))
  
  m_wmo <- merge(m_wmo, pop_per_country_per_age_per_sex, by=c("year","country","age_name","sex_name"))
  m_nocontrol <- merge(m_nocontrol, pop_per_country_per_age_per_sex, by=c("year","country","age_name","sex_name"))
  scc_wmo <- merge(scc_wmo, pop_per_country_per_age_per_sex, by=c("year","country","age_name","sex_name"))
  scc_nocontrol <- merge(scc_nocontrol, pop_per_country_per_age_per_sex, by=c("year","country","age_name","sex_name"))
  m_wmo$pop_country_age_sex_permillion <- m_wmo$pop_country_age_sex / (10^6)
  m_wmo$number_value_m_wmo <- m_wmo$Y_value_m * m_wmo$pop_country_age_sex_permillion
  m_nocontrol$pop_country_age_sex_permillion <- m_nocontrol$pop_country_age_sex / (10^6)
  m_nocontrol$number_value_m_nocontrol <- m_nocontrol$Y_value_m_no * m_nocontrol$pop_country_age_sex_permillion
  scc_wmo$pop_country_age_sex_permillion <- scc_wmo$pop_country_age_sex / (10^6)
  scc_wmo$number_value_scc_wmo <- scc_wmo$Y_value_scc * scc_wmo$pop_country_age_sex_permillion
  scc_nocontrol$pop_country_age_sex_permillion <- scc_nocontrol$pop_country_age_sex / (10^6)
  scc_nocontrol$number_value_scc_nocontrol <- scc_nocontrol$Y_value_scc_no * scc_nocontrol$pop_country_age_sex_permillion
  total <- merge(m_wmo, m_nocontrol, by=c("year", "country", "age_name", "sex_name", "pop_country_age_sex", "pop_country_age_sex_permillion", "simulation"), all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE)
  total <- merge(total, scc_wmo, by=c("year", "country", "age_name", "sex_name", "pop_country_age_sex", "pop_country_age_sex_permillion", "simulation"), all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE)
  total <- merge(total, scc_nocontrol, by=c("year", "country", "age_name", "sex_name", "pop_country_age_sex", "pop_country_age_sex_permillion", "simulation"), all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE)
  total$number_val_wmo <- rowSums(total[, c("number_value_m_wmo", "number_value_scc_wmo")], na.rm=TRUE)
  total$number_val_nocontrol <- rowSums(total[, c("number_value_m_nocontrol", "number_value_scc_nocontrol")], na.rm=TRUE)
  total$dif <- total$number_val_nocontrol - total$number_val_wmo
  total <- total[, c("year", "country", "age_name", "sex_name", "pop_country_age_sex", "simulation", 
                     "number_value_m_wmo", "number_value_scc_wmo",
                     "number_value_m_nocontrol", "number_value_scc_nocontrol", 
                     "number_val_wmo", "number_val_nocontrol", "dif")]
  fwrite(total, paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/death_results/total_dea_control_", year, ".csv"))
  return(total)
}


##################################cataract
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


library(foreach)
library(doParallel)
num_cores <- 10
registerDoParallel(cores = num_cores)
years <- 2061:2100
all_totals <- foreach(year = years, .combine = rbind, .packages = c("data.table")) %dopar% {
  print(year)
  m_wmo <- fread(paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cataract_results/WMO/cataract_wmo_3000_origin_", year, ".csv"))
  m_nocontrol <- fread(paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cataract_results/Nocontrol/cataract_nocontrol_3000_origin_", year, ".csv"))
  
  colnames(m_wmo)[which(names(m_wmo) == "Y")] <- "Y_value"
  colnames(m_nocontrol)[which(names(m_nocontrol) == "Y")] <- "Y_value_no"

  m_wmo <- merge(m_wmo, pop_per_country_per_age_per_sex, by=c("year","country","age_name","sex_name"))
  m_nocontrol <- merge(m_nocontrol, pop_per_country_per_age_per_sex, by=c("year","country","age_name","sex_name"))
  
  m_wmo$pop_country_age_sex_permillion <- m_wmo$pop_country_age_sex / (10^6)
  m_wmo$daly_value_wmo <- m_wmo$Y_value * m_wmo$pop_country_age_sex_permillion
  m_nocontrol$pop_country_age_sex_permillion <- m_nocontrol$pop_country_age_sex / (10^6)
  m_nocontrol$daly_value_nocontrol <- m_nocontrol$Y_value_no * m_nocontrol$pop_country_age_sex_permillion
  
  m_wmo <-m_wmo[,c("year","country","age_name","sex_name","pop_country_age_sex","pop_country_age_sex_permillion","simulation","Y_value","daly_value_wmo")]
  m_nocontrol <-m_nocontrol[,c("year","country","age_name","sex_name","pop_country_age_sex","pop_country_age_sex_permillion","simulation","Y_value_no","daly_value_nocontrol")]
  
  total <- merge(m_wmo, m_nocontrol, by=c("year", "country", "age_name", "sex_name", "pop_country_age_sex", "pop_country_age_sex_permillion", "simulation"), all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE)
  total$dif <- total$daly_value_nocontrol - total$daly_value_wmo
  total <- total[, c("year", "country", "age_name", "sex_name", "pop_country_age_sex", "simulation", 
                     "daly_value_wmo", "daly_value_nocontrol", "dif")]

  fwrite(total, paste0("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cataract_total_results/cataract_total_control_", year, ".csv"))

  return(total)
}



library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
cl <- makeCluster(12)
registerDoParallel(cl)
years <- 1980:2100
# 文件路径和保存路径模板
# input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence_results/total_inc_control_"
#output_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence_results/year_sum_"
# input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/death_results/total_dea_control_"
# output_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/death_results/year_sum_"
input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cataract_total_results/cataract_total_control_"
output_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cataract_total_results/year_sum_"
foreach(year = years, .packages = c("data.table", "dplyr")) %dopar% {
  input_file <- paste0(input_path_template, year, ".csv")
  inc <- fread(input_file)
  result <- inc %>%
    group_by(year, simulation) %>%
    summarize(
      dif_sum = sum(dif, na.rm = TRUE)
    )
  output_file <- paste0(output_path_template, year, ".csv")
  fwrite(result, file = output_file)
}
stopCluster(cl)


library(data.table)
years <- 1980:2100
# input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/death_results/year_sum_"
# output_file <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/death_results/year_sum_death.csv"
input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cataract_total_results/year_sum_"
output_file <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cataract_total_results/year_sum_cataract.csv"
# input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence_results/year_sum_shuffled_"
# output_file <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence_results/year_sum_shuffled_incidence.csv"
# input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence_results/year_sum_shuffled_"
# output_file <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/incidence_results/year_sum_shuffled_incidence.csv"
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


year_sum <- fread("/WORK/genggn_work/hechangpei/STILT/TUV/age_sum/age_sum_mtkl/2025/incidence_results/year_sum_incidence.csv")
library(dplyr)
result <- year_sum %>%
  group_by(simulation) %>%
  summarize(
    total_dif_sum = sum(dif_sum, na.rm = TRUE), 
  )
print(result)
quantiles <- quantile(result$total_dif_sum, probs = c(0.025,0.5,0.975), na.rm = TRUE)
print(quantiles)


library(data.table)
library(dplyr)
number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_1980_2100.csv")
head(number)
number$pop_country_age_sex <- number$pop_country_age_sex_permillion*10^6
head(number)
filtered_number <- number %>%
  filter(
    number_value_m_nocontrol > pop_country_age_sex |
      number_value_bcc_nocontrol > pop_country_age_sex |
      number_value_scc_nocontrol > pop_country_age_sex
  )
print(filtered_number)
