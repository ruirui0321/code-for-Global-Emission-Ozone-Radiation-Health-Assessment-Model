############Output monthly radiation and see the distribution
library(data.table)
ra <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/TUV/TUV results file/No Control/1979_15th.csv")
#The following gen_raster function requires the lat,lon, and value columns
library(raster)
gen_raster <- function(value_cells, r.ext, r.res_row, r.res_col) {
  # Calculate the number of rows and columns based on resolutions
  n.rows <- ceiling((r.ext[4] - r.ext[3]) / r.res_row)
  n.cols <- ceiling((r.ext[2] - r.ext[1]) / r.res_col)
  
  # Create a raster with specified rows, columns, extent, and CRS
  value_raster <- raster(matrix(NA, nrow = n.rows, ncol = n.cols),
                         xmn = r.ext[1], xmx = r.ext[2], ymn = r.ext[3], ymx = r.ext[4],
                         crs = "+proj=longlat +datum=WGS84")
  
  # Calculate the row and column indices for each value cell
  row_indices <- ceiling((r.ext[4] - value_cells[, 1]) / r.res_row)
  col_indices <- ceiling((value_cells[, 2] - r.ext[1]) / r.res_col)
  
  # Assign values to the corresponding cells in the raster
  for (i in 1:nrow(value_cells)) {
    value_raster[row_indices[i], col_indices[i]] <- value_cells[i, 3]
  }
  return(value_raster)
}
r.ext <- c(-180, 180, -90, 90)
r.res_row <-1
r.res_col <- 1.25
#########'4' represent the SCUP-human and '10' represent the NMSC
ra <- ra[,c("10","lat","lon","month")]

quantile(ra$`10`,na.rm=TRUE)
percentiles <- seq(0.01, 1, by = 0.01)
quantile(ra$`10`,probs = percentiles,na.rm=TRUE)
library(colorRamps)
breaks <- seq(0, 6.1, 0.001)
cols <- matlab.like(length(breaks))
min_value = min(breaks)
max_value = max(breaks)
breaks_label = seq(0, 6, 0.5)
library(sp)
library(sf)
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)
world_new <- as(world_new, "Spatial")
for (j in 1:12){
  i<- 1979
  #j <- 1
  ra_m <- ra[ra$month == j, ]
  ra_m <- ra_m[,c("lat","lon","10")]
  ra_m <- data.frame(ra_m)
  r <- gen_raster(ra_m, r.ext, r.res_row,r.res_col)
  r[r<min_value] <- min_value
  r[r>max_value] <- max_value
  output_file <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/radiation_15_NMSC_nocontrol/", i, sprintf("%02d", j), ".tiff", sep = "")
  tiff(output_file, width = 640*4, height = 400*4, res = 99*4, compression = "lzw")
  tmp <- spplot(r,
                col.regions=cols, at=breaks, maxpixels=500000,
                colorkey=list(labels=list(labels=c(expression(paste("0 (",  "W/m2",")")), "0.5", "1", "1.5", "2","2.5","3","3.5","4","4.5","5","5.5","6"), at=breaks_label), space="right", width=1.2),
                xlim=c(-180,180),ylim=c(-90,90),
                panel=function(...) {
                  panel.gridplot(...)
                  sp.polygons(world_new, col="black",cex=1)
                },
                main = sprintf("%d/%02d", i, j))
  print(tmp)
  dev.off()
}

############add up the whole results in a year and see what happens for the whole year
head(ra)
get_days_in_month <- function(year, month) {
  days_in_month <- c(31, ifelse(year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0), 29, 28), 
                     31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  return(days_in_month[month])
}
# sample
# year <- 2024
# month <- 2
# days <- get_days_in_month(year, month)
# print(paste("Year:", year, ", Month:", month, ", Days:", days))
year <- 1979
ra$days <- get_days_in_month(year, ra$month)
unique(ra$days)
ra$sum_m <- ra$`10`*ra$days
library(data.table)
setDT(ra)
sum_ra <- ra[, .(sum_y = sum(sum_m)), by = .(lat, lon)]
print(sum_ra)

percentiles <- seq(0.01, 1, by = 0.01)
quantile(sum_ra$sum_y,probs = percentiles,na.rm=TRUE)
library(colorRamps)
breaks <- seq(100, 1800, 1)
cols <- matlab.like(length(breaks))
min_value = min(breaks)
max_value = max(breaks)
breaks_label = seq(200, 1800, 200)
library(sp)
library(sf)
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)
world_new <- as(world_new, "Spatial")

i<- 1979
#j <- 1
sum_ra <- sum_ra[,c("lat","lon","sum_y")]
sum_ra <- data.frame(sum_ra)
r <- gen_raster(sum_ra, r.ext, r.res_row,r.res_col)
r[r<min_value] <- min_value
r[r>max_value] <- max_value

output_file <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/radiation_annual_NMSC_nocontrol/", i,  ".tiff", sep = "")
tiff(output_file, width = 640*4, height = 400*4, res = 99*4, compression = "lzw")
tmp <- spplot(r,
              col.regions=cols, at=breaks, maxpixels=500000,
              colorkey=list(labels=list(labels=c(expression(paste("200 (",  "W/m2",")")), "400", "600", "800", "1000", "1200", "1400", "1600", "1800"), at=breaks_label), space="right", width=1.2),
              xlim=c(-180,180),ylim=c(-90,90),
              panel=function(...) {
                panel.gridplot(...)
                sp.polygons(world_new, col="black",cex=1)
              },
              main = sprintf("%d", i))
print(tmp)
dev.off()


##########################Open each year's file separately and calculate cumulatively
library(data.table)
get_days_in_month <- function(year, month) {
  days_in_month <- c(31, ifelse(year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0), 29, 28), 
                     31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  return(days_in_month[month])
}
# Vectorize the function
get_days_in_month <- Vectorize(get_days_in_month)
total_ra <- data.frame(lon = numeric(0), lat = numeric(0), sum_y = numeric(0), year = numeric(0))
folder_path <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/WMO A1"
for (year in 1958:2100){
  print(year)
  file_name <- paste0(year, "_15th.csv")
  file_path <- file.path(folder_path, file_name)
  ra <- fread(file_path)
  ra <- ra[,c("4","lat","lon","month","year")]
  ra$days <- get_days_in_month(ra$year, ra$month)
  ra$sum_m <- ra$`4`*ra$days
  library(data.table)
  setDT(ra)
  sum_ra <- ra[, .(sum_y = sum(sum_m)), by = .(lat, lon)]
  sum_ra$year <- year
  total_ra <- rbind(total_ra,sum_ra)
}
years_to_copy <- 1890:1957
rows_to_copy <- total_ra[total_ra$year == 1958, ]
new_data <- data.frame(year = rep(years_to_copy, each = nrow(rows_to_copy)), rows_to_copy[, -4])
unique(new_data$year)
merged_data <- rbind(total_ra, new_data)
write.csv(merged_data,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual.csv",row.names = FALSE)
head(merged_data)


##########################Open each year's file separately and calculate cumulatively for cataract
library(data.table)
get_days_in_month <- function(year, month) {
  days_in_month <- c(31, ifelse(year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0), 29, 28), 
                     31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  return(days_in_month[month])
}
# Vectorize the function
get_days_in_month <- Vectorize(get_days_in_month)
total_ra <- data.frame(lon = numeric(0), lat = numeric(0), sum_y = numeric(0), year = numeric(0))
folder_path <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/WMO A1"
for (year in 1958:2100){
  print(year)
  file_name <- paste0(year, "_15th.csv")
  file_path <- file.path(folder_path, file_name)
  ra <- fread(file_path)
  ra <- ra[,c("8","lat","lon","month","year")]
  ra$days <- get_days_in_month(ra$year, ra$month)
  ra$sum_m <- ra$`8`*ra$days
  library(data.table)
  setDT(ra)
  sum_ra <- ra[, .(sum_y = sum(sum_m)), by = .(lat, lon)]
  sum_ra$year <- year
  total_ra <- rbind(total_ra,sum_ra)
}
years_to_copy <- 1880:1957
rows_to_copy <- total_ra[total_ra$year == 1958, ]
new_data <- data.frame(year = rep(years_to_copy, each = nrow(rows_to_copy)), rows_to_copy[, -4])
unique(new_data$year)
merged_data <- rbind(total_ra, new_data)
write.csv(merged_data,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_cataract.csv",row.names = FALSE)
head(merged_data)