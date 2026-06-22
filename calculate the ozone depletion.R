#################################process the original TOC data from 0.5°*0.5° to 1.25°*1°
library("ncdf4")
library("data.table")
# set the file folder path
folder_path <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/original TOC data"
files <- list.files(folder_path, pattern = "*.nc", full.names = TRUE)
df_all <- data.table(lon = numeric(), lat = numeric(), toc = numeric(), year = integer(), month = integer())
for (file in files) {
  #file <- files[1]
  print(file)
  filename <- basename(file)
  year <- as.numeric(substr(filename, 1, 4))
  month <- as.numeric(substr(filename, 5, 6))
  example_nc <- nc_open(file)
  lon <- ncvar_get(example_nc, "longitude")
  lat <- ncvar_get(example_nc, "latitude")
  toc <- ncvar_get(example_nc, "total_ozone_column")
  nc_close(example_nc)
  #make the grid network
  lon_lat_grid <- expand.grid(lat = lat, lon = lon)
  toc_vector <- as.vector(t(toc))  # convert to vector
  dt <- data.table(lon_lat_grid, toc = toc_vector, year = rep(year, length(toc_vector)), month = rep(month, length(toc_vector)))
  df_all <- rbindlist(list(df_all, dt), use.names = TRUE)
}
# write to csv
fwrite(df_all, file = "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/original TOC data/toc_out.csv", row.names = FALSE)

unique(df_all$lon)
unique(df_all$lat)
# delete the data of -90 latitude
df_all <- df_all[df_all$lat != -90, ]
resample <- data.frame(lon = numeric(0), lat = numeric(0), c = numeric(0), year = numeric(0), month = numeric(0))
unique(df_all$year)
# resample the grid from 0.5°*0.5° to 1.25°*1°
for (i in 1979:1991){
  print(i)
  #i <- 1990
  for (j in 1:12){
    df <- df_all[df_all$year==i & df_all$month==j,]
    df <- data.frame(df)
    lons <- seq(-179.375, 179.375, by = 1.25)
    lats <- seq(-89.5, 89.5, by = 1)
    # resample the longitude network
    interpolated_data_lon <- lapply(split(df, df$lon), function(sub_df) {
      interpolated_values <- approx(sub_df$lat, sub_df$toc, xout = lats, method = "linear", rule = 2)$y
      result <- data.frame(lat = lats, lon = sub_df$lon[1], toc = interpolated_values, 
                           year = sub_df$year[1], month = sub_df$month[1])
      return(result)
    })
    df_interpolated_lon <- do.call(rbind, interpolated_data_lon)
    
    # resample the latitude network
    interpolated_data_lat <- lapply(split(df_interpolated_lon, df_interpolated_lon$lat), function(sub_df) {
      interpolated_values <- approx(sub_df$lon, sub_df$toc, xout = lons, method = "linear", rule = 2)$y
      result <- data.frame(lat = sub_df$lat[1], lon = lons, toc = interpolated_values, 
                           year = sub_df$year[1], month = sub_df$month[1])
      return(result)
    })
    df_interpolated_lat <- do.call(rbind, interpolated_data_lat)
    resample <- rbind(resample, df_interpolated_lat)
  }
  
}
write.csv(resample,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/original TOC data/toc_resample_125_1.csv",row.names = FALSE)

# drawing the distribution of original and resampled toc
library(data.table)
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
r.res_row <- 0.5
r.res_col <- 0.5

df_all <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/original TOC data/toc_out.csv")
head(df_all)
quantile(df_all$toc,na.rm=TRUE)
library(colorRamps)
breaks <- seq(135, 545, 1)
cols <- matlab.like(length(breaks))
min_value = min(breaks)
max_value = max(breaks)
breaks_label = seq(140, 540, 50)
library(sp)
library(sf)
# load the map
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)
world_new <- as(world_new,"Spatial")
for (i in 1979:1991){
  for (j in 1:12){
    df <- df_all[df_all$year == i & df_all$month == j, ]
    df <- df[,c("lat","lon","toc")]
    df <- data.frame(df)
    r <- gen_raster(df, r.ext, r.res_row,r.res_col)
    r[r<min_value] <- min_value
    r[r>max_value] <- max_value
    output_file <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/original toc data/", i, sprintf("%02d", j), ".tiff", sep = "")
    tiff(output_file, width = 640*4, height = 400*4, res = 99*4, compression = "lzw")
    tmp <- spplot(r,
                  col.regions=cols, at=breaks, maxpixels=500000,
                  colorkey=list(labels=list(labels=c(expression(paste("140 (",  "DU",")")), "190", "240", "290","340","390","440","490","540"
                                                     ), at=breaks_label), space="right", width=1.2),
                  xlim=c(-180,180),ylim=c(-90,90),
                  panel=function(...) {
                    panel.gridplot(...)
                    sp.polygons(world_new, col="black",cex=1)
                  },
                  main = sprintf("%d/%02d", i, j))
    print(tmp)
    dev.off()
  }
}

r.res_row <- 1
r.res_col <- 1.25
df_all <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/original TOC data/toc_resample_125_1.csv")
head(df_all)
library(colorRamps)
breaks <- seq(135, 545, 1)
cols <- matlab.like(length(breaks))
min_value = min(breaks)
max_value = max(breaks)
breaks_label = seq(140, 540, 50)
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
world_new <- as(world_new,"Spatial")

for (i in 1979:1991){
  for (j in 1:12){
    print(i)
    print(j)
    df <- df_all[df_all$year == i & df_all$month == j, ]
    df <- df[,c("lat","lon","toc")]
    df <- data.frame(df)
    r <- gen_raster(df, r.ext, r.res_row,r.res_col)
    r[r<min_value] <- min_value
    r[r>max_value] <- max_value
    output_file <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/resampled toc data/", i, sprintf("%02d", j), ".tiff", sep = "")
    tiff(output_file, width = 640*4, height = 400*4, res = 99*4, compression = "lzw")
    tmp <- spplot(r,
                  col.regions=cols, at=breaks, maxpixels=500000,
                  colorkey=list(labels=list(labels=c(expression(paste("140 (",  "DU",")")), "190", "240", "290","340","390","440","490","540"), at=breaks_label), space="right", width=1.2),
                  xlim=c(-180,180),ylim=c(-90,90),
                  panel=function(...) {
                    panel.gridplot(...)
                    sp.polygons(world_new, col="black",cex=1)
                  },
                  main = sprintf("%d/%02d", i, j))
    print(tmp)
    dev.off()
  }
}

#####################################################################################################################################
# project the toc according to EESC
# WMO A1
library(data.table)
df_all<-fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/original TOC data/toc_resample_125_1.csv")
df_all
df_all$EESC<- 0.0
df_all$EESC[df_all$year==1979]<-1100.3765
df_all$EESC[df_all$year==1980]<-1157.9
df_all$EESC[df_all$year==1981]<-1218.183
df_all$EESC[df_all$year==1982]<-1281.716
df_all$EESC[df_all$year==1983]<-1327.602
df_all$EESC[df_all$year==1984]<-1377.157
df_all$EESC[df_all$year==1985]<-1426.1925
df_all$EESC[df_all$year==1986]<-1471.3385
df_all$EESC[df_all$year==1987]<-1520.8365
df_all$EESC[df_all$year==1988]<-1570.9385
df_all$EESC[df_all$year==1989]<-1622.0335
df_all$EESC[df_all$year==1990]<-1672.467
df_all$EESC[df_all$year==1991]<-1735.103
df_all$Month_Group <- cut(df_all$month, breaks = 12, labels = FALSE)
# generate an enpty dataframe
df_trend <- data.frame(lon = numeric(0), lat = numeric(0), avg_TOC_EESC_Ratio = numeric(0), month = numeric(0))
library(dplyr)
for (month_group in unique(df_all$Month_Group)) {
  #month_group <- 1
  subset_data <- df_all[df_all$Month_Group == month_group, ]
  # calculate the difference of toc and EESC between adjacent years, and it begins at 1980, that is, the difference between 1980 and 1979
  result_df <- subset_data %>%
    arrange(lon, lat, year) %>%
    group_by(lon, lat) %>%
    mutate(
      toc_diff = toc - lag(toc),
      EESC_diff = EESC - lag(EESC)
    ) %>%
    ungroup()
  result_df <- data.frame(result_df)
  result_df$TOC_EESC_Ratio <- result_df$toc_diff / result_df$EESC_diff
  result_df <- na.omit(result_df)
  # calculate the average value of TOC_EESC_Ratio of the group of specific longitude and latitude
  average_TOC_EESC_Ratio <- result_df %>%
    group_by(lon, lat) %>%
    summarize(avg_TOC_EESC_Ratio = mean(TOC_EESC_Ratio, na.rm = TRUE))
  average_TOC_EESC_Ratio <- data.frame(average_TOC_EESC_Ratio)
  average_TOC_EESC_Ratio$month <- month_group
  df_trend <- rbind(df_trend,average_TOC_EESC_Ratio)
}

# draw to see the ratio every month and everay grid
percentiles <- seq(0.01, 1, by = 0.01)
quantile(df_trend$avg_TOC_EESC_Ratio,probs = percentiles,na.rm=TRUE)
library(colorRamps)
breaks <- seq(-0.131, 0.031, 0.00001)
cols <- matlab.like(length(breaks))
min_value = min(breaks)
max_value = max(breaks)
breaks_label = seq(-0.13, 0.03, 0.01)
library(rgdal)
library(sp)
library(sf)
shapefile <- readOGR(dsn = "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
for (i in 1:12){
  df <- df_trend[df_trend$month == i,]
  df <- df[,c("lat","lon","avg_TOC_EESC_Ratio")]
  df <- data.frame(df)
  r <- gen_raster(df, r.ext, r.res_row,r.res_col)
  r[r<min_value] <- min_value
  r[r>max_value] <- max_value
  output_file <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/avg_TOC_EESC_Ratio/", i, ".tiff", sep = "")
  tiff(output_file, width = 640*4, height = 400*4, res = 99*4, compression = "lzw")
  tmp <- spplot(r,
                col.regions=cols, at=breaks, maxpixels=500000,
                colorkey=list(labels=list(labels=c(expression(paste("-0.13 (",  "",")")),"-0.12","-0.11", "-0.1", "-0.09", "-0.08", "-0.07","-0.06","-0.05","-0.04","-0.03","-0.02","-0.01","0","0.01","0.02","0.03"
                ), at=breaks_label), space="right", width=1.2),
                xlim=c(-180,180),ylim=c(-90,90),
                panel=function(...) {
                  panel.gridplot(...)
                  sp.polygons(shapefile, col="black",cex=1)
                },
                main = sprintf("%02d", i))
  print(tmp)
  dev.off()
}

# project the toc after 1991
toc_pre <- merge(df_all, df_trend, by = c( "lon", "lat", "month"))
library(tidyr)
toc_pre <- toc_pre %>%
  complete(year = 1979:2100, month = 1:12, lon, lat, fill = list(eesc = NA_real_))
tail(toc_pre)
toc_pre <- data.frame(toc_pre)
# fill the eesc values from Atmospheric mixing ratios (in ppt) and EESC of the ODSs considered in the WMO A1 scenario.xlsx
eesc_values <- c(
  1796.0095,1850.505,1901.1895,1935.398,1956.346,
  1961.256,1965.098,1941.498,1916.6945,1916.346,1905.722,1879.381,1843.769,1817.1145,
  1800.1305,1780.943,1774.032,1757.832,1740.695,1723.7425,1704.549,1685.252,1670.936,
  1660.5825,1645.8205,1625.8085,1618.4125,1613.92,1596.639,1579.641,1566.2025,1557.1025,
  1545.2925,1536.762,1524.899,1512.5255,1500.422,1487.6465,1474.914,1462.171,1448.791,
  1435.37,1421.772,1407.757,1393.899,1380.2575,1366.707,1352.6815,1339.818,1326.78,
  1313.524,1300.0115,1288.151,1275.34,1262.291,1250.9075,1238.282,1227.1765,1214.993,
  1203.8415,1192.068,1181.429,1170.134,1160.143,1149.165,1139.227,1128.388,1119.2865,
  1109.5185,1099.797,1090.6595,1080.922,1072.6375,1063.807,1055.3665,1046.6785,1038.538,
  1030.643,1022.1465,1014.5135,1007.1355,999.7925,991.8305,984.8665,977.9095,971.1065,
  964.3855,957.3,950.698,944.6905,938.1785,932.181,925.7675,919.6255,914.222,908.5375,
  902.975,897.6615,892.213,886.2585,881.192,876.4135,871.143,866.1525,861.305,856.4465,
  852.3555,847.538,842.9245)
for (j in 1992:2100) {
  indices <- (j - 1992) %% length(eesc_values) + 1
  toc_pre$EESC[toc_pre$year == j] <- eesc_values[indices]
}
tail(toc_pre)
toc_pre$Month_Group <- toc_pre$month
# fill the avg_TOC_EESC_Ratio column from previous year
toc_pre <- toc_pre %>%
  group_by(lon, lat, month) %>%
  fill(avg_TOC_EESC_Ratio, .direction = "downup") %>%
  ungroup()
toc_pre <- data.frame(toc_pre)
tail(toc_pre)
# calculate the delta EESC by minus the EESC of 1980, which is 1157.9
toc_pre$d_eesc <- toc_pre$EESC-1157.9
quantile(toc_pre$d_eesc)

# calculate the toc after 1991
toc_pre <- toc_pre %>%
  group_by(lon, lat) %>%
  mutate(new_toc = ifelse(year >= 1992, toc[year == 1980] + avg_TOC_EESC_Ratio * d_eesc, toc)) %>%
  ungroup() %>%
  select(-toc) %>%
  rename(toc = new_toc)
tail(toc_pre)
toc_pre <- data.frame(toc_pre)
unique(toc_pre$year)
df_all <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/original TOC data/toc_resample_125_1.csv")
quantile(df_all$toc)
quantile(toc_pre$toc)
# control the range of projected toc, the min can not under 100 DU and the max can not above the max of toc in 1979-1991, which is 545 DU
toc_pre$toc[toc_pre$toc < 100] <- 100
toc_pre$toc[toc_pre$toc > 545] <- 545
toc_pre <- toc_pre[,c("year","month","lon","lat","toc")]
write.csv(toc_pre,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data/toc_pre_WMO_1979_2100.csv",row.names = FALSE)

# drawing to see the distribution of toc projected
library(data.table)
toc_pre <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data_125_1/toc_pre_WMO_1979_2100.csv")
quantile(toc_pre$toc,na.rm=TRUE)
library(colorRamps)
breaks <- seq(100, 545, 1)
cols <- matlab.like(length(breaks))
min_value = min(breaks)
max_value = max(breaks)
breaks_label = seq(100, 540, 40)
library(rgdal)
library(sp)
library(sf)
shapefile <- readOGR(dsn = "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
for (i in 2050:2100){
  for (j in 1:12){
    df <- toc_pre[toc_pre$year == i & toc_pre$month == j, ]
    df <- df[,c("lat","lon","toc")]
    df <- data.frame(df)
    r <- gen_raster(df, r.ext, r.res_row,r.res_col)
    r[r<min_value] <- min_value
    r[r>max_value] <- max_value
    output_file <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/projected toc data/", i, sprintf("%02d", j), ".tiff", sep = "")
    tiff(output_file, width = 640*4, height = 400*4, res = 99*4, compression = "lzw")
    tmp <- spplot(r,
                  col.regions=cols, at=breaks, maxpixels=500000,
                  colorkey=list(labels=list(labels=c(expression(paste("100 (",  "DU",")")), "140", "180", "220", "260","300","340","380","420","460","500","540"
                  ), at=breaks_label), space="right", width=1.2),
                  xlim=c(-180,180),ylim=c(-90,90),
                  panel=function(...) {
                    panel.gridplot(...)
                    sp.polygons(shapefile, col="black",cex=1)
                  },
                  main = sprintf("%d/%02d", i, j))
    print(tmp)
    dev.off()
  }
}

###############################################################################################################################
# process some other input data of tuv model
# elevation data
library("ncdf4")
library("data.table")
example_nc <- nc_open("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/elevation data/ETOPO_2022_v1_60s_N90W180_surface.nc")
lon <- ncvar_get(example_nc, "lon")
lat <- ncvar_get(example_nc, "lat")
elevation <- ncvar_get(example_nc, "z")
dim(elevation)
quantile(elevation)
nc_close(example_nc)
#head(elevation)
lon_lat_grid <- expand.grid(lat = lat, lon = lon)
ele_vector <- as.vector(t(elevation))  # convert to vector
dt <- data.table(lon_lat_grid, ele = ele_vector)
head(dt)

lons <- seq(-179.375, 179.375, by = 1.25)
lats <- seq(-89.5, 89.5, by = 1)
# resample by lontitude
interpolated_data_lon <- lapply(split(dt, dt$lon), function(sub_df) {
  interpolated_values <- approx(sub_df$lat, sub_df$ele, xout = lats, method = "linear", rule = 2)$y
  result <- data.frame(lat = lats, lon = sub_df$lon[1], ele = interpolated_values)
  return(result)
})
df_interpolated_lon <- do.call(rbind, interpolated_data_lon)
# resample by latitude
interpolated_data_lat <- lapply(split(df_interpolated_lon, df_interpolated_lon$lat), function(sub_df) {
  interpolated_values <- approx(sub_df$lon, sub_df$ele, xout = lons, method = "linear", rule = 2)$y
  result <- data.frame(lat = sub_df$lat[1], lon = lons, ele = interpolated_values)
  return(result)
})
df_interpolated_lat <- do.call(rbind, interpolated_data_lat)
resample <- rbind(resample, df_interpolated_lat)
write.csv(resample,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data/resample_ele.csv")


# project toc data of 1958 to 1978
# WMO A1 scenario
library(data.table)
df_all<-fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/original TOC data/toc_resample_125_1.csv")
df_all$EESC<- 0.0
df_all$EESC[df_all$year==1979]<-1100.3765
df_all$EESC[df_all$year==1980]<-1157.9
df_all$EESC[df_all$year==1981]<-1218.183
df_all$EESC[df_all$year==1982]<-1281.716
df_all$EESC[df_all$year==1983]<-1327.602
df_all$EESC[df_all$year==1984]<-1377.157
df_all$EESC[df_all$year==1985]<-1426.1925
df_all$EESC[df_all$year==1986]<-1471.3385
df_all$EESC[df_all$year==1987]<-1520.8365
df_all$EESC[df_all$year==1988]<-1570.9385
df_all$EESC[df_all$year==1989]<-1622.0335
df_all$EESC[df_all$year==1990]<-1672.467
df_all$EESC[df_all$year==1991]<-1735.103
df_all$Month_Group <- cut(df_all$month, breaks = 12, labels = FALSE)
df_trend <- data.frame(lon = numeric(0), lat = numeric(0), avg_TOC_EESC_Ratio = numeric(0), month = numeric(0))
library(dplyr)
for (month_group in unique(df_all$Month_Group)) {
  subset_data <- df_all[df_all$Month_Group == month_group, ]
  result_df <- subset_data %>%
    arrange(lon, lat, year) %>%
    group_by(lon, lat) %>%
    mutate(
      toc_diff = toc - lag(toc),
      EESC_diff = EESC - lag(EESC)
    ) %>%
    ungroup()
  result_df <- data.frame(result_df)
  result_df$TOC_EESC_Ratio <- result_df$toc_diff / result_df$EESC_diff
  result_df <- na.omit(result_df)
  average_TOC_EESC_Ratio <- result_df %>%
    group_by(lon, lat) %>%
    summarize(avg_TOC_EESC_Ratio = mean(TOC_EESC_Ratio, na.rm = TRUE))
  average_TOC_EESC_Ratio <- data.frame(average_TOC_EESC_Ratio)
  average_TOC_EESC_Ratio$month <- month_group
  df_trend <- rbind(df_trend,average_TOC_EESC_Ratio)
}
toc_pre <- merge(df_all, df_trend, by = c( "lon", "lat", "month"))
library(tidyr)
# the beginning year was set to be 1958, cause the EESC begins in 1958, the toc before 1958 keep the same as values in 1958
toc_pre <- toc_pre %>%
  complete(year = 1958:1991, month = 1:12, lon, lat, fill = list(eesc = NA_real_))
tail(toc_pre)
toc_pre <- data.frame(toc_pre)
eesc_values <- c(
  551.03,559.348,568.899,582.633,593.147,608.159,620.523,637.71,653.301,673.741,692.215,715.954,741.747,
  767.7735,798.3555,829.169,863.9365,902.531,942.269,991.1145,1048.5855)
for (j in 1958:1978) {
  indices <- (j - 1958) %% length(eesc_values) + 1
  toc_pre$EESC[toc_pre$year == j] <- eesc_values[indices]
}
toc_pre$Month_Group <- toc_pre$month
toc_pre <- toc_pre %>%
  group_by(lon, lat, month) %>%
  fill(avg_TOC_EESC_Ratio, .direction = "downup") %>%
  ungroup()
toc_pre <- data.frame(toc_pre)
tail(toc_pre)
# the baseline is also the EESC in 1980
toc_pre$d_eesc <- toc_pre$EESC-1157.9
toc_pre <- toc_pre %>%
  group_by(lon, lat) %>%
  mutate(new_toc = ifelse(year <= 1978, toc[year == 1980] + avg_TOC_EESC_Ratio * d_eesc, toc)) %>%
  ungroup() %>%
  select(-toc) %>%
  rename(toc = new_toc)
toc_pre
toc_pre <- data.frame(toc_pre)
quantile(toc_pre$toc)
toc_pre$toc[toc_pre$toc < 100] <- 100
toc_pre$toc[toc_pre$toc > 545] <- 545
quantile(toc_pre$toc)
percentiles <- seq(0.01, 1, by = 0.01)
quantile(toc_pre$toc,probs = percentiles,na.rm=TRUE)
toc_pre <- toc_pre[,c("year","month","lon","lat","toc")]
unique(toc_pre$year)
toc_pre <- toc_pre[toc_pre$year<=1978,]
write.csv(toc_pre,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data/toc_pre_WMO_1958_1978.csv",row.names = FALSE)


# combime the input data of WMO A1 scenario, include the toc data and the elevation data
toc_1958_1978 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data/toc_pre_WMO_1958_1978.csv")
toc_1979_2100 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data/toc_pre_WMO_1979_2100.csv")
total_toc <- rbind(toc_1958_1978,toc_1979_2100)
unique(total_toc$year)
ele <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data/resample_ele.csv")
# change the m to km
ele$ele <- ele$ele/1000
total <- merge(total_toc,ele,by=c("lat","lon"))

# filter the data by population grid
library(rgdal)
library(sp)
library(sf)
library(ggplot2)
library(dplyr)
library(data.table)
shapefile <- readOGR(dsn = "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
library(raster)
SSP2_2020 <- raster("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/SSP2/SSP2_2020.tif")
xmin <- -180
xmax <- 180
ymin <- -90
ymax <- 90
res_x <- 1.25
res_y <- 1
# define new raster template
template_raster <- raster(extent(-180, 180, -90, 90), resolution=c(1.25, 1), crs=crs(SSP1_2025))
# obtain the resolution of raster
resolutions <- res(SSP2_2020)
res_x <- resolutions[1]
res_y <- resolutions[2]
# calculate the factor of a new resolution
factor_x <- ceiling(1.25 / res_x)
factor_y <- ceiling(1.0 / res_y)
# print the factor
print(paste("Factor X:", factor_x))
print(paste("Factor Y:", factor_y))
# use the aggregate function to calculate
aggregated_raster <- aggregate(SSP2_2020, c(factor_x, factor_y), fun=mean, expand=TRUE, na.rm=TRUE)
# convert to sf
shapefile_sf <- st_as_sf(shapefile)
# convert to dataframe
raster_df <- as.data.frame(aggregated_raster, xy=TRUE)
colnames(raster_df) <- c("lon", "lat", "population")
raster_df <- na.omit(raster_df)
ggplot() +
  geom_raster(data = raster_df, aes(x = lon, y = lat, fill = population), interpolate = TRUE) +
  geom_sf(data = shapefile_sf, fill = NA, color = "black") +
  scale_fill_viridis_c(option = "D") +
  labs(title = "World Population Distribution with Country Borders") +
  theme_minimal()

unique(raster_df$lon)
unique(raster_df$lat)
# adjust the lat to proximate 0.5°
raster_df$lat <- round(raster_df$lat * 2) / 2
raster_df <- raster_df[,c("lon","lat")]
write.csv(raster_df,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/SSP2_2020_grid.csv",row.names = FALSE)

grid <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/SSP2_2020_grid.csv")
setDT(total)
setDT(grid)
# set the key of datatable
setkey(total, lon, lat)
setkey(grid, lon, lat)
# match
filtered_total <- grid[total, nomatch = 0]  # nomatch=0 Indicates that rows that do not match are not returned
head(filtered_total)
# zstart is Surface elevation, km, above sea level. Must be a positive value.
filtered_total$ele[filtered_total$ele < 0] <- 0
write.csv(filtered_total,file = "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data/WMO_tuv_inp.csv",row.names = FALSE)

#####################################################################################################################################
# make the tuv model input of No Control scenario, see the file Emissions and atmospheric mixing ratios and EESC of the ODSs considered in the No Control scenario.xlsx
library(data.table)
df_all<-fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/original TOC data/toc_resample_125_1.csv")
quantile(df_all$toc,na.rm=TRUE)
df_all$EESC<- 0.0
df_all$EESC[df_all$year==1979]<-1100.3765
df_all$EESC[df_all$year==1980]<-1157.9
df_all$EESC[df_all$year==1981]<-1218.183
df_all$EESC[df_all$year==1982]<-1281.716
df_all$EESC[df_all$year==1983]<-1327.602
df_all$EESC[df_all$year==1984]<-1377.157
df_all$EESC[df_all$year==1985]<-1426.1925
df_all$EESC[df_all$year==1986]<-1471.3385
df_all$EESC[df_all$year==1987]<-1520.8365
df_all$EESC[df_all$year==1988]<-1570.9385
df_all$EESC[df_all$year==1989]<-1622.0335
df_all$EESC[df_all$year==1990]<-1672.467
df_all$EESC[df_all$year==1991]<-1735.103
df_all$Month_Group <- cut(df_all$month, breaks = 12, labels = FALSE)
df_trend <- data.frame(lon = numeric(0), lat = numeric(0), avg_TOC_EESC_Ratio = numeric(0), month = numeric(0))
library(dplyr)
for (month_group in unique(df_all$Month_Group)) {
  subset_data <- df_all[df_all$Month_Group == month_group, ]
  result_df <- subset_data %>%
    arrange(lon, lat, year) %>%
    group_by(lon, lat) %>%
    mutate(
      toc_diff = toc - lag(toc),
      EESC_diff = EESC - lag(EESC)
    ) %>%
    ungroup()
  result_df <- data.frame(result_df)
  result_df$TOC_EESC_Ratio <- result_df$toc_diff / result_df$EESC_diff
  result_df <- na.omit(result_df)
  average_TOC_EESC_Ratio <- result_df %>%
    group_by(lon, lat) %>%
    summarize(avg_TOC_EESC_Ratio = mean(TOC_EESC_Ratio, na.rm = TRUE))
  average_TOC_EESC_Ratio <- data.frame(average_TOC_EESC_Ratio)
  average_TOC_EESC_Ratio$month <- month_group
  df_trend <- rbind(df_trend,average_TOC_EESC_Ratio)
}

# drawing to see the distribution of ratio
percentiles <- seq(0.01, 1, by = 0.01)
quantile(df_trend$avg_TOC_EESC_Ratio,probs = percentiles,na.rm=TRUE)
library(colorRamps)
breaks <- seq(-0.125, 0.035, 0.00001)
cols <- matlab.like(length(breaks))
min_value = min(breaks)
max_value = max(breaks)
breaks_label = seq(-0.12, 0.03, 0.01)
library(rgdal)
library(sp)
library(sf)
shapefile <- readOGR(dsn = "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
for (i in 1:12){
  #i <- 1
  df <- df_trend[df_trend$month == i,]
  df <- df[,c("lat","lon","avg_TOC_EESC_Ratio")]
  df <- data.frame(df)
  r <- gen_raster(df, r.ext, r.res_row,r.res_col)
  r[r<min_value] <- min_value
  r[r>max_value] <- max_value
  output_file <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/avg_TOC_EESC_Ratio of No Control/", i, ".tiff", sep = "")
  tiff(output_file, width = 640*4, height = 400*4, res = 99*4, compression = "lzw")
  tmp <- spplot(r,
                col.regions=cols, at=breaks, maxpixels=500000,
                colorkey=list(labels=list(labels=c(expression(paste("-0.12 (",  "",")")),"-0.11", "-0.1", "-0.09", "-0.08", "-0.07","-0.06","-0.05","-0.04","-0.03","-0.02","-0.01","0","0.01","0.02","0.03"
                ), at=breaks_label), space="right", width=1.2),
                xlim=c(-180,180),ylim=c(-90,90),
                panel=function(...) {
                  panel.gridplot(...)
                  sp.polygons(shapefile, col="black",cex=1)
                },
                main = sprintf("%02d", i))
  print(tmp)
  dev.off()
}
toc_pre <- merge(df_all, df_trend, by = c( "lon", "lat", "month"))
library(tidyr)
toc_pre <- toc_pre %>%
  complete(year = 1992:2100, month = 1:12, lon, lat, fill = list(eesc = NA_real_))
tail(toc_pre)
toc_pre <- data.frame(toc_pre)
eesc_values <- c(
  1145.623887,1198.135909,1251.394301,1305.446022,1360.441537,1416.401647,1474.336827,1534.225726,1596.164957,
  1660.15238,1726.265809,1794.509308,1864.895944,
  1937.436142,2012.094901,2088.968717,2166.99437,2246.299495,2327.009644,2414.61682,2491.15992,2572.029114,
  2667.906171,2760.740738,2845.858582,2932.533809,3025.069054,3123.939658,3221.261824,3322.968766,3424.742843,
  3531.859687,3641.060329,3750.935307,3863.244786,3976.450661,4095.704665,4220.440474,4343.581809,4474.598535,
  4609.85077,4741.608979,4875.170077,5016.581536,5161.641482,5310.462804,5463.161255,5619.855561,5780.66753,
  5945.722162,6115.147757,6289.076037,6467.642254,6650.985315,6839.247901,7032.576593,7231.121996,7435.038871,
  7644.486268,7859.627664,8080.631099,8307.669322,8540.91994,8780.565565,9026.79397,9279.798249,9539.776983,
  9806.934401,10081.48056,10363.63151,10653.6095,10951.64314,11257.9676,11572.82481,11896.46368,12229.14027,
  12571.11805,12922.66809,13284.06929,13655.60865,14037.58144,14430.29153,14834.05159,15249.18336,15676.01794,
  16114.89606,16566.16836,17030.19568,17507.34937,17998.01163,18502.57576,19021.44658,19555.04069,20103.78688,
  20668.12646,21248.51362,21845.41586,22459.31434,23090.70428,23740.09544,24408.01247,25094.99541,25801.60009,
  26528.39869,27275.9801,28044.95054,28835.93397,29649.57269,30486.52784,31347.47997,32233.12963,33144.19793,
  34081.42719,35045.58154,36037.44757,37057.835,38107.57736,39187.53272,40298.58437,41441.64163,42617.64057,
  43827.5448
)
for (j in 1979:2100) {
  indices <- (j - 1979) %% length(eesc_values) + 1
  toc_pre$EESC[toc_pre$year == j] <- eesc_values[indices]
}
toc_pre$Month_Group <- toc_pre$month
toc_pre <- toc_pre %>%
  group_by(lon, lat, month) %>%
  fill(avg_TOC_EESC_Ratio, .direction = "downup") %>%
  ungroup()
toc_pre <- data.frame(toc_pre)
tail(toc_pre)
toc_pre$d_eesc <- toc_pre$EESC-1198.135909

# project the toc after 1991
toc_pre <- toc_pre %>%
  group_by(lon, lat) %>%
  mutate(new_toc = ifelse(year >= 1979, toc[year == 1980] + avg_TOC_EESC_Ratio * d_eesc, toc)) %>%
  ungroup() %>%
  select(-toc) %>%
  rename(toc = new_toc)
tail(toc_pre)
toc_pre <- data.frame(toc_pre)
percentiles <- seq(0.01, 1, by = 0.01)
quantile(toc_pre$toc,probs = percentiles,na.rm=TRUE)
# control the range of projected toc, the min can not under 100 DU and the max can not above the max of toc in 1979-1991, which is 545 DU
toc_pre$toc[toc_pre$toc < 100] <- 100
toc_pre$toc[toc_pre$toc > 545] <- 545

breaks <- seq(100, 545, 1)
cols <- matlab.like(length(breaks))
min_value = min(breaks)
max_value = max(breaks)
breaks_label = seq(100, 540, 40)
library(rgdal)
library(sp)
library(sf)
shapefile <- readOGR(dsn = "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
for (i in 2050:2100){
  for (j in 1:12){
    df <- toc_pre[toc_pre$year == i & toc_pre$month == j, ]
    df <- df[,c("lat","lon","toc")]
    df <- data.frame(df)
    r <- gen_raster(df, r.ext, r.res_row,r.res_col)
    r[r<min_value] <- min_value
    r[r>max_value] <- max_value
    output_file <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/projected toc data of No Control/", i, sprintf("%02d", j), ".tiff", sep = "")
    tiff(output_file, width = 640*4, height = 400*4, res = 99*4, compression = "lzw")
    tmp <- spplot(r,
                  col.regions=cols, at=breaks, maxpixels=500000,
                  colorkey=list(labels=list(labels=c(expression(paste("100 (",  "DU",")")), "140", "180", "220", "260","300","340","380","420","460","500","540"
                  ), at=breaks_label), space="right", width=1.2),
                  xlim=c(-180,180),ylim=c(-90,90),
                  panel=function(...) {
                    panel.gridplot(...)
                    sp.polygons(shapefile, col="black",cex=1)
                  },
                  main = sprintf("%d/%02d", i, j))
    print(tmp)
    dev.off()
  }
}
toc_pre <- toc_pre[,c("year","month","lon","lat","toc")]
write.csv(toc_pre,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data/toc_pre_Nocontrol_1979_2100.csv",row.names = FALSE)

# project toc data of 1958 to 1978
# No Control scenario
library(data.table)
df_all<-fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/original TOC data/toc_resample_125_1.csv")
df_all$EESC<- 0.0
df_all$EESC[df_all$year==1979]<-1100.3765
df_all$EESC[df_all$year==1980]<-1157.9
df_all$EESC[df_all$year==1981]<-1218.183
df_all$EESC[df_all$year==1982]<-1281.716
df_all$EESC[df_all$year==1983]<-1327.602
df_all$EESC[df_all$year==1984]<-1377.157
df_all$EESC[df_all$year==1985]<-1426.1925
df_all$EESC[df_all$year==1986]<-1471.3385
df_all$EESC[df_all$year==1987]<-1520.8365
df_all$EESC[df_all$year==1988]<-1570.9385
df_all$EESC[df_all$year==1989]<-1622.0335
df_all$EESC[df_all$year==1990]<-1672.467
df_all$EESC[df_all$year==1991]<-1735.103
df_all$Month_Group <- cut(df_all$month, breaks = 12, labels = FALSE)
df_trend <- data.frame(lon = numeric(0), lat = numeric(0), avg_TOC_EESC_Ratio = numeric(0), month = numeric(0))
library(dplyr)
for (month_group in unique(df_all$Month_Group)) {
  subset_data <- df_all[df_all$Month_Group == month_group, ]
  result_df <- subset_data %>%
    arrange(lon, lat, year) %>%
    group_by(lon, lat) %>%
    mutate(
      toc_diff = toc - lag(toc),
      EESC_diff = EESC - lag(EESC)
    ) %>%
    ungroup()
  result_df <- data.frame(result_df)
  result_df$TOC_EESC_Ratio <- result_df$toc_diff / result_df$EESC_diff
  result_df <- na.omit(result_df)
  average_TOC_EESC_Ratio <- result_df %>%
    group_by(lon, lat) %>%
    summarize(avg_TOC_EESC_Ratio = mean(TOC_EESC_Ratio, na.rm = TRUE))
  average_TOC_EESC_Ratio <- data.frame(average_TOC_EESC_Ratio)
  average_TOC_EESC_Ratio$month <- month_group
  df_trend <- rbind(df_trend,average_TOC_EESC_Ratio)
}
toc_pre <- merge(df_all, df_trend, by = c( "lon", "lat", "month"))
library(tidyr)
# the beginning year was set to be 1958, cause the EESC begins in 1958, the toc before 1958 keep the same as values in 1958
toc_pre <- toc_pre %>%
  complete(year = 1958:1991, month = 1:12, lon, lat, fill = list(eesc = NA_real_))
tail(toc_pre)
toc_pre <- data.frame(toc_pre)
eesc_values <- c(551.03,571.4454987,586.2586796,600.2202016,614.5851927,629.7383274,645.7057732,663.5463434,683.1322909,704.325391,727.0578927,751.2294036,
778.4604103,808.4248969,840.7976932,875.532416,912.3523722,953.0522856,997.1064406,1044.153178,1094.081695)
for (j in 1958:1978) {
  indices <- (j - 1958) %% length(eesc_values) + 1
  toc_pre$EESC[toc_pre$year == j] <- eesc_values[indices]
}
toc_pre$Month_Group <- toc_pre$month
toc_pre <- toc_pre %>%
  group_by(lon, lat, month) %>%
  fill(avg_TOC_EESC_Ratio, .direction = "downup") %>%
  ungroup()
toc_pre <- data.frame(toc_pre)
tail(toc_pre)
# the baseline is also the EESC in 1980
toc_pre$d_eesc <- toc_pre$EESC-1198.135909
toc_pre <- toc_pre %>%
  group_by(lon, lat) %>%
  mutate(new_toc = ifelse(year <= 1978, toc[year == 1980] + avg_TOC_EESC_Ratio * d_eesc, toc)) %>%
  ungroup() %>%
  select(-toc) %>%
  rename(toc = new_toc)
toc_pre
toc_pre <- data.frame(toc_pre)
quantile(toc_pre$toc)
toc_pre$toc[toc_pre$toc < 100] <- 100
toc_pre$toc[toc_pre$toc > 545] <- 545
quantile(toc_pre$toc)
percentiles <- seq(0.01, 1, by = 0.01)
quantile(toc_pre$toc,probs = percentiles,na.rm=TRUE)
toc_pre <- toc_pre[,c("year","month","lon","lat","toc")]
unique(toc_pre$year)
toc_pre <- toc_pre[toc_pre$year<=1978,]
write.csv(toc_pre,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data/toc_pre_Nocontrol_1958_1978.csv",row.names = FALSE)

# combime the input data of No Control scenario, include the toc data and the elevation data
toc_1958_1978 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data/toc_pre_Nocontrol_1958_1978.csv")
toc_1979_2100 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data/toc_pre_Nocontrol_1979_2100.csv")
total_toc <- rbind(toc_1958_1978,toc_1979_2100)
unique(total_toc$year)
ele <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data/resample_ele.csv")
ele$ele <- ele$ele/1000
total <- merge(total_toc,ele,by=c("lat","lon"))

grid <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data/SSP2_2020_grid.csv")
setDT(total)
setDT(grid)
# set the key of datatable
setkey(total, lon, lat)
setkey(grid, lon, lat)
# match
filtered_total <- grid[total, nomatch = 0]  # nomatch=0 Indicates that rows that do not match are not returned
head(filtered_total)
# zstart is Surface elevation, km, above sea level. Must be a positive value.
filtered_total$ele[filtered_total$ele < 0] <- 0
write.csv(filtered_total,file = "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/tuv input data/Nocontrol_tuv_inp.csv",row.names = FALSE)
