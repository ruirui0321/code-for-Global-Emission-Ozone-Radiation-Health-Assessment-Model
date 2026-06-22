library(foreach)
library(doParallel)
# Define year list
#years <- 1958:2050
# Define the year of the two intervals
years1 <- 1979:2020
years2 <- 2051:2100
years <- c(years1, years2)
no_cores <- detectCores() - 1
registerDoParallel(cores = no_cores)
results <- foreach(year = years) %dopar% {
# set the work dir
  work_dir <- sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/TUV/TUV_WMO/%d", year)
  setwd(work_dir)
  months <- sprintf("%02d", 1:12)
  lons <- seq(-179.375, 179.375, by = 1.25)  #Adjust according to the actual step size
  lats <- seq(-89.5, 89.5, by = 1)  #Adjust according to the actual step size
  combined_df <- data.frame(
    '1' = numeric(),
    '2' = numeric(),
    '3' = numeric(),
    '4' = numeric(),
    '5' = numeric(),
    '6' = numeric(),
    '7' = numeric(),
    '8' = numeric(),
    '9' = numeric(),
    '10' = numeric(),
    'year' = numeric(),
    'month' = character(),
    'lon' = numeric(),
    'lat' = numeric(),
    stringsAsFactors = FALSE  # Prevents string conversion to factors
  )
  for (month in months) {
    for (lon in lons) {
      for (lat in lats) {
        file_name <- sprintf("%4d%s_%.3f_%.1f.txt", year, month, lon, lat)
        if (file.exists(file_name)) {
          # Read lines 160 to 177 of the file
          lines <- readLines(file_name)[160:177]
          # Use the textConnection function to convert these lines into a connection that can be read by read.table
          data_conn <- textConnection(lines)
          data_frame <- read.table(data_conn, header = TRUE, fill = TRUE)
          close(data_conn)
          # Calculate the sums for columns X1 through X8 using the colSums function
          sums <- colSums(data_frame[, c("sza.","deg.","X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")], na.rm = TRUE)
          summed_df <- as.data.frame(t(sums))
          colnames(summed_df) <- 1:10
          summed_df$year <- year
          summed_df$month <- month
          summed_df$lon <- lon
          summed_df$lat <- lat
          combined_df <- rbind(combined_df, summed_df)
        }
      }
    }
  }
  file_path <- sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/TUV/TUV_WMO/%d_15th.csv", year)
  write.csv(combined_df, file = file_path, row.names = FALSE)
}
stopImplicitCluster()



# Do not add up the daily dose, output the original file (if required),not stored
setwd("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/TUV/TUV_store/1980")
# Defines ranges of year, month, longitude, and latitude
year <- 1980
months <- sprintf("%02d", 1:12)
lons <- seq(-179.375, 179.375, by = 1.25)
lats <- seq(-89.5, 89.5, by = 1)
combined_df <- data.frame(
  'hour' = numeric(),
  'sza' = numeric(),
  '1' = numeric(),
  '2' = numeric(),
  '3' = numeric(),
  '4' = numeric(),
  '5' = numeric(),
  '6' = numeric(),
  '7' = numeric(),
  '8' = numeric(),
  '9' = numeric(),
  '10' = numeric(),
  'year' = numeric(),
  'month' = character(),
  'lon' = numeric(),
  'lat' = numeric(),
  stringsAsFactors = FALSE
)
for (month in months) {
  for (lon in lons) {
    for (lat in lats) {
      print(month)
      print(lon)
      print(lat)
      file_name <- sprintf("%4d%s_%.3f_%.1f.txt", year, month, lon, lat)
      if (file.exists(file_name)) {
        lines <- readLines(file_name)[160:177]
        data_conn <- textConnection(lines)
        data_frame <- read.table(data_conn, header = TRUE, fill = TRUE)
        close(data_conn)
        df <- data_frame[, c("time.","hrs.","sza.","deg.","X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")]
        colnames(df) <- c("hour", "sza", paste0("X", 1:10))
        df$year <- year
        df$month <- month
        df$lon <- lon
        df$lat <- lat
        combined_df <- rbind(combined_df, df)
      }
    }
  }
}
write.csv(combined_df,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/TUV/TUV_store/1980_15th_original.csv",row.names = FALSE)

