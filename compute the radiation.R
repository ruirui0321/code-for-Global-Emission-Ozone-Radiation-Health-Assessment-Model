library(data.table)
library(lubridate)
library(foreach)
library(doParallel)
no_cores <- detectCores() - 1
print(no_cores)
registerDoParallel(cores=no_cores)
#Set up input files for different scenarios
df_all <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/TUV/WMO_tuv_inp.csv")
# Calculate the time zone difference tmzone based on longitude
df_all$tmzone <- round(df_all$lon / 15)  # 15 degrees per longitude varies by one time zone
#Set the computing time to 4:00 to 20:00
df_all$tstart <- 4
df_all$tstop <- 20
lons <- unique(df_all$lon)
lats <- unique(df_all$lat)
df_all <- df_all[df_all$year >= 2091 & df_all$year <=2100,]
for (j in 1:length(lons)){
  for (k in 1:length(lats)){
    df_all_sp <- df_all[df_all$lon == lons[j] & df_all$lat == lats[k],]
    foreach (i = 1:nrow(df_all_sp))%dopar%{
        if (i == 0) {
        NULL
      } else {
      #i <- 1
      print(i)
      df <- df_all_sp[i,]
      lon <- df$lon
      lat <- df$lat
      year <- df$year
      month <- df$month
      tmzone <- df$tmzone
      tstart <- df$tstart
      tstop <- df$tstop
      o3col <- df$toc
      zstart <- df$ele
      # Use sprintf to format the month into two digits
      f_month <- sprintf("%02d", month)
      date_string <- sprintf("%s%s", year, f_month)
      # compute, when the time is greater than or equal to 2050, set the time to be 2050, calculate the value for the 15th day of each month
      command <- sprintf('echo -e "\\n1\\nlon\\n%s\\nlat\\n%s\\noutfil\\n%s\\ntmzone\\n%s\\ntstart\\n%s\\ntstop\\n%s\\niyear\\n2050\\nimonth\\n%s\\niday\\n15\\nnt\\n17\\no3col\\n%s\\nzstart\\n%s\\nzout\\n%s\\n\\n\\n" | C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/TUV/V5.3.2/tuv', lon, lat,date_string,tmzone,tstart,tstop,month,o3col,zstart,zstart)
#      command <- sprintf('echo -e "\\n1\\nlon\\n%s\\nlat\\n%s\\noutfil\\n%s\\ntmzone\\n%s\\ntstart\\n%s\\ntstop\\n%s\\niyear\\n%s\\nimonth\\n%s\\niday\\n15\\nnt\\n17\\no3col\\n%s\\nzstart\\n%s\\nzout\\n%s\\n\\n\\n" | C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/TUV/V5.3.2/tuv', lon, lat,date_string,tmzone,tstart,tstop,year,month,o3col,zstart,zstart)
      # executive command
      system(command, intern = TRUE)
      original_file_path <- sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/TUV/%s.txt", date_string)
      # The path to the new file, dynamically determining the folder based on the year
      new_file_dir <- sprintf("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/TUV/TUV_WMO/%s", year)
      new_file_path <- sprintf("%s/%s_%s_%s.txt", new_file_dir, date_string, lon, lat)
      if (!dir.exists(new_file_dir)) {
        dir.create(new_file_dir, recursive = TRUE)
      }
      # Rename the file (actually move it)
      file.rename(original_file_path, new_file_path)
      }
    }
  }
}
stopImplicitCluster()
