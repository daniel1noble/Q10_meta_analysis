###################################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                       ERA5 Reanalysis 1 CLIMATE DATA
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###################################################################################

# This version doesn't actively download point estimates of temperature. 
# Instead a NetCDF for ERA5 was downloaded for 1979-present.

# See Word doc: Data/Downloading ERA5 historic climate data.docx

# Note that the data is stored in Kelvin and transformed.
# 2m surface temperature
#     scale_factor: 0.00176319797961702
#     add_offset: 257.703670097788
# Sea Surface Temperature
#     scale_factor: 0.00061697729459776
#     add_offset: 289.46490147229

# Time is recorded in hours since 1900-01-01 00:00:00.0

rm(list=ls())
library(ncdf4)

geo_data = read.csv("./data/climate_data/Species_LatLong_plusHabitat.csv")

# First remove missing lat and long
geo_data <- geo_data %>% dplyr::filter(!is.na(lat) & !is.na(long))

# Identify which coarse ERA5 cell these lat/longs fall within:
geo_data$ERA5col     = geo_data$long
xid                  = which((!is.na(geo_data$ERA5col)) & (geo_data$ERA5col<0) )
geo_data$ERA5col[xid]= (180-abs(geo_data$ERA5col[xid]))+180   # Longitude is stored only as positive degrees east, no negatives for west.
geo_data$ERA5col     = cut(geo_data$ERA5col, seq(0,360,0.25), labels=c(1:1440))

geo_data$ERA5row     = geo_data$lat                                          # Curiously latitude does have negatives
geo_data$ERA5row     = as.numeric(cut(geo_data$ERA5row, seq(-90,90,0.25), labels=c(1:720)))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Open NETCDF file
ncfname   = paste0("./data/climate_data/ERA5/ERA5_LST_and_SST_1979-present.nc")
ERA5      = nc_open(ncfname)
dname     = "t2m"
lon       = ncvar_get(ERA5, "longitude") # ncvar_get(ERA5.air, "lon")
nlon      = length(lon) # number of columns
lat       = ncvar_get(ERA5, "latitude") # ncvar_get(ERA5.air, "lat", verbose = F)
nlat      = length(lat) # number of rows
t         = ncvar_get(ERA5, "time")                 # units: hours since 1900-01-01 00:00:00.0
nt        = dim(t)                                  # monthly averages

# Multiply 't' by 3600 so it becomes seconds since 1900 instead of hours.
t.Date    = as.POSIXct(as.numeric(t)*3600, origin="1900-01-01 00:00:00") 

# Full layer, at first time step
map = ncvar_get(ERA5, dname, 
                start= c(1,1,1,1),       # 4 dimensions: longitude, latitude, 'expver' (variable), time
                count= c(nlon,nlat,1,1) )
image(map)                               # Its both, upside down, back to front, and centered on 180' west...

# Convert to raster for somewhat easier plotting
library(raster)
map2 = t(map)
# Suffle the "WEST" to the left
map2 = cbind(map2[,721:1440],map2[,1:720])
map.raster = raster(map2, xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326)
plot(map.raster)

# Note in the SST version the land is masked out, but air temperature is global.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

geo_data <- na.omit(geo_data)

# Create unique col that will take rowname because of repeat species
geo_data$row <- paste0(geo_data$species_full, "_",geo_data$lat, ",", geo_data$long)
clim_data_matrix <- data.frame(matrix(NA, nrow = dim(geo_data)[1], ncol = 522))
names <- gsub("* [10][10].+", "", t.Date)
colnames(clim_data_matrix) <- c("species", "lat", "long", names)

for(i in 1:nrow(geo_data)){
  
  # Annual time series in one cell:
  point.timeseries = ncvar_get(ERA5, dname, 
                               start= c(geo_data$ERA5col[i],geo_data$ERA5row[i],1,1),
                               count= c(1,1,1,nt) )
  clim_data_matrix[i,1] <- geo_data$species_full[i]
  clim_data_matrix[i,2] <- geo_data$lat[i]
  clim_data_matrix[i,3] <- geo_data$long[i]
  clim_data_matrix[i,c(4:522)] <- point.timeseries
  plot(point.timeseries ~ t.Date, type="l")   # point.timeseries ~ c(1:nt)
}

# Now we can do some simple summaries, like CV
cv <- function(x){
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}
clim_data_matrix$cv <- apply(clim_data_matrix[,4:522], 1, function(x) cv(x)) # Coefficient of variation
clim_data_matrix$sd <- apply(clim_data_matrix[,4:522], 1, function(x) sd(x, na.rm = TRUE)) # sd
clim_data_matrix$mean <- apply(clim_data_matrix[,4:522], 1, function(x) mean(x, na.rm = TRUE)) # mean
clim_data_matrix$acf_lag1 <- apply(clim_data_matrix[,4:522], 1, function(x) acf(as.numeric(x), lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2,1,1]) # autocorrelation for lag w
clim_data_matrix$acf_all <- apply(clim_data_matrix[,4:522], 1, function(x) sd(acf(as.numeric(x), plot = FALSE, na.action = na.pass)$acf)) # deviation in autocorrelation values...this is probably not that useful

write.csv(clim_data_matrix, "./output/climate_data/temp_timeseries.csv")

r1 <- acf(as.numeric(clim_data_matrix[1,4:522]), lag.max = 522 - 4, plot = FALSE, na.action = na.pass)$acf[-1]
lag <- c(1:518)
plot(r1 ~ lag)
