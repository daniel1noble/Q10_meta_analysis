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
geo_data$ERA5col                    = geo_data$long
geo_data$ERA5col[geo_data$ERA5col<0]= (180-abs(geo_data$ERA5col[geo_data$ERA5col<0]))+180   # Longitude is stored only as positive degrees east, no negatives for west.
geo_data$ERA5col                    = as.numeric(cut(geo_data$long, seq(0,360,0.25), labels=c(1:1440)))

geo_data$ERA5row                    = geo_data$lat                                          # Curiously latitude does have negatives
geo_data$ERA5row                    = as.numeric(cut(geo_data$lat, seq(-90,90,0.25), labels=c(1:720)))

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

# Note in the SST version the land is masked out, but air temperature is global.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

spp  = unique(geo_data$species_full)

for(i in which(complete.cases(geo_data[,c("lat","long")])) ){
  
  # Annual time series in one cell:
  point.timeseries = ncvar_get(ERA5, dname, 
                               start= c(geo_data$ERA5col[i],geo_data$ERA5row[i],1,1),
                               count= c(1,1,1,nt) )
  plot(point.timeseries ~ t.Date, type="l")   # point.timeseries ~ c(1:nt)
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
  
  ## Create a copy of the data as an array in R (which unlike the netcdf, makes
  ## it possible to index multiple locations at once)
  #X         = ncvar_get(ERA5, dname, start= c(1,1,1,1), count= c(nlon,nlat,1,nt) )
  #nc_close(ERA5)
  #
  #monthly.temp.data = matrix(NA, nrow=nt, ncol=nrow(geo_data),
  #                           dimnames=list(c(1:nt)))
  #for(j in 1:nt){ monthly.temp.data[j,] = X[,,j][spXY] }
  #
  ## Annual mean and SD
  #annual.temp.means = aggregate(monthly.temp.data, by=list(format(t.Date,"%Y")),mean,na.rm=T)
  #annual.temp.sd    = aggregate(monthly.temp.data, by=list(format(t.Date,"%Y")),sd,na.rm=T)
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
  
  # AT THIS POINT YOU CAN SUMMARISE HOWEVER YOU NEED
  # The 't.Date' object will classify by different units of time if needed.
  
  
  
  ## Save file for each species
  # write.csv(Tdat, paste0("Output/ERA5/",sp,"_temperature_history.csv"))
  
  
} #  End of species loop





  monthly.temp.data = matrix(NA, nrow=nt, ncol=nrow(geo_data),
                             dimnames=list(c(1:nt)))
  # This is now an array that is formatted as: X[longitude, latitude, temp in Kelvin]
  t <- with(na.omit(geo_data), X[long, lat, ])

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#         Microclimate
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Microclimate paper:
# Kearney et al. 2019 "A method for computing hourly, historical, terrain-corrected microclimate anywhere on earth"
# https://doi.org/10.1111/2041-210X.13330

  devtools::install_github("ilyamaclean/microclima")  
  
# N.B. The function automatically assumes the raster should be 200 cells by 200 cells,
#      so for a 30m resolution grid this is 6km across.
#      I've reduced this to 50 cells across (1.5km x 1.5km) to make it faster


  
# Now we need the elevation at the point. Which we get using a loop because it can only take a single lat long coord at a time.
run = FALSE
if(run){
  lat <- c()
  for(i in 1:dim(geo_data)[1]){
    elevation = microclima::get_dem(lat  = geo_data$lat[i], long = geo_data$long[i], resolution = 30, xdims = 50, ydims = 50)
    lat <- c(elevation, lat)
  }
  names(lat) <- paste0(1:nrow(geo_data), "_", geo_data$species_full)
saveRDS(lat, "./output/climate_data/elevation")
} else{
  elev <- readRDS("./output/climate_data/elevation")
}
# plot(r)

s1 = Sys.time()

# And... we'll cycle through microclimate generation one year at a time between 1981 and 2020
for(Y in c(1981:2022)){
  
  temps = microclima::runauto(elev, dstart = paste0("01/01/",Y,")"),  # start date
                              dfinish   = paste0("31/12/",Y,")"),  # end date
                              hgt = 0.1, l = NA, x = NA,
                              habitat = as.character(habitats$descriptor[gbif$Habitat[S]]),
                              plot.progress = FALSE)
  
  # Annual and monthly means of DEM 'r'
  r.means = mean(apply(temps$temps[,,], 3, mean,na.rm=T))  # Start with annual
  for(j in unique(months(temps$tme)) ){ r.means = c(r.means, mean(apply(temps$temps[,,months(temps$tme)==j],3,mean,na.rm=T)) )  }
  
  # Annual and monthly SDs of mean temp of DEM 'r'
  r.sd = sd(apply(temps$temps[,,], 3, mean,na.rm=T))       # Note the SD is after we have averaged across the 5x5 cells.
  for(j in unique(months(temps$tme)) ){ r.sd = c(r.sd, sd(apply(temps$temps[,,months(temps$tme)==j],3,mean,na.rm=T)) )  }
  
  # Add to matrix
  annual.thermal.data = rbind(annual.thermal.data, 
                              c(gbif$Fid[S],gbif$latitude[S], gbif$longitude[S], 
                                Y, r.means, r.sd) )
  
  rm(temps, r.means, r.sd)
  dev.off()
  print(Y)
  
}
