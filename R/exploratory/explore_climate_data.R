###################################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                       GLOBAL HISTORIC TEMPERATURE
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###################################################################################

rm(list=ls())
pardefault = par()

coords = read.csv("coord.csv")

plot(coords[,2:3], xlim=c(-180,180), ylim=c(-70,70))
abline(h=0, lty=2)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #


library(ncdf4)
# Open the NetCDF data set, and print some basic information.
SSTdir  = "gis_data/"
ncname  = "HadISST_sst"
ncfname = paste0(SSTdir,ncname, ".nc", sep = "")
dname   = "sst" 

# Open a NetCDF file
HAD.sst = nc_open(ncfname)
print(HAD.sst)

# Get the longtiudes and latitudes as before, using now the ncvar_get() function in ncdf4.
lon     = ncvar_get(HAD.sst, "longitude")
nlon    = dim(lon)
head(lon)

lat     = ncvar_get(HAD.sst, "latitude", verbose = F)
nlat    = dim(lat)
head(lat)

# Get the time variable and its attributes using the ncvar_get() and ncatt_get() functions, and also get the number of times using the dim() function.
t       = ncvar_get(HAD.sst, "time")
tunits  = ncatt_get(HAD.sst, "time", "units")
nt      = dim(t)

# Get the variable and its attributes, and verify the size of the array.
sst.array = ncvar_get(HAD.sst, dname)
dlname    = ncatt_get(HAD.sst, dname, "long_name")
dunits    = ncatt_get(HAD.sst, dname, "units")
fillvalue = ncatt_get(HAD.sst, dname, "_FillValue")
dim(sst.array)

# Get the global attributes.
title       = ncatt_get(HAD.sst, 0, "Title")
description = ncatt_get(HAD.sst, 0, "description")
institution = ncatt_get(HAD.sst, 0, "institution")
datasource  = ncatt_get(HAD.sst, 0, "source")
references  = ncatt_get(HAD.sst, 0, "reference")
history     = ncatt_get(HAD.sst, 0, "history")
Conventions = ncatt_get(HAD.sst, 0, "Conventions")

# Close the NetCDF file using the nc_close() function.

nc_close(HAD.sst)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# The time variable, in "time-since" units can be converted into "real" (or more easily readable) 
# time values by splitting the time tunits$value string into its component parts, and then using 
# the chron() function to determine the absolute value of each time value from the time origin.

# Split the time units string into fields
tustr     = strsplit(tunits$value, " ")
tdstr     = strsplit(unlist(tustr)[3], "-")
tmonth    = as.integer(unlist(tdstr)[2])
tday      = as.integer(unlist(tdstr)[3])
tyear     = as.integer(unlist(tdstr)[1])

library(chron)
times     = chron(t, origin = c(tmonth, tday, tyear))
times     = as.POSIXct(times, "%d/%m/%y %H:%M:%S")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# In NetCDF file, values of a variable that are either missing or simply not available (i.e. ocean grid points in 
# a terrestrial data set) are flagged using specific "fill values" (_FillValue) or missing values (missing_value), 
# the values of which are set as attributes of a variable. In R, such unavailable data are indicated using the "NA"
# value. The following code fragment illustrates how to replace the NetCDF variable's fill values with R NA's .

sst.array[sst.array == fillvalue$value] = NA

# Also values of SST need to be divided by 100
sst.array = sst.array / 100

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

library(RColorBrewer)
library(lattice)
m         = 1
sst.slice = sst.array[, , m]

grid   = expand.grid(lon = lon, lat = lat)
cutpts = c(-50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50)
levelplot(sst.slice ~ lon * lat, data = grid, at = cutpts, cuts = 11, pretty = T, 
          col.regions = (rev(brewer.pal(10, "RdBu"))))

cutpts = seq(-10, 35, l=100)
levelplot(sst.slice ~ lon * lat, data = grid, at = cutpts, cuts = 100, pretty = T, 
          col.regions = colorRampPalette(c("navyblue","yellow","darkred"))(length(cutpts)) )


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

coords$ncol = coords$nrow = NA
for(i in 1:nrow(coords)){ 
  coords$nrow[i] = which.min(abs(coords$long[i]-lon)) 
  coords$ncol[i] = which.min(abs(coords$lat[i]-lat))
}
head(coords)

# 150 years of SST at oordinate 'm'!
m = 1
y = sst.array[coords$nrow[m],coords$ncol[m],]
plot( y ~ times, type="l" )
# Select dates after 1980
ti = which(times > as.POSIXct("1980-01-01"))
y = sst.array[coords$nrow[m],coords$ncol[m],ti]
plot( y ~ times[ti], type="l", col="dodgerblue")
# Coefficient of variation
cv(y)



################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################

rm(list=ls())

library("devtools")
library(microclima) #devtools::install_github("ilyamaclean/microclima")
library(NicheMapR) #devtools::install_github("mrke/NicheMapR")

# EXAMPLE FROM MEE PAPER:
# get DEM for Pico, Azores
r = microclima::get_dem(lat = 38.467429, long = -28.398995, resolution = 30)

# Takes ~ c. 5 minutes to run
temps = microclima::runauto(r, "10/06/2010", "15/06/2010", hgt = 0.1,
                             l = NA, x = NA, habitat = "Barren or sparsely vegetated",
                             plot.progress = FALSE)

names(temps)
# A list object is returned with the following elements: 
# 1) an array of temperatures for each grid cell and hour, 
dim(temps$temps)
# 2) an extent object defining the spatial extent over which the model was run, 
temps$e
# 3) a POSIXlt object of hours over which the model is run, 
summary(temps$tme)
# 4) a character vector describing the units of temperature (usually ?C), 
# 5) a raster object of maximum, mean and minimum temperatures and frost hours
par(mfrow=c(2,2))
plot(temps$tmax, main="max")
plot(temps$tmin, main="min")
plot(temps$tmean, main="mean")
#plot(temps$frosthours)  All NA's

# Visualize spatial variation in temperature CV
temp.cv = temps$tmean
temp.cv[1:ncell(temp.cv)] = apply(temps$temps, c(2,1), cv, na.rm=T)
plot(temp.cv, main="CV")

par(pardefault)

# Plot timeseries 
tmean = apply(temps$temps, 3, mean, na.rm=T)
tmax  = apply(temps$temps, 3, max, na.rm=T)
tmin  = apply(temps$temps, 3, min, na.rm=T)

plot( tmean ~ as.POSIXct(temps$tme), type="l",
      las=1,xlab="Time",ylab=temps$units,
      ylim=c(range(c(tmin,tmax),na.rm=T)+c(-1,+1)))
lines( tmax  ~ as.POSIXct(temps$tme), lty=3 )
lines( tmin  ~ as.POSIXct(temps$tme), lty=3 )

# Although spatial auto-correlation means its not strictly appropriate, could
# even plot the spatial-CV over time 
tcv  = apply(temps$temps, 3, cv, na.rm=T)
plot( tcv ~ as.POSIXct(temps$tme), type="l",
      las=1,xlab="Time",ylab="CV")



# So the equivalent for each location in the acclimation dataset is 

coords = read.csv("C:/Users/pickl/OneDrive/Documents/Nobel_climate/ecto_coords.csv")
i = 2
coords[i,]
# Aaah a spot on the beach in Finland... lets call it forest, although it could be
# a fish in the lake 

# N.B. The function automatically assumes the raster should be 200 cells by 200 cells,
#      so for a 30m resolution grid this is 6km across.
#   >> To save myself some time I have made this 33 cells, or 1km across.

local.dem = microclima::get_dem(lat = coords$lat[i], long = coords$long[i], 
                                resolution = 30, xdims = 33, ydims = 33)


# The earliest you can go with the latest reanalysis datset is 1979. But it 
# does seem like you could go further.
# Anyway for now I select 1990 - 2020

# Most weather stations are 2m of the ground, but given your variety of ectotherms, maybe the
# setting of 10cm above the ground is a reasonable compromise?

# A bigger issue is the choice of habitat. 
# 'l' and 'x' manually control the variation in Leaf Area Index (LAI) and their orientation 
# over space and time. Globally you will have evergreen and deciduous systems etc.
# Instead you can select form 17 habitat types:
habitats
# Unless you have a very good understanding of the habitat's a species uses, you might need to 
# run this process more than once to get upper and lower bounds...


# SAMPLE POINT #1
coords[1,]
# This point is just off the southern coast of Montpellier in France. 
# Given this first point was marine, I thought I'd try the category for 'Open water'.
# However the function throws an error when it internally tries to identify what the LAI for
# this location would have been for that habitat type. Specifying 0 values for 'l' and 'x' 
# didn't help either.

# This is why I switched to SAMPLE POINT 2!
# Now the habitat type is 'Evergreen needleleaf forest'

# AND, iyt turns out the function only works for one year at a time!

s1 = Sys.time()
temps = microclima::runauto(local.dem, "01/01/2010", "31/12/2010", hgt = 0.1,
                             l = 0, x = 0, 
                            habitat = "Evergreen needleleaf forest",
                             plot.progress = FALSE)
s2 = s1 - Sys.time()

# 1 year takes
print(s2)



par(mfrow=c(2,2))
plot(temps$tmax, main="max")
plot(temps$tmin, main="min")
plot(temps$tmean, main="mean")
# Spatial variation in temperature CV
temp.cv = temps$tmean
temp.cv[1:ncell(temp.cv)] = apply(temps$temps, c(2,1), cv, na.rm=T)
plot(temp.cv, main="CV")





































# SST
https://www.metoffice.gov.uk/hadobs/hadisst/index.html
https://psl.noaa.gov/data/gridded/data.cobe2.html
http://apdrc.soest.hawaii.edu/las/v6/dataset?catitem=1


# ATMOSPHERIC PARAMETERS 
# ERA5 Archive
https://www.ecmwf.int/en/forecasts/datasets/reanalysis-datasets/era5
# Click on parameter listings:
https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Parameterlistings
# No. 48 - 2 metre temperature is standard weather station temp.

# KRIGR FOR improving resolution of ERA-5
https://erikkusch.com/projects/krigr-downloading-and-downscaling-of-era5-data-using-r/
# Reminder that the guy said you need to train it differently for different hemispheres because relationship of aspect in the DEM changes.

# Kearney microclim is at 15km resolution
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4387738/#b7
https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.02360

  



