# ! TODO metti a posto sta roba


getwd()
################################################################################
################################################################################
#Step 1: DATA DOWNLOAD
## Download data from copernicus UrbPop (temperature and specific humidity)

################################################################################
################################################################################
#Step 2: OPENING AND EXTRACTION OF THE MAXIMUM AND MINIMUM VALUES

#Load packages
library(ncdf4) # package for netcdf manipulation
library(RNetCDF)
library(raster)
library(rgdal)
library(lubridate)
library(dismo)

# the part of the name of the directory where to pick the files
temp_directory <- '../temperature'
temp_name_start <- '/tas_Trieste_UrbClim_' 
hum_spec_directory <- '../hum_spec'
hum_spec_name_start <- '/huss_Trieste_UrbClim_' 
middle <- '2008_01'
name_end <- '_v1.0.nc'
mycrs <- CRS("+init=epsg:3035")
#mycrs <- CRS('+init=epsg:3035 +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')

years <- c("2008", "2009", "2010", "2011","2012","2013","2014","2015","2016","2017")
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
hours <- seq(1:24)

base_time = as.POSIXct("2008-01-01 00:00:00",
                       format = "%Y-%m-%d %H:%M:%S")

# nc <- stack("../temperature/tas_Trieste_UrbClim_2008_01_v1.0.nc")
#years <- c("2008")
#months <- c("01")





#one year
for (y in 1:length(years))
{
  for (m in 1:length(months))
  {
    s <- stack()
    file_name <- paste(temp_directory,temp_name_start,years[y],'_',months[m],name_end, sep="")
    
    nc_data <- nc_open(file_name)

    # base_time = as.POSIXct("2008-01-01 00:00:00",
    #                        format = "%Y-%m-%d %H:%M:%S")
    # 
    # 
    # time_now = base_time + lubridate::hours(743)
    lon <- ncvar_get(nc_data, "x")
    lat <- ncvar_get(nc_data, "y", verbose = F)
    t <- ncvar_get(nc_data, "time")
    print(length(t))

    
    var.array <- ncvar_get(nc_data, "tas") # tas è temperatura
   
    for (times in 1: length(t)){
      #print(times);
      var.slice <- var.array[, , times]
      
      r <- raster(t(var.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=mycrs)
      r = flip(r, direction='y')
      # writeRaster(r, "test.tif", "GTiff", overwrite=TRUE)
      #plot(r)
      s <- addLayer(s, r)
      # writeRaster(r2, "test.tif", "GTiff", overwrite=TRUE)
    }
    


   
    
    
    
    
    
    
    #at the end of this loop s is a raster stack of the 24 hours of all the days of month m
    #the min and max can be calculated.
    minM <-min(s)
    maxM <-max(s)
    
    
    minfname<-paste("../temperature_mod/min/min",years[y],months[m], ".tif", sep="")
    maxfname<-paste("../temperature_mod/max/max",years[y],months[m], ".tif", sep="")
    
    
    writeRaster(minM, minfname,format="GTiff")
    writeRaster(maxM, maxfname,format="GTiff")
    
    # For each month of each year two rasters are written one with the minimum and another one with the maximum
    
    removeTmpFiles(h=0)
    
  }
}
print (paste(Sys.time(),"Step 2 done"))


################################################################################
# HUMIDITY
################################################################################
#one year
for (y in 1:length(years))
{
  for (m in 1:length(months))
  {
    s <- stack()
    file_name <- paste(hum_spec_directory,hum_spec_name_start,years[y],'_',months[m],name_end, sep="")
    
    nc_data <- nc_open(file_name)
    
    # base_time = as.POSIXct("2008-01-01 00:00:00",
    #                        format = "%Y-%m-%d %H:%M:%S")
    # 
    # 
    # time_now = base_time + lubridate::hours(743)
    lon <- ncvar_get(nc_data, "x")
    lat <- ncvar_get(nc_data, "y", verbose = F)
    t <- ncvar_get(nc_data, "time")
    print(length(t))
    
    
    var.array <- ncvar_get(nc_data, "huss") # tas è temperatura
    
    for (times in 1: length(t)){
      #print(times);
      var.slice <- var.array[, , times]
      
      r <- raster(t(var.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=mycrs)
      r = flip(r, direction='y')
      # writeRaster(r, "test.tif", "GTiff", overwrite=TRUE)
      #plot(r)
      s <- addLayer(s, r)
      # writeRaster(r2, "test.tif", "GTiff", overwrite=TRUE)
    }
    
    
    
    
    
    
    
    
    
    
    #at the end of this loop s is a raster stack of the 24 hours of all the days of month m
    #the min and max can be calculated.
    minM <-min(s)
    maxM <-max(s)
    
    # add mean for humidity
    j<-stack()
    
    j<-addLayer(j, minM, maxM)
    
    res<-mean(j)
    QV2M_mean<-paste("../hum_spec_mod/mean/QV2M_mean",years[y],months[m], ".tif", sep="")
    minfname<-paste("../hum_spec_mod/min/min",years[y],months[m], ".tif", sep="")
    maxfname<-paste("../hum_spec_mod/max/max",years[y],months[m], ".tif", sep="")
    
    
    writeRaster(minM, minfname,format="GTiff")
    writeRaster(maxM, maxfname,format="GTiff")
    writeRaster(res, QV2M_mean,format="GTiff")
    
    # For each month of each year two rasters are written one with the minimum and another one with the maximum
    
    removeTmpFiles(h=0)
    
  }
}

print (paste(Sys.time(),"Step 2 done"))















# for the moment I'll use only Vmin

# temp_directory <- '../temperature'
# temp_name_start <- '/tas_Trieste_UrbClim_' 
# hum_spec_directory <- '../hum_spec'
# hum_spec_name_start <- '/huss_Trieste_UrbClim_' 
# middle <- '2008_01'
# name_end <- '_v1.0.nc'

temp_max_pattern <- '../temperature_mod/max'
temp_min_pattern <- '../temperature_mod/min'
hum_max_pattern <- '../hum_spec_mod/max'
hum_min_pattern <- '../temperature_mod/min'
hum_mean_pattern <- '../temperature_mod/mean'


getwd()
for (y in 1:length(years))
{

  temp_max<-list.files(path=temp_max_pattern, pattern = years[y])

  
  
  #max Temperature
  janmax<-raster(paste(temp_max_pattern, '/',temp_max[1], sep = ''))
  febmax<-raster(paste(temp_max_pattern, '/',temp_max[2], sep = ''))
  marmax<-raster(paste(temp_max_pattern, '/',temp_max[3], sep = ''))
  aprmax<-raster(paste(temp_max_pattern, '/',temp_max[4], sep = ''))
  maymax<-raster(paste(temp_max_pattern, '/',temp_max[5], sep = ''))
  junmax<-raster(paste(temp_max_pattern, '/',temp_max[6], sep = ''))
  julmax<-raster(paste(temp_max_pattern, '/',temp_max[7], sep = ''))
  augmax<-raster(paste(temp_max_pattern, '/',temp_max[8], sep = ''))
  sepmax<-raster(paste(temp_max_pattern, '/',temp_max[9], sep = ''))
  octmax<-raster(paste(temp_max_pattern, '/',temp_max[10], sep = ''))
  novmax<-raster(paste(temp_max_pattern, '/',temp_max[11], sep = ''))
  decmax<-raster(paste(temp_max_pattern, '/',temp_max[12], sep = ''))
  
  
  maxTemp<-stack(janmax,febmax,marmax,aprmax,maymax,junmax,julmax,augmax,sepmax,octmax,novmax,decmax)
  
  
  temp_min<-list.files(path=temp_min_pattern, pattern = years[y])
  
  
  
  #min Temperature
  janmin<-raster(paste(temp_min_pattern, '/',temp_min[1], sep = ''))
  febmin<-raster(paste(temp_min_pattern, '/',temp_min[2], sep = ''))
  marmin<-raster(paste(temp_min_pattern, '/',temp_min[3], sep = ''))
  aprmin<-raster(paste(temp_min_pattern, '/',temp_min[4], sep = ''))
  maymin<-raster(paste(temp_min_pattern, '/',temp_min[5], sep = ''))
  junmin<-raster(paste(temp_min_pattern, '/',temp_min[6], sep = ''))
  julmin<-raster(paste(temp_min_pattern, '/',temp_min[7], sep = ''))
  augmin<-raster(paste(temp_min_pattern, '/',temp_min[8], sep = ''))
  sepmin<-raster(paste(temp_min_pattern, '/',temp_min[9], sep = ''))
  octmin<-raster(paste(temp_min_pattern, '/',temp_min[10], sep = ''))
  novmin<-raster(paste(temp_min_pattern, '/',temp_min[11], sep = ''))
  decmin<-raster(paste(temp_min_pattern, '/',temp_min[12], sep = ''))
  

  minTemp<-stack(janmin,febmin,marmin,aprmin,maymin,junmin,julmin,augmin,sepmin,octmin,novmin,decmin)
  
  
  hum_min<-list.files(path=hum_min_pattern, pattern = years[y])
  
  

  #Humidity
  janprec<-raster(paste(hum_min_pattern, '/',hum_min[1], sep = ''))
  febprec<-raster(paste(hum_min_pattern, '/',hum_min[2], sep = ''))
  marprec<-raster(paste(hum_min_pattern, '/',hum_min[3], sep = ''))
  aprprec<-raster(paste(hum_min_pattern, '/',hum_min[4], sep = ''))
  mayprec<-raster(paste(hum_min_pattern, '/',hum_min[5], sep = ''))
  junprec<-raster(paste(hum_min_pattern, '/',hum_min[6], sep = ''))
  julprec<-raster(paste(hum_min_pattern, '/',hum_min[7], sep = ''))
  augprec<-raster(paste(hum_min_pattern, '/',hum_min[8], sep = ''))
  sepprec<-raster(paste(hum_min_pattern, '/',hum_min[9], sep = ''))
  octprec<-raster(paste(hum_min_pattern, '/',hum_min[10], sep = ''))
  novprec<-raster(paste(hum_min_pattern, '/',hum_min[11], sep = ''))
  decprec<-raster(paste(hum_min_pattern, '/',hum_min[12], sep = ''))
  
  prec<-stack(janprec,febprec,marprec,aprprec,mayprec,junprec,julprec,augprec,sepprec,octprec,novprec,decprec)
  
  ######################################
  # NOTE: temperature is converted to Celsius
  ######################################
  minTemp <- minTemp - 273.15
  maxTemp <- maxTemp - 273.15
  bios<-biovars(prec,minTemp,maxTemp)
  outname<-paste("../bio_adj/",years[y],"min_hum_bios.tif",sep="")
  writeRaster(bios,filename=outname,bylayer=TRUE,suffix="names", format="GTiff")
}


################################################################################
################################################################################
#STEP 4: MERGING BIOS BY DECADES AND VERSION 
##For each version (each in one folder) the mean of the bios is calculated by decade

#Load packages
library(rgdal)
library(raster)

#set the working directory in the max folder


bioclim<-c("bio1.tif","bio2.tif","bio3.tif","bio4.tif","bio5.tif","bio6.tif","bio7.tif","bio8.tif","bio9.tif","bio10.tif","bio11.tif","bio12.tif","bio13.tif","bio14.tif","bio15.tif","bio16.tif","bio17.tif","bio18.tif","bio19.tif")

for (bio in 1:length(bioclim)) #loop is run per bioclimatic variable
{
  
  
  listfiles <- list.files(path='../bio_adj', pattern = bioclim[bio])
  listfiles <- paste('../bio_adj/', listfiles, sep = '')
  
  
  s<-stack(listfiles) 
  res<-mean(s)
  outname<-paste("../trieste_clim_adj/bio",bio, ".tif", sep="")
  writeRaster(res, outname,format="GTiff")
 
}

bioclim<-c("bio1.tif","bio2.tif","bio3.tif","bio4.tif","bio5.tif","bio6.tif","bio7.tif","bio8.tif","bio9.tif","bio10.tif","bio11.tif","bio12.tif","bio13.tif","bio14.tif","bio15.tif","bio16.tif","bio17.tif","bio18.tif","bio19.tif")



plot(resultRaster[[12]])

################################################################################
# END
################################################################################

################################################################################
# NOW I HAVE TO CONVERT THE VARIABLES FOLLOWING MERRACLIM
################################################################################
# Step 6: The final values have been multiplied by 10 
# for the temperature related variables (BIO1-BIO11) 
# and by 100,000 the humidity related variables (BIO12-BIO19)
# to store the information as integers and therefore using rasters with a smaller 
# depth of pixel allowing a faster download and easier manipulation in GIS software.
# 
# Step 7: As the biovars function was designed to be used with precipitation,
# not specific humidity, some of the resulting bioclimatic variables needed to
# be divided to have ecological meaning. Accordingly,
# the resulting BIO12 has been divided by 12 to obtain the final MERRAclim BIO12,
# which describes the annual mean of specific humidity instead of cumulative annual rainfall. 
# The resulting BIO16, BIO17, BIO18 and BIO19 have all been divided by 3 
# so that the corresponding final MERRAclim variables inform on quarterly means 
# instead of cumulative quarterly precipitation.



bio1 <- raster("../trieste_clim_adj/bio1.tif")
bio2 <- raster("../trieste_clim_adj/bio2.tif")
bio3 <- raster("../trieste_clim_adj/bio3.tif")
bio4 <- raster("../trieste_clim_adj/bio4.tif")
bio5 <- raster("../trieste_clim_adj/bio5.tif")
bio6 <- raster("../trieste_clim_adj/bio6.tif")
bio7 <- raster("../trieste_clim_adj/bio7.tif")
bio8 <- raster("../trieste_clim_adj/bio8.tif")
bio9 <- raster("../trieste_clim_adj/bio9.tif")
bio10 <- raster("../trieste_clim_adj/bio10.tif")
bio11 <- raster("../trieste_clim_adj/bio11.tif")
bio12 <- raster("../trieste_clim_adj/bio12.tif")
bio13 <- raster("../trieste_clim_adj/bio13.tif")
bio14 <- raster("../trieste_clim_adj/bio14.tif")
bio15 <- raster("../trieste_clim_adj/bio15.tif")
bio16 <- raster("../trieste_clim_adj/bio16.tif")
bio17 <- raster("../trieste_clim_adj/bio17.tif")
bio18 <- raster("../trieste_clim_adj/bio18.tif")
bio19 <- raster("../trieste_clim_adj/bio19.tif")


# MULTIPLY TEMPERATURE BY 10

bio1 <- bio1 * 10
bio2 <- bio2 * 10
bio3 <- bio3 * 10
bio4 <- bio4 * 10
bio5 <- bio5 * 10
bio6 <- bio6 * 10
bio7 <- bio7 * 10
bio8 <- bio8 * 10
bio9 <- bio9 * 10
bio10 <- bio10 * 10
bio11 <- bio11 * 10

plot(bio12)
bio12 <- bio12 / 12
plot(bio12)

plot(bio13)
plot(bio14)
plot(bio15)

plot(bio16)
bio16 <- bio16 / 3
plot(bio16)

plot(bio17)
bio17 <- bio17 / 3
plot(bio17)

plot(bio18)
bio18 <- bio18 / 3
plot(bio18)

plot(bio19)
bio19 <- bio19 / 3
plot(bio19)




writeRaster(bio1, "../trieste_final/bio1.tif",format="GTiff")
writeRaster(bio2, "../trieste_final/bio2.tif",format="GTiff")
writeRaster(bio3, "../trieste_final/bio3.tif",format="GTiff")
writeRaster(bio4, "../trieste_final/bio4.tif",format="GTiff")
writeRaster(bio5, "../trieste_final/bio5.tif",format="GTiff")
writeRaster(bio6, "../trieste_final/bio6.tif",format="GTiff")
writeRaster(bio7, "../trieste_final/bio7.tif",format="GTiff")
writeRaster(bio8, "../trieste_final/bio8.tif",format="GTiff")
writeRaster(bio9, "../trieste_final/bio9.tif",format="GTiff")
writeRaster(bio10, "../trieste_final/bio10.tif",format="GTiff")
writeRaster(bio11, "../trieste_final/bio11.tif",format="GTiff")
writeRaster(bio12, "../trieste_final/bio12.tif",format="GTiff")
writeRaster(bio13, "../trieste_final/bio13.tif",format="GTiff")
writeRaster(bio14, "../trieste_final/bio14.tif",format="GTiff")
writeRaster(bio15, "../trieste_final/bio15.tif",format="GTiff")
writeRaster(bio16, "../trieste_final/bio16.tif",format="GTiff")
writeRaster(bio17, "../trieste_final/bio17.tif",format="GTiff")
writeRaster(bio18, "../trieste_final/bio18.tif",format="GTiff")
writeRaster(bio19, "../trieste_final/bio19.tif",format="GTiff")


################################################################################
# NOW CHANGE THE REFERENCE SYSTEM TO EPGS 4326
################################################################################

bio1 <- raster("../trieste_final/bio1.tif")
bio2 <- raster("../trieste_final/bio2.tif")
bio3 <- raster("../trieste_final/bio3.tif")
bio4 <- raster("../trieste_final/bio4.tif")
bio5 <- raster("../trieste_final/bio5.tif")
bio6 <- raster("../trieste_final/bio6.tif")
bio7 <- raster("../trieste_final/bio7.tif")
bio8 <- raster("../trieste_final/bio8.tif")
bio9 <- raster("../trieste_final/bio9.tif")
bio10 <- raster("../trieste_final/bio10.tif")
bio11 <- raster("../trieste_final/bio11.tif")
bio12 <- raster("../trieste_final/bio12.tif")
bio13 <- raster("../trieste_final/bio13.tif")
bio14 <- raster("../trieste_final/bio14.tif")
bio15 <- raster("../trieste_final/bio15.tif")
bio16 <- raster("../trieste_final/bio16.tif")
bio17 <- raster("../trieste_final/bio17.tif")
bio18 <- raster("../trieste_final/bio18.tif")
bio19 <- raster("../trieste_final/bio19.tif")

bio1 <- projectRaster(bio1, crs=CRS("+init=epsg:4326"))
bio2 <- projectRaster(bio2, crs=CRS("+init=epsg:4326"))
bio3 <- projectRaster(bio3, crs=CRS("+init=epsg:4326"))
bio4 <- projectRaster(bio4, crs=CRS("+init=epsg:4326"))
bio5 <- projectRaster(bio5, crs=CRS("+init=epsg:4326"))
bio6 <- projectRaster(bio6, crs=CRS("+init=epsg:4326"))
bio7 <- projectRaster(bio7, crs=CRS("+init=epsg:4326"))
bio8 <- projectRaster(bio8, crs=CRS("+init=epsg:4326"))
bio9 <- projectRaster(bio9, crs=CRS("+init=epsg:4326"))
bio10 <- projectRaster(bio10, crs=CRS("+init=epsg:4326"))
bio11 <- projectRaster(bio11, crs=CRS("+init=epsg:4326"))
bio12 <- projectRaster(bio12, crs=CRS("+init=epsg:4326"))
bio13 <- projectRaster(bio13, crs=CRS("+init=epsg:4326"))
bio14 <- projectRaster(bio14, crs=CRS("+init=epsg:4326"))
bio15 <- projectRaster(bio15, crs=CRS("+init=epsg:4326"))
bio16 <- projectRaster(bio16, crs=CRS("+init=epsg:4326"))
bio17 <- projectRaster(bio17, crs=CRS("+init=epsg:4326"))
bio18 <- projectRaster(bio18, crs=CRS("+init=epsg:4326"))
bio19 <- projectRaster(bio19, crs=CRS("+init=epsg:4326"))

plot(bio1)

writeRaster(bio1, "../trieste_final_latlon/bio1.tif",format="GTiff")
writeRaster(bio2, "../trieste_final_latlon/bio2.tif",format="GTiff")
writeRaster(bio3, "../trieste_final_latlon/bio3.tif",format="GTiff")
writeRaster(bio4, "../trieste_final_latlon/bio4.tif",format="GTiff")
writeRaster(bio5, "../trieste_final_latlon/bio5.tif",format="GTiff")
writeRaster(bio6, "../trieste_final_latlon/bio6.tif",format="GTiff")
writeRaster(bio7, "../trieste_final_latlon/bio7.tif",format="GTiff")
writeRaster(bio8, "../trieste_final_latlon/bio8.tif",format="GTiff")
writeRaster(bio9, "../trieste_final_latlon/bio9.tif",format="GTiff")
writeRaster(bio10, "../trieste_final_latlon/bio10.tif",format="GTiff")
writeRaster(bio11, "../trieste_final_latlon/bio11.tif",format="GTiff")
writeRaster(bio12, "../trieste_final_latlon/bio12.tif",format="GTiff")
writeRaster(bio13, "../trieste_final_latlon/bio13.tif",format="GTiff")
writeRaster(bio14, "../trieste_final_latlon/bio14.tif",format="GTiff")
writeRaster(bio15, "../trieste_final_latlon/bio15.tif",format="GTiff")
writeRaster(bio16, "../trieste_final_latlon/bio16.tif",format="GTiff")
writeRaster(bio17, "../trieste_final_latlon/bio17.tif",format="GTiff")
writeRaster(bio18, "../trieste_final_latlon/bio18.tif",format="GTiff")
writeRaster(bio19, "../trieste_final_latlon/bio19.tif",format="GTiff")
