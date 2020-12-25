
setwd("C:\\Users\\santo\\OneDrive\\Desktop\\forQgis") 

####### For mapping
library(gstat)# Use gstat's idw routine
library(sp)# Used for the spsample function
library(sf)
library(GISTools)
library(tmaptools)
library(rgdal)
library(ranger)
library("geoR")
library("raster")
library(GSIF)# Global Soil Information Facilities
library(raster)
library(dplyr)
library(spData)
library(readxl)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(shiny)   # for web applications
## 

CAF<- st_read("south_field\\south_field.shp",stringsAsFactors=FALSE)
CAF_sf<-st_as_sf(CAF)
st_crs(CAF_sf)  ## UTMZ11
head(CAF_sf)
st_bbox(CAF_sf)
plot(CAF_sf)



Scaf<-read_excel("terrain attributes.xlsx")
Scaf= na.omit(Scaf)#nemove NAs
summary(Scaf)
str(Scaf)
head(Scaf)
class(Scaf)

coordinates(Scaf) <- ~EASTING + NORTHING
projection(Scaf) <- projection(CAF_sf)
class(Scaf)
summary(Scaf) 
names(Scaf)



# Create an empty grid where n is the total number of cells

grd <- as.data.frame(spsample(Scaf, "regular", n = 100000))

names(grd)       <- c("EASTING", "NORTHING")
coordinates(grd) <- c("EASTING", "NORTHING")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
plot(grd)

# Add Scaf's projection information to the empty grid

proj4string(grd) <- proj4string(Scaf)

# Interpolate the grid cells using a power value of 2 (idp=2.0)

ele.idw <- gstat::idw(Ele ~ 1, Scaf, newdata=grd, idp=2.0)# Slope interpolation
slope.idw <- gstat::idw(S ~ 1, Scaf, newdata=grd, idp=2.0)# elevation interpoltion
asp.idw <- gstat::idw(Asp ~ 1, Scaf, newdata=grd, idp=2.0)# Slope interpolation
TCa.idw <- gstat::idw(TCA ~ 1, Scaf, newdata=grd, idp=2.0)
prc.idw <- gstat::idw(prcur ~ 1, Scaf, newdata=grd, idp=2.0)# Slope interpolation
tcu.idw <- gstat::idw(tcur ~ 1, Scaf, newdata=grd, idp=2.0)# elevation interpoltion
ah.idw <- gstat::idw(AH ~ 1, Scaf, newdata=grd, idp=2.0)# Slope interpolation
Ci.idw <- gstat::idw(CI ~ 1, Scaf, newdata=grd, idp=2.0)
twi.idw <- gstat::idw(TWI ~ 1, Scaf, newdata=grd, idp=2.0)# Slope interpolation
Vd.idw <- gstat::idw(VD ~ 1, Scaf, newdata=grd, idp=2.0)# elevation interpoltion
agsr.idw <- gstat::idw(AGSR ~ 1, Scaf, newdata=grd, idp=2.0)# Slope interpolation

r<- raster(ele.idw)
r.m <- mask(r, CAF_sf)

r1<- raster(slope.idw)
r1.m <- mask(r1, CAF_sf)

r2<- raster(asp.idw)
r2.m <- mask(r2, CAF_sf)

r3<- raster(TCa.idw)
r3.m <- mask(r3, CAF_sf)

r4<- raster(prc.idw)
r4.m <- mask(r4, CAF_sf)

r5<- raster(tcu.idw)
r5.m <- mask(r5, CAF_sf)

r6<- raster(ah.idw)
r6.m <- mask(r6, CAF_sf)

r7<- raster(Ci.idw)
r7.m <- mask(r7, CAF_sf)

r8<- raster(twi.idw)
r8.m <- mask(r8, CAF_sf)

r9<- raster(Vd.idw)
r9.m <- mask(r9, CAF_sf)

r10<- raster(agsr.idw)
r10.m <- mask(r10, CAF_sf)

#overlaying multiple plots



ele<- tm_shape(r.m)+
  tm_raster(n=5,palette = "RdBu", style = "cont", auto.palette.mapping = FALSE,
            title="Elevation \n (m)") + tm_layout(inner.margins = c(0,0.01, 0, 0.05))+
  tm_shape(Scaf) + tm_dots() +
  tm_legend(legend.outside=F)


slop <- tm_shape(r1.m) + 
  tm_raster(n=5,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Slope \n (degree)") + tm_layout(inner.margins = c(0,0.01, 0, 0.05))+
  
  tm_shape(Scaf) + tm_dots()+ 
  tm_legend(legend.outside=F)
print(slop)

asp<- tm_shape(r2.m) + 
  tm_raster(n=5,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Aspect \n (degree)") + tm_layout(inner.margins = c(0,0.01, 0, 0.05))+
  tm_shape(Scaf) + tm_dots() +
  tm_legend(legend.outside=F)


tca <- tm_shape(r3.m) + 
  tm_raster(n=5,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Total catchment area \n (m²)") + tm_layout(inner.margins = c(0,0.01, 0, 0.05))+
  tm_shape(Scaf) + tm_dots(size=) + 
  tm_legend(legend.outside=F)

prc<- tm_shape(r4.m) + 
  tm_raster(n=5,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Profile curvature \n (radians/m)") + tm_layout(inner.margins = c(0,0.01, 0, 0.05))+
  tm_shape(Scaf) + tm_dots(size=) +
  tm_legend(legend.outside=F)


tcur <- tm_shape(r5.m) + 
  tm_raster(n=5,palette = "RdBu" , auto.palette.mapping = FALSE,
            title="Tangential curvature \n (radians/m)") + tm_layout(inner.margins = c(0,0.01, 0, 0.05))+
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_scale_bar(position = c(0.2, .005), size=1)+
  tm_shape(Scaf) + tm_dots() + 
  tm_legend(legend.outside=F)

AH <- tm_shape(r6.m) + 
  tm_raster(n=5,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Analytical hillshade \n (radian)") + tm_layout(inner.margins = c(0,0.01, 0, 0.05))+
  tm_shape(Scaf) + tm_dots() + 
  tm_legend(legend.outside=F)

CI<- tm_shape(r7.m) + 
  tm_raster(n=5,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Convergence index") + tm_layout(inner.margins = c(0,0.01, 0, 0.05))+
            tm_compass(type = "8star", position = c("right", "top")) +
  tm_scale_bar(position = c(0.2, .005), size=1)+
  tm_shape(Scaf) + tm_dots() +
  tm_legend(legend.outside=F)


twi <- tm_shape(r8.m) + 
  tm_raster(n=,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Topographic wetness index") + tm_layout(inner.margins = c(0,0.01, 0, 0.05))+
  tm_shape(Scaf) + tm_dots() + 
  tm_legend(legend.outside=F)

vd <- tm_shape(r9.m) + 
  tm_raster(n=5,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Valley depth \n (m)") + tm_layout(inner.margins = c(0,0.01, 0, 0.05))+
  tm_shape(Scaf) + tm_dots() + 
  tm_legend(legend.outside=F)

Agsr <- tm_shape(r10.m) + 
  tm_raster(n=5,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Annual global solar radiation \n (kWh/m²/year)") + tm_layout(inner.margins = c(0,0.01, 0, 0.05))+
  tm_shape(Scaf) + tm_dots() + 
  tm_legend(legend.outside=F)


# savings graphs


tmap_save(ele+ tm_layout(inner.margins = c(0,0.02, 0, 0.05)),
          "elevation IDW.png", width = 1920, height = 1080, asp =1.5)
tmap_save(slop+ tm_layout(inner.margins = c(0,0.02, 0, 0.05)),
          "slope IDW.png", width = 1920, height = 1080, asp =1.5)
tmap_save(asp+ tm_layout(inner.margins = c(0,0.02, 0, 0.05)),
          "aspect IDW.png", width = 1920, height = 1080, asp =1.5)
tmap_save(tca+ tm_layout(inner.margins = c(0,0.02, 0, 0.05)),
          "TCA IDW.png", width = 1920, height = 1080, asp =1.5)
tmap_save(prc+ tm_layout(inner.margins = c(0,0.02, 0, 0.05)),
          "PRC IDW.png", width = 1920, height = 1080, asp =1.5)
tmap_save(tcur+ tm_layout(inner.margins = c(0,0.02, 0, 0.05)),
          "TCUR IDW.png", width = 1920, height = 1080, asp =1.5)

tmap_save(AH+ tm_layout(inner.margins = c(0,0.02, 0, 0.05)),
          "Analytical Hilshade IDW.png", width = 1920, height = 1080, asp =1.5)
tmap_save(CI+ tm_layout(inner.margins = c(0,0.02, 0, 0.05)),
          "ConvergenceIDW.png", width = 1920, height = 1080, asp =1.5)
tmap_save(twi+ tm_layout(inner.margins = c(0,0.02, 0, 0.05)),
          "Total wetness.png", width = 1920, height = 1080, asp =1.5)
tmap_save(vd+ tm_layout(inner.margins = c(0,0.02, 0, 0.05)),
          "VD IDW.png", width = 1920, height = 1080, asp =1.5)
tmap_save(Agsr+ tm_layout(inner.margins = c(0,0.02, 0, 0.05)),
          "AGSR IDW.png", width = 1920, height = 1080, asp =1.5)

tmap_save(ele+ tm_layout(inner.margins = c(0,0.02, 0, 0.05)),
          "eleIDW.png", width = 1920, height = 1080, asp =1.5)
