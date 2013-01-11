##################################################################################
#  
# Authors : Alfonso Crisci & Valentina Grasso
# IBIMET CNR Institute of Biometeorology Firenze via caproni 8,50145,Italia                              
# mail: a.crisci@ibimet.cnr.it
# file: socialgeosensing_mclass_code.r
# github: https://github.com/alfcrisci/socialgeosensing 
##################################################################################
# Installing libraries and load


if (!require("RColorBrewer")) {install.packages("RColorBrewer");library(RColorBrewer)}
if (!require("raster")) {install.packages("raster");library(raster)}
if (!require("rgdal")) {install.packages("rgdal");library(rgdal)}
if (!require("maptools")) {install.packages("maptools");library(maptools)}
if (!require("rworldmap")) {install.packages("rworldmap");library(rworldmap)}
if (!require("ncdf")) {install.packages("ncdf");library(ncdf)}


##################################################################################
# Call specific functions for socialgesensing

source("grid_functions.r")


##################################################################################
# Daily keytagged Twitter Number: aggregating italian native tweets (noreplies & no RT )
# keytags: AFA OR CALDO OR SETE language:it

DKTN_total<-c(333,865,1221,1372,805,679)

##################################################################################
# Load climate and weather layers as GIS time indexed brick
# NOAA NCEP reanalisys 1  850 hPa tempreature (*K)



rea_aprile_day=brick("data_weather/rea_aprile_day.nc")

##################################################################################
# Load climate and weather layers as GIS time indexed brick
# WRF-ARW 9km analisys tmax surface   tempreature (*C)
# Source: LaMMA-IBIMET CNR 

<<<<<<< HEAD
<<<<<<< HEAD
wrf_tmax_day=brick("data_weather/wrf.tmax.heatwave.nc")
=======
wrf_tmax_day=brick("wrf.tmax.heatwave.nc")
>>>>>>> 6474d0cf4a7e0d821823b1915cd20dba6014fdef
=======
wrf_tmax_day=brick("data_weather/wrf.tmax.heatwave.nc")
>>>>>>> parent of 0d3c880... erase

##################################################################################

##################################################################################
# Calculation and mapping  

<<<<<<< HEAD
<<<<<<< HEAD
# Linear association by R lm function REANALISYS (http://reanalysis.org/)large scale grid 
=======
# Tree levels of significance in pixelwise linear association between DTKN and weather done by R lm function data : REANALISYS NCEP NOAA (large scale grid) 
>>>>>>> 6474d0cf4a7e0d821823b1915cd20dba6014fdef
=======
# Linear association by R lm function REANALISYS (http://reanalysis.org/)large scale grid 
>>>>>>> parent of 0d3c880... erase

cor_map_s=class_sign_regresvec(rea_aprile_day,DKTN_total)
 
png(file = "REA_sign_lm.png",width = 1024, height = 768, bg = "transparent")
plot(cor_map_s,xlim = c(-5, 20), ylim = c(35,46),col=brewer.pal(3,"Reds"),legend=F)
nations_bounds <- getMap(resolution = "low")
plot(nations_bounds,xlim = c(5, 20), ylim = c(35, 46),asp = 1,add=T)
legend("bottomleft", inset=.05, title="Social network Association",c("No","Weak","Strong"), fill=brewer.pal(3,"Reds"), horiz=TRUE)
dev.off()

# Non parametric  association by spearman rank correlation function 
<<<<<<< HEAD
# not perform because not respect linear hypothesis of association between weather and time-aggregated/keytagged social streams metric.
=======

>>>>>>> parent of 0d3c880... erase

cor_map=stackcortimevec(rea_aprile_day,DKTN_total,method="spearman")

png(file = "REA_sign_lm.png",width = 1024, height = 768, bg = "transparent")
plot(cor_map,xlim = c(5, 20), ylim = c(30, 46),col=brewer.pal(4,"Reds"),legend=F)
nations_bounds <- getMap(resolution = "low")
plot(nations_bounds,xlim = c(5, 20), ylim = c(35, 46),asp = 1,add=T)
legend("bottomleft", inset=.0, title="Social network Association",c("No","Weak","Noticeable","Strong"), fill=brewer.pal(4,"Reds"), horiz=TRUE)
dev.off()

<<<<<<< HEAD
<<<<<<< HEAD
# Linear association by R lm function WRF-ARW (http://wrf-model.org) finer scale grid 
=======
# Tree levels of significance in pixelwise linear association between DTKN and weather done by R lm function Data WRF ARW 9km daily forecasts (finer scale grid) 
>>>>>>> 6474d0cf4a7e0d821823b1915cd20dba6014fdef
=======
# Linear association by R lm function WRF-ARW (http://wrf-model.org) finer scale grid 
>>>>>>> parent of 0d3c880... erase


cor_map_w=class_sign_regresvec(wrf_tmax_day,DKTN_total) 

png(file = "WRF_sign_lm.png",width = 1024, height = 768, bg = "transparent")
plot(cor_map_w,xlim = c(0, 20), ylim = c(35, 47),col=brewer.pal(3,"Reds"),legend=F)
nations_bounds <- getMap(resolution = "low")
plot(nations_bounds,xlim = c(0, 20), ylim = c(35, 47),asp = 1,add=T)
legend("bottomleft", inset=.05, title="Social network Association",c("No","Weak","Strong"), fill=brewer.pal(3,"Reds"), horiz=TRUE)
dev.off()



##################################################################################
# Reference

#http://www.naturalearthdata.com/forums/topic.php?id=400
#http://www.milanor.net/blog/?p=534

