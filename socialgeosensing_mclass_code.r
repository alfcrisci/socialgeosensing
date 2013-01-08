##################################################################################
#  
# Authors : Alfonso Crisci & Valentina Grasso
# IBIMET CNR Institute of Biometeorology Firenze via caproni8,50145,Italia                              
# mail:a.crisci@ibimet.cnr.it
# github: https://github.com/alfcrisci/socialgeosensing 
##################################################################################
# Installing libraries and load

doInstall <- TRUE
toInstall <- c("raster", "RColorBrewer","rgdal","maptools","rworldmap","ncdf")
if(doInstall){ install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)


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



rea_aprile_day=brick("rea_aprile_day.nc")

##################################################################################
# Load climate and weather layers as GIS time indexed brick
# WRF-ARW 9km analisys tmax surface   tempreature (*C)
# Source: LaMMA-IBIMET CNR 

wrf_tmax_day=brick("wrf.tmax.heatwave.nc")

##################################################################################

##################################################################################
# Calculation and mapping  

# Tree levels of significance in pixelwise linear association between DTKN and weather done by R lm function data : REANALISYS NCEP NOAA (large scale grid) 

cor_map_s=class_sign_regresvec(rea_aprile_day,DKTN_total)
 
png(file = "REA_sign_lm.png",width = 1024, height = 768, bg = "transparent")
plot(cor_map_s,xlim = c(-5, 20), ylim = c(35,46),col=brewer.pal(3,"Reds"),legend=F)
nations_bounds <- getMap(resolution = "low")
plot(nations_bounds,xlim = c(5, 20), ylim = c(35, 46),asp = 1,add=T)
legend("bottomleft", inset=.05, title="Social network Association",c("No","Weak","Strong"), fill=brewer.pal(3,"Reds"), horiz=TRUE)
dev.off()

# Non parametric  association by spearman rank correlation function 
# not perform because not respect linear hypothesis of association between weather and time-aggregated and keytagged social streams

cor_map=stackcortimevec(rea_aprile_day,DKTN_total,method="spearman")

png(file = "REA_sign_lm.png",width = 1024, height = 768, bg = "transparent")
plot(cor_map,xlim = c(5, 20), ylim = c(30, 46),col=brewer.pal(4,"Reds"),legend=F)
nations_bounds <- getMap(resolution = "low")
plot(nations_bounds,xlim = c(5, 20), ylim = c(35, 46),asp = 1,add=T)
legend("bottomleft", inset=.0, title="Social network Association",c("No","Weak","Noticeable","Strong"), fill=brewer.pal(4,"Reds"), horiz=TRUE)
dev.off()

# Tree levels of significance in pixelwise linear association between DTKN and weather done by R lm function Data WRF ARW 9km daily forecasts (finer scale grid) 


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

