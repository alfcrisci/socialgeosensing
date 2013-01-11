##################################################################################
<<<<<<< HEAD
#  
# Authors : Alfonso Crisci & Valentina Grasso
# IBIMET CNR Institute of Biometeorology Firenze via caproni 8,50145,Italia                              
# mail: a.crisci@ibimet.cnr.it
# file: grid_functions.r
# github: https://github.com/alfcrisci/socialgeosensing 
# Geosensing functions
##################################################################################

=======
#
# Authors : Alfonso Crisci & Valentina Grasso
# IBIMET CNR Institute of Biometeorology Firenze via caproni 8, 50145, Italia
# mail:a.crisci@ibimet.cnr.it
# github: https://github.com/alfcrisci/socialgeosensing

# R Functions for weather grids elaboration
##################################################################################


>>>>>>> 6474d0cf4a7e0d821823b1915cd20dba6014fdef
require(raster)
require(rgdal)

class_sign_regresvec<- function(x, vectime) {
        lmregr_i <- function(x) { summary(lm(x ~ vectime))$coefficients[8] }
        
         reclassify(calc(x, fun=lmregr_i), c(-Inf,0.05,2, 0.051,0.10,1,0.101,Inf,NA))
}


class_resign_cortimevec <- function(x, vectime, method='spearman') {
        myscor <- function(x) {
                cor.test(x, vectime, method=method)$p.value 
        }
        reclassify(calc(x, fun=myscor), c(-Inf,0.05,2, 0.051,0.10,1,0.101,Inf,NA))
       
}


sign_regresvec<- function(x, vectime) {
        lmregr_i <- function(x) { summary(lm(x ~ vectime))$coefficients[8] }
        calc(x, fun=lmregr_i )
}


sign_cortimevec <- function(x, vectime, method='spearman') {
        myscor <- function(x) {
                cor.test(x, vectime, method=method)$p.value 
        }
        calc(x, fun=myscor)
}





regresvec<- function(x, vectime) {
        lmregr <- function(x) { lm(x ~ vectime)$coefficients }
        calc(x, fun=lmregr )
}

stackregresvec<- function(x, vectime) {
        lmregr <- function(x) { lm(x ~ vectime)$coefficients }
        calc(x, fun=lmregr )
}


stackcortimevec <- function(x, vectime, method='spearman') {
        mycor <- function(x) {
                 cor(x, vectime, method=method)
        }
        calc(x, fun=mycor )
}

stack2stack_cor <- function(s1, s2, method='spearman') {
        mycor <- function(v) {
                x <- v[1:split]
                y <- v[(split+1):(2*split)]
                cor(x, y, method=method)
        }
        s <- stack(s1, s2)
        split <- nlayers(s)/2
        calc(s, fun=mycor )
}

##############################################################
