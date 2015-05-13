# get adjacency metrics   

# load packages and data

# clear workspace
rm(list = ls())
# packages
library(sp)
library(raster)
library(Matrix)
library(maps)
library(rgdal)
library(maptools)
library(shapefiles)
library(fields)
library(spdep)
library(igraph)

# set working directory

# load shapefiles
admin0 <- shapefile('../data/shapefiles/countries_wa')
# plot 
#plot(admin0)
# simplify shapefile
#tmp <- gSimplify(admin0, tol = 1, topologyPreserve = TRUE)
#plot(tmp)
# calculate adjacency matrix
# generate a grid of pixels 
nb.r <- poly2nb(admin0,
                queen = TRUE)


mat <- nb2mat(nb.r,
              style = 'B',
              zero.policy = TRUE) 

colnames(mat) <- rownames(mat) <- admin0$NAME

tmp <- nblag(nb.r, 
             maxlag = 8)

tmp1 <- nblag_cumul(tmp)
