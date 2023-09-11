# Figure 3: BC Coastline and salmon time-series #

## Load packages ##
  
library(terra)
library(bcmaps)
library(tidyverse)

## Load shapefiles ##

bc <- vect(bc_bound_hres())
bc_farms <- vect("Data/shapefiles/bc_farms.shp") # BC salmon farms 
alaska <- vect("Data/shapefiles/alaska.shp") # washington
washington <- vect("Data/shapefiles/washington.shp") # washington

## Project coordinate system ## 

LL.CRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
bc <- project(bc, LL.CRS)  
bc_farms <- project(bc_farms, LL.CRS) 
washington <- project(washington, LL.CRS) 
alaska <- project(alaska, LL.CRS)

## Load salmon data ##

# robertson <- read.csv("Data/8_RobertsonChinook.csv") %>% 
#   filter(Stock == "Robertson") 
# 
# south_thompson <- read.csv("Data/9_SThompsonChinook.csv") %>% 
#   filter(population == "south_thompson_all") %>% 
#   select(year, tot_run)

## Plot the map ##

# The map

# BC map
plot(bc, 
     xlim=c(-130,-124), ylim=c(47, 56), 
     col = "white", border=NA, xaxt="n", yaxt="n", ann=F)
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "lightblue") 
plot(bc, 
     xlim=c(-130,-124), ylim=c(47, 56), 
     col = "white", border=NA, xaxt="n", yaxt="n", ann=F, add = T)

# Washington 
plot(washington, 
     col='gray', border=NA, xaxt="n", yaxt="n", ann=F, add=T)

# Alaska
plot(alaska,
     col='gray', border=NA, xaxt="n", yaxt="n", ann=F, add=T)

# Farms along BC coast 
points(bc_farms)
box()

# Now add the insets

# Robertson

# coords1 <- c(0.11, 0.34, # x1, x2
#              0.25, 0.4) # y1, y2
# 
# par(fig = coords1, new = TRUE, mar = c(0,0,0,0))
# 
# plot(x = robertson$SmoltYear, y = robertson$SAR,
#      xlab = "Year", ylab = "Marine survival, Chinook",
#      pch = 16,
#      cex.axis = 0.75)

# South Thompson Creek 

# coords2 <- c(0.71, 0.94, # x1, x2
#             0.45, 0.6) # y1, y2
# 
# par(fig = coords2, new = TRUE, mar = c(0,0,0,0))
# 
# plot(x = south_thompson$year, y = south_thompson$tot_run,
#      xlab = "Year", ylab = "Total run size, Chinook",
#      pch = 16,
#      cex.axis = 0.75)
# 
# 



