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
bc <- project(bc, LL.CRS) # project to lat / long? 
bc_farms <- project(bc_farms, LL.CRS) # project to lat / long? 
washington <- project(washington, LL.CRS) # project to lat / long? 
alaska <- project(alaska, LL.CRS)

## Load salmon data ##

south_thompson <- read.csv("Data/CK_TotalRun_Final.csv") %>% 
  filter(population == "south_thompson_all") %>% 
  select(year, tot_run)
south_thompson

## Plot the map ##

# The map

# BC map
plot(bc, 
     xlim=c(-130,-124), ylim=c(47, 56), 
     col = "white", border=NA, xaxt="n", yaxt="n", ann=F)
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "lightblue"
     ) 
plot(bc, 
     xlim=c(-130,-124), ylim=c(47, 56), col = "white",
     border=NA, xaxt="n", yaxt="n", ann=F, add = T)

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

# South Thompson Creek 
u <- par("usr")
v <- c(
  grconvertX(u[1:2], "user", "ndc"),
  grconvertY(u[3:4], "user", "ndc")
)
c <- c(0.75, v[2]-0.01, 0.55, v[4]-0.01)
par(fig=c, new=TRUE, mar=c(0,0,0,0))

plot(x = south_thompson$year, y = south_thompson$tot_run,
     xlab = "Year", ylab = "Total run size, Chinook",
     pch = 16)
mtext(text = "South Thompson Creek",
      side = 2)


# 
# rect(par("usr")[1], par("usr")[3],
#      par("usr")[2], par("usr")[4],
#      col = rgb(138,185,229, maxColorValue=255)) # Color
# plot(BC, xlim=c(-128.6,-122.3), ylim=c(48.5, 51.5), col="white", border=NA, xaxt="n", yaxt="n", ann=F, add=T)
# plot(WA, col='grey91', border=NA, xaxt="n", yaxt="n", ann=F, add=T)
# polygon(c(-126.72,-126.72,-126.21,-126.21),c(50.58,50.9,50.9,50.58), bg=NA, lty=1, lwd=1.5) #Broughton
# text(-124.3, 51.7, "British\nColumbia", cex=inset.cex)
# text(-123.8, 47.4, "Washington", cex=inset.cex)
# box()


