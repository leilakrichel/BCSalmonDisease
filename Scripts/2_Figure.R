# Figure 2: Sea Lice Counts #

## Load packages ## 

library(tidyverse)

## Load the data ## 

motiles <- read.csv("Data/5_SeaLiceCounts.csv")

## Prepare the data ##

motiles <- motiles %>%
  filter(Month %in% c("April", "May", "June")) %>%
  mutate(Mots = .$Average.L..salmonis.motiles.per.fish,
         Mots1 = Mots) 
           
motiles$Mots[which(motiles$Mots > 6)] <- 12      

zones <- motiles %>% 
  filter(Finfish.Aquaculture.Management.Unit != "Barkley Sound") %>% 
  .$Finfish.Aquaculture.Management.Unit %>% unique()

years <- seq(2011,2022)

## Plot the data ##

# tiff(filename = "Plots/2_Figure.tif",
#      width = 29, height = 18, units = "cm", 
#      res = 200,
#      pointsize = 10,
#      compression = "lzw")

par(mfrow = c(3, 3),
    mar = c(0.5, 2, 0.2, 0.2),
    oma=c(4.1,3,4,0.2))

# draw a plot for each sea lice reporting zone using a for loop 

for (i in seq_along(zones)) {
  
# subset data frame by sea lice reporting zone
zones_motiles <- motiles %>%
    filter(Finfish.Aquaculture.Management.Unit == zones[i])

if (i %in% 1:6) {

  plot(x = zones_motiles$Year, y = zones_motiles$Mots,
       xaxt = "n", xlab = NA,
       ylim = c(0, 9), yaxt = "n", ylab = NA,
       type = "p")

# add axes labels 
  axis(side = 1, at = unique(motiles$Year), 
       tick = TRUE, labels = FALSE)
  axis(side = 2, at = c(0, 3, 6), 
       tick = TRUE, labels = TRUE)

} else {
  
  plot(x = zones_motiles$Year, y = zones_motiles$Mots, 
       xaxt = "n", xlab = NA,
       ylim = c(0, 9), yaxt = "n", ylab = NA,
       type = "p")
  
# add axes labels 
  axis(side = 1, at = unique(motiles$Year), tick = TRUE, labels = TRUE)
  axis(side = 2, at = c(0, 3, 6), tick = TRUE, labels = TRUE)
}

# change symbols to represent observations by month

sites <- unique(zones_motiles$Site.Common.Name)

for(j in 1:length(sites)) {
  
  zz1 <- subset(zones_motiles, Site.Common.Name == sites[j])
  
  zz2 <- subset(zz1, Month == "April")
  points(zz2$Year - 0.18, zz2$Mots, pch = 0)
  
  zz2 <- subset(zz1, Month == "May")
  points(zz2$Year, zz2$Mots, pch = 1)
  
  zz2 <- subset(zz1, Month == "June")
  points(zz2$Year + 0.18, zz2$Mots, pch = 2)
  
}

# add label for sea lice reporting zone 
text(x = 2010.5, y = 6.3, zones[i], pos = 4, cex = 1.1)

# add red line for sea lice reporting threshold 
abline(h = 3, col = "red")

# add grey box at the top of the plot 
polygon(x = c(2010.5, 2022.5, 2022.5, 2010.5), 
        y = c(7, 7, 9.5, 9.5),
        col = grey(0.8))

# add sea lice reporting numbers in the grey box
text(c(2011-0.5), 7.5, "Lice>6:", pos=4)
text(c(2011-0.5), 8.5, "Upper:", pos=4)
text(c(2011-0.5), 8, "Lower:", pos=4)

N <-c() # number of observations where number of motiles per fish is over 6
L <-c() # minimum over 6
U <-c() # maximum over 6

for (t in seq_along(years)) {
  
  m <- subset(zones_motiles, 
              Year == unique(zones_motiles$Year)[t])$Mots1
  
  k <- which(m > 6)
  N[t] <- length(k)
  
  if (N[t] > 0) {
  
  L[t] <- min(m[k], na.rm = TRUE)
  U[t] <- max(m[k], na.rm = TRUE)
  
  text(years[t], 8, L[t])
  text(years[t], 8.5, U[t])

  }
  }

text(years[2:length(years)], 7.5, N[2:12])

}

# add common x and y axis labels and legend
mtext('Year', side = 1, line = 2.5, outer = TRUE, cex = 1.2)
mtext('Average number of motiles per fish', 
      side = 2, line = 1, outer = TRUE, cex = 1.2)

# dev.off()

## END ##
