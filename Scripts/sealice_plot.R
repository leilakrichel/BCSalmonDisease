# Figure 2: Sea Lice Counts #

## Load packages ## 

library(tidyverse)

## Load the data ## 
#quartz()

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

labs <- c()
for (i in 1:length(years)) {

  labs[i] <- paste0("'", i + 10)
  
}


## Plot the data ##

png("Plots/sealice_plot.png", 
    width = 2110, height = 1600,
    # width = 20, height = 10,
    # units = "in"
    )

par(mfrow = c(3, 3),
    mar = c(0.5, 4, 0.2, 0.2),
    oma=c(6.5,5,4,0.33),
    mgp = c(0, 1.4, 0))

# draw a plot for each sea lice reporting zone using a for loop 

for (i in seq_along(zones)) {
  
# subset data frame by sea lice reporting zone
zones_motiles <- motiles %>%
    filter(Finfish.Aquaculture.Management.Unit == zones[i])

if (i ==1) {
  
  plot(NA,
       xaxt = "n", xlab = NA,
       ylim = c(0, 10), xlim = c(2010.7, 2022.11),#c(min(motiles$Year), max(motiles$Year)),
       yaxt = "n", ylab = NA,
       #type = "p"
  )
  
  clip(2010, 2019.5, 0, 4)
  abline(h = 3, col = "grey33", lwd = 5.5, lty = 3,
         which = 1)
  clip(2019.5, 2025, 0, 4)
  abline(h = 2, col = "grey33", lwd = 5.5, lty = 3,
         which = 1)
  
  # add axes labels 
  axis(side = 1, at = unique(motiles$Year), 
       tick = TRUE, labels = FALSE)
  axis(side = 2, at = c(0, 3, 6), 
       tick = TRUE, labels = TRUE, cex.axis = 1.8 + 1.5)
  
} else if (i %in% 2:6) {

  plot(NA,
       xaxt = "n", xlab = NA,
       ylim = c(0, 10), xlim = c(2010.7, 2022.11),#c(min(motiles$Year), max(motiles$Year)),
       yaxt = "n", ylab = NA,
       #type = "p"
       )
  
  abline(h = 3, col = "grey33", lwd = 5.5, lty = 3,
         which = 1)

# add axes labels 
  axis(side = 1, at = unique(motiles$Year), 
       tick = TRUE, labels = FALSE)
  axis(side = 2, at = c(0, 3, 6), 
       tick = TRUE, labels = TRUE, cex.axis = 1.8 + 1.5)

} else {
  
  plot(NA, #x = zones_motiles$Year, y = zones_motiles$Mots, 
       xaxt = "n", xlab = NA,
       ylim = c(0, 10), xlim = c(2010.7, 2022.11),#c(min(motiles$Year), max(motiles$Year)),
       yaxt = "n", ylab = NA,
       #type = "p"
       )

  abline(h = 3, col = "grey33", lwd = 5.5, lty = 3,
         which = 1)
  
  
  
# add axes labels 
  axis(side = 1, at = unique(motiles$Year), 
       labels = labs, lwd.ticks = 1.1,
       tick = TRUE, cex.axis = 1.8 + 1.3)
  axis(side = 2, at = c(0, 3, 6), cex.axis = 1.8 + 1.3,
       tick = TRUE, labels = TRUE)
  
}

# change symbols to represent observations by month

sites <- unique(zones_motiles$Site.Common.Name)

for(j in 1:length(sites)) {
  
  zz1 <- subset(zones_motiles, Site.Common.Name == sites[j])
  
  zz2 <- subset(zz1, Month == "April")
  points(zz2$Year - 0.23, zz2$Mots, pch = 21,
         col = alpha("black", 0.25),
         bg = alpha("black", 0.2),
         cex = 2.8 + 1.3)

  zz2 <- subset(zz1, Month == "May")
  points(zz2$Year, zz2$Mots, pch = 22, 
         col = alpha("black", 0.25),
         bg = alpha("black", 0.2),
         cex = 2.8 + 1.3)

  zz2 <- subset(zz1, Month == "June")
  points(zz2$Year + 0.23, zz2$Mots, pch = 24, 
         col = alpha("black", 0.25),
         bg = alpha("black", 0.2),
         cex = 2.8 + 1.3)
  
}

# add label for sea lice reporting zone 
text(x = 2010.5 - 0.1 , y = 6.3, 
     zones[i], pos = 4, 
     cex = 2.3 + 1.5, font = 4)

# add red line for sea lice reporting threshold 
# abline(h = 3, col = "grey33", lwd = 3, lty = 3,
#        which = 1)

# add grey box at the top of the plot 
polygon(x = c(2010.2, 2022.6, 
              2022.6, 2010.2), 
        y = c(7, 7, 10.5, 10.5),
        col = grey(0.8))

# add sea lice reporting numbers in the grey box
text(c(2011-0.75), 9.8, "Upper:", pos=4, cex = 1.8 + 1.3)
text(c(2011-0.75), 8.65, "Lower:", pos=4, cex = 1.8 + 1.3)
text(c(2011-0.75), 7.5, "Lice>6:", pos=4, cex = 1.8 + 1.3)

N <-c() # number of observations where number of motiles per fish is over 6
L <-c() # minimum over 6
U <-c() # maximum over 6

for (t in seq_along(years)) {
  
  m <- subset(
    zones_motiles, 
    Year == unique(zones_motiles$Year)[t])$Mots1
  
  k <- which(m > 6)
  N[t] <- length(k)
  
  if (N[t] > 0) {
  
  L[t] <- min(m[k], na.rm = TRUE)
  U[t] <- max(m[k], na.rm = TRUE)
  
  text(years[t] + 0.1, 8.65, L[t] %>% round(1), cex = 1.7 + 1.3)
  text(years[t] + 0.1, 9.8, U[t] %>% round(1), cex = 1.7 + 1.3)

  }
  }

text(years[2:length(years)] + 0.1, 7.55, N[2:12], cex = 1.7 + 1.3)

}

# add common x and y axis labels and legend
mtext('Year', side = 1, 
      line = 5, outer = TRUE, cex = 2 + 1.3)

ylabel <- expression(Mean~motile~italic(L.~salmonis)~abundance~on~farms)
mtext(ylabel, 
      side = 2, 
      line = 1.3, outer = TRUE, cex = 2 + 1.3)


par(fig = c(0, 1, 0, 1), 
    oma = c(0, 0, 0, 0), 
    mar = c(0, 0, 0, 0), 
    new = TRUE,
    adj = 1)

plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(-0.89, 1.097,
       legend = c("April", "May", "June"),
       pch = c(21, 22, 24), cex = 2 + 1.5, 
       horiz = TRUE, 
       #fill = gray(c(0.6, 0.3, 0.9)),
       #pt.col = alpha("black", 1),
       pt.lwd = 0.3,
       pt.bg = gray(c(0.85, 0.85, 0.85)),
       pt.cex = 4.5,
       seg.len = 1, bty = "n")
text(-0.90, 1.052, 'Month', 
      cex = 2 + 1.5, font = 2)

# abline(h = -0.102, col = "grey33", lwd = 3, lty = 3)

dev.off()

## END ##
