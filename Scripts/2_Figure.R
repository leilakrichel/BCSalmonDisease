# Figure 2: Sea Lice Counts #

## Load packages ## 

library(tidyverse)

## Load the data ## 

motiles <- read.csv("Data/5_SeaLiceCounts.csv")

## Prepare the data ##

motiles <- motiles %>%
  filter(Month %in% c("April", "May", "June")) %>%
  mutate(Mots = .$Average.L..salmonis.motiles.per.fish) 
           
motiles$Mots[which(motiles$Mots > 6)] <- 12      

zones <- motiles %>% 
  filter(Finfish.Aquaculture.Management.Unit != "Barkley Sound") %>% 
  .$Finfish.Aquaculture.Management.Unit %>% unique()

years <- seq(2011,2022)

## Plot the data ##

par(mfrow = c(3, 3),
    mar = c(0.5, 2, 0.2, 0.2),
    oma=c(4.1,3,4,0.2))

# draw a plot for each sea lice reporting zone using the for loop 

for (i in seq_along(zones)) {

# subset dataframe by sea lice reporting zone 
subset_motiles <- motiles %>%
  filter(Finfish.Aquaculture.Management.Unit == zones[i])

if (i %in% 1:6) {

  plot(x = subset_motiles$Year, y = subset_motiles$Mots,
       xaxt = "n", xlab = NA,
       ylim = c(0, 9), yaxt = "n", ylab = NA,
       type = "p", pch = c(16, 17, 18))

# add axes labels 
  axis(side = 1, at = unique(motiles$Year), 
       tick = TRUE, labels = FALSE)
  axis(side = 2, at = c(0, 3, 6), 
       tick = TRUE, labels = TRUE)

} else {
  
  plot(x = subset_motiles$Year, y = subset_motiles$Mots, 
       xaxt = "n", xlab = "Year",
       ylim = c(0, 9), yaxt = "n", ylab = NA,
       type = "p", pch = c(16, 17, 18))
  
  # add axes labels 
  axis(side = 1, at = unique(motiles$Year), tick = TRUE, labels = TRUE)
  axis(side = 2, at = c(0, 3, 6), tick = TRUE, labels = TRUE)
}

# add label for sea lice reporting zone 
text(x = 2010.5, y = 6.3, zones[i], pos = 4)

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

N <-c() 
L <-c() 
U <-c()

for (t in seq_along(years)) {
  
  m <- subset(subset_motiles, 
              Year == unique(subset_motiles$Year)[t])$Mots
  
  k <- which(m > 6)
    # ifelse(which(m > 6) > 0, 
    #           which(m > 6), 
    #           as.numeric(0))
  N[t] <- length(k)
  L[t] <- min(m[k], na.rm = TRUE)
  U[t] <- max(m[k], na.rm = TRUE)
  
}

text(years[2:length(years)], 7.5, N[2:12])

for (tt in seq_along(years)) {
  
  if (N[tt] > 0) {
  
  text(years[tt], 8, L[tt])
    
  }
   
}

}

