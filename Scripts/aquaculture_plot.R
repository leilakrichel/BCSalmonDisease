# Figure 1: Aquaculture and commercial fisheries #

## Load packages ## 

library(tidyverse)
library(readxl)

## Load the data ## 

global_farmed <- read.csv("Data/2_GlobalAquaculture.csv") 
global_pacificcommercial <- read.csv("Data/3_GlobalPacificCapture.csv") 
global_atlanticcommercial <- read.csv("Data/4_GlobalAtlanticCapture.csv") 

BC_farmed <- read_excel("Data/1_BCSalmon.xlsx", sheet = 3) # farmed atlantic salmon
BC_commercial <- read_excel("Data/1_BCSalmon.xlsx", sheet = 2) # commercially-caught pacific salmon 

## Prepare the data ## 

# Farmed salmon global 

global_farmed <- global_farmed %>%
  select(-seq(2, 146, 2))
colnames(global_farmed) <- c("country", as.character(2021:1950))
global_farmed <- global_farmed %>%
  gather(key = "year", value = "quantity",-1) %>% 
  group_by(year) %>% 
  summarize(quantity = sum(quantity, na.rm = TRUE))

# Global commercial pacific salmon capture 

global_pacificcommercial <- global_pacificcommercial %>%
  select(-seq(2, 146, 2))
colnames(global_pacificcommercial) <- c("country", as.character(2021:1950))
global_pacificcommercial <- global_pacificcommercial %>%
  gather(key = "year", value = "quantity",-1) %>% 
  group_by(year) %>% 
  summarize(quantity = sum(quantity, na.rm = TRUE))

# Global commercial atlantic salmon capture 

global_atlanticcommercial <- global_atlanticcommercial %>% 
  select(-seq(2, 146, 2)) 
colnames(global_atlanticcommercial) <- c("country", as.character(2021:1950))  
global_atlanticcommercial <- global_atlanticcommercial %>% 
  gather(key = "year", value = "quantity", -1)%>% 
  group_by(year) %>% 
  summarize(quantity = sum(quantity, na.rm = TRUE))

# BC farmed atlantic salmon

BC_farmed <- BC_farmed %>%
  filter(year >= 1991) %>%
  transmute(year,
            farmed_atlantic = BC_TotalFarmedSalmon - (Canada_FarmedSalmon - Canada_FarmedAtlantic)) %>%
  select(year, farmed_atlantic) %>%
  rbind(BC_farmed %>%
          filter(year %in% 1986:1990) %>%
          transmute(year,
                    farmed_atlantic = BC_FarmedAtlantic)) %>% 
  arrange(year)

years <- seq(1950, 2020, by = 10)

## Plot the data ##

# tiff(filename = "Plots/1_Figure.tif",
#      width = 27, height = 15, units = "cm", 
#      res = 200,
#      pointsize = 10,
#      compression = "lzw")

par(mfrow = c(1, 2), 
    mgp = c(1, 0.1, 0),
    mar = c(2, 3, 1.8, 0.5), 
    cex.axis = 0.72, tck = -0.01, cex.lab = 1)

### A plot ###

# line for farmed atlantic salmon
plot(x = global_farmed$year, 
     y = global_farmed$quantity,
     ylab = "Tonnes", xlab = "Year",
     type = "l", lty = 2, lwd = 1.8, yaxt = "n")

# points(x = global_farmed$year, y = global_farmed$quantity,
#        pch = 16, cex = 0.8)

# line for commercially-caught pacific salmon
lines(x = global_pacificcommercial$year, 
      y = global_pacificcommercial$quantity,
      lwd = 1.8)
# points(x = global_pacificcommercial$year, y = global_pacificcommercial$quantity,
#        pch = 2, cex = 0.8)

# line for commercially-caught atlantic salmon
lines(x = global_atlanticcommercial$year, 
      y = global_atlanticcommercial$quantity,
      lty = 3, lwd = 1.8)
# points(x = global_atlanticcommercial$year, 
#        y = global_atlanticcommercial$quantity,
#        lty = 3, cex = 0.8)

# legend
legend("topleft", 
       #title = "BC quantities",
       legend = c("Farmed Atlantic salmon", 
                  "Commercially-caught Pacific salmon",
                  "Commercially-caught Atlantic salmon"),
       lty = c(2, 1, 3), lwd = 1.8, cex = 0.85, bty = "n"
       # pch = c(16, 2, 0), cex = 0.85, bty = "n"
       )

labels_at <- seq(global_farmed$quantity %>% min, 3e6, by = 1e6)
labels <- scales::label_scientific(digits=2)(labels_at)
axis(2, at = labels_at, labels = labels)

title("A: Global", cex.main = 1.1, font = 2, adj = 0.03, line = 0.2)

### B plot ###

# line for commercially caught pacific salmon
plot(x = BC_commercial$year, y = BC_commercial$salmon, 
     ylab = "Tonnes", xlab = "Year",
     type = "l", xaxt = "n", yaxt = "n", tck = -0.01, lty = 1, lwd = 1.8,
     ylim = c(0, max(BC_farmed$farmed_atlantic) + 3e4))
# points(x = BC_commercial$year, y = BC_commercial$salmon, 
#        pch = 2,cex = 0.8)

# line for farmed atlantic salmon
lines(x = BC_farmed$year, y = BC_farmed$farmed_atlantic, lty = 2, lwd = 2.1)
# points(x = BC_farmed$year, y = BC_farmed$farmed_atlantic, 
#        pch = 16, cex = 0.8)

# add custom x-axis
axis(1, at = seq(1950, 2020, by = 10))
# axis(2, at = seq(0, 150000, 
#      length.out = 6))

# legend
legend("topleft", 
       #title = "BC quantities",
       legend = c("Farmed Atlantic salmon", 
                  "Commercially-caught Pacific salmon"),
       lty = c(2, 1, 3), lwd = 1.8, cex = 0.85, bty = "n")

labels_at <- seq(0, 12e5, by = 3e4)
labels <- scales::label_scientific(digits=2)(labels_at)
axis(2, at = labels_at, labels = labels)

# add plot label 
title("B: British Columbia", cex.main = 1.1, font = 2, adj = 0.03, line = 0.2)



# dev.off()

## END ##