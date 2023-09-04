# Figure 1: Aquaculture and commercial fisheries #

## Load packages ## 

library(tidyverse)
library(readxl)

## Load the data ## 

global_farmed <- read.csv("Data/2_GlobalAquaculture.csv") 
global_pacificcommercial <- read.csv("Data/3_GlobalPacificCapture.csv") 
global_atlanticcommercial <- read.csv("Data/4_GlobalAtlanticCapture.csv") 

BC_farmed <- read_excel("Data/1_BCSalmon.xlsx", sheet = 1) # farmed atlantic salmon
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

## Plot the data ##

tiff("Plots/1_Figure.tif")

par(mfrow = c(1, 2))

### A plot ###

# line for farmed atlantic salmon
plot(x = global_farmed$year, y = global_farmed$quantity,
     ylab = "Quantity (tonnes)", xlab = "Year",
     type = "l")
points(x = global_farmed$year, y = global_farmed$quantity,
     pch = 0)

# line for commercially-caught pacific salmon
lines(x = global_pacificcommercial$year, y = global_pacificcommercial$quantity)
points(x = global_pacificcommercial$year, y = global_pacificcommercial$quantity,
       pch = 2)

# line for commercially-caught atlantic salmon
lines(x = global_atlanticcommercial$year, y = global_atlanticcommercial$quantity)
points(x = global_atlanticcommercial$year, y = global_atlanticcommercial$quantity)

# legend
legend("topleft", 
       #title = "BC quantities",
       legend = c("Farmed Atlantic salmon", 
                  "Commercially-caught Atlantic salmon",
                  "Commercially-caught Pacific salmon"),
       pch = c(0, 1, 2))

### B plot ###

# line for commercially caught pacific salmon
plot(x = BC_commercial$year, y = BC_commercial$salmon, 
     ylab = "Quantity (tonnes)", xlab = "Year",
     type = "l",
     ylim = c(0, max(BC_farmed$farmed_atlantic)))
points(x = BC_commercial$year, y = BC_commercial$salmon, 
       pch = 2)

# line for farmed atlantic salmon
lines(x = BC_farmed$year, y = BC_farmed$farmed_atlantic)
points(x = BC_farmed$year, y = BC_farmed$farmed_atlantic, 
       pch = 0, col = "black")

# legend
legend("topleft", 
       #title = "BC quantities",
       legend = c("Farmed Atlantic salmon", 
                  "Commercially-caught Pacific salmon"),
       pch = c(0, 2))

dev.off()

## END ##