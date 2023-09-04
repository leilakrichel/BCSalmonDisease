# Figure 1: Aquaculture and commercial fisheries #

## Load packages ##

library(tidyverse)
library(readxl)

## Load the data ##

# Set wd to BCSalmonDisease git repo

# Data compiled from:
## DFO:
## FAO: 

# global_farmed <- read.csv("Data/aquaculture_quantity.csv")
# global_pacificcommercial
# global_atlanticcommercial

BC_farmed <- read_excel("Data/fisheriesdata.xlsx", sheet = 1) # farmed atlantic salmon
BC_commercial <- read_excel("Data/fisheriesdata.xlsx", sheet = 2) # commercially-caught pacific salmon 

## Prepare the data ## 

# Need to get numbers for farmed atlantic salmon in BC

BC_farmed <- BC_farmed %>%
  filter(year >= 1991) %>%
  select(1, 2, 5, 6, 7) %>%
  transmute(year,
            salmon = DFO_BCsalmon - (DFO_sum - FAO_atlantic)) %>%
  select(year, salmon) %>%
  rbind(BC_farmed %>%
          filter(year %in% 1986:1990) %>%
          transmute(year,
                    salmon = DFO_BCsalmon)) %>% 
  arrange(year)

## Plot the data ##

### A plot ###

### B plot ###

# line for commercially caught pacific salmon
plot(x = BC_commercial$year, y = BC_commercial$salmon, 
     ylab = "Quantity (tonnes)", xlab = "Year",
     type = "l")
points(x = BC_commercial$year, y = BC_commercial$salmon, 
       pch = 2)

# line for farmed atlantic salmon
lines(x = BC_farmed$year, y = BC_farmed$salmon)
points(x = BC_farmed$year, y = BC_farmed$salmon, 
       pch = 0, col = "black")

# legend
legend("topleft", 
       #title = "BC quantities",
       legend = c("Farmed Atlantic salmon", 
                  "Commercially-caught Pacific salmon"),
       pch = c(0, 2))

### Paste A + B plots together ###


## END ##