# Figure 1: Aquaculture and commercial fisheries #

## Load packages ##

library(tidyverse)
library(readxl)

## Load the data ##

# The excel file contains data for:
## Commerically-caught salmon in BC over time from DFO 
## Farmed salmon production in BC (from DFO) and in Canada (from FAO) over time 


# global_farmed <- read.csv("Data/aquaculture_quantity.csv")
# global_pacificcommercial
# global_atlanticcommercial

BC_farmed <- read_excel("Data/1_BCSalmon.xlsx", sheet = 1) # farmed atlantic salmon
BC_commercial <- read_excel("Data/1_BCSalmon.xlsx", sheet = 2) # commercially-caught pacific salmon 

## Prepare the data ## 

# Need to get numbers for farmed atlantic salmon in BC

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

### A plot ###

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

### Paste A + B plots together ###


## END ##