#### Time-series for map ####

library(readxl)

# contains data for numerous systems

bc_salmon <- read.csv("Data/bc_spawners.csv") 

# other data will be called separately 

#### Fraser sockeye ####
fraser <- read.csv("Data/FraserRunSize.csv")

png("Plots/3_Figure_timeseries/frasersockeye_returns.png",
    width = 850, height = 600,
    bg = NA) # transparent background

par(mar = c(2.4, 6, 0, 1), 
    mgp = c(0, 1.3, 0))

plot(
  x = fraser$Year,
  y = fraser$Run.Size,
  xlab = "", ylab = "",
  yaxt = "n", xaxt = "n",
  type = "l", lwd = 5, col = "navyblue", bty = "n")

# mtext("Total returns", cex = 7,
#       side = 2, line = 0.4)

axis(1, at = c(fraser$Year %>% min(), 
               fraser$Year %>% max()), 
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)
box(bty = "L", 
    lwd = 2.5)

dev.off()

#### South Thompson ####

south_thompson <- bc_salmon %>% 
  filter(system == "South-Thompson") %>% 
  select(year, returns) %>% drop_na(returns)

png("Plots/3_Figure_timeseries/chinooksthompson_returns.png",
    width = 850, height = 600, bg = NA)

par(mar = c(2.4, 6, 0, 1), mgp = c(0, 1.3, 0))

plot(
  x = south_thompson$year,
  y = south_thompson$returns,
  xlab = "", ylab = "",
  yaxt = "n", xaxt = "n",
  type = "l", lwd = 5, col = "navyblue", bty = "n")

# mtext("Total returns", cex = 7,
#       side = 2, line = 0.4)

axis(1, at = c(south_thompson$year %>% min(), 
               south_thompson$year %>% max()), 
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)
box(bty = "L", lwd = 2.5)

dev.off()


#### North Thompson ####

north_thompson <- bc_salmon %>% 
  filter(system == "North-Thompson") 

png("Plots/3_Figure_timeseries/cohonthompson_returns.png", 
    width = 850, height = 600, bg = NA)

par(mar = c(2.4, 6, 0, 1), mgp = c(0, 1.3, 0))

plot(
  x = north_thompson$year,
  y = north_thompson$returns,
  xlab = "", ylab = "",
  yaxt = "n", xaxt = "n",
  type = "l", lwd = 5, col = "navyblue", bty = "n")
 
# mtext("Total returns", cex = 7,
#       side = 2, line = 0.4)

axis(1, at = c(north_thompson$year %>% min(), 
               north_thompson$year %>% max()), 
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)
box(bty = "L", lwd = 2.5)

dev.off()

#### Fraser pink body size ####

pink_bodysize <- read.csv("Data/pink_bodysize.csv") %>% 
  select(1, 2)
colnames(pink_bodysize)[2] <- "weight"

png("Plots/3_Figure_timeseries/pinkfraser_bodysize.png", 
    width = 850, height = 600, bg = NA)

par(mar = c(2.4, 6, 0, 1), mgp = c(0, 1.3, 0))

plot(
  x = pink_bodysize$Year,
  y = pink_bodysize$weight,
  xlab = NA,
  ylab = "",
  yaxt = "n",
  xaxt = "n",
  type = "l", lwd = 5, lty = 1,
  col = "darkorange3", bty = "n")

# mtext("Weight", cex = 7,
#       side = 2, line = 1.3)

axis(1, at = c(pink_bodysize$Year %>% min(),
               pink_bodysize$Year %>% max()),
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)
box(bty = "L", lwd = 2.5)

dev.off()

#### Viner Sound chum ####

chum <- read.csv("Data/Chum_stock_recruit_unfiltered_2018.csv")
# chum_rivers <- c("KINGCOME RIVER", "KAKWEIKEN RIVER", "AHNUHATI RIVER", "VINER SOUND CREEK", "AHTA RIVER")
chum <- chum %>% 
  filter(river == "VINER SOUND CREEK") %>% 
  drop_na(returns) %>% 
  select(river, year, returns) %>% 
  group_by(year, river) %>% 
  summarize(returns = sum(returns)) 

png("Plots/3_Figure_timeseries/chumviner_returns.png", 
    width = 850, height = 600, bg = NA)

par(mar = c(2.4, 6, 0, 1), mgp = c(0, 1.3, 0))

plot(
  x = viner_chum$year,
  y = viner_chum$returns,
  xlab = NA, 
  ylab = "",
  yaxt = "n",
  xaxt = "n",
  type = "l", lwd = 5,
  col = "navyblue", bty = "n")

# mtext("Total returns", cex = 7,
#       side = 2, line = 0.4)

axis(1, at = c(viner_chum$year %>% min(), 
               viner_chum$year %>% max()), 
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)
box(bty = "L", lwd = 2.5)

dev.off()

#### Broughton odd and even year pinks ####

pink <- read.csv("Data/Pink_stock_recruit_unfiltered_2018.csv")
pink_rivers <- c("WAKEMAN RIVER", "AHNUHATI RIVER", "KAKWEIKEN RIVER", "KINGCOME RIVER", "LULL CREEK", "GLENDALE CREEK", "AHTA RIVER")
pink <- pink %>% 
  filter(River %in% pink_rivers) %>% 
  group_by(Yr, EO) %>% 
  summarize(Returns = sum(Returns, na.rm = TRUE)) # total returns is sum of returns to `pink rivers`

even_pink <- pink %>% 
  filter(EO == "E") %>% 
  mutate(Yr = sort(Yr))
odd_pink <- pink %>% 
  filter(EO == "O") %>% 
  mutate(Yr = sort(Yr))

png("Plots/3_Figure_timeseries/pinkbroughton_returns.png", 
    width = 850, height = 600, bg = NA)

par(mar = c(2.4, 6, 0, 1), mgp = c(0, 1.3, 0))

plot(
  x = even_pink$Yr, # even pinks
  y = even_pink$Returns,
  xlab = "", ylab = "",
  yaxt = "n", xaxt = "n",
  type = "l", lwd = 5, col = "navyblue", bty = "n")

lines(x = odd_pink$Yr, # odd pinks
      y = odd_pink$Returns,
      type = "l", lty = 2, lwd = 4, col = "navyblue")

# mtext("Total returns", cex = 7,
#       side = 2, line = 0.4)

axis(1, at = c(even_pink$Yr %>% min(), 
               even_pink$Yr %>% max()), 
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)

legend("topleft", legend = c("Even", "Odd"), col = "navyblue",
       lty = c(1, 3), cex = 3.5, lwd = 4, bty = "n")

box(bty = "L", lwd = 2.5)

dev.off()

#### Broughton even year pinks ####

png("Plots/3_Figure_timeseries/evenpinkbroughton_returns.png", 
    width = 850, height = 600, bg = NA)

par(mar = c(2.4, 6, 0, 1), mgp = c(0, 1.3, 0))

plot(
  x = even_pink$Yr,
  y = even_pink$Returns,
  xlab = "", ylab = "",
  yaxt = "n", xaxt = "n",
  type = "l", lwd = 5, col = "navyblue", bty = "n")

# mtext("Total returns", cex = 7,
#       side = 2, line = 0.4)

axis(1, at = c(even_pink$Yr %>% min(), 
               even_pink$Yr %>% max()), 
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)

text(x = 1950 + 9, 
     y = even_pink$Returns %>% max(),
     "Even years", cex = 3.5)
     
box(bty = "L", lwd = 2.5)

dev.off()

#### Broughton odd year pinks ####

png("Plots/3_Figure_timeseries/oddpinkbroughton_returns.png", 
    width = 850, height = 600, bg = NA)

par(mar = c(2.4, 6, 0, 1), mgp = c(0, 1.3, 0))

plot(
  x = odd_pink$Yr,
  y = odd_pink$Returns,
  xlab = "", ylab = "",
  yaxt = "n", xaxt = "n",
  type = "l", lwd = 5, col = "navyblue", bty = "n")

# mtext("Total returns", cex = 7,
#       side = 2, line = 0.4)

axis(1, at = c(odd_pink$Yr %>% min(), 
               odd_pink$Yr %>% max()), 
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)

text(x = 1950 + 9, 
     y = odd_pink$Returns %>% max(),
     "Odd years", cex = 3.5)

box(bty = "L", lwd = 2.5)

dev.off()

#### Owikeno Lake sockeye ####

owikeno <- bc_salmon %>% 
  filter(system == "Owikeno")

png("Plots/3_Figure_timeseries/sockeyeowikeno_returns.png", 
    width = 850, height = 600, bg = NA)

par(mar = c(2.4, 6, 0, 1), mgp = c(0, 1.3, 0))

plot(
  x = owikeno$year,
  y = owikeno$returns,
  xlab = "", ylab = "",
  yaxt = "n", xaxt = "n",
  type = "l", lwd = 5, col = "navyblue", bty = "n")

# mtext("Total returns", cex = 7,
#       side = 2, line = 0.4)

axis(1, at = c(owikeno$year %>% min(), 
               owikeno$year %>% max()), 
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)
box(bty = "L", lwd = 2.5)

dev.off()

#### Skeena sockeye ####

Skeena <- bc_salmon %>% 
  filter(system == "Skeena/Nass")

png("Plots/3_Figure_timeseries/sockeyeskeena_returns.png", 
    width = 850, height = 600, bg = NA)

par(mar = c(2.4, 6, 0, 1), mgp = c(0, 1.3, 0))

plot(
  x = Skeena$year,
  y = Skeena$returns,
  xlab = "", ylab = "",
  yaxt = "n", xaxt = "n",
  type = "l", lwd = 5, col = "navyblue", bty = "n")

# mtext("Total returns", cex = 7,
#       side = 2, line = 0.4)

axis(1, at = c(Skeena$year %>% min(), 
               Skeena$year %>% max()), 
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)
box(bty = "L", lwd = 2.5)

dev.off()


#### Strait of Georgia chinook ####

straitgeorgia <- read.csv("Data/five_yr_mean_age_model_preds.csv") %>% 
  filter(group == "sog_oceantype")

png("Plots/3_Figure_timeseries/chinookSOG_age.png", 
    width = 850, height = 600, bg = NA)

par(mar = c(2.4, 6, 0, 1), mgp = c(0, 1.3, 0))

plot(
  x = straitgeorgia$year,
  y = straitgeorgia$mean_age,
  xlab = "", ylab = "",
  yaxt = "n", xaxt = "n",
  type = "l", lwd = 5, lty = 1, col = "darkgreen", bty = "n")

# mtext("Age at maturity", cex = 7,
#       side = 2, line = 1.2)

axis(1, at = c(straitgeorgia$year %>% min(), 
               straitgeorgia$year %>% max()), 
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)
box(bty = "L", lwd = 2.5)

dev.off()

#### West Coast Van Island survival ####

wcvi_survival <- read.csv("Data/Welch_2020_Review_of_Coastwide_Decline_SAR_Data.csv") %>%
  filter(Region == "WCVI") %>%
  arrange(SmoltYear)

png("Plots/3_Figure_timeseries/wcvi_survival.png", 
    width = 850, height = 600, bg = NA)

par(mar = c(2.4, 6, 0, 1), mgp = c(0, 1.3, 0))

plot(
  x = wcvi_survival$SmoltYear,
  y = wcvi_survival$SAR,
  xlab = "", ylab = "",
  yaxt = "n", xaxt = "n",
  type = "l", lwd = 5, lty = 1, col = "6", bty = "n")

# mtext("Age at maturity", cex = 7,
#       side = 2, line = 1.3)

axis(1, at = c(wcvi_survival$SmoltYear %>% min(), 
               wcvi_survival$SmoltYear %>% max()), 
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)
box(bty = "L", lwd = 2.5)

dev.off()

#### Central coast chum ####

central_coast <- read_excel("Data/11_CentralCoastChum.xlsx")
# # area 6 : khutze, arnoup, green, nias, tyler, soda
# # area 7 : mussel, kainet, salmon_bay, quartcha, neekas, roscoe, clatse, kwakusdis, kunsoot, cooper_inlet

area6 <- central_coast %>% 
  filter(pfma == 6) %>%
  filter(total_run != "NA") %>% 
  select(population, year, total_run) %>% 
  transmute(population, year, total_run = as.numeric(total_run)) %>% 
  group_by(year) %>% 
  summarize(returns = sum(total_run))

png("Plots/3_Figure_timeseries/chumcentralcoast6_returns.png", 
    width = 850, height = 600, bg = NA)

par(mar = c(2.4, 6, 0, 1), mgp = c(0, 1.3, 0))

plot(
  x = area6$year,
  y = area6$returns,
  xlab = "", ylab = "",
  yaxt = "n",xaxt = "n",
  type = "l", lwd = 5, col = "navyblue", bty = "n")

# mtext("Total returns", cex = 7,
#       side = 2, line = 0.4)

axis(1, at = c(area6$year %>% min(), 
               area6$year %>% max()), 
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)
box(bty = "L", lwd = 2.5)

text(x = 1960 + 5, 
     y = area6$returns %>% max(),
     "Area 6", cex = 3.5)

dev.off()

area7 <- central_coast %>% 
  filter(pfma == 7) %>%
  filter(total_run != "NA") %>% 
  select(population, year, total_run) %>% 
  transmute(population, year, total_run = as.numeric(total_run)) %>% 
  group_by(year) %>% 
  summarize(returns = sum(total_run))

png("Plots/3_Figure_timeseries/chumcentralcoast7_returns.png", 
    width = 850, height = 600, bg = NA)

par(mar = c(2.4, 6, 0, 1), mgp = c(0, 1.3, 0))

plot(
  x = area7$year,
  y = area7$returns,
  xlab = "", ylab = "",
  yaxt = "n", xaxt = "n",
  type = "l", lwd = 5, col = "navyblue", bty = "n")

# mtext("Total returns", cex = 7,
#       side = 2, line = 0.4)

axis(1, at = c(area7$year %>% min(), 
               area7$year %>% max()), 
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)
box(bty = "L", lwd = 2.5)

text(x = 1960 + 5, 
     y = area7$returns %>% max(),
     "Area 7", cex = 3.5)

dev.off()

nass <- read_excel("Data/lowerNass_PSF_2023-11-10.xlsx")

png("Plots/3_Figure_timeseries/sockeyenass_returns.png", 
    width = 850, height = 600, bg = NA)

par(mar = c(2.4, 6, 0, 1), mgp = c(0, 1.3, 0))

plot(
  x = nass$year,
  y = nass$total_run,
  xlab = "", ylab = "",
  yaxt = "n", xaxt = "n",
  type = "l", lwd = 5, col = "navyblue", bty = "n")

# mtext("Total returns", cex = 7,
#       side = 2, line = 0.4)

axis(1, at = c(nass$year %>% min(), 
               nass$year %>% max()), 
     lwd = 2, cex.axis = 2.5)
axis(2, lwd = 2, tck = 0, labels = NA)
box(bty = "L", lwd = 2.5)

dev.off()




