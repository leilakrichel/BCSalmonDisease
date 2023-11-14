# Figure 3: Salmon time-series #

## Load packages ##
  
library(sf)
library(tidyverse)

## Load shapefiles ##

bc <- readRDS("Data/BC_lowRes.rds") # Provincial boundary
wb <- readRDS("Data/waterbodies_lowRes.rds") # Waterbodies
wc <- readRDS("Data/watercourse_lowRes.rds") # Water courses

farms <- st_read(dsn = "Data/shapefiles/bc_farms.shp")
coastWA <- st_read("Data/shapefiles/washington.shp")

## Load the salmon data ##

pink <- read.csv("Data/Pink_stock_recruit_unfiltered_2018.csv")
pink_rivers <- c("WAKEMAN RIVER", "AHNUHATI RIVER", "KAKWEIKEN RIVER", "KINGCOME RIVER", "LULL CREEK", "GLENDALE CREEK", "AHTA RIVER")
pink <- pink %>% 
  filter(River %in% pink_rivers) %>% 
  group_by(Yr, EO) %>% 
  summarize(Returns = sum(Returns, na.rm = TRUE))

chum <- read.csv("Data/Chum_stock_recruit_unfiltered_2018.csv")
chum_rivers <- c("KINGCOME RIVER", "KAKWEIKEN RIVER", "AHNUHATI RIVER", "VINER SOUND CREEK", "AHTA RIVER")
chum <- chum %>% 
  filter(river %in% chum_rivers) %>% 
  drop_na(returns) %>% 
  select(river, year, returns) %>% 
  group_by(year, river) %>% 
  summarize(returns = sum(returns)) 

pink_bodysize <- read.csv("Data/HistoricalPinkWeightDownload.csv") %>% 
  select(1, 2)

colnames(pink_bodysize)[2] <- "weight"

straitgeorgia <- read.csv("Data/five_yr_mean_age_model_preds.csv")

bc_salmon <- read.csv("Data/bc_spawners.csv")

central_coast <- read_excel("Data/11_CentralCoastChum.xlsx")



# farm_colour <- "coral3"
# 
#### Rotate map ####
 
rot <- function(a) {
  
  matrix(c(cos(a), sin(a), -sin(a), cos(a)), 
         nrow = 2, ncol = 2)
  }

bc <- st_geometry(bc)
centroid_bc <- st_centroid(bc) # calculate centroid 
bc_rot <- (bc - centroid_bc) * rot(pi/4) * 0.80 + centroid_bc # rotate around centroid

# Plot
par(mar = c(0, 0, 0, 0), bg = "red") # set margins
plot(NULL, xlim = c(-140, -110), ylim = c(45, 60))

u <- par("usr")
v <- c(grconvertX(u[1:2], "user", "ndc"),
       grconvertY(u[3:4], "user", "ndc"))

par(new = TRUE,
    fig = c(v[1] + 0.235, 
      v[2] - 0.265, 
      v[3], 
      v[4]),
    mar = c(0.15, 2, 0.15, 0.5),
    bg = "white") # set margins

plot(st_geometry(bc_rot),
     border = "black",
     lwd = 0.8, bg = "grey", col = "white",
     xlim = c(-130, -125), 
     ylim = c(52, 56),
     cex.axis = 0.8) # bc coastline

# plot(st_geometry(coastWA),
#      border = "black",
#      lwd = 0.8, bg = "grey",
#      col = "white", add = TRUE) # washington
# plot(st_geometry(wc), col = "black", add = TRUE) # rivers
# plot(st_geometry(farms), col = farm_colour, add = TRUE, pch = 19, cex = 0.8) # farms
box(lwd = 2)
# 
# plot(st_geometry(wc[which(wc$name_en == "Fraser River"), ]), col = 6, add = TRUE, lwd = 2)
# plot(st_geometry(wc[which(wc$name_en == "Skeena River"), ]), col = 6, add = TRUE, lwd = 2)
# plot(st_geometry(wc[which(wc$name_en == "Thompson River"), ]), col = 6, add = TRUE, lwd = 2)

# # Inset set-up

## Fraser sockeye

Fraser <- bc_salmon %>% filter(system == "Fraser")

par(fig = c(v[1] + 0.73, # x1
            v[2], # x2
            v[3] ,#+ 0.012, # y1
            v[4] - 0.75), # y2
    new = TRUE,
    mgp = c(1, 0, 0),
    mar = c(2, 2, 1.5, 0.4),
    cex.axis = 0.55
    # cex.lab = 1
    ) 

plot(
  x = Fraser$year,
  y = Fraser$returns,
  xlab = list("Year"), 
  ylab = NA,#list("Total returns"),
  ylim = c(0, max(Fraser$returns) + 5e6),
  xaxt = "no", yaxt = "no",
  type = "l", pch = 16, lwd = 1.5, tck = 0.02,
  col = "grey1")
title("Fraser river sockeye",
      line = -0.7, cex.main = 0.7,
      font.main = 4, col.main = "gray1")
mtext("Total returns", cex = 0.9, 
      side = 2, line = 0.8)
axis(1, at = seq(Fraser$year %>% min(),
                 Fraser$year %>% max(),
                 by = 10), tck = .02)
axis(2, at = seq(0,
                 Fraser$returns %>% max(),
                 length.out = 4), tck = .02)

box()

# South Thompson chinook

south_thompson <- bc_salmon %>% filter(system == "South-Thompson") %>% 
  select(year, returns) %>% drop_na(returns)

par(fig = c(v[1] + 0.73, # x1
            v[2], # x2
            v[3] + 0.2, # y1
            v[4] - 0.55), # y2
    new = TRUE,
    mgp = c(1, 0, 0),
    mar = c(2, 2, 1.5, 0.4), 
    cex.axis = 0.52,
    cex.lab = 1) 

plot(
  x = south_thompson$year,
  y = south_thompson$returns,
  xlab = "", ylab = "",
  ylim = c(0, max(south_thompson$returns) + 4e4),
  xaxt = "no", yaxt = "no",
  type = "l", pch = 16, lwd = 1.5, tck = 0.02)
title("South Thompson creek chinook",
      line = -0.7, cex.main = 0.7,
      font.main = 4, col.main = "gray1")
mtext("Total returns", cex = 0.9, 
      side = 2, line = 0.8)
axis(1, at = seq(south_thompson$year %>% min() + 1,
                 south_thompson$year %>% max() + 1,
                 by = 10), tck = .02)
axis(2, at = seq(0,
                 south_thompson$returns %>% max(),
                 length.out = 5), tck = .02)

box()

# North Thompson coho

north_thompson <- bc_salmon %>% filter(system == "North-Thompson")

par(fig = c(v[1] + 0.73, # x1
            v[2], # x2
            v[3] + 0.4, # y1
            v[4] - 0.35), # y2
    new = TRUE,
    mgp = c(1, 0, 0),
    mar = c(2, 2, 1.5, 0.4), 
    cex.axis = 0.52,
    cex.lab = 1) 
plot(
  x = north_thompson$year,
  y = north_thompson$returns,
  ylim = c(0, max(north_thompson$returns) + 3e4),
  xaxt = "no", yaxt = "no",
  xlab = "", ylab = "",
  type = "l", pch = 16, lwd = 1.5, tck = 0.02)
title("North Thompson creek coho", 
      line = -0.7, cex.main = 0.7, 
      font.main = 4, col.main = "gray1")
mtext("Total returns", cex = 0.9, 
      side = 2, line = 0.8)
axis(1, at = seq(north_thompson$year %>% min() + 1,
                 north_thompson$year %>% max(),
                 by = 10), tck = .02)
axis(2, at = seq(0,
                 north_thompson$returns %>% max(),
                 length.out = 5), tck = .02)

box()

# Robertson creek chinook  

robertson <- bc_salmon %>% filter(system == "Robertson-creek") %>% 
  mutate(year = sort(year))

par(fig = c(v[1] + 0.73, # x1
            v[2], # x2
            v[3] + 0.6, # y1
            v[4] - 0.15), # y2
    new = TRUE,
    mgp = c(1, 0, 0),
    mar = c(2, 2, 1.5, 0.4), 
    cex.axis = 0.52,
    cex.lab = 1) 

plot(
  x = robertson$year,
  y = robertson$marine_survival,
  ylim = c(0, 0.1),
  xlab = "", ylab = "",
  xaxt = "no", yaxt = "no",
  type = "l", pch = 16, lwd = 1.7, tck = 0.02
  )
title("Robertson creek chinook", 
      line = -0.7, cex.main = 0.7, 
      font.main = 4, col.main = "gray1")
mtext("Marine survival", cex = 0.9, 
      side = 2, line = 0.8)
axis(1, at = seq(robertson$year %>% min() + 1,
                 robertson$year %>% max(),
                 by = 10), tck = .02)
axis(2, at = seq(0,
                 0.08,
                 length.out = 5), tck = .02)
box()

# Fraser river pink body size

par(fig = c(v[1] + 0.73, # x1
            v[2], # x2
            v[3] + 0.8, # y1
            v[4]), # y2
    new = TRUE,
    mgp = c(1, 0, 0),
    mar = c(2, 2, 0.2, 0.4), 
    cex.axis = 0.52,
    cex.lab = 1) 

plot(
  x = pink_bodysize$Year,
  y = pink_bodysize$weight,
  xlab = "", ylab = "",
  ylim = c(min(pink_bodysize$weight), 3.5),
  xaxt = "no", yaxt = "no",
  type = "l", pch = 16, lwd = 1.7, tck = 0.02)
title("Fraser river pink", 
      line = -0.7, cex.main = 0.7, 
      font.main = 4, col.main = "gray1")
mtext("Mean weight (kg)", cex = 0.85, 
      side = 2, line = 0.8)
axis(1, at = seq(pink_bodysize$Year %>% min() + 3,
                 pink_bodysize$Year %>% max() - 1,
                 by = 10), tck = .02)
axis(2, at = seq(1,
                 3.5,
                 length.out = 4) %>% round(1), tck = .02)

box()

# Broughton chum

viner_chum <- chum %>% 
  mutate(year = sort(year)) %>% 
  filter(river == "VINER SOUND CREEK")

par(fig = c(v[1], # x1
            v[2] - 0.73, # x2
            v[3], #+ 0.012, # y1
            v[4] - 0.75), # y2
    new = TRUE,
    mgp = c(1, 0, 0),
    mar = c(2, 2, 1.5, 0.4),
    cex.axis = 0.52,
    cex.lab = 1) 
plot(
  x = viner_chum$year,
  y = viner_chum$returns,
  xlab = "Year", ylab = "",
  ylim = c(0, max(viner_chum$returns) + 2e4),
  xaxt = "no", yaxt = "no",
  type = "l", pch = 16, lwd = 1.7, tck = 0.02)
title("Viner Sound creek chum", 
      line = -0.7, cex.main = 0.7, 
      font.main = 4, col.main = "gray1")
mtext("Total returns", cex = 0.9, 
      side = 2, line = 0.8 
      )
axis(1, at = seq(viner_chum$year %>% min() + 1,
                 viner_chum$year %>% max() - 3,
                 by = 10), tck = .02)
axis(2, at = seq(0,
                 viner_chum$returns %>% max(),
                 length.out = 5), tck = .02)


box()

# Broughton pink

even_pink <- pink %>% 
  filter(EO == "E") %>% 
  mutate(Yr = sort(Yr))
odd_pink <- pink %>% 
  filter(EO == "O") %>% 
  mutate(Yr = sort(Yr))

par(fig = c(v[1], # x1
            v[2] - 0.73, # x2
            v[3] + 0.2, # y1
            v[4] - 0.55), # y2
    new = TRUE,
    mgp = c(1, 0, 0),
    mar = c(2, 2, 1.5, 0.4), 
    cex.axis = 0.52,
    cex.lab = 1) 
plot(
  x = even_pink$Yr,
  y = even_pink$Returns,
  ylim = c(0, max(even_pink$Returns) + 1e6),
  xaxt = "no", yaxt = "no",
  xlab = "", ylab = "",
  type = "l", pch = 16, lwd = 1.7, col = 6, tck = 0.02)
lines(x = odd_pink$Yr,
      y = odd_pink$Returns,
      type = "l", pch = 16, lwd = 1.7)
title("Broughton pink", 
      line = -0.7, cex.main = 0.7, adj = 0.8,
      font.main = 4, col.main = "gray1")
mtext("Total returns", cex = 0.9, 
      side = 2, line = 0.8)
legend("topleft", legend = c("Even", "Odd"), col = c(6, "black"), 
       lty = c(1, 1), cex = 0.6, lwd = 1.5)
axis(1, at = seq(even_pink$Yr %>% min(),
                 even_pink$Yr %>% max() - 1,
                 by = 10), tck = .02)
axis(2, at = seq(0,
                 max(even_pink$Returns) + 1e6,
                 length.out = 5), tck = .02)

box()


# Owikeno sockeye  

owikeno <- bc_salmon %>% filter(system == "Owikeno")

par(fig = c(v[1], # x1
            v[2] - 0.73, # x2
            v[3] + 0.4, # y1
            v[4] - 0.35), # y2
    new = TRUE,
    mgp = c(1, 0, 0),
    mar = c(2, 2, 1.5, 0.4), 
    cex.axis = 0.52,
    cex.lab = 1) 

plot(
  x = owikeno$year,
  y = owikeno$returns,
  xlab = "", ylab = "",
  xaxt = "no", yaxt = "no",
  ylim = c(0, max(owikeno$returns) + 1e6),
  type = "l", pch = 16, lwd = 1.7, tck = 0.02)
title("Owikeno sockeye", 
      line = -0.7, cex.main = 0.7, 
      font.main = 4, col.main = "gray1")
mtext("Total returns", cex = 0.9, 
      side = 2, line = 0.8)
axis(1, at = seq(owikeno$year %>% min() + 2,
                 owikeno$year %>% max() + 2,
                 by = 10), tck = .02)
axis(2, at = seq(0,
                 max(owikeno$returns) + 1e6,
                 length.out = 5), tck = .02)

box()

# Skeena sockeye  

Skeena <- bc_salmon %>% filter(system == "Skeena/Nass")

par(fig = c(v[1], # x1
            v[2] - 0.73, # x2
            v[3] + 0.6, # y1
            v[4] - 0.15), # y2
    new = TRUE,
    mgp = c(1, 0, 0),
    mar = c(2, 2, 1.5, 0.4), 
    cex.axis = 0.52,
    cex.lab = 1) 

plot(
  x = Skeena$year,
  y = Skeena$returns,
  xlab = "", ylab = "",
  ylim = c(0, max(Skeena$returns) + 1.3e6),
  xaxt = "no", yaxt = "no",
  type = "l", pch = 16, lwd = 1.7, tck = 0.02)
title("Skeena river sockeye", 
      line = -0.7, cex.main = 0.7, 
      font.main = 4, col.main = "gray1")
mtext("Total returns", cex = 0.9, 
      side = 2, line = 0.8)
axis(1, at = seq(Skeena$year %>% min() - 2,
                 Skeena$year %>% max() - 1,
                 by = 10), tck = .02)
axis(2, at = seq(0,
                 max(Skeena$returns) + 1e6,
                 length.out = 5), tck = .02)

box()

# strait of georgia

straitgeorgia <- straitgeorgia %>% filter(group == "sog_oceantype")

par(fig = c(v[1], # x1
            v[2] - 0.73, # x2
            v[3] + 0.8, # y1
            v[4]), # y2
    new = TRUE,
    mgp = c(1, 0, 0),
    mar = c(2, 2, 0.2, 0.4), 
    cex.axis = 0.52) 

plot(
  x = straitgeorgia$year,
  y = straitgeorgia$mean_age,
  xlab = "", ylab = "",
  ylim = c(3.5, 4),
  xaxt = "no", yaxt = "no",
  
  type = "l", pch = 16, lwd = 1.7, tck = 0.02)
title("Strait of Georgia chinook", 
      line = -0.7, cex.main = 0.7, 
      font.main = 4, col.main = "gray1")
mtext("Mean age", cex = 0.9, 
      side = 2, line = 0.8)

axis(1, at = seq(straitgeorgia$year %>% min() + 1,
                 straitgeorgia$year %>% max() - 1,
                 by = 10), tck = .02)
axis(2, at = seq(3.4,
                 4,
                 length.out = 4) %>% round(1), tck = .02)


box()

