# Figure 4: BC Coastline and farm tenure locations #

## Load the data ####

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

bc_salmon <- read.csv("Data/bc-salmon-spawners-harvest-er 2.csv")

# Plot ####

par(mfrow = c(4, 3),
    mar = c(1.2, 1.2, 1.2, 1.2))

Fraser <- bc_salmon %>% filter(system == "Fraser")

plot(
  x = Fraser$year,
  y = Fraser$returns,
  xlab = list("Year"), 
  ylab = list("Total returns"),
  ylim = c(0, max(Fraser$returns) + 5e6),
  xaxt = "no", yaxt = "no",
  type = "l", pch = 16, lwd = 1.5, tck = 0.02,
  col = "grey1")
title("Fraser river sockeye",
      line = -1.8, cex.main = 1,
      font.main = 4, col.main = "gray1")
axis(1, at = seq(Fraser$year %>% min(),
                 Fraser$year %>% max(),
                 by = 10))
axis(2, at = seq(0,
                 Fraser$returns %>% max(),
                 length.out = 4))

box()

# South Thompson chinook

south_thompson <- bc_salmon %>% filter(system == "South-Thompson") %>% 
  select(year, returns) %>% drop_na(returns)

plot(
  x = south_thompson$year,
  y = south_thompson$returns,
  xlab = "", ylab = "Total returns",
  ylim = c(0, max(south_thompson$returns) + 4e4),
  xaxt = "no", yaxt = "no",
  type = "l", pch = 16, lwd = 1.5, tck = 0.02)
title("South Thompson creek chinook",
      line = -0.7, cex.main = 0.7,
      font.main = 4, col.main = "gray1")
axis(1, at = seq(south_thompson$year %>% min() + 1,
                 south_thompson$year %>% max() + 1,
                 by = 10), tck = .02)
axis(2, at = seq(0,
                 south_thompson$returns %>% max(),
                 length.out = 5), tck = .02)

box()

# North Thompson coho

north_thompson <- bc_salmon %>% filter(system == "North-Thompson")

plot(
  x = north_thompson$year,
  y = north_thompson$returns,
  ylim = c(0, max(north_thompson$returns) + 3e4),
  xaxt = "no", yaxt = "no",
  xlab = "", ylab = "Total returns",
  type = "l", pch = 16, lwd = 1.5, tck = 0.02)
title("North Thompson creek coho", 
      line = -0.7, cex.main = 0.7, 
      font.main = 4, col.main = "gray1")
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


plot(
  x = robertson$year,
  y = robertson$marine_survival,
  ylim = c(0, 0.1),
  xlab = "", ylab = "Marine survival",
  xaxt = "no", yaxt = "no",
  type = "l", pch = 16, lwd = 1.7, tck = 0.02
)
title("Robertson creek chinook", 
      line = -0.7, cex.main = 0.7, 
      font.main = 4, col.main = "gray1")
axis(1, at = seq(robertson$year %>% min() + 1,
                 robertson$year %>% max(),
                 by = 10), tck = .02)
axis(2, at = seq(0,
                 0.08,
                 length.out = 5), tck = .02)
box()

# Fraser river pink body size


plot(
  x = pink_bodysize$Year,
  y = pink_bodysize$weight,
  xlab = "", ylab = "Weight (kg)",
  ylim = c(min(pink_bodysize$weight), 3.5),
  xaxt = "no", yaxt = "no",
  type = "l", pch = 16, lwd = 1.7, tck = 0.02)
title("Fraser river pink", 
      line = -0.7, cex.main = 0.7, 
      font.main = 4, col.main = "gray1")
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


plot(
  x = viner_chum$year,
  y = viner_chum$returns,
  xlab = "Year", ylab = "Total returns",
  ylim = c(0, max(viner_chum$returns) + 2e4),
  xaxt = "no", yaxt = "no",
  type = "l", pch = 16, lwd = 1.7, tck = 0.02)
title("Viner Sound creek chum", 
      line = -0.7, cex.main = 0.7, 
      font.main = 4, col.main = "gray1")
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


plot(
  x = even_pink$Yr,
  y = even_pink$Returns,
  ylim = c(0, max(even_pink$Returns) + 1e6),
  xaxt = "no", yaxt = "no",
  xlab = "", ylab = "Total returns",
  type = "l", pch = 16, lwd = 1.7, col = 6, tck = 0.02)
lines(x = odd_pink$Yr,
      y = odd_pink$Returns,
      type = "l", pch = 16, lwd = 1.7)
title("Broughton pink", 
      line = -0.7, cex.main = 0.7, adj = 0.8,
      font.main = 4, col.main = "gray1")
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

plot(
  x = owikeno$year,
  y = owikeno$returns,
  xlab = "", ylab = "Total returns",
  xaxt = "no", yaxt = "no",
  ylim = c(0, max(owikeno$returns) + 1e6),
  type = "l", pch = 16, lwd = 1.7, tck = 0.02)
title("Owikeno sockeye", 
      line = -0.7, cex.main = 0.7, 
      font.main = 4, col.main = "gray1")
axis(1, at = seq(owikeno$year %>% min() + 2,
                 owikeno$year %>% max() + 2,
                 by = 10), tck = .02)
axis(2, at = seq(0,
                 max(owikeno$returns) + 1e6,
                 length.out = 5), tck = .02)

box()

# Skeena sockeye  

Skeena <- bc_salmon %>% filter(system == "Skeena/Nass")

plot(
  x = Skeena$year,
  y = Skeena$returns,
  xlab = "", ylab = "Total returns",
  ylim = c(0, max(Skeena$returns) + 1.3e6),
  xaxt = "no", yaxt = "no",
  type = "l", pch = 16, lwd = 1.7, tck = 0.02)
title("Skeena river sockeye", 
      line = -0.7, cex.main = 0.7, 
      font.main = 4, col.main = "gray1")
axis(1, at = seq(Skeena$year %>% min() - 2,
                 Skeena$year %>% max() - 1,
                 by = 10), tck = .02)
axis(2, at = seq(0,
                 max(Skeena$returns) + 1e6,
                 length.out = 5), tck = .02)

box()

# strait of georgia

straitgeorgia <- straitgeorgia %>% filter(group == "sog_oceantype")

plot(
  x = straitgeorgia$year,
  y = straitgeorgia$mean_age,
  xlab = "", ylab = "Total returns",
  ylim = c(3.5, 4),
  xaxt = "no", yaxt = "no",
  
  type = "l", pch = 16, lwd = 1.7, tck = 0.02)
title("Strait of Georgia chinook", 
      line = -0.7, cex.main = 0.7, 
      font.main = 4, col.main = "gray1")

axis(1, at = seq(straitgeorgia$year %>% min() + 1,
                 straitgeorgia$year %>% max() - 1,
                 by = 10), tck = .02)
axis(2, at = seq(3.4,
                 4,
                 length.out = 4) %>% round(1), tck = .02)


box()
