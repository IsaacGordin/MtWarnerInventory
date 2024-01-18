## Isaac Gordin
## NRC 534 - Forest measurements
## Final Project: Forest Inventory Report
## Due 12/12 @ 10:10 am

# Load required R packages
library(plyr)
library(plotrix)
library("writexl")

# Load data
os_dat <- read.table("overstory_dat.csv", header = TRUE, sep = ",")
regen_dat <- read.table("regen_dat.csv", header = TRUE, sep = ",")
dwm_dat <- read.table("dwm_dat.csv", header = TRUE, sep = ",")

#The Basal Area Factor used is 20
baf <- 20

#Define the forester's constant
fc <- pi/(4*144)

# Define function to find t-value - COPIED FROM CLASS NOTES
find_t <- function(conf, n_plots){
  qt(1 - ((1-(conf/100))/2), n_plots - 1)
}

#Add Basal Area to os_dat
os_dat$ba <- ((os_dat$Dbh)^2) * fc

#Add expansion factor to os_dat
os_dat$ef <- baf/os_dat$ba

ph <- os_dat[os_dat$Stand == "pine-hardwoods",]
hh <- os_dat[os_dat$Stand == "hemlock-hardwoods",]

#Number of plots for respective stands, pine hardwood and hemlock hardwood
phPlots <- 14 
hhPlots <- 46 

#Using our defined function we can determine these tStar 95% confidence interval values
ph_tStar <- find_t(95, phPlots)
hh_tStar <- find_t(95, hhPlots)

#NUMBER 1

#Define BAA, TPA, VPA values for the overstory data
osDatTable <- ddply(os_dat, .(Stand, PlotID), summarize, tpa = sum(ef), 
                    baa = sum(ef*ba), vpa = (sum(ef*Vol, na.rm = TRUE)))

#Averaging BAA, VPA, TPA values and generating 95% confidence interval values
num1 <- ddply(osDatTable, .(Stand), summarize, meanBAA = mean(baa), meanTPA = mean(tpa), meanVPA = mean(vpa), 
              cilBAA = mean(baa) + -1*find_t(95, length(Stand))*std.error(baa), 
              ciuBAA = mean(baa) + 1*find_t(95, length(Stand))*std.error(baa),
              cilVPA = mean(vpa) + -1*find_t(95, length(Stand))*std.error(vpa), 
              ciuVPA = mean(vpa) + 1*find_t(95, length(Stand))*std.error(vpa),
              cilTPA = mean(tpa) + -1*find_t(95, length(Stand))*std.error(tpa), 
              ciuTPA = mean(tpa) + 1*find_t(95, length(Stand))*std.error(tpa))
#Numbers 3 & 4
osDatTableSpHH <- ddply(hh, .(PlotID, Species), summarize, baa = sum(ef*ba), 
                      vpa = (sum(ef*Vol, na.rm = TRUE)), .drop = FALSE)

osDatTableSpHHTest <- ddply(hh, .(Species), summarize, baa = sum(ef*ba), 
                        vpa = (sum(ef*Vol, na.rm = TRUE)), .drop = FALSE)


osDatTableSpPH <- ddply(ph, .(PlotID, Species), summarize, baa = sum(ef*ba), 
                        vpa = (sum(ef*Vol, na.rm = TRUE)), .drop = FALSE)

osDatTableSpPHTest <- ddply(ph, .(Species), summarize, baa = sum(ef*ba), 
                        vpa = (sum(ef*Vol, na.rm = TRUE)), .drop = FALSE)
#Averaging BAA, VPA, values and generating 95% confidence interval values
num34HH <- ddply(osDatTableSpHH, .(Species), summarize, 
                 meanBAA = round(mean(baa),2), meanVPA = round(mean(vpa),2), 
      cilBAA = mean(baa) + -1*find_t(95, hhPlots)*std.error(baa), 
      ciuBAA = mean(baa) + 1*find_t(95, hhPlots)*std.error(baa),
      cilVPA = mean(vpa) + -1*find_t(95, hhPlots)*std.error(vpa), 
      ciuVPA = mean(vpa) + 1*find_t(95, hhPlots)*std.error(vpa))

#Averaging BAA, VPA, values and generating 95% confidence interval values
num34PH <- ddply(osDatTableSpPH, .(Species), summarize, 
                 meanBAA = round(mean(baa),2), meanVPA = round(mean(vpa),2), 
      cilBAA = mean(baa) + -1*find_t(95, phPlots)*std.error(baa), 
      ciuBAA = mean(baa) + 1*find_t(95, phPlots)*std.error(baa),
      cilVPA = mean(vpa) + -1*find_t(95, phPlots)*std.error(vpa), 
      ciuVPA = mean(vpa) + 1*find_t(95, phPlots)*std.error(vpa))

#NUMBER 5 total volume and 95 percent confidence intervals by product class for each stand
osDatTablVol <- ddply(os_dat, .(Stand, Product), summarize, vol = Vol, .drop = FALSE)

#Summing volumes and calculating confidence intervals - do i need to organize based on plot
num5 <- ddply(osDatTablVol, .(Stand, Product), summarize, totVol = sum(vol, na.rm = TRUE), 
      cilVol = sum(vol, na.rm = TRUE) + -1*find_t(95, length(Stand))*std.error(vol, na.rm = TRUE),
      ciuVol = sum(vol, na.rm = TRUE) + 1*find_t(95, length(Stand))*std.error(vol, na.rm = TRUE))

#NUMBER 6, diameter distribution figures per stand
phDiamTable <- ddply(ph, .(Dbh), summarize, tpa = sum(ef))
barplot(phDiamTable$tpa, space = 0, names.arg = phDiamTable$Dbh, 
        main = "Diameter Distribution for \n Pine-Hardwoods Stand", 
        xlab = "Diameter class (in)", ylab = "Trees per acre",axis.lty = 1)

hhDiamTable <- ddply(hh, .(Dbh), summarize, tpa = sum(ef))
barplot(hhDiamTable$tpa, space = 0, names.arg = hhDiamTable$Dbh, 
        main = "Diameter Distribution for \n Hemlock-Hardwoods Stand", 
        xlab = "Diameter class (in)", ylab = "Trees per acre",axis.lty = 1)

#NUMBER 7 seedlings and saplings per acre and 95% CI's
phRegen <- regen_dat[regen_dat$Stand == "pine-hardwoods",]
hhRegen <- regen_dat[regen_dat$Stand == "hemlock-hardwoods",]
regEF = 1000 #plot = 1/1000 acre
regenDatTabPH <- ddply(phRegen, .(Subplot, Species), summarize, 
                     SPA = sum(n_seed * regEF), 
                     SaPA = sum(n_sap * regEF), .drop = FALSE)
regenDatTabHH <- ddply(hhRegen, .(Subplot, Species), summarize, 
                       SPA = sum(n_seed * regEF), 
                       SaPA = sum(n_sap * regEF), .drop = FALSE)

#Averaging determing seedlings and saplings per acre with 95% CI intervals
rgPH <- ddply(regenDatTabPH, .(Species), summarize, spa = mean(SPA, na.rm = TRUE), sapa = mean(SaPA, na.rm = TRUE),
      cilSeed = mean(SPA) + -1*find_t(95, length(Subplot))*std.error(SPA), 
      ciuSeed = mean(SPA) + 1*find_t(95, length(Subplot))*std.error(SPA),
      cilSap = mean(SaPA) + -1*find_t(95, length(Subplot))*std.error(SaPA), 
      ciuSap = mean(SaPA) + 1*find_t(95, length(Subplot))*std.error(SaPA))

rgHH <- ddply(regenDatTabHH, .(Species), summarize, spa = mean(SPA, na.rm = TRUE), sapa = mean(SaPA, na.rm = TRUE),
      cilSeed = mean(SPA) + -1*find_t(95, length(Subplot))*std.error(SPA), 
      ciuSeed = mean(SPA) + 1*find_t(95, length(Subplot))*std.error(SPA),
      cilSap = mean(SaPA) + -1*find_t(95, length(Subplot))*std.error(SaPA), 
      ciuSap = mean(SaPA) + 1*find_t(95, length(Subplot))*std.error(SaPA))

#NUMBER 8 DWM per acre by decay class
# Estimate average DWM diameter
dwm_dat$d_bar <- with(dwm_dat, (1/2)*(dl + du))

# Estimate DWM volume (in cubic feet)
dwm_dat$vol <- with(dwm_dat, ((d_bar/2)^2)*pi*length)

# Calculate DWM ratio
dwm_dat$ratio <- with(dwm_dat, vol/(dl + du + 2*length)) #Do i need this line

# Sum ratio by transect and decay class
dwm_trans_dat <- ddply(dwm_dat, .(Stand, TransID, decay_class), summarize, dwm_tot = sum(ratio), .drop = FALSE)

# Replace NA values with zero (no DWM present along transect)
dwm_trans_dat$dwm_tot[is.na(dwm_trans_dat$dwm_tot)] <- 0

# Area of interest
A <- 43560

# Transect length
L <- 325 

# Put volume of DWM on per-acre scale
dwm_trans_dat$VPA <- ((pi*A)/L)*dwm_trans_dat$dwm_tot

# Estimate mean DWM per acre by decay class
DWMTab <- ddply(dwm_trans_dat, .(Stand, decay_class), summarize, vpa = round(mean(VPA),1))

#9 Total amount of DWM within each stand
ddply(dwm_dat, .(Stand), summarize, totVol = sum(vol))
#10 mean TPA and BAA and corresponding 95 percent confidence intervals of snags by stand.
osSnagTable <- ddply(os_dat, .(Stand, PlotID, Product), summarize, tpa = sum(ef), 
                     baa = sum(ef*ba))

snagTabPH <- ddply(ph, .(PlotID, Product), summarize, tpa = sum(ef), 
                   baa = sum(ef*ba), .drop = FALSE)
snagTabHH <- ddply(hh, .(PlotID, Product), summarize, tpa = sum(ef), 
                   baa = sum(ef*ba), .drop = FALSE)

#average baa and tpa with 95 confidence interval
ddply(osSnagTable, .(Stand, Product), summarize, meanBAA = mean(baa), meanTPA = mean(tpa), 
      cilBAA = mean(baa) + -1*find_t(95, length(Stand))*std.error(baa), 
      ciuBAA = mean(baa) + 1*find_t(95, length(Stand))*std.error(baa),
      cilTPA = mean(tpa) + -1*find_t(95, length(Stand))*std.error(tpa), 
      ciuTPA = mean(tpa) + 1*find_t(95, length(Stand))*std.error(tpa))

ddply(snagTabPH, .(Product), summarize, meanBAA = mean(baa), meanTPA = mean(tpa), 
      cilBAA = mean(baa) + -1*find_t(95, phPlots)*std.error(baa), 
      ciuBAA = mean(baa) + 1*find_t(95, phPlots)*std.error(baa),
      cilTPA = mean(tpa) + -1*find_t(95, phPlots)*std.error(tpa), 
      ciuTPA = mean(tpa) + 1*find_t(95, phPlots)*std.error(tpa))

ddply(snagTabHH, .(Product), summarize, meanBAA = mean(baa), meanTPA = mean(tpa), 
      cilBAA = mean(baa) + -1*find_t(95, hhPlots)*std.error(baa), 
      ciuBAA = mean(baa) + 1*find_t(95, hhPlots)*std.error(baa),
      cilTPA = mean(tpa) + -1*find_t(95, hhPlots)*std.error(tpa), 
      ciuTPA = mean(tpa) + 1*find_t(95, hhPlots)*std.error(tpa))
