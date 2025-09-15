# SCRIPT 4
# Measure PB escapes and suppression areas

library(dplyr)
library(sf)
library(units)


wfHistory <- "2021WGS84.shp" # Spatial fire history data for years to 2021.
wfHistoryDat <- sf::st_read(wfHistory)
wfHistoryDat <- wfHistoryDat[wfHistoryDat$fih_year1 >= 2000 & wfHistoryDat$fih_year1 <= 2021,]

# Find possible fires that were PB escapes
matches <- grepl("escape", wfHistoryDat$fih_commen, ignore.case = TRUE)
check <- wfHistoryDat[matches,]
changeList <- c(7393)
wfHistoryDat$fih_cause[wfHistoryDat$OBJECTID == changeList] <- 2 # Change to PB escape

areasWF   <- st_area(wfHistoryDat[wfHistoryDat$fih_fire_t != "PB",])
totalWF   <- sum(areasWF)
totalWF_ha <- set_units(totalWF, ha)
areasPB   <- st_area(wfHistoryDat[wfHistoryDat$fih_fire_t == "PB",])
totalPB   <- sum(areasPB)
totalPB_ha <- set_units(totalPB, ha)
area_escapes <- st_area(wfHistoryDat[wfHistoryDat$fih_cause == 2,])
total_escapes <- sum(area_escapes)
total_escapes_ha <- set_units(total_escapes, ha)
areaNorthcliffe <- st_area(wfHistoryDat[wfHistoryDat$OBJECTID == 11413,])
totalNorthcliffe <- sum(areaNorthcliffe)
totalNorthcliffe_ha <- set_units(totalNorthcliffe, ha)

PB_prop <- totalPB_ha / (totalWF_ha + totalPB_ha)
PBescapes <- total_escapes_ha / totalWF_ha
Suppression_reduction <- totalNorthcliffe_ha / totalWF_ha

summTable <- data.frame(
  WF = round(totalWF_ha),
  PB = paste0(round(totalPB_ha), ", ", round(PB_prop*100), "%"),
  PB_escapes = paste0(round(total_escapes_ha), ", ", round(PBescapes*100), "%"),
  Easy_suppression = paste0(round(totalNorthcliffe_ha), ", ", round(Suppression_reduction*100), "%")
)

