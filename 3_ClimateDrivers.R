# SCRIPT 3
# BUILD CLIMATE - FIRE MODEL

library(dplyr)
library(sf)
library(corrplot)
library(ggplot2)
library(patchwork)
source("ProjectionFunctions.R")


# 1. Import data
SSP_short <- SSPs[SSPs$Year>2021,] # Trim to future years
pipTab <- read.csv("PIP.csv") # PIP calculation data
wfHistory <- "2021WGS84.shp" # Spatial fire history data for years to 2021. Record 7393 corrected to fih_cause 2
Climate <- read.csv("climTR.csv") %>%
  mutate(RainDiff = Rwint - lag(Rwint))# Climate data for 1957-2021, with new rainfall difference column
 

# 2. Get climate-fire relationship
climCos <- darimiGanbi(wfHistory, Climate, Exclusion = F, minYear = 1957, maxYear = 2021) 

# 3. Build climate models

C1 <- lm(WF~HdS, data = climCos[[1]])
C1summ <- summary(C1)
f1 <- C1summ$fstatistic
p_1 <- pf(f1["value"], f1["numdf"], f1["dendf"], lower.tail = FALSE)

C1b <- lm(WF~SynVar, data = climCos[[1]])
C1bsumm <- summary(C1b)
f1b <- C1bsumm$fstatistic
p_1b <- pf(f1b["value"], f1b["numdf"], f1b["dendf"], lower.tail = FALSE)

C2 <- lm(WF~HdS+HdA, data = climCos[[1]])
C2summ <- summary(C2)
f2 <- C2summ$fstatistic
p_2 <- pf(f2["value"], f2["numdf"], f2["dendf"], lower.tail = FALSE)

C2b <- lm(WF~HdS+LiS, data = climCos[[1]])
C2bsumm <- summary(C2b)
f2b <- C2bsumm$fstatistic
p_2b <- pf(f2b["value"], f2b["numdf"], f2b["dendf"], lower.tail = FALSE)

C2c <- lm(WF~HdS+Rwint, data = climCos[[1]])
C2csumm <- summary(C2c)
f2c <- C2csumm$fstatistic
p_2c <- pf(f2c["value"], f2c["numdf"], f2c["dendf"], lower.tail = FALSE)

C2d <- lm(WF~HdS+RainDiff, data = climCos[[1]])
C2dsumm <- summary(C2d)
f2d <- C2dsumm$fstatistic
p_2d <- pf(f2d["value"], f2d["numdf"], f2d["dendf"], lower.tail = FALSE)

C3 <- lm(WF~HdS+HdA+Rwint, data = climCos[[1]])
C3summ <- summary(C3)
f3 <- C3summ$fstatistic
p_3 <- pf(f3["value"], f3["numdf"], f3["dendf"], lower.tail = FALSE)

C3b <- lm(WF~HdS+HdA+RainDiff, data = climCos[[1]])
C3bsumm <- summary(C3b)
f3b <- C3bsumm$fstatistic
p_3b <- pf(f3b["value"], f3b["numdf"], f3b["dendf"], lower.tail = FALSE)

C3c <- lm(WF~HdS+HdA+LiS, data = climCos[[1]])
C3csumm <- summary(C3c)
f3c <- C3csumm$fstatistic
p_3c <- pf(f3c["value"], f3c["numdf"], f3c["dendf"], lower.tail = FALSE)

C3d <- lm(WF~HdS+Rwint+Raut, data = climCos[[1]])
C3dsumm <- summary(C3d)
f3d <- C3dsumm$fstatistic
p_3d <- pf(f3d["value"], f3d["numdf"], f3d["dendf"], lower.tail = FALSE)

C4 <- lm(WF~HdS+HdA+LiS+Rwint, data = climCos[[1]])
C4summ <- summary(C4)
f4 <- C4summ$fstatistic
p_4 <- pf(f4["value"], f4["numdf"], f4["dendf"], lower.tail = FALSE)

C4b <- lm(WF~HdS+HdA+Raut+Rwint, data = climCos[[1]])
C4bsumm <- summary(C4b)
f4b <- C4bsumm$fstatistic
p_4b <- pf(f4b["value"], f4b["numdf"], f4b["dendf"], lower.tail = FALSE)

C5 <- lm(WF~HdS+HdA+LiS+Raut+Rwint, data = climCos[[1]])
C5summ <- summary(C5)
f5 <- C5summ$fstatistic
p_5 <- pf(f5["value"], f5["numdf"], f5["dendf"], lower.tail = FALSE)

# Lc
Lc1 <- lm(WF~Temp, data = climCos[[1]])
Lc1summ <- summary(Lc1)
fc1 <- Lc1summ$fstatistic
pc_1 <- pf(fc1["value"], fc1["numdf"], fc1["dendf"], lower.tail = FALSE)

Lc2 <- lm(WF~Temp+Rwint, data = climCos[[1]])
Lc2summ <- summary(Lc2)
fc2 <- Lc2summ$fstatistic
pc_2 <- pf(fc2["value"], fc2["numdf"], fc2["dendf"], lower.tail = FALSE)

Lc2b <- lm(WF~Temp+RainDiff, data = climCos[[1]])
Lc2bsumm <- summary(Lc2b)
fc2b <- Lc2bsumm$fstatistic
pc_2b <- pf(fc2b["value"], fc2b["numdf"], fc2b["dendf"], lower.tail = FALSE)

Lc3 <- lm(WF~Temp+Rwint+Raut, data = climCos[[1]])
Lc3summ <- summary(Lc3)
fc3 <- Lc3summ$fstatistic
pc_3 <- pf(fc3["value"], fc3["numdf"], fc3["dendf"], lower.tail = FALSE)

# Model table
Clim_Models <- data.frame(
  Model = c("C1", "C1b", "C2", "C2b", "C2c", "C2d", "C3", "C3b", "C3c", "C3d", "C4", "C4b", "C5", "Lc1", "Lc2", "Lc2b", "Lc3"),
  Formula = c("HdS", "SynVar", "HdS+HdA", "HdS+LiS", "HdS+Rwint", "HdS+RainDiff",
              "HdS+HdA+Rwint", "HdS+HdA+RainDiff", "HdS+HdA+LiS", "HdS+Rwint+Raut", 
              "HdS+HdA+LiS+Rwint", "HdS+HdA+Raut+Rwint", "HdS+HdA+LiS+Raut+Rwint",
              "Temp", "Temp+Rwint", "Temp+RainDiff", "Temp+Rwint+Raut"),
  R_squared = c(round(C1summ$r.squared,2), round(C1bsumm$r.squared,2), round(C2summ$r.squared, 2), 
                round(C2bsumm$r.squared, 2), round(C2csumm$r.squared, 2), round(C2dsumm$r.squared, 2),
                round(C3summ$r.squared, 2), round(C3bsumm$r.squared, 2), round(C3csumm$r.squared,2), 
                round(C3dsumm$r.squared,2), round(C4summ$r.squared,2), round(C4bsumm$r.squared,2), round(C5summ$r.squared,2),
                round(Lc1summ$r.squared, 2), round(Lc2summ$r.squared, 2), round(Lc2bsumm$r.squared, 2), round(Lc3summ$r.squared, 2)),
  p_value = c(p_1, p_1b, p_2, p_2b, p_2c, p_2d, p_3, p_3b, p_3c, p_3d, p_4, p_4b, p_5, pc_1, pc_2, pc_2b, pc_3),
  AIC = c(AIC(C1), AIC(C1b), AIC(C2), AIC(C2b), AIC(C2c), AIC(C2d), AIC(C3), AIC(C3b), AIC(C3c), AIC(C3d), 
          AIC(C4), AIC(C4b), AIC(C5), AIC(Lc1), AIC(Lc2), AIC(Lc2b), AIC(Lc3))
) %>%
  arrange(AIC)
Clim_Models
write.csv(Clim_Models, "Clim_Models.csv", row.names = FALSE)

# 4. Format fire history data and remove outliers
currentAge <- pipTab[,c(2,24)] %>%
  mutate(Prop = round(X2021/sum(X2021, na.rm = T),4)) %>%
  dplyr::select(Age, Prop)
currentAge$Prop[which(is.na(currentAge$Prop))] <- 0
tab <- mana(ganadingaTab = pipTab, ageCol = 2, pipCols = c(47:68), Outliers = T)

Lc <-  Lc2
SD <- summary(Lc)$sigma
SE <- SD/sqrt(nrow(climCos[[1]]))

# Create a new data frame with the model predictions
SA <- climCos[[1]]
SA$modFull <- pmax(0,predict(C3))
SA$mod <- pmax(0,predict(Lc))
       