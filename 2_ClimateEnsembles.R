# SCRIPT 2
# IMPORT ALL SHARED SOCIOECONOMIC PATHWAY PREDICTIONS AND BUILD ENSEMBLES

library(readxl)
library(dplyr)
library(corrplot)
library(broom)

# Collect SSPs for temp and rainfall
dataSets <- excel_sheets("SSPs.xlsx")
T_SSP1_2.6 <- read_excel("SSPs.xlsx", sheet = dataSets[1])
T_SSP2_4.5 <- read_excel("SSPs.xlsx", sheet = dataSets[2])
T_SSP3_7.0 <- read_excel("SSPs.xlsx", sheet = dataSets[3])
R_SSP1_2.6 <- read_excel("SSPs.xlsx", sheet = dataSets[4])
R_SSP2_4.5 <- read_excel("SSPs.xlsx", sheet = dataSets[5])
R_SSP3_7.0 <- read_excel("SSPs.xlsx", sheet = dataSets[6])
HistoricalConditions <- read_excel("SSPs.xlsx", sheet = dataSets[7])

# Find mean cool season rainfall for 2000-2021 to use as a baseline for comparison
meanStudyPeriod <- mean(HistoricalConditions$Rainfall[HistoricalConditions$Year >= 2000 & HistoricalConditions$Year <= 2021], na.rm = TRUE)
HistoricalConditions$R_anomaly <- (HistoricalConditions$Rainfall - meanStudyPeriod)/92

# Find best hindcasting models for temperature
T1 <- T_SSP1_2.6[T_SSP1_2.6$Year >= 1910 & T_SSP1_2.6$Year <= 2023, ] %>%
  mutate(Historical = HistoricalConditions$Temp)
T_out <- vector()
CMIPs_T <- names(T_SSP1_2.6)[-1]
for (i in 1:length(CMIPs_T)) {
  T_out[i] <- cor.test(T1$Historical, T1[[CMIPs_T[i]]], method = "pearson", use = "complete.obs")$estimate[[1]]
}
CMIP_T <- data.frame('CMIP' = CMIPs_T, 'T_Rsq' = T_out^2)
CMIP_T <- CMIP_T[ order(CMIP_T$T_Rsq, decreasing = TRUE), ]
Best_T <- CMIP_T$CMIP[1:3]
ensemble_T <- lm(HistoricalConditions$Temp ~ T1[[Best_T[1]]] + T1[[Best_T[2]]] + T1[[Best_T[3]]])
Rsq_T <- summary(ensemble_T)$r.squared
p_T <- glance(ensemble_T)$p.value[[1]]
SD_T <- summary(ensemble_T)$sigma

# Find best hindcasting models for rainfall
R1 <- R_SSP1_2.6[T_SSP1_2.6$Year >= 1910 & T_SSP1_2.6$Year <= 2023, ] %>%
  mutate(Historical = HistoricalConditions$R_anomaly)
R_out <- vector()
CMIPs_R <- names(R_SSP1_2.6)[-1]
for (i in 1:length(CMIPs_R)) {
  R_out[i] <- cor.test(R1$Historical, R1[[CMIPs_R[i]]], method = "pearson", use = "complete.obs")$estimate[[1]]
}
CMIP_R <- data.frame('CMIP' = CMIPs_R, 'R_Rsq' = R_out^2)
CMIP_R <- CMIP_R[ order(CMIP_R$R_Rsq, decreasing = TRUE), ]
Best_R <- CMIP_R$CMIP[1:3]
ensemble_R <- lm(HistoricalConditions$R_anomaly ~ R1[[Best_R[1]]] + R1[[Best_R[2]]] + R1[[Best_R[3]]])
Rsq_R <- summary(ensemble_R)$r.squared
p_R <- glance(ensemble_R)$p.value[[1]]
SD_R <- summary(ensemble_R)$sigma

# Build the ensembles
SSPs <- data.frame('Year' = 1850:2100, 
                   'SSP1' = ensemble_T$coefficients[[1]]+
                     ensemble_T$coefficients[[2]]*T_SSP1_2.6[[Best_T[1]]] + 
                     ensemble_T$coefficients[[3]]*T_SSP1_2.6[[Best_T[2]]] + 
                     ensemble_T$coefficients[[4]]*T_SSP1_2.6[[Best_T[3]]],
                   'SSP2' = ensemble_T$coefficients[[1]]+
                     ensemble_T$coefficients[[2]]*T_SSP2_4.5[[Best_T[1]]] + 
                     ensemble_T$coefficients[[3]]*T_SSP2_4.5[[Best_T[2]]] + 
                     ensemble_T$coefficients[[4]]*T_SSP2_4.5[[Best_T[3]]],
                   'SSP3' = ensemble_T$coefficients[[1]]+
                     ensemble_T$coefficients[[2]]*T_SSP3_7.0[[Best_T[1]]] + 
                     ensemble_T$coefficients[[3]]*T_SSP3_7.0[[Best_T[2]]] + 
                     ensemble_T$coefficients[[4]]*T_SSP3_7.0[[Best_T[3]]],
                   'R1' = meanStudyPeriod+92*(ensemble_R$coefficients[[1]] + 
                     ensemble_R$coefficients[[2]]*R_SSP1_2.6[[Best_R[1]]] + 
                     ensemble_R$coefficients[[3]]*R_SSP1_2.6[[Best_R[2]]] + 
                     ensemble_R$coefficients[[4]]*R_SSP1_2.6[[Best_R[3]]]),
                   'R2' = meanStudyPeriod+92*(ensemble_R$coefficients[[1]] +
                     ensemble_R$coefficients[[2]]*R_SSP2_4.5[[Best_R[1]]] + 
                     ensemble_R$coefficients[[3]]*R_SSP2_4.5[[Best_R[2]]] + 
                     ensemble_R$coefficients[[4]]*R_SSP2_4.5[[Best_R[3]]]),
                   'R3' = meanStudyPeriod+92*(ensemble_R$coefficients[[1]] +
                     ensemble_R$coefficients[[2]]*R_SSP3_7.0[[Best_R[1]]] + 
                     ensemble_R$coefficients[[3]]*R_SSP3_7.0[[Best_R[2]]] + 
                     ensemble_R$coefficients[[4]]*R_SSP3_7.0[[Best_R[3]]]))

