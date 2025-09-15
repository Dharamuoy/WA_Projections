# SCRIPT 5
# Modelling future scenarios

library(dplyr)
library(extraDistr)
library(MASS)
source("ProjectionFunctions.R")

# TREATMENTS
annualBurn <- c(0.08, 0, 0, 0.08) # Prop area burnt by PB annually
suppression <- c(1, (1-as.numeric(PBescapes)), (1-as.numeric(Suppression_reduction)) *(1-as.numeric(PBescapes)), (1-as.numeric(Suppression_reduction))) # Current, no PB escapes, no PB escapes + rapid suppression
reps <- 100
startYear <- 2022
endYear <- 2100
#___________________________________________________________________
# 1. Create mean fire scenarios relative to 2010

meanR <- (mean(SSPs[157:172, 5])+mean(SSPs[157:172, 6])+mean(SSPs[157:172, 7]))/3
meanT <- (mean(SSPs[157:172, 2])+mean(SSPs[157:172, 3])+mean(SSPs[157:172, 4]))/3
Baseline <- Lc2$coefficients[[1]]+Lc2$coefficients[[2]]*meanT+Lc2$coefficients[[3]]*meanR

# 1) extract betas, vcov and residual sd
beta_mu   <- coef(Lc2)
beta_V    <- vcov(Lc2)
sigma_res <- summary(Lc2)$sigma

# 2) prepare storage data frames
Climate1 <- data.frame(Year = SSP_short$Year, 
                       Mean = predict(Lc2, newdata = data.frame(Rwint = SSP_short$R1, Temp  = SSP_short$SSP1)
                                      )
)
Climate1[ , 3:(reps+2) ] <- NA_real_

Climate2 <- data.frame(Year = SSP_short$Year,
  Mean = predict(Lc2, newdata = data.frame(Rwint = SSP_short$R2, Temp  = SSP_short$SSP2)
  )
)
Climate2[ , 3:(reps+2) ] <- NA_real_

Climate3 <- data.frame(Year = SSP_short$Year,
  Mean = predict(Lc2, newdata = data.frame(Rwint = SSP_short$R3, Temp  = SSP_short$SSP3)
  )
)
Climate3[ , 3:(reps+2) ] <- NA_real_

# 3) Build baseline fire expectations from three emissions scenarios
# Randomise climate variables accumulating error from all component model residuals 

for (rep in seq_len(reps)) {
  beta_draw <- mvrnorm(1, mu = beta_mu, Sigma = beta_V) # Randomise LFclimate using uncertainty of inputs
  
  # — SSP 1 —
  R1_draw <- rnorm(nrow(SSP_short), mean = SSP_short$R1,  sd = SD_R)
  T1_draw <- rnorm(nrow(SSP_short), mean = SSP_short$SSP1, sd = SD_T)
  X1_draw <- cbind(1, T1_draw, R1_draw)
  eps1 <- rnorm(nrow(SSP_short), 0, sigma_res)
  
  y1  <- as.vector(X1_draw %*% beta_draw + eps1)
  WF1 <- pmax(0, round((y1 / Baseline), 2))
  Climate1[ , rep + 2 ] <- WF1
  
  # — SSP 2 —
  R2_draw <- rnorm(nrow(SSP_short), mean = SSP_short$R2,  sd = SD_R)
  T2_draw <- rnorm(nrow(SSP_short), mean = SSP_short$SSP2, sd = SD_T)
  X2_draw <- cbind(1, T2_draw, R2_draw)
  eps2     <- rnorm(nrow(SSP_short), 0, sigma_res)
  
  y2  <- as.vector(X2_draw %*% beta_draw + eps2)
  WF2 <- pmax(0, round((y2 / Baseline), 2))
  Climate2[ , rep + 2 ] <- WF2
  
  # — SSP 3 —
  R3_draw <- rnorm(nrow(SSP_short), mean = SSP_short$R3,  sd = SD_R)
  T3_draw <- rnorm(nrow(SSP_short), mean = SSP_short$SSP3, sd = SD_T)
  X3_draw <- cbind(1, T3_draw, R3_draw)
  eps3     <- rnorm(nrow(SSP_short), 0, sigma_res)
  
  y3  <- as.vector(X3_draw %*% beta_draw + eps3)
  WF3 <- pmax(0, round((y3 / Baseline), 2))
  Climate3[ , rep + 2 ] <- WF3
}

SSPs <- SSPs %>%
  mutate(S1 = predict(Lc2, newdata = data.frame(Rwint = R1,   Temp = SSP1)) / Baseline,
    S2 = predict(Lc2, newdata = data.frame(Rwint = R2,   Temp = SSP2)) / Baseline,
    S3 = predict(Lc2, newdata = data.frame(Rwint = R3,   Temp = SSP3)) / Baseline
  )

# 2. Project wildfire likelihood into the future under varying management scenarios

# Collect the age class makeup of the landscape
ageDat <- pipTab[,c(2,24)] %>%
  mutate(Prop = round(X2021/sum(X2021, na.rm = T),4))
ageTab <- ageDat[which(complete.cases(ageDat)),] %>%
  dplyr::select(Age, Prop)

# Run the Monte Carlo analyses for each SSP and management scenario, accounting for variability in climatic drivers and flammability feedbacks
ScenarioA <- gunamaGurad(ageTab, startYear = 2022, endYear = endYear, meanFrequency = 1, frequencyTab = Climate1, reps = reps,
                         trendForm = "Q3", constants = c(q_params$coefficients[[1]], q_params$coefficients[[2]], q_params$coefficients[[3]], q_params$coefficients[[4]]), domain = Domain, 
                         SEtrendForm = "Q2", SEconstants = c(SEParams$coefficients[[1]], SEParams$coefficients[[2]], SEParams$coefficients[[3]]),
                         annualBurn = annualBurn, suppression = suppression, burnYear  = c(NA, NA, NA, NA), PBDuration  = c(NA, NA, NA, NA), freeCores = 1)

ScenarioB <- gunamaGurad(ageTab, startYear = 2022, endYear = endYear, meanFrequency = 1, frequencyTab = Climate2, reps = reps,
                         trendForm = "Q3", constants = c(q_params$coefficients[[1]], q_params$coefficients[[2]], q_params$coefficients[[3]], q_params$coefficients[[4]]), domain = Domain, 
                         SEtrendForm = "Q2", SEconstants = c(SEParams$coefficients[[1]], SEParams$coefficients[[2]], SEParams$coefficients[[3]]),
                         annualBurn = annualBurn, suppression = suppression, burnYear  = c(NA, NA, NA, NA), PBDuration  = c(NA, NA, NA, NA), freeCores = 1)

ScenarioC <- gunamaGurad(ageTab, startYear = 2022, endYear = endYear, meanFrequency = 1, frequencyTab = Climate3, reps = reps,
                         trendForm = "Q3", constants = c(q_params$coefficients[[1]], q_params$coefficients[[2]], q_params$coefficients[[3]], q_params$coefficients[[4]]), domain = Domain, 
                         SEtrendForm = "Q2", SEconstants = c(SEParams$coefficients[[1]], SEParams$coefficients[[2]], SEParams$coefficients[[3]]),
                         annualBurn = annualBurn, suppression = suppression, burnYear  = c(NA, NA, NA, NA), PBDuration  = c(NA, NA, NA, NA), freeCores = 1)


