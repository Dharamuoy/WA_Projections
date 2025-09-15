# SCRIPT 6
# Analyse modelling results

library(ggplot2) 
library(patchwork)
library(effsize)
library(forcats)
library(MASS)
library(purrr)
library(weights)
library(dplyr)

# A. FIRE LIKELIHOOD EFFECTS

likelihoodTrajectoriesA_X <- ScenarioA %>%
  group_by(Treatment, Year, Rep) %>%
  summarise(wmean = weighted.mean(Likelihood, Prop)) %>%
  ungroup()
likelihoodTrajectoriesA_mean <- likelihoodTrajectoriesA_X %>%
  group_by(Treatment, Year) %>%
  summarise(wLikelihood = mean(wmean)) %>%
  ungroup() 
likelihoodTrajectoriesA_sd <- likelihoodTrajectoriesA_X %>%
  group_by(Treatment, Year) %>%
  summarise(wSE = sd(wmean)/sqrt(reps), SD = sd(wmean)) %>%
  ungroup()
likelihoodTrajectoriesA_ <- likelihoodTrajectoriesA_mean %>%
  left_join(likelihoodTrajectoriesA_sd) %>%
  mutate(Emissions = "SSP1-2.6")


likelihoodTrajectoriesB_X <- ScenarioB %>%
  group_by(Treatment, Year, Rep) %>%
  summarise(wmean = weighted.mean(Likelihood, Prop)) %>%
  ungroup()
likelihoodTrajectoriesB_mean <- likelihoodTrajectoriesB_X %>%
  group_by(Treatment, Year) %>%
  summarise(wLikelihood = mean(wmean)) %>%
  ungroup() 
likelihoodTrajectoriesB_sd <- likelihoodTrajectoriesB_X %>%
  group_by(Treatment, Year) %>%
  summarise(wSE = sd(wmean)/sqrt(reps), SD = sd(wmean)) %>%
  ungroup()
likelihoodTrajectoriesB_ <- likelihoodTrajectoriesB_mean %>%
  left_join(likelihoodTrajectoriesB_sd) %>%
  mutate(Emissions = "SSP2-4.5")


likelihoodTrajectoriesC_X <- ScenarioC %>%
  group_by(Treatment, Year, Rep) %>%
  summarise(wmean = weighted.mean(Likelihood, Prop)) %>%
  ungroup()
likelihoodTrajectoriesC_mean <- likelihoodTrajectoriesC_X %>%
  group_by(Treatment, Year) %>%
  summarise(wLikelihood = mean(wmean)) %>%
  ungroup() 
likelihoodTrajectoriesC_sd <- likelihoodTrajectoriesC_X %>%
  group_by(Treatment, Year) %>%
  summarise(wSE = sd(wmean)/sqrt(reps), SD = sd(wmean)) %>%
  ungroup()
likelihoodTrajectoriesC_ <- likelihoodTrajectoriesC_mean %>%
  left_join(likelihoodTrajectoriesC_sd) %>%
  mutate(Emissions = "SSP3-7.0")

likelihoodTrajectories <- likelihoodTrajectoriesA_ %>%
  rbind(likelihoodTrajectoriesB_, likelihoodTrajectoriesC_)

# ___________________________________________________________________________________________________
# B. ALL FIRE FREQUENCY EFFECTS
LFTrajectoriesA_X  <- ScenarioA %>%
  group_by(Treatment, Year, Rep) %>%
  summarise(wmean = weighted.mean((WF + Planned), Prop)) %>%
  ungroup()

LFTrajectoriesB_X  <- ScenarioB %>%
  group_by(Treatment, Year, Rep) %>%
  summarise(wmean = weighted.mean((WF + Planned), Prop)) %>%
  ungroup()

LFTrajectoriesC_X  <- ScenarioC %>%
  group_by(Treatment, Year, Rep) %>%
  summarise(wmean = weighted.mean((WF + Planned), Prop)) %>%
  ungroup()

# ___________________________________________________________________________________________________
# C. CROWN DAMAGE EFFECTS
scorchTrajectoriesA_X <- ScenarioA %>%
  mutate(Damage = case_when(WF == 1 ~ 90.7,
                            Planned == 1 ~ 42.7,
                            TRUE ~ 0)) %>%
  group_by(Treatment, Year, Rep) %>%
  summarise(wmean = weighted.mean(Damage, Prop)) %>%
  ungroup()
scorchTrajectoriesA_mean <- scorchTrajectoriesA_X %>%
  group_by(Treatment, Year) %>%
  summarise(sFrequency = mean(wmean)) %>%
  ungroup() 
scorchTrajectoriesA_sd <- scorchTrajectoriesA_X %>%
  group_by(Treatment, Year) %>%
  summarise(sSE = sd(wmean)/sqrt(reps), SD = sd(wmean)) %>%
  ungroup()
scorchTrajectoriesA_ <- scorchTrajectoriesA_mean %>%
  left_join(scorchTrajectoriesA_sd) %>%
  mutate(Emissions = "SSP1-2.6")

scorchTrajectoriesB_X <- ScenarioB %>%
  mutate(Damage = case_when(WF == 1 ~ 90.7,
                            Planned == 1 ~ 42.7,
                            TRUE ~ 0)) %>%
  group_by(Treatment, Year, Rep) %>%
  summarise(wmean = weighted.mean(Damage, Prop)) %>%
  ungroup()
scorchTrajectoriesB_mean <- scorchTrajectoriesB_X %>%
  group_by(Treatment, Year) %>%
  summarise(sFrequency = mean(wmean)) %>%
  ungroup() 
scorchTrajectoriesB_sd <- scorchTrajectoriesB_X %>%
  group_by(Treatment, Year) %>%
  summarise(sSE = sd(wmean)/sqrt(reps), SD = sd(wmean)) %>%
  ungroup()
scorchTrajectoriesB_ <- scorchTrajectoriesB_mean %>%
  left_join(scorchTrajectoriesB_sd) %>%
  mutate(Emissions = "SSP2-4.5")

scorchTrajectoriesC_X <- ScenarioC %>%
  mutate(Damage = case_when(WF == 1 ~ 90.7,
                            Planned == 1 ~ 42.7,
                            TRUE ~ 0)) %>%
  group_by(Treatment, Year, Rep) %>%
  summarise(wmean = weighted.mean(Damage, Prop)) %>%
  ungroup()
scorchTrajectoriesC_mean <- scorchTrajectoriesC_X %>%
  group_by(Treatment, Year) %>%
  summarise(sFrequency = mean(wmean)) %>%
  ungroup() 
scorchTrajectoriesC_sd <- scorchTrajectoriesC_X %>%
  group_by(Treatment, Year) %>%
  summarise(sSE = sd(wmean)/sqrt(reps), SD = sd(wmean)) %>%
  ungroup()
scorchTrajectoriesC_ <- scorchTrajectoriesC_mean %>%
  left_join(scorchTrajectoriesC_sd) %>%
  mutate(Emissions = "SSP3-7.0")

scorchTrajectories <- scorchTrajectoriesA_ %>%
  rbind(scorchTrajectoriesB_, scorchTrajectoriesC_) %>%
  mutate(Treatment = factor(Treatment),
         Treatment = fct_recode(Treatment,
                                "PB+A+" = '1',        
                                "PB-A+" = '2',
                                "PB-A-" = '3',
                                "PB+A-" = '4'),
         Treatment = factor(Treatment,                    
                            levels = c("PB+A+","PB+A-","PB-A+","PB-A-")))

# ___________________________________________________________________________________________________
# D. AGE CLASS TRAJECTORIES
A_TrajectoriesA_X  <- ScenarioA %>%
  mutate(wAge = Prop/reps) %>%
  dplyr::select(Rep, Year, Treatment, Year, TSF) %>%
  group_by(Rep, Year, Treatment, TSF) %>%
  summarise_if(is.numeric, sum) %>%
  distinct(Treatment, Year, Rep, TSF) %>%
  group_by(Treatment, Year, Rep) %>%
  summarise(
    wmean = n_distinct(TSF),
    .groups = "drop"
  )

A_TrajectoriesB_X  <- ScenarioB %>%
  mutate(wAge = Prop/reps) %>%
  dplyr::select(Rep, Year, Treatment, Year, TSF) %>%
  group_by(Rep, Year, Treatment, TSF) %>%
  summarise_if(is.numeric, sum) %>%
  distinct(Treatment, Year, Rep, TSF) %>%
  group_by(Treatment, Year, Rep) %>%
  summarise(
    wmean = n_distinct(TSF),
    .groups = "drop"
  )

A_TrajectoriesC_X  <- ScenarioC %>%
  mutate(wAge = Prop/reps) %>%
  dplyr::select(Rep, Year, Treatment, Year, TSF) %>%
  group_by(Rep, Year, Treatment, TSF) %>%
  summarise_if(is.numeric, sum) %>%
  distinct(Treatment, Year, Rep, TSF) %>%
  group_by(Treatment, Year, Rep) %>%
  summarise(
    wmean = n_distinct(TSF),
    .groups = "drop"
  )

# ___________________________________________________________________________________________________

# MEASURE STUDY OUTCOMES

# 1. Wildfire likelihood
L2100 <- list(
  SSP1_2.6 = likelihoodTrajectoriesA_X,
  SSP2_4.5 = likelihoodTrajectoriesB_X,
  SSP3_7.0 = likelihoodTrajectoriesC_X
)

L2100_tab <- map_dfr(L2100, outcome_tests, .id = "Climate", value = "wmean") %>%
  mutate(Treatment = recode(Treatment,         # turn the numeric codes into labels
                            `2` = "PB-",
                            `3` = "PB-A-",
                            `4` = "A-"),
         Treatment = fct_relevel(Treatment,   # set the order
                                 "PB-",
                                 "A-",
                                 "PB-A-")
  ) %>%
  arrange(Climate, Treatment)  
write.csv(L2100_tab, "L2100_tab.csv", row.names = FALSE)

# Climate effects
L2100C2_tab <- climate_attribution(df_control = L2100[[1]], df = L2100[[2]]) %>%
  mutate(Climate = "SSP2_4.5") 
L2100C3_tab <- climate_attribution(df_control = L2100[[1]], df = L2100[[3]]) %>%
  mutate(Climate = "SSP3_7.0")
L2100C_tab <- rbind(L2100C2_tab, L2100C3_tab)
write.csv(L2100C_tab, "L2100C_tab.csv", row.names = FALSE)

# 2. Fire likelihood
LF2100 <- list(
  SSP1_2.6 = LFTrajectoriesA_X,
  SSP2_4.5 = LFTrajectoriesB_X,
  SSP3_7.0 = LFTrajectoriesC_X
)

LF2100_tab <- map_dfr(LF2100, outcome_tests, .id = "Climate", value = "wmean") %>%
  mutate(Treatment = recode(Treatment,         # turn the numeric codes into labels
                            `2` = "PB-",
                            `3` = "PB-A-",
                            `4` = "A-"),
         Treatment = fct_relevel(Treatment,   # set the order
                                 "PB-",
                                 "A-",
                                 "PB-A-")
  ) %>%
  arrange(Climate, Treatment)  
write.csv(LF2100_tab, "LF2100_tab.csv", row.names = FALSE)

# Climate effects
LF2100C2_tab <- climate_attribution(df_control = LF2100[[1]], df = LF2100[[2]]) %>%
  mutate(Climate = "SSP2_4.5") 
LF2100C3_tab <- climate_attribution(df_control = LF2100[[1]], df = LF2100[[3]]) %>%
  mutate(Climate = "SSP3_7.0")
LF2100C_tab <- rbind(LF2100C2_tab, LF2100C3_tab)
write.csv(LF2100C_tab, "LF2100C_tab.csv", row.names = FALSE)

# 3. High severity fire likelihood
S2100 <- list(
  SSP1_2.6 = scorchTrajectoriesA_X,
  SSP2_4.5 = scorchTrajectoriesB_X,
  SSP3_7.0 = scorchTrajectoriesC_X
)

S2100_tab <- map_dfr(S2100, outcome_tests, .id = "Climate", value = "wmean") %>%
  mutate(Treatment = recode(Treatment,         # turn the numeric codes into labels
                            `2` = "PB-",
                            `3` = "PB-A-",
                            `4` = "A-"),
         Treatment = fct_relevel(Treatment,   # set the order
                                 "PB-",
                                 "A-",
                                 "PB-A-")
  ) %>%
  arrange(Climate, Treatment)
write.csv(S2100_tab, "S2100_tab.csv", row.names = FALSE)

# Climate effects
S2100C2_tab <- climate_attribution(df_control = S2100[[1]], df = S2100[[2]]) %>%
  mutate(Climate = "SSP2_4.5") 
S2100C3_tab <- climate_attribution(df_control = S2100[[1]], df = S2100[[3]]) %>%
  mutate(Climate = "SSP3_7.0")
S2100C_tab <- rbind(S2100C2_tab, S2100C3_tab)
write.csv(S2100C_tab, "S2100C_tab.csv", row.names = FALSE)

# F. Age class diversity
AC2100 <- list(
  SSP1_2.6 = A_TrajectoriesA_X,
  SSP2_4.5 = A_TrajectoriesB_X,
  SSP3_7.0 = A_TrajectoriesC_X
)

AC2100_tab <- map_dfr(AC2100, outcome_tests, .id = "Climate", value = "wmean") %>%
  mutate(Treatment = recode(Treatment,         # turn the numeric codes into labels
                            `2` = "PB-",
                            `3` = "PB-A-",
                            `4` = "A-"),
         Treatment = fct_relevel(Treatment,   # set the order
                                 "PB-",
                                 "A-",
                                 "PB-A-")
  ) %>%
  arrange(Climate, Treatment)
write.csv(AC2100_tab, "AC2100_tab.csv", row.names = FALSE)

# Climate effects
AC2100C2_tab <- climate_attribution(df_control = AC2100[[1]], df = AC2100[[2]]) %>%
  mutate(Climate = "SSP2_4.5") 
AC2100C3_tab <- climate_attribution(df_control = AC2100[[1]], df = AC2100[[3]]) %>%
  mutate(Climate = "SSP3_7.0")
AC2100C_tab <- rbind(AC2100C2_tab, AC2100C3_tab)
write.csv(AC2100C_tab, "AC2100C_tab.csv", row.names = FALSE)

# ___________________________________________________________________________________________________

# Total burn area per treatment; SSP 1
totalBurnA_ <- ScenarioA %>%
  mutate(Burn = round(pmax(Planned,WF) * 528343 * Prop * (1/reps),0)) %>%
  dplyr::select(Treatment, Burn) %>%
  group_by(Treatment) %>%
  summarise_if(is.numeric, sum)

# Total burn area per treatment; SSP 2
totalBurnB_ <- ScenarioB %>%
  mutate(Burn = round(pmax(Planned,WF) * 528343 * Prop * (1/reps),0)) %>%
  dplyr::select(Treatment, Burn) %>%
  group_by(Treatment) %>%
  summarise_if(is.numeric, sum)

# Total burn area per treatment; SSP 3
totalBurnC_ <- ScenarioC %>%
  mutate(Burn = round(pmax(Planned,WF) * 528343 * Prop * (1/reps),0)) %>%
  dplyr::select(Treatment, Burn) %>%
  group_by(Treatment) %>%
  summarise_if(is.numeric, sum)

######################################################
# Analyse age-class distributions
ageTrajectoriesA_ <- ScenarioA[ScenarioA$Year == endYear,] %>%
  mutate(wAge = Prop/reps) %>%
  dplyr::select(Treatment, TSF, wAge) %>%
  group_by(Treatment, TSF) %>%
  summarise_if(is.numeric, sum)
ageTrajectoriesB_ <- ScenarioB[ScenarioB$Year == endYear,] %>%
  mutate(wAge = Prop/reps) %>%
  dplyr::select(Treatment, TSF, wAge) %>%
  group_by(Treatment, TSF) %>%
  summarise_if(is.numeric, sum)
ageTrajectoriesC_ <- ScenarioC[ScenarioC$Year == endYear,] %>%
  mutate(wAge = Prop/reps) %>%
  dplyr::select(Treatment, TSF, wAge) %>%
  group_by(Treatment, TSF) %>%
  summarise_if(is.numeric, sum)
allAgesA_ <- unique(ageTrajectoriesA_$TSF)

for (Treat in unique(ageTrajectoriesA_$Treatment)) {
  if (length(which(!allAgesA_ %in% unique(ageTrajectoriesA_$TSF[ageTrajectoriesA_$Treatment == Treat])))>0) {
    missing <- data.frame(Treatment = Treat, TSF = which(!allAgesA_ %in% unique(ageTrajectoriesA_$TSF[ageTrajectoriesA_$Treatment == Treat])), 
                          wAge = 0)
    ageTrajectoriesA_ <- ageTrajectoriesA_ %>% rbind(missing)
  }
}


# Overlay flammability model with ageTrajectoriesA_
newdata_ <- data.frame(Age = seq.int(1, max(tab$Age)))

PBA <- ageTrajectoriesA_[ageTrajectoriesA_$Treatment == 1,] %>%
  mutate(wAgeA = wAge)%>%
  dplyr::select(TSF, wAgeA)
PBxAx <- ageTrajectoriesA_[ageTrajectoriesA_$Treatment == 3,] %>%
  mutate(wAgeB = wAge)%>%
  dplyr::select(TSF, wAgeB)
out <- data.frame('Age' = newdata_, 'Likelihood'= pmax(predict(q_mod,   newdata = newdata_),0), 
                  'SE' = pmax(predict(SE_mod, newdata = newdata_),0)) %>%
  left_join(currentAge, by = "Age") %>%
  mutate(TSF = Age,
         wAgeO = Prop) 
outB <- data.frame('Age' = 73:max(ageTrajectoriesA_$TSF), 'Likelihood'= NA, 'SE' = NA, 'Prop' = 0, 
                   'TSF' = 73:max(ageTrajectoriesA_$TSF), 'wAgeO' = 0)
out <- out %>%
  rbind(outB) %>%
  left_join(PBA, by = "TSF") %>%
  left_join(PBxAx, by = "TSF")
out$wAgeA[is.na(out$wAgeA)] <- 0
out$wAgeB[is.na(out$wAgeB)] <- 0

# Summarise pyrodiversity
Scen_A_ <- ScenarioA[ScenarioA$Year == endYear,] %>%
  filter(Prop > 0) %>%                    
  group_by(Treatment, Rep) %>%            
  summarise(
    PDV = n_distinct(TSF),       
    .groups = "drop"
  ) %>%
  mutate(Treatment = factor(Treatment),
         Treatment = fct_recode(Treatment,
                                "PB+A+" = '1',        
                                "PB-A+" = '2',
                                "PB-A-" = '3',
                                "PB+A-" = '4'),
         Treatment = factor(Treatment,                    
                            levels = c("PB+A+","PB+A-","PB-A+","PB-A-")),
         Emissions = "SSP1-2.6")
Scen_A_all <- ScenarioA %>%
  filter(Prop > 0) %>%                    
  group_by(Year, Treatment) %>%            
  summarise(
    PDV = n_distinct(TSF),       
    .groups = "drop"
  ) %>%
  mutate(Treatment = factor(Treatment),
         Treatment = fct_recode(Treatment,
                                "PB+A+" = '1',        
                                "PB-A+" = '2',
                                "PB-A-" = '3',
                                "PB+A-" = '4'),
         Treatment = factor(Treatment,                    
                            levels = c("PB+A+","PB+A-","PB-A+","PB-A-")),
         Emissions = "SSP1-2.6")
Scen_B_ <- ScenarioB[ScenarioB$Year == endYear,] %>%
  filter(Prop > 0) %>%                    # only keep WAge > 0
  group_by(Treatment, Rep) %>%            # one group per Rep × Treatment
  summarise(
    PDV = n_distinct(TSF),       # count distinct TSF
    .groups = "drop"
  ) %>%
  mutate(Treatment = factor(Treatment),
         Treatment = fct_recode(Treatment,
                                "PB+A+" = '1',        
                                "PB-A+" = '2',
                                "PB-A-" = '3',
                                "PB+A-" = '4'),
         Treatment = factor(Treatment,                    
                            levels = c("PB+A+","PB+A-","PB-A+","PB-A-")),
         Emissions = "SSP2-4.5")
Scen_B_all <- ScenarioB %>%
  filter(Prop > 0) %>%                    
  group_by(Year, Treatment) %>%            
  summarise(
    PDV = n_distinct(TSF),       
    .groups = "drop"
  ) %>%
  mutate(Treatment = factor(Treatment),
         Treatment = fct_recode(Treatment,
                                "PB+A+" = '1',        
                                "PB-A+" = '2',
                                "PB-A-" = '3',
                                "PB+A-" = '4'),
         Treatment = factor(Treatment,                    
                            levels = c("PB+A+","PB+A-","PB-A+","PB-A-")),
         Emissions = "SSP2-4.5")
Scen_C_ <- ScenarioC[ScenarioC$Year == endYear,] %>%
  filter(Prop > 0) %>%                    # only keep WAge > 0
  group_by(Treatment, Rep) %>%            # one group per Rep × Treatment
  summarise(
    PDV = n_distinct(TSF),       # count distinct TSF
    .groups = "drop"
  ) %>%
  mutate(Treatment = factor(Treatment),
         Treatment = fct_recode(Treatment,
                                "PB+A+" = '1',        
                                "PB-A+" = '2',
                                "PB-A-" = '3',
                                "PB+A-" = '4'),
         Treatment = factor(Treatment,                    
                            levels = c("PB+A+","PB+A-","PB-A+","PB-A-")),
         Emissions = "SSP3-7.0")
Scen_C_all <- ScenarioC %>%
  filter(Prop > 0) %>%                    
  group_by(Year, Treatment) %>%            
  summarise(
    PDV = n_distinct(TSF),       
    .groups = "drop"
  ) %>%
  mutate(Treatment = factor(Treatment),
         Treatment = fct_recode(Treatment,
                                "PB+A+" = '1',        
                                "PB-A+" = '2',
                                "PB-A-" = '3',
                                "PB+A-" = '4'),
         Treatment = factor(Treatment,                    
                            levels = c("PB+A+","PB+A-","PB-A+","PB-A-")),
         Emissions = "SSP3-7.0")

PDV <- rbind(Scen_A_, Scen_B_, Scen_C_)
PDV_All <- rbind(Scen_A_all, Scen_B_all, Scen_C_all)


PDVt <- data.frame('Emissions' = c("SSP2-4.5", "SSP2-4.5", "SSP2-4.5", "SSP2-4.5",
                                   "SSP1-2.6", "SSP1-2.6", "SSP1-2.6", "SSP1-2.6", 
                                   "SSP3-7.0", "SSP3-7.0", "SSP3-7.0", "SSP3-7.0"), 
                   'Treatment' = c("PB+A+", "PB-A+", "PB+A-", "PB-A-","PB+A+", "PB-A+", "PB+A-", "PB-A-", 
                                   "PB+A+", "PB-A+", "PB+A-", "PB-A-"), PDV = c(
                                     length(unique(ageTrajectoriesA_$TSF[which(ageTrajectoriesA_$wAge > 0 & ageTrajectoriesA_$Treatment == 1)])),
                                     length(unique(ageTrajectoriesA_$TSF[which(ageTrajectoriesA_$wAge > 0 & ageTrajectoriesA_$Treatment == 2)])),
                                     length(unique(ageTrajectoriesA_$TSF[which(ageTrajectoriesA_$wAge > 0 & ageTrajectoriesA_$Treatment == 4)])),
                                     length(unique(ageTrajectoriesA_$TSF[which(ageTrajectoriesA_$wAge > 0 & ageTrajectoriesA_$Treatment == 3)])),
                                     length(unique(ageTrajectoriesB_$TSF[which(ageTrajectoriesB_$wAge > 0 & ageTrajectoriesB_$Treatment == 1)])),
                                     length(unique(ageTrajectoriesB_$TSF[which(ageTrajectoriesB_$wAge > 0 & ageTrajectoriesB_$Treatment == 2)])),
                                     length(unique(ageTrajectoriesB_$TSF[which(ageTrajectoriesB_$wAge > 0 & ageTrajectoriesB_$Treatment == 4)])),
                                     length(unique(ageTrajectoriesB_$TSF[which(ageTrajectoriesB_$wAge > 0 & ageTrajectoriesB_$Treatment == 3)])),
                                     length(unique(ageTrajectoriesC_$TSF[which(ageTrajectoriesC_$wAge > 0 & ageTrajectoriesC_$Treatment == 1)])),
                                     length(unique(ageTrajectoriesC_$TSF[which(ageTrajectoriesC_$wAge > 0 & ageTrajectoriesC_$Treatment == 2)])),
                                     length(unique(ageTrajectoriesC_$TSF[which(ageTrajectoriesC_$wAge > 0 & ageTrajectoriesC_$Treatment == 4)])),
                                     length(unique(ageTrajectoriesC_$TSF[which(ageTrajectoriesC_$wAge > 0 & ageTrajectoriesC_$Treatment == 3)])))
) %>%
  mutate(Treatment = factor(Treatment),
         Treatment = factor(Treatment,                    
                            levels = c("PB+A+","PB+A-","PB-A+","PB-A-")))
Final_L_A <- ScenarioA %>%
  filter(Year == 2100) %>%
  group_by(Rep, Treatment) %>%
  summarise(wmean = weighted.mean(Likelihood, Prop)) %>%
  summarise(
    Diff = wmean[Treatment == 1] /
      wmean[Treatment == 3]
  ) %>%
  mutate(Emissions = "SSP1_2.6") %>%
  ungroup()

Final_L_B <- ScenarioB %>%
  filter(Year == 2100) %>%
  group_by(Rep, Treatment) %>%
  summarise(wmean = weighted.mean(Likelihood, Prop)) %>%
  summarise(
    Diff = wmean[Treatment == 1] /
      wmean[Treatment == 3]
  ) %>%
  mutate(Emissions = "SSP2_4.5") %>%
  ungroup()

Final_L_C <- ScenarioC %>%
  filter(Year == 2100) %>%
  group_by(Rep, Treatment) %>%
  summarise(wmean = weighted.mean(Likelihood, Prop)) %>%
  summarise(
    Diff = wmean[Treatment == 1] /
      wmean[Treatment == 3]
  ) %>%
  mutate(Emissions = "SSP3_7.0") %>%
  ungroup()

Final_L <- Final_L_A %>%
  rbind(Final_L_B, Final_L_C)

# Fig. 3

TT <- ggplot(SSPs[SSPs$Year>=1950,], aes(x = Year, y = SSP1)) +
  geom_line(color = "royalblue4") +
  geom_smooth(fill = "royalblue4", alpha = 0.3) +
  geom_line(aes(y = SSP2), color = "darkolivegreen4") +
  geom_smooth(aes(y = SSP2), color = "darkolivegreen4", fill = "darkolivegreen4", alpha = 0.3) +
  geom_line(aes(y = SSP3), color = "gold3") +
  geom_smooth(aes(y = SSP3), color = "gold3", fill = "gold3", alpha = 0.3) +
  labs(title ="a.",  x = "Year", y = expression(bold("T anomaly ("^" o"*"C)"))) +
  theme_bw() +
  theme(legend.text = element_text(size=9, colour = "black"),
        legend.title = element_text(size=9, colour = "black", face="bold"),
        axis.text.x  = element_text(vjust=1.5, size=11, colour = "black"),
        axis.text.y  = element_text(size=11, colour = "black"),
        axis.title.y = element_text(size=11, face="bold"),
        axis.title.x = element_text(size=11, face="bold"),
        plot.title = element_text(vjust=1.5, face="bold", size=13, colour = "black"))

RF <- ggplot(SSPs[SSPs$Year>=1950,], aes(x = Year, y = R1)) +
  geom_line(color = "royalblue4") +
  geom_smooth(fill = "royalblue4", alpha = 0.3) +
  geom_line(aes(y = R2), color = "darkolivegreen4") +
  geom_smooth(aes(y = R2), color = "darkolivegreen4", fill = "darkolivegreen4", alpha = 0.3) +
  geom_line(aes(y = R3), color = "gold3") +
  geom_smooth(aes(y = R3), color = "gold3", fill = "gold3", alpha = 0.3) +
  labs(title ="b.",  color = "Scenario", x = "Year", y = expression(bold("Rainfall"[winter]~"(mm)"))) +
  theme_bw() +
  theme(legend.text = element_text(size=9, colour = "black"),
        legend.title = element_text(size=9, colour = "black", face="bold"),
        axis.text.x  = element_text(vjust=1.5, size=11, colour = "black"),
        axis.text.y  = element_text(size=11, colour = "black"),
        axis.title.y = element_text(size=11, face="bold"),
        axis.title.x = element_text(size=11, face="bold"),
        plot.title = element_text(vjust=1.5, face="bold", size=13, colour = "black")) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5))) 

SSP_P <- ggplot(SSPs[SSPs$Year>=1950,], aes(x = Year, y = S1)) +
  scale_color_manual(values = c("SSP2-4.5" = "darkolivegreen4", "SSP1-2.6" = "royalblue4", "SSP3-7.0" = "gold3")) +
  geom_line(aes(color = "SSP1-2.6")) +
  geom_smooth(fill = "royalblue4", alpha = 0.3) +
  geom_line(aes(y = S2, color = "SSP2-4.5")) +
  geom_smooth(aes(y = S2), color = "darkolivegreen4", fill = "darkolivegreen4", alpha = 0.3) +
  geom_line(aes(y = S3, color = "SSP3-7.0")) +
  geom_smooth(aes(y = S3), color = "gold3", fill = "gold3", alpha = 0.3) +
  geom_hline(aes(yintercept  = 1), linetype = 3) +
  labs(title ="c.",  color = "Scenario", x = "Year", y = expression(bold("L"[C]))) +
  theme_bw() +
  theme(legend.text = element_text(size=9, colour = "black"),
        legend.title = element_text(size=9, colour = "black", face="bold"),
        axis.text.x  = element_text(vjust=1.5, size=11, colour = "black"),
        axis.text.y  = element_text(size=11, colour = "black"),
        axis.title.y = element_text(size=11, face="bold"),
        axis.title.x = element_text(size=11, face="bold"),
        plot.title = element_text(vjust=1.5, face="bold", size=13, colour = "black")) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5)))


Trajectories <- ggplot(likelihoodTrajectories[likelihoodTrajectories$Treatment == 1,], aes(x = Year, y = wLikelihood)) +
  facet_wrap(~Emissions) +
  geom_line(aes(color = "PB+A+")) +
  geom_smooth(aes(y = wLikelihood, color = "PB+A+"), fill = "coral4", alpha = 0.2) +
  geom_ribbon(aes(ymin = wLikelihood - wSE, ymax = wLikelihood + wSE, fill = "PB+A+"),
              alpha = 0.4) +
  geom_line(data = likelihoodTrajectories[likelihoodTrajectories$Treatment == 2,], aes(x = Year, y = wLikelihood, color = "PB-A+")) +
  geom_smooth(data = likelihoodTrajectories[likelihoodTrajectories$Treatment == 2,], aes(y = wLikelihood, color = "PB-A+"), fill = "darkolivegreen3", alpha = 0.2) +
  geom_ribbon(aes(ymin = likelihoodTrajectories$wLikelihood[likelihoodTrajectories$Treatment == 2] - likelihoodTrajectories$wSE[likelihoodTrajectories$Treatment == 2], 
                  ymax = likelihoodTrajectories$wLikelihood[likelihoodTrajectories$Treatment == 2] + likelihoodTrajectories$wSE[likelihoodTrajectories$Treatment == 2],
                  fill = "PB-A+"), alpha = 0.4) +
  geom_line(data = likelihoodTrajectories[likelihoodTrajectories$Treatment == 4,], aes(x = Year, y = wLikelihood, color = "PB+A-")) +
  geom_smooth(data = likelihoodTrajectories[likelihoodTrajectories$Treatment == 4,], aes(y = wLikelihood, color = "PB+A-"), fill = "darkgoldenrod1", alpha = 0.2) +
  geom_ribbon(aes(ymin = likelihoodTrajectories$wLikelihood[likelihoodTrajectories$Treatment == 4] - likelihoodTrajectories$wSE[likelihoodTrajectories$Treatment == 4], 
                  ymax = likelihoodTrajectories$wLikelihood[likelihoodTrajectories$Treatment == 4] + likelihoodTrajectories$wSE[likelihoodTrajectories$Treatment == 4],
                  fill = "PB+A-"), alpha = 0.4) +
  geom_line(data = likelihoodTrajectories[likelihoodTrajectories$Treatment == 3,], aes(x = Year, y = wLikelihood, color = "PB-A-")) +
  geom_smooth(data = likelihoodTrajectories[likelihoodTrajectories$Treatment == 3,], aes(y = wLikelihood, color = "PB-A-"), fill = "darkolivegreen", alpha = 0.2) +
  geom_ribbon(aes(ymin = likelihoodTrajectories$wLikelihood[likelihoodTrajectories$Treatment == 3] - likelihoodTrajectories$wSE[likelihoodTrajectories$Treatment == 3], 
                  ymax = likelihoodTrajectories$wLikelihood[likelihoodTrajectories$Treatment == 3] + likelihoodTrajectories$wSE[likelihoodTrajectories$Treatment == 3],
                  fill = "PB-A-"), alpha = 0.4) +
  geom_hline(aes(yintercept  = mean(likelihoodTrajectories$wLikelihood[likelihoodTrajectories$Treatment == 1 & likelihoodTrajectories$Year == 2022])), linetype = 2) +
  scale_fill_manual(values = c("PB+A+" = "coral4", "PB+A-" = "darkgoldenrod1", "PB-A+" = "darkolivegreen3", "PB-A-" = "darkolivegreen")) +
  scale_color_manual(values = c("PB+A+" = "coral4", "PB+A-" = "darkgoldenrod1", "PB-A+" = "darkolivegreen3", "PB-A-" = "darkolivegreen")) +
  labs(title ="d.",  color = "Scenario", fill = "Scenario", x = "Year", y = expression(bold(L[WF]~"("*ha^{-1}~year^{-1}*")"))) +
  theme_bw() +
  theme(legend.text = element_text(size=9, colour = "black"),
        legend.title = element_text(size=9, colour = "black", face="bold"),
        axis.text.x  = element_text(vjust=1.5, size=9, colour = "black"),
        axis.text.y  = element_text(size=9, colour = "black"),
        axis.title.y = element_text(size=11, face="bold"),
        axis.title.x = element_text(size=11, face="bold"),
        plot.title = element_text(vjust=1.5, face="bold", size=13, colour = "black"),
        panel.spacing = unit(1, "lines")) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5)))  

coeff <- 4
AC <- ggplot(out, aes(x = TSF, y = wAgeO, color = "Original")) +
  geom_area(fill = "black", alpha = 0.2) +
  geom_area(aes(x = TSF, y = wAgeA, color = "PB+A+"), fill = "coral4", alpha = 0.4) +
  geom_area(aes(x = TSF, y = wAgeB, color = "PB-A-"), fill = "darkolivegreen", alpha = 0.4) +
  geom_line(aes(y = Likelihood * coeff), linewidth = 1, color = "gray") +
  geom_ribbon(aes(ymin = (pmax(0,(Likelihood - SE) * coeff)), 
                  ymax = (Likelihood + SE) * coeff),
              fill = NA, linetype = 2, color = "gray") +
  scale_color_manual(values = c("Original" = "black", "PB+A+" = "coral4", "PB-A-" = "darkolivegreen")) +
  scale_y_continuous(name = "Prop. area", sec.axis = sec_axis(trans=~./coeff, name = expression(bold(L[F]~"("*ha^{-1}~year^{-1}*")")))) +
  labs(title ="e.",  color = "Distribution", x = "Age Class") +
  theme_bw() +
  theme(legend.text = element_text(size=9, colour = "black"),
        legend.title = element_text(size=9, colour = "black", face="bold"),
        axis.text.x  = element_text(vjust=1.5, size=11, colour = "black"),
        axis.text.y  = element_text(size=11, colour = "black"),
        axis.title.y = element_text(size=11, face="bold"),
        axis.title.y.right = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size=11, face="bold"),
        plot.title = element_text(vjust=1.5, face="bold", size=13, colour = "black")) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5)))


Final_L_graph <- ggplot(Final_L, aes(x = Emissions, y = Diff, group = Emissions)) +
  geom_boxplot(notch = TRUE, outliers = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size  = 3, colour = "red") + 
  stat_summary(
    fun     = mean,
    geom    = "text",
    aes(label = sprintf("%.2f", ..y..)),
    hjust   = -0.5,
    vjust   = -1.7
  ) +
  labs(title ="f.", x = "Emission pathway", y = expression(bold("L2100"["ratio"]))) +
  theme_bw() +
  theme(legend.text = element_text(size=11, colour = "black"),
        legend.title = element_text(size=11, colour = "black", face="bold"),
        axis.text.x  = element_text(vjust=1.5, size=9, colour = "black"),
        axis.text.y  = element_text(size=9, colour = "black"),
        axis.title.y = element_text(size=11, face="bold"),
        axis.title.x = element_text(size=11, face="bold"),
        plot.title = element_text(vjust=1.5, face="bold", size=13, colour = "black"))  


