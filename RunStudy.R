# MASTER SCRIPT TO RUN THE FULL STUDY

# 1. Biophysical feedback to fire
source("C:/Users/Ngarrag0/Documents/Research/SouthernForests/ProjectedFire/ERL/1_BioFeedback.R") 
# 2. Build the best climate ensembles for 3 SSPs
source("C:/Users/Ngarrag0/Documents/Research/SouthernForests/ProjectedFire/ERL/2_ClimateEnsembles.R") 
# 3. Calculate LF_climate and Lc
source("C:/Users/Ngarrag0/Documents/Research/SouthernForests/ProjectedFire/ERL/3_ClimateDrivers.R") 
# 4. Measure Effect_A- & Effect_PB-
source("C:/Users/Ngarrag0/Documents/Research/SouthernForests/ProjectedFire/ERL/4_treatmentAreas.R") 
# 5. Run the projections
source("C:/Users/Ngarrag0/Documents/Research/SouthernForests/ProjectedFire/ERL/5_Projections.R") 
# 6. Analyse the results
source("C:/Users/Ngarrag0/Documents/Research/SouthernForests/ProjectedFire/ERL/6_Analyses.R") 

# OUTPUTS
# ___________________________________________________________________
# Fig. S1
windows(9,4.5)
ModGraph + SEGraph +
  plot_layout(ncol = 2)

# Fig. S2
windows (9,9)
corrplot(method = "number", climCos[[2]]) # Fig. S1

# Fig. 3
windows(10,8.2)
(TT | RF | SSP_P) /
  (Trajectories) /
  (AC | Final_L_graph) +
  plot_layout(guides = "keep")

Clim_Models # Table S1
C3summ # LF_climate
Lc2summ # Lc

# Treatment areas
summTable

# Fire impact tables
L2100_tab # WF likelihood - treatments
L2100C_tab # WF likelihood - climate
LF2100_tab # All fire likelihood - treatments
LF2100C_tab # All fire likelihood - climate
S2100_tab # Likelihood of high severity fire - treatments
S2100C_tab # Likelihood of high severity fire - climate
AC2100_tab # Age class diversity - treatments
AC2100C_tab # Age class diversity - climate

Message1
Summary1
Message2
Summary2

# Biophysical feedback
Message3

