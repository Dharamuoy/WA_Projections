# FUNCTIONS USED IN PROJECTION STUDIES

#_____________________________________________________________________________________________
pairwise_tests <- function(df, year = 2100, value = "wmean", group = "Treatment") {
  # Filter to the year of interest
  d2100 <- df %>%
    filter(!is.na(.data[[group]]), Year == year)
  
  # Unique groups
  trts  <- sort(unique(d2100[[group]]))
  pairs <- combn(trts, 2, simplify = FALSE)
  
  map_dfr(pairs, function(pair) {
    g1 <- d2100[[value]][d2100[[group]] == pair[1]]
    g2 <- d2100[[value]][d2100[[group]] == pair[2]]
    
    wt <- wtd.t.test(g1, g2, alternative = "two.tailed")$coefficients
    cd <- cohen.d(g1, g2, hedges.correction = TRUE)
    
    tibble(
      Treatment_1      = pair[1],
      Treatment_2      = pair[2],
      t_statistic = wt[1],
      df          = wt[2],
      p_value     = wt[3],
      cohen_d     = cd$estimate,
      d_lo        = cd$conf.int[1],
      d_hi        = cd$conf.int[2]
    )
  })
}

outcome_tests <- function(df,
                          years    = c(2030, 2050, 2100, "All"),
                          value    = "wmean",
                          group    = "Treatment",
                          control  = 1,
                          start = 2022
) {
  # 1. Identify the treatments to test against the control
  trts   <- sort(unique(df[[group]]))
  others <- append(setdiff(trts, control), "Climate")
  
  # 2. Start a wide output: one row per "other" treatment
  outcomes <- data.frame(Treatment = others)
  
  # 3. Loop over each period
  for (yr in years) {
    
    if(yr == "All") {
      # if "All", use the whole dataset
      dfy <- df %>%
        filter(!is.na(.data[[group]]))
    } else {
      # otherwise, filter to the year of interest
      dfy <- df %>%
        filter(!is.na(.data[[group]]), Year == yr)
    }
    
    # Build the control‐vs‐other pairs
      pairs <- setNames(
        lapply(others, function(x) c(control, x)),
        paste0(control, "_", others)
      )
    
    # Run the tests
    test <- map_dfr(pairs, function(pair) {
      if(pair[2] == "Climate"){
        dfyC <- df %>%
          filter(!is.na(.data[[group]]), Year == start)
        g1 <- na.omit(dfyC[[value]][dfyC[[group]] == pair[1]])
        g2 <- dfy[[value]][dfy[[group]] == pair[1]]
        wt <- t.test(g1, g2, alternative = "two.sided")$coefficients
      } else {
        g1 <- dfy[[value]][dfy[[group]] == pair[1]]
        g2 <- dfy[[value]][dfy[[group]] == pair[2]]
        wt <- wtd.t.test(g1, g2, alternative = "two.tailed")$coefficients
      }
      
      cd <- cohen.d(g2, g1, hedges.correction = TRUE)
      
      # Find significance
      Ssym <- case_when(wt[3] < 0.001 ~ "***",
                        wt[3] < 0.01  ~ "**",
                        wt[3] < 0.05  ~ "*",
                        TRUE          ~ "")
      
      tibble(
        cohen_d     = paste0(sprintf("%.2f", cd$estimate), Ssym),
        Attribution = paste0(round((100*(mean(g2) - mean(g1))/mean(g1)), 1),"%",
                             " (", round(mean(g1),3), " | ", round(mean(g2),3), ")"),
      )
    })
    
    # 4. Rename the remaining cols so they carry the year suffix
    names(test) <- paste0(names(test), "_", yr)
    
    # 5. Add to the output table
    outcomes <- cbind(outcomes, test)
  }
  
  outcomes
}


#_____________________________________________________________________________________________
climate_attribution <- function(df_control, df,
                          years    = c(2030, 2050, 2100, "All"),
                          value    = "wmean",
                          group    = "Treatment",
                          control  = 1,
                          start = 2022
) {
  
  outcomes <- data.frame(Treatment = control)
  
  # loop over each period
  for (yr in years) {
    
    if(yr == "All") {
      # if "All", use the whole dataset
      dfy <- df %>%
        filter(!is.na(.data[[group]]))
      dfyC <- df_control %>%
        filter(!is.na(.data[[group]]))
    } else {
      # otherwise, filter to the year of interest
      dfy <- df %>%
        filter(!is.na(.data[[group]]), Year == yr)
      dfyC <- df_control %>%
        filter(!is.na(.data[[group]]), Year == yr)
    }
    
    # run the tests
        g1 <- dfyC[[value]][dfyC[[group]] == control]
        g2 <- dfy[[value]][dfy[[group]] == control]
        wt <- wtd.t.test(g1, g2, alternative = "two.tailed")$coefficients
      
      
      cd <- cohen.d(g2, g1, hedges.correction = TRUE)
      
      # Significance
      Ssym <- case_when(wt[3] < 0.001 ~ "***",
                        wt[3] < 0.01  ~ "**",
                        wt[3] < 0.05  ~ "*",
                        TRUE          ~ "")
      
      test <- tibble(
        cohen_d     = paste0(sprintf("%.2f", cd$estimate), Ssym),
        Attribution = paste0(round((100*(mean(g2) - mean(g1))/mean(g1)), 1),"%",
                             " (", round(mean(g1),3), " | ", round(mean(g2),3), ")"),
      )
    
    # rename the remaining cols so they carry the year suffix
    names(test) <- paste0(names(test), "_", yr)
    outcomes <- cbind(outcomes, test)
  }
  
  outcomes
}

#_____________________________________________________________________________________________
# Calculate climatic drivers of fire frequency

darimiGanbi <- function(wfHistory, Climate, Type_F = "fih_fire_t", Date_F   = "fih_date1",
                        Exclusion = F, ExcCol = "fih_cause", ExcVal = 2,
                        minYear = 1956, maxYear = 2021){
  # Load data
  FireHist <- sf::st_read(wfHistory)
  
  # Filter out excluded records if specified
  if (Exclusion) {
    FireHist <- FireHist[FireHist[[ExcCol]] != ExcVal, ]
  }
  
  FireHist <- FireHist[FireHist[[Type_F]] != "PB",]
  FireHist$Year_F <- as.integer(substr(FireHist[[Date_F]], 1, 4))
  minYear <- max(minYear,min(FireHist$Year_F), min(Climate$Year))
  maxYear <- min(maxYear, max(Climate$Year))
  Climate <- Climate[Climate$Year >= minYear & Climate$Year<= maxYear,]
  
  if (maxYear <= minYear) {
    stop("Check start and finish years in the minYear, maxYear, Climate and wfHistory.")
  }
  FireHistO <- FireHist[order(FireHist$Year_F, decreasing = FALSE) & FireHist$Year_F >= minYear, ]
  Climate$WF <- NA
  
  # 1. Collect wildfire data
  for (Y in minYear:maxYear) {
    cat("Measuring wildfire area for year", Y, "\n")
    wfYear <- FireHistO[FireHistO$Year_F == Y, ]
    
    # Union overlapping polygons
    union_shapefile <- st_union(wfYear)
    
    # Calculate area of each non-overlapping polygon
    areas <- st_area(union_shapefile)
    
    # Sum up the areas to get the total area
    total_area <- sum(areas)
    
    # Convert to ha
    units <- attr(total_area, "units")
    if (units$numerator[1] == "m") {
      Area <- round((as.numeric(total_area)/10000),0)
    } else {Area <- round(as.numeric(total_area),0)}
    
    Climate$WF[Climate$Year == Y] <- Area
  }
  
  # 2. Test for correlations
  cat("\n", "Building a correlation matrix and finishing.", "\n")
  cor_matrix <-cor(Climate)
  
  out <- list(Climate, cor_matrix)
  return(out)
}

#_____________________________________________________________________________________________
# Predict future wildfire frequency under different burn scenarios, across a landscape with a range of ages

gunamaGurad <- function(ageTab, startYear = 2024, endYear = 2100, meanFrequency = 0.039, frequencyTab, reps = 100,
                        trendForm = "Burr", constants = c(2.3056, 1.6711), domain = 100,
                        SEtrendForm = "Q2", SEconstants = c(-0.00000311, 0.00019310, 0.00219032),
                        annualBurn = c(0, 0, 0.05, 0.05), suppression = c(1, 0.95, 1, 0.95),
                        burnYear = c(NA, NA, NA, 5), PBDuration = c(NA, NA, NA, 12), freeCores = 1){
  
  # 1. List start ages
  startAges <- unique(ageTab$Age)
  
  # 2. Create a cluster of cores with replicated R on each
  nCores <- max(parallel::detectCores() - freeCores,1)
  cl <- parallel::makeCluster(nCores)
  
  # 3. Load the packages
  parallel::clusterEvalQ(cl,
                         { library(dplyr)
                           library(extraDistr)})
  
  # 4. Load the inputs
  parallel::clusterExport(cl, varlist=c('startYear', 'endYear', 'meanFrequency', 'frequencyTab', 'reps',
                                        'trendForm', 'constants', 'domain', 'SEtrendForm', 'SEconstants',
                                        'annualBurn', 'suppression', 'burnYear', 'PBDuration'), environment())
  
  cat("Projecting fire likelihood for the years", startYear, "to", endYear, "for", length(annualBurn), "management scenarios.", "\n")
  parT <- parallel::parLapply(cl, startAges, gunamaMudi)
  
  parallel::stopCluster(cl)
  
  # 6. Summarise and export results
  cat("Compiling results", "\n", "\n")
  
  out <- data.frame()
  for (a in 1:length(startAges)) {
    fireDetail <- parT[[a]] %>%
      left_join(ageTab, by = c("sAge" = "Age"))
    out <- rbind(out, fireDetail)
  }
  
  return(out)
}

#_____________________________________________________________________________________________
# Rearranges the piptab for trend modelling

mana <- function(ganadingaTab, ageCol = 2, pipCols = c(47:68), Outliers = T) {
  dat <- ganadingaTab[,c(ageCol,pipCols)]
  datPivot <- tidyr::pivot_longer(dat, 2:ncol(dat), names_to = "Year", values_to = "value")[,-(2)]
  tab <- datPivot[which(complete.cases(datPivot)),]
  
  if (Outliers) {
    varAges <- tab %>%
      group_by(tab$Age) %>%
      summarize_all(sd)
    meanSD <- mean(varAges$value, na.rm = TRUE)
    SDSD <- sd(varAges$value, na.rm = TRUE)
    remAges <- which(varAges$value>meanSD+SDSD)
    outPoints <- vector()
    outN <- 1
    for (remage in remAges) {
      l <- filter(tab, tab$Age == remage)$value
      OL <- (outliers::grubbs.test(l, type = 10, opposite = FALSE, two.sided = TRUE))
      outlier <- as.numeric(gsub("[^0-9.]", "", OL$alternative))
      outPoints[outN] <- (which(tab$value == outlier & tab$Age == remage))
      outN <- outN+1
    }
    tab <- tab[-outPoints,]
  }
  return(tab)
}


#_____________________________________________________________________________________________
# Helper function nested in gunamaGurad

gunamaMudi <- function(yrs) {
  tabs <- gunama(startAge = yrs, startYear, endYear, meanFrequency, frequencyTab, reps,
                 trendForm, constants, domain, SEtrendForm, SEconstants,
                 annualBurn, suppression, burnYear, PBDuration)
  
  fireDetail <- tabs %>%
    mutate(sAge = yrs)
  
  return(fireDetail)
}


#_____________________________________________________________________________________________
# Predict future wildfire frequency under different burn scenarios.

gunama <- function(startAge = 15, startYear = 2024, endYear = 2100,
                   meanFrequency = 0.039, frequencyTab, reps = 100,
                   trendForm = "Burr", constants = c(2.3056, 1.6711), domain = 100,
                   SEtrendForm = "Q2", SEconstants = c(-0.00000311, 0.00019310, 0.00219032),
                   annualBurn = c(0, 0, 0.05, 0.05), suppression = c(1, 0.95, 1, 0.95),
                   burnYear = c(NA, NA, NA, 5), PBDuration = c(NA, NA, NA, 12)) {
  
  testLengths <- all(length(annualBurn) == length(suppression), length(suppression) == length(burnYear), length(burnYear) == length(PBDuration))
  if (!testLengths) {
    stop("Not all treatments have full details.")
  }
  
  ageClassStructure <- data.frame()
  fireDetail <- data.frame()
  Treatment <- 1
  
  for (propPB in annualBurn) {
    supRed <- suppression[Treatment]
    Scenarios <- data.frame()
    Rep <- vector()
    
    for (rep in 1:reps) {
      Age <- startAge
      Year <- startYear
      ROW <- 1
      noFire <- vector()
      Planned <- vector()
      WF <- vector()
      Y <- vector()
      A <- vector()
      L <- vector()
      FR <- vector()
      SE <- vector()
      
      
      while (Year <= endYear) {
        AgeX <- min(domain,Age)
        AgeY <- case_when(!is.na(PBDuration[Treatment]) & (Year - startYear) > PBDuration[Treatment] ~ 0,
                          is.na(burnYear[Treatment]) ~ Age,
                          TRUE ~ burnYear[Treatment])
        if (exists("frequencyTab")) {
          meanFrequency <- frequencyTab[frequencyTab$Year == Year,rep+2] * supRed
        }
        
        Y[ROW] <- Year
        A[ROW] <- Age
        Rep[ROW] <- rep
        FR[ROW] <- case_when(trendForm == "Burr" ~ constants[1]*constants[2]*((0.1*AgeX^(constants[1]-1))/((1+(0.1*AgeX)^constants[1])^constants[2]+1)),
                             trendForm == "Q3" ~ constants[1] * AgeX^3 + constants[2] * AgeX^2 + constants[3] * AgeX +constants[4],
                             trendForm == "Q2" ~ constants[1] * AgeX^2 + constants[2] * AgeX + constants[3],
                             TRUE ~ 1)
        SE[ROW] <- case_when(SEtrendForm == "Burr" ~ SEconstants[1]*SEconstants[2]*((0.1*AgeX^(SEconstants[1]-1))/((1+(0.1*AgeX)^SEconstants[1])^SEconstants[2]+1)),
                             SEtrendForm == "Q3" ~ SEconstants[1] * AgeX^3 + SEconstants[2] * AgeX^2 + SEconstants[3] * AgeX +SEconstants[4],
                             SEtrendForm == "Q2" ~ SEconstants[1] * AgeX^2 + SEconstants[2] * AgeX + SEconstants[3],
                             TRUE ~ 1)
        
        #L[ROW] <- FR[ROW]*meanFrequency
        L[ROW] <- max((rtnorm(n=1, mean = FR[ROW], sd = SE[ROW], a = 0, b = Inf) * meanFrequency),0)
        
        # Determine whether a bushfire occurs
        if(runif(1) < L[ROW]) {
          noFire[ROW] <- 0
          Planned[ROW] <- 0
          WF[ROW] <- 1
          Age <- 0
          
          # Determine if a PB occurs
        } else if(runif(1) < propPB & Age == AgeY) {
          noFire[ROW] <- 0
          Planned[ROW] <- 1
          WF[ROW] <- 0
          Age <- 0
        } else { # If no fire occurs, add empty values
          noFire[ROW] <- 1
          Planned[ROW] <- 0
          WF[ROW] <- 0
        }
        
        Age <- Age+1
        Year <- Year+1
        ROW <- ROW+1
      }
      REP <- data.frame(Rep = Rep,
                        Year = Y,
                        TSF = A,
                        Likelihood = L,
                        noFire = noFire,
                        Planned = Planned,
                        WF = WF,
                        stringsAsFactors = FALSE)
      Scenarios <- rbind(Scenarios, REP)
    }
    
    Scenarios <- Scenarios %>%
      mutate(BurnProportion = propPB,
             Suppression = supRed,
             Treatment = Treatment)
    fireDetail <- rbind(fireDetail, Scenarios)
    Treatment <- Treatment + 1
  }
  return(fireDetail)
}