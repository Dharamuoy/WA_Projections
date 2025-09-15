# SCRIPT 1
# Fit function to biophysical feedback measured in Zylstra et al. (2024)

suppressWarnings(suppressMessages(library(raster)))
suppressWarnings(suppressMessages(library(parallel)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(tidyr)))
suppressWarnings(suppressMessages(library(outliers)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(patchwork)))
library(minpack.lm)
library(zoo)
library(stats) 

# Set B
newPIP <- read.csv("PIP.csv")
newPIPa <- newPIP[,-c(1,3:46)]
newPIPTab <- pivot_longer(newPIPa, X2000.1:X2021.1, names_to = "Year", values_to = "value")[,-(2)]
tabB <- newPIPTab[which(complete.cases(newPIPTab)),]
tabB$gp <- 5*ceiling(tabB$Age/5)

varAges <- tabB %>%
  group_by(Age) %>%
  summarize_all(sd)
meanSD <- mean(varAges$value, na.rm = TRUE)
SDSD <- sd(varAges$value, na.rm = TRUE)
remAges <- which(varAges$value>meanSD+SDSD)
outPoints <- vector()
outN <- 1
for (remage in remAges) {
  l <- filter(tabB, Age == remage)$value
  OL <- (outliers::grubbs.test(l, type = 10, opposite = FALSE, two.sided = TRUE))
  outlier <- as.numeric(gsub("[^0-9.]", "", OL$alternative))
  outPoints[outN] <- (which(tabB$value == outlier & tabB$Age == remage))
  outN <- outN+1
}
tabC <- tabB[-outPoints,]
#tabD <- tabB[-1129,]

OUT <- tabC %>%
  group_by(Age) %>%
  summarize_all(mean)
OUT$names <- factor(OUT$gp, levels=c("5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75"))

# Fit model
q_mod <- nlsLM(
  value ~ a*Age^3 + b*Age^2 + c*Age + d,
  data = OUT,
  start = list(a = 0.000005, b = 0.000005, c = 0.005, d = 0.001) 
)
q_params <- summary(q_mod)

# Predict values
OUT$mod <- predict(q_mod)

# Domain = the highest age where modelled values > 0
co <- coef(q_mod)
all_roots <- polyroot(c(co["d"], co["c"], co["b"], co["a"]))
Domain <- floor((Re(all_roots)[abs(Im(all_roots)) < 1e-8])[2])

# Fit smoothed standard error
OUT <- OUT %>%
  mutate(
    Squares = (value - mod)^2
  ) %>%
  arrange(Age)
OUT <- OUT %>%
  mutate(
    Smoothed_SE = sqrt(
      rollapply(
        Squares,
        width = 5,        # e.g. a 5‐point window; change as needed
        FUN   = mean,
        fill  = NA,
        align = "center"
      )
    )
  )

# Fit SEmodel
SE_mod <- nlsLM(
  Smoothed_SE ~ a*Age^2 + b*Age + c,
  data = OUT,
  start = list(a = -0.000005, b = 0.000005, c = 0.005) 
)
SEParams <- summary(SE_mod)
OUTshort <- OUT[3:70,]
OUTshort$SE<- predict(SE_mod)

ModGraph <- ggplot(data = OUT, aes(names, value)) +
  geom_boxplot() +
  geom_smooth(method="lm", formula = y ~ poly(x, 3), linewidth = 0.5, color= "black",
              fill = "black", alpha = 0.1, aes(group=1)) +
  ggtitle("Equation 1 - biophysical feedback") +
  ylim(0,NA) +
  labs(y = expression(paste("Fire likelihood (ha"^"-1"*"year"^"-1"*")")), x = "Years since fire") + 
  theme_bw() +
  theme(axis.text.x  = element_text(vjust=1.5, size=11),
        axis.text.y  = element_text(size=11),
        axis.title.y = element_text(face="bold", size=14),
        axis.title.x = element_text(face="bold", size=14),
        plot.title = element_text(vjust=1.5, face="bold", size=14, colour = "black"))

SEGraph <- ggplot(data = OUT, aes(Age, Smoothed_SE)) +
  geom_point(aes(Age, Smoothed_SE))+
  geom_smooth(method="lm", formula = y ~ poly(x, 2), linewidth = 0.5, color= "black",
              fill = "black", alpha = 0.1, aes(group=1)) +
  ggtitle("Equation 2 - Standard Error of Eq. 1") +
  ylim(0,NA) +
  labs(y = expression(paste("Standard Error (ha"^"-1"*"year"^"-1"*")")), x = "Years since fire") + 
  theme_bw() +
  theme(axis.text.x  = element_text(vjust=1.5, size=11),
        axis.text.y  = element_text(size=11),
        axis.title.y = element_text(face="bold", size=14),
        axis.title.x = element_text(face="bold", size=14),
        plot.title = element_text(vjust=1.5, face="bold", size=14, colour = "black"))

#Model summaries
Message1 <- paste("Biophysical feedback model, R\u00B2 =", round(cor(OUT$value, OUT$mod)^2,3), "p =", round(cor.test(OUT$value, OUT$mod)$p.value, 3))
Message2 <- paste("Biophysical feedback SE, R\u00B2 =", round(cor(OUTshort$Smoothed_SE, OUTshort$SE)^2,3), "p =", round(cor.test(OUTshort$Smoothed_SE, OUTshort$SE)$p.value, 3))
Summary1 <- base::summary(q_mod)
Summary2 <- base::summary(SE_mod)




