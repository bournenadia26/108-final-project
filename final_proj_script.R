## ----setup, include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(fig.width = 4, fig.height = 4)

## ----libaries, include=FALSE--------------------------------------------------------
# make sure to download any you dont have with: install.packages("package_name")
library(here)
library(corrplot)
library(tidyverse)


## ----prep---------------------------------------------------------------------------
Demographic <- read.table(here("data", "Demographic.txt")) # more robust read-in

# change column names to match correct variables
colnames(Demographic) <- c('ID','County','state','Land_area','total_population',
                          'Population_18to34','Population_65','Physicians','beds',
                          'Y_i','Graduate_highschool','Graduate_Bachelor','below_poverty',
                          'unemployment','capita_income','personal_income','Geographic_region')
Demographic2 <- Demographic

# Variable names and dimensions of the table
head(Demographic2)
names(Demographic2)
dim(Demographic2)
summary(Demographic2)

Demographic2$Y_i <- log(Demographic$Y_i) # log transform Y

colnames(Demographic2)[10] <- "LogY_i"

region1 <- Demographic2[Demographic2$Geographic_region == 1,]
region2 <- Demographic2[Demographic2$Geographic_region == 2,]
region3 <- Demographic2[Demographic2$Geographic_region == 3,]
region4 <- Demographic2[Demographic2$Geographic_region == 4,]

n1<-nrow(region1)
n2<-nrow(region2)
n3<-nrow(region3)
n4<-nrow(region4)


## ----corr plots, warning=FALSE------------------------------------------------------
# general corr
cor(Demographic2[,4:17])
corrplot(cor(Demographic2[,4:17]))

# per region corr
regions <- list(region1, region2, region3, region4)
top_list <- list()

for (i in seq_along(regions)) {
  
  cat("FOR REGION: ", i, "\n")
  
  region <- regions[[i]]
  cors <- sapply(region[, 4:17], function(x) cor(x, region$LogY_i))
  ranked <- sort(abs(cors), decreasing = TRUE)
  
  top_list[[i]] <- head(ranked, 5)
  
  corrplot(cor(region$LogY_i, region[,4:17]))
  
  print(top_list[[i]])
}


## -----------------------------------------------------------------------------------
# log(Y_i) = Beta_0 + Beta_1*total_population

boxplot(cbind(region1$LogY_i, region1$total_population),names = c("Total 
                                                                  serious crimes (log-scale)", "Total Population"))

pairs(cbind(region1$LogY_i, region1$total_population),labels = c("Total 
                                                               serious crimes (log-scale)", "Total Population"), lwd=4)

pairs(cbind(region1$LogY_i, log(region1$total_population)), labels = c("Total 
                                                                       serious crimes (log-scale)", "Total population (log-scale)"), lwd = 4)


## -----------------------------------------------------------------------------------
# log(Y_i) = Beta_0 + Beta_1*total_population

boxplot(cbind(region1$LogY_i, region1$Physicians),names = c("Total serious 
      crimes (log-scale)", "Physicians"))

pairs(cbind(region1$LogY_i, region1$Physicians),labels=c("Total serious crimes 
      (log-scale)", "Physicians"), lwd=4)

pairs(cbind(region1$LogY_i, log(region1$Physicians)), labels = c("Total serious 
      crimes (log-scale)", "Physicians (log-scale)"), lwd = 4)


## -----------------------------------------------------------------------------------
# log(Y_i) = Beta_0 + Beta_1*total_population

boxplot(cbind(region1$LogY_i, region1$personal_income),names = c("Total serious crimes (log-scale)", "Personal Income"))

pairs(cbind(region1$LogY_i, region1$personal_income),labels = c("Total serious crimes  (log-scale)", "Personal Income"), lwd=4)

pairs(cbind(region1$LogY_i, log(region1$personal_income)), labels = c("Total serious crimes (log-scale)", "Personal Income (log-scale)"), lwd = 4)


## -----------------------------------------------------------------------------------
# Region 1
# log(Y_i) = Beta_0 + Beta_1*total_population

boxplot(cbind(region1$LogY_i, region1$Population_18to34),names = c("Total serious crimes (log-scale)", "Population (Age 18-34)"))

pairs(cbind(region1$LogY_i, region1$Population_18to34),labels=c("Total serious crimes  (log-scale)", "Population (Age 18-34)"), lwd=4)

pairs(cbind(region1$LogY_i, log(region1$Population_18to34)), labels = c("Total serious crimes (log-scale)", "Population (Age 18-34) (log-scale)"), lwd = 4)


## ----SLR models---------------------------------------------------------------------
model_1 <- lm(LogY_i ~ total_population, data = region1)
model_2 <- lm(LogY_i ~ Physicians, data = region2)
model_3 <- lm(LogY_i ~ personal_income, data = region3)
model_4 <- lm(LogY_i ~ Population_18to34, data = region4)


## ----model stats & CI---------------------------------------------------------------
# REGION 1
summary(model_1) # all model stats
plot(model_1) # all four diagnostic plots
confint(model_1) # 95% CI for both b0 and b1

# REGION 2
summary(model_2)
plot(model_2) # insane outlier here
confint(model_2)

# REGION 3
summary(model_3)
plot(model_3) # & here
confint(model_3)

# REGION 4
summary(model_4)
plot(model_4)
confint(model_4)


## -----------------------------------------------------------------------------------
# REGION 1
model_1b <- lm(log(Demographic$Y_i) ~ log(Demographic$total_population), data = region1)

anova(model_1b)
summary(model_1b)
plot(model_1b) # greatly improved linearity
confint(model_1b)

# REGION 2
model_2b <- lm(log(Demographic$Y_i) ~ log(Demographic$Physicians), data=region2)

anova(model_2b)
summary(model_2b)
plot(model_2b)
confint(model_2b)

# REGION 3
model_3b <- lm(log(Demographic$Y_i) ~ log(Demographic$personal_income), data = region3)

anova(model_3b)
summary(model_3b)
plot(model_3b) # greatly improved linearity, previous outliers arent as bad now
confint(model_3b)

# REGION 4
model_4b <- lm(log(Demographic$Y_i) ~ log(Demographic$Population_18to34), data = region4)

anova(model_4b)
summary(model_4b)
plot(model_4b)
confint(model_4b)


## -----------------------------------------------------------------------------------
# Total Population
region1 |>
        ggplot(aes(x = log(total_population), y = LogY_i)) +
        geom_point() +
        theme_bw()

# Physicians
region2 |>
        ggplot(aes(x = log(Physicians))) +
        geom_histogram() +
        theme_bw()

region2 |>
        ggplot(aes(x = log(Physicians), y = LogY_i)) +
        geom_point() +
        theme_bw()

# Personal Income
region3 |>
        ggplot(aes(x = log(personal_income), y = LogY_i)) +
        geom_point() +
        theme_bw()

# Population 18-34
region4 |>
        ggplot(aes(x = log(Population_18to34), y = LogY_i)) +
        geom_point() +
        theme_bw()


## -----------------------------------------------------------------------------------
# Setting up data and choosing a region

nrow(region4)
head(region4)

pairs(region4[,5:12], lwd=2)
cor(region4[,5:12])


## -----------------------------------------------------------------------------------
model_TP <- lm(LogY_i ~ total_population, data = region4)
model_logTP <- lm(LogY_i ~ log(total_population), data = region4)

plot(model_TP)
summary(model_TP)
plot(model_logTP)
summary(model_logTP)

model_P <- lm(LogY_i ~ Physicians, data = region4)
model_logP <- lm(LogY_i ~ log(Physicians), data = region4)

plot(model_P)
summary(model_P)
plot(model_logP)
summary(model_logP)

model_PI <- lm(LogY_i ~ personal_income, data = region4)
model_logPI <- lm(LogY_i ~ log(personal_income), data = region4)

plot(model_PI)
summary(model_PI)
plot(model_logPI)
summary(model_logPI)


## -----------------------------------------------------------------------------------
model <- lm(LogY_i ~ Population_18to34 + log(total_population) + log(Physicians) + log(personal_income), data = region4)

plot(model)
summary(model)
anova(model)


## -----------------------------------------------------------------------------------
# log transformations turned out to be the best for the extra 3 predictors
model_4_1 <- lm(LogY_i ~ 1, data = region4)

model_4_2 <- lm(LogY_i ~ Population_18to34, data = region4)

model_4_3 <- lm(LogY_i ~ log(total_population), data = region4)

model_4_4 <- lm(LogY_i ~ log(Physicians), data = region4)

model_4_5 <- lm(LogY_i ~ log(personal_income), data = region4)

model_4_6 <- lm(LogY_i ~ Population_18to34 + log(total_population), data = region4)

model_4_7 <- lm(LogY_i ~ Population_18to34 + log(Physicians), data = region4)

model_4_8 <- lm(LogY_i ~ Population_18to34 + log(personal_income), data = region4)

model_4_9 <- lm(LogY_i ~ log(total_population) + log(Physicians), data = region4)

model_4_10 <- lm(LogY_i ~ log(total_population) + log(personal_income), data = region4)

model_4_11 <- lm(LogY_i ~ log(Physicians) + log(personal_income), data = region4)

model_4_12 <- lm(LogY_i ~ Population_18to34 + log(total_population) + log(Physicians), data = region4)

model_4_13 <- lm(LogY_i ~ Population_18to34 + log(total_population) + log(personal_income), data = region4)

model_4_14 <- lm(LogY_i ~ Population_18to34 + log(Physicians) + log(personal_income), data = region4)

model_4_15 <- lm(LogY_i ~ log(total_population) + log(Physicians) + log(personal_income), data = region4)

model_4_16 <- lm(LogY_i ~ Population_18to34 + log(total_population) + log(Physicians) + log(personal_income), data = region4)

models <- list(model_4_1, model_4_2, model_4_3, model_4_4, model_4_5, model_4_6, model_4_7, model_4_8, model_4_9, model_4_10, model_4_11, model_4_12, model_4_13, model_4_14, model_4_15, model_4_16)


## -----------------------------------------------------------------------------------
n <- nrow(region4)
p_full <- length(coef(model_4_16))
MSE_full <- sum(residuals(model_4_16)^2) / (n - p_full)

results <- data.frame(
  variables = c("None", "Population_18to34", "log(Total_Population)", "log(Physicians)", "log(Personal_Income)", "18to34+logTotPop", "18to34+logPhys", "18to34+logIncome", "logTotPop+logPhys", "logTotPop+logIncome", "logPhys+logIncome", "18to34+logTotPop+logPhys", "18to34+logTotPop+logIncome", "18to34+logPhys+logIncome", "logTotPop+logPhys+logIncome", "All four"),
  p = sapply(models, function(m) length(coef(m))),
  SSE = sapply(models, function(m) sum(residuals(m)^2)),
  R2 = sapply(models, function(m) summary(m)$r.squared),
  R2_adj = sapply(models, function(m) summary(m)$adj.r.squared),
  C_p = sapply(models, function(m) {
               SSE_p <- sum(residuals(m)^2)
               p_p   <- length(coef(m))
               SSE_p / MSE_full - (n - 2 * p_p)
               }),
  AIC = sapply(models, AIC),
  BIC = sapply(models, BIC),
  PRESS = sapply(models, function(m) {
                 sum((residuals(m) / (1 - hatvalues(m)))^2)
                 }),
  F_pval = sapply(models, function(m) {
                  f <- summary(m)$fstatistic
                  if (is.null(f)) return(NA)  # null model has no F stat
                  pf(f[1], f[2], f[3], lower.tail = FALSE)
                  }),
  sig_predictors = sapply(models, function(m) {
                          coefs <- summary(m)$coefficients
                          sum(coefs[-1, 4] < 0.05) # exclude intercept row, count p < 0.05
                          })
)

results


## -----------------------------------------------------------------------------------
plot(model_4_15)
summary(model_4_15)

